unit uUpdateDatabase; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uBasedatamodule,uDBaseDM, dbf,uAppconsts,uData,db,Utils,FileUtil;

type

  { TfMain }

  TfMain = class(TForm)
    lbMandants: TListBox;
    bCheck: TButton;
    lbInfo: TListBox;
    bUpdate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bCheckClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbMandantsSelectionChange(Sender: TObject; User: boolean);
    procedure dbMandantDataDBInfo(info: string);
    procedure bUpdateClick(Sender: TObject);
  private
    { private declarations }
    procedure Updaterefs(Dataset : TDataSet;Spaces : string = '');
  public
    { public declarations }
    DataDB : TBaseDataModule;
    Mandants : array of Pointer;
  end;

var
  fMain: TfMain;

implementation

procedure TfMain.FormCreate(Sender: TObject);
begin

end;

{ TfMain }

procedure TfMain.bCheckClick(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  bUpdate.Enabled := False;
  FreeAndNil(DataDB);
  with Data.Mandants.DataSet do
    begin
      DataDB := CreateDataModule(FieldbyName('DBTYP').AsString);
      if Assigned(DataDB) then
        begin
          DataDB.DontBind:=True;
          Data.DataModule := DataDB;
          DataDB.Mandants := Data.Mandants.DataSet;
          DataDB.SetProperties(FieldbyName('DBPROP').AsString);
          DataDB.OnInfo :=@dbMandantDataDBInfo;
          DataDB.CreateDB;
          DataDB.OpenDB;
          if not DataDB.CheckDB then
            bUpdate.Enabled := True;
          DataDB.Rights.Open;
          if ((DataDB.Rights.FieldDefs.IndexOf('REF_ID') = -1) or ((DataDB.Rights.FieldByName('REF_ID').IsNull) and (DataDB.Rights.RecordCount > 0)))
          then
            bUpdate.Enabled := True;
          DataDB.CloseDB;
        end;
    end;
  FreeAndNil(DataDB);
  Screen.Cursor:=crDefault;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  Data.AppendMandant;
  with Data.Mandants.DataSet do
    begin
      Open;
      First;
      while not EOF do  //List mandants and validate Tables
        begin
          Setlength(Mandants,length(Mandants)+1);
          Mandants[length(Mandants)-1] := GetBookmark;
          lbMandants.Items.Add(FieldbyName('NAME').AsString);
          Next;
        end;
    end;
end;

procedure TfMain.lbMandantsSelectionChange(Sender: TObject; User: boolean);
begin
  if lbMandants.ItemIndex = -1 then exit;
  Data.Mandants.DataSet.GotoBookmark(Mandants[lbMandants.ItemIndex]);
  bCheck.Enabled := True;
end;

procedure TfMain.dbMandantDataDBInfo(info: string);
begin
  lbInfo.Items.Add(info);
  lbInfo.ItemIndex := lbInfo.Items.Count-1;
  lbInfo.MakeCurrentVisible;
  Application.ProcessMessages;
end;

procedure TfMain.bUpdateClick(Sender: TObject);
var
  i: Integer;
begin
  Screen.Cursor:=crHourglass;
  bUpdate.Enabled := False;
  FreeAndNil(DataDB);
  with Data.Mandants.DataSet do
    begin
      DataDB := CreateDataModule(FieldbyName('DBTYP').AsString);
      if Assigned(DataDB) then
        begin
          DataDB.DontBind:=True;
          Data.DataModule := DataDB;
          DataDB.Mandants := Data.Mandants.DataSet;
          DataDB.SetProperties(FieldbyName('DBPROP').AsString);
          DataDB.OnInfo :=@dbMandantDataDBInfo;
          for i := 1 to DataDB.Tables.Count-1 do
            TDataSet(DataDB.Tables[i]).Open;
          if not DataDB.AlterDB then
            begin
              Showmessage('Error updating DB');
            end;
          DataDB.CloseDB;
        end;
      FreeAndNil(DataDB);
      DataDB := CreateDataModule(FieldbyName('DBTYP').AsString);
      if Assigned(DataDB) then
        begin
          DataDB.DontBind:=False;
          Data.DataModule := DataDB;
          DataDB.Mandants := Data.Mandants.DataSet;
          DataDB.SetProperties(FieldbyName('DBPROP').AsString);
          DataDB.OpenDB;
          DataDB.Rights.Open;
          dbMandantDataDBInfo('Updating ID´s');
          if ((DataDB.Rights.FieldDefs.IndexOf('REF_ID') = -1) or ((DataDB.Rights.FieldByName('REF_ID').IsNull) and (DataDB.Rights.RecordCount > 0))) then
            //ref IDS are not there this is an new DB Version with REF Fields
            begin
              DataDB.BeginTransaction;
              for i := 0 to DataDB.Tables.Count-1 do
                begin
                  if DataDB.IsNoChild(TDataSet(DataDB.Tables[i]))
                  and (not Assigned(DataDB.MasterSources[i])) then
                    begin
                      dbMandantDataDBInfo(TDataSet(DataDB.Tables[i]).Name);
                      DataDB.OpenChilds(TDataSet(DataDB.Tables[i]));
                      UpdateRefs(TDataSet(DataDB.Tables[i]));
                    end;
                end;
              DataDB.CommitTransaction;
              dbMandantDataDBInfo('Updating Msg Ref');
              DataDB.BeginTransaction;
              with DataDB.MessageIdx do
                begin
                  First;
                  while not EOF do
                    begin
                      Data.SetFilter(Data.Messages,'ID='+Data.DataModule.QuoteValue(FieldByName('ID').AsString));
                      if Data.Messages.DataSet.RecordCount > 0 then
                        begin
                          Data.MessageIdx.DataSet.Edit;
                          Data.MessageIDX.FieldByName('MSG_ID').AsVariant := Data.Messages.FieldByName('SQL_ID').AsVariant;
                          Data.MessageIdx.DataSet.Post;
                        end;
                      Next;
                    end;
                end;
              DataDB.CommitTransaction;
            end;
        end;
      FreeAndNil(DataDB);
    end;
  Screen.Cursor:=crDefault;
end;

procedure TfMain.Updaterefs(Dataset: TDataSet;Spaces : string = '');
var
  NewID: LargeInt;
  i: Integer;
begin
  DataDB.SetFilter(DataSet,'');
  if not Dataset.Active then
    DataSet.Open;
  if DataSet.RecordCount = 0 then exit;
  DataSet.First;
  while not DataSet.EOF do
    begin
      if (Spaces = '') or (DataSet.Name = 'MESSAGEIDX') then
        dbMandantDataDBInfo(Spaces+'Rec '+IntToStr(DataSet.RecNo)+' of '+IntToStr(DataSet.RecordCount));
      NewID := DataDB.GetUniID;
      DataSet.Edit;
      DataSet.FieldByName('SQL_ID').AsVariant := NewID;
      if DataSet.FieldDefs.IndexOf('REF_ID') > -1 then
        begin
          i := DataDB.Tables.IndexOf(DataSet);
          if i > -1 then
            if Assigned(DataDB.MasterSources[i]) then
              DataSet.FieldByName('REF_ID').AsVariant := TDataSource(DataDB.MasterSources[i]).FieldByName('SQL_ID').AsVariant;
        end;
      DataSet.Post;
      for i := 0 to DataDB.Tables.Count-1 do
        if Assigned(DataDB.MasterSources[i]) and (TDataSource(DataDB.MasterSources[i]).DataSet = DataSet) then
          begin
            Updaterefs(TDataSet(DataDB.Tables[i]),Spaces+' ');
          end;
      DataSet.Next;
    end;
end;

initialization
  {$I uupdatedatabase.lrs}

end.
