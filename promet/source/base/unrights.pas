{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uNRights;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ButtonPanel, DBGrids, Variants, Grids, DbCtrls,
  uExtControls, db;

type
  THackGrid = class(TStringGrid);

  { TfNRights }

  TfNRights = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbUser: TComboBox;
    cbRight: TComboBox;
    dgRights: TExtDBGrid;
    Permissions: TDatasource;
    dnStandart: TDBNavigator;
    Label1: TLabel;
    procedure ApplicationIBaseDBInterfaceDataAfterInsert(DataSet: TDataSet);
    procedure cbRightSelect(Sender: TObject);
    procedure cbUserSelect(Sender: TObject);
    procedure dgRightsDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure dgRightsSelectEditor(Sender: TObject; Column: TColumn;
      var Editor: TWinControl);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Users : TStringList;
    FID : Variant;
  public
    { public declarations }
    procedure SetupDB;
    function Execute(Id : Variant) : Boolean;
  end; 

var
  fNRights: TfNRights;

implementation

uses uError, uBaseDbInterface;

resourcestring
  strYoumustselectanUserFirst        = 'Sie müssen zuerst einen Benutzer auswählen';

{ TfNRights }

procedure TfNRights.FormCreate(Sender: TObject);
begin
  Users := TStringList.Create;
end;

procedure TfNRights.dgRightsSelectEditor(Sender: TObject; Column: TColumn;
  var Editor: TWinControl);
var
  i: Integer;
begin
  if Column.FieldName = 'USER' then
    begin
      Editor:=cbUser;
      Editor.BoundsRect := dgRights.CellRect(THackGrid(Sender).Col,THackGrid(Sender).Row);
      for i := 0 to Users.Count-1 do
        if Users.ValueFromIndex[i] = Column.Field.AsString then
          cbUser.Text := Users.Names[i];
    end
  else if Column.FieldName = 'RIGHT' then
    begin
      Editor:=cbRight;
      Editor.BoundsRect := dgRights.CellRect(THackGrid(Sender).Col,THackGrid(Sender).Row);
      cbRight.Text:=cbRight.Items[Column.Field.AsInteger];
    end;
end;

procedure TfNRights.cbUserSelect(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    begin
      if Data.Permissions.DataSet.RecordCount > 0 then
        begin
          if (not ((Data.Permissions.DataSet.State = dsInsert) or (Data.Permissions.DataSet.State = dsEdit))) then
            Data.Permissions.DataSet.Edit;
        end
      else if (not ((Data.Permissions.DataSet.State = dsInsert) or (Data.Permissions.DataSet.State = dsEdit))) then
        Data.Permissions.DataSet.Insert;
    end;
  with Application as IBaseDBInterface do
    begin
      Data.Permissions.FieldByName('USER').AsVariant:=Users.Values[cbUser.Text];
    end;
end;

procedure TfNRights.cbRightSelect(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    begin
      if Data.Permissions.DataSet.RecordCount > 0 then
        begin
          if (not ((Data.Permissions.DataSet.State = dsInsert) or (Data.Permissions.DataSet.State = dsEdit))) then
            Data.Permissions.DataSet.Edit;
        end
      else if (not ((Data.Permissions.DataSet.State = dsInsert) or (Data.Permissions.DataSet.State = dsEdit))) then
        Data.Permissions.DataSet.Insert;
    end;
  with Application as IBaseDBInterface do
    begin
      if Data.Permissions.FieldByName('REF_ID_ID').AsVariant <> FID then
        begin
          fError.ShowWarning(strYoumustselectanUserFirst);
          exit;
        end;
      Data.Permissions.FieldByName('RIGHT').AsInteger := cbRight.ItemIndex;
    end;
end;

procedure TfNRights.ApplicationIBaseDBInterfaceDataAfterInsert(DataSet: TDataSet
  );
begin
  if (DataSet.State = dsEdit) or (DataSet.State = dsInsert) then
    DataSet.FieldByName('REF_ID_ID').AsVariant:=FID;
end;

procedure TfNRights.dgRightsDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  i: Integer;
begin
  if Column.FieldName = 'USER' then
    begin
      TDbGrid(Sender).Canvas.FillRect(Rect);
      for i := 0 to Users.Count-1 do
        if Users.ValueFromIndex[i] = Column.Field.AsString then
          TDbGrid(Sender).Canvas.TextOut(Rect.Left+2,rect.Top+2,Users.Names[i]);
    end
  else if Column.FieldName = 'RIGHT' then
    begin
      TDbGrid(Sender).Canvas.FillRect(Rect);
      if not Column.Field.IsNull then
        TDbGrid(Sender).Canvas.TextOut(Rect.Left+2,rect.Top+2,cbRight.Items[Column.Field.AsInteger]);
    end;
end;

procedure TfNRights.FormDestroy(Sender: TObject);
begin
  Users.Free;
end;

procedure TfNRights.SetupDB;
begin
  with Application as IBaseDBInterface do
    begin
      Permissions.DataSet := Data.Permissions.DataSet;
    end;
end;

function TfNRights.Execute(Id: Variant): Boolean;
var
  i: Integer;
  aUser: LongInt;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfNRights,fNRights);
      Self := fNRights;
    end;
  with Application as IBaseDBInterface do
    begin
      Data.SetFilter(Data.Permissions,'"REF_ID_ID"='+Data.QuoteValue(VarToStr(Id)),0,'','ASC',False,True,False);
      aUser := Data.Users.GetBookmark;
      FID := ID;
      Data.Users.DataSet.First;
      Users.Clear;
      while not Data.Users.DataSet.EOF do
        begin
          if Data.Users.Leaved.IsNull then
            Users.Values[Data.Users.FieldByName('NAME').AsString] := Data.Users.FieldByName('SQL_ID').AsString;
          Data.Users.DataSet.Next;
        end;
      with cbUser do
        begin
          Clear;
          for i := 0 to Users.Count-1 do
            Items.Add(Users.Names[i]);
        end;
      Data.Users.GotoBookmark(aUser);
      SetupDB;
      cbUser.Visible:=False;
      cbRight.Visible:=False;
      Data.Permissions.DataSet.AfterInsert:=@ApplicationIBaseDBInterfaceDataAfterInsert;
      Result := Showmodal = mrOK;
      Data.Permissions.DataSet.AfterInsert:=nil;
    end;
end;

initialization
  {$I unrights.lrs}

end.
