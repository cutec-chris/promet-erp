program tablestowiki;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this }, db, Utils, FileUtil, Forms,
  uData, uBaseCustomApplication, uOrder, uMasterdata,
  uPerson, pcmdprometapp, uProjects, uBaseDbClasses,uWiki,uBaseDBInterface;
type
  TTablesToWiki = class(TBaseCustomApplication)
  private
    Orders :  TOrder;
  protected
    procedure DoRun; override;
    procedure WriteMessage(s : string);
    procedure WriteError(s : string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  TBaseDBDatasetClass = class of TBaseDBDataset;
procedure TTablesToWiki.DoRun;
var
  aConnection: TComponent;
  aWiki: TWikiList;
  aOverview: TStringList;
  procedure AddTable(aClass : TBaseDBDatasetClass;aLevel : string = '');
  var
    aTable: TBaseDBDataset;
    aTableC: TStringList;
    a: Integer;
    aCaption: String;
  begin
    aTable := aClass.Create(nil,Data);
    with aTable.DataSet as IBaseManageDB do
      begin
        aCaption := TableCaption;
        if aCaption = '' then aCaption := TableName;
        aOverview.Add(aLevel+'[[Admin-Book/Tables/'+TableName+'|'+aCaption+']]');
        aTableC :=  TStringList.Create;
        aTableC.Add('Datenbankname:'+TableName);
        aTableC.Add('==Felder==');
        aTableC.Add('{|');
        aTableC.Add('|-');
        aTableC.Add('| Feldname || Feldinhalt || Feldgrösse || Beschreibung');
        //aTableC.Add('! Feldname !! Feldinhalt !! Feldgrösse !! Beschreibung')
        for a := 0 to ManagedFieldDefs.Count-1 do
          begin
            aTable.DataSet.FieldDefs.Add(ManagedFieldDefs[a].Name,ManagedFieldDefs[a].DataType,ManagedFieldDefs[a].Size);
          end;
        aTable.SetDisplayLabels(aTable.DataSet);
        for a := 0 to ManagedFieldDefs.Count-1 do
          begin
            aTableC.Add('|-');
            aTableC.Add('|'+ManagedFieldDefs[a].Name+'||'+aTable.DataSet.FieldDefs[a].DisplayName+'||'+IntToStr(ManagedFieldDefs[a].Size)+'||');
          end;
        aTableC.Add('|-');
        aTableC.Add('|}');
      end;
    with aTable.DataSet as IBaseSubDataSets do
      begin
        if GetCount>0 then
          aTableC.Add('==Untertabellen==');
        for a := 0 to GetCount-1 do
          begin
            with GetSubDataSetIdx(a).DataSet as IBaseManageDB do
              begin
                aCaption := TableCaption;
                if aCaption = '' then aCaption := TableName;
                aTableC.Add('[[Admin-Book/Tables/'+TableName+'|'+aCaption+']]');
              end;
            AddTable(TBaseDBDataSetClass(GetSubDataSetIdx(a).ClassType),aLevel+'*');
          end;
      end;
    with aTable.DataSet as IBaseManageDB do
      begin
        aWiki.FindWikiPage('Admin-Book/Tables/'+TableName,True);
        with aTable.DataSet as IBaseManageDB do
          begin
            aCaption := TableCaption;
            if aCaption = '' then aCaption := TableName;
          end;
        aWiki.DataSet.Edit;
        aWiki.DataSet.FieldByName('NAME').AsString:=TableName;
        aWiki.DataSet.FieldByName('CAPTION').AsString:=aCaption;
        aWiki.DataSet.FieldByName('DATA').AsString:=aTableC.Text;
        aWiki.DataSet.Post;
      end;
    aTableC.Free;
    aTable.Free;
  end;

begin
  Login;
  aWiki := TWikiList.Create(nil,Data);
  aOverview := TStringList.Create;
  aOverview.Add('==Stammdaten==');
  AddTable(TMasterdata);
  AddTable(TPerson);
  aOverview.Add('==Bewegungsdaten==');
  AddTable(TOrder);
  aOverview.Add('==Projektverwaltung==');
  AddTable(TProject);
  writeln(aOverview.Text);
  aWiki.FindWikiPage('Admin-Book/Tables/Tableoverview',True);
  aWiki.DataSet.Edit;
  aWiki.DataSet.FieldByName('DATA').AsString:=aOverview.Text;
  aWiki.DataSet.Post;
  aOverview.free;
  aWiki.Free;
  //readln;
  // stop program loop
  Terminate;
end;
procedure TTablesToWiki.WriteMessage(s: string);
begin
  writeln(s);
end;
procedure TTablesToWiki.WriteError(s: string);
begin
  writeln('ERROR:'+s);
end;
constructor TTablesToWiki.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;
destructor TTablesToWiki.Destroy;
begin
  inherited Destroy;
end;
var
  Application: TTablesToWiki;
begin
  Application:=TTablesToWiki.Create(nil);
  Application.Run;
  Application.Free;
end.
