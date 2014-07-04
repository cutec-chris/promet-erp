{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
unit uProjectFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LR_DBSet, LR_Class, Forms, Controls, ComCtrls,
  Buttons, ActnList, Menus, ExtCtrls, DbCtrls, StdCtrls, uExtControls,
  DBZVDateTimePicker, db, uPrometFrames, uPrometFramesInplace, uBaseDBClasses,
  Dialogs, Spin, EditBtn,variants,uProjectFlow,uTasks,Graphics;
type

  { TfProjectFrame }

  TfProjectFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acClose: TAction;
    acCopy: TAction;
    acDelete: TAction;
    acPaste: TAction;
    acSave: TAction;
    acSetTreeDir: TAction;
    acShowTreeDir: TAction;
    acStartTimeRegistering: TAction;
    acExport: TAction;
    acImport: TAction;
    acPrint: TAction;
    acRights: TAction;
    acRestart: TAction;
    acGotoParent: TAction;
    acGantt: TAction;
    acInactiveGantt: TAction;
    acCalculatePlan: TAction;
    acMoveOldTasks: TAction;
    ActionList1: TActionList;
    bAssignTree: TSpeedButton;
    bDelegated2: TSpeedButton;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    bExecute: TSpeedButton;
    bExecute1: TSpeedButton;
    bExecute2: TToolButton;
    bShowTree: TSpeedButton;
    cbStatus: TComboBox;
    cbType: TExtDBCombobox;
    cbCategory: TExtDBCombobox;
    bProjectColor: TColorButton;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBCheckBox3: TDBCheckBox;
    DBZVDateTimePicker4: TDBZVDateTimePicker;
    DBZVDateTimePicker5: TDBZVDateTimePicker;
    eParent: TEditButton;
    eManager: TEditButton;
    iProject: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lVAT1: TLabel;
    MandantDetails: TDatasource;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    pNav2: TPanel;
    pNav3: TPanel;
    PopupMenu1: TPopupMenu;
    pPreviewImage: TPanel;
    pStatus: TPanel;
    PTasks: TfrDBDataSet;
    dExport: TSaveDialog;
    sePriority: TSpinEdit;
    sARed: TShape;
    sAYellow: TShape;
    sAGreen: TShape;
    Shape4: TShape;
    Tasks: TDatasource;
    DBZVDateTimePicker1: TDBZVDateTimePicker;
    DBZVDateTimePicker2: TDBZVDateTimePicker;
    DBZVDateTimePicker3: TDBZVDateTimePicker;
    dImport: TOpenDialog;
    eNumber: TDBEdit;
    History: TDatasource;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mInfo: TDBMemo;
    eName: TDBMemo;
    gbTree: TGroupBox;
    lname: TLabel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pComponents: TPanel;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pNav1: TPanel;
    Projects: TDatasource;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miCopy: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miStartTimeregistering: TMenuItem;
    pcPages: TExtMenuPageControl;
    pmAction: TPopupMenu;
    Report: TfrReport;
    sbMenue: TSpeedButton;
    ToolBar1: TPanel;
    ToolBar2: TToolBar;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tsInfo: TTabSheet;
    Users: TDatasource;
    procedure acCalculatePlanExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acGanttExecute(Sender: TObject);
    procedure acGotoParentExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acInactiveGanttExecute(Sender: TObject);
    procedure acMoveOldTasksExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRestartExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure bProjectColorColorChanged(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eManagerButtonClick(Sender: TObject);
    procedure eManagerExit(Sender: TObject);
    procedure eParentButtonClick(Sender: TObject);
    procedure eParentExit(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchOpenItemL(aLink: string): Boolean;
    procedure pcPagesChange(Sender: TObject);
    procedure ProjectsStateChange(Sender: TObject);
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
    procedure sbMenueClick(Sender: TObject);
    procedure sePriorityChange(Sender: TObject);
    procedure TProjectStateChange(Sender: TObject);
  private
    { private declarations }
    FEditable : Boolean;
    FOwners : TStringList;
    FOnStartTime: TOnStartTime;
    FProjectFlow: TProjectFlow;
    procedure AddHistory(Sender: TObject);
    procedure AddPositions(Sender: TObject);
    procedure AddImages(Sender: TObject);
    procedure AddLinks(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    procedure AddTasks(Sender: TObject);
    procedure AddOverview(Sender: TObject);
    procedure RefreshFlow;
  protected
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
    procedure DoOpen;override;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure ShowFrame; override;
    procedure FrameAdded; override;
    procedure GotoTask(aLink : string);
    property OnStartTime : TOnStartTime read FOnStartTime write FOnStartTime;
  end;
var
  aNewProjectName: String;
procedure AddToMainTree(aAction : TAction;Node : TTreeNode);
implementation
uses uData,uProjects,uHistoryFrame,uLinkFrame,uImageFrame,uDocuments,
  uDocumentFrame,uIntfStrConsts,uMainTreeFrame,uBaseDBInterface,uEditableTab,
  uFilterFrame,uBaseSearch,Utils,uprojectimport,uBaseERPDBClasses,uSelectReport,
  uNRights,uprojectpositions,uSearch,LCLProc,utask,uprojectoverview,uBaseVisualApplication,
  uGanttView,uWikiFrame,uWiki;
{$R *.lfm}
resourcestring
  strNoParent                     = '<kein Vorfahr>';
  strSearchFromProjects           = 'Mit Öffnen wird das gewählte Projekt als Vohrfahr übernommen';
  strEnterProcessName             = 'geben Sie einen neuen Namen für den Prozess an';
  strProcessName                  = 'Prozessname';
  strInactiveStatus               = 'Der gewähte Status ist kein aktiver Status, sollen alle aktiven Aufgaben deaktiviert werden ?';
  strActiveStatus                 = 'Der gewähte Status ist ein aktiver Status, sollen alle aktiven Aufgaben aktiviert werden ?';
  strNewProject                   = 'neues Projekt';
procedure AddToMainTree(aAction : TAction;Node : TTreeNode);
var
  aDataSet: TProject;
  Node1: TTreeNode;
begin
  if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
      Data.SetFilter(Data.Tree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('P')+'))',0,'','ASC',False,True,True);
      Data.Tree.DataSet.First;
      while not Data.Tree.dataSet.EOF do
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
          TTreeEntry(Node1.Data).DataSource := Data.Tree;
          TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
          TTreeEntry(Node1.Data).Typ := etDir;
          fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
          Data.Tree.DataSet.Next;
        end;
    end;
end;
procedure TfProjectFrame.ProjectsStateChange(Sender: TObject);
var
  aEnabled: Boolean;
begin
  aEnabled := DataSet.CanEdit or DataSet.Changed;
  acSave.Enabled := aEnabled;
  acCancel.Enabled:= aEnabled;
end;

procedure TfProjectFrame.ReportGetValue(const ParName: String;
  var ParValue: Variant);
var
  aKey: String;
begin
  if Uppercase(ParName) = 'USER' then
    begin
      aKey := StringReplace(TProject(FDataSet).Tasks.FieldByName('USER').AsString,'=','',[rfReplaceAll]);
      if trim(FOwners.Values[aKey]) = '' then
        FOwners.Values[aKey] := TProject(FDataSet).Tasks.UserName;
      ParValue := FOwners.Values[aKey]
    end
  else if Uppercase(ParName) = 'OWNER' then
    begin
      aKey := StringReplace(TProject(FDataSet).Tasks.FieldByName('OWNER').AsString,'=','',[rfReplaceAll]);
      if trim(FOwners.Values[aKey]) = '' then
        FOwners.Values[aKey] := TProject(FDataSet).Tasks.OwnerName;
      ParValue := FOwners.Values[aKey]
    end;
end;

procedure TfProjectFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfProjectFrame.sePriorityChange(Sender: TObject);
begin
  if not DataSet.CanEdit then DataSet.DataSet.Edit;
  DataSet.FieldByName('GPRIORITY').AsInteger:=sePriority.Value;
end;

procedure TfProjectFrame.TProjectStateChange(Sender: TObject);
var
  bProject: TProject;
begin
  if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf(['P',DataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      bProject := Tproject.Create(nil,Data);
      bProject.Select(TProject(DataSet).Id.AsVariant);
      bproject.Open;
      bProject.Tasks.Open;
      if (Data.States.DataSet.FieldByName('ACTIVE').AsString='N') then
        begin
          if not bProject.Tasks.DataSet.Locate('ACTIVE','N',[]) then
            if MessageDlg(strInactiveStatus,mtInformation,[mbYes,mbNo],0) = mrYes then
              begin
                bProject.Tasks.First;
                while not bProject.Tasks.EOF do
                  begin
                    if bProject.Tasks.FieldByName('ACTIVE').AsString<>'N' then
                      begin
                        if not bProject.Tasks.CanEdit then
                          bProject.Tasks.DataSet.Edit;
                        bProject.Tasks.DataSet.FieldByName('ACTIVE').AsString:='N';
                        if bProject.Tasks.CanEdit then
                          bProject.Tasks.Post;
                      end;
                    bProject.Tasks.Next;
                  end;
              end;
        end
      else
        begin
          if bProject.Tasks.DataSet.Locate('ACTIVE','N',[]) then
            if MessageDlg(strActiveStatus,mtInformation,[mbYes,mbNo],0) = mrYes then
              begin
                bProject.Tasks.First;
                while not bProject.Tasks.EOF do
                  begin
                    if bProject.Tasks.FieldByName('ACTIVE').AsString<>'Y' then
                      begin
                        if not bProject.Tasks.CanEdit then
                          bProject.Tasks.DataSet.Edit;
                        bProject.Tasks.DataSet.FieldByName('ACTIVE').AsString:='Y';
                        if bProject.Tasks.CanEdit then
                          bProject.Tasks.Post;
                      end;
                    bProject.Tasks.Next;
                  end;
              end;
        end;
      bProject.Free;
    end;
end;

procedure TfProjectFrame.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='PROJECT';
  TfHistoryFrame(Sender).DataSet := TProject(FDataSet).History;
  TfHistoryFrame(Sender).SetRights(FEditable);
end;
procedure TfProjectFrame.AddPositions(Sender: TObject);
begin
  TProject(FDataSet).Positions.Open;
  TfProjectPositions(Sender).SetDataSet(TProject(FDataSet).Positions);
  TPrometInplaceFrame(Sender).SetRights(FEditable);
  TfProjectPositions(Sender).SetFocus;
end;
procedure TfProjectFrame.AddImages(Sender: TObject);
begin
  TfImageFrame(Sender).DataSet := TProject(FDataSet).Images;
  TProject(FDataSet).Images.Open;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfProjectFrame.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='PROJECT';
  TfLinkFrame(Sender).DataSet := TProject(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfProjectFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsInteger,'P',DataSet.FieldByName('SQL_ID').AsString,Null,Null);
    end;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfProjectFrame.AddTasks(Sender: TObject);
begin
  TfTaskFrame(Sender).BaseName:='PROJECTTASKS';
  TfTaskFrame(Sender).GridView.DefaultRows:='GLOBALWIDTH:%;COMPLETED:30;SUMMARY:200;STARTDATE:60;DUEDATE:60;USER:100;OWNER:100;PERCENT:40';
  TfTaskFrame(Sender).bDelegated1.Down:=True;
  TfTaskFrame(Sender).bDependencies1.Down:=True;
  TfTaskFrame(Sender).bFuture1.Down:=True;
  TfTaskFrame(Sender).UserID := Data.Users.Id.AsVariant;
  TfTaskFrame(Sender).IgnoreUser := True;
  TfTaskFrame(Sender).GridView.NumberField:='GPRIORITY';
  TfTaskFrame(Sender).GridView.SortField:='GPRIORITY';
  TfTaskFrame(Sender).DataSet := TProject(FDataSet).Tasks;
  TfTaskFrame(Sender).OnStartTime:=FOnStartTime;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfProjectFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;
procedure TfProjectFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      Application.ProcessMessages;
      DataSet.Delete;
      FDataSet.CascadicCancel;
//      Data.Commit(FConnection);
//      Data.StartTransaction(FConnection);
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;

procedure TfProjectFrame.acExportExecute(Sender: TObject);
var
  aStream: TFileStream;
  aFN: String;
begin
  Screen.Cursor:=crHourGlass;
  if dExport.Execute then
    begin
      aFN := dExport.FileName;
      if Pos('.',aFN)=0 then
        aFN := aFN+'.gan';
      aStream := TFileStream.Create(aFN,fmCreate);
      case dExport.FilterIndex of
      1:ExportGAN(aStream,TProject(DataSet));
      end;
      aStream.Free;
    end;
  Screen.Cursor:=crDefault;
end;

procedure TfProjectFrame.acGanttExecute(Sender: TObject);
var
  aType: Char;
  aFound: Boolean;
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).DataSet.CascadicCancel;
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
  aType := 'P';
  if not Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      Data.SetFilter(Data.States,'');
      aFound := Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]);
    end
  else aFound := True;
  fGanttView.Execute(TProject(DataSet),'',True,(Sender = bExecute2) and (not (aFound and (Data.States.FieldByName('ACTIVE').AsString='Y'))));
  if not Assigned(Self) then exit;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).DataSet.CascadicCancel;
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
end;
procedure TfProjectFrame.acGotoParentExecute(Sender: TObject);
var
  aParent: TProject;
begin
  aParent := TProject.Create(nil,Data);
  aParent.Select(TProject(DataSet).FieldByName('PARENT').AsVariant);
  aParent.Open;
  if aParent.Count>0 then
    Data.GotoLink(Data.BuildLink(aParent.DataSet));
  aParent.free;
end;

procedure TfProjectFrame.acImportExecute(Sender: TObject);
var
  aStream: TFileStream;
begin
  Screen.Cursor:=crHourGlass;
  if dImport.Execute then
    begin
      aStream := TFileStream.Create(UTF8ToSys(dImport.FileName),fmOpenRead);
      case dImport.FilterIndex of
      1:ImportGAN(aStream,TProject(DataSet));
      end;
      DoOpen;
      aStream.Free;
    end;
  Screen.Cursor:=crDefault;
end;

procedure TfProjectFrame.acInactiveGanttExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).DataSet.CascadicCancel;
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
  fGanttView.Execute(TProject(DataSet),'',True,True);
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).DataSet.CascadicCancel;
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
end;

procedure TfProjectFrame.acMoveOldTasksExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).GridView.BeginUpdate;
  fGanttView.MoveAndCalculate(TProject(DataSet));
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).GridView.EndUpdate;
end;

procedure TfProjectFrame.acPrintExecute(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  fSelectReport.Report := Report;
  fSelectReport.SetLanguage;
  if Supports(FDataSet, IBaseHistory, Hist) then
    History.DataSet := Hist.GetHistory.DataSet;
  Users.DataSet := Data.Users.DataSet;
  Tasks.DataSet := TProject(DataSet).Tasks.DataSet;
  MandantDetails.DataSet := Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  PList.DataSet := DataSet.DataSet;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      fSelectReport.ReportType := 'PRJ';
    end;
  fSelectReport.Showmodal;
end;
var
  bProject: TProject;
  aProject: TProject;
procedure ReplaceField(aField: TField; aOldValue: string; var aNewValue: string);
begin
  if aField.FieldName='COMPLETED' then aNewValue:='N';
  if aField.FieldName='SEEN' then aNewValue:='N';
  if aField.FieldName='PERCENT' then aNewValue:='0';
  if aField.FieldName='PROJECTID' then
    begin
      aNewValue:=bProject.Id.AsString;
    end;
  if aField.FieldName='NAME' then
    begin
      with aField.DataSet as IBaseManageDB do
        if TableName='PROJECTS' then
          aNewValue := aNewProjectName;
    end;
end;
procedure TfProjectFrame.acRestartExecute(Sender: TObject);
var
  cProject: TProject;
  aTask: TTask;
  aLink: String;
  bTask: TTask;
begin
  aProject := DataSet as TProject;
  aNewProjectName := InputBox(strProcessName,strEnterProcessName,aProject.Text.AsString);
  if aNewProjectName=aProject.Text.AsString then exit;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  Data.SetFilter(aProject.Tasks,Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(aProject.Id.AsString));
  bProject := TProject.Create(nil,Data);
  bProject.ImportFromXML(aProject.ExportToXML,False,@ReplaceField);
  cProject := TProject.Create(nil,Data);
  cProject.Select(bProject.Id.AsVariant);
  cProject.Open;
  cProject.Tasks.Open;
  bProject.Tasks.DataSet.Close;
  bProject.Tasks.Open;
  bProject.Tasks.DataSet.First;
  while not bProject.Tasks.DataSet.EOF do
    begin
      if aProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS',VarArrayOf([bProject.Tasks.FieldByName('SUMMARY').AsString,bProject.Tasks.FieldByName('WORKSTATUS').AsVariant]),[]) then
        begin
          aTask := TTask.Create(nil,Data);
          aTask.Select(aProject.Tasks.Id.AsVariant);
          aTask.Open;
          aTask.Dependencies.Open;
          with aTask.Dependencies.DataSet do
            begin
              First;
              while not EOF do
                begin
                  aLink := FieldByName('LINK').AsString;
                  if pos('{',aLink) > 0 then
                    aLink := copy(aLink,0,pos('{',aLink)-1);
                  aLink := copy(aLink,7,length(aLink));
                  if aProject.Tasks.GotoBookmark(StrToIntDef(aLink,0)) then
                    begin
                      if cProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS;PARENT',VarArrayOf([aProject.Tasks.FieldByName('SUMMARY').AsString,aProject.Tasks.FieldByName('WORKSTATUS').AsVariant,aProject.Tasks.FieldByName('PARENT').AsVariant]),[]) then
                        begin
                          bTask := TTask.Create(nil,Data);
                          bTask.Select(bProject.Tasks.Id.AsVariant);
                          bTask.Open;
                          bTask.Dependencies.Open;
                          while bTask.Dependencies.Locate('LINK',FieldByName('LINK').AsString,[]) do
                            bTask.Dependencies.Delete;
                          bTask.Dependencies.Add(Data.BuildLink(cProject.Tasks.DataSet));
                          bTask.Free;
                        end;
                    end;
                  Next;
                end;
            end;
          aTask.Free;
        end;
      bProject.Tasks.DataSet.Next;
    end;
  bProject.Tasks.DataSet.First;
  while not bProject.Tasks.DataSet.EOF do
    begin
      if not bProject.Tasks.FieldByName('PARENT').IsNull then
        begin
          if aProject.Tasks.GotoBookmark(bProject.Tasks.FieldByName('PARENT').AsVariant) then
            if cProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS',VarArrayOf([aProject.Tasks.FieldByName('SUMMARY').AsString,aProject.Tasks.FieldByName('WORKSTATUS').AsVariant]),[]) then
              begin
                if not bProject.Tasks.CanEdit then bProject.Tasks.DataSet.Edit;
                bProject.Tasks.FieldByName('PARENT').AsVariant:=cProject.Tasks.Id.AsVariant;
                bProject.Tasks.DataSet.Post;
              end;
        end;
      bProject.Tasks.DataSet.Next;
    end;
  cProject.Free;
  if not bProject.CanEdit then bProject.DataSet.Edit;
  bProject.FieldByName('PARENT').Clear;
  aLink := Data.BuildLink(bProject.DataSet);
  Data.GotoLink(aLink);
  bProject.Free;
  Screen.Cursor:=crDefault;
end;
procedure TfProjectFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfProjectFrame.acCancelExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
        begin
          TfTaskFrame(pcPages.ActivePage.Controls[0]).DataSet.CascadicCancel;
          TfTaskFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
        end;
      FDataSet.CascadicCancel;
    end;
end;

procedure TfProjectFrame.acCalculatePlanExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).GridView.BeginUpdate;
  fGanttView.Calculate(TProject(DataSet));
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).GridView.EndUpdate;
end;

procedure TfProjectFrame.acSaveExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
        begin
          TfTaskFrame(pcPages.ActivePage.Controls[0]).Post;
          TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
        end;
      FDataSet.CascadicPost;
    end;
end;
procedure TfProjectFrame.acSetTreeDirExecute(Sender: TObject);
begin
  if fMainTreeFrame.GetTreeEntry = -1 then exit;
  with DataSet.DataSet do
    begin
      Edit;
      FieldbyName('TREEENTRY').AsInteger:=fMainTreeFrame.GetTreeEntry;
      fMainTreeFrame.tvMain.Selected.Collapse(true);
    end;
end;

procedure TfProjectFrame.bProjectColorColorChanged(Sender: TObject);
begin
  DataSet.Edit;
  DataSet.FieldByName('COLOR').AsString:=ColorToString(bProjectColor.ButtonColor);
end;

procedure TfProjectFrame.cbStatusSelect(Sender: TObject);
var
  tmp: String;
begin
  tmp := copy(cbStatus.text,pos('(',cbStatus.text)+1,length(cbStatus.text));
  tmp := copy(tmp,0,pos(')',tmp)-1);
  if not FDataSet.CanEdit then FDataSet.DataSet.Edit;
  FDataSet.FieldByName('STATUS').AsString:=tmp;
  acSave.Execute;
  DoOpen;
end;

procedure TfProjectFrame.AddOverview(Sender: TObject);
begin
  TfProjectOverviewFrame(Sender).ParentProject:=TProject(fDataSet);
end;
procedure TfProjectFrame.RefreshFlow;
var
  aProj: TProject;
  aStat : string = '';
  aStart : TDateTime=0;
  aEnd : TDateTime=0;
  aColor: Graphics.TColor;
begin
  pStatus.Visible:=False;
  pcPages.Top:=96;
  exit;
  aProj := TProject.Create(nil,Data);
  aProj.Select(DataSet.Id.AsVariant);
  aProj.Open;
  Data.SetFilter(aProj.Tasks,Data.QuoteField('CLASS')+'='+Data.QuoteValue('M'),0,'DUEDATE','ASC');
  FProjectFlow.StartDate:=trunc(Now());
  FProjectFlow.EndDate:=0;
  while not aProj.Tasks.EOF do
    begin
      if aProj.Tasks.FieldByName('WORKSTATUS').AsString<>aStat then
        begin
          aStat:=aProj.Tasks.FieldByName('WORKSTATUS').AsString;
          aColor := clYellow;
          if Data.States.DataSet.Locate('STATUS',aStat,[]) then
            StringToColorDef(Data.States.DataSet.FieldByName('COLOR').AsString,aColor);
          if aStart = 0 then
            begin
              aStart := aProj.Tasks.FieldByName('DUEDATE').AsDateTime;
              if FProjectFlow.StartDate=trunc(Now()) then
                FProjectFlow.StartDate:=aStart;
            end
          else if (aProj.Tasks.FieldByName('DUEDATE').AsDateTime<>0) and (aStart>0) then
            begin
              aEnd := aProj.Tasks.FieldByName('DUEDATE').AsDateTime;
              if FProjectFlow.EndDate<aEnd then
                FProjectFlow.EndDate:=aEnd;
              FProjectFlow.AddSection(aStart,aEnd,aStat,aColor);
              aStart := aEnd;
              aEnd := 0;
              aStart := 0;
            end;
        end;
      aProj.Tasks.Next;
    end;
  if FProjectFlow.StartDate=trunc(Now()) then
    begin
      FProjectFlow.EndDate:=trunc(Now())+30;
    end;
  FProjectFlow.StartDate:=FProjectFlow.StartDate-10;
  FProjectFlow.EndDate:=FProjectFlow.EndDate+10;
  aProj.Free;
end;
procedure TfProjectFrame.eNameChange(Sender: TObject);
begin
  if eName.Lines.Count > 0 then
    TabCaption := eName.Lines[0];
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;

procedure TfProjectFrame.eManagerButtonClick(Sender: TObject);
var
  i : Integer = 0;
begin
  fSearch.SetLanguage;
  while i < fSearch.cbSearchType.Count do
    begin
      if fSearch.cbSearchType.Items[i] <> strUsers then
        fSearch.cbSearchType.Items.Delete(i)
      else
        inc(i);
    end;
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItemL;
  fSearch.Execute(True,'TASKSL',strSearchFromProjects);
  fSearch.SetLanguage;
end;

procedure TfProjectFrame.eManagerExit(Sender: TObject);
begin
  if trim(eParent.Text)='' then
    begin
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      DataSet.FieldByName('PMANAGER').Clear;
    end;
end;

procedure TfProjectFrame.eParentButtonClick(Sender: TObject);
var
  i : Integer = 0;
begin
  fSearch.SetLanguage;
  while i < fSearch.cbSearchType.Count do
    begin
      if fSearch.cbSearchType.Items[i] <> strProjects then
        fSearch.cbSearchType.Items.Delete(i)
      else
        inc(i);
    end;
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'TASKSP',strSearchFromProjects);
  fSearch.SetLanguage;
end;

procedure TfProjectFrame.eParentExit(Sender: TObject);
begin
  if trim(eParent.Text)='' then
    begin
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      DataSet.FieldByName('PARENT').Clear;
    end;
end;
function TfProjectFrame.fSearchOpenItem(aLink: string): Boolean;
var
  aParent: TProject;
begin
  aParent := TProject.Create(nil,Data);
  aParent.SelectFromLink(aLink);
  aParent.Open;
  if aParent.Count>0 then
    begin
      eParent.Text:=aParent.FieldByName('NAME').AsString;
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      dataSet.FieldByName('PARENT').AsVariant:=aParent.Id.AsVariant;
    end;
  aParent.free;
end;

function TfProjectFrame.fSearchOpenItemL(aLink: string): Boolean;
var
  aParent: TUser;
begin
  aParent := TUser.Create(nil,Data);
  aParent.SelectFromLink(aLink);
  aParent.Open;
  if aParent.Count>0 then
    begin
      eManager.Text:=aParent.FieldByName('NAME').AsString;
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      dataSet.FieldByName('PMANAGER').AsString:=aParent.FieldByName('ACCOUNTNO').AsString;
    end;
  aParent.free;
end;

procedure TfProjectFrame.pcPagesChange(Sender: TObject);
begin

end;

procedure TfProjectFrame.DoOpen;
var
  s: TStream;
  GraphExt: String;
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  aParent: TProject;
  aSubProject: TProject;
  aType: Char;
  tmp: String;
  aFound: Boolean;
  aTasks: TfTaskFrame;
  aParentU: TUser;
  aWiki: TWikiList;
  aWikiPage: TfWikiFrame;
  aWikiIdx: Integer;
  aID: String;
  Inserted: Boolean;
begin
  SetRights;
  pcPages.ClearTabClasses;
  pcPages.CloseAll;
  pcPages.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  TProject(DataSet).History.Open;
  if TProject(DataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  pcPages.AddTabClass(TfImageFrame,strImages,@AddImages);
  if TProject(DataSet).FieldByName('PARENT').IsNull then
    eParent.Text:=strNoParent
  else
    begin
      aParent := TProject.Create(nil,Data);
      aParent.Select(TProject(DataSet).FieldByName('PARENT').AsVariant);
      aParent.Open;
      if aParent.Count>0 then
        eParent.Text:=aParent.Text.AsString;
      aParent.free;
    end;
  if TProject(dataSet).FieldByName('PMANAGER').IsNull then
    eManager.Text:=''
  else
    begin
      aParentU := TUser.Create(nil,Data);
      aParentU.SelectByAccountno(dataSet.FieldByName('PMANAGER').AsString);
      aParentU.Open;
      if aParentU.Count>0 then
        eManager.Text:=aParentU.FieldByName('NAME').AsString;
      aParentU.free;
    end;

  cbCategory.Items.Clear;
  aType := 'P';
  Data.Categories.CreateTable;
  Data.SetFilter(Data.Categories,Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType));
  Data.Categories.First;
  while not Data.Categories.EOF do
    begin
      if Data.Categories.FieldByName('ACTIVE').AsString<>'N' then
        cbCategory.Items.Add(Data.Categories.FieldByName('NAME').AsString);
      Data.Categories.DataSet.Next;
    end;

  cbStatus.Items.Clear;
  cbStatus.Text := '';
  if DataSet.FieldByName('COLOR').IsNull then
    bProjectColor.ButtonColor:=clBlue
  else bProjectColor.ButtonColor:=StringToColor(DataSet.FieldByName('COLOR').AsString);
  aType := 'P';
  if not Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      Data.SetFilter(Data.States,'');
      aFound := Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]);
    end
  else aFound := True;
  if aFound then
    begin
      cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
      cbStatus.Text := Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')';
    end
  else cbStatus.Text:=FDataSet.FieldByName('STATUS').AsString;
  tmp := trim(Data.States.FieldByName('DERIVATIVE').AsString);
  if (length(tmp) = 0) or (tmp[length(tmp)] <> ';') then
    tmp := tmp+';';
  if tmp <> ';' then
    begin
      while pos(';',tmp) > 0 do
        begin
          if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,copy(tmp,0,pos(';',tmp)-1)]),[loCaseInsensitive]) then
            cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
        end;
    end
  else
    begin
      Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType));
      with Data.States.DataSet do
        begin
          First;
          while not eof do
            begin
              if cbStatus.Items.IndexOf(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')') = -1 then
                cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
              Next;
            end;
        end;
    end;

  pNav2.Visible := TProject(DataSet).FieldByName('TYPE').AsString = 'C';
  if not TProject(DataSet).Images.DataSet.Active then
    TProject(DataSet).Images.DataSet.Open;
  s := TProject(DataSet).Images.DataSet.CreateBlobStream(TProject(DataSet).Images.FieldByName('IMAGE'),bmRead);
  if (S=Nil) or (s.Size = 0) then
    begin
      iProject.Picture.Clear;
    end
  else
    begin
      GraphExt :=  s.ReadAnsiString;
      iProject.Picture.LoadFromStreamWithFileExt(s,GraphExt);
    end;
  s.Free;
  if TProject(DataSet).Images.Count > 0 then
    pcPages.AddTab(TfImageFrame.Create(Self),False);
  TProject(DataSet).Images.DataSet.Close;
  pcPages.AddTabClass(TfProjectOverviewFrame,strOverview,@AddOverview);
  aSubProject := TProject.Create(nil,Data);
  aSubProject.SelectFromParent(fDataSet.Id.AsVariant);
  aSubProject.Open;
  if aSubProject.Count>0 then
    begin
      pcPages.AddTab(TfProjectOverviewFrame.Create(Self),True)
    end;
  aSubProject.Free;
  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TProject(DataSet).Links.Open;
  if TProject(DataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.Create(Self,Data,DataSet.Connection);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsInteger,'P',DataSet.FieldByName('SQL_ID').AsString,Null,Null);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          aDocFrame.DataSet := aDocuments;
          pcPages.AddTab(aDocFrame,False);
          aDocFrame.BaseElement:=DataSet;
        end;
    end;
  pcPages.AddTabClass(TfProjectPositions,strCosts,@AddPositions);
  TProject(DataSet).Positions.Open;
  if TProject(DataSet).Positions.Count > 0 then
    pcPages.AddTab(TfProjectPositions.Create(Self),False);
  sePriority.OnChange:=nil;
  sePriority.Value:=DataSet.FieldByName('GPRIORITY').AsInteger;
  sePriority.OnChange:=@sePriorityChange;
  eName.SetFocus;
  if Data.Users.Rights.Right('OPTIONS') > RIGHT_READ then
    begin
      pcPages.CanHaveCustomTabs(@TBaseVisualApplication(Application).OnAddCustomTab);
    end;
  with Application as TBaseVisualApplication do
    AddTabClasses('PRJ',pcPages);
  with Application as TBaseVisualApplication do
    AddTabs(pcPages);
  SetRights;
  RefreshFlow;
  pcPages.AddTabClass(TfTaskFrame,strTasks,@AddTasks);
  Inserted := DataSet.State=dsInsert;
  TProject(DataSet).Tasks.Open;
  if (TProject(DataSet).Tasks.Count > 0) or Inserted then
    begin
      aTasks := TfTaskFrame.Create(Self);
      pcPages.AddTab(aTasks,True);
      aTasks.GridView.GotoRowNumber(aTasks.GridView.gList.FixedRows);
    end;
  if Inserted or (TProject(DataSet).Tasks.Count = 0) then
    pcPages.PageIndex:=0;
  if DataSet.State<> dsInsert then
    begin
      aWiki := TWikiList.Create(nil,Data);
      if aWiki.FindWikiFolder('Promet-ERP-Help/forms/'+Self.ClassName+'/') then
        begin
          while not aWiki.EOF do
            begin
              aWikiPage := TfWikiFrame.Create(Self);
              aID := IntToStr(Int64(DataSet.Id.AsVariant));
              aWikiPage.Variables.Values['SQL_ID'] := aID;
              aWikiPage.Variables.Values['ID'] := TBaseDbList(DataSet).Number.AsString;
              aWikiPage.Variables.Values['TEXT'] := TBaseDbList(DataSet).Text.AsString;
              aWikiIdx := -1;
              if Assigned(TBaseDbList(DataSet).Status) then
                aWikiPage.Variables.Values['STATUS'] := TBaseDbList(DataSet).Status.AsString;
              if aWikiPage.OpenWikiPage('Promet-ERP-Help/forms/'+Self.ClassName+'/'+aWiki.Text.AsString) then
                aWikiIdx := pcPages.AddTab(aWikiPage,False,aWiki.FieldByName('CAPTION').AsString)
              else aWikiPage.Free;
              if aWiki.FieldByName('CAPTION').AsString = strOverview then
                begin
                  pcPages.Pages[aWikiIdx+1].PageIndex:=0;
                  pcPages.PageIndex:=0;
                end;
              aWikiPage.LeftBar:=True;
              aWiki.Next;
            end;
        end;
      aWiki.Free;
    end;
end;
function TfProjectFrame.SetRights: Boolean;
begin
  FEditable := ((Data.Users.Rights.Right('PROJECTS') > RIGHT_READ));
  Result := FEditable;
  acDelete.Enabled:=FEditable and (Data.Users.Rights.Right('PROJECTS') > RIGHT_WRITE);
  acPaste.Enabled:=FEditable;
  acRights.Enabled:=Data.Users.Rights.Right('PROJECTS') >= RIGHT_PERMIT;
  eNumber.Enabled:=FEditable;
  cbStatus.Enabled:=FEditable;
  cbType.Enabled:=FEditable;
  eParent.ReadOnly:=not FEditable;
  eName.Enabled:=FEditable;

  pComponents.Enabled := FEditable;
  mInfo.ReadOnly:=not FEditable;
end;
procedure TfProjectFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  acSave.Enabled:=False;
  acCancel.Enabled:=False;
  if not Assigned(AValue) then exit;
  Projects.DataSet := AValue.DataSet;
  Tproject(AValue).OnStateChange:=@TProjectStateChange;
end;
constructor TfProjectFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwners := TStringList.Create;
  {
  FProjectFlow := TProjectFlow.Create(Self);
  FprojectFlow.Parent:=pStatus;
  FProjectFlow.Align:=alClient;
  FProjectFlow.Font.Height:=8;
  }
  FProjectFlow := nil;
end;
destructor TfProjectFrame.Destroy;
begin
  FOwners.Destroy;
  if Assigned(FConnection) then
    begin
      CloseConnection(acSave.Enabled);
      DataSet.Destroy;
      DataSet := nil;
      FreeAndNil(FConnection);
    end;
  //FreeAndNil(FProjectFlow);
  inherited Destroy;
end;
function TfProjectFrame.OpenFromLink(aLink: string) : Boolean;
begin
  Result := False;
  if not ((copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS')
  or (copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS.ID')) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
//  Data.StartTransaction(FConnection);
  DataSet := TProject.Create(Self,Data,FConnection);
  DataSet.OnChange:=@ProjectsStateChange;
  if copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS.ID' then
    Data.SetFilter(FDataSet,Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1)
  else
    Data.SetFilter(FDataSet,Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1);
  if FDataSet.Count > 0 then
    begin
      TabCaption := TProject(FDataSet).Text.AsString;
      DoOpen;
      Result := True;
    end;
end;
procedure TfProjectFrame.New;
begin
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
//  Data.StartTransaction(FConnection);
  TabCaption := strNewProject;
  DataSet := TProject.Create(Self,Data,FConnection);
  DataSet.OnChange:=@ProjectsStateChange;
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
end;
procedure TfProjectFrame.SetLanguage;
begin
end;
procedure TfProjectFrame.ShowFrame;
begin
  inherited ShowFrame;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).ShowFrame;
end;

procedure TfProjectFrame.FrameAdded;
begin
  inherited FrameAdded;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).FrameAdded;
end;

procedure TfProjectFrame.GotoTask(aLink: string);
var
  aTask: TTask;
begin
  aTask := TTask.Create(nil,Data);
  aTask.SelectFromLink(aLink);
  aTask.Open;
  if aTask.Count>0 then
    begin

    end;
  aTask.Free;
end;

end.

