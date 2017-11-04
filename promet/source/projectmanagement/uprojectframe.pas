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
  Dialogs, Spin, EditBtn,variants,uProjectFlow,uTasks,Graphics,uMeasurement,uBaseDatasetInterfaces;
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
    acCalculate: TAction;
    acAddImage: TAction;
    acPasteImage: TAction;
    acScreenshot: TAction;
    acRegorganize: TAction;
    acDeleteThumb: TAction;
    ActionList1: TActionList;
    bAssignTree: TSpeedButton;
    bChangeNumber: TSpeedButton;
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
    cbClass: TExtDBCombobox;
    cbStatus: TComboBox;
    cbType: TExtDBCombobox;
    cbCategory: TExtDBCombobox;
    bProjectColor: TColorButton;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBCheckBox3: TDBCheckBox;
    DBCheckBox4: TDBCheckBox;
    eWish: TDBZVDateTimePicker;
    eCreated: TDBZVDateTimePicker;
    eInitiator: TEditButton;
    eParent: TEditButton;
    eManager: TEditButton;
    iProject: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    lCreated: TLabel;
    Label15: TLabel;
    lWish: TLabel;
    Label9: TLabel;
    lVAT1: TLabel;
    lVAT2: TLabel;
    MandantDetails: TDatasource;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    pNav2: TPanel;
    pNav3: TPanel;
    PopupMenu1: TPopupMenu;
    pPreviewImage: TPanel;
    pStatus: TPanel;
    PTasks: TfrDBDataSet;
    dExport: TSaveDialog;
    sbAddImage: TSpeedButton;
    sbAddImage1: TSpeedButton;
    sbClipboardToImage: TSpeedButton;
    sbClipboardToImage1: TSpeedButton;
    sePriority: TSpinEdit;
    sARed: TShape;
    sAYellow: TShape;
    sAGreen: TShape;
    Shape4: TShape;
    Tasks: TDatasource;
    DBZVDateTimePicker1: TDBZVDateTimePicker;
    eTarget: TDBZVDateTimePicker;
    dImport: TOpenDialog;
    eNumber: TDBEdit;
    History: TDatasource;
    lTarget: TLabel;
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
    procedure acDeleteThumbExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acCalculatePlanExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acGanttExecute(Sender: TObject);
    procedure acGotoParentExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acInactiveGanttExecute(Sender: TObject);
    procedure acCalculateExecute(Sender: TObject);
    procedure acPasteImageExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRegorganizeExecute(Sender: TObject);
    procedure acRestartExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure bChangeNumberClick(Sender: TObject);
    procedure bProjectColorColorChanged(Sender: TObject);
    procedure cbCategorySelect(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure eInitiatorButtonClick(Sender: TObject);
    procedure eInitiatorEnter(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eManagerButtonClick(Sender: TObject);
    procedure eManagerExit(Sender: TObject);
    procedure eParentButtonClick(Sender: TObject);
    procedure eParentExit(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchOpenItemL(aLink: string): Boolean;
    function fSearchOpenItemLI(aLink: string): Boolean;
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
    FMeasurement: TMeasurement;
    procedure AddMeasurement(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure AddPositions(Sender: TObject);
    procedure AddImages(Sender: TObject);
    procedure AddLinks(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    procedure AddTasks(Sender: TObject);
    procedure AddOverview(Sender: TObject);
    procedure AddFinance(Sender: TObject);
    procedure RefreshFlow;
    procedure ReplaceField(aField: TField; aOldValue: string;
      var aNewValue: string);
  protected
    Reopen : Boolean;
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
  uGanttView,uWikiFrame,uWiki,ufinance,uthumbnails,Clipbrd,uscreenshotmain,
  uBaseApplication,umeasurements,uscriptimport;
{$R *.lfm}
resourcestring
  strNoParent                     = '<kein Vorfahr>';
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
      if Data.Users.Rights.Right('RESOURCEVIEW')>RIGHT_READ then
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etAction;
          TTreeEntry(Node1.Data).Action := aAction;
        end;
      Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('P')+'))';
      Data.Tree.DataSet.Filtered:=True;
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
      Data.Tree.DataSet.Filtered:=False;
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
      bProject := Tproject.Create(nil);
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

procedure TfProjectFrame.AddMeasurement(Sender: TObject);
begin
  TfMeasurementFrame(Sender).DataSet := FMeasurement;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
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
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'P',DataSet.FieldByName('SQL_ID').AsString,Null,Null,0);
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
      FDataSet.CascadicCancel;
      DataSet.Delete;
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;

procedure TfProjectFrame.acExportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icExport,'P',FDataSet) then
    DataSet.DataSet.Refresh;
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
  aParent := TProject.Create(nil);
  aParent.Select(TProject(DataSet).FieldByName('PARENT').AsVariant);
  aParent.Open;
  if aParent.Count>0 then
    Data.GotoLink(Data.BuildLink(aParent.DataSet));
  aParent.free;
end;

procedure TfProjectFrame.acImportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icImport,'P',FDataSet) then
    DataSet.DataSet.Refresh;
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

procedure TfProjectFrame.acCalculateExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).GridView.BeginUpdate;
  fGanttView.Project := TProject(DataSet);
  fGanttView.acRefreshWizard.Execute;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
    end;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).GridView.EndUpdate;
end;
procedure TfProjectFrame.acPasteImageExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  if Clipboard.HasPictureFormat then
    begin
      pcPages.AddTab(TfImageFrame.Create(Self),False);
      aSheet := pcPages.GetTab(TfImageFrame);
      if Assigned(aSheet) then
        begin
          Application.ProcessMessages;
          TfImageFrame(aSheet.Controls[0]).acPaste.Execute;
          TfImageFrame(aSheet.Controls[0]).DataSet.Post;
          aThumbnails := TThumbnails.Create(nil);
          aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
          aThumbnails.Open;
          while aThumbnails.Count>0 do
            aThumbnails.Delete;
          TProject(DataSet).GenerateThumbnail;
          aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
          aThumbnails.Open;
          if aThumbnails.Count>0 then
            begin
              aStream := TMemoryStream.Create;
              Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
              aStream.Position:=0;
              iProject.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
              aStream.Free;
              acPasteImage.Visible:=False;
              acAddImage.Visible:=False;
              acScreenshot.Visible:=False;
              acDeleteThumb.Visible:=True;
            end
          else
            begin
              iProject.Picture.Clear;
              acPasteImage.Visible:=True;
              acAddImage.Visible:=True;
              acScreenshot.Visible:=True;
            end;
          aThumbnails.Free;
        end;
    end;
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

procedure TfProjectFrame.acRegorganizeExecute(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  TProject(DataSet).Reorganize;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
  Screen.Cursor:=crDefault;
end;

var
  bProject: TProject;
  aProject: TProject;
procedure TfProjectFrame.ReplaceField(aField: TField; aOldValue: string; var aNewValue: string);
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
  aXML: String;
begin
  aProject := DataSet as TProject;
  aNewProjectName := InputBox(strProcessName,strEnterProcessName,aProject.Text.AsString);
  if aNewProjectName=aProject.Text.AsString then exit;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  Data.SetFilter(aProject.Tasks,Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(aProject.Id.AsString));
  bProject := TProject.Create(nil);
  aXML := aProject.ExportToXML;
  bProject.ImportFromXML(aXML,False,@ReplaceField);
  cProject := TProject.Create(nil);
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
          aTask := TTask.Create(nil);
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
                  if aProject.Tasks.GotoBookmark(StrToInt64Def(aLink,0)) then
                    begin
                      if cProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS;PARENT',VarArrayOf([aProject.Tasks.FieldByName('SUMMARY').AsString,aProject.Tasks.FieldByName('WORKSTATUS').AsVariant,aProject.Tasks.FieldByName('PARENT').AsVariant]),[]) then
                        begin
                          bTask := TTask.Create(nil);
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
  bProject.Reorganize;
  aLink := Data.BuildLink(bProject.DataSet);
  Data.GotoLink(aLink);
  bProject.Free;
  Screen.Cursor:=crDefault;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    TfTaskFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
end;
procedure TfProjectFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfProjectFrame.acCancelExecute(Sender: TObject);
begin
  Abort;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).DataSet.CascadicCancel;
      TfTaskFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
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
procedure TfProjectFrame.acScreenshotExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Application.ProcessMessages;
  Application.CreateForm(TfScreenshot,fScreenshot);
  with BaseApplication as IBaseApplication do
    fScreenshot.SaveTo:=AppendPathDelim(GetInternalTempDir)+'screenshot.jpg';
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  if DataSet.State=dsInsert then
    begin
      DataSet.Post;
      DataSet.Edit;
    end;
  pcPages.AddTab(TfImageFrame.Create(Self),False);
  aSheet := pcPages.GetTab(TfImageFrame);
  if Assigned(aSheet) then
    begin
      Application.ProcessMessages;
      with TfImageFrame(aSheet.Controls[0]) do
        begin
          if not DataSet.CanEdit then
            DataSet.Insert;
          with BaseApplication as IBaseApplication do
            iPreview.Picture.LoadFromFile(AppendPathDelim(GetInternalTempDir)+'screenshot.jpg');
          DataSet.Post;
        end;
      aThumbnails := TThumbnails.Create(nil);
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      while aThumbnails.Count>0 do
        aThumbnails.Delete;
      TProject(DataSet).GenerateThumbnail;
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      if aThumbnails.Count>0 then
        begin
          aStream := TMemoryStream.Create;
          Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
          aStream.Position:=0;
          iProject.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
          aStream.Free;
          acPasteImage.Visible:=False;
          acAddImage.Visible:=False;
          acScreenshot.Visible:=False;
          acDeleteThumb.Visible:=True;
        end
      else
        begin
          iProject.Picture.Clear;
          acPasteImage.Visible:=True;
          acAddImage.Visible:=True;
          acScreenshot.Visible:=True;
        end;
      aThumbnails.Free;
    end;

  Application.MainForm.Show;
end;

procedure TfProjectFrame.acDeleteThumbExecute(Sender: TObject);
var
  aThumbnails: TThumbnails;
begin
  aThumbnails := TThumbnails.Create(nil);
  aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
  aThumbnails.Open;
  while aThumbnails.Count>0 do
    aThumbnails.Delete;
  aThumbnails.Free;
  iProject.Picture.Clear;
  acDeleteThumb.Visible:=False;
  acScreenshot.Visible:=True;
  acPasteImage.Visible:=True;
  acAddImage.Visible:=True;
end;

procedure TfProjectFrame.acSaveExecute(Sender: TObject);
begin
  Save;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfTaskFrame) then
    begin
      TfTaskFrame(pcPages.ActivePage.Controls[0]).Post;
      TfTaskFrame(pcPages.ActivePage.Controls[0]).acRefresh.Execute;
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

procedure TfProjectFrame.bChangeNumberClick(Sender: TObject);
var
  str: String;
begin
  str := DataSet.FieldByName('ID').AsString;
  if InputQuery(strChangeNumer,strnewNumber,str) and (str <> DataSet.FieldByName('ID').AsString) then
    begin
      with DataSet.DataSet do
        begin
          Edit;
          FieldbyName('ID').AsString:=str;
        end;
    end;
end;

procedure TfProjectFrame.bProjectColorColorChanged(Sender: TObject);
begin
  if DataSet.DataSet.ControlsDisabled then exit;
  DataSet.Edit;
  DataSet.FieldByName('COLOR').AsString:=ColorToString(bProjectColor.ButtonColor);
end;

procedure TfProjectFrame.cbCategorySelect(Sender: TObject);
begin
  Data.Categories.Open;
  Data.Categories.DataSet.Filter:=Data.QuoteField('TYPE')+'='+Data.QuoteValue('P');
  Data.Categories.DataSet.Filtered:=True;
  if Data.Categories.Locate('NAME',cbCategory.Text,[loCaseInsensitive]) then
    if not Data.Categories.FieldByName('COLOR').IsNull then
      begin
        DataSet.Edit;
        DataSet.FieldByName('COLOR').AsString:=Data.Categories.FieldByName('COLOR').AsString;
      end;
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
  Reopen := True;
  DoOpen;
end;

procedure TfProjectFrame.eInitiatorButtonClick(Sender: TObject);
begin
  fSearch.AllowSearchTypes(strUsers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItemLI;
  fSearch.Execute(True,'TASKSL',strSearchFromProjects);
  fSearch.SetLanguage;
end;

procedure TfProjectFrame.eInitiatorEnter(Sender: TObject);
begin
  if trim(eParent.Text)='' then
    begin
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      DataSet.FieldByName('PINITED').Clear;
    end;
end;

procedure TfProjectFrame.AddOverview(Sender: TObject);
begin
  TfObjectStructureFrame(Sender).ParentObject:=TBaseDbList(fDataSet);
end;

procedure TfProjectFrame.AddFinance(Sender: TObject);
begin
  TfFinance(Sender).DataSet := FDataSet;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
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
  aProj := TProject.Create(nil);
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
begin
  fSearch.AllowSearchTypes(strUsers);
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
  fSearch.AllowSearchTypes(strProjects);
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
  aParent := TProject.Create(nil);
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
  aParent := TUser.Create(nil);
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

function TfProjectFrame.fSearchOpenItemLI(aLink: string): Boolean;
var
  aParent: TUser;
begin
  aParent := TUser.Create(nil);
  aParent.SelectFromLink(aLink);
  aParent.Open;
  if aParent.Count>0 then
    begin
      eInitiator.Text:=aParent.FieldByName('NAME').AsString;
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      dataSet.FieldByName('PINITED').AsString:=aParent.FieldByName('ACCOUNTNO').AsString;
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
  aType: string;
  tmp: String;
  aFound: Boolean;
  aTasks: TfTaskFrame;
  aParentU: TUser;
  aWiki: TWikiList;
  aWikiPage: TfWikiFrame;
  aWikiIdx: Integer;
  aID: String;
  Inserted: Boolean;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  DataSet.DataSet.DisableControls;
  try
  TProject(DataSet).OpenItem;
  FEditable := ((Data.Users.Rights.Right('PROJECTS') > RIGHT_READ));

  TProject(DataSet).History.Open;
  pcPages.NewFrame(TfHistoryFrame,TProject(DataSet).History.Count > 0,strHistory,@AddHistory);

  if TProject(DataSet).FieldByName('PARENT').IsNull then
    eParent.Text:=strNoParent
  else
    begin
      aParent := TProject.Create(nil);
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
      aParentU := TUser.Create(nil);
      aParentU.SelectByAccountno(dataSet.FieldByName('PMANAGER').AsString);
      aParentU.Open;
      if aParentU.Count>0 then
        eManager.Text:=aParentU.FieldByName('NAME').AsString;
      aParentU.free;
    end;
  if TProject(dataSet).FieldByName('PINITED').IsNull then
    eInitiator.Text:=''
  else
    begin
      aParentU := TUser.Create(nil);
      aParentU.SelectByAccountno(dataSet.FieldByName('PINITED').AsString);
      aParentU.Open;
      if aParentU.Count>0 then
        eInitiator.Text:=aParentU.FieldByName('NAME').AsString;
      aParentU.free;
    end;

  cbCategory.Items.Clear;
  aType := 'P';
  Data.Categories.Open;
  Data.Categories.DataSet.Filter:=Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType);
  Data.Categories.DataSet.Filtered:=True;
  Data.Categories.First;
  while not Data.Categories.EOF do
    begin
      if Data.Categories.FieldByName('ACTIVE').AsString<>'N' then
        cbCategory.Items.Add(Data.Categories.FieldByName('NAME').AsString);
      Data.Categories.DataSet.Next;
    end;
  cbClass.Items.Clear;
  aType := 'K';
  Data.Categories.Open;
  Data.Categories.DataSet.Filter:=Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType);
  Data.Categories.DataSet.Filtered:=True;
  Data.Categories.First;
  while not Data.Categories.EOF do
    begin
      if Data.Categories.FieldByName('ACTIVE').AsString<>'N' then
        cbClass.Items.Add(Data.Categories.FieldByName('NAME').AsString);
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

  aThumbnails := TThumbnails.Create(nil);
  aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
  aThumbnails.Open;
  if aThumbnails.Count>0 then
    begin
      aStream := TMemoryStream.Create;
      Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
      aStream.Position:=0;
      iProject.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
      aStream.Free;
      acPasteImage.Visible:=False;
      acAddImage.Visible:=False;
      acScreenshot.Visible:=False;
      acDeleteThumb.Visible:=True;
    end
  else
    begin
      iProject.Picture.Clear;
      acPasteImage.Visible:=True;
      acAddImage.Visible:=True;
      acScreenshot.Visible:=True;
      acDeleteThumb.Visible:=False;
    end;
  pcPages.NewFrame(TfImageFrame,(FDataSet.State = dsInsert) or (aThumbnails.Count > 0),strImages,@AddImages);
  aThumbnails.Free;

  aSubProject := TProject.Create(nil);
  aSubProject.SelectFromParent(fDataSet.Id.AsVariant);
  aSubProject.Open;
  pcPages.NewFrame(TfObjectStructureFrame,aSubProject.Count>0,strStructure,@AddOverview);
  aSubProject.Free;

  TProject(DataSet).Links.Open;
  pcPages.NewFrame(TfLinkFrame,(TProject(DataSet).Links.Count > 0),strLinks,@AddLinks);

  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if Assigned(pcPages.GetTab(TfDocumentFrame)) then
    pcPages.GetTab(TfDocumentFrame).Free;
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) and (not Assigned(pcPages.GetTab(TfDocumentFrame))) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data,DataSet.Connection);
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

  TProject(DataSet).Positions.Open;
  pcPages.NewFrame(TfProjectPositions,TProject(DataSet).Positions.Count > 0,strCosts,@AddPositions);

  sePriority.OnChange:=nil;
  sePriority.Value:=DataSet.FieldByName('GPRIORITY').AsInteger;
  sePriority.OnChange:=@sePriorityChange;
  eName.SetFocus;
  if Data.Users.Rights.Right('OPTIONS') > RIGHT_READ then
    begin
      pcPages.CanHaveCustomTabs(@TBaseVisualApplication(Application).OnAddCustomTab);
    end;

  pcPages.NewFrame(TfFinance,(not DataSet.FieldByName('COSTCENTRE').IsNull)
                          or (not DataSet.FieldByName('ACCOUNT').IsNull)
                          or (not DataSet.FieldByName('ACCOUNTINGINFO').IsNull),strFinance,@AddFinance);

  with Application as TBaseVisualApplication do
    AddTabClasses('PRJ',pcPages);
  with Application as TBaseVisualApplication do
    AddTabs(pcPages);
  SetRights;
  RefreshFlow;
  pcPages.AddTabClass(TfMeasurementFrame,strMeasurement,@AddMeasurement);
  if not Reopen then
    begin
      FreeAndNil(FMeasurement);
      try
        FMeasurement := TMeasurement.CreateEx(nil,Data,DataSet.Connection,DataSet.DataSet);
        FMeasurement.CreateTable;
        FMeasurement.Open;
        if FMeasurement.Count>0 then
          pcPages.AddTab(TfMeasurementFrame.Create(Self),False);
      except
      end;
    end;

  pcPages.AddTabClass(TfTaskFrame,strTasks,@AddTasks);
  if not Assigned(pcPages.GetTab(TfTaskFrame)) then
    begin
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
    end;
  if (DataSet.State<> dsInsert) and (DataSet.Id.AsVariant<>Null) and (not Assigned(pcPages.GetTab(TfWikiFrame))) then
    begin
      aWiki := TWikiList.Create(nil);
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
              if aWikiPage.OpenWikiPage('Promet-ERP-Help/forms/'+Self.ClassName+'/'+aWiki.FieldByName('NAME').AsString) then
                begin
                  aWikiIdx := pcPages.AddTab(aWikiPage,False,aWiki.FieldByName('CAPTION').AsString);
                  aWikiPage.SetRights(FEditable);
                  aWikiPage.LeftBar:=True;
                end
              else aWikiPage.Free;
              if (aWiki.FieldByName('CAPTION').AsString = strOverview) or (Uppercase(aWiki.FieldByName('NAME').AsString)='OVERVIEW')  then
                begin
                  pcPages.Pages[aWikiIdx+1].PageIndex:=0;
                  pcPages.PageIndex:=0;
                end;
              aWiki.Next;
            end;
        end;
      aWiki.Free;
    end;
  SetRights;
  if HasHelp then AddHelp(Self);
  finally
    DataSet.DataSet.EnableControls;
  end;
  pcPages.Change;
  Reopen := False;
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
  Reopen := False;
  {
  FProjectFlow := TProjectFlow.Create(Self);
  FprojectFlow.Parent:=pStatus;
  FProjectFlow.Align:=alClient;
  FProjectFlow.Font.Height:=8;
  }
  FProjectFlow := nil;
  {$ifdef DARWIN}
  cbStatus.Style:=csDropdown;
  {$endif}
end;
destructor TfProjectFrame.Destroy;
begin
  if Assigned(FOwners) then
    FOwners.Destroy;
  if Assigned(DataSet) then
    begin
      DataSet.Destroy;
      DataSet := nil;
    end;
  inherited Destroy;
end;
function TfProjectFrame.OpenFromLink(aLink: string) : Boolean;
begin
  inherited;
  Result := False;
  if not ((copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS')
  or (copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS.ID')) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  DataSet := TProject.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@ProjectsStateChange;
  if copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS.ID' then
    Data.SetFilter(FDataSet,Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1)
  else
    Data.SetFilter(FDataSet,Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1);
  if FDataSet.Count = 0 then
    begin
      if copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS.ID' then
        Data.SetFilter(FDataSet,Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1)
      else
        Data.SetFilter(FDataSet,Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1);
    end;
  if FDataSet.Count > 0 then
    begin
      TabCaption := TProject(FDataSet).Text.AsString;
      DoOpen;
      Result := True;
    end;
end;
procedure TfProjectFrame.New;
begin
  inherited;
  TabCaption := strNewProject;
  DataSet := TProject.CreateEx(Self,Data,FConnection);
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
  aTask := TTask.Create(nil);
  aTask.SelectFromLink(aLink);
  aTask.Open;
  if aTask.Count>0 then
    begin

    end;
  aTask.Free;
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfProjectFrame);
end.

