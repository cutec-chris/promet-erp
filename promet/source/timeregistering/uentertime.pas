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
Created 27.12.2011
*******************************************************************************}
unit uEnterTime;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Dialogs, DBCtrls, StdCtrls,
  Buttons, uIntfStrConsts, db, ExtCtrls, Utils, LCLType, EditBtn,
  ActnList, Grids, Spin, DBGrids, Menus, ComCtrls, LR_Class, LR_DBSet,
  DateUtils, Math, uTimes, uGridView, uFilterFrame,
  uBaseVisualControls,uBaseDatasetInterfaces;
type

  { TfEnterTime }

  TfEnterTime = class(TForm)
    acFilter: TAction;
    acPrint: TAction;
    acGotoID: TAction;
    acMinimizeToTray: TAction;
    acInsert: TAction;
    acStartstandartEntry: TAction;
    acStart: TAction;
    acPause: TAction;
    acStop: TAction;
    acDelete: TAction;
    acUseasNewEntry: TAction;
    acUse: TAction;
    ActionList1: TActionList;
    bAddJob: TSpeedButton;
    bAddJob1: TSpeedButton;
    bChange: TBitBtn;
    bDeleteJob: TSpeedButton;
    bTaskDone: TSpeedButton;
    bHide: TBitBtn;
    bStop: TBitBtn;
    bPause: TBitBtn;
    bStart: TBitBtn;
    cbShowDialog: TCheckBox;
    cbCategory: TComboBox;
    eJob: TComboBox;
    eLink: TEditButton;
    eProject: TEditButton;
    iLink: TImage;
    iLink1: TImage;
    iLink3: TImage;
    lActiveEntry: TLabel;
    lEntry: TLabel;
    lJob1: TLabel;
    lJob2: TLabel;
    lNotes: TLabel;
    lProject: TLabel;
    lTimelist: TLabel;
    mNotes: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    TimeList: TDatasource;
    PTimes: TfrDBDataSet;
    frReport1: TfrReport;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lPausesValue: TLabel;
    lPauses: TLabel;
    lTimeWeek: TLabel;
    lTimeTodayValue: TLabel;
    lTimeToday: TLabel;
    lShowMin: TLabel;
    lTimeWeek1: TLabel;
    lTimeWeekValue: TLabel;
    lTimeMonthValue: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    pTimeList: TPanel;
    pmGrid: TPopupMenu;
    rbNormaltime: TRadioButton;
    rbIndustrialTime: TRadioButton;
    seShowDialog: TSpinEdit;
    Timer: TTimer;
    tmShowDialog: TTimer;
    procedure acDeleteExecute(Sender: TObject);
    procedure acGotoIDExecute(Sender: TObject);
    procedure acInsertExecute(Sender: TObject);
    procedure acMinimizeToTrayExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acStartExecute(Sender: TObject);
    procedure acStartstandartEntryExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acUseasNewEntryExecute(Sender: TObject);
    procedure acUseExecute(Sender: TObject);
    procedure AskUserforContinue(aData: PtrInt);
    procedure AsyncStartTimereg(Data: PtrInt);
    procedure bAddJob1Click(Sender: TObject);
    procedure bCloseKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bTaskDoneClick(Sender: TObject);
    procedure cbShowDialogChange(Sender: TObject);
    procedure DatasourceBeforeScroll(DataSet: TDataSet);
    procedure DatasourceGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure DatasourceSetText(Sender: TField; const aText: string);
    procedure eJobGetItems(Sender: TObject);
    procedure eJobSelect(Sender: TObject);
    procedure eLinkButtonClick(Sender: TObject);
    procedure eLinkChange(Sender: TObject);
    procedure eProjectButtonClick(Sender: TObject);
    procedure FListFilterChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FTimesAfterPost(DataSet: TDataSet);
    procedure FTimesBeforeEdit(DataSet: TDataSet);
    procedure gListSelectEditor(Sender: TObject; Column: TColumn;
      var Editor: TWinControl);
    procedure iLinkClick(Sender: TObject);
    procedure InplaceMemoChange(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure bAddJobClick(Sender: TObject);
    procedure bDeleteJobClick(Sender: TObject);
    procedure seMaxresultsChange(Sender: TObject);
    procedure SetJobTextAsync(Data: PtrInt);
    function SetlinkfromSearch(aLink: string): Boolean;
    function SetProjectfromSearch(aLink: string): Boolean;
    function SetListLinkfromSearch(aLink: string): Boolean;
    procedure StartSetText(Sender: TField; const aText: string);
    procedure TimeListDataChange(Sender: TObject; Field: TField);
    procedure TimerTimer(Sender: TObject);
    procedure tmShowDialogTimer(Sender: TObject);
  private
    { private declarations }
    FAutoFilter : string;
    FLink : string;
    ActDate: Double;
    aOldSize : Integer;
    ColumnWidthHelper : TColumnWidthHelper;
    FOnMinimize: TNotifyEvent;
    FPBtn: TSpeedButton;
    FProject: string;
    FTask: string;
    FTimes: TTimes;
    FIntTimes: TTimes;
    InplaceMemo : TInplaceMemo;
    FJobText: String;
    FNode : TTreeNode;
    FList : TfFilter;
    Startmodified: Boolean;
    Endmodified: Boolean;
    Startvalue: TDateTime;
    Endvalue: TDateTime;
    InAfterScroll: Boolean;
    NoTimeRegCache : string;
    WorkTimeMessage : Boolean;
    procedure SetOnMinimize(const AValue: TNotifyEvent);
    procedure SetPBtn(AValue: TSpeedButton);
    procedure SetProject(const AValue: string);
    procedure SetLink(const AValue: string);
    procedure SetTask(AValue: string);
    procedure UpdateEditors;
    procedure RemoveEditors;
  public
    { public declarations }
    procedure FListGetCellText(Sender: TObject; DataCol: Integer;
      Column: TColumn; var aText: string);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetupDB;
    procedure SetRights;
    procedure Calculate(aDoRefresh : Boolean = True);
    procedure StopActualTime;
    procedure DoSetup;
    procedure SetLanguage;
    procedure StarttimeRegistering(aLink : string);
    function CommandReceived(Sender : TObject;aCommand : string) : Boolean;
    property Link : string read FLink write SetLink;
    property Project : string read FProject write SetProject;
    property Task : string read FTask write SetTask;
    property Times : TTimes read FTimes;
    property OnMinimize : TNotifyEvent read FOnMinimize write SetOnMinimize;
    property PauseBtn : TSpeedButton read FPBtn write SetPBtn;
    property Node : TTreeNode read FNode write FNode;
    procedure refreshNode;
    procedure SetActive;
    procedure StartTimereg;
  end;
var
  fEnterTime: TfEnterTime;
function GetWorkTime(User : string) : Integer;
function CorrectHouredTimeWithSpareTime(User : string;Day : TDateTime;Hours : Integer) : Integer;
function GetFreeTime(User : string;Day : TDateTime;Hours : Integer) : Integer;
function GetFreeDayTime(User : string;Day : TDateTime) : Integer;
resourcestring
  strStart                      = 'beginnen';
  strChange                     = 'austauschen';
  strTimeRegistration           = 'Zeiterfassung';
  strContinuePreviousTimeEntry  = 'Zeiterfassung für'+lineending+'%s'+lineending+'%s'+lineending+'%s'+lineending+lineending+'wurde heute beendet (%s), haben Sie in der Zwischenzeit daran weitergearbeitet?';
  strPause                      = '»Pause«';
  strInvalidTimeFormat          = 'Ungültiges Zeitformat !';
  strEndTimeNotSet              = 'Der letzte Zeiteintrag wurde nicht abgeschlossen,'+lineending+' bitte korrigieren Sie die Endzeit und starten Sie einen neuen Eintrag manuell!';
  strWorkTimeOverdune           = 'Ihre Regelarbeitszeit wurde überschritten';
implementation
{$R *.lfm}
uses
  uRowEditor, uSearch, uBaseSearch, uSelectreport, uBaseApplication,variants,
  uBaseDBInterface,uProjects,uMasterdata,uPerson,uMainTreeFrame,uData,uBaseDbClasses,
  utask
  {$IFDEF WINDOWS}
  ,Windows
  {$ENDIF}
  ;
procedure TfEnterTime.RadioButton2Change(Sender: TObject);
begin
  Calculate;
end;
procedure TfEnterTime.bAddJobClick(Sender: TObject);
begin
  cbCategory.Items.Add(cbCategory.Text);
  with Application as IBaseDBInterface do
    DBConfig.WriteString('JOBS',cbCategory.Items.Text);
end;
procedure TfEnterTime.bDeleteJobClick(Sender: TObject);
begin
  if cbCategory.Items.IndexOf(cbCategory.Text) > -1 then
    cbCategory.Items.Delete(cbCategory.Items.IndexOf(cbCategory.Text));
  with Application as IBaseDBInterface do
    DBConfig.WriteString('JOBS',cbCategory.Items.Text);
end;
procedure TfEnterTime.seMaxresultsChange(Sender: TObject);
begin
  acFilter.Execute;
end;

procedure TfEnterTime.SetJobTextAsync(Data: PtrInt);
begin
  eJob.Text:=FJobText;
end;

function TfEnterTime.SetlinkfromSearch(aLink: string): Boolean;
begin
  Link := aLink;
  Result := True;
end;
function TfEnterTime.SetProjectfromSearch(aLink: string): Boolean;
begin
  Project := aLink;
  Result := True;
end;
function TfEnterTime.SetListLinkfromSearch(aLink: string): Boolean;
var
  aProject: TProject;
begin
  FList.gList.EditorMode:=False;
  FList.gList.SelectedColumn.Field.Text:=aLink;
  if FList.gList.SelectedColumn.FieldName='PROJECT' then
    begin
      aProject := TProject.Create(nil);
      aProject.SelectFromLink(aLink);
      aProject.Open;
      if aProject.Count>0 then
        FList.gList.DataSource.DataSet.FieldByName('PROJECTID').AsVariant:=aProject.Id.AsVariant;
      aProject.Free;
    end;
  FList.gList.DataSource.DataSet.FieldByName('TASKID').Clear;
  FList.gList.EditorMode:=True;
  Result := True;
end;
procedure TfEnterTime.StartSetText(Sender: TField; const aText: string);
var
  NewTime: TDateTime;
begin
  if aText = '' then
    begin
      Sender.Clear;
      exit;
    end;
  if not TryStrToDateTime(aText,NewTime) then
    begin
      if not TryStrToTime(aText,NewTime) then
        begin
          Sender.Clear;
          exit;
        end;
    end;
  if trunc(NewTime) = 0 then
    NewTime := NewTime+trunc(ActDate);
  Sender.AsDateTime:=NewTime;
end;
procedure TfEnterTime.TimeListDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if Field.FieldName = 'START' then Startmodified := True;
  if Field.FieldName = 'END' then Endmodified := True;
end;
procedure TfEnterTime.TimerTimer(Sender: TObject);
begin
  if not Assigned(uData.Data) then exit;
  Calculate(False);
end;
procedure TfEnterTime.tmShowDialogTimer(Sender: TObject);
var
  aWnd: TWinControl = nil;
begin
  if Assigned(Parent) and (Parent is TForm) then
    aWnd := TForm(Parent)
  else aWnd := Application.MainForm;
  if Assigned(aWnd) then
    begin
      {$IFDEF WINDOWS}
      ShowWindow(aWnd.Handle,SW_SHOWNOACTIVATE);
      FlashWindow(aWnd.Handle,True);
      {$ENDIF}
      aWnd.Show;
    end;
end;
procedure TfEnterTime.SetProject(const AValue: string);
begin
  if FProject=AValue then exit;
  FProject:=AValue;
  with Application as IBaseDBInterface do
    begin
      eProject.Text:=Data.GetLinkDesc(FProject);
      if Data.GetLinkIcon(FProject) <> -1 then
        fVisualControls.Images.GetBitmap(Data.GetLinkIcon(FProject),iLink1.Picture.Bitmap)
      else iLink1.Picture.Clear;
    end;
end;
procedure TfEnterTime.SetOnMinimize(const AValue: TNotifyEvent);
begin
  if FOnMinimize=AValue then exit;
  FOnMinimize:=AValue;
  bHide.Visible:=True;
end;

procedure TfEnterTime.SetPBtn(AValue: TSpeedButton);
begin
  if FPBtn=AValue then Exit;
  FPBtn:=AValue;
  if Assigned(FPBtn) then
    FPbtn.Down:=not acPause.Enabled;
end;

procedure TfEnterTime.SetLink(const AValue: string);
begin
  if FLink = AValue then exit;
  FLink := AValue;
  with Application as IBaseDBInterface do
    begin
      eLink.Text:=Data.GetLinkDesc(FLink);
      if Data.GetLinkIcon(FLink) <> -1 then
        fVisualControls.Images.GetBitmap(Data.GetLinkIcon(FLink),iLink.Picture.Bitmap)
      else iLink.Picture.Clear;
    end;
end;
procedure TfEnterTime.SetTask(AValue: string);
begin
  if FTask=AValue then Exit;
  FTask:=AValue;
  bTaskDone.Enabled:=False;
  FJobText:=Ftask;
  with Application as IBaseDBInterface do
    begin
      FJobText:=Data.GetLinkDesc(FTask);
      if Data.GetLinkIcon(FTask) <> -1 then
        begin
          fVisualControls.Images.GetBitmap(Data.GetLinkIcon(FTask),iLink3.Picture.Bitmap);
          bTaskDone.Enabled:=True;
        end
      else iLink3.Picture.Clear;
    end;
  Application.QueueAsyncCall(@SetJobTextAsync,0);
end;
procedure TfEnterTime.UpdateEditors;
var
  i: Integer;
begin
  with FList do
    begin
      for i := 0 to gList.Columns.Count-1 do
        begin
          if (gList.Columns[i].FieldName = 'LINK')
          or (gList.Columns[i].FieldName = 'PROJECT') then
            gList.Columns[i].ButtonStyle:=cbsEllipsis;
          if gList.Columns[i].FieldName = 'CATEGORY' then
            gList.Columns[i].PickList.Assign(cbCategory.Items);
          if gList.Columns[i].FieldName = 'NOTE' then
            gList.Columns[i].ReadOnly:=False;
        end;
      gList.OnEditButtonClick:=@eLinkButtonClick;
    end;
end;
procedure TfEnterTime.RemoveEditors;
var
  i: Integer;
begin
  with FList do
    begin
      for i := 0 to gList.Columns.Count-1 do
        begin
          if gList.Columns[i].ButtonStyle = cbsEllipsis then
            gList.Columns[i].ButtonStyle:=cbsAuto;
          gList.Columns[i].PickList.Clear;
          gList.Columns[i].ReadOnly:=False;
        end;
    end;
end;

constructor TfEnterTime.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FList := TfFilter.Create(Self);
  FList.Parent := pTimeList;
  FTimes := nil;
  FList.DBNavigator1.VisibleButtons := FList.DBNavigator1.VisibleButtons+[nbPost];
end;

destructor TfEnterTime.Destroy;
begin
  Timer.Enabled:=False;
  tmShowDialog.Enabled := False;
  if Assigned(FList) then
    begin
      FList.DataSet := nil;
      FList.Destroy;
      FList := nil;
    end;
  inherited Destroy;
end;
procedure TfEnterTime.acGotoIDExecute(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    Data.GotoLink(Times.FieldByName('LINK').AsString);
end;
procedure TfEnterTime.acDeleteExecute(Sender: TObject);
begin
  Times.DataSet.Delete;
end;
procedure TfEnterTime.acInsertExecute(Sender: TObject);
begin
  Times.DataSet.Insert;
end;
procedure TfEnterTime.acMinimizeToTrayExecute(Sender: TObject);
begin
  if Assigned(FOnMinimize) then
    FOnMinimize(Self);
  tmShowDialog.Enabled:=False;
  tmShowDialog.Enabled:=cbShowDialog.Checked;
end;
procedure TfEnterTime.acPauseExecute(Sender: TObject);
begin
  Times.DataSet.Refresh;
  Times.DataSet.First;
  if not ((Times.DataSet.State = dsInsert) or (Times.DataSet.State = dsEdit)) then
    Times.dataSet.Edit;
  Times.FieldByName('END').AsFloat := Now();
  Times.DuplicateRecord(False);
  Times.FieldByName('ISPAUSE').AsString := 'Y';
  Times.FieldByName('START').AsFloat := Now();
  Times.FieldByName('END').Clear;
  Times.DataSet.Post;
  Calculate;
  Times.DataSet.First;
  Times.DataSet.Refresh;
  with Application as IBaseDBInterface do
    DBConfig.WriteString('NOTIMEREG','');
  refreshNode;
end;
procedure TfEnterTime.acPrintExecute(Sender: TObject);
begin
  fSelectReport.ReportType:='TRR';
  fSelectReport.Report := frReport1;
  fSelectreport.SetLanguage;
  fSelectReport.Execute;
end;
procedure TfEnterTime.acStartExecute(Sender: TObject);
var
  aProject: TProject;
  aTasks: TTask;
begin
  FList.ClearFilter;
  Times.DataSet.Refresh;
  Times.DataSet.First;
  Calculate;
  if (Times.FieldByName('END').IsNull) and (not Times.DataSet.EOF) then
    begin //Change
      if not ((Times.DataSet.State = dsInsert) or (Times.DataSet.State = dsEdit)) then
        Times.dataSet.Edit;
      Times.FieldByName('END').AsFloat := Now();
      Times.DataSet.Post;
      Calculate;
    end;
  if (not acStop.Enabled) or (Sender = nil) then
    begin
      with Application as IBaseApplication do
        Info('Timeregistering:start Time');
      Times.DataSet.Insert;
      Times.FieldByName('LINK').AsString := FLink;
      Times.FieldByName('PROJECT').AsString := FProject;
      aProject := TProject.Create(nil);
      aProject.SelectFromLink(FProject);
      aProject.Open;
      aTasks := TTask.Create(nil);
      aTasks.SelectActiveByUser(Data.Users.Accountno.AsString);
      aTasks.Filter(Data.QuoteField('SUMMARY')+'='+Data.QuoteValue(eJob.Text));
      if aProject.Count>0 then
        begin
          Times.FieldByName('PROJECTID').AsVariant := aProject.Id.AsVariant;
          if aTasks.Locate('PROJECTID',aProject.Id.AsVariant,[]) then
            Times.FieldByName('TASKID').AsVariant := aTasks.Id.AsVariant;
        end
      else if FProject='' then
        begin
          if aTasks.Count>0 then
            Times.FieldByName('TASKID').AsVariant := aTasks.Id.AsVariant;
        end;
      aTasks.Free;
      aProject.Free;
      Times.FieldByName('CATEGORY').AsString := cbCategory.Text;
      Times.FieldByName('JOB').AsString := eJob.Text;
      Times.FieldByName('NOTE').AsString := mNotes.Lines.Text;
      Times.FieldByName('ISPAUSE').AsString := 'N';
      Times.FieldByName('START').AsFloat := Now();
      Times.DataSet.Post;
    end
  else //pause
    begin
      with Application as IBaseApplication do
        Info('Timeregistering:pause Time ');
      Times.DuplicateRecord(False);
      Times.FieldByName('ISPAUSE').AsString := 'N';
      Times.FieldByName('START').AsFloat := Now();
      Times.FieldByName('END').Clear;
      Times.DataSet.Post;
    end;
  acFilter.Execute;
  with Application as IBaseDbInterface do
    DBConfig.WriteString('NOTIMEREG','');
  Calculate;
  Times.DataSet.Refresh;
  with Application as IBaseDbInterface do
  if (DBConfig.ReadString('TIMEREGCLOSEONSTART','Y') <> 'N') or (Assigned(Parent) and (Parent is TFrame)) then
    acMinimizeToTray.Execute;
  refreshNode;
end;
procedure TfEnterTime.acStartstandartEntryExecute(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    begin
      Link := DBConfig.ReadString('TIMELINK','');
      Project := DBConfig.ReadString('TIMEPROJECT','');
      cbCategory.Text := DBConfig.ReadString('TIMECAT','');
      eJob.Text:=DBConfig.ReadString('TIMEJOB','');
      mNotes.Lines.Text:=DBConfig.ReadString('TIMENOTES','');
    end;
  acStartExecute(nil);
end;
procedure TfEnterTime.acStopExecute(Sender: TObject);
begin
  StopActualTime;
  Calculate;
  Times.DataSet.First;
  with Application as IBaseDbInterface do
    DBConfig.WriteString('NOTIMEREG','Y');
end;
procedure TfEnterTime.acUseasNewEntryExecute(Sender: TObject);
begin
  if not Times.Active then exit;
  Link := Times.FieldByName('LINK').AsString;
  Project := Times.FieldByName('PROJECT').AsString;
  cbCategory.Text := Times.FieldByName('CATEGORY').AsString;
  eJob.Text:=Times.FieldByName('JOB').AsString;
  mNotes.Lines.Text:=Times.FieldByName('NOTE').AsString;
  Task:='';
end;
procedure TfEnterTime.acUseExecute(Sender: TObject);
var
  aProject: TProject;
  aTasks: TTask;
begin
  if not ((Times.State = dsInsert) or (Times.dataSet.State = dsEdit)) then
    Times.dataSet.Edit;
  Times.FieldByName('PROJECT').AsString := FProject;
  aProject := TProject.Create(nil);
  aProject.SelectFromLink(FProject);
  aProject.Open;
  if aProject.Count>0 then
    begin
      FList.gList.DataSource.DataSet.FieldByName('PROJECTID').AsVariant:=aProject.Id.AsVariant;
      aTasks := TTask.Create(nil);
      aTasks.SelectFromLink(Task);
      atasks.Open;
      if aTasks.Count>0 then
        FList.gList.DataSource.DataSet.FieldByName('TASKID').AsVariant:=aTasks.Id.AsVariant
      else
        FList.gList.DataSource.DataSet.FieldByName('TASKID').Clear;
      aTasks.Free;
    end;
  aProject.Free;
  Times.FieldByName('LINK').AsString := FLink;
  Times.FieldByName('CATEGORY').AsString := cbCategory.text;
  Times.FieldByName('JOB').AsString := eJob.text;
  Times.FieldByName('NOTE').AsString := mNotes.Lines.Text;
  Times.DataSet.Post;
end;

procedure TfEnterTime.AskUserforContinue(aData: PtrInt);
var
  aRes: TModalResult;
begin
  with Application as IBaseDbInterface do
    begin
      Times.First;
      if Trunc(Times.FieldByName('END').AsFloat) = Trunc(Now()) then
        begin
          if (Now()-Times.FieldByName('END').AsDateTime) > (MSecsPerSec*60*30) then
            aRes := MessageDlg(strTimeRegistration,Format(strContinuePreviousTimeEntry,[Data.GetLinkDesc(Times.FieldByName('LINK').AsString),Data.GetLinkDesc(Times.FieldByName('PROJECT').AsString),Times.FieldByName('JOB').AsString,Times.FieldByName('END').AsString]),mtConfirmation,[mbNo,mbYes],0)
          else
            aRes := MessageDlg(strTimeRegistration,Format(strContinuePreviousTimeEntry,[Data.GetLinkDesc(Times.FieldByName('LINK').AsString),Data.GetLinkDesc(Times.FieldByName('PROJECT').AsString),Times.FieldByName('JOB').AsString,Times.FieldByName('END').AsString]),mtConfirmation,[mbYes,mbNo],0);
          if aRes = mrYes then
            begin
              Times.Edit;
              Times.FieldByName('END').Clear;
              Times.Post;
              exit;
            end;
        end;
      if not (Times.FieldByName('END').IsNull) then
        begin
          if DBConfig.ReadString('TIMESTANDARTSTART','Y') = 'Y' then
            begin
              Times.DuplicateRecord;
              Times.DataSet.Edit;
              Times.FieldByName('LINK').AsString := DBConfig.ReadString('TIMELINK','');
              Times.FieldByName('PROJECT').AsString := DBConfig.ReadString('TIMEPROJECT','');
              Times.FieldByName('CATEGORY').AsString := DBConfig.ReadString('TIMECATEGORY','');
              Times.FieldByName('JOB').AsString := DBConfig.ReadString('TIMEJOB','');
              Times.FieldByName('NOTE').AsString := DBConfig.ReadString('TIMENOTES','');
              Times.FieldByName('START').AsFloat := Now();
              Times.FieldByName('END').Clear;
              Times.Post;
            end
          else
            begin
              Times.DuplicateRecord;
              Times.DataSet.Edit;
              Times.FieldByName('ISPAUSE').AsString := 'N';
              Times.FieldByName('START').AsFloat := Now();
              Times.FieldByName('END').Clear;
              Times.DataSet.Post;
            end;
          Times.DataSet.Refresh;
        end
      else
        begin
          ShowMessage(strEndTimeNotSet);
        end;
    end;
end;

procedure TfEnterTime.AsyncStartTimereg(Data: PtrInt);
begin
  acStartExecute(nil);
end;

procedure TfEnterTime.bAddJob1Click(Sender: TObject);
var
  aTask: TTask;
  aProject: TProject;
begin
  aTask := TTask.Create(nil);
  aTask.Insert;
  aTask.Text.AsString:=eJob.Text;
  aProject := TProject.Create(nil);
  aProject.SelectFromLink(Project);
  aProject.Open;
  if aProject.Count>0 then
    begin
      aTask.Project.AsString:=aProject.Text.AsString;
      aTask.FieldByName('PROJECTID').AsVariant:=aProject.Id.AsVariant;
    end;
  aProject.Free;
  aTask.FieldByName('CATEGORY').AsString:=cbCategory.Text;
  aTask.Post;
end;

procedure TfEnterTime.bCloseKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfEnterTime.bTaskDoneClick(Sender: TObject);
var
  aTask: TTask;
begin
  aTask := TTask.Create(nil);
  aTask.SelectFromLink(Task);
  aTask.Open;
  if aTask.Count>0 then
    begin
      aTask.Edit;
      aTask.FieldByName('COMPLETED').AsString:='Y';
      aTask.Post;
    end;
  aTask.Free;
end;

procedure TfEnterTime.cbShowDialogChange(Sender: TObject);
begin
  if cbShowDialog.Checked then
    begin
      with Application as IBaseDbInterface do
        DBConfig.WriteString('TIMEREGDLG',IntToStr(seShowDialog.Value));
      tmShowDialog.Interval := seShowDialog.Value*60*1000;
    end
  else  with Application as IBaseDbInterface do
    DBConfig.WriteString('TIMEREGDLG','');
  tmShowDialog.Enabled := cbShowDialog.Checked;
end;
procedure TfEnterTime.DatasourceBeforeScroll(DataSet: TDataSet);
begin
  if not Times.FieldByName('START').IsNull then
    ActDate := Times.FieldByName('START').AsDateTime;
end;
procedure TfEnterTime.DatasourceGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := Sender.AsString;
end;
procedure TfEnterTime.DatasourceSetText(Sender: TField; const aText: string);
var
  NewTime: TDateTime;
begin
  if aText = '' then
    begin
      Sender.Clear;
      exit;
    end;
  if not TryStrToDateTime(aText,NewTime) then
    begin
      if not TryStrToTime(aText,NewTime) then
        begin
          Sender.Clear;
          exit;
        end;
    end;
  if (trunc(NewTime) = 0) then
    NewTime := NewTime+trunc(Sender.DataSet.FieldByName('START').AsDateTime);
  Sender.AsDateTime:=NewTime;
end;

procedure TfEnterTime.eJobGetItems(Sender: TObject);
var
  aTasks: TTask;
begin
  aTasks := TTask.Create(nil);
  aTasks.SelectActiveByUser(Data.Users.Accountno.AsString);
  aTasks.Open;
  eJob.Items.Clear;
  while not aTasks.EOF do
    begin
      eJob.Items.Add(aTasks.Text.AsString+' - '+aTasks.Project.AsString);
      aTasks.Next;
    end;
  aTasks.Free;
end;

procedure TfEnterTime.eJobSelect(Sender: TObject);
var
  aTasks: TTask;
  aProject: TProject;
begin
  aTasks := TTask.Create(nil);
  aTasks.SelectActiveByUser(Data.Users.Accountno.AsString);
  aTasks.Open;
  if aTasks.Locate('SUMMARY;PROJECT',VarArrayof([copy(eJob.Text,0,pos(' - ',eJob.Text)-1),copy(eJob.Text,pos(' - ',eJob.Text)+3,length(eJob.Text))]),[loCaseInsensitive]) then
    begin
      aProject := TProject.Create(nil);
      aProject.Select(aTasks.FieldByName('PROJECTID').AsVariant);
      aProject.Open;
      if aProject.Count>0 then
        Project:=Data.BuildLink(aProject.DataSet);
      aProject.Free;
      Task:=Data.BuildLink(aTasks.DataSet);
      cbCategory.Text:=aTasks.FieldByName('CATEGORY').AsString;
    end
  else Task := '';
  aTasks.Free;
end;

procedure TfEnterTime.eLinkButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  if (FList.gList.SelectedColumn.FieldName = 'LINK') or (Sender = eLink) then
    begin
      fSearch.OnOpenItem:=@SetListLinkfromSearch;
      if Sender = eLink then
        fSearch.OnOpenItem:=@SetLinkfromSearch;
      if fSearch.Execute(True,'TIMELINK',strSearchfromTimeregisteringMode) then
        Link := fSearch.GetLink;
      fList.gList.SetFocus;
    end
  else if FList.gList.SelectedColumn.FieldName = 'PROJECT' then
    begin
      fSearch.OnOpenItem:=@SetListLinkfromSearch;
      fSearch.AllowSearchTypes(strProjects);
      if fSearch.Execute(True,'TIMEPROJ',strSearchfromTimeregisteringMode) then
        Project := fSearch.GetLink;
      fList.gList.SetFocus;
    end;

end;

procedure TfEnterTime.eLinkChange(Sender: TObject);
begin
  FLink := eLink.Text;
  iLink.Picture.Clear;
end;
procedure TfEnterTime.eProjectButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@SetProjectfromSearch;
  fSearch.AllowSearchTypes(strProjects);
  fSearch.Execute(True,'TIMEPROJ',strSearchfromTimeregisteringMode);
end;
procedure TfEnterTime.FListFilterChanged(Sender: TObject);
begin
  RemoveEditors;
  UpdateEditors;
  FTimes.FieldByName('START').OnSetText:=@StartSetText;
  FTimes.FieldByName('END').OnSetText:=@DatasourceSetText;
end;
procedure TfEnterTime.FListGetCellText(Sender: TObject; DataCol: Integer;
  Column: TColumn; var aText: string);
const
  memowidth = 300; // 10 Zeichen
var
  s: string;
  nF: Extended;
begin
  if not Assigned(Column) then exit;
  if not Assigned(Column.Field) then exit;
  if (Column.FieldName = 'END') and (trunc(TDBGrid(Sender).DataSource.DataSet.FieldByName('START').AsDateTime) = trunc(TDBGrid(Sender).DataSource.DataSet.FieldByName('END').AsDateTime)) then
    begin
      aText := FormatDateTime(ShortTimeFormat,Column.Field.AsDateTime);
    end
  else
  if ((Column.FieldName = 'LINK')
  or (Column.FieldName = 'JOB')
  or (Column.FieldName = 'PROJECT')
  or (Column.FieldName = 'NOTE'))
  and (Assigned(Column.Field))
  and (TDBGrid(Sender).DataSource.DataSet.FieldByName('ISPAUSE').AsString = 'Y')
  then
    begin
      aText := strPause;
    end
  else if (Column.FieldName = 'LINK') or (Column.FieldName = 'PROJECT') then
    begin
      with Application as IBaseDbInterface do
        aText := Data.GetLinkDesc(Column.Field.AsString);
    end
  else if (Column.FieldName = 'DURATION') then
    begin
      if TryStrToFloat(aText,nF) then
        aText := DayTimeToStr(nF);
    end
  else
    begin
      with (Sender as TDBGrid) do
        begin
          if Column.Field.IsBlob then
            begin
              s := copy(Column.Field.AsString, 1, memowidth);
              aText := s;
            end;
        end;
    end;
end;
procedure TfEnterTime.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled:=False;
end;
procedure TfEnterTime.FormCreate(Sender: TObject);
begin
  {$IFDEF MAINAPP}
  bHide.Width := 0;
  {$ENDIF}
  InplaceMemo := TInplaceMemo.Create(Self);
  InplaceMemo.SetValue:=False;
  {$IFDEF LCL_CARBON}
  bHide.Visible := False;
  {$ENDIF}
end;
procedure TfEnterTime.FormDestroy(Sender: TObject);
begin
  InplaceMemo.Free;
end;
procedure TfEnterTime.FormHide(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TfEnterTime.FormShow(Sender: TObject);
begin
  Calculate(false);
end;

procedure TfEnterTime.FTimesAfterPost(DataSet: TDataSet);
var
  Rec: LongInt;
  aTime: Double;
begin
  Calculate;
end;
procedure TfEnterTime.FTimesBeforeEdit(DataSet: TDataSet);
begin
  Startmodified := False;
  Endmodified := False;
end;
procedure TfEnterTime.gListSelectEditor(Sender: TObject; Column: TColumn;
  var Editor: TWinControl);
begin
  if Column.FieldName = 'NOTE' then
    begin
      Editor := InplaceMemo;
      InplaceMemo.Text:=Times.FieldByName('NOTE').AsString;
      InplaceMemo.OnChange:=@InplaceMemoChange;
    end;
end;
procedure TfEnterTime.iLinkClick(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    Data.GotoLink(FLink);
end;
procedure TfEnterTime.InplaceMemoChange(Sender: TObject);
begin
  FList.gList.DataSource.DataSet.FieldByName('NOTE').AsString:=TInplaceMemo(Sender).Text;
end;
procedure TfEnterTime.SetupDB;
begin
  if FList.FilterType<>'T' then
    begin
      FTimes.DataSet.BeforeScroll:=@DataSourceBeforeScroll;
      FTimes.DataSet.AfterPost:=@FTimesAfterPost;
      FList.DefaultRows:='GLOBALWIDTH:%;START:100;END:60;LINK:100;PROJECT:100;JOB:150;ISPAUSE:30;';
      FList.FilterType:='T';
      FList.Editable:=True;
      FList.Sortable:=False;
      FList.Align:=alClient;
      FList.Dataset := FTimes;
      FList.OnGetCellText:=@FListGetCellText;
      FList.gList.PopupMenu := pmGrid;
      FList.gList.OnSelectEditor:=@gListSelectEditor;
      FList.SortDirection:=sdDescending;
      FList.SortField:='START';
      TimeList.DataSet := FTimes.DataSet;
      FList.OnFilterChanged:=@FListFilterChanged;
      FList.AddToolbarAction(acDelete);
      FList.AddToolbarAction(acInsert);
      acUseAsNewEntry.Execute;
    end;
  FTimes.Open;
  FTimes.FieldByName('START').OnSetText:=@StartSetText;
  FTimes.FieldByName('END').OnSetText:=@DatasourceSetText;
  with Application as IBaseDBInterface do
    begin
      cbCategory.Items.Text:=DBConfig.ReadString('JOBS','');
      seShowDialog.OnChange:=nil;
      cbShowDialog.OnChange:=nil;
      seShowDialog.Value := DBConfig.ReadInteger('TIMEREGDLG',20);
      cbShowDialog.Checked := DBConfig.ReadString('TIMEREGDLG','') <> '';
      seShowDialog.OnChange:=@cbShowDialogChange;
      cbShowDialog.OnChange:=@cbShowDialogChange;
      tmShowDialog.Interval := seShowDialog.Value*60*1000;
      tmShowDialog.Enabled := cbShowDialog.Checked;
    end;
  WorkTimeMessage:=True;
  RemoveEditors;
  UpdateEditors;
  Calculate;
end;
procedure TfEnterTime.SetRights;
var
  aRight: LongInt;
begin
  with Application as IBaseDbInterface do
    aRight := Data.Users.Rights.Right('TIMEREG',True);
  bStop.Visible:=aRight > RIGHT_READ;
end;
procedure TfEnterTime.Calculate(aDoRefresh : Boolean = True);
var
  aPauses : TDateTime;
  aTimes : TDateTime;
  aPausesTD : TDateTime;
  aTimesTD : TDateTime;
  aTimesTW : TDateTime;
  aTimesTM : TDateTime;
  Typ,ID : string;
  Job: String;
  aEnd: TDateTime;
  aStart: TDateTime;
  aRec: LongInt;
begin
  if not Assigned(Parent) then exit;
  if not Assigned(Data) then exit;
  if (FIntTimes.DataSet.State <> dsBrowse) then exit;
  try
  if aDoRefresh then
    begin
      FIntTimes.DataSet.Refresh;
      with Application as IBaseDBInterface do
        NoTimeRegCache := DBConfig.ReadString('NOTIMEREG','N');
    end;
  with Application as IBaseDBInterface do
    if NoTimeRegCache = '' then NoTimeRegCache := DBConfig.ReadString('NOTIMEREG','N');
  FIntTimes.DataSet.First;
  if (((not (FIntTimes.FieldByName('END').IsNull)) and (FIntTimes.FieldByName('ISPAUSE').AsString <> 'Y')) or (FIntTimes.DataSet.EOF))
  or (NoTimeRegCache = 'Y') then
    begin
      acStart.Enabled := True;
      acUse.Enabled := False;
      acPause.Enabled := False;
      acStop.Enabled := False;
      if Assigned(FPBtn) then
        FPbtn.Down:=not acPause.Enabled;
    end
  else if FIntTimes.FieldByName('ISPAUSE').AsString = 'Y' then
    begin
      acStart.Enabled := true;
      acUse.Enabled := False;
      acStop.Enabled := true;
      acPause.Enabled := false;
      if Assigned(FPBtn) then
        FPbtn.Down:=not acPause.Enabled;
    end
  else
    begin
      acStart.Enabled := True;
      acUse.Enabled := True;
      acStop.Enabled := True;
      acPause.Enabled := True;
      if Assigned(FPBtn) then
        FPbtn.Down:=not acPause.Enabled;
    end;

  aPauses := 0;
  aTimes := 0;
  aPausesTD := 0;
  aTimesTD := 0;
  aTimesTW := 0;
  aTimesTM := 0;
  FIntTimes.DataSet.First;
  while not FIntTimes.DataSet.EOF do
    begin
      if FIntTimes.FieldByName('END').IsNull then
        aEnd := Now()
      else
        aEnd := FIntTimes.FieldByName('END').AsDateTime;
      aStart := FIntTimes.FieldByName('START').AsDateTime;
      if (FIntTimes.FieldByName('ISPAUSE').AsString = 'N') then
        aTimes := aTimes+aEnd-aStart
      else
        aPauses := aPauses+aEnd-aStart;

      if (trunc(FIntTimes.FieldByName('START').AsDateTime) = trunc(Now)) then
        begin
          aTimesTD := aTimes;
          aPausesTD := aPauses;
        end;
      if  (YearOf(FIntTimes.FieldByName('START').AsDateTime)  = YearOf(Now))
      and (WeekOf(FIntTimes.FieldByName('START').AsDateTime) = WeekOf(Now())) then
        aTimesTW := aTimes;
      if  (YearOf(FIntTimes.FieldByName('START').AsDateTime)  = YearOf(Now))
      and (MonthOf(FIntTimes.FieldByName('START').AsDateTime) = MonthOf(Now())) then
        aTimesTM := aTimes;
      FIntTimes.DataSet.Next;
    end;
  if rbNormaltime.Checked then
    begin
      lTimeTodayValue.Caption := DateTimeToHourString(aTimesTD);
      lPausesValue.Caption := DateTimeToHourString(aPausesTD);
      lTimeWeekValue.Caption := DateTimeToHourString(aTimesTW);
      lTimeMonthValue.Caption := DateTimeToHourString(aTimesTM);
    end
  else
    begin
      lTimeTodayValue.Caption := DateTimeToIndustrialTime(aTimesTD);
      lTimeWeekValue.Caption := DateTimeToIndustrialTime(aTimesTW);
      lTimeMonthValue.Caption := DateTimeToIndustrialTime(aTimesTM);
    end;
  except
  end;
end;

procedure TfEnterTime.StopActualTime;
begin
  Timer.Enabled:=False;
  if Assigned(Times.DataSet) and (Times.dataSet.Active) then
    begin
      Times.DataSet.Refresh;
      Times.DataSet.First;
      if (Times.FieldByName('END').IsNull) and (not Times.DataSet.EOF) then
        begin //Change
          if not ((Times.DataSet.State = dsInsert) or (Times.dataSet.State = dsEdit)) then
            Times.dataSet.Edit;
          Times.FieldByName('END').AsFloat := Now();
          Times.DataSet.Post;
        end;
//      fTasks.PauseActiveTask;
    end;
end;
procedure TfEnterTime.DoSetup;
var
  aRes: LongInt;
begin
  with Application as IBaseApplication do
    Info('Timeregistering:DoSetup');
  if not Assigned(Times) then
    begin
      with Application as IBaseDbInterface do
        begin
          FTimes := TTimes.CreateEx(nil,Data,nil,Data.Users.DataSet);
          FIntTimes := TTimes.CreateEx(nil,Data,nil,Data.Users.DataSet);
          FTimes.CreateTable;
        end;
      Times.Open;
      FIntTimes.Open;
    end;
  if not Times.DataSet.Active then
    Times.Open;
  Times.DataSet.First;
  if Times.DataSet.EOF then exit;
  with Application as IBaseDbInterface do
    begin
      if DBConfig.ReadString('NOTIMEREG','N') = 'Y' then exit;
      Application.QueueAsyncCall(@AskUserforContinue,0);
    end;
  with Application as IBaseApplication do
    Info('Timeregistering:DoSetup end');
end;
procedure TfEnterTime.SetLanguage;
begin
end;
procedure TfEnterTime.StarttimeRegistering(aLink: string);
begin
  //TODO: Start Timeregistering
  Link := aLink;
  cbCategory.Text:='';
  mNotes.Lines.Clear;
  Calculate;
  acStart.Execute;
end;

function TfEnterTime.CommandReceived(Sender: TObject; aCommand: string
  ): Boolean;
begin
  Result := False;
  if copy(aCommand,0,10) = 'Time.enter' then
    begin
      aCommand := copy(aCommand,12,length(aCommand));
      Project:=copy(aCommand,0,pos(';',aCommand)-1);
      aCommand := copy(aCommand,pos(';',aCommand)+1,length(aCommand));
      Task:=copy(aCommand,0,pos(';',aCommand)-1);
      aCommand := copy(aCommand,pos(';',aCommand)+1,length(aCommand));
      Link:=copy(aCommand,0,pos(')',aCommand)-1);
      mNotes.Clear;
      Result := True;
    end
  else if aCommand = 'Time.start' then
    StartTimereg
  else if aCommand = 'OnClick(/Zeiterfassung/Standardeintrag starten)' then
    begin
      acStartstandartEntry.Execute;
      Result := True;
    end
  else if copy(aCommand,0,23)='OnClick(/Zeiterfassung)' then Result := True;
end;

procedure TfEnterTime.refreshNode;
begin
  if Assigned(FNode) and Assigned(FNode.Data) then
    begin
      FreeAndNil(TTreeEntry(FNode.Data).SubText);
      TTreeEntry(FNode.Data).SubText := TStringList.Create;
      if (Times.DataSet.FieldByName('ISPAUSE').AsString = 'Y') then
        TTreeEntry(FNode.Data).SubText.Add(strPause)
      else
        begin
          TTreeEntry(FNode.Data).SubText.Add(Data.GetLinkDesc(FProject));
          TTreeEntry(FNode.Data).SubText.Add(cbCategory.Text);
        end;
      FNode.TreeView.Invalidate;
    end;
end;

procedure TfEnterTime.SetActive;
begin
  FList.SetActive;
end;

procedure TfEnterTime.StartTimereg;
begin
  Application.QueueAsyncCall(@AsyncStartTimereg,0);
end;

function GetWorkTime(User : string): Integer;
begin
  Result := 8;
  //TODO: Setting for every User
end;
function CorrectHouredTimeWithSpareTime(User : string;Day: TDateTime; Hours: Integer): Integer;
var
  RemainingHours : Integer;
  TargetDay : TDateTime;
begin
  RemainingHours := Hours;
  Result := 0;
  TargetDay := Day;
  while RemainingHours >= GetWorkTime(User) do
    begin
      Result := Result+GetWorkTime(User);
      dec(Remaininghours,GetFreeDayTime(User,TargetDay));
      TargetDay := TargetDay+1;
    end;
  Result := Result+RemainingHours;
end;
function GetFreeTime(User : string;Day: TDateTime; Hours: Integer): Integer;
var
  RemainingHours : Integer;
  TargetDay : TDateTime;
begin
  RemainingHours := Hours;
  Result := 0;
  TargetDay := Day;
  while RemainingHours >= GetWorkTime(User) do
    begin
      Result := Result+GetFreeDayTime(User,TargetDay);
      dec(Remaininghours,GetWorkTime(User));
      TargetDay := TargetDay+1;
    end;
  Result := Result+RemainingHours;
end;
function GetFreeDayTime(User: string; Day: TDateTime): Integer;
begin
  Result := 0;
  if (DayOfWeek(Day) = 1) //Sonntag
  or (DayOfWeek(Day) = 7) then //Samstag
  //TODO:Samstagsarbeit
  else
  //TODO:termine und produktion abziehen
    Result := GetWorkTime(User);
end;
initialization
end.

