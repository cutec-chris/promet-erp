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

info@cu-tec.de
*******************************************************************************}
unit uEventEdit;
interface
uses
  LMessages, LCLProc, LCLType, LCLIntf,  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, VpData, VpEdPop, VpDateEdit,
  ComCtrls, VpBase, VpClock, VpBaseDS, VpDlg, VpConst, ZVDateTimePicker,
  uExtControls, Buttons, EditBtn, ButtonPanel, Spin, DbCtrls, Menus, ActnList,
  LR_DBSet, LR_Class, uIntfStrConsts, uCalendar, db, uBaseDbClasses,uBaseDatasetInterfaces;
type

  { TfEventEdit }

  TfEventEdit = class(TForm)
    acDelete: TAction;
    acRights: TAction;
    ActionList1: TActionList;
    AdvanceUpDown: TUpDown;
    AlarmAdvance: TEdit;
    AlarmSet: TCheckBox;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    AlarmAdvType: TComboBox;
    Bevel8: TBevel;
    Bevel9: TBevel;
    bExecute: TSpeedButton;
    CBAllDay: TCheckBox;
    cbCategory: TComboBox;
    cbPlanrel: TCheckBox;
    Datasource: TDatasource;
    DescriptionEdit: TEdit;
    eLocation: TEdit;
    EndDate: TZVDateTimePicker;
    EndTimeLbl: TLabel;
    History: TDatasource;
    Label3: TLabel;
    Label7: TLabel;
    MenuItem3: TMenuItem;
    miDelete: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    pcPages: TExtMenuPageControl;
    Image1: TImage;
    Image2: TImage;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lHour: TLabel;
    NotesMemo: TMemo;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    lMessage: TLabel;
    FileDialog: TOpenDialog;
    Panel8: TPanel;
    Panel9: TPanel;
    pgEvent: TPageControl;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pmAction: TPopupMenu;
    pNav1: TPanel;
    PUsers: TfrDBDataSet;
    RecurrenceEndsLbl: TLabel;
    RecurringType: TComboBox;
    RepeatUntil: TZVDateTimePicker;
    Report: TfrReport;
    sbMenue: TSpeedButton;
    seDuration: TFloatSpinEdit;
    StartDate: TZVDateTimePicker;
    StartTimeLbl: TLabel;
    tabEvent: TTabSheet;
    ToolButton1: TBitBtn;
    ToolButton2: TBitBtn;
    tsNotes: TTabSheet;
    ToolBar1: TPanel;
    ToolBar2: TPanel;
    Users: TDatasource;
    procedure acDeleteExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure bExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AlarmAdvanceChange(Sender: TObject);
    procedure AdvanceUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure fSelectReportReportGetValue(const ParName: String;
      var ParValue: Variant);
    procedure RecurringTypeChange(Sender: TObject);
    procedure AlarmSetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
    procedure seDurationChange(Sender: TObject);
    procedure StartDateExit(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private { Private declarations }
    AAVerifying: Boolean;
    CIVerifying: Boolean;
    FDataSet : TEvent;
    FEditable : Boolean;
    FDataStore: TVpCustomDataStore;
    procedure PopLists;
    procedure DoOpen;
    procedure AddDocuments(Sender: TObject);
    procedure AddLinks(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure Addusers(Sender: TObject);
  public { Public declarations }
    Event: TVpEvent;
    CatColorMap: TVpCategoryColorMap;
    Resource: TVpResource;
    Conflicts : Integer;
    TimeFormat: TVpTimeFormat;
    AlarmWavPath: string;
    FLastEndTime : TDateTime;                                            
    procedure PopulateDialog;
    procedure DePopulateDialog;
    procedure SetRights;
    function Execute(aEvent : TVpEvent;aResource : TVpResource;aDir : Variant;aDataStore : TVpCustomDataStore) : Boolean;
  end;
implementation
uses
  VpSR,uDocuments,uDocumentFrame,uData,uLinkFrame,uprometframesinplace,
  uSelectReport,uBaseDBInterface,uHistoryFrame,uNRights,umeetingusers;
resourcestring
  strEventinPast                = 'Das Ereignis liegt in der Vergangenheit';
procedure TfEventEdit.FormCreate(Sender: TObject);
begin
  PopLists;
end;

procedure TfEventEdit.bExecuteClick(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  DePopulateDialog;
  FDataStore.PostEvents;
  FDataSet.SelectById(Event.RecordID);
  FDataSet.Open;
  fSelectReport.Report := Report;
  fSelectReport.Report.OnGetValue:=@fSelectReportReportGetValue;
  fSelectReport.SetLanguage;
  //MandantDetails.DataSet := Data.MandantDetails.DataSet;
  //Data.MandantDetails.Open;
  if Supports(FDataSet, IBaseHistory, Hist) then
    History.DataSet := Hist.GetHistory.DataSet;
  DataSource.DataSet := FDataSet.DataSet;
  PList.DataSet := FDataSet.DataSet;
  Users.DataSet := Data.Users.DataSet;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      fSelectReport.ReportType := 'EVT';
    end;
  fSelectReport.Showmodal;
  fSelectReport.Report.OnGetValue:=nil;
end;

procedure TfEventEdit.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Application.ProcessMessages;
      FDataSet.Delete;
      FDataSet.CascadicCancel;
      FDataStore.RefreshEvents;
      ModalResult := mrCancel;
      Close;
    end;
end;

procedure TfEventEdit.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(FDataSet.Id.AsVariant);
end;

procedure TfEventEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfEventEdit.PopulateDialog;
var
  I: Integer;
  aType: Char;
begin
  cbCategory.Items.Clear;
  aType := 'C';
  Data.Categories.CreateTable;
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
  { Events }
  StartDate.DateTime := Event.StartTime;
  EndDate.DateTime := Event.EndTime;
  seDuration.Value := (Event.EndTime-Event.StartTime)*24;
  RepeatUntil.Date := Event.RepeatRangeEnd;

  CBAllDay.Checked := Event.AllDayEvent;
  cbCategory.Text:= Event.StrCategory;
  cbPlanrel.Checked:= Event.Category = 8;
  AlarmWavPath := Event.AlarmWavPath;

  DescriptionEdit.Text := Event.Description;
  DescriptionEdit.SelectAll;
  NotesMemo.Text := Event.Note;
  AlarmSet.Checked := Event.AlarmSet;
  AlarmSetClick(Self);
  if not Event.AlarmSet then
    AlarmAdvance.Text := '15'
  else
    AlarmAdvance.Text := IntToStr(Event.AlarmAdv);
  AlarmAdvType.ItemIndex := Ord(Event.AlarmAdvType);
  RecurringType.ItemIndex := Ord(Event.RepeatCode);
  RecurringTypeChange(Self);
  FLastEndTime := Event.EndTime;
  eLocation.Text:= Event.Location;
  DoOpen;
end;
procedure TfEventEdit.DePopulateDialog;
begin
  { Events }
  Event.StartTime := StartDate.DateTime;
  Event.EndTime := EndDate.DateTime;
  Event.RepeatRangeEnd := RepeatUntil.Date;
  Event.Description := DescriptionEdit.Text;
  Event.Note := NotesMemo.Text;
  Event.AlarmSet := AlarmSet.Checked;
  Event.AlarmAdv := StrToIntDef(AlarmAdvance.Text, 0);
  Event.AlarmAdvType := TVpAlarmAdvType(AlarmAdvType.ItemIndex);
  Event.RepeatCode := TVpRepeatType(RecurringType.ItemIndex);
  Event.AllDayEvent := CBAllDay.Checked;
  Event.StrCategory := cbCategory.Text;
  if cbPlanrel.Checked then
    Event.Category := 8
  else
    Event.Category :=  0;
  Event.AlarmWavPath := AlarmWavPath;
  Event.Location:=eLocation.Text;
  FDataSet.Edit;
end;

procedure TfEventEdit.SetRights;
begin

end;

procedure TfEventEdit.PopLists;
var
  I, Hour, Minute: Integer;
  MinStr, AMPMStr: string;
begin
  { RecurringList }
  RecurringType.Items.Add(RSNone);
  RecurringType.Items.Add(RSDaily);
  RecurringType.Items.Add(RSWeekly);
  RecurringType.Items.Add(RSMonthlyByDay);
  RecurringType.Items.Add(RSMonthlyByDate);
  RecurringType.Items.Add(RSYearlyByDay);
  RecurringType.Items.Add(RSYearlyByDate);
  RecurringType.Items.Add(RSCustom);
  RecurringType.ItemIndex := 0;

  { Alarm Advance Type }
  AlarmAdvType.Items.Add(RSMinutes);
  AlarmAdvType.Items.Add(RSHours);
  AlarmAdvType.Items.Add(RSDays);
  AlarmAdvType.ItemIndex := 0;
end;
procedure TfEventEdit.DoOpen;
var
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
begin
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(FDataSet.Id.AsInteger,'T',0);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          pcPages.AddTab(aDocFrame,False);
          aDocFrame.DataSet := aDocuments;
        end;
    end;
  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TEvent(FDataSet).Links.Open;
  if TEvent(FDataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
  pcPages.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  TEvent(FDataSet).History.Open;
  if TEvent(FDataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  pcPages.AddTabClass(TfMeetingUsers,strMeetingUsers,@AddUsers);
  TEvent(FDataSet).Users.Open;
  if TEvent(FDataSet).Users.Count > 0 then
    pcPages.AddTab(TfMeetingUsers.Create(Self),False);
  SetRights;
end;
procedure TfEventEdit.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(FDataSet.Id.AsInteger,'T',0);
    end;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfEventEdit.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='E';
  TfLinkFrame(Sender).DataSet := TEvent(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfEventEdit.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='CA';
  TfHistoryFrame(Sender).DataSet := TEvent(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfEventEdit.Addusers(Sender: TObject);
begin
  TfLinkFrame(Sender).DataSet := TEvent(FDataSet).Users;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

function TfEventEdit.Execute(aEvent: TVpEvent; aResource: TVpResource;
  aDir: Variant; aDataStore: TVpCustomDataStore): Boolean;
var
  ActControl: TWinControl;
  aInserted: Boolean = False;
begin
  FEditable := True;
  Event := aEvent;
  FDataSet := TEvent.Create(nil);
  FDataSet.SelectById(aEvent.RecordID);
  FDataSet.Open;
  FDataStore := aDataStore;
  if FDataSet.Count=0 then
    begin
      FDataSet.Insert;
      FDataSet.FieldByName('ID').AsVariant:=aEvent.RecordID;
      FDataSet.FieldByName('REF_ID_ID').AsVariant:=aDir;
      FDataSet.Post;
      aInserted := True;
    end;
  Resource := aResource;
  PopulateDialog;
  ActControl := Screen.ActiveControl;
  Show;
  while Visible do
    begin
      Application.ProcessMessages;
      sleep(100);
    end;
  Result := ModalResult = mrOk;
  if ModalResult = mrNone then
    if (MessageDlg(strItem+' '+FDataSet.FieldByName('SUMMARY').AsString,strItemnotSaved,mtInformation,[mbYes,mbNo],0) = mrYes) then
      Result := True;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
  if Result then
    begin
      DePopulateDialog;
      if FDataSet.CanEdit then
        FDataSet.Post;
      aDataStore.PostEvents;
    end
  else if aInserted and (FDataSet.Count>0) then
    FDataSet.Delete;
  FDataSet.Free;
end;
procedure TfEventEdit.AlarmAdvanceChange(Sender: TObject);
var
  I: Integer;
  Str: string;
begin
  if AAVerifying then exit;
  AAVerifying := true;
  { Don't allow non numeric values. }
  Str := AlarmAdvance.Text;
  I := Length(Str);
  if (Str[I] > #57) or (Str[I] < #48) then
    Delete(Str, I, 1);
  AlarmAdvance.Text := Str;
  AAVerifying := false;

  if Str <> '' then
    AdvanceUpDown.Position := StrToInt(Str);
end;
procedure TfEventEdit.AdvanceUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  { Inc or Dec AlarmAdvance according to which button was pressed }
{  case Button of
    btNext:
      AlarmAdvance.Text := IntToStr(StrToIntDef(AlarmAdvance.Text, 0) + 1);
    btPrev:
      AlarmAdvance.Text := IntToStr(StrToIntDef(AlarmAdvance.Text, 0) - 1);
  end;}
  AlarmAdvance.Text := IntToStr(AdvanceUpDown.Position);
end;

procedure TfEventEdit.fSelectReportReportGetValue(const ParName: String;
  var ParValue: Variant);
var
  Result: Integer = 0;
  i: Cardinal;
begin
  if Uppercase(ParName) = 'DURATION' then
    begin
      for i := trunc(FDataSet.FieldByName('STARTDATE').AsDateTime) to Trunc(FDataSet.FieldByName('ENDDATE').AsDateTime) do
        if not ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
          Result := Result+1;
      ParValue := IntToStr(Result);
    end;
end;

procedure TfEventEdit.RecurringTypeChange(Sender: TObject);
begin
  if (RecurringType.ItemIndex > 0)
  and (RepeatUntil.Date <= StartDate.Date)
  then
    RepeatUntil.Date := StartDate.Date + 365;

  RecurrenceEndsLbl.Enabled := (RecurringType.ItemIndex > 0);
  RepeatUntil.Enabled := RecurrenceEndsLbl.Enabled;
end;
procedure TfEventEdit.AlarmSetClick(Sender: TObject);
begin
  AlarmAdvance.Enabled  := AlarmSet.Checked;
  AlarmAdvType.Enabled  := AlarmSet.Checked;
  AdvanceUpDown.Enabled := AlarmSet.Checked;
  Event.SnoozeTime := 0.0;
end;
procedure TfEventEdit.FormShow(Sender: TObject);
begin
  DescriptionEdit.SetFocus;
end;

procedure TfEventEdit.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfEventEdit.seDurationChange(Sender: TObject);
begin
  EndDate.DateTime := StartDate.DateTime+(seDuration.Value/24);
end;
procedure TfEventEdit.StartDateExit(Sender: TObject);
begin
  EndDate.DateTime := StartDate.DateTime+(seDuration.Value/24);
end;
procedure TfEventEdit.ToolButton1Click(Sender: TObject);
begin
  Close;
end;
initialization
{$R *.lfm}
end.
 
