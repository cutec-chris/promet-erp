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
unit uCalendarFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Buttons, ExtCtrls, StdCtrls,
  ActnList, db, uPrometFrames, VpMonthView, VpWeekView, VpDayView, VpBaseDS,
  VpData, VpBase, uBaseDbInterface, uCalendar, DateUtils, ComCtrls, DbCtrls;
type
  TCustomPrometheusDataStore = class(TVpCustomDataStore)
  private
    FDataSet: TCalendar;
    FDirectory: Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNextID(TableName: string): int64;override;
    procedure LoadEvents; override;
    procedure RefreshEvents; override;
    procedure PostEvents; override;
    property DataSet : TCalendar read FDataSet write FDataSet;
    property Directory : Int64 read FDirectory write FDirectory;
  published
  end;

  { TfCalendarFrame }

  TfCalendarFrame = class(TPrometMainFrame)
    acDayView: TAction;
    acGotoToday: TAction;
    acMonthView: TAction;
    acNew: TAction;
    acPrint: TAction;
    ActionList1: TActionList;
    acWeekView: TAction;
    bDayView: TSpeedButton;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    bMonthView: TSpeedButton;
    bNew: TSpeedButton;
    bPrint: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    DayView: TVpDayView;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MonthView: TVpMonthView;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pDayView: TPanel;
    sbMenue: TSpeedButton;
    ToolBar1: TPanel;
    WeekView: TVpWeekView;
    procedure acDayViewExecute(Sender: TObject);
    procedure acGotoTodayExecute(Sender: TObject);
    procedure acMonthViewExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acWeekViewExecute(Sender: TObject);
    procedure DataStoreDateChanged(Sender: TObject; Date: TDateTime);
    procedure DayViewOwnerEditEvent(Sender: TObject; Event: TVpEvent;
      Resource: TVpResource; var AllowIt: Boolean);
    procedure MonthViewDblClick(Sender: TObject);
    procedure MonthViewEventDblClick(Sender: TObject; Event: TVpEvent);
    procedure WeekViewMouseWheel(Sender: TObject; Shift: TShiftState; Delta,
      XPos, YPos: Word);
  private
    { private declarations }
    procedure DoOpen;override;
  public
    { public declarations }
    FCalendarNode : TTreeNode;
    aDirectory : string;
    DataStore : TCustomPrometheusDataStore;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure OpenDir(Directory : LongInt);
    procedure DoRefresh;
  end;
procedure RefreshCalendar(FNode :TTreeNode);
procedure AddToMainTree(aAction : TAction;var FCalendarNode : TTreeNode);
implementation
uses uData, uMainTreeFrame, Math, uEventEdit, VpConst,uBaseDbClasses;
resourcestring
  strEventsThisWeek             = 'diese Woche: %d';
procedure RefreshCalendar(FNode: TTreeNode);
var
  List: TList;
  TimeStr: String;
  i: Integer;
  OldFilter: String;
  aHeight : Integer = 0;
  TodayStartTime: TDateTime;
  TodayEndTime: TDateTime;
  ShownEvents : Integer = 0;
  tmp: String;

  Cal: TCalendar;
  aDataStore: TCustomPrometheusDataStore;
begin
  if not Assigned(FNode) then exit;
  if Assigned(TTreeEntry(FNode.Data).SubText) then
    TTreeEntry(FNode.Data).SubText.Free;
  TTreeEntry(FNode.Data).SubText := TStringlist.Create;
  Cal := TCalendar.Create(nil,Data);
  Cal.CreateTable;
  aDataStore := TCustomPrometheusDataStore.Create(Application);
  Data.SetFilter(Cal,Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(Data.Users.Id.AsString)+' AND ("STARTDATE" < '+Data.DateToFilter(EndOfTheWeek(Now())+1)+') AND ("ENDDATE" > '+Data.DateToFilter(StartOfTheWeek(Now())-1)+' OR "ROTATION" > 0)');
  aDataStore.DataSet := Cal;
  aDataStore.Resource := TVpResource.Create(aDataStore.Resources);
  aDataStore.Resource.Description:='';
  aDataStore.LoadEvents;
  List := TList.Create;
  aDataStore.Resource.Schedule.EventsByDate(trunc(Now),List);
  i := 0;
  while i < List.Count do
    begin
      if  (TVpEvent(List[i]).StartTime < Now())
      and (not TVpEvent(List[i]).AllDayEvent) then
        List.Delete(i)
      else inc(i);
    end;
  for i := 0 to Min(2,List.Count-1) do
    begin
      TodayStartTime := TVpEvent(List[i]).StartTime;
      TodayEndTime := TVpEvent(List[i]).EndTime;
      if trunc(TodayStartTime) < trunc(Now()) then //First Event
        TodayStartTime := 0;
      if trunc(TodayEndTime) > trunc(Now()) then //Last Event
        TodayEndTime := 0.9999;
      TimeStr := FormatDateTime('hh:mm',TodayStartTime) + ' - ' + FormatDateTime('hh:mm', TodayEndTime) + ': ';
      if TVpEvent(List[i]).AllDayEvent then
        TimeStr := '';
      TimeStr := TimeStr+TVpEvent(List[i]).Description;
      if TTreeEntry(FNode.Data).SubText.IndexOf(TimeStr) = -1 then
        TTreeEntry(FNode.Data).SubText.Add(TimeStr);
      inc(ShownEvents);
    end;
  List.Free;
  TimeStr := '';
  for i := 0 to aDataStore.Resource.Schedule.EventCount-1 do
    if (trunc(aDataStore.Resource.Schedule.GetEvent(i).StartTime) <= trunc(EndOfTheWeek(Now())))
    and (trunc(aDataStore.Resource.Schedule.GetEvent(i).EndTime) >= trunc(Now()+1))
    and (TTreeEntry(FNode.Data).SubText.IndexOf(aDataStore.Resource.Schedule.GetEvent(i).Description) = -1)
    and (TTreeEntry(FNode.Data).SubText.IndexOf(LongDayNames[DayOfWeek(aDataStore.Resource.Schedule.GetEvent(i).StartTime)]+': '+aDataStore.Resource.Schedule.GetEvent(i).Description) = -1)
    then
    begin
      TimeStr := LongDayNames[DayOfWeek(aDataStore.Resource.Schedule.GetEvent(i).StartTime)]+': ';
      TodayStartTime := aDataStore.Resource.Schedule.GetEvent(i).StartTime;
      TodayEndTime := aDataStore.Resource.Schedule.GetEvent(i).EndTime;
      if trunc(TodayStartTime) < trunc(aDataStore.Resource.Schedule.GetEvent(i).StartTime) then //First Event
        TodayStartTime := 0;
      if trunc(TodayEndTime) > trunc(aDataStore.Resource.Schedule.GetEvent(i).StartTime) then //Last Event
        TodayEndTime := 0.9999;
      if not aDataStore.Resource.Schedule.GetEvent(i).AllDayEvent then
        TimeStr := TimeStr+FormatDateTime('hh:mm',TodayStartTime) + ' - ' + FormatDateTime('hh:mm', TodayEndTime) + ': ';
      TimeStr := TimeStr+aDataStore.Resource.Schedule.GetEvent(i).Description;
      if TimeStr <> '' then
        if TTreeEntry(FNode.Data).SubText.IndexOf(TimeStr) = -1 then
          TTreeEntry(FNode.Data).SubText.Add(TimeStr);
      inc(ShownEvents);
      if (ShownEvents > 3) then
        begin
          TTreeEntry(FNode.Data).SubText.Add('...');
          break;
        end;
    end;
  for i := 0 to TTreeEntry(FNode.Data).SubText.Count-1 do
    aHeight := aHeight+(FNode.TreeView.Canvas.TextExtent(TTreeEntry(FNode.Data).SubText[i]).cy-1);
  if aHeight > 32 then
    FNode.Height := aHeight
  else
    aHeight := 32;
  aDataStore.Resource.Free;
  aDataStore.Free;
  Cal.Free;
end;

procedure AddToMainTree(aAction: TAction; var FCalendarNode: TTreeNode);
var
  Node: TTreeNode;
  Node1: TTreeNode;
  aCal: TCalendar;
  procedure CollectUsers(aParent : Variant);
  var
    aUsers: TUser;
    bParent : Variant;
  begin
    if aParent = Null then exit;
    aUsers := TUser.Create(nil,Data);
    with aUsers.DataSet as IBaseDbFilter do
      begin
        SortFields := 'NAME';
        Limit:=0;
        SortDirection:=uBaseDbInterface.TSortDirection.sdAscending;
      end;
    Data.SetFilter(aUsers,Data.QuoteField('PARENT')+'='+Data.QuoteValue(aParent));
    aUsers.DataSet.First;
    while not aUsers.EOF do
      begin
        if (aUsers.FieldByName('TYPE').AsString <> 'G') then
          begin
            if ((aUsers.FieldByName('LEAVED').AsString='') or (not (aUsers.FieldByName('LEAVED').AsDateTime < Now())))
            and (Data.Users.FieldByName('ACCOUNTNO').AsString <> aUsers.FieldByName('ACCOUNTNO').AsString)
            then
              begin
                Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
                TTreeEntry(Node1.Data).Typ := etCalendarUser;
                TTreeEntry(Node1.Data).Text[0] := aUsers.FieldByName('NAME').AsString;
                TTreeEntry(Node1.Data).Rec := aUsers.GetBookmark;
                TTreeEntry(Node1.Data).DataSource := Data.Users;
              end;
          end
        else
          begin
            bParent := aUsers.FieldByName('SQL_ID').AsVariant;
            if bParent <> aParent then
              CollectUsers(bParent);
          end;
        aUsers.Next;
      end;
    aUsers.Free;
  end;
begin
  if Data.Users.Rights.Right('CALENDAR') > RIGHT_NONE then
    begin
      Data.RegisterLinkHandler('CALENDAR',@fMainTreeFrame.OpenLink,TCalendar);
      Node := FCalendarNode;
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etMyCalendar;
      if Data.Users.FieldByName('POSITION').AsString = 'LEADER' then
        begin
          CollectUsers(Data.Users.FieldByName('PARENT').AsVariant);
        end;
    end;
end;

procedure TfCalendarFrame.DoRefresh;
begin
  DataStoreDateChanged(DataStore,DataStore.Date);
end;
procedure TfCalendarFrame.DataStoreDateChanged(Sender: TObject; Date: TDateTime
  );
const
  NumDays = 1;
var
  aFilter: String;
  bFilter: String;
begin
  if pDayView.Visible then
    aFilter := Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(aDirectory)+' AND ("STARTDATE" < '+Data.DateToFilter(Date+NumDays)+') AND (("ENDDATE" > '+Data.DateToFilter(Date-NumDays)+') OR ("ROTATION" > 0))'
  else if MonthView.Visible then
    aFilter := Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(aDirectory)+' AND ("STARTDATE" < '+Data.DateToFilter(EndOfTheMonth(Date)+7)+') AND (("ENDDATE" > '+Data.DateToFilter(StartOfTheMonth(Date)-7)+') OR ("ROTATION" > 0))'
  else if WeekView.Visible then
    aFilter := Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(aDirectory)+' AND ("STARTDATE" < '+Data.DateToFilter(EndOfTheWeek(Date)+1)+') AND ("ENDDATE" > '+Data.DateToFilter(StartOfTheWeek(Date)-1)+' OR "ROTATION" > 0)';
  with DataSet.DataSet as IBaseDbFilter do
    bFilter := Filter;
  if aFilter <> bFilter then
    begin
      Data.SetFilter(DataSet,aFilter);
      DataStore.Resource.Schedule.ClearEvents;
      DataStore.LoadEvents;
    end;
end;
procedure TfCalendarFrame.DayViewOwnerEditEvent(Sender: TObject;
  Event: TVpEvent; Resource: TVpResource; var AllowIt: Boolean);
var
  aEventEdit: TfEventEdit;
begin
  aEventEdit := TfEventEdit.Create(Self);
  AllowIt := aEventEdit.Execute(Event,Resource,DataStore.FDirectory,DataStore);
  aEventEdit.Free;
  RefreshCalendar(FCalendarNode);
end;

procedure TfCalendarFrame.MonthViewDblClick(Sender: TObject);
begin
  bWeekView.Down:=True;
  bWeekView.Click;
end;

procedure TfCalendarFrame.MonthViewEventDblClick(Sender: TObject;
  Event: TVpEvent);
var
  aEventEdit: TfEventEdit;
  AllowIt: Boolean;
begin
  aEventEdit := TfEventEdit.Create(Self);
  AllowIt := aEventEdit.Execute(Event,DataStore.Resource,DataStore.FDirectory,DataStore);
  aEventEdit.Free;
  RefreshCalendar(FCalendarNode);
end;

procedure TfCalendarFrame.WeekViewMouseWheel(Sender: TObject;
  Shift: TShiftState; Delta, XPos, YPos: Word);
begin
  if  (Delta = 120) then
    DataStore.Date:=DataStore.Date-1
  else if (Delta > 120) or (Delta < 0) then
    DataStore.Date:=DataStore.Date+1;
  if Sender = WeekView then
    WeekView.Date:=DataStore.Date
  else if Sender = MonthView then
    MonthView.Date:=DataStore.Date
end;

procedure TfCalendarFrame.DoOpen;
begin
  inherited DoOpen;
end;

procedure TfCalendarFrame.acDayViewExecute(Sender: TObject);
begin
  pDayView.Visible := True;
  MonthView.Visible := False;
  WeekView.Visible := False;
  DayView.Date:=DataStore.Date;
  if Sender <> nil then
    DataStoreDateChanged(DataStore,DataStore.Date);
end;
procedure TfCalendarFrame.acGotoTodayExecute(Sender: TObject);
begin
  DataStore.Date:=Now();
  if bDayView.Down then bDayView.OnClick(Self);
  if bWeekView.Down then bWeekView.OnClick(Self);
  if bMonthView.Down then bMonthView.OnClick(Self);
end;
procedure TfCalendarFrame.acMonthViewExecute(Sender: TObject);
begin
  pDayView.Visible := False;
  MonthView.Visible := True;
  WeekView.Visible := False;
  MonthView.Date:=DataStore.Date;
  if Sender <> nil then
    DataStoreDateChanged(DataStore,DataStore.Date);
end;
procedure TfCalendarFrame.acNewExecute(Sender: TObject);
var
  StartTime: Extended;
  EndTime: Extended;
  Event: TVpEvent;
  aEventEdit: TfEventEdit;
begin
  StartTime := trunc(MonthView.Date) + 1 / 2; { default to 12:00 noon }
  EndTime := StartTime + (30 / MinutesInDay); { StartTime + 30 minutes }
  Event := DataStore.Resource.Schedule.AddEvent(DataStore.GetNextID('Events'), StartTime, EndTime);
  aEventEdit := TfEventEdit.Create(Self);
  aEventEdit.Execute(Event,DataStore.Resource,DataStore.FDirectory,DataStore);
  aEventEdit.Free;
  DataStoreDateChanged(DataStore,DataStore.Date);
end;
procedure TfCalendarFrame.acWeekViewExecute(Sender: TObject);
begin
  pDayView.Visible := False;
  MonthView.Visible := False;
  WeekView.Visible := True;
  WeekView.Date:=DataStore.Date;
  if Sender <> nil then
    DataStoreDateChanged(DataStore,DataStore.Date);
end;
constructor TfCalendarFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DataSet := TCalendar.Create(Self,Data);
  DataSet.CreateTable;
  DataStore := TCustomPrometheusDataStore.Create(Self);
  DataStore.DataSet := TCalendar(DataSet);
  DataStore.OnDateChanged:=@DataStoreDateChanged;
  DayView.DrawingStyle:=dsFlat;
  DayView.TimeFormat := tf24Hour;
  WeekView.DrawingStyle:=dsNone;
  WeekView.TimeFormat := tf24Hour;
  MonthView.DrawingStyle:=dsNone;
  MonthView.TimeFormat := tf24Hour;
  DayView.DataStore := DataStore;
  WeekView.DataStore := DataStore;
  MonthView.DataStore := DataStore;
  DataStore.Resource := TVpResource.Create(DataStore.Resources);
  DataStore.Resource.Description:='';
  pDayView.Visible := False;
  MonthView.Visible := False;
  WeekView.Visible := True;
  MonthView.OnDblClick:=@MonthViewDblClick;
end;
destructor TfCalendarFrame.Destroy;
begin
  DataSet.Free;
  DataSet := nil;
  if Assigned(DataStore.Resource) then
    DataStore.Resource.Free;
  DataStore.Free;
  inherited Destroy;
end;
function TfCalendarFrame.OpenFromLink(aLink: string) : Boolean;
begin
  Result := False;
end;
procedure TfCalendarFrame.New;
begin
  acNew.Execute;
end;
procedure TfCalendarFrame.SetLanguage;
begin
  MonthView.LoadLanguage;
  WeekView.LoadLanguage;
  DayView.LoadLanguage;
end;
procedure TfCalendarFrame.OpenDir(Directory: LongInt);
begin
  aDirectory := IntToStr(Directory);
  DataStore.Directory:=Directory;
  DataStoreDateChanged(DataStore,DataStore.Date);
  DoOpen;
end;
constructor TCustomPrometheusDataStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.FConnected:=True;
end;
destructor TCustomPrometheusDataStore.Destroy;
begin

  inherited Destroy;
end;
function TCustomPrometheusDataStore.GetNextID(TableName: string): int64;
begin
  Result := Data.GetUniID;
end;
procedure TCustomPrometheusDataStore.LoadEvents;
var
  Event: TVpEvent;
begin
  if not DataSet.DataSet.Active then exit;
  with Dataset.DataSet do
    begin
      First;
      while not EOF do
        begin
          if (frac(FieldByName('STARTDATE').AsDateTime) = 0)
          and (DataSet.DataSet.FieldDefs.IndexOf('STARTTIME') > -1) then
            begin
              Event := Resource.Schedule.AddEvent(FieldByName('ID').AsLargeInt,
                                                  FieldByName('STARTDATE').AsDateTime+FieldByName('STARTTIME').AsFloat,
                                                  FieldByName('ENDDATE').AsDateTime+FieldByName('ENDTIME').AsFloat);
            end
          else
            begin
              Event := Resource.Schedule.AddEvent(FieldByName('ID').AsLargeInt,
                                                  FieldByName('STARTDATE').AsDateTime,
                                                  FieldByName('ENDDATE').AsDateTime);
            end;
          if Event <> nil then
            begin
              Event.Loading := true;
              Event.Description := FieldByName('SUMMARY').AsString;
              Event.Note := FieldByName('DESCR').AsString;
              Event.Category := FieldByName('ICATEGORY').AsInteger;
  //            Event.AlarmWavPath := FieldByName('DingPath').AsString;
              Event.AllDayEvent := FieldByName('ALLDAY').AsString = 'Y';
              Event.AlarmSet := FieldByName('ALARM').AsString = 'Y';
              Event.AlarmAdv := FieldByName('ALARMADV').AsInteger;
              Event.AlarmAdvType := TVpAlarmAdvType(FieldByName('ALARMADVTP').AsInteger);
              Event.SnoozeTime := FieldByName('SNOOZE').AsFloat;
              Event.RepeatCode := TVpRepeatType(FieldByName('ROTATION').AsInteger);
              Event.RepeatRangeEnd := FieldByName('ROTTO').AsDateTime;
              Event.CustInterval := FieldByName('ROTCUS').AsInteger;
              Event.Location:= FieldByName('LOCATION').AsString;
              Event.StrCategory:= FieldByName('CATEGORY').AsString;
              Event.Changed:=False;
              Event.Loading := false;
            end;
          Next;
        end;
    end;
end;
procedure TCustomPrometheusDataStore.RefreshEvents;
begin
  Resource.Schedule.ClearEvents;
  LoadEvents;
end;
procedure TCustomPrometheusDataStore.PostEvents;
var
  J: Integer;
  Event: TVpEvent;
  UpdateNode: Boolean = False;
  OldFilter: String;
begin
  if not DataSet.DataSet.Active then exit;
  with DataSet.DataSet as IBaseDbFilter do
    OldFilter := Filter;
  if (Resource <> nil) and Resource.EventsDirty then
    begin
      { Dump this resource's dirty events to the DB }
      for J := pred(Resource.Schedule.EventCount) downto 0 do
        begin
          Event := Resource.Schedule.GetEvent(J);
          { if the delete flag is set then delete it from the database }
          { and free the event instance }
          Data.SetFilter(DataSet,Data.QuoteField('ID')+'='+Data.QuoteValue(intToStr(Event.RecordID)));
          if Event.Deleted then
            begin
              if DataSet.DataSet.Locate('ID', Event.RecordID, [loCaseInsensitive]) then
                DataSet.DataSet.Delete;
              Event.Free;
              UpdateNode := True;
              Continue;
            end;
          if Event.Changed then
            begin
              with DataSet.DataSet do
                begin
                  if Locate('ID', Event.RecordID, [loCaseInsensitive]) then
                    { this event already exists in the database so update it }
                    Edit
                  else
                    Append;
                  try
                    { if a particular descendant datastore uses autoincrementing }
                    { RecordID fields, then  don't overwrite them here. }
                    if (Event.RecordID <> -1) and (Event.RecordID <> FieldByName('ID').AsInteger) then
                      FieldByName('ID').AsInteger := Event.RecordID;
                    FieldByName('REF_ID_ID').AsInteger := FDirectory;
                    if FieldDefs.IndexOf('USER') <> -1 then
                      FieldByName('USER').AsString:=Data.Users.FieldByName('ACCOUNTNO').AsString;
                    FieldByName('STARTDATE').AsDateTime := Event.StartTime;
                    FieldByName('ENDDATE').AsDateTime := Event.EndTime;
                    FieldByName('SUMMARY').AsString := Event.Description;
                    FieldByName('DESCR').AsString := Event.Note;
                    FieldByName('ICATEGORY').AsInteger := Event.Category;
    //                FieldByName('DingPath').AsString := Event.AlarmWavPath;
                    if Event.AllDayEvent then
                      FieldByName('ALLDAY').AsString := 'Y'
                    else
                      FieldByName('ALLDAY').AsString := 'N';
                    if Event.AlarmSet then
                      FieldByName('ALARM').AsString := 'Y'
                    else
                      FieldByName('ALARM').AsString := 'N';
                    FieldByName('ALARMADV').AsInteger := Event.AlarmAdv;
                    FieldByName('ALARMADVTP').AsInteger := Ord(Event.AlarmAdvType);
                    FieldByName('SNOOZE').AsFloat := Event.SnoozeTime;
                    FieldByName('ROTATION').AsInteger := Ord(Event.RepeatCode);
                    FieldByName('ROTTO').AsDateTime := Event.RepeatRangeEnd;
                    FieldByName('ROTCUS').AsInteger := Event.CustInterval;
                    FieldByName('LOCATION').AsString := Event.Location;
                    FieldByName('CATEGORY').AsString:=Event.StrCategory;
                    Post;
                  except
                    Cancel;
                  end;
                  { if a particular descendant datastore uses autoincrementing    }
                  { RecordID fields then the RecordID is assigned by the database }
                  { and needs to be assigned here...}
                  if Event.RecordID = -1 then
                    Event.RecordID := FieldByName('ID').AsInteger;
                  Event.Changed := false;
                  UpdateNode := True;
                end;
            end;
        end;
      Resource.EventsDirty := false;
      Resource.Schedule.Sort;
      if not Loading then
        NotifyDependents;
    end;
  if UpdateNode then
    begin
      Data.SetFilter(DataSet,OldFilter);
//      fCalendar.CalendarNode := fCalendar.CalendarNode;
    end;
end;
{$R *.lfm}
end.

