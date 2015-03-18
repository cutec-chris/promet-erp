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
Created 05.05.2013
*******************************************************************************}
unit uTaskPlan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, gsGanttCalendar, uTask, Math, types,
  uPrometFrames, uIntfStrConsts, Variants,ugridview,grids,dbgrids,utasks;

type

  { TPInterval }

  TPInterval = class(TInterval)
  private
    FCalcUsage : Extended;
    FUser: string;
    FUserID : Variant;
    FWorkTime: Extended;
    FUsage : Extended;
    procedure CalcUsage(aConnection : TComponent);
    function GetUsageCalced: Boolean;
    function GetWorkTime: Extended;
  protected
    procedure SetStartDate(const Value: TDateTime); override;
    procedure SetFinishDate(const Value: TDateTime); override;
    procedure SetNetTime(AValue: TDateTime); override;
    function GetPercentMoveRect: TRect; override;
    function GetUsage: Extended; override;
    procedure PrepareDrawRect; override;
  public
    constructor Create(AGantt: TgsGantt); override;
    procedure SetUser(AValue: string;aConnection : TComponent);
    property User : string read FUser;
    property Usage : Extended read FUsage write FUsage;
    property UserID : Variant read FUserID write FUserID;
    property WorkTime : Extended read GetWorkTime write FWorkTime;
    property UsageCalced : Boolean read GetUsageCalced;
  end;

  { TRessource }

  TRessource = class(TInterval)
  private
    FAccountno: string;
    FUserI: TPInterval;
    procedure SetAccountno(AValue: string);
  public
    property Accountno : string read FAccountno write SetAccountno;
    property User : TPInterval read FUserI write FUserI;
  end;

  { TBackInterval }

  TBackInterval = class(TPInterval)
  protected
    function GetUsage: Extended; override;
  end;

  { TfTaskPlan }

  TfTaskPlan = class(TPrometMainFrame)
    acShowProject: TAction;
    acShowInProjectGantt: TAction;
    acOpen: TAction;
    acCancel: TAction;
    acRefresh: TAction;
    acUse: TAction;
    ActionList1: TActionList;
    bDayView: TSpeedButton;
    Bevel5: TBevel;
    Bevel7: TBevel;
    bMonthView: TSpeedButton;
    bRefresh: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    iHourglass: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    lDate: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miUserOptions: TMenuItem;
    pTasks: TPanel;
    Panel4: TPanel;
    Panel9: TPanel;
    pgantt: TPanel;
    Panel7: TPanel;
    pmAction: TPopupMenu;
    pmUSer: TPopupMenu;
    pmTask: TPopupMenu;
    spTasks: TSplitter;
    tbTop: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acCancelExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acShowInProjectGanttExecute(Sender: TObject);
    procedure acShowProjectExecute(Sender: TObject);
    procedure acUseExecute(Sender: TObject);
    procedure aIGroupDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aUnfinishedList : TList = nil);
    procedure aINewDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aUnfinishedList : TList = nil);
    procedure aIDrawBackground(Sender: TObject; aCanvas: TCanvas; aRect: TRect;
      aStart, aEnd: TDateTime; aDayWidth: Double; RectColor, FillColor,
  ProbemColor: TColor; HighlightDay: TDateTime;aUnfinishedList : TList = nil);
    procedure aIDrawBackgroundWeekends(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double; aColor: TColor;
  HighlightDay: TDateTime);
    procedure aIntervalChanged(Sender: TObject);
    procedure aItemClick(Sender: TObject);
    procedure aSubItemClick(Sender: TObject);
    procedure bDayViewClick(Sender: TObject);
    procedure bMonthViewClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bShowTasksClick(Sender: TObject);
    procedure bTodayClick(Sender: TObject);
    procedure bWeekViewClick(Sender: TObject);
    procedure FGanttCalendarClick(Sender: TObject);
    procedure FGanttCalendarDblClick(Sender: TObject);
    procedure FGanttCalendarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FGanttCalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGanttCalendarMoveOverInterval(Sender: TObject;
      aInterval: TInterval; X, Y: Integer);
    procedure FGanttCalendarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure AddUserIntervals(Sender: TObject);
    procedure FGanttCalendarStartDateChanged(Sender: TObject);
    procedure FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
    procedure FGanttTreeResize(Sender: TObject);
    procedure miUserOptionsClick(Sender: TObject);
    procedure pmActionPopup(Sender: TObject);
    procedure TCollectThreadTerminate(Sender: TObject);
    procedure TIntervalChanged(Sender: TObject);
  private
    { private declarations }
    FSelectedCol: TDateTime;
    FSelectedRow: Int64;
    FGantt: TgsGantt;
    FTasks : TTaskList;
    FCollectedTo : TDateTime;
    FCollectedFrom : TDateTime;
    FHintRect : TRect;
    FRow : Integer;
    FThreads : TList;
    FUsers : TStringList;
    FOwners : TStringList;
    aClickPoint: types.TPoint;
    FSelectedUser : TInterval;
    FTaskView: TfTaskFrame;
    FSelectedInt : TInterval;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(aParent : Variant;aUser : Variant);
    procedure CollectResources(aResource: TRessource; aFrom, aTo: TDateTime;
      asUser: string; aConnection: TComponent; CollectTasks,
  CollectCalendar: Boolean;Processmessages : Boolean = True);
    function GetIntervalFromCoordinates(Gantt: TgsGantt; X, Y, Index: Integer): TInterval;
    function GetTaskIntervalFromCoordinates(Gantt: TgsGantt; X, Y, Index: Integer;
      IgnoreX: Boolean=False): TInterval;
    function GetTaskFromCoordinates(Gantt : TgsGantt;X,Y,Index : Integer) : string;
  end;

  { TCollectThread }

  TCollectThread = class(TThread)
  private
    FResource: TRessource;
    FUser: String;
    FPlan: TWinControl;
    FFrom: TDateTime;
    FTo: TDateTime;
    FAttatchTo: TInterval;
    FTasks: Boolean;
    FCalendar: Boolean;
    FTmpResource : TRessource;
    FProcessmessages: Boolean;
    procedure Attatch;
    procedure Plan;
  public
    procedure Execute; override;
    constructor Create(aPlan : TWinControl;aFrom,aTo : TDateTime;aResource : TRessource;asUser : string;aTasks,aCalendar,aProcessmessages : Boolean;AttatchTo : TInterval = nil);
  end;
function ChangeTask(aTasks: TTaskList;aTask : TInterval;DoChangeMilestones : Boolean = False;aReason : string = '';DoSetTermins : Boolean = True) : Boolean;
resourcestring
  strSaveTaskChanges                      = 'Um die Aufgabe zu bearbeiten müssen alle Änderungen gespeichert werden, Sollen alle Änderungen gespeichert werden ?';
  strChangeMilestones                     = 'Sollen Meilensteintermine auch geändert werden ?';
implementation
uses uData,LCLIntf,uBaseDbClasses,uProjects,uTaskEdit,LCLProc,uGanttView,uColors,
  uCalendar,uTaskPlanOptions,uBaseDBInterface;
{$R *.lfm}

{ TCollectThread }

procedure TCollectThread.Attatch;
var
  aRes: TInterval;
  found: Boolean;
  i: Integer;
begin
  try
    while FTmpResource.IntervalCount>0 do
      begin
        aRes := FTmpResource.Interval[FTmpResource.IntervalCount-1];
        found := False;
        for i := 0 to FResource.IntervalCount-1 do
          if FResource.Interval[i].Id=aRes.Id then
            begin
              Found := True;
              break;
            end;
        FTmpResource.RemoveInterval(aRes);
        if not Found then
          FResource.AddInterval(aRes)
        else aRes.Free;
      end;
    FAttatchTo.Pointer:=FResource;
  except
    //seems someonw has destroyed our form before were ready
  end;
end;

procedure TCollectThread.Plan;
var
  aConnection: Classes.TComponent;
  TaskPlan : TfTaskPlan;
begin
  TaskPlan.CollectResources(FTmpResource,FFrom,FTo,FUser,Data.MainConnection,FTasks,FCalendar,FProcessmessages);
end;

procedure TCollectThread.Execute;
begin
  FTmpResource:=TRessource.Create(FResource.Gantt);
  if not Terminated then
    Synchronize(@Plan);
  if not Terminated then
    Synchronize(@Attatch);
  FTmpResource.Free;
end;

constructor TCollectThread.Create(aPlan: TWinControl; aFrom, aTo: TDateTime;
  aResource: TRessource; asUser: string; aTasks, aCalendar,
  aProcessmessages: Boolean; AttatchTo: TInterval);
begin
  FPlan := aPlan;
  FFrom := aFrom;
  FTo := aTo;
  FTasks := aTasks;
  FProcessmessages := aProcessmessages;
  FCalendar := aCalendar;
  FResource := aResource;
  FUser := asUser;
  FAttatchTo := AttatchTo;
  if FProcessmessages then
    Priority:=tpLower;
  FreeOnTerminate:=True;
  inherited Create(True);
end;

{ TRessource }

procedure TRessource.SetAccountno(AValue: string);
var
  aUser: TUser;
begin
  FAccountno:=AValue;
end;

{ TBackInterval }

function TBackInterval.GetUsage: Extended;
begin
  Result:=0;
end;

procedure TPInterval.CalcUsage(aConnection : TComponent);
var
  aCal: TEvent;
  aRange: Extended;
  aDay: Integer;
  aUser: TUser;
begin
  if FUsage = -1 then
    begin
      aUser := TUser.CreateEx(nil,Data,aConnection);
      aUser.SelectByAccountno(FUser);
      aUser.Open;
      FUsage := aUser.FieldByName('USEWORKTIME').AsInteger/100;
      if FUsage = 0 then FUsage := 1;
      FWorkTime:=aUser.WorkTime*FUsage;
      FUsage := FWorkTime/8;
      FUserID:=aUser.Id.AsVariant;
      aUser.Free;
    end;
  if FinishDate-StartDate > 0 then
    FCalcUsage := (FinishDate-StartDate)
  else FCalcUsage := 0;
  if FCalcUsage=0 then exit;
  if FUser <> '' then
    begin
      aCal := TEvent.CreateEx(nil,Data,aConnection);
      if FUserID<>Null then
        begin
          aCal.SelectPlanedByIdAndTime(FUserId,StartDate,FinishDate);
          aCal.Open;
          while not aCal.EOF do
            begin
              for aDay := trunc(StartDate) to trunc(FinishDate-0.01) do
                begin
                  aRange := aCal.GetTimeInRange(aDay,aDay+1);
                  if (aRange > 1) then
                    aRange := 1;
                  FCalcUsage := FCalcUsage-aRange;
                end;
              aCal.Next;
            end;
        end;
      for aDay := trunc(StartDate) to trunc(FinishDate-0.01) do
        if ((DayOfWeek(aDay) = 1) or (DayOfWeek(aDay) = 7)) then
          FCalcUsage := FCalcUsage-1;
      aCal.Free;
    end;
  if FCalcUsage>0 then
    FCalcUsage:=((NetTime*8)/WorkTime)/FCalcUsage
  else if FCalcUsage=0 then
    FCalcUsage := 1
  else
    FCalcUsage := 10;
end;

function TPInterval.GetUsageCalced: Boolean;
begin
  Result := FCalcUsage<>-1;
end;

function TPInterval.GetWorkTime: Extended;
begin
  if FCalcUsage=-1 then
    CalcUsage(nil);
  Result := FWorkTime;
end;

procedure TPInterval.SetUser(AValue: string;aConnection : TComponent);
begin
  FUser:=AValue;
  FCalcUsage:=-1;
end;

procedure TPInterval.SetStartDate(const Value: TDateTime);
begin
  inherited SetStartDate(Value);
  FCalcUsage:=-1;
end;

procedure TPInterval.SetFinishDate(const Value: TDateTime);
begin
  inherited SetFinishDate(Value);
  FCalcUsage:=-1;
end;

procedure TPInterval.SetNetTime(AValue: TDateTime);
begin
  inherited SetNetTime(AValue);
  FCalcUsage:=-1;
end;

function TPInterval.GetPercentMoveRect: TRect;
begin
  Result:=inherited GetPercentMoveRect;
  Result.Right:=Result.Left;
end;

function TPInterval.GetUsage: Extended;
begin
  if FCalcUsage = -1 then
    CalcUsage(nil);
  Result:=FCalcUsage;
end;

procedure TPInterval.PrepareDrawRect;
begin
  inherited PrepareDrawRect;
  if ResourceTimePerDay > 0 then
    IntervalDone:=StartDate+(NetTime*(1/ResourceTimePerDay))
  else IntervalDone := StartDate;
end;

constructor TPInterval.Create(AGantt: TgsGantt);
begin
  inherited Create(AGantt);
  FUsage:=-1;
end;

function ChangeTask(aTasks: TTaskList; aTask: TInterval;
  DoChangeMilestones: Boolean; aReason: string; DoSetTermins: Boolean): Boolean;
var
  aTaskI: TTask;
  aTaskI2: TTask;
  i: Integer;
  changed: Boolean;
begin
  result := False;
  if aTasks.DataSet.Locate('SQL_ID',aTask.Id,[]) then
    begin
      //Add new Dependencies
      for i := 0 to aTask.ConnectionCount-1 do
        begin
          aTaskI2 := TTask.Create(nil);
          aTaskI2.Select(aTask.Connection[i].Id);
          aTaskI2.Open;
          aTaskI2.Dependencies.Open;
          if not aTaskI2.Dependencies.DataSet.Locate('REF_ID_ID',aTasks.Id.AsVariant,[]) then
            begin
              aTaskI := TTask.Create(nil);
              aTaskI.Select(aTasks.Id.AsVariant);
              aTaskI.Open;
              aTaskI2.Dependencies.Add(Data.BuildLink(aTaskI.DataSet));
              aTaskI.Free;
            end;
          aTaskI2.Free;
        end;
      //Change Task
      if ((aTasks.FieldByName('CLASS').AsString<>'M') or DoChangeMilestones) and (aTasks.FieldByName('COMPLETED').AsString<>'Y') then
        begin
          Result := aTasks.FieldByName('CLASS').AsString='M';
          if aTasks.FieldByName('SUMMARY').AsString <> aTask.Task then
            begin
              if not aTasks.CanEdit then
                aTasks.DataSet.Edit;
              aTasks.FieldByName('SUMMARY').AsString := aTask.Task;
            end;
          Changed := False;
          if DoSetTermins then
            begin
              if trunc(aTasks.FieldByName('STARTDATE').AsDateTime)<>trunc(aTask.StartDate) then
                begin
                  if not aTasks.CanEdit then
                    aTasks.DataSet.Edit;
                  aTasks.FieldByName('STARTDATE').AsDateTime := aTask.StartDate;
                  changed := True;
                end;
              if (trim(aTasks.FieldByName('DUEDATE').AsString)='') or (trunc(aTasks.FieldByName('DUEDATE').AsDateTime) <> trunc(aTask.FinishDate)) then
                begin
                  if not aTasks.CanEdit then
                    aTasks.DataSet.Edit;
                  aTasks.FieldByName('DUEDATE').AsDateTime := aTask.FinishDate;
                  changed := True;
                end;
            end;
          if aTasks.CanEdit then
            begin
              aTasks.DataSet.Post;
              if (trim(aReason)<>'') and Changed then
                aTasks.History.AddItem(Data.Users.DataSet,aReason,'','',atasks.DataSet,ACICON_USEREDITED,'',True,True);
            end;
        end;
      if Assigned(aTask.Parent) then
        ChangeTask(aTasks,aTask.Parent,DoChangeMilestones);
    end;
  aTask.Changed:=False;
end;
procedure TfTaskPlan.FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=180;
  fgantt.Tree.Cells[2,0]:=strUsers;
  fgantt.Tree.ColWidths[3]:=0;
  fgantt.Tree.ColWidths[4]:=0;
  fgantt.Tree.ColWidths[5]:=0;
  fgantt.Tree.ColWidths[6]:=0;
  fgantt.Tree.ColWidths[7]:=0;
  FGantt.Tree.Width:=190;
  FGantt.Tree.OnResize:=@FGanttTreeResize;
  FGantt.Tree.ShowHint:=True;
  FGantt.Tree.Options:=FGantt.Tree.Options+[goCellHints];
  FGantt.Tree.Options:=FGantt.Tree.Options-[goHorzLine];
  FGantt.Tree.Options:=FGantt.Tree.Options-[goEditing];
  FGantt.Tree.AlternateColor:=$00FFE6E6;
end;

procedure TfTaskPlan.FGanttTreeResize(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=FGantt.Tree.Width-FGantt.Tree.ColWidths[3]-FGantt.Tree.ColWidths[4]-FGantt.Tree.ColWidths[5];
  fgantt.Tree.ColWidths[6]:=0;
  fgantt.Tree.ColWidths[7]:=0;
end;

procedure TfTaskPlan.miUserOptionsClick(Sender: TObject);
var
  aUser: TUser;
  CurrInterval: TInterval;
begin
  aUser := TUser.Create(nil);
  CurrInterval := TInterval(FGantt.Tree.Objects[0, FGantt.Tree.Row]);
  if Assigned(CurrInterval) and (CurrInterval is TPInterval) and (TPInterval(CurrInterval).User<>'') then
    begin
      aUser.SelectByAccountno(TPInterval(CurrInterval).User);
      aUser.Open;
      if fUserPlanOptions.Execute(aUser) then
        TPInterval(CurrInterval).SetUser(aUser.FieldByName('ACCOUNTNO').AsString,nil);
    end;
  aUser.Free;
end;

procedure TfTaskPlan.pmActionPopup(Sender: TObject);
var
  aInt: TInterval;
  aItem: TMenuItem;
  aSubItem: TMenuItem;
  i: Integer;
  bItem: TMenuItem;
  aPos: TPoint;
begin
  aClickPoint := FGantt.Calendar.ScreenToClient(Mouse.CursorPos);
  aPos := aClickPoint;
  pmAction.Items.Clear;
  for i := 0 to 100 do
    begin
      aInt := GetIntervalFromCoordinates(FGantt,aPos.X,aPos.Y,i);
      if Assigned(aInt) then
        begin
          aItem := TMenuItem.Create(pmAction);
          aItem.Caption:=aInt.Task;
          aItem.Tag:=i;
          pmAction.Items.Add(aItem);
          aSubItem := TMenuItem.Create(pmAction);
          aSubItem.Action:=acOpen;
          aSubItem.Tag:=i;
          aSubItem.OnClick:=@aSubItemClick;
          aItem.Add(aSubItem);
          FSelectedInt:=aInt;
          if aInt.Project<>'' then
            begin
              aSubItem := TMenuItem.Create(pmAction);
              aSubItem.Action:=acShowInProjectGantt;
              aSubItem.Tag:=i;
              aSubItem.OnClick:=@aSubItemClick;
              aItem.Add(aSubItem);
              aSubItem := TMenuItem.Create(pmAction);
              aSubItem.Action:=acShowProject;
              aSubItem.Tag:=i;
              aSubItem.OnClick:=@aSubItemClick;
              aItem.Add(aSubItem);
            end;
        end
      else break;
    end;
  if pmAction.Items.Count=1 then
    begin
      pmAction.Items.Remove(aItem);
      while aItem.Count>0 do
        begin
          bItem := aItem.Items[0];
          aItem.Remove(bItem);
          pmAction.Items.Add(bItem);
        end;
    end;
end;

procedure TfTaskPlan.TCollectThreadTerminate(Sender: TObject);
begin
  if FThreads.IndexOf(Sender) > -1 then
    FThreads.Remove(Sender);
  FGantt.Invalidate;
  iHourglass.Visible:=FThreads.Count>0;
end;

procedure TfTaskPlan.TIntervalChanged(Sender: TObject);
begin
  TInterval(TInterval(Sender).Pointer2).StartDate:=TInterval(Sender).StartDate;
  TInterval(TInterval(Sender).Pointer2).FinishDate:=TInterval(Sender).FinishDate;
  acUse.Enabled:=True;
  acCancel.Enabled:=True;
end;

function TfTaskPlan.GetTaskFromCoordinates(Gantt: TgsGantt; X, Y,Index: Integer
  ): string;
var
  aInt: gsGanttCalendar.TInterval;
begin
  aInt := GetTaskIntervalFromCoordinates(Gantt,X,Y,Index);
  if (aInt = nil) or (aInt.Id=Unassigned) then
    aInt := GetIntervalFromCoordinates(Gantt,X,Y,Index);
  if Assigned(aInt) then
    if aInt.Id <> Null then
      begin
        Result := 'TASKS@'+VarToStr(aInt.Id);
      end;
end;

procedure TfTaskPlan.bDayViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsWeekNum;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfTaskPlan.aIntervalChanged(Sender: TObject);
var
  i: Integer;
  oD: TDateTime;
  a: Integer;
begin
  with TInterval(Sender) do
    begin
      if FinishDate<(StartDate+Duration) then
        FinishDate := (StartDate+Duration);
      IntervalDone:=StartDate;
      for i := 0 to ConnectionCount-1 do
        begin
          oD := Connection[i].Duration;
          if Connection[i].StartDate<FinishDate+WaitTime then
            Connection[i].StartDate:=FinishDate+WaitTime;
          if Connection[i].FinishDate<Connection[i].StartDate+oD then
            Connection[i].FinishDate:=Connection[i].StartDate+oD;
          Connection[i].IntervalDone:=Connection[i].StartDate;
        end;
    end;
end;

procedure TfTaskPlan.aItemClick(Sender: TObject);
var
  List: TList;
  i: Integer;
  ay: Integer;
begin
  FSelectedInt := nil;
  List := TList.Create;
  FGantt.MakeIntervalList(List);
  for i := 0 to List.Count-1 do
    begin
      TInterval(List[i]).StartDate:=Now()-(365*10);
      TInterval(List[i]).FinishDate:=Now()-(365*10);
    end;
  ay := aClickPoint.Y-FGantt.Calendar.StartDrawIntervals;
  ay := ay div max(FGantt.Calendar.PixelsPerLine,1);
  ay := ay+(FGantt.Tree.TopRow-1);
  if (ay<List.Count) and (ay>-1) then
    if (Assigned(TInterval(List[ay]).Pointer)) and (TInterval(List[ay]).Pointer2<>Pointer(TMenuItem(Sender).Tag)) then
      begin
        TInterval(List[ay]).StartDate:=TInterval(TMenuItem(Sender).Tag).StartDate;
        TInterval(List[ay]).FinishDate:=TInterval(TMenuItem(Sender).Tag).FinishDate;
        TInterval(List[ay]).Pointer2 := TInterval(TMenuItem(Sender).Tag);
        TInterval(List[ay]).Color := TInterval(TMenuItem(Sender).Tag).Color;
        TInterval(List[ay]).Fixed := TInterval(TMenuItem(Sender).Tag).Fixed;
        FSelectedUser := TInterval(List[ay]);
        FSelectedInt := TInterval(TMenuItem(Sender).Tag);
        TInterval(List[ay]).OnChanged:=@TIntervalChanged;
        FGantt.Invalidate;
      end;
  List.Free;
end;

procedure TfTaskPlan.aSubItemClick(Sender: TObject);
begin
  TMenuItem(Sender).Action.Tag:=TMenuItem(Sender).Tag;
end;

procedure TfTaskPlan.aINewDrawBackground(Sender: TObject; aCanvas: TCanvas;
  aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;
  aUnfinishedList: TList);
begin
  aIDrawBackgroundWeekends(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,$e0e0e0,FSelectedCol);
  aIDrawBackground(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,clBlue,clLime,clRed,FSelectedCol,aUnfinishedList);
end;

procedure TfTaskPlan.aIDrawBackground(Sender: TObject; aCanvas: TCanvas;
  aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double; RectColor,
  FillColor, ProbemColor: TColor; HighlightDay: TDateTime;
  aUnfinishedList: TList);
var
  i: Integer;
  aDay: TDateTime;
  aResource: TRessource;
  aIStart: TDateTime;
  aIEnd: TDateTime;
  aRowTop: Integer;
  aAddTop : Integer = 0;
  aDst: TRect;
  a: Integer;
  Rect1: Classes.TRect;
  Rect2: Classes.TRect;
  cRect: TRect;
  WholeUsage: Extended;
  cHeight: Integer;
  procedure PaintRect(bCanvas : TCanvas;bRect : Trect;aInterval : TInterval);
  var
    cRect: types.TRect;
    aUsage: Extended;
  begin
    with bCanvas do
      begin
        Pen.Style := psSolid;
        Brush.Style := bsSolid;
        Brush.Color:=clWindow;
        Pen.Color:=RectColor;
        Rectangle(bRect);
        cRect := bRect;
        aUsage := (aInterval.PercentUsage);
        if aUsage>1 then
          Brush.Color:=ProbemColor
        else
          Brush.Color:=FillColor;
        if trunc(aDay)=trunc(HighlightDay) then
          aCanvas.Brush.Color:=Ligthen(aCanvas.Brush.Color,0.9);
        if Assigned(aInterval.Parent) and (aInterval.Parent is TRessource) and Assigned(TRessource(aInterval.Parent).User) then
          aUsage := aUsage*(1/TRessource(aInterval.Parent).User.FUsage);
        cRect.Top := cRect.Bottom-round((cRect.Bottom-cRect.Top-1)*aUsage);
        if cRect.Top<=bRect.Top then
          begin
            cRect.Top := bRect.Top+1;
          end;
        cRect.left := bRect.Left+1;
        pen.Style:=psClear;
        Rectangle(cRect);
      end;
  end;
  procedure MarkRect(bCanvas : TCanvas;bRect : Trect);
  begin
    with bCanvas do
      begin
        Pen.Style := psSolid;
        Brush.Color:=RectColor;
        pen.Color:=RectColor;
        Brush.Style:=bsFDiagonal;
        FillRect(aDst);
        Brush.Style:=bsSolid;
      end;
  end;
begin
  if Assigned(TInterval(Sender).Pointer) then
    begin
      aResource := TRessource(TInterval(Sender).Pointer);
      for i := 0 to aResource.IntervalCount-1 do
        if not (aResource.Interval[i] is TBackInterval) then
          aResource.Interval[i].ClearDrawRect;
    end;
  for i := 0 to round(aEnd-aStart) do
    begin
      aDay := aStart+i;
      if not ((DayOfWeek(aDay) = 1) or (DayOfWeek(aDay) = 7)) then
        begin
          WholeUsage := 0;
          if Assigned(TInterval(Sender).Pointer) then
            begin
              for a := 0 to aResource.IntervalCount-1 do
                if not (aResource.Interval[a] is TBackInterval) then
                  begin
                    if ((aResource.Interval[a].StartDate>aDay) and (aResource.Interval[a].StartDate<(aDay+1)))
                    or ((aResource.Interval[a].FinishDate>aDay) and (aResource.Interval[a].FinishDate<(aDay+1)))
                    or ((aResource.Interval[a].StartDate<=aDay) and (aResource.Interval[a].FinishDate>=(aDay+1)))
                    then
                      begin
                        if (aResource.Interval[a] is TPInterval) and TPInterval(aResource.Interval[a]).UsageCalced then
                          WholeUsage += aResource.Interval[a].PercentUsage
                        else if Assigned(aUnfinishedList) then
                          aUnfinishedList.Add(aResource.Interval[a])
                        else  WholeUsage += aResource.Interval[a].PercentUsage;
                        if aResource.Interval[a].IsDrawRectClear then
                          begin
                            aIStart := aResource.Interval[a].StartDate;
                            if aStart > aIStart then aIStart := aStart;
                            aIEnd := aResource.Interval[a].FinishDate;
                            if aEnd < aIEnd then aIEnd := aEnd;
                            if aIEnd<=aIStart then aIEnd := aIStart+1;
                            aIStart := trunc(aIStart);
                            aIEnd:=trunc(aIEnd)+1;
                            aResource.Interval[a].DrawRect:=Rect(round((aIStart-aStart)*aDayWidth),(aRect.Top+((aRect.Bottom-aRect.Top) div 4)-1)+aAddTop,round((aIEnd-aStart)*aDayWidth)-1,(aRect.Bottom-((aRect.Bottom-aRect.Top) div 4)-1)+aAddTop);
                          end;
                      end;
                  end;
              if WholeUsage>1.01 then
                aCanvas.Brush.Color:=ProbemColor
              else
                aCanvas.Brush.Color:=FillColor;
            end;
          if trunc(aDay)=trunc(HighlightDay) then
            aCanvas.Brush.Color:=Ligthen(aCanvas.Brush.Color,0.9);
          cRect := rect(round(i*aDayWidth),aRect.Top+1,round((i*aDayWidth)+aDayWidth),aRect.Bottom);
          cHeight := cRect.Bottom-cRect.Top;
          if WholeUsage<1 then
            cHeight := round(cHeight*WholeUsage);
          cRect.Top := cRect.Bottom-cHeight;
          aCanvas.FillRect(crect);
        end;
    end;
end;

procedure TfTaskPlan.aIDrawBackgroundWeekends(Sender: TObject;
  aCanvas: TCanvas; aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aColor : TColor;HighlightDay : TDateTime);
var
  i: Integer;
  aDay: Extended;
  aResource: TRessource;
  aIStart: TDateTime;
  aIEnd: TDateTime;
  bRect : Trect;
begin
  aCanvas.Brush.Style:=bsSolid;
  aCanvas.Brush.Color:=clWindow;
  aCanvas.FillRect(aRect);
  if Assigned(TInterval(Sender).Gantt) then
    aCanvas.Pen.Color:=TInterval(Sender).Gantt.Tree.GridLineColor
  else
    aCanvas.Pen.Color:=clBtnFace;
  aCanvas.Pen.Style:=psSolid;
  aCanvas.MoveTo(aRect.Left,aRect.Top);
  aCanvas.LineTo(aRect.Right,aRect.Top);
  for i := 0 to round(aEnd-aStart) do
    begin
      aDay := aStart+i;
      if trunc(aDay)=trunc(HighlightDay) then
        aCanvas.Brush.Color:=Ligthen(aColor,0.8)
      else
        aCanvas.Brush.Color:=aColor;
      if (DayOfWeek(aDay) = 1) or (DayOfWeek(aDay) = 7) then
        aCanvas.FillRect(round(i*aDayWidth),aRect.Top+1,round((i*aDayWidth)+aDayWidth),aRect.Bottom)
      else if trunc(aDay)=trunc(HighlightDay) then
        begin
          aCanvas.Brush.Color:=Ligthen(clSkyBlue,0.97);
          aCanvas.FillRect(round(i*aDayWidth),aRect.Top+1,round((i*aDayWidth)+aDayWidth),aRect.Bottom)
        end;
      aCanvas.Brush.Color:=clSkyBlue;
      if (trunc(aDay) = trunc(Now())) then
        aCanvas.FillRect(round(i*aDayWidth),aRect.Top+1,round((i*aDayWidth)+aDayWidth),aRect.Bottom);
    end;
  if Assigned(TInterval(Sender).Pointer) then
    begin
      aResource := TRessource(TInterval(Sender).Pointer);
      for i := 0 to aResource.IntervalCount-1 do
        if aResource.Interval[i] is TBackInterval then
          begin
            aResource.Interval[i].ClearDrawRect;
            if ((aResource.Interval[i].StartDate>aStart) and (aResource.Interval[i].StartDate<aEnd))
            or ((aResource.Interval[i].FinishDate>aStart) and (aResource.Interval[i].FinishDate<aEnd))
            or ((aResource.Interval[i].StartDate<=aStart) and (aResource.Interval[i].FinishDate>=aEnd))
            then
              begin
                aIStart := aResource.Interval[i].StartDate;
                if aStart > aIStart then aIStart := aStart;
                aIEnd := aResource.Interval[i].FinishDate;
                if aEnd < aIEnd then aIEnd := aEnd;
                if aIEnd<=aIStart then aIEnd := aIStart+1;
                bRect :=Rect(round((aIStart-aStart)*aDayWidth),aRect.Top+1,round((aIEnd-aStart)*aDayWidth),aRect.Bottom);
                aResource.Interval[i].DrawRect:=bRect;
                if aResource.Interval[i].Color<>clBlue then
                  aCanvas.Brush.Color:=aResource.Interval[i].Color
                else
                  aCanvas.Brush.Color:=aColor;
                aCanvas.FillRect(aResource.Interval[i].DrawRect);
              end;
          end;
    end;
end;

procedure TfTaskPlan.acShowProjectExecute(Sender: TObject);
var
  aTask: TTask;
  aProject: TProject;
  aLink: String;
begin
  aLink := GetTaskFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,TMenuItem(Sender).Tag);
  aTask := TTask.Create(nil);
  aTask.SelectFromLink(aLink);
  aTask.Open;
  if aTask.Count>0 then
    begin
      aProject := TProject.Create(nil);
      aProject.Select(aTask.FieldByName('PROJECTID').AsVariant);
      aProject.Open;
      if aProject.Count>0 then
        Data.GotoLink(Data.BuildLink(aProject.DataSet));
      aProject.Free;
    end;
  aTask.Free;
end;

procedure TfTaskPlan.acUseExecute(Sender: TObject);
var
  SetMileStones : Boolean = True;
  Asked : Boolean = False;
  procedure RecoursiveChange(aParent : TInterval);
  var
    i: Integer;
    aTasks: TTask;
  begin
    for i := 0 to aParent.IntervalCount-1 do
      begin
        RecoursiveChange(aParent.Interval[i]);
      end;
    if Assigned(aParent.Pointer) then
      begin
        for i := 0 to TInterval(aParent.Pointer).IntervalCount-1 do
          begin
            if TRessource(aParent.Pointer).Interval[i].Changed then
              begin
                aTasks := TTask.Create(nil);
                aTasks.Select(TRessource(aParent.Pointer).Interval[i].Id);
                aTasks.Open;
                debugln('changing '+TRessource(aParent.Pointer).Interval[i].Task);
                if (aTasks.FieldByName('CLASS').AsString='M') and (not Asked) then
                  begin
                    SetMileStones:=MessageDlg('',strChangeMilestones,mtConfirmation,[mbYes,mbNo],0) = mrYes;
                    Asked:=True;
                  end;
                ChangeTask(aTasks,TRessource(aParent.Pointer).Interval[i],SetMileStones);
                aTasks.Free;
              end;
          end;
      end;
  end;
var
  i: Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    begin
      RecoursiveChange(FGantt.Interval[i]);
    end;
  acUse.Enabled:=False;
  acCancel.Enabled:=False;
end;

procedure TfTaskPlan.aIGroupDrawBackground(Sender: TObject; aCanvas: TCanvas;
  aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;
  aUnfinishedList: TList);
var
  aDay: Integer;
  aInt: TInterval;
  WorkTimes: Extended;
  b: Integer;
  aTime: Extended;
  aUsage: Extended;
  function CollectWorkTimes(aInt : TInterval) : Extended;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to aInt.IntervalCount-1 do
      begin
        if aInt.Interval[i].IntervalCount>0 then
          Result := Result+CollectWorkTimes(aInt.Interval[i])
        else if aInt.Interval[i] is TPInterval then
          Result := Result+TPInterval(aInt.Interval[i]).WorkTime;
      end;
  end;
  function CollectTimeAtDay(Day : Integer;aInt : TInterval) : Extended;
  var
    aResource: TRessource;
    i: Integer;
    a: Integer;
    bTime: Extended;
  begin
    Result := 0;
    for i := 0 to aInt.IntervalCount-1 do
      begin
        if aInt.Interval[i].IntervalCount>0 then
          Result := Result+CollectTimeAtDay(Day,aInt.Interval[i])
        else if aInt.Interval[i] is TPInterval then
          begin
            if Assigned(TInterval(aInt.Interval[i]).Pointer) then
              begin
                aResource := TRessource(TInterval(aInt.Interval[i]).Pointer);
                for a := 0 to aResource.IntervalCount-1 do
                  begin
                    if aResource.Interval[a].FinishDate-aResource.Interval[a].StartDate>0 then
                      begin
                        bTime :=TPInterval(aResource.Interval[a]).PercentUsage*(aResource.Interval[a].FinishDate-aResource.Interval[a].StartDate);
                        bTime := bTime*TimeRangeOverlap(Day,Day+1,aResource.Interval[a].StartDate,aResource.Interval[a].FinishDate)/(aResource.Interval[a].FinishDate-aResource.Interval[a].StartDate);
                        bTime := bTime*TPInterval(aInt.Interval[i]).WorkTime;
                        //bTime := bTime*(1/TPInterval(aInt.Interval[i]).FUsage);
                        //if bTime>TPInterval(aInt.Interval[i]).WorkTime then
                        //  bTime := TPInterval(aInt.Interval[i]).WorkTime;
                        Result := Result+bTime;
                      end;
                  end;
              end;
          end;
      end;
  end;
begin
  aCanvas.Brush.Style:=bsSolid;
  aCanvas.Brush.Color:=$00FFE6E6;
  aCanvas.FillRect(aRect);
  aInt := TInterval(Sender);
  WorkTimes := CollectWorkTimes(TInterval(Sender));
  for aDay := trunc(aStart) to trunc(aEnd) do
    if ((DayOfWeek(aDay) <> 1) and (DayOfWeek(aDay) <> 7)) then
      begin
        b := aDay-trunc(aStart);
        aTime := CollectTimeAtDay(aDay,aInt);
        aCanvas.Brush.Color:=clLime;
        if aTime>WorkTimes then
          begin
            aTime := WorkTimes;
            aCanvas.Brush.Color:=clred;
          end;
        if WorkTimes>0 then
          aUsage := (aTime/WorkTimes)
        else aUsage := 0;
        aCanvas.FillRect(round(b*aDayWidth),aRect.Bottom-round(aUsage*(aRect.Bottom-aRect.Top)),round((b*aDayWidth)+aDayWidth),aRect.Bottom);
      end;
  {
  if Assigned(TInterval(Sender).Gantt) then
    aCanvas.Pen.Color:=TInterval(Sender).Gantt.Tree.GridLineColor
  else
    aCanvas.Pen.Color:=clBtnFace;
  aCanvas.Pen.Style:=psSolid;
  aCanvas.MoveTo(aRect.Left,aRect.Top);
  aCanvas.LineTo(aRect.Right,aRect.Top);
  }
end;

procedure TfTaskPlan.acShowInProjectGanttExecute(Sender: TObject);
var
  aTask: TTask;
  aProject: TProject;
  aLink: String;
begin
  aLink := GetTaskFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,TMenuItem(Sender).Tag);
  aTask := TTask.Create(nil);
  aTask.SelectFromLink(aLink);
  aTask.Open;
  if aTask.Count>0 then
    begin
      aProject := TProject.Create(nil);
      aProject.Select(aTask.FieldByName('PROJECTID').AsVariant);
      aProject.Open;
      if aProject.Count>0 then
        begin
          aProject.Tasks.SelectActivewithoutDeps;
          aProject.Tasks.Open;
          fGanttView.Execute(aProject,aLink);
        end;
      aProject.Free;
    end;
  aTask.Free;
end;

procedure TfTaskPlan.acOpenExecute(Sender: TObject);
var
  aLink: String;
  aEdit: TfTaskEdit;
  aInt: gsGanttCalendar.TInterval;
  aTask: TTask;
  gView : TfGanttView;
begin
  if Assigned(FSelectedInt) then
    begin
      aEdit :=TfTaskEdit.Create(Self);
      if aEdit.Execute('TASKS@'+VarToStr(FSelectedInt.Id)) then
        begin
          bRefresh.Click;
        end;
      aEdit.Free;
    end;
end;

procedure TfTaskPlan.acCancelExecute(Sender: TObject);
begin
  bRefreshClick(Sender);
end;

procedure TfTaskPlan.bMonthViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsQuarter;
  FGantt.MinorScale:=tsMonth;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfTaskPlan.bRefreshClick(Sender: TObject);
  procedure RefreshRes(aInt : TInterval);
  var
    i: Integer;
    aUser: String;
    tmpRes: TRessource;
    aIsub: TInterval;
  begin
    for i := 0 to aInt.IntervalCount-1 do
      RefreshRes(aInt.Interval[i]);
    if Assigned(aInt.Pointer) then
      begin
        aUser := TPInterval(aInt).User;
        TInterval(aInt.Pointer).Free;
        aInt.Pointer := nil;
        tmpRes := TRessource.Create(nil);
        tmpRes.User:=TPInterval(aInt);
        FThreads.Add(TCollectThread.Create(Self,FGantt.Calendar.VisibleStart,FGantt.Calendar.VisibleFinish,tmpRes,aUser,True,True,True,aInt));
        TCollectThread(FThreads[FThreads.Count-1]).OnTerminate:=@TCollectThreadTerminate;
        TCollectThread(FThreads[FThreads.Count-1]).Resume;
        aInt.Opened:=False;
        while aInt.IntervalCount>0 do
          aInt.Interval[0].Free;
        aIsub := TInterval.Create(FGantt);
        aInt.AddInterval(aISub);
      end;
  end;
var
  i: Integer;
begin
  if acUse.Enabled then
    if (MessageDlg(strTaskPlan,strItemnotSaved,mtInformation,[mbYes,mbNo],0) = mrYes) then
      acUse.Execute;
  FCollectedFrom:=Now();
  FCollectedTo:=FGantt.Calendar.VisibleFinish;
  for i := 0 to FGantt.IntervalCount-1 do
    RefreshRes(FGantt.Interval[i]);
  iHourglass.Visible:=True;
end;

procedure TfTaskPlan.bShowTasksClick(Sender: TObject);
begin
  FGantt.Calendar.Invalidate;
end;

procedure TfTaskPlan.bTodayClick(Sender: TObject);
begin
  FGantt.StartDate:=Now();
end;

procedure TfTaskPlan.bWeekViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsMonth;
  FGantt.MinorScale:=tsWeekNumPlain;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfTaskPlan.FGanttCalendarClick(Sender: TObject);
var
  aInt: TInterval;
  List: TList;
  ay: Integer;
  i: Integer;
  aItem: TMenuItem;
begin
  pmTask.Items.Clear;
  aClickPoint := FGantt.Calendar.ScreenToClient(Mouse.CursorPos);
  for i := 0 to 100 do
    begin
      aInt := GetIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,i);
      if not Assigned(aInt) then break;
      if Assigned(aInt) then
        begin
          aItem := TMenuItem.Create(pmTask);
          aItem.Caption:=aInt.Task+' ('+IntToStr(round(aInt.PercentUsage*100))+'%)';
          aItem.Tag:=PtrInt(aInt);
          aItem.OnClick:=@aItemClick;
          pmTask.Items.Add(aItem);
        end;
      if Assigned(FSelectedUser) and (aInt = TInterval(FSelectedUser.Pointer2)) then exit;
    end;
  if Assigned(FSelectedUser) then
    begin
      FSelectedUser.OnChanged:=nil;
      FSelectedUser.StartDate:=Now()-(365*10);
      FSelectedUser.FinishDate:=Now()-(365*10);
      FSelectedUser.Pointer2 := nil;
      FSelectedUser := nil;
      Invalidate;
    end;
  List := TList.Create;
  FGantt.MakeIntervalList(List);
  if pmTask.Items.Count=1 then
    begin
      aInt := GetIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,0);
      FSelectedInt := aInt;
      ay := aClickPoint.Y-FGantt.Calendar.StartDrawIntervals;
      ay := ay div max(FGantt.Calendar.PixelsPerLine,1);
      ay := ay+(FGantt.Tree.TopRow-1);
      if (ay<List.Count) and (ay>-1) then
        if (Assigned(TInterval(List[ay]).Pointer)) and Assigned(aInt) and (TInterval(List[ay]).Pointer2<>Pointer(aInt)) then
          begin
            TInterval(List[ay]).StartDate:=aInt.StartDate;
            TInterval(List[ay]).FinishDate:=aInt.FinishDate;
            TInterval(List[ay]).OnChanged:=@TIntervalChanged;
            TInterval(List[ay]).Pointer2:=aInt;
            FSelectedUser := TInterval(List[ay]);
            FGantt.Invalidate;
          end;
    end;
  List.Free;
end;

procedure TfTaskPlan.FGanttCalendarDblClick(Sender: TObject);
begin
  aClickPoint := FGantt.Calendar.ScreenToClient(Mouse.CursorPos);
  acOpen.Execute;
end;

procedure TfTaskPlan.FGanttCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ay: Integer;
  FtSelectedRow: Int64;
  FtSelectedCol: TDateTime;
begin
  lDate.Caption := DateToStr(FGantt.Calendar.VisibleStart+trunc((X/FGantt.Calendar.GetIntervalWidth)));
  FtSelectedRow := trunc((Y/FGantt.Calendar.GetIntervalHeight));
  FtSelectedCol := FGantt.Calendar.VisibleStart+trunc((X/FGantt.Calendar.GetIntervalWidth));
  if (FtSelectedCol<>FSelectedCol)
  or (FtSelectedRow<>FSelectedRow) then
    begin
      FSelectedCol := FtSelectedCol;
      FSelectedRow := FtSelectedRow;
      FGantt.Calendar.Invalidate;
    end;
  if fHintRect.Left>-1 then
    begin
      if (X<FHintRect.Left)
      or (X>FHintRect.Right)
      then
        begin
          FHintRect.Left:=-1;
          Application.CancelHint;
        end;
    end;
end;

procedure TfTaskPlan.FGanttCalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
    FGanttCalendarClick(FGantt.Calendar);
end;

procedure TfTaskPlan.FGanttCalendarMoveOverInterval(Sender: TObject;
  aInterval: TInterval; X, Y: Integer);
begin
  if (not Assigned(aInterval)) then
    begin
      FGantt.Calendar.Hint:='';
      exit;
    end;
  if FGantt.Calendar.Hint=aInterval.Task then exit;
  FHintRect:=aInterval.DrawRect;
  FGantt.Calendar.Hint:=aInterval.Task+LineEnding+aInterval.Resource;
  FGantt.Calendar.ShowHint:=True;
end;

procedure TfTaskPlan.FGanttCalendarShowHint(Sender: TObject;
  HintInfo: PHintInfo);
  function IsInRect(X, Y: Integer; R: TRect): Boolean;
  begin
    Result := (X >= R.Left) and (X <= R.Right); //and (Y >= R.Top) and (Y <= R.Bottom);
    FHintRect := R;
  end;
  function IsInRow(X, Y: Integer; R: TRect): Boolean;
  begin
    Result := (Y >= R.Top) and (Y <= R.Bottom);
  end;
var
  ay: Integer;
  i: Integer;
  List : TList;
  aPercent : Integer = 0;
  DD: String;
begin
  if HintInfo^.HintStr='' then
    begin
      List := TList.Create;
      FGantt.MakeIntervalList(List);
      ay := HintInfo^.CursorPos.Y-FGantt.Calendar.StartDrawIntervals;
      ay := ay div max(FGantt.Calendar.PixelsPerLine,1);
      ay := ay+(FGantt.Tree.TopRow-1);
      HintInfo^.HintStr := '';
      HintInfo^.HideTimeout:=30000;
      if (ay<List.Count) and (ay>-1) then
        if Assigned(TInterval(List[ay]).Pointer) then
          begin
            for i := 0 to TRessource(TInterval(List[ay]).Pointer).IntervalCount-1 do
              if not TRessource(TInterval(List[ay]).Pointer).Interval[i].IsDrawRectClear then
                if IsInRect(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y,TRessource(TInterval(List[ay]).Pointer).Interval[i].DrawRect) then
                  begin
                    aPercent := aPercent+round(TRessource(TInterval(List[ay]).Pointer).Interval[i].PercentUsage*100);
                    if not (TRessource(TInterval(List[ay]).Pointer).Interval[i] is TBackInterval) then
                      begin
                        if HintInfo^.HintStr <> '' then HintInfo^.HintStr := HintInfo^.HintStr+lineending;
                        if TRessource(TInterval(List[ay]).Pointer).Interval[i].DepDone then
                          DD := ' '
                        else DD := 'x';
                        HintInfo^.HintStr := HintInfo^.HintStr+IntToStr(round(TRessource(TInterval(List[ay]).Pointer).Interval[i].PercentUsage*100))+'% '+DD+' '+TRessource(TInterval(List[ay]).Pointer).Interval[i].Task+'-'+TRessource(TInterval(List[ay]).Pointer).Interval[i].Project;
                      end
                    else
                      begin
                        if HintInfo^.HintStr <> '' then HintInfo^.HintStr := HintInfo^.HintStr+lineending;
                        HintInfo^.HintStr := HintInfo^.HintStr+TRessource(TInterval(List[ay]).Pointer).Interval[i].Task;
                      end;
                  end;
          end;
      if HintInfo^.HintStr <> '' then
        HintInfo^.HintStr := TRessource(TInterval(List[ay]).Pointer).Resource+' '+Format(strFullPercent,[aPercent])+LineEnding+HintInfo^.HintStr;
      List.Free;
    end;
end;

procedure TfTaskPlan.AddUserIntervals(Sender: TObject);
  procedure AddRes(aInt : TInterval);
  var
    bInt: TInterval;
    i: Integer;
    aNew: TInterval;
  begin
    if Assigned(aInt.Pointer) then
      begin
        FGantt.BeginUpdate;
        while aInt.IntervalCount>0 do
          begin
            bInt := aInt.Interval[aInt.IntervalCount-1];
            aInt.RemoveInterval(bInt);
            if (bInt is TInterval) then
              bInt.Free;
          end;
        for i := TRessource(aInt.Pointer).IntervalCount-1 downto 0 do
          if TRessource(aInt.Pointer).Interval[i].Project<>'CAL' then
            if ((TRessource(aInt.Pointer).Interval[i].StartDate>FGantt.Calendar.VisibleStart)
            and (TRessource(aInt.Pointer).Interval[i].FinishDate<FGantt.Calendar.VisibleFinish))
            or ((TRessource(aInt.Pointer).Interval[i].StartDate>FGantt.Calendar.VisibleStart)
            and (TRessource(aInt.Pointer).Interval[i].StartDate<FGantt.Calendar.VisibleFinish))
            or ((TRessource(aInt.Pointer).Interval[i].FinishDate>FGantt.Calendar.VisibleStart)
            and (TRessource(aInt.Pointer).Interval[i].FinishDate<FGantt.Calendar.VisibleFinish))
            or ((TRessource(aInt.Pointer).Interval[i].StartDate<FGantt.Calendar.VisibleStart)
            and (TRessource(aInt.Pointer).Interval[i].FinishDate>FGantt.Calendar.VisibleFinish))
            then
              begin
                aNew := TInterval.Create(FGantt);
                aNew.StartDate:=TRessource(aInt.Pointer).Interval[i].StartDate;
                aNew.FinishDate:=TRessource(aInt.Pointer).Interval[i].FinishDate;
                aNew.Task:=TRessource(aInt.Pointer).Interval[i].Task+' - '+TRessource(aInt.Pointer).Interval[i].Project;
                aNew.Id := TRessource(aInt.Pointer).Interval[i].Id;
                aNew.Color:=TRessource(aInt.Pointer).Interval[i].Color;
                aNew.Fixed := TRessource(aInt.Pointer).Interval[i].Fixed;
                aNew.Pointer2:=TRessource(aInt.Pointer).Interval[i];
                aNew.OnChanged:=@TIntervalChanged;
                aNew.Project:=TRessource(aInt.Pointer).Interval[i].Project;
                aInt.AddInterval(aNew);
              end;
        if aInt.IntervalCount=0 then
          begin
            aNew := TInterval.Create(FGantt);
            aNew.Task:=strNoDataFound;
            aNew.Style:=isNone;
            aInt.AddInterval(aNew);
          end;
        FGantt.EndUpdate;
      end;
  end;
var
  i: Integer;
begin
  AddRes(TInterval(Sender));
end;

procedure TfTaskPlan.FGanttCalendarStartDateChanged(Sender: TObject);
var
  aDiff: Extended;
  procedure RefreshRes(aInt : TInterval);
  var
    i: Integer;
    aUser: String;
    tmpRes: TRessource;
  begin
    for i := 0 to aInt.IntervalCount-1 do
      RefreshRes(aInt.Interval[i]);
    if Assigned(aInt.Pointer) then
      begin
        aUser := TPInterval(aInt).User;
        if aDiff>0 then
          FThreads.Add(TCollectThread.Create(Self,FCollectedTo,FCollectedTo+aDiff,TRessource(aInt.Pointer),aUser,True,True,True,aInt))
        else
          FThreads.Add(TCollectThread.Create(Self,FCollectedFrom+aDiff,FCollectedFrom,TRessource(aInt.Pointer),aUser,True,True,True,aInt));
        iHourglass.Visible:=True;
        TCollectThread(FThreads[FThreads.Count-1]).OnTerminate:=@TCollectThreadTerminate;
        TCollectThread(FThreads[FThreads.Count-1]).Resume;
      end;
  end;
var
  i: Integer;
begin
  if FGantt.Calendar.VisibleFinish>FCollectedTo then
    begin
      aDiff := FGantt.Calendar.VisibleFinish-FCollectedTo;
      if aDiff<30 then aDiff := 30;
      for i := 0 to FGantt.IntervalCount-1 do
        RefreshRes(FGantt.Interval[i]);
      FCollectedTo:=FCollectedTo+aDiff;
    end;
  if FGantt.Calendar.VisibleStart<FCollectedFrom then
    begin
      aDiff := FGantt.Calendar.VisibleStart-FCollectedFrom;
      if aDiff>-30 then aDiff := -30;
      for i := 0 to FGantt.IntervalCount-1 do
        RefreshRes(FGantt.Interval[i]);
      FCollectedFrom:=FCollectedFrom+aDiff;
    end;
end;

constructor TfTaskPlan.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FThreads := TList.Create;
  FOwners := TStringList.Create;
  FUsers := TStringList.Create;
  FGantt := TgsGantt.Create(Self);
  FSelectedUser := nil;
  FGantt.Parent := pgantt;
  FGantt.Align:=alClient;
  FGantt.Tree.AfterUpdateCommonSettings:=@FGanttTreeAfterUpdateCommonSettings;
  FGantt.Tree.TaskEditable:=False;
  FGantt.Calendar.OnMoveOverInterval:=@FGanttCalendarMoveOverInterval;
  FGantt.Calendar.OnShowHint:=@FGanttCalendarShowHint;
  FGantt.Calendar.OnMouseMove:=@FGanttCalendarMouseMove;
  FGantt.Calendar.OnDblClick:=@FGanttCalendarDblClick;
  FGantt.Calendar.OnClick:=@FGanttCalendarClick;
  FGantt.Calendar.OnStartDateChanged:=@FGanttCalendarStartDateChanged;
  FGantt.Calendar.PopupMenu := pmAction;
  FGantt.Tree.PopupMenu := pmUSer;
  FGantt.Calendar.OnMouseUp:=@FGanttCalendarMouseUp;
  FCollectedTo:=Fgantt.Calendar.VisibleFinish;
  FCollectedFrom:=Now();
  bDayViewClick(nil);
  FGantt.Calendar.ShowHint:=True;
  FTaskView := TfTaskFrame.Create(Self);
  FTaskView.BaseName:='TPTASKS';
  FTaskView.Parent := pTasks;
  FTaskView.Align:=alClient;
  FTaskView.Panel9.Visible:=True;
end;

destructor TfTaskPlan.Destroy;
  procedure ClearResources(aInt : TInterval);
  var
    i: Integer;
    aInt1: TInterval;
  begin
    for i := 0 to aInt.IntervalCount-1 do
      ClearResources(aInt.Interval[i]);
    if Assigned(aInt.Pointer) then
      begin
        TInterval(aInt.Pointer).Free;
        aInt.Pointer := nil;
      end;
  end;
var
  i: Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    ClearResources(FGantt.Interval[i]);
  if Assigned(FDataSet) then
    begin
      FreeAndNil(FDataSet);
    end;
  while FThreads.Count>0 do
    Application.ProcessMessages;
  FThreads.Free;
  FOwners.Free;
  FUsers.Free;
  FTaskView.Free;
  inherited Destroy;
end;

procedure TfTaskPlan.Populate(aParent: Variant;aUser : Variant);
var
  aTasks: TTaskList;
var
  aNewInterval: TInterval;
  aTask: TTask;
  aInterval: TInterval;
  aDep: TInterval;
  i: Integer;
  aUserFilter : string ='';
  aRoot: TUser;
  aIRoot: TInterval;

  procedure CollectUsers(aIParent : TInterval;bParent : Variant);
  var
    aUsers: TUser;
    aINew: TPInterval;
    tmpRes: TRessource;
    aIsub: TInterval;
  begin
    aUsers := TUser.Create(nil);
    Data.SetFilter(aUsers,Data.QuoteField('PARENT')+'='+Data.QuoteValue(bParent));
    aUsers.First;
    while not aUsers.EOF do
      begin
        if aUsers.FieldByName('TYPE').AsString='G' then
          begin
            aINew := TPInterval.Create(FGantt);
            aINew.Task:=aUsers.FieldByName('NAME').AsString;
            aINew.StartDate:=Now()-(365*10);
            aINew.FinishDate:=Now()-(365*10);
            aINew.Visible:=True;
            aINew.Style:=isNone;
            aIParent.AddInterval(aINew);
            CollectUsers(aINew,aUsers.id.AsVariant);
            aINew.OnDrawBackground:=@aIGroupDrawBackground;
          end
        else if not ((aUsers.FieldByName('LEAVED').AsString<>'') and (aUsers.FieldByName('LEAVED').AsDateTime<Now())) and ((aUser = Null) or (aUser = aUsers.id.AsVariant)) then
          begin
            aINew := TPInterval.Create(FGantt);
            aINew.Task:=aUsers.FieldByName('NAME').AsString;
            aINew.StartDate:=Now()-(365*10);
            aINew.FinishDate:=Now()-(365*10);
            aINew.SetUser(aUsers.FieldByName('ACCOUNTNO').AsString,nil);
            aINew.Visible:=True;
            aINew.Style:=isNone;
            aIParent.AddInterval(aINew);
            tmpRes := TRessource.Create(nil);
            tmpRes.User:=aINew;
            FThreads.Add(TCollectThread.Create(Self,Now(),Fgantt.Calendar.VisibleFinish,tmpRes,aUsers.FieldByName('ACCOUNTNO').AsString,True,True,False,aINew));
            iHourglass.Visible:=True;
            FCollectedTo:=Fgantt.Calendar.VisibleFinish;
            FCollectedFrom:=Now();
            TCollectThread(FThreads[FThreads.Count-1]).OnTerminate:=@TCollectThreadTerminate;
            aINew.OnDrawBackground:=@aINewDrawBackground;
            aINew.OnExpand:=@AddUserIntervals;
            aIsub := TInterval.Create(FGantt);
            aINew.AddInterval(aISub);
          end;
        aUsers.Next;
      end;
    aUsers.Free;
  end;
begin
  if Data.Users.FieldByName('PARENT').AsVariant = Null then exit;
  if Data.Users.FieldByName('POSITION').AsString='LEADER' then
    begin
      if not Assigned(FDataSet) then
        begin
          FDataSet := TTaskList.Create(nil);
          TTaskList(FDataSet).SelectByDept(Data.Users.FieldByName('PARENT').AsVariant);
          TTaskList(FDataSet).ActualFilter := '('+TTaskList(FDataSet).ActualFilter+') AND ('+Data.ProcessTerm(Data.QuoteField('UNPLANNED')+'='+Data.QuoteValue(''))+')';
          FDataSet.Open;
          FTaskView.GridView.DefaultRows:='GLOBALWIDTH:%;COMPLETED:30;SUMMARY:200;STARTDATE:60;DUEDATE:60;USER:100;OWNER:100;PERCENT:40;UNPLANNED:30;';
          FTaskView.bDelegated1.Down:=True;
          FTaskView.bDependencies1.Down:=True;
          FTaskView.bFuture1.Down:=True;
          FTaskView.UserID := Data.Users.Id.AsVariant;
          FTaskView.IgnoreUser := True;
          FTaskView.DataSet := FDataSet;
        end;
    end
  else
    begin
      pTasks.Visible:=False;
      spTasks.Visible:=False;
    end;
  while FGantt.IntervalCount>0 do
    FGantt.DeleteInterval(0);
  aIRoot := TInterval.Create(FGantt);
  aIRoot.Style:=isNone;
  aRoot := TUser.Create(nil);
  aRoot.Open;
  FGantt.BeginUpdate;
  if aRoot.DataSet.Locate('SQL_ID',aParent,[]) then
    begin
      aIRoot.Task:=aRoot.FieldByName('NAME').AsString;
      aIRoot.Visible:=True;
      aIRoot.StartDate:=Now()-1;
      aIRoot.FinishDate:=Now()-1;
      aIRoot.OnDrawBackground:=@aIGroupDrawBackground;
      FGantt.AddInterval(aIRoot);
      CollectUsers(aIRoot,aParent);
    end;
  aRoot.Free;
  FGantt.EndUpdate;
  FGantt.Tree.TopRow:=1;
  FGantt.StartDate:=Now();
  acUse.Enabled:=False;
  acCancel.Enabled:=False;
  Application.ProcessMessages;
  for i := 0 to FThreads.Count-1 do
    TCollectThread(FThreads[i]).Resume
end;

procedure TfTaskPlan.CollectResources(aResource: TRessource; aFrom,
  aTo: TDateTime; asUser: string; aConnection: TComponent; CollectTasks,
  CollectCalendar: Boolean; Processmessages: Boolean);
var
  aUser: TUser;
  bTasks: TTaskList;
  bInterval: TPInterval;
  aDue: System.TDateTime;
  aStart: System.TDateTime;
  aCalendar: TCalendar;
  gView : TfGanttView;
begin
  aUser := TUser.CreateEx(nil,Data,aConnection);
  aUser.SelectByAccountno(asUser);
  aUser.Open;
  aResource.Resource := aUser.Text.AsString;
  aResource.Accountno := asUSer;
  if aUser.FieldByName('TYPE').AsString<>'G' then
    begin
      if CollectTasks then
        begin
          if Processmessages then
            Application.ProcessMessages;
          bTasks := TTaskList.CreateEx(nil,Data,aConnection);
          bTasks.SelectUncompletedByUser(asUser);
          bTasks.Open;
          with bTasks.DataSet do
            begin
              while not EOF do
                begin
                  bInterval := nil;
                  if  (not bTasks.FieldByName('DUEDATE').IsNull)
                  //and (not (bTasks.FieldByName('PLANTIME').IsNull) or (bTasks.FieldByName('STARTDATE').IsNull))
                  and (not (bTasks.FieldByName('PLANTASK').AsString='N'))
                  then
                    begin
                      bInterval := TPInterval.Create(nil);
                      gView.FillInterval(bInterval,bTasks,True,aUser);
                      if bInterval.FinishDate>aTo then
                        FreeAndNil(bInterval);
                    end;
                  if Assigned(bInterval) then
                    begin
                      bInterval.SetUser(asUser,aConnection);
                      bInterval.Changed:=False;
                      aResource.AddInterval(bInterval);
                    end;
                  Next;
                end;
            end;
          bTasks.Free;
        end;
      if CollectCalendar then
        begin
          if Processmessages then
            Application.ProcessMessages;
          aCalendar := TCalendar.CreateEx(nil,Data,aConnection);
          aCalendar.SelectPlanedByUserAndTime(asUser,aFrom,aTo);
          aCalendar.Open;
          with aCalendar.DataSet do
            begin
              First;
              while not EOF do
                begin
                  bInterval := TBackInterval.Create(nil);
                  bInterval.StartDate:=aCalendar.FieldByName('STARTDATE').AsDateTime;
                  bInterval.FinishDate:=aCalendar.FieldByName('ENDDATE').AsDateTime;
                  if aCalendar.FieldByName('ALLDAY').AsString = 'Y' then
                    begin
                      bInterval.StartDate := trunc(bInterval.StartDate);
                      bInterval.FinishDate := trunc(bInterval.FinishDate+1);
                      bInterval.Fixed:=True;
                      bInterval.Project:='CAL';
                    end;
                  bInterval.Task:=aCalendar.FieldByName('SUMMARY').AsString;
                  bInterval.Id:=aCalendar.Id.AsVariant;
                  aResource.AddInterval(bInterval);
                  bInterval.Changed:=False;
                  Next;
                end;
            end;
          aCalendar.Free;
        end;
      aResource.Sort;
    end;
  aUser.Free;
end;

function TfTaskPlan.GetIntervalFromCoordinates(Gantt: TgsGantt; X, Y,Index : Integer): TInterval;
var
  List: TList;
  aId : Variant;
  ay: Integer;
  i: Integer;
  aIdx : Integer = 0;
  function IsInRect(aX, aY: Integer; R: TRect): Boolean;
  begin
    Result := (aX >= R.Left) and (aX <= R.Right);
  end;
begin
  Result := nil;
  aId := Null;
  List := TList.Create;
  Gantt.MakeIntervalList(List);
  ay := Y-Gantt.Calendar.StartDrawIntervals;
  ay := ay div max(Gantt.Calendar.PixelsPerLine,1);
  ay := ay+(Gantt.Tree.TopRow-1);
  if (ay<List.Count) and (ay>-1) then
    begin
      if IsInRect(X,Y,TInterval(List[ay]).DrawRect) then
        aId := TInterval(List[ay]).Id;
      if not TInterval(List[ay]).IsDrawRectClear then
        if IsInRect(X,Y,TInterval(List[ay]).DrawRect) and (Index=0) then
          begin
            Result := TInterval(List[ay]);
          end;
      if Assigned(TInterval(List[ay]).Pointer) then
        begin
          for i := 0 to TRessource(TInterval(List[ay]).Pointer).IntervalCount-1 do
            if not TRessource(TInterval(List[ay]).Pointer).Interval[i].IsDrawRectClear then
              if IsInRect(X,Y,TRessource(TInterval(List[ay]).Pointer).Interval[i].DrawRect)
              and (not (TRessource(TInterval(List[ay]).Pointer).Interval[i] is TBackInterval))
              then
                begin
                  if aIdx = Index then
                    begin
                      Result := TRessource(TInterval(List[ay]).Pointer).Interval[i];
                      break;
                    end;
                  inc(aIdx);
                end;
        end;
    end;
  List.Free;
end;

function TfTaskPlan.GetTaskIntervalFromCoordinates(Gantt: TgsGantt; X, Y,
  Index: Integer;IgnoreX : Boolean = False): TInterval;
var
  List: TList;
  aId : Variant;
  ay: Integer;
  i: Integer;
  aIdx : Integer = 0;
  function IsInRect(aX, aY: Integer; R: TRect): Boolean;
  begin
    Result := (aX >= R.Left) and (aX <= R.Right);
  end;
begin
  Result := nil;
  aId := Null;
  List := TList.Create;
  Gantt.MakeIntervalList(List);
  ay := Y-Gantt.Calendar.StartDrawIntervals;
  ay := ay div max(Gantt.Calendar.PixelsPerLine,1);
  ay := ay+(Gantt.Tree.TopRow-1);
  if (ay<List.Count) and (ay>-1) then
    begin
      if IgnoreX or IsInRect(X,Y,TInterval(List[ay]).DrawRect) then
        Result := TInterval(List[ay]);
    end;
  List.Free;
end;

end.

