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
Created 01.05.2013
*******************************************************************************}
unit uGanttView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, Spin, ExtDlgs, gsGanttCalendar, uTask,
  Math, types, uProjects,uQuickHelpFrame,uBaseDbClasses;

type

  { TfGanttView }

  TfGanttView = class(TForm)
    acCenterTask: TAction;
    acOpen: TAction;
    acMakePossible: TAction;
    acAddSubProjects: TAction;
    acAddSnapshot: TAction;
    acExportToImage: TAction;
    acFindTimeSlot: TAction;
    acAddtask: TAction;
    acDeletetask: TAction;
    acAddHistory: TAction;
    acCalculatefromHere: TAction;
    acMoveTogetherFromHere: TAction;
    acCalculatePlanWithUsage: TAction;
    acCalculatePlanWithUsageWithoutDependencies: TAction;
    acCalculateForward: TAction;
    acCalculateBackwards: TAction;
    acRefreshWizard: TAction;
    ActionList1: TActionList;
    bCalculatePlan1: TSpeedButton;
    bCalculatePlanWithUsage: TSpeedButton;
    Bevel11: TBevel;
    Bevel12: TBevel;
    bCalculate2: TSpeedButton;
    Bevel10: TBevel;
    bDayView: TSpeedButton;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bMonthView: TSpeedButton;
    bMoveTogether: TSpeedButton;
    bSave1: TSpeedButton;
    bShowTasks: TSpeedButton;
    bShowTasks1: TSpeedButton;
    bHistory: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    cbSnapshot: TComboBox;
    iHourglass: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    lDate: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pCalc: TPanel;
    Panel9: TPanel;
    pCalc2: TPanel;
    pgantt: TPanel;
    Panel7: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SavePictureDialog1: TSavePictureDialog;
    seBuffer: TSpinEdit;
    tbTop: TPanel;
    RecalcTimer: TTimer;
    bSave: TSpeedButton;
    bCancel: TSpeedButton;
    bCSave: TSpeedButton;
    procedure acAddHistoryExecute(Sender: TObject);
    procedure acAddSnapshotExecute(Sender: TObject);
    procedure acAddSubProjectsExecute(Sender: TObject);
    procedure acAddtaskExecute(Sender: TObject);
    procedure acCalculatefromHereExecute(Sender: TObject);
    procedure acCenterTaskExecute(Sender: TObject);
    procedure acDeletetaskExecute(Sender: TObject);
    procedure acExportToImageExecute(Sender: TObject);
    procedure acFindTimeSlotExecute(Sender: TObject);
    procedure acMakePossibleExecute(Sender: TObject);
    procedure acMoveTogetherFromHereExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acRefreshWizardExecute(Sender: TObject);
    procedure aIntervalChanged(Sender: TObject);
    procedure aIntervalDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;
  aUnfinishedList: TList=nil);
    procedure bCalculatePlanClick(Sender: TObject);
    procedure bCalculatePlanWithUsageClick(Sender: TObject);
    procedure bCalculatePlanWithUsagewithoutDependChangeBounds(Sender: TObject);
    procedure bCalculatePlanWithUsagewithoutDependClick(Sender: TObject);
    procedure bMoveBack1Click(Sender: TObject);
    procedure bMoveBackClick(Sender: TObject);
    procedure bMoveFwdClick(Sender: TObject);
    procedure bDayViewClick(Sender: TObject);
    procedure bMonthViewClick(Sender: TObject);
    procedure bRefresh1Click(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bShowTasksClick(Sender: TObject);
    procedure bTodayClick(Sender: TObject);
    procedure bWeekViewClick(Sender: TObject);
    procedure cbSnapshotSelect(Sender: TObject);
    procedure FGanttCalendarClick(Sender: TObject);
    procedure FGanttCalendarDblClick(Sender: TObject);
    procedure FGanttCalendarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGanttCalendarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FGanttCalendarMoveOverInterval(Sender: TObject;
      aInterval: TInterval; X, Y: Integer);
    procedure FGanttCalendarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
    procedure FGanttTreeResize(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure RecalcTimerTimer(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bCSaveClick(Sender: TObject);
    procedure TCollectThreadTerminate(Sender: TObject);
  private
    { private declarations }
    FSelectedCol: TDateTime;
    FCollectUsageList : TList;
    FSelectedRow: Int64;
    FResourcesRead : Boolean;
    FGantt: TgsGantt;
    Fproject : TProject;
    FTasks : TTaskList;
    FThreads : TList;
    FRessources : TList;
    FHintRect : TRect;
    FRow : Integer;
    aClickPoint: TPoint;
    aSelInterval : Integer;
    FSnapshots : TInterval;
    FCriticalPathLength : float;
    FQuickHelpFrame: TfQuickHelpFrame;
    FSelectedInterval: TInterval;
    FIntervals : TList;
    FManualStarted : Boolean;
    FReasonText : string;
    function FindInterval(aParent: TInterval; aId: Variant): TInterval;
    function IntervalById(Id: Variant; Root: TInterval=nil): TInterval;
    procedure UpdateDependencies(aInt : TInterval);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddHelp;
    procedure Populate(aTasks: TTaskList; DoClean: Boolean=True;AddInactive : Boolean = False);
    procedure DoSave(aChangeMilestones : Boolean;aSetTermins : Boolean = True);
    procedure CleanIntervals;
    function FindCriticalPath: TInterval;
    procedure FillInterval(aInterval: TInterval; aTasks: TTaskList;
      MarkDependencies: Boolean=False; aUser: TUser=nil);
    procedure GotoTask(aLink : string);
    function Execute(aProject : TProject;aLink : string = ''; DoClean: Boolean=True;AddInactive : Boolean = False) : Boolean;
    function Calculate(aProject : TProject;DoClean: Boolean=True;AddInactive : Boolean = False) : Boolean;
    function MoveFwd(aProject : TProject;DoClean: Boolean=True;AddInactive : Boolean = False) : Boolean;
    function MoveAndCalculate(aProject : TProject;DoClean: Boolean=True;AddInactive : Boolean = False) : Boolean;
    procedure SetRights;
  end;

var
  fGanttView: TfGanttView;

implementation
uses uData,LCLIntf,uTaskEdit,variants,LCLProc,uTaskPlan,
  uIntfStrConsts,uColors,uBaseDBInterface,Grids,uLogWait,uWiki,
  uBaseApplication,uhistoryadditem,uRefreshWizard,uchangegantt;
{$R *.lfm}
resourcestring
  strSnapshot                             = 'Snapshot';
  strNoSnapshot                           = '<keiner>';
  strCommitChanges                        = 'Sollen wirklich alle Änderungen in die Aufgaben eingetragen werden ?';
  strCancelChanges                        = 'Sollen wirklich alle Änderungen verworfen werden ?';
  strCollectingTasks                      = 'Ansicht wird aufgebaut...';
  strCollectingDependencies               = 'Abhängigkeiten werden ermittelt...';
  strCollectingresourceTimes              = '... Urlaubszeiten ermitteln';
  strChangeMilestones                     = 'Meilensteine ändern';
  strNewTask                              = 'neue Aufgabe';
procedure TfGanttView.AddHelp;
var
  aWiki: TWikiList;
begin
  if Assigned(FQuickHelpFrame) then exit;
  aWiki := TWikiList.Create(nil);
  with BaseApplication as IBaseApplication do
  if aWiki.FindWikiPage('Promet-ERP-Help/workflows/tfganttview') then
    begin
      FQuickHelpFrame := TfQuickHelpFrame.Create(nil);
      if not FQuickHelpFrame.OpenWikiPage(aWiki) then
        FreeAndNil(FQuickHelpFrame)
      else
        begin
          FQuickHelpFrame.Parent:=Self;
          FQuickHelpFrame.Align:=alTop;
          FQuickHelpFrame.BorderSpacing.Around:=8;
        end;
    end;
  aWiki.Free;
end;
procedure TfGanttView.FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=160;
  fgantt.Tree.ColWidths[4]:=80;
  fgantt.Tree.ColWidths[5]:=80;
  fgantt.Tree.ColWidths[6]:=0;
  fgantt.Tree.ColWidths[7]:=0;
  FGantt.Tree.Width:=390;
  FGantt.Tree.OnResize:=@FGanttTreeResize;
  FGantt.Tree.ShowHint:=True;
  FGantt.Tree.ScrollBars:=ssVertical;
  FGantt.Tree.Options:=FGantt.Tree.Options+[goCellHints];
  FGantt.Tree.Options:=FGantt.Tree.Options-[goHorzLine];
  FGantt.Tree.AlternateColor:=$00FFE6E6;
end;

procedure TfGanttView.FGanttTreeResize(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=FGantt.Tree.Width-FGantt.Tree.ColWidths[3]-FGantt.Tree.ColWidths[4]-FGantt.Tree.ColWidths[5]-20; //20=scrollbarwidth maybe other values on other widgetsets
  fgantt.Tree.ColWidths[6]:=0;
  fgantt.Tree.ColWidths[7]:=0;
end;

procedure TfGanttView.PopupMenu2Popup(Sender: TObject);
var
  TP : TfTaskPlan;
  aPoint: types.TPoint;
begin
  aPoint := Mouse.CursorPos;
  aPoint := FGantt.Calendar.ScreenToControl(aPoint);
  FSelectedInterval := TP.GetTaskIntervalFromCoordinates(FGantt,aPoint.x,aPoint.y,0);
  acFindTimeSlot.Enabled:=Assigned(FSelectedInterval);
  acDeletetask.Enabled:=Assigned(FSelectedInterval);
end;

procedure TfGanttView.RecalcTimerTimer(Sender: TObject);
begin
  if FGantt.Calendar.IsDragging then exit;
  RecalcTimer.Enabled:=False;
  FindCriticalPath;
  FGantt.Calendar.Invalidate;
end;

procedure TfGanttView.bSaveClick(Sender: TObject);
var
  aDialog: TForm;
  aCheckBox: TCheckBox;
begin
  if fChangeGantt.Execute(FReasonText) then
    begin
      DoSave(fChangeGantt.cbChangeMilestones.Checked,fChangeGantt.cbSetAppt.Checked);
      if fChangeGantt.cbAddToProject.Checked then
        begin
          FProject.History.AddItem(Data.Users.DataSet,fChangeGantt.mRule.Text,'','',FProject.DataSet,ACICON_USEREDITED,'',True,True);
        end;
      if fChangeGantt.cbMakeSnapshot.Checked then
        Fproject.Makesnapshot(strSnapshot+' '+DateToStr(Now()));
    end;
end;

procedure TfGanttView.bCancelClick(Sender: TObject);
begin
  if (MessageDlg(strCancelChanges,mtInformation,[mbYes,mbNo],0) = mrYes) then
    bRefreshClick(nil);
end;

procedure TfGanttView.bCSaveClick(Sender: TObject);
begin
  bSave.Click;
  Close;
end;

procedure TfGanttView.TCollectThreadTerminate(Sender: TObject);
begin
  if FThreads.IndexOf(Sender) > -1 then
    FThreads.Remove(Sender);
  if FThreads.Count=0 then
    iHourglass.Visible:=False;
  FGantt.Invalidate;
end;

function TfGanttView.FindInterval(aParent : TInterval;aId : Variant) : TInterval;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to aParent.IntervalCount-1 do
    begin
      if aParent.Interval[i].Id = aId then
        begin
          Result := aParent.Interval[i];
          break;
        end
      else if aParent.Interval[i].IntervalCount>0 then
        begin
          Result := FindInterval(aParent.Interval[i],aId);
          if Assigned(Result) then break;
        end;
    end;
end;
function TfGanttView.IntervalById(Id : Variant;Root : TInterval = nil) : TInterval;
var
  i: Integer;
  ares: TInterval;
begin
  Result := nil;
  for i := 0 to FGantt.IntervalCount-1 do
    begin
      if FGantt.Interval[i].Id = id then
        begin
          Result := FGantt.Interval[i];
          break;
        end
      else if FGantt.Interval[i].IntervalCount>0 then
        begin
          Result := FindInterval(FGantt.Interval[i],Id);
          if Assigned(Result) then break;
        end;
    end;
  if Assigned(Result) and Assigned(Root) then
    begin
      ares := result;
      while Assigned(aRes.Parent) do
        ares := ares.Parent;
      if aRes<>Root then
        Result := nil;
    end;
end;

procedure TfGanttView.UpdateDependencies(aInt: TInterval);
var
  i: Integer;
  aTask: TTask;
  aDep: TInterval;
begin
  if not Assigned(aInt) then exit;
 while aInt.DependencyCount>0 do
    begin
      aDep := aInt.Dependencies[0];
      while (Assigned(aDep) and (aDep.ConnectionCount>0)) do
        aDep.DeleteConnection(0);
    end;
  aTask := TTask.Create(nil);
  aTask.Select(aInt.Id);
  aTask.Open;
  if aTask.Count>0 then
    begin
      aTask.Dependencies.Open;
      while not aTask.Dependencies.EOF do
        begin
          aDep := IntervalById(aTask.Dependencies.FieldByName('REF_ID_ID').AsVariant);
          if Assigned(aDep) then
            begin
              aDep.AddConnection(aInt,aTask.FieldByName('STARTDATE').IsNull and aTask.FieldByName('DUEDATE').IsNull,False);
            end;
          aTask.Dependencies.Next;
        end;
    end;
  aTask.Free;
end;

procedure TfGanttView.bDayViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsWeekNum;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfGanttView.aIntervalChanged(Sender: TObject);
var
  i: Integer;
  oD: TDateTime;
  a: Integer;
  aDur: TDateTime;
  c: Integer;
  oD2: TDateTime;
  Found: Boolean;
begin
  bSave.Enabled:=True;
  bCSave.Enabled:=True;
  bCancel.Enabled:=true;
  with TInterval(Sender) do
    begin
      if TInterval(Sender).Fixed then exit;
      TInterval(Sender).BeginUpdate;
      debugln('IntervalChanged('+TInterval(Sender).Task+')');
      if TInterval(Sender).StartDate<TInterval(Sender).Earliest then
        TInterval(Sender).StartDate:=TInterval(Sender).Earliest;
      //Move Forward
      aDur := NetDuration;
      if ResourceTimePerDay=0 then ResourceTimePerDay:=1;
      if NetDuration<(NetTime*(1/ResourceTimePerDay)) then
        aDur:=(NetTime*(1/ResourceTimePerDay));
      if aDur<0.5 then aDur:=0.5;
      if NetDuration<aDur then NetDuration:=aDur;
      if TInterval(Sender).StartDate<TInterval(Sender).Earliest then
        TInterval(Sender).StartDate:=TInterval(Sender).Earliest;
      if TInterval(Sender).Moved then
        begin
          //Add Weekends
          i := trunc(TInterval(Sender).StartDate);
          while i < TInterval(Sender).StartDate+aDur do
            begin
              if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
                aDur := aDur+1;
              inc(i,1);
            end;
          //if TInterval(Sender).FinishDate<(TInterval(Sender).StartDate+aDur) then
          TInterval(Sender).FinishDate := (TInterval(Sender).StartDate+aDur);
        end;
      IntervalDone:=TInterval(Sender).StartDate;
      for i := 0 to ConnectionCount-1 do
        begin
          Connection[i].BeginUpdate;
          oD := Connection[i].Duration;
          if Connection[i].StartDate<FinishDate+WaitTime then
            begin
              for c := 0 to Connection[i].IntervalCount-1 do
                if Connection[i].Interval[c].StartDate<FinishDate+WaitTime then
                  begin
                    oD2 := Connection[i].Interval[c].Duration;
                    Connection[i].Interval[c].BeginUpdate;
                    Connection[i].Interval[c].StartDate:=FinishDate+WaitTime;
                    Connection[i].Interval[c].FinishDate:=FinishDate+WaitTime+oD2;
                    Connection[i].Interval[c].EndUpdate;
                  end;
              Connection[i].StartDate:=FinishDate+WaitTime;
            end;
          if Connection[i].FinishDate<Connection[i].StartDate+oD then
            Connection[i].FinishDate:=Connection[i].StartDate+oD;
          Connection[i].IntervalDone:=Connection[i].StartDate;
          Connection[i].Moved:=True;
          Connection[i].EndUpdate;
        end;
    end;
  TInterval(Sender).ResetMovement;
  TInterval(Sender).Endupdate(True);
  RecalcTimer.Enabled := True;
  Found := False;
  for i := 0 to FRessources.Count-1 do
    for a := 0 to TRessource(FRessources[i]).IntervalCount-1 do
      if TRessource(FRessources[i]).Interval[a].Id=TInterval(Sender).Id then
        begin
          TRessource(FRessources[i]).Interval[a].StartDate:=TInterval(Sender).StartDate;
          TRessource(FRessources[i]).Interval[a].FinishDate:=TInterval(Sender).FinishDate;
          Found := True;
        end;
  if Found then
    FGantt.Invalidate;
end;
procedure TfGanttView.aIntervalDrawBackground(Sender: TObject; aCanvas: TCanvas;
  aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aUnfinishedList : TList = nil);
var
  TaskPlan : TfTaskPlan;
  i: Integer;
  aInterval: TInterval;
  aIStart: TDateTime;
  aIEnd: TDateTime;
  aAddTop : Integer = 0;
  aDrawRect: TRect;
begin
  Taskplan.aIDrawBackgroundWeekends(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,$e0e0e0,FSelectedCol);
  if bShowTasks.Down then
    Taskplan.aIDrawBackground(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,Ligthen(clBlue,1),Ligthen(clLime,0.9),Ligthen(clRed,0.9),FSelectedCol,FCollectUsageList);
  if Assigned(FSnapshots) then
    begin
      for i := 0 to FSnapshots.IntervalCount-1 do
        if FSnapshots.Interval[i].Id = TInterval(Sender).Id then
          begin
            //if TInterval(Sender).IntervalCount=0 then
              begin
                aInterval := FSnapshots.Interval[i];
                if ((aInterval.StartDate>aStart) and (aInterval.StartDate<(aEnd)))
                or ((aInterval.FinishDate>aStart) and (aInterval.FinishDate<(aEnd)))
                or ((aInterval.StartDate<=aStart) and (aInterval.FinishDate>=(aEnd)))
                then
                  begin
                    aIStart := aInterval.StartDate;
                    aIEnd := aInterval.FinishDate;
                    aCanvas.Brush.Color:=clYellow;
                    aDrawRect:=Rect(round((aIStart-aStart)*aDayWidth),(aRect.Top+((aRect.Bottom-aRect.Top) div 4)-1)+aAddTop,round((aIEnd-aStart)*aDayWidth)-1,(aRect.Bottom-((aRect.Bottom-aRect.Top) div 4)-1)+aAddTop);
                    aCanvas.Rectangle(aDrawRect);
                  end;
              end;
            break;
          end;
    end;
end;

procedure MoveTogether(Sender : TInterval;aDate : TDateTime;MoveTreeOnly : Boolean = False);
var
  aDur: TDateTime;
  i: Integer;
  oD: TDateTime;
  c: Integer;
  oD2: TDateTime;
  aBuffer: TDateTime;
begin
  with TInterval(Sender) do
    begin
      BeginUpdate;
      if Assigned(Parent) then
        StartDate := aDate;
      EndUpdate;
      if MoveTreeOnly then
        for i := 0 to ConnectionCount-1 do
          MoveTogether(Connection[i],aDate,MoveTreeOnly);
      if not MoveTreeOnly then
        for i := 0 to IntervalCount-1 do
          MoveTogether(Interval[i],aDate);
    end;
end;
procedure MoveForward(Sender : TInterval;MoveTreeOnly : Boolean = False);
var
  aDur: TDateTime;
  i: Integer;
  oD: TDateTime;
  c: Integer;
  oD2: TDateTime;
  aBuffer: TDateTime;
begin
  with TInterval(Sender) do
    begin
      BeginUpdate;
      //Move Forward
      aDur := NetDuration;
      if ResourceTimePerDay=0 then ResourceTimePerDay:=1;
      if NetDuration<(NetTime*(1/ResourceTimePerDay)) then
        aDur:=(NetTime*(1/ResourceTimePerDay));
      if aDur<0.5 then aDur:=0.5;
      if NetDuration<aDur then NetDuration:=aDur;
      if StartDate<Earliest then
        StartDate:=Earliest;
      //Move out of Weekends
      if DayOfWeek(trunc(StartDate))=7 then
        StartDate := trunc(StartDate)+1;
      if DayOfWeek(trunc(StartDate))=1 then
        StartDate := trunc(StartDate)+2;
      //Add Weekends
      i := trunc(StartDate);
      while i < StartDate+aDur do
        begin
          if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
            aDur := aDur+1;
          inc(i,1);
        end;
      //TODO: Urlaub
      FinishDate := (StartDate+aDur);
      aBuffer := WaitTime;
      //Add Weekends to Buffer
      i := trunc(FinishDate);
      while i < FinishDate+aBuffer do
        begin
          if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
            aBuffer := aBuffer+1;
          inc(i,1);
        end;

      for i := 0 to ConnectionCount-1 do
        begin
          Connection[i].BeginUpdate;
          oD := Connection[i].Duration;
          if Connection[i].StartDate<FinishDate+aBuffer then
            begin
              for c := 0 to Connection[i].IntervalCount-1 do
                if Connection[i].Interval[c].StartDate<FinishDate+aBuffer then
                  begin
                    oD2 := Connection[i].Interval[c].Duration;
                    Connection[i].Interval[c].BeginUpdate;
                    Connection[i].Interval[c].StartDate:=FinishDate+aBuffer;
                    Connection[i].Interval[c].FinishDate:=FinishDate+aBuffer+oD2;
                    Connection[i].Interval[c].EndUpdate;
                  end;
              Connection[i].StartDate:=FinishDate+aBuffer;
            end;
          if Connection[i].FinishDate<Connection[i].StartDate+oD then
            Connection[i].FinishDate:=Connection[i].StartDate+oD;
          Connection[i].IntervalDone:=Connection[i].StartDate;
          Connection[i].EndUpdate;
          MoveForward(Connection[i],MoveTreeOnly);
        end;
      EndUpdate;
      if not MoveTreeOnly then
        for i := 0 to IntervalCount-1 do
          MoveForward(Interval[i],MoveTreeOnly);
    end;
end;
procedure TfGanttView.bCalculatePlanClick(Sender: TObject);
  procedure MoveForwardCalc(Sender : TInterval);
  var
    aDur: TDateTime;
    i: Integer;
    oD: TDateTime;
    c: Integer;
    oD2: TDateTime;
    aBuffer: TDateTime;
    aNewStartDate: TDateTime;
  begin
    with TInterval(Sender) do
      begin
        BeginUpdate;
        aNewStartDate := 0;
        for i := 0 to DependencyCount-1 do
          if Dependencies[i].FinishDate>aNewStartDate then
            aNewStartDate:=Dependencies[i].FinishDate;
        if aNewStartDate>0 then StartDate := aNewStartDate;
        //Move Forward
        aDur := NetDuration;
        if ResourceTimePerDay=0 then ResourceTimePerDay:=1;
        aDur:=(NetTime*(1/ResourceTimePerDay));
        if aDur<0.5 then aDur:=0.5;
        NetDuration:=aDur;
        if Earliest>0 then
          if Earliest>StartDate then
            StartDate:=Earliest;
        //Move out of Weekends
        if DayOfWeek(trunc(StartDate))=7 then
          StartDate := trunc(StartDate)+1;
        if DayOfWeek(trunc(StartDate))=1 then
          StartDate := trunc(StartDate)+2;
        //Add Weekends
        i := trunc(StartDate);
        while i < StartDate+aDur do
          begin
            if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
              aDur := aDur+1;
            inc(i,1);
          end;
        //TODO: Urlaub
        FinishDate := (StartDate+aDur);
        //Buffer
        aBuffer := WaitTime;
        if aBuffer < (aDur*(fGanttView.seBuffer.Value/100)) then
          aBuffer := (aDur*(fGanttView.seBuffer.Value/100));
        //Add Weekends to Buffer
        i := trunc(FinishDate);
        while i < FinishDate+aBuffer do
          begin
            if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
              aBuffer := aBuffer+1;
            inc(i,1);
          end;
        for i := 0 to ConnectionCount-1 do
        EndUpdate;
        for i := 0 to IntervalCount-1 do
          MoveForwardCalc(Interval[i]);
      end;
  end;
var
  i : Integer;
begin
  FReasonText:='';
  Screen.Cursor := crHourGlass;
  for i := 0 to FGantt.IntervalCount-1 do
    MoveForwardCalc(FGantt.Interval[i]);
  FGantt.Invalidate;
  bMoveFwdClick(nil);
  Screen.Cursor := crDefault;
end;

procedure TfGanttView.bCalculatePlanWithUsageClick(Sender: TObject);
var
  UserTimes : TStringList;
  procedure MoveForwardCalc(Sender : TInterval);
  var
    aTask: TTask;
    aEarliest,aStart,aEnd,aDuration: TDateTime;
    i: Integer;
    aUserEnd: TDateTime;
    aUser: String;
    aUserTime: String;
  begin
    with TInterval(Sender) do
      begin
        BeginUpdate;
        aTask := TTask.Create(nil);
        aTask.Select(Id);
        aTask.Open;
        if aTask.Count>0 then
          begin
            aEarliest := Now();
            for i := 0 to DependencyCount-1 do
              if Dependencies[i].FinishDate+Dependencies[i].WaitTime>aEarliest then
                aEarliest:=Dependencies[i].FinishDate+Dependencies[i].WaitTime;
           aUser := aTask.FieldByName('USER').AsString;
           aUserTime := UserTimes.Values[aUser];
           if (trim(aUsertime)<>'') and (not TryStrToDateTime(aUserTime,aUserEnd)) then
             aUserEnd := Now();
            if aUserEnd>aEarliest then
              aEarliest:=aUserEnd;
            try
              if aTask.Terminate(aEarliest,aStart,aEnd,aDuration) then
                begin
                  StartDate:=aStart;
                  FinishDate:=aEnd;
                  UserTimes.Values[aUser]:=DateTimeToStr(aEnd);
                end;
            except
            end;
          end;
        aTask.Free;
        EndUpdate;
        for i := 0 to IntervalCount-1 do
          MoveForwardCalc(Interval[i]);
      end;
  end;
var
  i : Integer;
begin
  UserTimes := TStringList.Create;
  UserTimes.NameValueSeparator:=':';
  Screen.Cursor := crHourGlass;
  for i := 0 to FGantt.IntervalCount-1 do
    MoveForwardCalc(FGantt.Interval[i]);
  FGantt.Invalidate;
  Screen.Cursor := crDefault;
  UserTimes.Free;
end;

procedure TfGanttView.bCalculatePlanWithUsagewithoutDependChangeBounds(
  Sender: TObject);
begin

end;

procedure TfGanttView.bCalculatePlanWithUsagewithoutDependClick(Sender: TObject
  );
var
  UserTimes : TStringList;
  procedure MoveForwardCalc(Sender : TInterval);
  var
    aTask: TTask;
    aEarliest,aStart,aEnd,aDuration: TDateTime;
    i: Integer;
    aUserEnd: TDateTime;
    aUser: String;
    aUserTime: String;
  begin
    with TInterval(Sender) do
      begin
        BeginUpdate;
        aTask := TTask.Create(nil);
        aTask.Select(Id);
        aTask.Open;
        if aTask.Count>0 then
          begin
            aEarliest := Now();
            for i := 0 to DependencyCount-1 do
              if Dependencies[i].FinishDate+Dependencies[i].WaitTime>aEarliest then
                aEarliest:=Dependencies[i].FinishDate+Dependencies[i].WaitTime;
           aUser := aTask.FieldByName('USER').AsString;
           aUserTime := UserTimes.Values[aUser];
           if (trim(aUsertime)<>'') and (not TryStrToDateTime(aUserTime,aUserEnd)) then
             aUserEnd := Now();
            if aUserEnd>aEarliest then
              aEarliest:=aUserEnd;
            if aTask.Terminate(aEarliest,aStart,aEnd,aDuration,True) then
              begin
                StartDate:=aStart;
                FinishDate:=aEnd;
                UserTimes.Values[aUser]:=DateTimeToStr(aEnd);
              end;
          end;
        aTask.Free;
        EndUpdate;
        for i := 0 to IntervalCount-1 do
          MoveForwardCalc(Interval[i]);
      end;
  end;
var
  i : Integer;
begin
  UserTimes := TStringList.Create;
  UserTimes.NameValueSeparator:=':';
  Screen.Cursor := crHourGlass;
  for i := 0 to FGantt.IntervalCount-1 do
    MoveForwardCalc(FGantt.Interval[i]);
  FGantt.Invalidate;
  Screen.Cursor := crDefault;
  UserTimes.Free;
end;

procedure TfGanttView.bMoveBack1Click(Sender: TObject);
var
  i : Integer;
begin
  FReasonText:='';
  Screen.Cursor := crHourGlass;
  for i := 0 to FGantt.IntervalCount-1 do
    MoveTogether(FGantt.Interval[i],FGantt.Interval[i].StartDate);
  acCalculateForward.Execute;
  Screen.Cursor := crDefault;
end;
procedure TfGanttView.bMoveBackClick(Sender: TObject);
  function DoMoveBack(aInterval : TInterval) : TDateTime;
  var
    i: Integer;
    aDate,bDate: TDateTime;
    aDur: TDateTime;
    aBuffer: TDateTime;
  begin
    bDate := 99999999;
    with TInterval(aInterval) do
      begin
        BeginUpdate;
        for i := 0 to IntervalCount-1 do
          begin
            aDate := DoMoveBack(Interval[i]);
            if aDate < bDate then bDate := aDate;
          end;
        for i := 0 to ConnectionCount-1 do
          begin
            aDate := DoMoveBack(Connection[i]);
            if aDate < bDate then bDate := aDate;
          end;
        //Move Back
        aDur := NetDuration;
        if ResourceTimePerDay=0 then ResourceTimePerDay:=1;
        if NetDuration<(NetTime*(1/ResourceTimePerDay)) then
          aDur:=(NetTime*(1/ResourceTimePerDay));
        if aDur<0.5 then aDur:=0.5;
        if NetDuration<aDur then NetDuration:=aDur;
        if bDate<99999999 then
          FinishDate := bDate;
        //Add Weekends
        i := trunc(FinishDate);
        while i > FinishDate-aDur do
          begin
            if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
              aDur := aDur+1;
            dec(i,1);
          end;
        //TODO: Urlaub
        StartDate := (FinishDate-aDur);
        aBuffer := WaitTime;
        if aBuffer < (aDur*(seBuffer.Value/100)) then
          aBuffer := (aDur*(seBuffer.Value/100));
        EndUpdate;
        Result := StartDate;
      end;
  end;
var
  i: Integer;
  LastNode: TInterval;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    DoMoveBack(FGantt.Interval[i]);
  FGantt.Invalidate;
end;
procedure TfGanttView.bMoveFwdClick(Sender: TObject);
var
  i : Integer;
begin
  Screen.Cursor := crHourGlass;
  for i := 0 to FGantt.IntervalCount-1 do
    MoveForward(FGantt.Interval[i]);
  FGantt.Invalidate;
  Screen.Cursor := crDefault;
end;
procedure TfGanttView.acCenterTaskExecute(Sender: TObject);
var
  aTask: TTask;
  aDur: Int64;
  aStart: TDateTime;
  aEnd: TDateTime;
begin
  if Assigned(FGantt.Tree.Objects[0,FGantt.Tree.Row]) then
    begin
      aDur := round(FGantt.Calendar.Width/FGantt.Calendar.GetIntervalWidth);
      aStart := TInterval(FGantt.Tree.Objects[0,FGantt.Tree.Row]).StartDate;
      aEnd := TInterval(FGantt.Tree.Objects[0,FGantt.Tree.Row]).FinishDate;
      if (aEnd-aStart) > aDur then
        FGantt.StartDate:=aStart
      else
        FGantt.StartDate:=aStart-((aDur-((aEnd-aStart)/2)) / 2);
    end;
end;

procedure TfGanttView.acDeletetaskExecute(Sender: TObject);
var
  aTask: TTask;
  i: Integer;
begin
  if not Assigned(FSelectedInterval) then exit;
  aTask := TTask.Create(nil);
  try
    aTask.Select(FSelectedInterval.Id);
    aTask.Open;
    if aTask.Count>0 then
      begin
        aTask.Delete;
        UpdateDependencies(FSelectedInterval);
        for i := 0 to FSelectedInterval.ConnectionCount-1 do
          UpdateDependencies(FSelectedInterval.Connection[i]);
        FGantt.RemoveInterval(FSelectedInterval);
        FIntervals.Remove(FSelectedInterval);
        FreeAndNil(FSelectedInterval);
      end;
  finally
    aTask.Free;
  end;
end;

procedure TfGanttView.acExportToImageExecute(Sender: TObject);
var
  aStartTime: TDateTime;
  aEndTime: TDateTime;
  aList: TList;
  aBitmap: TBitmap;
  aTreeWidth: Integer;
  aGraphic: TGraphic;
  i: Integer;
  aDays: Extended;
  aPic: TPicture;
begin
  if SavePictureDialog1.Execute then
    begin
      aStartTime := Now()+999999;
      aEndTime := 0;
      for i := 0 to FGantt.IntervalCount-1 do
        begin
          if FGantt.Interval[i].StartDate<aStartTime then
            aStartTime := FGantt.Interval[i].StartDate;
          if FGantt.Interval[i].FinishDate>aEndTime then
            aEndTime := FGantt.Interval[i].FinishDate;
        end;
      aStartTime:=trunc(aStartTime)-1;
      aEndTime:=trunc(aEndTime)+1;
      FGantt.Align:=alNone;
      aList := TList.Create;
      Fgantt.MakeIntervalList(aList);
      aTreeWidth := FGantt.Tree.Width;
      FGantt.Tree.Width:=0;
      aBitmap := TBitmap.Create;
      aBitmap.Height := FGantt.Calendar.IntervalHeight*(aList.Count+3);
      FGantt.Height:=aBitmap.Height;
      aList.Free;
      aDays := (aEndTime-aStartTime);
      aBitmap.Width := round(UnitsBetweenDates(aStartTime,aEndTime+1,FGantt.MinorScale))*FGantt.PixelsPerMinorScale;
      Fgantt.Width:=aBitmap.Width;
      FGantt.StartDate:=aStartTime;
      FGantt.Tree.TopRow := 0;
      aBitmap.Canvas.Brush.Color:=clWhite;
      aBitmap.Canvas.FillRect(0,0,aBitmap.Width,aBitmap.Height);
      FGantt.Calendar.PaintToCanvas(aBitmap.Canvas,False);
      aPic := TPicture.Create;
      aPic.Bitmap.Assign(aBitmap);
      aPic.SaveToFile(SavePictureDialog1.FileName);
      aPic.Free;
      aBitmap.Free;
      FGantt.Tree.Width:=aTreeWidth;
      FGantt.Align:=alClient;
    end;
end;

procedure TfGanttView.acFindTimeSlotExecute(Sender: TObject);
var
  aTask: TTask;
  aStart,aEnd,aDuration : TDateTime;
  aEarliest: TDateTime;
  i: Integer;
begin
  if not Assigned(FSelectedInterval) then exit;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  aStart := now();
  aTask := TTask.Create(nil);
  aTask.Select(FSelectedInterval.Id);
  aTask.Open;
  if aTask.Count>0 then
    begin
      aEarliest := Now();
      for i := 0 to FSelectedInterval.DependencyCount-1 do
        if FSelectedInterval.Dependencies[i].FinishDate+FSelectedInterval.Dependencies[i].WaitTime>aEarliest then
          aEarliest:=FSelectedInterval.Dependencies[i].FinishDate+FSelectedInterval.Dependencies[i].WaitTime;
      if aTask.Terminate(aEarliest,aStart,aEnd,aDuration) then
        begin
          FSelectedInterval.StartDate:=aStart;
          FSelectedInterval.Duration:=aDuration;
          FSelectedInterval.FinishDate:=aEnd;
        end;
    end;
  aTask.Free;
  Screen.Cursor:=crDefault;
end;

procedure TfGanttView.acAddSubProjectsExecute(Sender: TObject);
var
  aProjects: TProjectList;
  aProject: TProject;
begin
  Application.ProcessMessages;
  if bCalculate2.Down then
    begin
      aProjects := TProjectList.Create(nil);
      aProjects.SelectFromParent(Fproject.Id.AsVariant);
      aProjects.Open;
      while not aProjects.EOF do
        begin
          aProject := TProject.Create(nil);
          aProject.Select(aProjects.Id.AsVariant);
          aProject.Open;
          if aProject.Count>0 then
            begin
              //if not FTasks.DataSet.Locate('COMPLETED','N',[]) then
              aProject.Tasks.SelectActive;
              aProject.Tasks.Open;
              Populate(aProject.Tasks,False);
            end;
          aProject.Free;
          aProjects.Next;
        end;
      aProjects.Free;
    end
  else Populate(FTasks);
end;

procedure TfGanttView.acAddtaskExecute(Sender: TObject);
var
  TP : TfTaskPlan;
  aActInt: TInterval;
  aParent: TTask;
  aTask: TTask;
  aInterval: TPInterval;
begin
  aActInt := TP.GetTaskIntervalFromCoordinates(FGantt,0,aClickPoint.y,0,True);
  aParent := TTask.Create(nil);
  if Assigned(aActInt) then
    begin
      aParent.Select(aActInt.Id);
      aParent.Open;
    end;
  aTask := TTask.Create(nil);
  aTask.Insert;
  aTask.FieldByName('PROJECT').AsString:=FTasks.FieldByName('PROJECT').AsString;
  aTask.FieldByName('PROJECTID').AsVariant:=FTasks.FieldByName('PROJECTID').AsVariant;
  aTask.FieldByName('SUMMARY').AsString:=strNewTask;
  if aParent.Count>0 then
    begin
      aTask.FieldByName('GPRIORITY').AsInteger:=aPArent.FieldByName('GPRIORITY').AsInteger;
      aTask.FieldByName('PARENT').AsVariant:=aPArent.FieldByName('PARENT').AsVariant;
    end;
  aTask.Post;
  FTasks.DataSet.Refresh;
  aInterval := TPInterval.Create(FGantt);
  FIntervals.Add(aInterval);
  FillInterval(aInterval,aTask);
  aInterval.Visible:=True;
  aInterval.Id := aTask.Id.AsVariant;
  aInterval.OnChanged:=@aIntervalChanged;
  aInterval.OnDrawBackground:=@aIntervalDrawBackground;
  if Assigned(aActInt) then
    begin
      if Assigned(aActInt.Parent) then
        aActInt.Parent.InsertInterval(aActInt.Index+1,aInterval)
      else aActInt.AddInterval(aInterval);
    end
  else
    FGantt.Interval[0].AddInterval(aInterval);
  aParent.Free;
  aTask.Free;
end;

procedure TfGanttView.acCalculatefromHereExecute(Sender: TObject);
var
  TP : TfTaskPlan;
  aLink: String;
  aInt: TInterval;
begin
  aLink := TP.GetTaskFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
  if aLink <> '' then
    begin
      aInt := TP.GetTaskIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
      if Assigned(aInt) then
        begin
          MoveForward(aInt,true);
        end;
    end;
end;

procedure TfGanttView.acAddSnapshotExecute(Sender: TObject);
var
  aName: String;
begin
  aName := strSnapshot+' '+IntToStr(cbSnapshot.Items.Count);
  if InputQuery(strSnapshot,strName,aName) then
    begin
      FTasks.First;
      while not FTasks.EOF do
        begin
          FTasks.MakeSnapshot(aName);
          FTasks.Next;
        end;
      cbSnapshot.Items.Add(aName);
    end;
end;

procedure TfGanttView.acAddHistoryExecute(Sender: TObject);
var
  i: Integer;
  aClass: TBaseDBDatasetClass;
  aObj: TBaseDBDataset;
  aHist : IBaseHistory;
begin
  if fHistoryAddItem.Execute then
    begin
      FProject.History.AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,'',fHistoryAddItem.eReference.Text,FProject.DataSet,ACICON_USEREDITED,'',True,True);
      for i := 0 to fHistoryAddItem.lbAdditional.Count-1 do
        if Data.ListDataSetFromLink(fHistoryAddItem.lbAdditional.Items[i],aClass) then
          begin
            aObj := aClass.Create(nil);
            if aObj is TBaseDbList then
              begin
                TBaseDBList(aObj).SelectFromLink(fHistoryAddItem.lbAdditional.Items[i]);
                aObj.Open;
                if aObj.Count>0 then
                  begin
                    if Supports(aObj,IBaseHistory,aHist) then
                      begin
                        aHist.History.AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,'',fHistoryAddItem.eReference.Text,nil,ACICON_USEREDITED,'',True,True);
                        aHist := nil;
                      end;
                  end;
              end;
            aObj.Destroy;
          end;
      fHistoryAddItem.lbAdditional.Clear;
    end;
end;

procedure TfGanttView.acMakePossibleExecute(Sender: TObject);
  function DoMove(aInterval : TInterval) : Boolean;
  var
    b: Integer;
    aDur: TDateTime;
  begin
    Result := False;
    if (aInterval.StartDate<Now()) and (not aInterval.Started) and (not aInterval.Fixed) then
      begin
        aDur := aInterval.Duration;
        aInterval.FinishDate:=Now()+aDur;
        aInterval.StartDate:=Now();
      end;
    for b := 0 to aInterval.IntervalCount-1 do
      Result := DoMove(aInterval.Interval[b]);
  end;
var
  i: Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    DoMove(FGantt.Interval[i]);
  acCalculateForward.Execute;
end;

procedure TfGanttView.acMoveTogetherFromHereExecute(Sender: TObject);
var
  TP : TfTaskPlan;
  aLink: String;
  aInt: TInterval;
begin
  aLink := TP.GetTaskFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
  if aLink <> '' then
    begin
      aInt := TP.GetTaskIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
      if Assigned(aInt) then
        begin
          MoveTogether(aInt,aInt.StartDate,True);
          MoveForward(aInt,True);
        end;
    end;
end;

procedure TfGanttView.acOpenExecute(Sender: TObject);
  function MustChange(aParent : TInterval) : Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if not Assigned(aParent) then exit;
    for i := 0 to aParent.IntervalCount-1 do
      begin
        if aParent.Interval[i].Changed then
          Result := True;
        Result := Result or MustChange(aParent.Interval[i]);
        if Result then break;
      end;
    if aParent.Changed then
      Result := True;
  end;
var
  aLink: String;
  aEdit: TfTaskEdit;
  TP : TfTaskPlan;
  aInt: gsGanttCalendar.TInterval;
  aTask: TTask;
  i: Integer;
  DoChange: Boolean;
begin
  aLink := TP.GetTaskFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
  if aLink <> '' then
    begin
      aInt := TP.GetTaskIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
      if Assigned(aInt) then
        begin
          DoChange := aInt.Changed;
          for i := 0 to aInt.DependencyCount-1 do
            if aInt.Dependencies[i].Changed then
              DoChange := True;
          if (not DoChange) or (MessageDlg(strSaveTaskChanges,mtInformation,[mbYes,mbNo],0) = mrYes) then
            begin
              if aInt.Changed then
                ChangeTask(FTasks,aInt);
              for i := 0 to aInt.DependencyCount-1 do
                if aInt.Dependencies[i].Changed then
                  ChangeTask(FTasks,aInt.Dependencies[i]);
              aEdit :=TfTaskEdit.Create(Self);
              if aEdit.Execute(aLink) then
                begin
                  aTask := TTask.Create(nil);
                  aTask.SelectFromLink(aLink);
                  aTask.Open;
                  FillInterval(TPInterval(aInt),aTask);
                  UpdateDependencies(TPInterval(aInt));
                  aTask.Free;
                  FGantt.Calendar.Invalidate;
                end;
              aEdit.Free;
            end;
        end;
    end;
end;

procedure TfGanttView.acRefreshWizardExecute(Sender: TObject);
begin
  if fRefreshWizard.Execute then
    begin
      FReasonText:=fRefreshWizard.mReason.Lines.Text;
      if fRefreshWizard.rbPlan1.Checked then
        acCalculatePlanWithUsage.Execute
      else if fRefreshWizard.rbPlan2.Checked then
        acCalculatePlanWithUsageWithoutDependencies.Execute
      else if fRefreshWizard.rbPlan3.Checked then
        acCalculateForward.Execute
      else if fRefreshWizard.rbPlan4.Checked then
        acMakePossible.Execute;
    end;
end;

procedure TfGanttView.bMonthViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsQuarter;
  FGantt.MinorScale:=tsMonth;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;
procedure TfGanttView.bRefresh1Click(Sender: TObject);
begin
  FindCriticalPath;
  FGantt.Calendar.Invalidate;
end;
procedure TfGanttView.bRefreshClick(Sender: TObject);
begin
  Populate(FTasks);
  if bCalculate2.Down then
    acAddSubProjects.Execute;
end;
procedure TfGanttView.bShowTasksClick(Sender: TObject);
begin
  Application.ProcessMessages;
  FGantt.Calendar.Invalidate;
end;
procedure TfGanttView.bTodayClick(Sender: TObject);
begin
  FGantt.StartDate:=Now();
end;
procedure TfGanttView.bWeekViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsMonth;
  FGantt.MinorScale:=tsWeekNumPlain;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfGanttView.cbSnapshotSelect(Sender: TObject);
var
  aInt: TInterval;
begin
  FreeAndNil(FSnapshots);
  if (cbSnapshot.Items[cbSnapshot.ItemIndex] <> strNoSnapshot) then
    begin
      FSnapshots := TInterval.Create(nil);
      FTasks.First;
      FTasks.Snapshots.Open;
      while not FTasks.EOF do
        begin
          if FTasks.Snapshots.DataSet.Locate('NAME',cbSnapshot.Text,[]) then
            begin
              aInt := TInterval.Create(nil);
              aInt.StartDate:=FTasks.Snapshots.FieldByName('STARTDATE').AsDateTime;
              aInt.FinishDate:=FTasks.Snapshots.FieldByName('ENDDATE').AsDateTime;
              aInt.Id:=FTasks.Id.AsVariant;
              FSnapshots.AddInterval(aInt);
            end;
          FTasks.Next;
        end;
    end;
  FGantt.Calendar.Invalidate;
end;

procedure TfGanttView.FGanttCalendarClick(Sender: TObject);
var
  TP : TfTaskPlan;
  aInt: TInterval;
  ay: Integer;
begin
  aClickPoint := FGantt.Calendar.ScreenToClient(Mouse.CursorPos);
  ay := aClickPoint.Y-FGantt.Calendar.StartDrawIntervals;
  ay := ay div max(FGantt.Calendar.PixelsPerLine,1);
  ay := ay+(FGantt.Tree.TopRow-1);
  FGantt.Tree.Row:=ay+1;
end;

procedure TfGanttView.FGanttCalendarDblClick(Sender: TObject);
begin
  aClickPoint := FGantt.Calendar.ScreenToClient(Mouse.CursorPos);
  acOpen.Execute;
end;

procedure TfGanttView.FGanttCalendarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  aClickPoint := FGantt.Calendar.ScreenToClient(Mouse.CursorPos);
end;

procedure TfGanttView.FGanttCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ay: Integer;
  FtSelectedRow: Int64;
  FtSelectedCol: Extended;
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
procedure TfGanttView.FGanttCalendarMoveOverInterval(Sender: TObject;
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
procedure TfGanttView.FGanttCalendarShowHint(Sender: TObject;
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
  DD: Char;
begin
  if HintInfo^.HintStr='' then
    begin
      List := TList.Create;
      FGantt.MakeIntervalList(List);
      ay := HintInfo^.CursorPos.Y-FGantt.Calendar.StartDrawIntervals;
      ay := ay div FGantt.Calendar.PixelsPerLine;
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
constructor TfGanttView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FManualStarted:=False;
  FCollectUsageList := TList.Create;
  aSelInterval:=0;
  FIntervals := TList.Create;
  FGantt := TgsGantt.Create(Self);
  FThreads := TList.Create;
  FGantt.Parent := pgantt;
  FGantt.Align:=alClient;
  FGantt.Tree.AfterUpdateCommonSettings:=@FGanttTreeAfterUpdateCommonSettings;
  FRessources := TList.Create;
  FGantt.Calendar.OnMoveOverInterval:=@FGanttCalendarMoveOverInterval;
  FGantt.Calendar.OnShowHint:=@FGanttCalendarShowHint;
  FGantt.Calendar.OnMouseMove:=@FGanttCalendarMouseMove;
  FGantt.Calendar.OnMouseDown:=@FGanttCalendarMouseDown;
  FGantt.Calendar.OnDblClick:=@FGanttCalendarDblClick;
  FGantt.Calendar.OnClick:=@FGanttCalendarClick;
  FGantt.Tree.PopupMenu:=PopupMenu1;
  FGantt.Calendar.PopupMenu:=PopupMenu2;
  bDayViewClick(nil);
  FGantt.Calendar.ShowHint:=True;
  FSnapshots := nil;
end;
destructor TfGanttView.Destroy;
begin
  CleanIntervals;
  FIntervals.Free;
  FThreads.Free;
  FRessources.Free;
  FCollectUsageList.Free;
  inherited Destroy;
end;
procedure TfGanttView.Populate(aTasks: TTaskList; DoClean: Boolean;
  AddInactive: Boolean);
var
  aNewInterval: TInterval;
  aTask: TTask;
  aInterval: TInterval;
  aDep: TInterval;
  i: Integer;
  aRoot: TInterval;
  deps : array of LargeInt;
  Snapshotsfound: Integer;
  function AddTask(AddParents : Boolean = True;Root : TInterval = nil) : TInterval;
  var
    aInterval: TInterval;
    aIParent: TInterval = nil;
    aRec: LargeInt;
    aParent : Variant;
    aDue: TDateTime;
    aStart: TDateTime;
    i: Integer;
    bTasks: TTaskList;
    bInterval: TInterval;
    aColFinish: TDateTime;
    aTh: TCollectThread;
  begin
    Result := nil;
    if (aTasks.FieldByName('PARENT').AsString <> '') then
      begin
        aIParent := IntervalById(aTasks.FieldByName('PARENT').AsVariant,Root);
        if not Assigned(aIParent) then
          begin
            aRec := aTasks.GetBookmark;
            aParent := aTasks.FieldByName('PARENT').AsVariant;
            if not (aParent=aTasks.Id.AsVariant) then
              if aTasks.DataSet.Locate('SQL_ID',aParent,[]) then
                aIParent := AddTask(True,Root);
            aTasks.GotoBookmark(aRec);
          end;
      end;
    aInterval := TPInterval.Create(FGantt);
    FIntervals.Add(aInterval);
    FillInterval(aInterval,aTasks);
    aInterval.Visible:=True;
    aInterval.Id := aTasks.Id.AsVariant;
    aInterval.OnChanged:=@aIntervalChanged;
    aInterval.OnDrawBackground:=@aIntervalDrawBackground;
    if Assigned(aIParent) then
      begin
        aIParent.AddInterval(aInterval);
      end
    else
      aRoot.AddInterval(aInterval);
    Result := aInterval;
    aInterval.Pointer := nil;
    if aTasks.FieldByName('USER').AsString <> '' then
      begin
        for i := 0 to FRessources.Count-1 do
          if TRessource(FRessources[i]).Accountno = aTasks.FieldByName('USER').AsString then
            begin
              aInterval.Pointer := FRessources[i];
              break;
            end;
        if not Assigned(aInterval.Pointer) then
          begin
            i := FRessources.Add(TRessource.Create(nil));
            aInterval.Pointer := TRessource(FRessources[i]);
            TRessource(FRessources[i]).Accountno:=aTasks.FieldByName('USER').AsString;
            if (not Assigned(FProject)) or (Fproject.FieldByName('END').AsDateTime<Now()) then
              aColFinish := Now()+100
            else
              aColFinish := FProject.FieldByName('END').AsDateTime+30;
            aTh := TCollectThread.Create(FGantt.Calendar,Now()-30,aColFinish,TRessource(FRessources[i]),aTasks.FieldByName('USER').AsString,False,True,True,aInterval);
            aTh.Resume;
            FThreads.Add(TCollectThread.Create(FGantt.Calendar,Now()-30,aColFinish,TRessource(FRessources[i]),aTasks.FieldByName('USER').AsString,True,False,True,aInterval));
            TCollectThread(FThreads[FThreads.Count-1]).OnTerminate:=@TCollectThreadTerminate;
          end;
      end;
    aInterval.Changed:=False;
  end;
begin
  setlength(deps,0);
  Screen.Cursor:=crHourGlass;
  FGantt.BeginUpdate;
  fLogWaitForm.SetLanguage;
  fLogWaitForm.Show;
  fLogWaitForm.ShowInfo(strCollectingTasks);
  try
    if DoClean then
      begin
        CleanIntervals;
      end;
    bSave.Enabled:=False;
    bCSave.Enabled:=False;
    bCancel.Enabled:=False;
    aTasks.First;
    aRoot := TInterval.Create(FGantt);
    FIntervals.Add(aRoot);
    FGantt.AddInterval(aRoot);
    aRoot.Task:=TProjectList(aTasks.Parent).Text.AsString;
    cbSnapshot.Items.Clear;
    cbSnapshot.Items.Add(strNoSnapshot);
    Snapshotsfound := 0;
    while not aTasks.EOF do
      begin
        if (aTasks.FieldByName('ACTIVE').AsString<>'N') or AddInactive then
          if IntervalById(aTasks.Id.AsVariant)=nil then
            begin
              aInterval := AddTask(True,aRoot);
              //aTasks.Dependencies.Open;
              if {aTasks.Dependencies.Count>0} atasks.FieldByName('DEPDONE').AsString='N' then
                begin
                  setlength(deps,length(deps)+1);
                  deps[length(deps)-1] := aTasks.GetBookmark;
                end;
              if Snapshotsfound<200 then
                begin
                  if not aTasks.Snapshots.DataSet.Active then aTasks.Snapshots.Open;
                  aTasks.Snapshots.First;
                  while not aTasks.Snapshots.EOF do
                    begin
                      if cbSnapshot.Items.IndexOf(aTasks.Snapshots.FieldByName('NAME').AsString)=-1 then
                        cbSnapshot.Items.Add(aTasks.Snapshots.FieldByName('NAME').AsString);
                      aTasks.Snapshots.Next;
                      inc(Snapshotsfound);
                    end;
                end
              else aTasks.Snapshots.Close;
            end;
        aTasks.Next;
      end;
    fLogWaitForm.ShowInfo(strCollectingDependencies);
    for i := low(deps) to high(deps) do
      begin
        aTasks.GotoBookmark(deps[i]);
        aTasks.Dependencies.Open;
        aTasks.Dependencies.First;
        while not aTasks.Dependencies.DataSet.EOF do
          begin
            aDep := IntervalById(aTasks.Dependencies.FieldByName('REF_ID_ID').AsVariant);
            if Assigned(aDep) then
              begin
                aInterval := IntervalById(aTasks.Id.AsVariant);
                aDep.AddConnection(aInterval,aTasks.FieldByName('STARTDATE').IsNull and aTasks.FieldByName('DUEDATE').IsNull,False);
              end;
            aTasks.Dependencies.Next;
          end;
      end;
    if aRoot.IntervalCount>0 then
      aRoot.Visible:=True;
  finally
    FGantt.EndUpdate;
    fLogWaitForm.Hide;
    Screen.Cursor:=crDefault;
  end;
  FGantt.Tree.TopRow:=1;
  FGantt.StartDate:=Now();
  FindCriticalPath;
end;
procedure TfGanttView.DoSave(aChangeMilestones: Boolean; aSetTermins: Boolean);
var
  aStart: TDateTime;
  aEnd: Extended;
  procedure RecoursiveChange(aParent : TInterval);
  var
    i: Integer;
  begin
    for i := 0 to aParent.IntervalCount-1 do
      begin
        if aParent.Interval[i].Changed then
          begin
            ChangeTask(FProject.Tasks,aParent.Interval[i],aChangeMilestones,FReasonText,aSetTermins);
          end;
        if aParent.Interval[i].StartDate<aStart then
          aStart:=aParent.Interval[i].StartDate;
        if aParent.Interval[i].FinishDate>aEnd then
          aEnd:=aParent.Interval[i].FinishDate;
        RecoursiveChange(aParent.Interval[i]);
      end;
    if aParent.Changed then
      ChangeTask(FProject.Tasks,aParent);
  end;
var
  i: Integer;
begin
  aStart := trunc(Now())+10000;
  aEnd := trunc(Now())-10000;
  Application.ProcessMessages;
  Screen.Cursor:=crHourGlass;
  FindCriticalPath;
  for i := 0 to FGantt.IntervalCount-1 do
    RecoursiveChange(FGantt.Interval[i]);
  FProject.Edit;
  Fproject.FieldByName('CRITPATH').AsFloat:=FCriticalPathLength;
  if aStart<>trunc(Now())+10000 then
    Fproject.FieldByName('START').AsDateTime:=aStart;
  if aEnd<>trunc(Now())-10000 then
    Fproject.FieldByName('END').AsDateTime:=aEnd;
  Screen.Cursor:=crDefault;
end;
procedure TfGanttView.CleanIntervals;
var
  i: Integer;
  aInt: TInterval;
begin
  FGantt.BeginUpdate;
  while FIntervals.Count>0 do
    begin
       aInt := TInterval(FIntervals[FIntervals.Count-1]);
       FIntervals.Remove(aInt);
       FGantt.RemoveInterval(aInt);
       aInt.Free;
    end;
  FIntervals.Clear;
  for i := 0 to FRessources.Count-1 do
    begin
      while TRessource(FRessources[i]).IntervalCount>0 do
        begin
          aInt := TInterval(TRessource(FRessources[i]).Interval[TRessource(FRessources[i]).IntervalCount-1]);
          TRessource(FRessources[i]).RemoveInterval(aInt);
          aInt.Free;
        end;
      TRessource(FRessources[i]).Free;
    end;
  FRessources.Clear;
  FGantt.EndUpdate;
end;
function TfGanttView.FindCriticalPath : TInterval;
var
  aLastInterval: TInterval;
  bTmp: TInterval;
  y: Integer;
  aIntervals : TList;
  aLen: Extended;

  function FindLastConn(aInterval : TInterval) : TInterval;
  var
    aTmp: TInterval;
    i: Integer;
  begin
    Result := nil;
    for i := 0 to aInterval.IntervalCount-1 do
      begin
        if aIntervals.IndexOf(aInterval.Interval[i])=-1 then
          aIntervals.Add(aInterval.Interval[i]);
        aInterval.Interval[i].InCriticalPath:=False;
        aTmp := FindLastConn(aInterval.Interval[i]);
        if aTmp.Latest>Result.Latest then
          Result := aTmp;
      end;
    for i := 0 to aInterval.ConnectionCount-1 do
      begin
        if aIntervals.IndexOf(aInterval.Connection[i])=-1 then
          aIntervals.Add(aInterval.Connection[i]);
        aTmp := aInterval.Connection[i];
        if aTmp.Latest>Result.Latest then
          Result := aTmp;
      end;
    if aIntervals.IndexOf(aInterval)=-1 then
      aIntervals.Add(aInterval);
    if aInterval.Latest>Result.Latest then
      Result := aInterval;
  end;

  procedure DoPath(aInterval : TInterval);
  var
    aLatestConn: TInterval;
    i: Integer;
  begin
    aLatestConn := nil;
    for i := 0 to aIntervals.Count-1 do
      if TInterval(aIntervals[i]).ConnectionExists(aInterval) then
        if TInterval(aIntervals[i]).Latest>aLatestconn.Latest then
          begin
            aLatestConn := TInterval(aIntervals[i]);
          end;
    if Assigned(aLatestConn) then
      begin
        aLatestConn.InCriticalPath:=True;
        FCriticalPathLength+=aLatestConn.NetTime+aLatestConn.WaitTime;
        DoPath(aLatestConn);
      end;
  end;

begin
  FCriticalPathLength := 0.0;
  aLastInterval := nil;
  aIntervals := TList.Create;
  for y := 0 to fGantt.IntervalCount-1 do
    begin
      bTmp := FindLastConn(FGantt.Interval[y]);
      if bTmp.Latest>aLastInterval.Latest then
        aLastInterval := bTmp;
    end;
  for y := 0 to aIntervals.Count-1 do
    TInterval(aIntervals[y]).InCriticalPath:=False;
  if Assigned(aLastInterval) then
    begin
      DoPath(aLastInterval);
    end;
  if Assigned(aLastInterval) then
    aLastInterval.InCriticalPath:=True;
  aIntervals.Free;
  Result := aLastInterval;
  FGantt.Calendar.Invalidate;
end;
procedure TfGanttView.FillInterval(aInterval : TInterval; aTasks: TTaskList;MarkDependencies : Boolean = False;aUser : TUser = nil);
var
  aStart: TDateTime;
  aDue: TDateTime;
  aChanged: Boolean;
  FUsage: Extended;
  FWorkTime: Extended;
  i: Integer;
  DestrUser: Boolean = False;
begin
  aChanged := aTasks.CalcDates(aStart,aDue);
  aInterval.Task:=aTasks.FieldByName('SUMMARY').AsString;
  aInterval.Project:=aTasks.FieldByName('PROJECT').AsString;
  aInterval.Started:=not aTasks.FieldByName('STARTEDAT').IsNull;
  if MarkDependencies then
    if (aTasks.FieldByName('DEPDONE').AsString='N') then
      begin
        aInterval.Color:=clLtGray;
      end;
  if (aTasks.FieldByName('COMPLETED').AsString='Y') then
    begin
      aInterval.Color:=clGray;
      aInterval.Fixed:=True;
    end;
  aInterval.Id:=aTasks.Id.AsVariant;
  aInterval.DepDone := aTasks.FieldByName('DEPDONE').AsString <> 'N';
  if not aTasks.FieldByName('PLANTIME').IsNull then
    aInterval.NetTime:=aTasks.FieldByName('PLANTIME').AsFloat;
  if not Assigned(aUser) then
    begin
      aUser := TUser.Create(nil);
      DestrUser := True;
    end;
  if not aUser.Locate('ACCOUNTNO',aTasks.FieldByName('USER').AsString,[]) then
    begin
      aUser.SelectByAccountno(aTasks.FieldByName('USER').AsString);
      aUser.Open;
    end;
  if aUser.Count>0 then
    begin
      aInterval.Resource := aUser.Text.AsString;
      FUsage := aUser.FieldByName('USEWORKTIME').AsInteger/100;
      if FUsage = 0 then FUsage := 1;
      FWorkTime:=aUser.WorkTime*FUsage;
      FUsage := FWorkTime/8;
      if aInterval is TPInterval then
        begin
          TPInterval(aInterval).Usage:=FUsage;
          TPInterval(aInterval).UserID:=aUser.Id.AsVariant;
          TPInterval(aInterval).WorkTime:=FWorkTime;
        end;
      aInterval.ResourceTimePerDay:=FUsage;
    end
  else
    aInterval.ResourceTimePerDay := 1;
  if DestrUser then
    aUser.Free;
  aInterval.FinishDate:=aDue;
  aInterval.StartDate:=aStart;
  if not aTasks.FieldByName('EARLIEST').IsNull then
    aInterval.Earliest := aTasks.FieldByName('EARLIEST').AsDateTime;
  if not (aTasks.FieldByName('BUFFERTIME').AsString = '') then
    aInterval.WaitTime:=aTasks.FieldByName('BUFFERTIME').AsFloat;
  aInterval.NetDuration:=aInterval.Duration;
  for i := trunc(aInterval.StartDate) to Trunc(aInterval.FinishDate) do
    if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
      aInterval.NetDuration:= aInterval.NetDuration-1;
  aInterval.Changed:=aChanged;
end;
procedure TfGanttView.GotoTask(aLink: string);
var
  aTask: TTask;
  i: Integer;
  tTop: objpas.Integer;
  aDur: Int64;
  aStart: TDateTime;
  aEnd: TDateTime;
begin
  aTask := TTask.Create(nil);
  aTask.SelectFromLink(aLink);
  aTask.Open;
  if aTask.Count>0 then
    begin
      for i := 0 to FGantt.Tree.RowCount-1 do
        if Assigned(FGantt.Tree.Objects[0, i])
        and (TInterval(FGantt.Tree.Objects[0, i]).Id=aTask.Id.AsVariant) then
          begin
            tTop := (FGantt.Tree.Height div FGantt.Tree.DefaultRowHeight)-2;
            tTop := i-(tTop div 2);
            if tTop<0 then tTop := 0;
            FGantt.Tree.TopRow:=tTop;
            FGantt.Tree.Row:=i;
            FGantt.Tree.Col:=2;
            aDur := round(FGantt.Calendar.Width/FGantt.Calendar.GetIntervalWidth);
            aTask.CalcDates(aStart,aEnd);
            if (aEnd-aStart) > aDur then
              FGantt.StartDate:=aStart
            else
              FGantt.StartDate:=aStart-((aDur-((aEnd-aStart)/2)) / 2);
            break;
          end;
    end;
  aTask.Free;
end;
function TfGanttView.Execute(aProject: TProject; aLink: string;
  DoClean: Boolean; AddInactive: Boolean): Boolean;
var
  i: Integer;
  a: Extended;
  Found: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGanttView,fGanttView);
      Self := fGanttView;
    end;
  FReasonText:='';
  FResourcesRead := False;
  FProject := aproject;
  FTasks := aProject.Tasks;
  Populate(FTasks,DoClean,AddInactive);
  SetRights;
  Addhelp;
  ModalResult := mrNone;
  bShowTasks.Down:=False;
  if aLink <> '' then
    GotoTask(aLink);
  Caption := strGanttView+' - '+FTasks.Parent.FieldByName('NAME').AsString;
  Show;
  while Visible do
    begin
      if bShowTasks.Down then
        begin
          if (FThreads.Count>0) and (not FManualStarted) then
            begin
              TCollectThread(FThreads[0]).Resume;
              iHourglass.Visible:=True;
            end
          else
            FGantt.Invalidate;
          Found := False;
          for i := 0 to 50 do
            if FCollectUsageList.Count>0 then
              begin
                a := TPInterval(FCollectUsageList.Items[0]).PercentUsage;
                FCollectUsageList.Delete(0);
                Found := True;
              end;
          if Found then
            begin
              iHourglass.Visible:=True;
              FGantt.Invalidate;
            end
          else if (FThreads.Count=0) then
            iHourglass.Visible:=false;
        end;
      Application.ProcessMessages;
      sleep(100);
    end;
  Result := ModalResult = mrOK;
  for i := 0 to FThreads.Count-1 do
    begin
      TCollectThread(FThreads[i]).Terminate;
      TCollectThread(FThreads[i]).Resume;
    end;
  FThreads.Clear;
  FCollectUsageList.Clear;
  CleanIntervals;
end;

function TfGanttView.Calculate(aProject: TProject; DoClean: Boolean;
  AddInactive: Boolean): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGanttView,fGanttView);
      Self := fGanttView;
    end;
  FProject := aproject;
  FTasks := aProject.Tasks;
  Populate(FTasks,DoClean,AddInactive);
  SetRights;
  bCalculatePlanClick(nil);
  bSave.Click;
end;

function TfGanttView.MoveFwd(aProject: TProject; DoClean: Boolean;
  AddInactive: Boolean): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGanttView,fGanttView);
      Self := fGanttView;
    end;
  FProject := aproject;
  FTasks := aProject.Tasks;
  Populate(FTasks,DoClean,AddInactive);
  SetRights;
  bMoveFwdClick(nil);
  bSave.Click;
end;

function TfGanttView.MoveAndCalculate(aProject: TProject; DoClean: Boolean;
  AddInactive: Boolean): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGanttView,fGanttView);
      Self := fGanttView;
    end;
  FProject := aproject;
  FTasks := aProject.Tasks;
  Populate(FTasks,DoClean,AddInactive);
  SetRights;
  acMakePossible.Execute;
  bSave.Click;
end;

procedure TfGanttView.SetRights;
begin
  acMakePossible.Enabled := Data.Users.Rights.Right('PROJECTS') > RIGHT_READ;
  acAddSnapshot.Enabled := Data.Users.Rights.Right('PROJECTS') > RIGHT_READ;
  pCalc.Enabled := Data.Users.Rights.Right('PROJECTS') > RIGHT_READ;
end;

end.

