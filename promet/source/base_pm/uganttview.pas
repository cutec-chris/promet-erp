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
  Math, uProjects;

type

  { TfGanttView }

  TfGanttView = class(TForm)
    acCenterTask: TAction;
    acOpen: TAction;
    acMakePossible: TAction;
    acAddSubProjects: TAction;
    acAddSnapshot: TAction;
    acExportToImage: TAction;
    ActionList1: TActionList;
    bCalculate1: TSpeedButton;
    bCalculate2: TSpeedButton;
    bMoveBack: TSpeedButton;
    bDayView: TSpeedButton;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bMonthView: TSpeedButton;
    bMoveBack1: TSpeedButton;
    bSave1: TSpeedButton;
    bShowTasks: TSpeedButton;
    bMoveFwd: TSpeedButton;
    bShowTasks1: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    cbSnapshot: TComboBox;
    Label4: TLabel;
    Label8: TLabel;
    lDate: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem1: TMenuItem;
    Panel10: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pCalc: TPanel;
    Panel9: TPanel;
    pgantt: TPanel;
    Panel7: TPanel;
    PopupMenu1: TPopupMenu;
    SavePictureDialog1: TSavePictureDialog;
    seBuffer: TSpinEdit;
    tbTop: TPanel;
    RecalcTimer: TTimer;
    bSave: TSpeedButton;
    bCancel: TSpeedButton;
    bCSave: TSpeedButton;
    procedure acAddSnapshotExecute(Sender: TObject);
    procedure acAddSubProjectsExecute(Sender: TObject);
    procedure acCenterTaskExecute(Sender: TObject);
    procedure acExportToImageExecute(Sender: TObject);
    procedure acMakePossibleExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure aIntervalChanged(Sender: TObject);
    procedure aIntervalDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double);
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
    procedure FGanttCalendarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FGanttCalendarMoveOverInterval(Sender: TObject;
      aInterval: TInterval; X, Y: Integer);
    procedure FGanttCalendarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
    procedure FGanttTreeResize(Sender: TObject);
    procedure RecalcTimerTimer(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bCSaveClick(Sender: TObject);
  private
    { private declarations }
    FResourcesRead : Boolean;
    FGantt: TgsGantt;
    Fproject : TProject;
    FTasks : TTaskList;
    FRessources : TList;
    FHintRect : TRect;
    FRow : Integer;
    aClickPoint: TPoint;
    aSelInterval : Integer;
    FSnapshots : TInterval;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(aTasks: TTaskList; DoClean: Boolean=True;AddInactive : Boolean = False);
    procedure DoSave;
    procedure CleanIntervals;
    function FindCriticalPath: TInterval;
    procedure FillInterval(aInterval : TInterval;aTasks : TTaskList);
    procedure GotoTask(aLink : string);
    function Execute(aProject : TProject;aLink : string = ''; DoClean: Boolean=True;AddInactive : Boolean = False) : Boolean;
    function Calculate(aProject : TProject;DoClean: Boolean=True;AddInactive : Boolean = False) : Boolean;
    procedure SetRights;
  end;

var
  fGanttView: TfGanttView;

implementation
uses uData,LCLIntf,uBaseDbClasses,uTaskEdit,variants,LCLProc,uTaskPlan,
  uIntfStrConsts,uColors,uBaseDBInterface,Grids,uLogWait;
{$R *.lfm}
resourcestring
  strSnapshot                             = 'Snapshot';
  strNoSnapshot                           = '<keiner>';
  strCommitChanges                        = 'Sollen wirklich alle Änderungen in die Aufgaben eingetragen werden ?';
  strCancelChanges                        = 'Sollen wirklich alle Änderungen verworfen werden ?';
  strCollectingTasks                      = 'Ansicht wird aufgebaut...';
  strCollectingDependencies               = 'Abhängigkeiten werden ermittelt...';
  strCollectingresourceTimes              = '... Urlaubszeiten ermitteln';

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

procedure TfGanttView.RecalcTimerTimer(Sender: TObject);
begin
  if FGantt.Calendar.IsDragging then exit;
  RecalcTimer.Enabled:=False;
  FindCriticalPath;
  FGantt.Calendar.Invalidate;
end;

procedure TfGanttView.bSaveClick(Sender: TObject);
begin
  if (MessageDlg(strCommitChanges,mtInformation,[mbYes,mbNo],0) = mrYes) then
    DoSave;
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
begin
  bSave.Enabled:=True;
  bCSave.Enabled:=True;
  bCancel.Enabled:=true;
  with TInterval(Sender) do
    begin
      if TInterval(Sender).Fixed then exit;
      TInterval(Sender).BeginUpdate;
      //if TInterval(Sender).MovedFwd then
        begin
          debugln('IntervalChanged('+TInterval(Sender).Task+')');
          //Move Forward
          aDur := NetDuration;
          if ResourceTimePerDay=0 then ResourceTimePerDay:=1;
          if NetDuration<(NetTime*(1/ResourceTimePerDay)) then
            aDur:=(NetTime*(1/ResourceTimePerDay));
          if aDur<0.5 then aDur:=0.5;
          if NetDuration<aDur then NetDuration:=aDur;
          if TInterval(Sender).StartDate<TInterval(Sender).Earliest then
            TInterval(Sender).StartDate:=TInterval(Sender).Earliest;
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
          IntervalDone:=TInterval(Sender).StartDate;
          for i := 0 to ConnectionCount-1 do
            begin
              Connection[i].BeginUpdate;
              oD := Connection[i].Duration;
              if Connection[i].StartDate<FinishDate+Buffer then
                begin
                  for c := 0 to Connection[i].IntervalCount-1 do
                    if Connection[i].Interval[c].StartDate<FinishDate+Buffer then
                      begin
                        oD2 := Connection[i].Interval[c].Duration;
                        Connection[i].Interval[c].BeginUpdate;
                        Connection[i].Interval[c].StartDate:=FinishDate+Buffer;
                        Connection[i].Interval[c].FinishDate:=FinishDate+Buffer+oD2;
                        Connection[i].Interval[c].EndUpdate;
                      end;
                  Connection[i].StartDate:=FinishDate+Buffer;
                end;
              if Connection[i].FinishDate<Connection[i].StartDate+oD then
                Connection[i].FinishDate:=Connection[i].StartDate+oD;
              Connection[i].IntervalDone:=Connection[i].StartDate;
              Connection[i].EndUpdate;
            end;
        end;
      TInterval(Sender).ResetMovement;
      TInterval(Sender).Endupdate(True);
      RecalcTimer.Enabled := True;
    end;
end;
procedure TfGanttView.aIntervalDrawBackground(Sender: TObject; aCanvas: TCanvas;
  aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double);
var
  TaskPlan : TfTaskPlan;
  i: Integer;
  aInterval: TInterval;
  aIStart: TDateTime;
  aIEnd: TDateTime;
  aAddTop : Integer = 0;
  aDrawRect: TRect;
begin
  Taskplan.aIDrawBackgroundWeekends(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth);
  if bShowTasks.Down then
    Taskplan.aIDrawBackground(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,Ligthen(clBlue,1),Ligthen(clLime,0.9),Ligthen(clRed,0.9));
  if Assigned(FSnapshots) then
    begin
      for i := 0 to FSnapshots.IntervalCount-1 do
        if FSnapshots.Interval[i].Id = TInterval(Sender).Id then
          begin
            if TInterval(Sender).IntervalCount=0 then
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
procedure TfGanttView.bMoveBack1Click(Sender: TObject);
  procedure MoveForward(Sender : TInterval;aDate : TDateTime);
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
        TInterval(Sender).BeginUpdate;
        if Assigned(TInterval(Sender).Parent) then
          TInterval(Sender).StartDate := aDate;
        TInterval(Sender).EndUpdate;
        for i := 0 to IntervalCount-1 do
          MoveForward(Interval[i],aDate);
      end;
  end;
var
  i : Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    MoveForward(FGantt.Interval[i],FGantt.Interval[i].StartDate);
  bMoveFwd.Click;
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
        aBuffer := Buffer;
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
  procedure MoveForward(Sender : TInterval);
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
        TInterval(Sender).BeginUpdate;
        //Move Forward
        aDur := NetDuration;
        if ResourceTimePerDay=0 then ResourceTimePerDay:=1;
        if NetDuration<(NetTime*(1/ResourceTimePerDay)) then
          aDur:=(NetTime*(1/ResourceTimePerDay));
        if aDur<0.5 then aDur:=0.5;
        if NetDuration<aDur then NetDuration:=aDur;
        if TInterval(Sender).StartDate<TInterval(Sender).Earliest then
          TInterval(Sender).StartDate:=TInterval(Sender).Earliest;
        //Move out of Weekends
        if DayOfWeek(trunc(TInterval(Sender).StartDate))=7 then
          TInterval(Sender).StartDate := trunc(TInterval(Sender).StartDate)+1;
        if DayOfWeek(trunc(TInterval(Sender).StartDate))=1 then
          TInterval(Sender).StartDate := trunc(TInterval(Sender).StartDate)+2;
        //Add Weekends
        i := trunc(TInterval(Sender).StartDate);
        while i < TInterval(Sender).StartDate+aDur do
          begin
            if ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
              aDur := aDur+1;
            inc(i,1);
          end;
        //TODO: Urlaub
        TInterval(Sender).FinishDate := (TInterval(Sender).StartDate+aDur);
        aBuffer := Buffer;
        if aBuffer < (aDur*(seBuffer.Value/100)) then
          aBuffer := (aDur*(seBuffer.Value/100));
        //Add Weekends to Buffer
        i := trunc(TInterval(Sender).FinishDate);
        while i < TInterval(Sender).FinishDate+aBuffer do
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
          end;
        TInterval(Sender).EndUpdate;
        for i := 0 to IntervalCount-1 do
          MoveForward(Interval[i]);
      end;
  end;
var
  i : Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    MoveForward(FGantt.Interval[i]);
  FGantt.Invalidate;
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

procedure TfGanttView.acAddSubProjectsExecute(Sender: TObject);
var
  aProjects: TProjectList;
  aProject: TProject;
begin
  Application.ProcessMessages;
  if bCalculate2.Down then
    begin
      aProjects := TProjectList.Create(nil,Data);
      aProjects.SelectFromParent(Fproject.Id.AsVariant);
      aProjects.Open;
      while not aProjects.EOF do
        begin
          aProject := TProject.Create(nil,Data);
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
procedure TfGanttView.acMakePossibleExecute(Sender: TObject);
  function DoMove(aInterval : TInterval) : Boolean;
  var
    b: Integer;
    aDur: TDateTime;
  begin
    Result := False;
    if (aInterval.StartDate<Now()) and (not aInterval.Started) then
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
  bMoveFwd.Click;
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
begin
  aLink := TP.GetTaskFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
  if aLink <> '' then
    begin
      aInt := TP.GetTaskIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
      if Assigned(aInt) then
        begin
          if (not aInt.Changed) or (MessageDlg(strSaveTaskChanges,mtInformation,[mbYes,mbNo],0) = mrYes) then
            begin
              if aInt.Changed then
                ChangeTask(FTasks,aInt);
              aEdit :=TfTaskEdit.Create(nil);
              if aEdit.Execute(aLink) then
                begin
                  aTask := TTask.Create(nil,Data);
                  aTask.SelectFromLink(aLink);
                  aTask.Open;
                  FillInterval(TPInterval(aInt),aTask);
                  aTask.Free;
                  FGantt.Calendar.Invalidate;
                  aEdit.Free;
                end;
            end;
        end;
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
  if (bShowTasks.Down) and (not FResourcesRead) then
    begin
      if (MessageDlg(strCancelChanges,mtInformation,[mbYes,mbNo],0) = mrYes) then
        begin
          bRefreshClick(nil);
          FResourcesRead:=True;
        end;
    end;
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
procedure TfGanttView.FGanttCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ay: Integer;
begin
  lDate.Caption := DateToStr(FGantt.Calendar.VisibleStart+trunc((X/FGantt.Calendar.GetIntervalWidth)));

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
  aSelInterval:=0;
  FGantt := TgsGantt.Create(Self);
  FGantt.Parent := pgantt;
  FGantt.Align:=alClient;
  FGantt.Tree.AfterUpdateCommonSettings:=@FGanttTreeAfterUpdateCommonSettings;
  FRessources := TList.Create;
  FGantt.Calendar.OnMoveOverInterval:=@FGanttCalendarMoveOverInterval;
  FGantt.Calendar.OnShowHint:=@FGanttCalendarShowHint;
  FGantt.Calendar.OnMouseMove:=@FGanttCalendarMouseMove;
  FGantt.Calendar.OnDblClick:=@FGanttCalendarDblClick;
  FGantt.Calendar.OnClick:=@FGanttCalendarClick;
  FGantt.Tree.PopupMenu:=PopupMenu1;
  bDayViewClick(nil);
  FGantt.Calendar.ShowHint:=True;
  FSnapshots := nil;
end;
destructor TfGanttView.Destroy;
begin
  CleanIntervals;
  FRessources.Free;
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
  function FindInterval(aParent : TInterval;aId : Variant) : TInterval;
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
  function IntervalById(Id : Variant;Root : TInterval = nil) : TInterval;
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
    aUser: TUser;
    aProject: TProject;
    TaskPlan : TfTaskPlan = nil;
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
            //fLogWaitForm.ShowInfo(strCollectingresourceTimes);
            //TCollectThread.Create(FGantt.Calendar,TRessource(FRessources[i]),aTasks.FieldByName('USER').AsString,aInterval);
            TaskPlan.CollectResources(TRessource(FRessources[i]),aTasks.FieldByName('USER').AsString,nil,bShowTasks.Down);
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
    FGantt.AddInterval(aRoot);
    aRoot.Task:=TProjectList(aTasks.Parent).Text.AsString;
    cbSnapshot.Items.Clear;
    cbSnapshot.Items.Add(strNoSnapshot);
    while not aTasks.EOF do
      begin
        if (aTasks.FieldByName('ACTIVE').AsString<>'N') or AddInactive then
          if IntervalById(aTasks.Id.AsVariant)=nil then
            begin
              aInterval := AddTask(True,aRoot);
              aTasks.Dependencies.Open;
              if aTasks.Dependencies.Count>0 then
                begin
                  setlength(deps,length(deps)+1);
                  deps[length(deps)-1] := aTasks.GetBookmark;
                end;
              if not aTasks.Snapshots.DataSet.Active then aTasks.Snapshots.Open;
              aTasks.Snapshots.First;
              while not aTasks.Snapshots.EOF do
                begin
                  if cbSnapshot.Items.IndexOf(aTasks.Snapshots.FieldByName('NAME').AsString)=-1 then
                    cbSnapshot.Items.Add(aTasks.Snapshots.FieldByName('NAME').AsString);
                  aTasks.Snapshots.Next;
                end;
            end;
        aTasks.Next;
      end;
    fLogWaitForm.ShowInfo(strCollectingDependencies);
    for i := low(deps) to high(deps) do
      begin
        aTasks.GotoBookmark(deps[i]);
        aTask := TTask.Create(nil,Data);
        aTask.CreateTable;
        aTask.Select(aTasks.Id.AsVariant);
        aTask.Open;
        aTask.Dependencies.Open;
        aTask.Dependencies.First;
        while not aTask.Dependencies.DataSet.EOF do
          begin
            aDep := IntervalById(aTask.Dependencies.FieldByName('REF_ID_ID').AsVariant);
            if Assigned(aDep) then
              begin
                aInterval := IntervalById(aTasks.Id.AsVariant);
                aDep.AddConnection(aInterval,aTask.FieldByName('STARTDATE').IsNull and aTask.FieldByName('DUEDATE').IsNull,False);
              end;
            aTask.Dependencies.Next;
          end;
        aTask.Free;
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
procedure TfGanttView.DoSave;
  procedure RecoursiveChange(aParent : TInterval);
  var
    i: Integer;
  begin
    for i := 0 to aParent.IntervalCount-1 do
      begin
        if aParent.Interval[i].Changed then
          begin
            ChangeTask(FProject.Tasks,aParent.Interval[i]);
          end;
        RecoursiveChange(aParent.Interval[i]);
      end;
    if aParent.Changed then
      ChangeTask(FProject.Tasks,aParent);
  end;
var
  i: Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    RecoursiveChange(FGantt.Interval[i]);
end;
procedure TfGanttView.CleanIntervals;
var
  i: Integer;
begin
  FGantt.BeginUpdate;
  while FGantt.IntervalCount>0 do
    begin
      FGantt.Interval[0].Free;
      FGantt.DeleteInterval(0);
    end;
  for i := 0 to FRessources.Count-1 do TRessource(FRessources[i]).Free;
  FRessources.Clear;
  FGantt.EndUpdate;
end;
function TfGanttView.FindCriticalPath : TInterval;
var
  aLastInterval: TInterval;
  bTmp: TInterval;
  y: Integer;
  aIntervals : TList;

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
        DoPath(aLatestConn);
      end;
  end;

begin
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
procedure TfGanttView.FillInterval(aInterval : TInterval; aTasks: TTaskList);
var
  aUser: TUser;
  aStart: TDateTime;
  aDue: TDateTime;
  aChanged: Boolean;
  FUsage: Extended;
  FWorkTime: Extended;
  i: Integer;
begin
  aChanged := aTasks.CalcDates(aStart,aDue);
  aInterval.Task:=aTasks.FieldByName('SUMMARY').AsString;
  aInterval.Project:=aTasks.FieldByName('PROJECT').AsString;
  aInterval.Started:=not aTasks.FieldByName('STARTEDAT').IsNull;
  if aTasks.FieldByName('COMPLETED').AsString='Y' then
    begin
      aInterval.Color:=clGray;
      aInterval.Fixed:=True;
    end;
  aInterval.Id:=aTasks.Id.AsVariant;
  aInterval.DepDone := aTasks.FieldByName('DEPDONE').AsString <> 'N';
  if not aTasks.FieldByName('PLANTIME').IsNull then
    aInterval.NetTime:=aTasks.FieldByName('PLANTIME').AsFloat;
  aUser := TUser.Create(nil,Data);
  aUser.SelectByAccountno(aTasks.FieldByName('USER').AsString);
  aUser.Open;
  if aUser.Count>0 then
    begin
      aInterval.Resource := aUser.Text.AsString;
      FUsage := aUser.FieldByName('USEWORKTIME').AsInteger/100;
      if FUsage = 0 then FUsage := 1;
      FWorkTime:=aUser.WorkTime*FUsage;
      FUsage := FWorkTime/8;
      aInterval.ResourceTimePerDay:=FUsage;
    end
  else
    aInterval.ResourceTimePerDay := 1;
  aUser.Free;
  aInterval.FinishDate:=aDue;
  aInterval.StartDate:=aStart;
  if not aTasks.FieldByName('EARLIEST').IsNull then
    aInterval.Earliest := aTasks.FieldByName('EARLIEST').AsDateTime;
  if not (aTasks.FieldByName('BUFFERTIME').AsString = '') then
    aInterval.Buffer:=aTasks.FieldByName('BUFFERTIME').AsFloat;
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
  aTask := TTask.Create(nil,Data);
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
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGanttView,fGanttView);
      Self := fGanttView;
    end;
  FResourcesRead := False;
  FProject := aproject;
  FTasks := aProject.Tasks;
  Populate(FTasks,DoClean,AddInactive);
  SetRights;
  ModalResult := mrNone;
  if aLink <> '' then
    GotoTask(aLink);
  Caption := strGanttView+' - '+FTasks.Parent.FieldByName('NAME').AsString;
  Show;
  while Visible do
    begin
      Application.ProcessMessages;
      sleep(100);
    end;
  Result := ModalResult = mrOK;
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
  bMoveFwdClick(nil);
  bSave.Click;
end;

procedure TfGanttView.SetRights;
begin
  acMakePossible.Enabled := Data.Users.Rights.Right('PROJECTS') > RIGHT_READ;
  acAddSnapshot.Enabled := Data.Users.Rights.Right('PROJECTS') > RIGHT_READ;
  pCalc.Enabled := Data.Users.Rights.Right('PROJECTS') > RIGHT_READ;
end;

end.

