{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.05.2013
*******************************************************************************}
unit uGanttView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, gsGanttCalendar, uTask,Math,uProjects;

type

  { TfGanttView }

  TfGanttView = class(TForm)
    acCenterTask: TAction;
    acOpen: TAction;
    acMakePossible: TAction;
    acAddSubProjects: TAction;
    acAddSnapshot: TAction;
    ActionList1: TActionList;
    bCalculate1: TSpeedButton;
    bCalculate2: TSpeedButton;
    bDayView: TSpeedButton;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bMonthView: TSpeedButton;
    bShowTasks: TSpeedButton;
    bCalculate: TSpeedButton;
    bRefresh: TSpeedButton;
    bShowTasks1: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    cbSnapshot: TComboBox;
    Label8: TLabel;
    lDate: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem1: TMenuItem;
    Panel10: TPanel;
    Panel4: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pgantt: TPanel;
    Panel7: TPanel;
    PopupMenu1: TPopupMenu;
    tbTop: TPanel;
    RecalcTimer: TTimer;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acAddSnapshotExecute(Sender: TObject);
    procedure acAddSubProjectsExecute(Sender: TObject);
    procedure acCenterTaskExecute(Sender: TObject);
    procedure acMakePossibleExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure aIntervalChanged(Sender: TObject);
    procedure aIntervalDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double);
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
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
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
    procedure Populate(aTasks: TTaskList; DoClean: Boolean=True);
    procedure DoSave;
    procedure CleanIntervals;
    procedure FindCriticalPath;
    procedure FillInterval(aInterval : TInterval;aTasks : TTaskList);
    procedure GotoTask(aLink : string);
    function Execute(aProject : TProject;aLink : string = '') : Boolean;
  end;

var
  fGanttView: TfGanttView;

implementation
uses uData,LCLIntf,uBaseDbClasses,uTaskEdit,variants,LCLProc,uTaskPlan,
  uIntfStrConsts,uColors,uBaseDBInterface,Grids;
{$R *.lfm}
resourcestring
  strSaveChanges                          = 'Um die Aufgabe zu bearbeiten müssen alle Änderungen gespeichert werden, Sollen alle Änderungen gespeichert werden ?';
  strSnapshot                             = 'Snapshot';
  strNoSnapshot                           = '<keiner>';
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
  FGantt.Tree.Options:=FGantt.Tree.Options+[goCellHints];
  FGantt.Tree.Options:=FGantt.Tree.Options-[goHorzLine];
  FGantt.Tree.AlternateColor:=$00FFE6E6;
end;

procedure TfGanttView.FGanttTreeResize(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=FGantt.Tree.Width-FGantt.Tree.ColWidths[3]-FGantt.Tree.ColWidths[4]-FGantt.Tree.ColWidths[5];
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

procedure TfGanttView.ToolButton1Click(Sender: TObject);
begin
  ModalResult := mrOK;
  Close;
end;

procedure TfGanttView.ToolButton2Click(Sender: TObject);
begin
  ModalResult := mrAbort;
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
  function DoMoveBack(aInterval,aConn : TInterval;aTime : TDateTime) : TDateTime;
  var
    b: Integer;
    aTmp: TDateTime;
    c: Integer;
    aDur: TDateTime;
    bInt: TInterval;
    IsMoved: Boolean = False;
    aParent: TInterval;
  begin
    Result := 0;
    if not assigned(aInterval) then exit;
    for c := 0 to aInterval.ConnectionCount-1 do
      if aInterval.Connection[c] = aConn then
        begin
          bInt := aInterval;
          aTmp := (bInt.FinishDate+bInt.Buffer)-aConn.StartDate;
          if (aTmp > Result) or (Result=0) then Result := aTmp;
          if aTmp>0 then
            begin
              bInt.BeginUpdate;
              aDur := bInt.Duration;
              bInt.FinishDate:=aConn.StartDate-bInt.Buffer;
              bInt.StartDate:=bInt.FinishDate-aDur;
              IsMoved := True;
              bInt.EndUpdate;
            end;
        end;
    for b := 0 to aInterval.IntervalCount-1 do
      Result := DoMoveBack(aInterval.Interval[b],aConn,aTime);
  end;

begin
  with TInterval(Sender) do
    begin
      if bCalculate.Down then
        begin
          debugln('IntervalChanged('+TInterval(Sender).Task+')');
          TInterval(Sender).BeginUpdate;
          //Move Forward
          aDur := Duration;
          if TInterval(Sender).StartDate<TInterval(Sender).Earliest then
            TInterval(Sender).StartDate:=TInterval(Sender).Earliest;
          if FinishDate<(StartDate+aDur) then
            FinishDate := (StartDate+aDur);
          IntervalDone:=StartDate;
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
          //Move back
          for i := 0 to TInterval(Sender).Gantt.IntervalCount-1 do
            DoMoveBack(TInterval(Sender).Gantt.Interval[i],TInterval(Sender),TInterval(Sender).FinishDate);
          TInterval(Sender).Endupdate;
          RecalcTimer.Enabled := True;
        end;
      for i := 0 to FRessources.Count-1 do
        for a := 0 to TRessource(FRessources[i]).IntervalCount-1 do
          if TRessource(FRessources[i]).Interval[a].Id = TInterval(Sender).Id then
            begin
              TRessource(FRessources[i]).BeginUpdate;
              TRessource(FRessources[i]).Interval[a].StartDate:=TInterval(Sender).StartDate;
              TRessource(FRessources[i]).Interval[a].FinishDate:=TInterval(Sender).FinishDate;
              TRessource(FRessources[i]).EndUpdate;
            end;
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
    Taskplan.aIDrawBackground(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,Ligthen(clBlue,5),Ligthen(clLime,0.8),Ligthen(clRed,0.8));
  if Assigned(FSnapshots) then
    begin
      for i := 0 to FSnapshots.IntervalCount-1 do
        if FSnapshots.Interval[i].Id = TInterval(Sender).Id then
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
            break;
          end;
    end;
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
      aTask := TTask.Create(nil,Data);
      aTask.Select(TInterval(FGantt.Tree.Objects[0,FGantt.Tree.Row]).Id);
      aTask.Open;
      if aTask.Count>0 then
        begin
          aDur := round(FGantt.Calendar.Width/FGantt.Calendar.GetIntervalWidth);
          aTask.CalcDates(aStart,aEnd);
          if (aEnd-aStart) > aDur then
            FGantt.StartDate:=aStart
          else
            FGantt.StartDate:=aStart-((aDur-((aEnd-aStart)/2)) / 2);
          aTask.Free;
        end;
    end;
end;

procedure TfGanttView.acAddSubProjectsExecute(Sender: TObject);
var
  aProjects: TProjectList;
  aProject: TProject;
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
          if not FTasks.DataSet.Locate('COMOPLETED','N',[]) then
            aProject.Tasks.SelectActive;
          aProject.Tasks.Open;
          Populate(aProject.Tasks,False);
        end;
      aProject.Free;
      aProjects.Next;
    end;
  aProjects.Free;
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
      aEdit :=TfTaskEdit.Create(nil);
      if (not MustChange(TP.GetTaskIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval))) or (MessageDlg(strSaveChanges,mtInformation,[mbYes,mbNo],0) = mrYes) then
        begin
          DoSave;
          if aEdit.Execute(aLink) then
            begin
              aInt := TP.GetTaskIntervalFromCoordinates(FGantt,aClickPoint.X,aClickPoint.Y,aSelInterval);
              if Assigned(aInt) then
                begin
                  aTask := TTask.Create(nil,Data);
                  aTask.SelectFromLink(aLink);
                  aTask.Open;
                  FillInterval(TPInterval(aInt),aTask);
                  aTask.Free;
                  FGantt.Calendar.Invalidate;
                end;
            end;
        end;
      aEdit.Free;
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
end;
procedure TfGanttView.bShowTasksClick(Sender: TObject);
begin
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
  if cbSnapshot.Items[cbSnapshot.ItemIndex] = strNoSnapshot then exit;
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
procedure TfGanttView.Populate(aTasks: TTaskList;DoClean : Boolean = True);
var
  aNewInterval: TInterval;
  aTask: TTask;
  aInterval: TInterval;
  aDep: TInterval;
  i: Integer;
  aRoot: TInterval;
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
    TaskPlan : TfTaskPlan;
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
            TaskPlan.CollectResources(TRessource(FRessources[i]),aTasks.FieldByName('USER').AsString);
          end;
      end;
  end;
begin
  FGantt.BeginUpdate;
  try
    if DoClean then
      begin
        CleanIntervals;
      end;
    aTasks.First;
    aRoot := TInterval.Create(FGantt);
    FGantt.AddInterval(aRoot);
    aRoot.Task:=TProjectList(aTasks.Parent).Text.AsString;
    cbSnapshot.Items.Clear;
    cbSnapshot.Items.Add(strNoSnapshot);
    while not aTasks.EOF do
      begin
        if aTasks.FieldByName('ACTIVE').AsString<>'N' then
          if IntervalById(aTasks.Id.AsVariant)=nil then
            begin
              aInterval := AddTask(True,aRoot);
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
    aTasks.First;
    while not aTasks.EOF do
      begin
        if aTasks.FieldByName('ACTIVE').AsString<>'N' then
          begin
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
                    aDep.AddConnection(aInterval,aTask.FieldByName('STARTDATE').IsNull and aTask.FieldByName('DUEDATE').IsNull);
                  end;
                aTask.Dependencies.Next;
              end;
            aTask.Free;
          end;
        aTasks.Next;
      end;
    if aRoot.IntervalCount>0 then
      aRoot.Visible:=True;
  finally
    FGantt.EndUpdate;
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

procedure TfGanttView.FindCriticalPath;
var
  y: Integer;
  aLastDate : TDateTime;
  function DoPath(aInterval : TInterval) : Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to aInterval.ConnectionCount-1 do
      Result := Result
      or (DoPath(aInterval.Connection[i]) and (aInterval.Connection[i].StartDate<=aInterval.FinishDate));
    if (aInterval.ConnectionCount = 0) and (aInterval.IntervalCount = 0) and (aInterval.FinishDate>=aLastDate) then Result := True;
    if (aInterval.ConnectionCount = 0) and (aInterval.IntervalCount > 0) then
      for i := 0 to aInterval.IntervalCount-1 do
        Result := Result or DoPath(aInterval.Interval[i]);
    aInterval.InCriticalPath := Result;
  end;
begin
  aLastDate := 0;
  for y := 0 to fGantt.IntervalCount-1 do
    if FGantt.Interval[y].FinishDate>aLastDate then
      aLastDate := FGantt.Interval[y].FinishDate;
  for y := 0 to fGantt.IntervalCount-1 do
    if not Assigned(FGantt.Interval[y].Parent) then
      DoPath(FGantt.Interval[y]);
  FGantt.Calendar.Invalidate;
end;

procedure TfGanttView.FillInterval(aInterval : TInterval; aTasks: TTaskList);
var
  aUser: TUser;
  aStart: TDateTime;
  aDue: TDateTime;
  aChanged: Boolean;
begin
  aChanged := aTasks.CalcDates(aStart,aDue);
  aInterval.Task:=aTasks.FieldByName('SUMMARY').AsString;
  aInterval.Project:=aTasks.FieldByName('PROJECT').AsString;
  aInterval.Started:=not aTasks.FieldByName('STARTEDAT').IsNull;
  if aTasks.FieldByName('COMPLETED').AsString='Y' then
    aInterval.Color:=clGray;
  aInterval.Id:=aTasks.Id.AsVariant;
  aInterval.DepDone := aTasks.FieldByName('DEPDONE').AsString <> 'N';
  if not aTasks.FieldByName('PLANTIME').IsNull then
    aInterval.NetTime:=aTasks.FieldByName('PLANTIME').AsFloat;
  aUser := TUser.Create(nil,Data);
  aUser.SelectByAccountno(aTasks.FieldByName('USER').AsString);
  aUser.Open;
  if aUser.Count>0 then
    aInterval.Resource := aUser.Text.AsString;
  aUser.Free;
  aInterval.FinishDate:=aDue;
  aInterval.StartDate:=aStart;
  if not aTasks.FieldByName('EARLIEST').IsNull then
    aInterval.Earliest := aTasks.FieldByName('EARLIEST').AsDateTime;
  if not (aTasks.FieldByName('BUFFERTIME').AsString = '') then
    aInterval.Buffer:=aTasks.FieldByName('BUFFERTIME').AsFloat;
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
function TfGanttView.Execute(aProject: TProject; aLink: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGanttView,fGanttView);
      Self := fGanttView;
    end;
  FProject := aproject;
  FTasks := aProject.Tasks;
  Populate(FTasks);
  ModalResult := mrNone;
  if aLink <> '' then
    GotoTask(aLink);
  Caption := strGanttView+' - '+FTasks.Parent.FieldByName('NAME').AsString;
  Show;
  while Visible do
    Application.ProcessMessages;
  Result := ModalResult = mrOK;
  if Result then
    begin
      DoSave;
    end;
  CleanIntervals;
end;

end.

