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
Created 25.06.2013
*******************************************************************************}
unit uroughpklanningframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, TAGraph, LR_DBSet,
  LR_Class, LR_View, Forms, Controls, ComCtrls, Buttons, ActnList, Menus,
  ExtCtrls, DbCtrls, StdCtrls, uExtControls, DBZVDateTimePicker, db,
  uPrometFrames, uPrometFramesInplace, uBaseDBClasses, Dialogs, Spin, EditBtn,
  DBGrids, variants,uStatistic,SynCompletion,md5,LCLType,
  TASeries, TACustomSeries,fpsqlparser,Clipbrd,gsGanttCalendar,uProjects,Grids,
  uIntfStrConsts,Graphics,Math,uBaseDatasetInterfaces;
type
  TFillingThread = class;
  TIntDepartment = class
  public
    Name : string;
    Shortname : string;
    Accountno : string;
    Time : Real;
    FullTime : real;
    constructor Create;
  end;
  TProjectInterval = class(TInterval)
  private
    FList : TList;
    function getDepartment(aDepartment : Integer): TIntDepartment;
    function GetDeptCount: Integer;
  public
    property Departments[aDepartment : Integer] : TIntDepartment read getDepartment;
    procedure AddTime(aAccountNo, aDept, aShortName: string; aTime: Real);
    property DepartmentCount : Integer read GetDeptCount;
    constructor Create(AGantt: TgsGantt);override;
    destructor Destroy;override;
  end;
  TfRoughPlanningFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acClose: TAction;
    acDelete: TAction;
    acOpen: TAction;
    acSave: TAction;
    acExport: TAction;
    acImport: TAction;
    acPrint: TAction;
    acRights: TAction;
    acRestart: TAction;
    acGotoParent: TAction;
    acExecute: TAction;
    acShowInProjectGantt: TAction;
    acShowProject: TAction;
    ActionList1: TActionList;
    acUse: TAction;
    bDayView: TSpeedButton;
    bDelegated2: TSpeedButton;
    Bevel10: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    bMonthView: TSpeedButton;
    bRefresh: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lDate: TLabel;
    MenuItem1: TMenuItem;
    miCopy: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    N1: TMenuItem;
    N1001: TMenuItem;
    N101: TMenuItem;
    N1501: TMenuItem;
    N2: TMenuItem;
    N2001: TMenuItem;
    N251: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N501: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N751: TMenuItem;
    miDelete: TMenuItem;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pmTree: TPopupMenu;
    tbTop: TPanel;
    Timer1: TTimer;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acCancelExecute(Sender: TObject);
    procedure acUseExecute(Sender: TObject);
    procedure bDayViewClick(Sender: TObject);
    procedure bDelegated2Click(Sender: TObject);
    procedure bMonthViewClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bTodayClick(Sender: TObject);
    procedure bWeekViewClick(Sender: TObject);
    procedure FRoughCalendarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FRoughCalendarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FRoughTreeAfterUpdateCommonSettings(Sender: TObject);
    procedure aIntDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aUnfinishedList : TList = nil);
    procedure RefreshTimes(Data: PtrInt);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FEditable : Boolean;
    FRough: TgsGantt;
    FUpdateCount : Integer;
  protected
    aThread: TFillingThread;
    fRefreshList : TList;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetLanguage;override;
    procedure ShowFrame; override;
    procedure StartFilling;
    procedure aSubIntChanged(Sender: TObject);
    function GetAvalibeTimeInRange(asUser : string; aStart,
  aEnd: TDateTime): Real;
    function GetProjectTimes(aAccountno : string;aStart,aEnd : TDateTime) : real;
    procedure CollectActualProject(aProjects: TProjectList;aInt : TInterval;aConn : TComponent;aUsers : TUser);
  end;
  TFillingThread = class(TThread)
  private
    FFrame: TfRoughPlanningFrame;
    aInt: TInterval;
    procedure AddInterval;
    procedure RoughVisible;
    procedure StartFilling;
  public
    procedure Execute;override;
    constructor Create(aFrame : TfRoughPlanningFrame);
  end;

procedure AddToMainTree(aAction : TAction;Node : TTreeNode);
var
  MainNode : TTreeNode;
implementation
uses uData,uBaseDBInterface,uBaseERPDBClasses,uCalendar,uMainTreeFrame;
resourcestring
  strRoughPlanning                                      = 'Grobplanung';
{$R *.lfm}
procedure AddToMainTree(aAction: TAction; Node: TTreeNode);
var
  Node1: TTreeNode;
begin
  if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
    end;
end;
constructor TIntDepartment.Create;
begin
  FullTime:=-1;
end;
function TProjectInterval.getDepartment(aDepartment : Integer): TIntDepartment;
begin
  Result := TIntDepartment(FList[aDepartment]);
end;
function TProjectInterval.GetDeptCount: Integer;
begin
  result := FList.Count;
end;
procedure TProjectInterval.AddTime(aAccountNo,aDept,aShortName: string; aTime: Real);
var
  Found: Boolean = False;
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
    if TIntDepartment(FList[i]).Name=aDept then
      begin
        TIntDepartment(FList[i]).Time:=TIntDepartment(FList[i]).Time+aTime;
        Found := True;
      end;
  if not Found then
    begin
      FList.Add(TIntDepartment.Create);
      TIntDepartment(FList[Flist.Count-1]).Name:=aDept;
      TIntDepartment(FList[Flist.Count-1]).ShortName:=aShortName;
      TIntDepartment(FList[Flist.Count-1]).Accountno:=aAccountNo;
      TIntDepartment(FList[Flist.Count-1]).Time:=aTime;
      TIntDepartment(FList[Flist.Count-1]).FullTime:=-1;
    end;
end;
constructor TProjectInterval.Create(AGantt: TgsGantt);
begin
  FList := TList.Create;
  inherited Create(AGantt);
end;
destructor TProjectInterval.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
procedure TfRoughPlanningFrame.aSubIntChanged(Sender: TObject);
var
  i: Integer;
  aDur: TDateTime;
  a: Integer;
  b: PtrInt;
begin
  for i := 0 to TInterval(Sender).ConnectionCount-1 do
    if TInterval(Sender).FinishDate>TInterval(Sender).Connection[i].StartDate then
      begin
        aDur := TInterval(Sender).Connection[i].Duration;
        TInterval(Sender).Connection[i].StartDate:=TInterval(Sender).FinishDate;
        TInterval(Sender).Connection[i].Duration:=aDur;
      end;
  if Sender is TProjectInterval then
    begin
      for a := 0 to TProjectInterval(Sender).DepartmentCount-1 do
        begin
          fRefreshList.Add(TProjectInterval(Sender).Departments[a]);
          Application.QueueAsyncCall(@RefreshTimes,b);
        end;
      FRough.Calendar.Invalidate;
    end;
end;
procedure TFillingThread.AddInterval;
begin
  FFrame.FRough.AddInterval(aInt);
end;
procedure TFillingThread.RoughVisible;
begin
  FFrame.FRough.Visible:=True;
  FFrame.FRough.Invalidate;
  FFrame.bRefresh.Enabled := True;
  Screen.Cursor:=crDefault;
  Application.ProcessMessages;
end;

procedure TFillingThread.StartFilling;
begin
  Screen.Cursor:=crHourGlass;
  FFrame.bRefresh.Enabled := False;
end;

procedure TFillingThread.Execute;
var
  aConn: TComponent;
  aProjects: TProjectList;
  aState: TStates;
  aUsers: TUser;
  b: Integer;
  i: Integer;
  a: Integer;
  aSubInt: TProjectInterval;
  aDepartment: TIntDepartment;
begin
  Synchronize(@StartFilling);
  aConn := Data.GetNewConnection;
  aProjects :=  TProjectList.CreateEx(nil,Data,aConn);
  with aProjects.DataSet as IBaseDbFilter do
    Data.SetFilter(aProjects,Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'='+Data.QuoteValue('Y')),0,'GPRIORITY','ASC');
  aState := TStates.CreateEx(nil,Data,aConn);
  aState.Open;
  aUsers := TUser.CreateEx(nil,Data,aConn);
  aUsers.Open;
  while (not aProjects.EOF) and (not Terminated) do
    begin
      if aState.DataSet.Locate('STATUS;TYPE',VarArrayOf([trim(aProjects.FieldByName('STATUS').AsString),'P']),[]) then
        if aState.DataSet.FieldByName('ACTIVE').AsString<>'N' then
          begin
            aInt := TInterval.Create(FFrame.FRough);
            aInt.Id:=aProjects.Id.AsVariant;
            FFrame.CollectActualProject(aProjects,aInt,aConn,aUsers);
            Synchronize(@AddInterval);
          end;
      aProjects.Next;
    end;
  aProjects.Free;
  aState.Free;
  aUsers.Free;
  aConn.Free;
  for b := 0 to FFrame.FRough.IntervalCount-1 do
    begin
      aInt := FFrame.FRough.Interval[b];
      for i := 0 to aInt.IntervalCount-1 do
        begin
          aSubInt := TProjectInterval(aInt.Interval[i]);
          if aSubInt.DepartmentCount>0 then
            begin
              for a := 0 to aSubInt.DepartmentCount-1 do
                begin
                  aDepartment := aSubInt.Departments[a];
                  if aDepartment.FullTime=-1 then
                    aDepartment.FullTime := FFRame.GetAvalibeTimeInRange(aDepartment.Accountno,aInt.Interval[i].StartDate,aInt.Interval[i].FinishDate);
                end;
            end;
        end;
    end;
  Synchronize(@RoughVisible);
  FFrame.aThread:=nil;
end;
constructor TFillingThread.Create(aFrame: TfRoughPlanningFrame);
begin
  FFrame := aFrame;
  Priority:=tpHighest;
  //Execute;
  //Free;
  inherited Create(False)
end;
procedure TfRoughPlanningFrame.aIntDrawBackground(Sender: TObject;
  aCanvas: TCanvas; aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;
  aUnfinishedList: TList);
var
  Style : TTextStyle;
  i: Integer;
  aDRect: TRect;
  aAddTop : Integer = 0;
  aInt: TInterval;
  aDeptWidth: real;
  a: Integer;
  aDerect: TRect;
  aRRect : Trect;
  aDrawTime: Real;
  aTop: Int64;
  aDepartment: TIntDepartment;
  startinc: Integer;
  aSubInt: TProjectInterval;
  aDay: Extended;
begin
  if FUpdateCount>0 then exit;
  FillChar(Style, SizeOf(TTextStyle), 0);
  Style.SystemFont := True;
  Style.Alignment:=taCenter;
  Style.Layout:=tlCenter;
  Style.Clipping:=True;
  aInt := TInterval(Sender);

  for i := 0 to round(aEnd-aStart) do
    begin
      aDay := aStart+i;
      aCanvas.Brush.Color:=clSkyBlue;
      if (trunc(aDay) = trunc(Now())) then
        aCanvas.FillRect(round(i*aDayWidth),aRect.Top,round((i*aDayWidth)+aDayWidth),aRect.Bottom);
    end;

  if aInt.IntervalCount=0 then
    begin
      aCanvas.Font.Color:=clGrayText;
      aCanvas.TextRect(aRect,aRect.Left,aRect.Top,'keine Plandaten vorhanden',Style);
      aCanvas.Font.Color:=clNormalText;
    end
  else if Assigned(aThread) then
    begin
      for i := 0 to aInt.IntervalCount-1 do
        begin
          aDRect:=Rect(round((aInt.Interval[i].StartDate-aStart)*aDayWidth)-1,(aRect.Top)+aAddTop,round((aInt.Interval[i].FinishDate-aStart)*aDayWidth)+1,(aRect.Bottom)+aAddTop);
          aInt.Interval[i].DrawRect := aDrect;
          aCanvas.Pen.Style:=psSolid;
          aCanvas.Pen.Color:=clRed;
          aCanvas.Brush.Color:=clWindow;
          aCanvas.Rectangle(aDRect);
        end;
    end
  else
    begin
      for i := 0 to aInt.IntervalCount-1 do
        begin
          aDRect:=Rect(round((aInt.Interval[i].StartDate-aStart)*aDayWidth)-1,(aRect.Top)+aAddTop,round((aInt.Interval[i].FinishDate-aStart)*aDayWidth)+1,(aRect.Bottom)+aAddTop);
          aCanvas.Brush.Style:=bsClear;
          aInt.Interval[i].DrawRect := aDrect;
          if i = aInt.IntervalCount-1 then
            aCanvas.TextRect(Rect(aDRect.Right+10,aDRect.Top,aDRect.Right+300,aDRect.Bottom),aDRect.Right+10,aDRect.Top,aInt.Task,Style);
          aCanvas.Pen.Style:=psSolid;
          aCanvas.Pen.Width:=2;
          aCanvas.Pen.Color:=clRed;
          aCanvas.Brush.Color:=clWindow;
          aCanvas.Rectangle(aDRect);
          aCanvas.Pen.Width:=1;
          aSubInt := TProjectInterval(aInt.Interval[i]);
          if aSubInt.DepartmentCount>0 then
            begin
              aDeptWidth := ((adRect.Right-aDRect.Left)-2) / aSubInt.DepartmentCount;
              for a := 0 to aSubInt.DepartmentCount-1 do
                begin
                  aDepartment := aSubInt.Departments[a];
                  if aDepartment.FullTime=-1 then
                    aDepartment.FullTime := GetAvalibeTimeInRange(aDepartment.Accountno,aInt.Interval[i].StartDate,aInt.Interval[i].FinishDate);
                  aDrawTime := aDepartment.FullTime;
                  aDrawTime -= GetProjectTimes(aDepartment.Accountno,aSubInt.StartDate,aSubInt.FinishDate);
                  aDerect := Rect(round(((aInt.Interval[i].StartDate-aStart)*aDayWidth)+(aDeptWidth*a)),(aRect.Top)+aAddTop+1,round(((aInt.Interval[i].StartDate-aStart)*aDayWidth)+(aDeptWidth*(a+1))),(aRect.Bottom)+aAddTop-1);
                  aCanvas.Pen.Style:=psSolid;
                  acanvas.Pen.Color := clSilver;
                  aCanvas.Brush.Color:=clWindow;
                  aCanvas.Rectangle(aDeRect);
                  aCanvas.Pen.Style:=psClear;
                  aCanvas.Brush.Color:=clLime;
                  if aDrawTime<0 then
                    begin
                      aDrawTime := 0;
                      aCanvas.Brush.Color:=clRed;
                    end;
                  aRRect := aDerect;
                  if aDepartment.FullTime>0 then
                    begin
                      aTop := round(((aDerect.Bottom-aDerect.Top)*(aDrawTime/aDepartment.FullTime)));
                      aTop := aDerect.Top+aTop;
                    end
                  else aTop := aDerect.Bottom;
                  aRrect.Top:=aTop+1;
                  aRrect.Left:=aDerect.Left+1;
                  aRRect.Right:=min(aDerect.Left+11,aDerect.Right);

                  aCanvas.Rectangle(aRRect);
                  aRRect := aDerect;
                  aRRect.Left:=min(aDerect.Left+11,aDerect.Right);
                  aCanvas.Font.Size:=8;
                  aCanvas.TextRect(aRRect,aRRect.Left,aRRect.Top,aDepartment.ShortName,Style);
                  aCanvas.Font.Size:=0;
                end;
            end;
        end;
    end;
end;
procedure TfRoughPlanningFrame.RefreshTimes(Data: PtrInt);
begin
  Timer1.Enabled := True;
end;
procedure TfRoughPlanningFrame.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to fRefreshList.Count-1 do
    TIntDepartment(fRefreshList[i]).FullTime:=-1;
  fRefreshList.Clear;
  FRough.Calendar.Invalidate;
end;
procedure TfRoughPlanningFrame.bDayViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsWeekNum;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfRoughPlanningFrame.bDelegated2Click(Sender: TObject);
begin
  if Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then
    Data.GotoLink('PROJECTS@'+IntToStr(TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id));
end;
procedure TfRoughPlanningFrame.acUseExecute(Sender: TObject);
begin

end;
procedure TfRoughPlanningFrame.acCancelExecute(Sender: TObject);
begin
  StartFilling;
end;
procedure TfRoughPlanningFrame.bMonthViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsQuarter;
  FRough.MinorScale:=tsMonth;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfRoughPlanningFrame.bRefreshClick(Sender: TObject);
var
  CurrInterval: TInterval;
  aProjects: TProjectList;
  aUsers: TUser;
begin
  Screen.Cursor:=crHourGlass;
  CurrInterval := TInterval(FRough.Tree.Objects[0, FRough.Tree.Row]);
  if Assigned(CurrInterval) then
    begin
      aProjects := TProjectList.Create(nil);
      aProjects.Select(CurrInterval.Id);
      aProjects.Open;
      aUsers := TUser.Create(nil);
      aUsers.Open;
      while CurrInterval.IntervalCount > 0 do
        CurrInterval.DeleteInterval(0);
      CollectActualProject(aProjects,CurrInterval,nil,aUsers);
      aUsers.Free;
      aProjects.Free;
    end;
  FRough.Invalidate;
  Screen.Cursor:=crDefault;
end;
procedure TfRoughPlanningFrame.bTodayClick(Sender: TObject);
begin
  FRough.StartDate:=Now();
end;
procedure TfRoughPlanningFrame.bWeekViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsMonth;
  FRough.MinorScale:=tsWeekNumPlain;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfRoughPlanningFrame.FRoughCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lDate.Caption := DateToStr(FRough.Calendar.VisibleStart+trunc((X/FRough.Calendar.GetIntervalWidth)));
end;
procedure TfRoughPlanningFrame.FRoughCalendarShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  List: TList;
  ay: Integer;
  aInt: TProjectInterval;
  i: Integer;
  aPInt: TInterval;
  aDrect: TRect;
  aDePrect: TRect;
  aDepartment: TIntDepartment;
  aDeptWidth: Extended;
  a: Integer;
  function IsInRect(X, Y: Integer; R: TRect): Boolean;
  begin
    Result := (X >= R.Left) and (X <= R.Right); //and (Y >= R.Top) and (Y <= R.Bottom);
  end;
begin
  if HintInfo^.HintStr='' then
    begin
      List := TList.Create;
      FRough.MakeIntervalList(List);
      ay := HintInfo^.CursorPos.Y-FRough.Calendar.StartDrawIntervals;
      ay := ay div max(FRough.Calendar.PixelsPerLine,1);
      ay := ay+(FRough.Tree.TopRow-1);
      HintInfo^.HintStr := '';
      HintInfo^.HideTimeout:=30000;
      if (ay<List.Count) and (ay>-1) then
        if TInterval(List[ay]).Style=isNone then
          begin
            aPInt := TInterval(List[ay]);
            HintInfo^.HintStr:=aPInt.Task+' '+DateToStr(aPInt.StartDate)+' - '+DateToStr(aPInt.FinishDate);
            for i := 0 to aPInt.IntervalCount-1 do
              begin
                if IsInRect(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y,TProjectInterval(aPInt.Interval[i]).DrawRect) then
                  begin
                    aInt := TProjectInterval(aPInt.Interval[i]);
                    HintInfo^.HintStr:=HintInfo^.HintStr+LineEnding+aInt.Task+' '+DateToStr(aInt.StartDate)+' - '+DateToStr(aInt.FinishDate);
                    aDrect := aInt.DrawRect;
                    if aInt.DepartmentCount>0 then
                      begin
                        aDeptWidth := ((adRect.Right-aDRect.Left)-2) / aInt.DepartmentCount;
                        for a := 0 to aInt.DepartmentCount-1 do
                          begin
                            aDepartment := aInt.Departments[a];
                            if IsInRect(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y,Rect(round(aDrect.Left+(aDeptWidth*a)),0,round(aDrect.Left+(aDeptWidth*(a+1))),0)) then
                              begin
                                HintInfo^.HintStr:=HintInfo^.HintStr+LineEnding+aDepartment.Name;
                                HintInfo^.HintStr:=HintInfo^.HintStr+LineEnding+'verfügbare Kapazität:'+FormatFloat('0.0',aDepartment.FullTime);
                                HintInfo^.HintStr:=HintInfo^.HintStr+LineEnding+'benötigte Kapazität :'+FormatFloat('0.0',aDepartment.Time);
                                HintInfo^.HintStr:=HintInfo^.HintStr+LineEnding+'andere Projekte :'+FormatFloat('0.0',GetProjectTimes(aDepartment.Accountno,aInt.StartDate,aInt.FinishDate)-aDepartment.Time);
                              end;
                          end;
                      end;
                  end;
              end;
          end;

    end;
end;
procedure TfRoughPlanningFrame.FRoughTreeAfterUpdateCommonSettings(
  Sender: TObject);
begin
  FRough.Tree.ColWidths[0]:=0;
  FRough.Tree.ColWidths[1]:=0;
  FRough.Tree.ColWidths[2]:=300;
  FRough.Tree.Cells[2,0]:=strProject;
  FRough.Tree.ColWidths[3]:=0;
  FRough.Tree.ColWidths[4]:=0;
  FRough.Tree.ColWidths[5]:=0;
  FRough.Tree.ColWidths[6]:=0;
  FRough.Tree.ColWidths[7]:=0;
  FRough.Tree.Width:=310;
  FRough.Tree.PopupMenu := pmTree;
end;
function TfRoughPlanningFrame.SetRights: Boolean;
begin

end;

constructor TfRoughPlanningFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateCount:=0;
  TabCaption:=strRoughPlanning;
  frefreshList := TList.Create;
  FRough := TgsGantt.Create(Self);
  FRough.Parent := Panel1;
  FRough.Align:=alClient;
  FRough.Tree.AfterUpdateCommonSettings:=@FRoughTreeAfterUpdateCommonSettings;
  FRough.Tree.Options:=FRough.Tree.Options-[goEditing];
  FRough.Calendar.OnShowHint:=@FRoughCalendarShowHint;
  FRough.Calendar.ShowHint:=True;
  FRough.Calendar.OnMouseMove:=@FRoughCalendarMouseMove;
  //FRough.Visible:=False;
end;

destructor TfRoughPlanningFrame.Destroy;
begin
  if Assigned(aThread) then
    begin
      aThread.Terminate;
      aThread.WaitFor;
    end;
  FRough.Free;
  fRefreshList.Free;
  inherited Destroy;
end;

procedure TfRoughPlanningFrame.SetLanguage;
begin
end;

procedure TfRoughPlanningFrame.ShowFrame;
begin
  inherited ShowFrame;
end;

procedure TfRoughPlanningFrame.StartFilling;
begin
  aThread := TFillingThread.Create(Self);
end;

function TfRoughPlanningFrame.GetAvalibeTimeInRange(asUser : string; aStart,
  aEnd: TDateTime): Real;
var
  aUser: TUser;
  aUsers: TUser;
  aCalendar: TCalendar;
  aDayT: Real;
  bStart: TDateTime;
  bEnd: TDateTime;
  aDay: Integer;
  aWeekEnds: Integer;
begin
  Result := 0;
  aUser := TUser.Create(nil);
  aUser.SelectByAccountno(asUser);
  aUser.Open;
  if aUser.FieldByName('TYPE').AsString='G' then
    begin
      aUsers := TUser.Create(nil);
      with aUsers.DataSet as IBaseDbFilter do
        Filter := Data.QuoteField('PARENT')+'='+Data.QuoteValue(aUser.Id.AsString);
      aUsers.Open;
      while not aUsers.EOF do
        begin
          Result := Result+GetAvalibeTimeInRange(aUsers.FieldByName('ACCOUNTNO').AsString,aStart,aEnd);
          aUsers.Next;
        end;
      aUsers.Free;
    end
  else
    begin
      //verfügbare Zeit
      aDayT := 1;
      if not aUser.FieldByName('WORKTIME').IsNull then
        aDayT := aUser.FieldByName('WORKTIME').AsInteger/8;
      if not aUser.FieldByName('USEWORKTIME').IsNull then
        aDayT := aDayT*(aUser.FieldByName('USEWORKTIME').AsInteger/100);//Usage
      //Wochenenden
      aWeekEnds := 0;
      for aDay := trunc(aStart) to trunc(aEnd) do
        begin
          if (DayOfWeek(aDay) = 1) or (DayOfWeek(aDay) = 7) then
            inc(aWeekEnds);
        end;
      Result := aDayT*(aEnd-aStart-aWeekEnds);
      //Kalender Abzüge
      {
      aCalendar := TCalendar.Create(nil);
      aCalendar.SelectPlanedByUserAndTime(asUser,aStart,aEnd);
      aCalendar.Open;
      with aCalendar.DataSet do
        begin
          First;
          while not EOF do
            begin
              bStart:=aCalendar.FieldByName('STARTDATE').AsDateTime;
              bEnd:=aCalendar.FieldByName('ENDDATE').AsDateTime;
              if aCalendar.FieldByName('ALLDAY').AsString = 'Y' then
                begin
                  bStart := trunc(bStart);
                  bEnd :=   trunc(bEnd+1);
                end;
              Result := Result-((bEnd-bStart)*aDayT);
              Next;
            end;
        end;
      aCalendar.Free;
      }
    end;
  aUser.Free;
end;

function TfRoughPlanningFrame.GetProjectTimes(aAccountno : string;aStart, aEnd: TDateTime): real;
var
  a: Integer;
  i: Integer;
  b: Integer;
  aDept: TIntDepartment;
  aInt: TInterval;
  aIEnd: TDateTime;
  aIStart: TDateTime;
  DontSet: Boolean;
  c: Integer;
  aFactor: Extended;
begin
  Result := 0;
  if aEnd-aStart<=0 then exit;
  for i := 0 to FRough.IntervalCount-1 do
    begin
      for c := 0 to FRough.Interval[i].IntervalCount-1 do
        with TProjectInterval(FRough.Interval[i].Interval[c]) do
          begin
            if DepartmentCount=0 then continue;
            for b := 0 to DepartmentCount-1 do
              if Departments[b].Accountno=aAccountno then
                break;
            if Departments[b].Accountno<>aAccountno then continue;
            aDept := Departments[b];
            aIEnd := aEnd;
            aIStart := aStart;
            DontSet := False;
            aInt := FRough.Interval[i].Interval[c];
            if (aInt.StartDate<=aStart) and (aInt.FinishDate>=aEnd) then //Vollzeit
            else if (aInt.StartDate>=aStart) and (aInt.FinishDate<=aEnd) then
              begin
                aIStart := aInt.StartDate;
                aIEnd := aInt.FinishDate;
              end
            else if (aInt.StartDate>=aStart) and (aInt.StartDate<=aEnd) and (aInt.FinishDate>=aEnd) then
              aIStart := aInt.StartDate
            else if (aInt.StartDate<=aStart) and (aInt.FinishDate<=aEnd) and (aInt.FinishDate>=aStart) then
              aIEnd := aInt.FinishDate
            else DontSet := True;
            if aInt.FinishDate-aInt.StartDate>0 then
              begin
                aFactor := (aIEnd-aIStart)/(aInt.FinishDate-aInt.StartDate);
                if ((aDept.Time*(aFactor))<0) then DontSet := True;
              end
            else DontSet := True;
            if not DontSet then
              Result := Result+aDept.Time*(aFactor);
          end;
    end;
  if Result<0 then
    Result := 0;
end;

procedure TfRoughPlanningFrame.CollectActualProject(aProjects: TProjectList;
  aInt: TInterval;aConn : TComponent;aUsers : TUser);
var
  aProject: TProject;
  aSubInt: TProjectInterval = NIL;
  aOldSubInt: TInterval;
  aWK: String;
  aDept: TDataSet;
begin
  inc(FUpdateCount);
  aInt.Task:=aProjects.Text.AsString;
  aInt.OnDrawBackground:=@aIntDrawBackground;
  aInt.Style:=isNone;
  aInt.Visible:=True;
  aProject := TProject.CreateEx(nil,Data,aConn);
  aProject.Select(aProjects.Id.AsVariant);
  aProject.Open;
  Data.SetFilter(aProject.Tasks,Data.QuoteField('CLASS')+'='+Data.QuoteValue('M'),0,'DUEDATE','ASC');
  aSubInt := nil;
  while not aProject.Tasks.EOF do
    begin
      if (aProject.Tasks.FieldByName('CLASS').AsString='M') and (aProject.Tasks.FieldByName('DUEDATE').AsString<>'') and (aProject.Tasks.FieldByName('ACTIVE').AsString<>'N') and (aProject.Tasks.FieldByName('COMPLETED').AsString<>'Y') then
        begin
          aOldSubInt := aSubInt;
          aSubInt := TProjectInterval.Create(FRough);
          aSubInt.Task:=aProject.Tasks.FieldByName('SUMMARY').AsString;
          aSubInt.StartDate:=aProject.Tasks.FieldByName('STARTDATE').AsDateTime;
          if (aSubInt.StartDate=0) and Assigned(aOldSubInt) then
            aSubInt.StartDate:=aOldSubInt.FinishDate;
          if (aSubInt.StartDate=0) then
            aSubInt.StartDate:=Now();
          aSubInt.FinishDate:=aProject.Tasks.FieldByName('DUEDATE').AsDateTime;
          if aSubInt.StartDate>aSubInt.FinishDate then
            aSubInt.StartDate:=aSubInt.FinishDate;
          if Assigned(aOldSubInt) then
            aOldSubInt.AddConnection(aSubInt,False,False);
          aInt.AddInterval(aSubInt);
          aWK := aProject.Tasks.FieldByName('WORKSTATUS').AsString;
          if Data.IsSQLDB then
            begin
              aDept := Data.GetNewDataSet('select cast(sum('+Data.QuoteField('PLANTIME')+') as INT) as '+Data.QuoteField('TIME')+','+Data.QuoteField('USER')+' from '+Data.QuoteField('TASKS')+' where '+Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(aProject.Id.AsString)+' and '+Data.QuoteField('WORKSTATUS')+'='+Data.QuoteValue(aWK)+' group by '+Data.QuoteField('USER'));
              aDept.Open;
              while not aDept.EOF do
                begin
                  if aUsers.DataSet.Locate('ACCOUNTNO',aDept.FieldByName('USER').AsString,[]) then
                    begin
                      if (aUsers.FieldByName('PARENT').IsNull) or (aUsers.FieldByName('TYPE').AsString='G') then
                        aSubInt.AddTime(aUsers.FieldByName('ACCOUNTNO').AsString,aUsers.FieldByName('NAME').AsString,aUsers.IDCode.AsString,aDept.FieldByName('TIME').AsFloat)
                      else
                        begin
                          aUsers.DataSet.Locate('SQL_ID',aUsers.FieldByName('PARENT').AsString,[]);
                          aSubInt.AddTime(aUsers.FieldByName('ACCOUNTNO').AsString,aUsers.FieldByName('NAME').AsString,aUsers.IDCode.AsString,aDept.FieldByName('TIME').AsFloat);
                        end;
                    end;
                  aDept.Next;
                end;
              aDept.Free;
            end;
          aSubInt.Changed:=false;
          aSubInt.OnChanged:=@aSubIntChanged;
        end;
      aProject.Tasks.Next;
    end;
  aProject.Free;
  dec(FUpdateCount);
end;

end.

