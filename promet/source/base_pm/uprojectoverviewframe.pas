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
Created 06.05.2014
*******************************************************************************}
unit uprojectoverviewframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, TAGraph, LR_DBSet,
  LR_Class, LR_View, Forms, Controls, ComCtrls, Buttons, ActnList, Menus,
  ExtCtrls, DbCtrls, StdCtrls, uExtControls, DBZVDateTimePicker, db,
  uPrometFrames, uPrometFramesInplace, uBaseDBClasses, Dialogs, Spin, EditBtn,
  DBGrids, variants,uStatistic,SynCompletion,md5,LCLType,
  TASeries, TACustomSeries,fpsqlparser,Clipbrd,gsGanttCalendar,uProjects,Grids,
  uIntfStrConsts,Graphics,Math;
type
  TProjectInterval = class(TInterval)
  public
    constructor Create(AGantt: TgsGantt);override;
    destructor Destroy;override;
  end;
  TfProjectOverviewFrame = class(TPrometMainFrame)
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
  private
    { private declarations }
    FEditable : Boolean;
    FRough: TgsGantt;
    FUpdateCount : Integer;
  protected
    fRefreshList : TList;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetLanguage;override;
    procedure ShowFrame; override;
    procedure StartFilling;
  end;

procedure AddToMainTree(aAction : TAction;Node : TTreeNode);
var
  MainNode : TTreeNode;
implementation
uses uData,uBaseDBInterface,uBaseERPDBClasses,uCalendar,uMainTreeFrame;
resourcestring
  strProjectOverview                                    = 'Projektübersicht';
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
constructor TProjectInterval.Create(AGantt: TgsGantt);
begin
  inherited Create(AGantt);
end;
destructor TProjectInterval.Destroy;
begin
  inherited Destroy;
end;
procedure TfProjectOverviewFrame.bDayViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsWeekNum;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfProjectOverviewFrame.bDelegated2Click(Sender: TObject);
begin
  if Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then
    Data.GotoLink('PROJECTS@'+IntToStr(TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id));
end;
procedure TfProjectOverviewFrame.acUseExecute(Sender: TObject);
begin

end;
procedure TfProjectOverviewFrame.acCancelExecute(Sender: TObject);
begin
end;
procedure TfProjectOverviewFrame.bMonthViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsQuarter;
  FRough.MinorScale:=tsMonth;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfProjectOverviewFrame.bRefreshClick(Sender: TObject);
var
  CurrInterval: TInterval;
  aProjects: TProjectList;
  aUsers: TUser;
begin
  Screen.Cursor:=crHourGlass;
  Screen.Cursor:=crDefault;
end;
procedure TfProjectOverviewFrame.bTodayClick(Sender: TObject);
begin
  FRough.StartDate:=Now();
end;
procedure TfProjectOverviewFrame.bWeekViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsMonth;
  FRough.MinorScale:=tsWeekNumPlain;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfProjectOverviewFrame.FRoughCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lDate.Caption := DateToStr(FRough.Calendar.VisibleStart+trunc((X/FRough.Calendar.GetIntervalWidth)));
end;
procedure TfProjectOverviewFrame.FRoughCalendarShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  List: TList;
  ay: Integer;
  aInt: TProjectInterval;
  i: Integer;
  aPInt: TInterval;
  aDrect: TRect;
  aDePrect: TRect;
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
                  end;
              end;
          end;

    end;
end;
procedure TfProjectOverviewFrame.FRoughTreeAfterUpdateCommonSettings(
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
function TfProjectOverviewFrame.SetRights: Boolean;
begin

end;

constructor TfProjectOverviewFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateCount:=0;
  TabCaption:=strProjectOverview;
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

destructor TfProjectOverviewFrame.Destroy;
begin
  FRough.Free;
  fRefreshList.Free;
  inherited Destroy;
end;

procedure TfProjectOverviewFrame.SetLanguage;
begin
end;

procedure TfProjectOverviewFrame.ShowFrame;
begin
  inherited ShowFrame;
end;

procedure TfProjectOverviewFrame.StartFilling;
begin

end;

end.

