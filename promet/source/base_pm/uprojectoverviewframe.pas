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
  uIntfStrConsts,Graphics,Math,uBaseDatasetInterfaces;
type
  TProjectInterval = class(TInterval)
  public
    constructor Create(AGantt: TgsGantt);override;
    destructor Destroy;override;
  end;
  TfProjectOVFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acClose: TAction;
    acDelete: TAction;
    acSave: TAction;
    acExport: TAction;
    acImport: TAction;
    acPrint: TAction;
    acRights: TAction;
    acRestart: TAction;
    acGotoParent: TAction;
    acExecute: TAction;
    acShowProjectGantt: TAction;
    acShowProject: TAction;
    acHigherPrio: TAction;
    acLowerPrio: TAction;
    ActionList1: TActionList;
    acUse: TAction;
    bDayView: TSpeedButton;
    bDelegated2: TSpeedButton;
    bDelegated3: TSpeedButton;
    bDelegated4: TSpeedButton;
    bDelegated5: TSpeedButton;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    bMonthView: TSpeedButton;
    bParentProjects: TSpeedButton;
    bRefresh: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lDate: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
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
    Panel10: TPanel;
    Panel4: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pmTree: TPopupMenu;
    tbTop: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acCancelExecute(Sender: TObject);
    procedure acHigherPrioExecute(Sender: TObject);
    procedure acLowerPrioExecute(Sender: TObject);
    procedure acShowProjectExecute(Sender: TObject);
    procedure acShowProjectGanttExecute(Sender: TObject);
    procedure acUseExecute(Sender: TObject);
    procedure aIntDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aUnfinishedList : TList = nil);
    procedure bDayViewClick(Sender: TObject);
    procedure bDelegated2Click(Sender: TObject);
    procedure bParentProjectsClick(Sender: TObject);
    procedure bMonthViewClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bTodayClick(Sender: TObject);
    procedure bWeekViewClick(Sender: TObject);
    procedure FRoughCalendarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FRoughCalendarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FRoughTreeAfterUpdateCommonSettings(Sender: TObject);
    procedure FRoughTreeResize(Sender: TObject);
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
uses uData,uBaseDBInterface,uBaseERPDBClasses,uCalendar,uMainTreeFrame,uTaskPlan,
  LCLProc,uGanttView;
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
procedure TfProjectOVFrame.bDayViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsWeekNum;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;

procedure TfProjectOVFrame.bDelegated2Click(Sender: TObject);
begin

end;

procedure TfProjectOVFrame.bParentProjectsClick(Sender: TObject);
begin
  Application.ProcessMessages;
  bRefresh.Click;
end;

procedure TfProjectOVFrame.acUseExecute(Sender: TObject);
begin

end;

procedure TfProjectOVFrame.aIntDrawBackground(Sender: TObject;
  aCanvas: TCanvas; aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;
  aUnfinishedList: TList);
var
  TaskPlan : TfTaskPlan;
begin
  Taskplan.aIDrawBackgroundWeekends(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,$e0e0e0,Now());
  Taskplan.aIDrawBackground(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,clBlue,clLime,clRed,Now());
end;

procedure TfProjectOVFrame.acCancelExecute(Sender: TObject);
begin
end;

procedure TfProjectOVFrame.acHigherPrioExecute(Sender: TObject);
var
  aProjects: TProjectList;
  aPrio: Integer;
  bPrio: Integer;
  aRow: Integer;
  aState: TStates;
begin
  debugln('*increment Prioritys');
  if not Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then exit;
  aState := TStates.Create(nil);
  aState.Open;
  aProjects := TProjectList.Create(nil);
  with aProjects.DataSet as IBaseDbFilter do
    Data.SetFilter(aProjects,Data.ProcessTerm(Data.QuoteField('STATUS')+'<>'+Data.QuoteValue('I'))+' AND ('+Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'<>'+Data.QuoteValue('N'))+' OR '+Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'='+Data.QuoteValue(''))+')',0,'GPRIORITY','ASC');
  if aProjects.Locate('SQL_ID',TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id,[]) then
    begin
      aProjects.Prior;
      while (not aProjects.FieldByName('GPRIORITY').IsNull)
        and (not aProjects.EOF)
        and (aState.DataSet.Locate('STATUS;TYPE',VarArrayOf([trim(aProjects.FieldByName('STATUS').AsString),'P']),[]))
        and (not (aState.DataSet.FieldByName('ACTIVE').AsString<>'N'))
        do
          aProjects.Prior;
      aPrio := aProjects.FieldByName('GPRIORITY').AsInteger;
      bPrio := aPrio+10;
      while (not aProjects.FieldByName('GPRIORITY').IsNull) and (not aProjects.EOF) do
        begin
          if aState.DataSet.Locate('STATUS;TYPE',VarArrayOf([trim(aProjects.FieldByName('STATUS').AsString),'P']),[]) then
            if aState.DataSet.FieldByName('ACTIVE').AsString<>'N' then
              begin
                debugln('set '+aProjects.Text.AsString+' to '+IntToStr(bPrio));
                aProjects.Edit;
                aProjects.FieldByName('GPRIORITY').AsInteger:=bPrio;
                inc(bPrio,10);
                aProjects.Post;
              end;
          aProjects.Next;
        end;
      if aProjects.Locate('SQL_ID',TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id,[]) then
        begin
          aProjects.Edit;
          debugln('set '+aProjects.Text.AsString+' to '+IntToStr(aPrio));
          aProjects.FieldByName('GPRIORITY').AsInteger:=aPrio;
          aProjects.Post;
        end;
    end;
  aProjects.Free;
  aState.Free;
  bRefresh.Click;
end;

procedure TfProjectOVFrame.acLowerPrioExecute(Sender: TObject);
var
  aProjects: TProjectList;
  aPrio: Integer;
  bPrio: Integer;
  aState: TStates;
begin
  debugln('*decrement Prioritys');
  if not Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then exit;
  aState := TStates.Create(nil);
  aState.Open;
  aProjects := TProjectList.Create(nil);
  with aProjects.DataSet as IBaseDbFilter do
    Data.SetFilter(aProjects,Data.ProcessTerm(Data.QuoteField('STATUS')+'<>'+Data.QuoteValue('I'))+' AND ('+Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'<>'+Data.QuoteValue('N'))+' OR '+Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'='+Data.QuoteValue(''))+')',0,'GPRIORITY','ASC');
  if aProjects.Locate('SQL_ID',TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id,[]) then
    begin
      aPrio := aProjects.FieldByName('GPRIORITY').AsInteger;
      bPrio := aPrio+10;
      if aProjects.FieldByName('GPRIORITY').IsNull then
        begin
          aProjects.Edit;
          aProjects.FieldByName('GPRIORITY').AsInteger:=bPrio;
          inc(bPrio,10);
          aProjects.Post;
        end;
      while (not aProjects.FieldByName('GPRIORITY').IsNull) and (not aProjects.EOF) do
        begin
          if aState.DataSet.Locate('STATUS;TYPE',VarArrayOf([trim(aProjects.FieldByName('STATUS').AsString),'P']),[]) then
            if aState.DataSet.FieldByName('ACTIVE').AsString<>'N' then
              begin
                debugln('set '+aProjects.Text.AsString+' to '+IntToStr(bPrio));
                aProjects.Edit;
                aProjects.FieldByName('GPRIORITY').AsInteger:=bPrio;
                inc(bPrio,10);
                aProjects.Post;
              end;
          aProjects.Next;
        end;
    end;
  aProjects.Free;
  aState.Free;
  bRefresh.Click;
end;

procedure TfProjectOVFrame.acShowProjectExecute(Sender: TObject);
begin
  if Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then
    Data.GotoLink('PROJECTS@'+IntToStr(TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id));
end;

procedure TfProjectOVFrame.acShowProjectGanttExecute(Sender: TObject);
var
  aProject: TProject;
  aLink: String;
begin
  if Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then
    begin
      aProject := TProject.Create(nil);
      aLink := 'PROJECTS@'+IntToStr(TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id);
      aProject.SelectFromLink(aLink);
      aProject.Open;
      if aProject.Count>0 then
        begin
          aProject.Tasks.SelectActive;
          aProject.Tasks.Open;
          fGanttView.Execute(aProject);
        end;
      aProject.Free;
    end;
  bRefresh.Click;
end;

procedure TfProjectOVFrame.bMonthViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsQuarter;
  FRough.MinorScale:=tsMonth;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfProjectOVFrame.bRefreshClick(Sender: TObject);
var
  CurrInterval: TInterval;
  aProjects: TProjectList;
  aUsers: TUser;
  arec : Variant;
  i: Integer;
begin
  Screen.Cursor:=crHourGlass;
  FRough.BeginUpdate;
  if Assigned(FRough.Tree.Objects[0,FRough.Tree.Row]) then
    aRec := TInterval(FRough.Tree.Objects[0,FRough.Tree.Row]).Id;
  while FRough.IntervalCount>0 do
    FRough.DeleteInterval(0);
  StartFilling;
  FRough.EndUpdate;
  for i := 0 to FRough.Tree.RowCount-1 do
    if Assigned(FRough.Tree.Objects[0,i]) and (TInterval(FRough.Tree.Objects[0,i]).Id=aRec) then
      begin
        FRough.Tree.Row:=i;
        break;
      end;
  Screen.Cursor:=crDefault;
end;
procedure TfProjectOVFrame.bTodayClick(Sender: TObject);
begin
  FRough.StartDate:=Now();
end;
procedure TfProjectOVFrame.bWeekViewClick(Sender: TObject);
begin
  FRough.MinorScale:=tsDay;
  FRough.MajorScale:=tsMonth;
  FRough.MinorScale:=tsWeekNumPlain;
  FRough.Calendar.StartDate:=FRough.Calendar.StartDate;
end;
procedure TfProjectOVFrame.FRoughCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lDate.Caption := DateToStr(FRough.Calendar.VisibleStart+trunc((X/FRough.Calendar.GetIntervalWidth)));
end;
procedure TfProjectOVFrame.FRoughCalendarShowHint(Sender: TObject;
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
begin  exit;
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
procedure TfProjectOVFrame.FRoughTreeAfterUpdateCommonSettings(
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

procedure TfProjectOVFrame.FRoughTreeResize(Sender: TObject);
begin
  FRough.Tree.ColWidths[0]:=0;
  FRough.Tree.ColWidths[1]:=0;
  FRough.Tree.ColWidths[2]:=FRough.Tree.Width-FRough.Tree.ColWidths[3]-FRough.Tree.ColWidths[4]-FRough.Tree.ColWidths[5]-20; //20=scrollbarwidth maybe other values on other widgetsets
  FRough.Tree.ColWidths[6]:=0;
  FRough.Tree.ColWidths[7]:=0;
end;

function TfProjectOVFrame.SetRights: Boolean;
begin

end;

constructor TfProjectOVFrame.Create(AOwner: TComponent);
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
  FRough.Tree.OnResize:=@FRoughTreeResize;
  FRough.Calendar.OnShowHint:=@FRoughCalendarShowHint;
  FRough.Calendar.ShowHint:=True;
  FRough.Calendar.OnMouseMove:=@FRoughCalendarMouseMove;
  //FRough.Visible:=False;
  bMonthView.Click;
end;

destructor TfProjectOVFrame.Destroy;
begin
  FRough.Free;
  fRefreshList.Free;
  inherited Destroy;
end;

procedure TfProjectOVFrame.SetLanguage;
begin
end;

procedure TfProjectOVFrame.ShowFrame;
begin
  inherited ShowFrame;
end;

procedure TfProjectOVFrame.StartFilling;
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
    for i := 0 to FRough.IntervalCount-1 do
      begin
        if FRough.Interval[i].Id = id then
          begin
            Result := FRough.Interval[i];
            break;
          end
        else if FRough.Interval[i].IntervalCount>0 then
          begin
            Result := FindInterval(FRough.Interval[i],Id);
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
  function AddFromDB(aProjects : TProjectList;lvl : Integer = 0) : TProjectInterval;
  var
    aInt: TProjectInterval;
    aParent: TInterval;
    aParents: TProjectList;
  begin
    aInt := TProjectInterval.Create(FRough);
    aInt.Id:=aProjects.Id.AsVariant;
    aInt.Task:=aProjects.Text.AsString;
    aInt.StartDate:=aProjects.FieldByName('START').AsDateTime;
    aInt.FinishDate:=aProjects.FieldByName('END').AsDateTime;
    if aInt.FinishDate=0 then
      aInt.FinishDate:=aProjects.FieldByName('TARGET').AsDateTime;
    if aInt.StartDate=0 then
      aInt.StartDate:=aProjects.FieldByName('CREATEDAT').AsDateTime;
    if aInt.FinishDate<=aInt.StartDate then
      aInt.FinishDate:=aInt.StartDate+1;
    aInt.Visible:=True;
    aParent := IntervalById(aProjects.FieldByName('PARENT').AsVariant);
    if not aProjects.FieldByName('COLOR').IsNull then
      aInt.Color:=StringToColor(aProjects.FieldByName('COLOR').AsString);
    if Assigned(aParent) then
      aParent.AddInterval(aInt)
    else
      begin
        aParents := TProjectList.Create(nil);
        aParents.Select(aProjects.FieldByName('PARENT').AsVariant);
        aParents.Open;
        if (bParentProjects.Down) and (aParents.Count>0) and (aProjects.FieldByName('PARENT').AsVariant<>aProjects.Id.AsVariant) and (lvl<20) then
          begin
            aParent := AddFromDB(aParents,lvl+1);
            aParent.AddInterval(aInt);
          end
        else
          FRough.AddInterval(aInt);
        aParents.Free;
      end;
    aInt.OnDrawBackground:=@aIntDrawBackground;
    Result := aInt;
  end;

var
  aProjects: TProjectList;
  aInt: TInterval;
  aState: TStates;
  aParent: TInterval;
begin
  aProjects :=  TProjectList.Create(nil);
  with aProjects.DataSet as IBaseDbFilter do
    Data.SetFilter(aProjects,Data.ProcessTerm(Data.QuoteField('STATUS')+'<>'+Data.QuoteValue('I'))+' AND ('+Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'<>'+Data.QuoteValue('N'))+' OR '+Data.ProcessTerm(Data.QuoteField('GROSSPLANNING')+'='+Data.QuoteValue(''))+')',0,'GPRIORITY','ASC');
  aState := TStates.Create(nil);
  aState.Open;
  FRough.BeginUpdate;
  aProjects.First;
  while (not aProjects.EOF)  do
    begin
      if not aProjects.FieldByName('GPRIORITY').IsNull then
        if aState.DataSet.Locate('STATUS;TYPE',VarArrayOf([trim(aProjects.FieldByName('STATUS').AsString),'P']),[]) then
          if aState.DataSet.FieldByName('ACTIVE').AsString<>'N' then
            if not Assigned(IntervalById(aProjects.Id.AsVariant)) then
              aInt := AddFromDB(aProjects);
      aProjects.Next;
    end;
  aProjects.First;
  while (not aProjects.EOF)  do
    begin
      if aProjects.FieldByName('GPRIORITY').IsNull then
        if aState.DataSet.Locate('STATUS;TYPE',VarArrayOf([trim(aProjects.FieldByName('STATUS').AsString),'P']),[]) then
          if aState.DataSet.FieldByName('ACTIVE').AsString<>'N' then
            if not Assigned(IntervalById(aProjects.Id.AsVariant)) then
              aInt := AddFromDB(aProjects);
      aProjects.Next;
    end;
  aState.Free;
  aProjects.Free;
  FRough.EndUpdate;
end;

end.

