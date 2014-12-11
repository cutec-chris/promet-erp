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
unit utask;
//TODO:Trigger for updating sum(hours)
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, uBaseDbInterface, db, uBaseERPDBClasses,Math;
type
  TDependencies = class;
  TTaskSnapshots = class(TBaseDbDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  //In memory class to hold an task during calculations
  TTaskInterval = class
  private
    FDueDate: TDateTime;
    FPlan: Double;
    FStartDate: TDateTime;
  public
    property StartDate : TDateTime read FStartDate write FStartDate;
    property DueDate : TDateTime read FDueDate write FDueDate;
    property PlanTime : Double read FPlan write FPlan;
  end;
  TTaskList = class(TBaseERPList,IBaseHistory)
    procedure DataSetAfterCancel(aDataSet: TDataSet);
    procedure DataSetAfterPost(aDataSet: TDataSet);
    procedure DataSetBeforeDelete(aDataSet: TDataSet);
    procedure DataSetBeforePost(aDataSet: TDataSet);
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FAddProjectOnPost : Boolean;
    FCompletedChanged : Boolean;
    FCheckedChanged : Boolean;
    FAddSummaryOnPost : Boolean;
    FDueDateChanged: Boolean;
    FStartDateChanged : Boolean;
    FHistory: TBaseHistory;
    FSnapshots: TTaskSnapshots;
    FTempUsers : TUser;
    FDependencies: TDependencies;
    FDS: TDataSource;
    DoCheckTask : Boolean;
    FUserID: String;
    function GetownerName: string;
    function GetProject: TField;
    function GetUserName: string;
    function GetHistory: TBaseHistory;
  protected
    procedure OpenItem(AccHistory: Boolean=True); override;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure SelectActiveByUser(AccountNo : string);
    procedure SelectActive;
    procedure SelectActivewithoutDeps;
    procedure SelectByUser(AccountNo : string);
    procedure SelectUncompletedByUser(AccountNo : string);
    procedure SelectActiveByUserChangedSince(AccountNo: string; aDate: TdateTime);
    procedure SelectByDept(aDept : Variant);
    procedure SelectByParent(aParent : Variant);
    procedure SelectUncompletedByParent(aParent : Variant);
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    procedure SetDisplayLabels(aDataSet: TDataSet); override;
    function CreateTable : Boolean;override;
    procedure CascadicPost;override;
    procedure CascadicCancel;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    procedure CheckChilds;
    procedure CheckDependTasks;
    procedure MoveDependTasks;
    procedure Open; override;
    function CalcDates(var aStart, aDue: TDateTime): Boolean;
    function GetUnterminatedDependencies: TStrings;
    //This function calculates the earliest possible Start and Enddate for the Task
    function Terminate(aEarliest : TDateTime;var aStart,aEnd,aDuration : TDateTime) : Boolean;
    //This function retuirns True if one of the Dependencies of aTask or its Dependencies (recoursive) points to This Task
    function DependsOnMe(aTask: TTaskList; aDeep: Integer=30): Boolean;
    //This function calculates the earliest possible Start and Enddate for the Task and sets it as Start and Enddate
    function Terminate(aEarliest : TDateTime) : Boolean;
    function WaitTimeDone : TDateTime;
    function GetInterval : TTaskInterval;
    procedure MakeSnapshot(aName : string);
    procedure DisableDS;
    property OwnerName : string read GetownerName;
    property UserName : string read GetUserName;
    property Project : TField read GetProject;
    property History : TBaseHistory read FHistory;
    property UserID : String read FUserID write FUserID;
    property Snapshots : TTaskSnapshots read FSnapshots;
    property Dependencies : TDependencies read FDependencies;
  end;

  { TTaskLinks }

  TTaskLinks = class(TLinks)
  private
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
  end;

  { TDependencies }

  TDependencies = class(TBaseDBDataset)
    procedure DataSetAfterDelete(aDataSet: TDataSet);
    procedure DataSetBeforeDelete(aDataSet: TDataSet);
  private
    Ref_Id : Variant;
    FTask: TTaskList;
  protected
    property Task : TTaskList read FTask write FTask;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Add(aLink : string);
    procedure SelectByLink(aLink : string);
  end;
  TTask = class(TTaskList)
  private
    FLinks: TTaskLinks;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure CheckDependencies(aLevel: Integer=0);
    property Links : TTaskLinks read FLinks;
  end;
  TMoveTasksEvent = procedure(Sender: TObject;var Allowed : Boolean);

var
  OnMoveTasks : TMoveTasksEvent;
resourcestring
  strTaskCompleted          = 'Aufgabe fertiggestellt';
  strTaskreopened           = 'Aufgabe wiedereröffnet';
  strTaskUDelegated         = '%s - wurde Ihnen delegiert';
  strTaskSCompleted         = '%s - erledigt';
  strTaskSChecked           = '%s - geprüft';
  strTaskSreopened          = '%s - wiedereröffnet';
  strTaskChecked            = 'Aufgabe geprüft';
  strProjectChanged         = 'Project geändert';
  strDelegated              = 'an %s delegiert';
  strPlantime               = 'Planzeit';
  strActtime                = 'Istzeit';
  strBuffertime             = 'Wartezeit';
  strCompletedAt            = 'fertiggestellt';
  strCompleted              = 'fertig';
  strStarted                = 'gestartet';
  strTaskAdded              = '%s - hinzugefügt';
  strTaskDeleted            = '%s - gelöscht';
  strHasChilds              = 'hat Untereinträge';
  strPercentDone            = '% erledigt';
  strWorkstatus             = 'Bearbeitungsstatus';
  strRenamed                = 'umbenannt in "%s"';
implementation
uses uBaseApplication,uIntfStrConsts,uProjects,uData,uCalendar;

function CompareStarts(Item1, Item2: Pointer): Integer;
begin
  Result:= 0;
  if (TTaskInterval(Item1)).StartDate > (TTaskInterval(Item2)).StartDate then
    Result:= 1
  else
    if (TTaskInterval(Item1)).StartDate < (TTaskInterval(Item2)).StartDate then
      Result:= -1
end;

procedure TTaskSnapshots.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TASKSNAPSHOTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,True);
            Add('STARTDATE',ftDateTime,0,True);
            Add('ENDDATE',ftDateTime,0,True);
          end;
    end;
end;

procedure TDependencies.DataSetAfterDelete(aDataSet: TDataSet);
var
  aTask: TTask;
begin
  if Assigned(fTask) then
    begin
      if DataSet.RecordCount = 0 then
        begin
          if not FTask.CanEdit then
            FTask.DataSet.Edit;
          FTask.FieldByName('DEPDONE').AsString := 'Y';
        end;
    end
  else
    begin
      aTask := TTask.CreateEx(nil,DataModule);
      aTask.Select(Ref_Id);
      aTask.Open;
      if aTask.Count>0 then
        aTask.CheckDependencies;
      aTask.Free;
    end;
end;

procedure TDependencies.DataSetBeforeDelete(aDataSet: TDataSet);
begin
  Ref_Id := aDataSet.FieldByName('REF_ID').AsVariant;
end;

constructor TDependencies.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  DataSet.AfterDelete:=@DataSetAfterDelete;
  DataSet.BeforeDelete:=@DataSetBeforeDelete;
  FTask:=nil;
end;
procedure TDependencies.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DEPENDENCIES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID_ID',ftLargeint,0,False);
            Add('LINK',ftString,200,False); //Link
            Add('ICON',ftInteger,0,False); //LinkIcon
            Add('NAME',ftString,100,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('REF_ID_ID','REF_ID_ID',[]);
          end;
    end;
end;
procedure TDependencies.Add(aLink: string);
var
  tmp: String;
begin
  Open;
  if not Task.CanEdit then
    Task.DataSet.Edit;
  Task.FieldByName('DEPDONE').AsString:='N';
  Task.DataSet.Post;
  with DataSet do
    begin
      Append;
      with BaseApplication as IBaseDbInterface do
        begin
          tmp := copy(aLink,7,length(aLink));
          if pos('{',tmp)>0 then tmp := copy(tmp,0,pos('{',tmp)-1);
          FieldByName('REF_ID_ID').AsString := tmp;
          FieldByName('NAME').AsString:=Data.GetLinkDesc(aLink);
          FieldByName('LINK').AsString:=aLink;
          FieldByName('ICON').AsInteger:=Data.GetLinkIcon(aLink);
        end;
      Post;
    end;
end;

procedure TDependencies.SelectByLink(aLink: string);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('LINK')+'='+QuoteValue(aLink)+')';
    end;
end;
procedure TTaskLinks.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('RREF_ID').AsVariant:=(Parent as TTask).Id.AsVariant;
end;
constructor TTask.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FLinks := TTaskLinks.CreateEx(Self,DM,aConnection);
end;
destructor TTask.Destroy;
begin
  FLinks.Free;
  inherited Destroy;
end;
procedure TTask.CheckDependencies(aLevel : Integer = 0);
var
  aTask: TTask;
  aCompCount : Double = 0;
  AllCompleted : String = 'Y';
  aPercentage: Extended;
  aTime: TDateTime;
  tmp : string = '';
  i: Integer;
begin
  if aLevel>10 then exit;//recoursion check we may have an circulating dependency
  for i := 0 to aLevel-1 do
  tmp := tmp+' ';
  //debugln(tmp+'CheckDependencies:'+DataSet.FieldByName('SUMMARY').AsString);
  if not Dependencies.DataSet.Active then
    Dependencies.Open;
  Dependencies.DataSet.First;
  while not Dependencies.DataSet.EOF do
    begin
      aTask := TTask.CreateEx(Self,DataModule,Connection);
      aTask.SelectFromLink(Dependencies.FieldByName('LINK').AsString);
      aTask.Open;
      if aTask.Count>0 then
        begin
          aCompCount:=aCompCount+(aTask.FieldByName('PERCENT').AsInteger/100);
          if aTask.FieldByName('COMPLETED').AsString <> 'Y' then
            begin
              aTask.CheckDependencies(aLevel+1);
              AllCompleted:='N';
            end;
        end;
      aTask.Free;
      Dependencies.DataSet.Next;
    end;
  if not CanEdit then
    DataSet.Edit;
  DataSet.FieldByName('DEPDONE').AsString := AllCompleted;
  DataSet.Post;
end;

procedure TTaskList.MakeSnapshot(aName: string);
var
  aStart: TDateTime;
  aEnd: TDateTime;
begin
  CalcDates(aStart,aEnd);
  Snapshots.Append;
  Snapshots.FieldByName('NAME').AsString:=aName;
  Snapshots.FieldByName('STARTDATE').AsDateTime:=aStart;
  Snapshots.FieldByName('ENDDATE').AsDateTime:=aEnd;
  Snapshots.Post;
end;

procedure TTaskList.CheckChilds;
var
  aTasks: TTaskList;
  aCompCount : Double = 0;
  AllCompleted : String = 'Y';
  aPercentage: Extended;
  Chngd: Boolean=False;
begin
  //debugln('CheckChilds:'+DataSet.FieldByName('SUMMARY').AsString);
  if Id.IsNull then exit;
  aTasks := TTaskList.CreateEx(Self,DataModule,Connection);
  aTasks.SelectByParent(Id.AsVariant);
  aTasks.Open;
  with aTasks.DataSet do
    begin
      First;
      if not EOF then
        begin
          while not EOF do
            begin
//              aTasks.CheckChilds;
              aCompCount:=aCompCount+(aTasks.FieldByName('PERCENT').AsInteger/100);
              if aTasks.FieldByName('COMPLETED').AsString <> 'Y' then AllCompleted:='N';
              Next;
            end;
          aPercentage := 100*(aCompCount/aTasks.Count);
          if (DataSet.FieldByName('COMPLETED').AsString <> AllCompleted)
          or (DataSet.FieldByName('PERCENT').AsInteger <> round(aPercentage)) then
            begin
              if not CanEdit then
                DataSet.Edit;
              if (DataSet.FieldByName('PERCENT').AsInteger <> round(aPercentage)) then
                begin
                  DataSet.FieldByName('PERCENT').AsInteger := round(aPercentage);
                  Chngd := True;
                end;
              if (DataSet.FieldByName('COMPLETED').AsString <> AllCompleted) then
                begin
                  DataSet.FieldByName('COMPLETED').AsString := AllCompleted;
                  Chngd:=True;
                end;
              if Chngd then
                DataSet.Post
              else DataSet.Cancel;
            end;
        end;
    end;
  aTasks.Free;
end;
procedure TTaskList.CheckDependTasks;
var
  aDeps: TDependencies;
  aTask: TTask;
  AllCompleted: Char = 'Y';
  HasTrigger: Boolean;
  aTime: TDateTime;
begin
  //debugln('CheckDependtasks:'+DataSet.FieldByName('SUMMARY').AsString);
  HasTrigger := Data.TriggerExists('TASKS_INS_DEPEND');
  aDeps := TDependencies.CreateEx(Self,DataModule,Connection);
  aDeps.SelectByLink(Data.BuildLink(DataSet));
  aDeps.Open;
  aDeps.DataSet.First;
  while not aDeps.DataSet.EOF do
    begin
      aTask := TTask.CreateEx(Self,DataModule,Connection);
      aTask.Select(aDeps.FieldByName('REF_ID').AsVariant);
      aTask.Open;
      if (FieldByName('COMPLETED').AsString='Y') and (FieldByName('BUFFERTIME').AsFloat>0) then
        begin  //set Earlyes begin when Buffertime was > 0
          if FieldByName('COMPLETEDAT').IsNull then
            aTime := Now()
          else
            aTime := FieldByName('COMPLETEDAT').AsDateTime;
          if not aTask.CanEdit then aTask.DataSet.Edit;
          aTask.FieldByName('EARLIEST').AsDateTime := aTime+FieldByName('BUFFERTIME').AsFloat;
          aTask.Post;
        end;
      if (not HasTrigger) then
        begin
          aTask.CheckDependencies;
          if aTask.FieldByName('DEPDONE').AsString <> 'Y' then AllCompleted:='N';
        end;
      aTask.Free;
      aDeps.DataSet.Next;
    end;
  aDeps.Free;
end;

procedure TTaskList.MoveDependTasks;
var
  aDeps: TDependencies;
  aTask: TTask;
begin
  aDeps := TDependencies.CreateEx(Self,DataModule,Connection);
  aDeps.SelectByLink(Data.BuildLink(DataSet));
  aDeps.Open;
  with aDeps.DataSet do
    begin
      First;
      while not EOF do
        begin
          aTask := TTask.CreateEx(Self,DataModule,Connection);
          aTask.Select(aDeps.FieldByName('REF_ID').AsVariant);
          aTask.Open;
          if (aTask.FieldByName('COMPLETED').AsString<>'Y') and (aTask.FieldByName('STARTDATE').AsDateTime<DataSet.FieldByName('DUEDATE').AsDateTime) then
            begin
              aTask.DataSet.DisableControls;
              if not aTask.CanEdit then aTask.DataSet.Edit;
              aTask.FieldByName('STARTDATE').AsDateTime:=DataSet.FieldByName('DUEDATE').AsDateTime;
              aTask.Post;
              atask.DataSet.EnableControls;
            end;
          aTask.Free;
          Next;
        end;
    end;
  aDeps.Free;
end;

procedure TTaskList.Open;
begin
  inherited Open;
end;

function TTaskList.CalcDates(var aStart, aDue: TDateTime) : Boolean;
var
  MinimalTaskLength: Extended;
  aDur: Extended;
begin
  Result := False;//Changed
  aDue := FieldByName('DUEDATE').AsDateTime;
  aStart := FieldByName('STARTDATE').AsDateTime;
  MinimalTaskLength := StrToFloatDef(FieldByName('PLANTIME').AsString,1);
  if (aStart>0) and (aDue>0) then
    aDur := aDue-aStart
  else aDur := MinimalTaskLength;
  if FieldByName('COMPLETED').AsString = 'Y' then
    begin
      if FieldByName('STARTEDAT').AsDateTime > 0 then
        aStart := FieldByName('STARTEDAT').AsDateTime;
      //else if (FieldByName('COMPLETEDAT').AsDAteTime > 0) then
      //  aStart := 0;
      if FieldByName('COMPLETEDAT').AsDAteTime > 0 then
        aDue := FieldByName('COMPLETEDAT').AsDAteTime;
    end
  else
    begin
      if aDur>MinimalTaskLength then MinimalTaskLength:=aDur;
      if (aStart < Now()) and (aDue=0) then
        aStart := Now();
      if aStart < FieldByName('EARLIEST').AsDateTime then
        begin
          aStart := FieldByName('EARLIEST').AsDateTime;
          Result := True;
        end;
    end;
  if (aDue=0) and (aStart=0) then
    begin
      aStart := Now();
      aDue := aStart+StrToFloatDef(FieldByName('PLANTIME').AsString,1);
    end
  else if aStart = 0 then
    aStart := aDue-StrToFloatDef(FieldByName('PLANTIME').AsString,1)
  else if aDue=0 then
    aDue := aStart+StrToFloatDef(FieldByName('PLANTIME').AsString,1);
  if aDue<aStart+MinimalTaskLength then
    begin
      aDue := aStart+MinimalTaskLength;
      result := True;
    end;
end;

function TTaskList.GetUnterminatedDependencies: TStrings;
var
  aTask: TTask;
begin
  Result := TStringList.Create;
  Dependencies.Open;
  Dependencies.First;
  aTask := TTask.CreateEx(nil,DataModule,Connection);
  while not Dependencies.EOF do
    begin
      aTask.SelectFromLink(Dependencies.FieldByName('LINK').AsString);
      aTask.Open;
      if aTask.Count>0 then
        begin
          if (aTask.FieldByName('DUEDATE').IsNull) and (aTask.FieldByName('COMPLETED').AsString<>'Y') then
            Result.Add(Dependencies.FieldByName('LINK').AsString);
        end;
      Dependencies.Next;
    end;
  aTask.Free;
end;

function TTaskList.Terminate(aEarliest: TDateTime; var aStart, aEnd,
  aDuration: TDateTime): Boolean;
var
  aStartDate : TDateTime;
  aTask: TTask;
  aUser: TUser;
  ResourceTimePerDay:Double;
  Usage: Extended;
  WorkTime: Extended;
  Duration: Extended;
  bTasks: TTaskList;
  aActStartDate: TDateTime=0;
  aNextStartDate:TDateTime=0;
  aFound: Boolean=false;
  aNetTime: Double;
  aIntervals: TList;
  i: Integer;
  Int1: TTaskInterval;
  Int2: TTaskInterval;
  aCalendar: TCalendar;
  aInterval: TTaskInterval;
  aTime: Extended;
  a: Int64;
  aNow: Int64;
  aPercent: Double;
  FUsage: Extended;
  FWorkTime: Extended;
  TimeNeeded: Double;
  aDayUseTime: Extended;
  aActEndDate: Extended;
begin
  Result := False;
  //Get Latest Dependency
  Dependencies.Open;
  aTask := TTask.CreateEx(nil,DataModule,Connection);
  aStartDate:=Now();
  while not Dependencies.EOF do
    begin
      aTask.SelectFromLink(Dependencies.FieldByName('LINK').AsString);
      aTask.Open;
      if aTask.Count>0 then
        begin
          if aTask.WaitTimeDone>aStartDate then
            aStartDate:=aTask.WaitTimeDone;
        end;
      Dependencies.Next;
    end;
  aTask.Free;
  //Get Earlyiest
  if not FieldByName('EARLIEST').IsNull then
    if FieldByName('EARLIEST').AsDateTime>aStartDate then
      aStartDate:=FieldByName('EARLIEST').AsDateTime;
  if aStartDate<aEarliest then
    aStartDate:=aEarliest;
  //Calculate duration
  aUser := TUser.CreateEx(nil,Data);
  aUser.SelectByAccountno(FieldByName('USER').AsString);
  aUser.Open;
  if aUser.Count>0 then
    begin
      Usage := aUser.FieldByName('USEWORKTIME').AsInteger/100;
      if Usage = 0 then Usage := 1;
      WorkTime:=aUser.WorkTime*Usage;
      Usage := WorkTime/8;
      ResourceTimePerDay:=Usage;
    end
  else
    ResourceTimePerDay := 1;
  if not FieldByName('PLANTIME').IsNull then
    aNetTime := FieldByName('PLANTIME').AsFloat
  else aNetTime := 1;
  Duration:=(aNetTime*(1/ResourceTimePerDay));
  if Duration<0.5 then Duration:=0.5;
  //Collect Tasks
  aIntervals := TList.Create;
  //Find first free Slot
  bTasks := TTaskList.CreateEx(nil,DataModule,Connection);
  if aUser.FieldByName('TYPE').AsString<>'G' then
    begin
      bTasks.SelectUncompletedByUser(FieldByName('USER').AsString);
      bTasks.SortFields:='STARTDATE';
      bTasks.SortDirection:=sdAscending;
      bTasks.Open;

      aIntervals.Add(TTaskInterval.Create);
      TTaskInterval(aIntervals[0]).DueDate:=aStartDate;
      with bTasks.DataSet do
        begin
          while not EOF do
            begin
              if  (not bTasks.FieldByName('STARTDATE').IsNull)
              and (not bTasks.FieldByName('DUEDATE').IsNull)
              //and (not (bTasks.FieldByName('PLANTASK').AsString='N'))
              and (not (bTasks.Id.AsVariant=Self.Id.AsVariant))
              then
                if not DependsOnMe(bTasks,2) then
                  aIntervals.Add(bTasks.GetInterval);
              Next;
            end;
          if bTasks.EOF then
            aFound := True;
        end;
      //Collect Calendar entrys
      aCalendar := TCalendar.CreateEx(nil,DataModule,Connection);
      aCalendar.SelectPlanedByUserAndTime(FieldByName('USER').AsString,Now()-30,Now()+(1*365));
      aCalendar.Open;
      with aCalendar.DataSet do
        begin
          First;
          while not EOF do
            begin
              aInterval := TTaskInterval.Create;
              aInterval.StartDate:=aCalendar.FieldByName('STARTDATE').AsDateTime;
              aInterval.DueDate:=aCalendar.FieldByName('ENDDATE').AsDateTime;
              aInterval.PlanTime:=-1;
              if aCalendar.FieldByName('ALLDAY').AsString = 'Y' then
                begin
                  aInterval.StartDate := trunc(aInterval.StartDate);
                  aInterval.DueDate := trunc(aInterval.DueDate+1);
                  aInterval.PlanTime:=aInterval.DueDate-aInterval.StartDate;
                end;
              aIntervals.Add(aInterval);
              Next;
            end;
        end;
      aCalendar.Free;
    end;
  aUser.Free;
  //Sort by Start Date
  aIntervals.Sort(@CompareStarts);
  //Find Slot
  aFound := False;
  aNow := round(aStartDate);
  TimeNeeded := Duration;
  while (not ((aFound) and (TimeNeeded<=0))) do
    begin
      if (not ((DayOfWeek(aNow)=1) or (DayOfWeek(aNow)=7))) then
        begin
          aPercent := 0;
          for i := 1 to aIntervals.Count-1 do
            begin
              Int2 := TTaskInterval(aIntervals[i]);
              if ((trunc(Int2.StartDate)<=aNow)
              and (trunc(Int2.DueDate)>=aNow)) then
                aPercent := aPercent+((Int2.PlanTime/(Int2.DueDate-Int2.StartDate))*(1/ResourceTimePerDay));
            end;
          if not aFound then
            if aPercent<0.7 then
              begin
                aFound := True;
                aActStartDate:=aNow;
              end;
          if aFound then
            begin
              aDayUseTime := 1-aPercent;
              if aDayUseTime<0 then aDayUseTime:=0;
              TimeNeeded:=TimeNeeded-aDayUseTime;
              if TimeNeeded<=0 then
                begin
                  aActEndDate := aNow+0.99999;
                  break;
                end;
              if aDayUseTime<0 then
                begin
                  TimeNeeded := Duration;
                  aFound := False;
                end;
            end;
        end;
      inc(aNow);
    end;
  if not aFound then
    begin
      aActStartDate:=TTaskInterval(aIntervals[aIntervals.Count-1]).DueDate;
      aFound:=True;
    end;
  bTasks.Free;
  //Set it
  if aFound then
    begin
      aStart:=aActStartDate;
      aEnd:=aActEndDate;
      aDuration:=aNetTime;
      Result := True;
    end;
  aIntervals.Clear;
  aIntervals.Free;
end;
function TTaskList.DependsOnMe(aTask: TTaskList;aDeep : Integer = 30): Boolean;
  function RecourseDepends(bTask : TTaskList;aDeep : Integer = 0) : Boolean;
  var
    cTask: TTaskList;
  begin
    Result := False;
    if aDeep>1 then exit;
    Result := bTask.Id.AsVariant=Id.AsVariant;
    if not Result then
      begin
        bTask.Dependencies.Open;
        bTask.Dependencies.First;
        cTask := TTaskList.CreateEx(nil,DataModule,Connection);
        while not bTask.Dependencies.EOF do
          begin
            cTask.SelectFromLink(bTask.Dependencies.FieldByName('LINK').AsString);
            cTask.Open;
            if cTask.Count>0 then
              begin
                Result := RecourseDepends(cTask,aDeep+1);
                if Result then break;
              end;
            bTask.Dependencies.Next;
          end;
        cTask.Free;
      end;
  end;

begin
  Result := RecourseDepends(aTask,aDeep);
end;
function TTaskList.Terminate(aEarliest: TDateTime): Boolean;
var
  aStart,aEnd,aDuration : TDateTime;
begin
  if Terminate(aEarliest,aStart,aEnd,aDuration) then
    begin
      Edit;
      FieldByName('STARTDATE').AsDateTime:=aStart;
      FieldByName('DUEDATE').AsDateTime:=aEnd;
      FieldByName('PLANTIME').AsFloat:=aDuration;
    end;
end;

function TTaskList.WaitTimeDone: TDateTime;
begin
  Result := Now();
  if FieldByName('DUEDATE').AsDateTime>0 then
    begin
      Result := FieldByName('DUEDATE').AsDateTime+FieldByName('BUFFERTIME').AsFloat;
    end;
end;
function TTaskList.GetInterval: TTaskInterval;
begin
  Result := TTaskInterval.Create;
  Result.StartDate:=FieldByName('STARTDATE').AsDateTime;
  Result.DueDate:=FieldByName('DUEDATE').AsDateTime;
  Result.PlanTime := FieldByName('PLANTIME').AsFloat;
end;
procedure TTaskList.DisableDS;
begin
  FDS.Enabled:=False;
  FDS.DataSet := nil;
end;

procedure TTaskList.DataSetAfterCancel(aDataSet: TDataSet);
begin
  FCheckedChanged:=False;
  FCompletedChanged:=False;
  FAddProjectOnPost:=False;
  FAddProjectOnPost:=false;
  FDueDateChanged:=False;
end;

procedure TTaskList.DataSetAfterPost(aDataSet: TDataSet);
var
  aParent: TTask;
  aProject: TProject;
  aUser: TUser;
begin
  if DoCheckTask then
    begin
      if not DataSet.FieldByName('PARENT').IsNull then
        begin
          aParent := TTask.CreateEx(Self,DataModule,Connection);
          aParent.Select(DataSet.FieldByName('PARENT').AsVariant);
          aParent.Open;
          if aParent.Count > 0 then
            aParent.CheckChilds;
          aParent.Free;
        end;
      CheckDependTasks;
      DoCheckTask := False;
    end;
  if FAddProjectOnPost then
    begin
      if (trim(FDS.DataSet.FieldByName('SUMMARY').AsString)<>'') and (DataSet.Tag<>111) then
        begin
          aProject := TProject.CreateEx(Self,Data,Connection);
          aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
          aProject.Open;
          if (aProject.Count>0) then
            begin
              History.AddItem(Self.DataSet,strProjectChanged,'',DataSet.FieldByName('PROJECT').AsString,DataSet,ACICON_EDITED);
              aProject.History.Open;
              aProject.History.AddItem(aProject.DataSet,Format(strTaskAdded,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',DataSet,ACICON_TASKADDED);
            end;
          aProject.Free;
          FAddProjectOnPost:=False;
        end;
    end;
  if FAddSummaryOnPost then
    begin
      DataSet.DisableControls;
      if not History.DataSet.Active then History.Open;
      History.AddItem(Self.DataSet,Format(strRenamed,[FDS.DataSet.FieldByName('SUMMARY').AsString]),'','',nil,ACICON_RENAMED);
      DataSet.EnableControls;
      FAddSummaryOnPost:=false;
    end;
  if FDueDateChanged then
    begin
      if (FieldByName('COMPLETED').AsString<>'Y') then
        begin
          DataSet.DisableControls;
          if not History.DataSet.Active then History.Open;
          if FDS.DataSet.FieldByName('DUEDATE').AsDateTime>0 then
            History.AddItem(Self.DataSet,Format(strDueDateChanged,[DateToStr(trunc(FDS.DataSet.FieldByName('DUEDATE').AsDateTime))]),'','',nil,ACICON_DATECHANGED)
          else
            History.AddItem(Self.DataSet,strDueDateDeleted,'','',nil,ACICON_DATECHANGED);
          if (DataSet.FieldByName('CLASS').AsString = 'M') then
            begin
              aProject := TProject.CreateEx(Self,Data,Connection);
              aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
              aProject.Open;
              if aProject.Count>0 then
                begin
                  aProject.History.Open;
                  if FDS.DataSet.FieldByName('DUEDATE').AsDateTime>0 then
                    aProject.History.AddItem(aProject.DataSet,Format(strDueDateChanged,[DateToStr(trunc(FDS.DataSet.FieldByName('DUEDATE').AsDateTime))]),Data.BuildLink(aProject.DataSet),FDS.DataSet.FieldByName('SUMMARY').AsString,nil,ACICON_DATECHANGED)
                  else
                    aProject.History.AddItem(aProject.DataSet,strDueDateDeleted,Data.BuildLink(aProject.DataSet),FDS.DataSet.FieldByName('SUMMARY').AsString,nil,ACICON_DATECHANGED);
                end;
              aProject.Free;
            end;
          if not FDS.DataSet.FieldByName('DUEDATE').IsNull then
            MoveDependTasks;
          DataSet.EnableControls;
        end;
      FDueDateChanged:=False;
    end;
  if FStartDateChanged then
    begin
      if not (DataSet.FieldByName('DUEDATE').AsString='') then
        begin
          if ((DataSet.FieldByName('DUEDATE').AsDateTime-StrToFloatDef(DataSet.FieldByName('PLANTIME').AsString,0)) < DataSet.FieldByName('STARTDATE').AsDateTime) then
            begin
              DataSet.DisableControls;
              if not Canedit then DataSet.Edit;
                DataSet.FieldByName('DUEDATE').AsDateTime := DataSet.FieldByName('STARTDATE').AsDateTime+Max(StrToFloatDef(DataSet.FieldByName('PLANTIME').AsString,0),1);
              DataSet.EnableControls;
            end;
        end;
      FStartDateChanged:=False;
    end;
  FCheckedChanged:=False;
  FCompletedChanged:=False;
  FAddProjectOnPost:=False;
  FAddProjectOnPost:=false;
  FDueDateChanged:=False;
end;
procedure TTaskList.DataSetBeforeDelete(aDataSet: TDataSet);
var
  aParent: TTask;
  aTasks: TTaskList;
  Clean: Boolean;
  i: Integer;
  aProject: TProject;
  aDeps: TDependencies;
  aTask: TTask;
begin
  if aDataSet.ControlsDisabled then exit;
  if trim(FDS.DataSet.FieldByName('SUMMARY').AsString)<>'' then
    begin
      //debugln('TasksBeforeDelete:'+FDS.DataSet.FieldByName('SUMMARY').AsString);
      aProject := TProject.CreateEx(Self,Data,Connection);
      aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
      aProject.Open;
      if (aProject.Count>0) then
        begin
          aProject.History.Open;
          aProject.History.AddItem(aProject.DataSet,Format(strTaskDeleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',DataSet,ACICON_TASKCLOSED);
        end;
      aProject.Free;
    end;
  if  (Data.TriggerExists('TASKS_DEL_CHILD')) then
    begin
      //debugln('TasksBeforeDelete:using Trigger');
      exit;
    end;
  aParent := TTask.CreateEx(Self,DataModule,Connection);
  aParent.Select(DataSet.FieldByName('PARENT').AsVariant);
  aParent.Open;
  if aParent.Count > 0 then
    begin
      aTasks := TTaskList.CreateEx(Self,DataModule,Connection);
      aTasks.SelectByParent(aParent.Id.AsVariant);
      aTasks.Open;
      if (aTasks.Count = 1)
      and (aTasks.Id.AsVariant = Self.Id.AsVariant) then
        begin
          aParent.DataSet.Edit;
          aParent.FieldByName('HASCHILDS').AsString:='N';
          aParent.DataSet.Post;
        end;
      Clean := True;
      for i := 0 to aTasks.Count-1 do
        begin
          if (aTasks.FieldByName('CHECKED').AsString = 'N') and (aTasks.Id.AsVariant <> Self.Id.AsVariant) then
            Clean := False;
          aTasks.Next;
        end;
      if Clean then
        begin
          aParent.DataSet.Edit;
          aParent.FieldByName('CHECKED').AsString:='Y';
          aParent.DataSet.Post;
        end;
      aTasks.Free;
    end;
  aParent.Free;
  //Delete dependencies that points on me
  aDeps := TDependencies.CreateEx(nil,DataModule);
  aDeps.SelectByLink(Data.BuildLink(aDataSet));
  aDeps.Open;
  Dependencies.Open;
  while aDeps.Count>0 do
    begin
      //connect next and previous Tasks dependencies
      Dependencies.First;
      while not Dependencies.EOF do
        begin
          aTask := TTask.CreateEx(nil,DataModule,Connection);
          aTask.Select(aDeps.FieldByName('REF_ID').AsVariant);
          aTask.Open;
          if aTask.Count>0 then
            aTask.Dependencies.Add(Dependencies.FieldByName('LINK').AsString);
          aTask.Free;
          Dependencies.Next;
        end;
      aDeps.Delete;
    end;
  aDeps.Free;
  while Dependencies.Count>0 do
    Dependencies.Delete;
end;

procedure TTaskList.DataSetBeforePost(aDataSet: TDataSet);
var
  aParent: TTask;
  aProject: TProject;
  Informed1: String;
  aUser: TUser;
  Informed2: String;
  aDeps: TDependencies;
begin
  if FCompletedChanged then
    begin
      DataSet.DisableControls;
      if FieldByName('COMPLETED').AsString='Y' then
        begin
          DataSet.FieldByName('PERCENT').AsInteger:=100;
          DataSet.FieldByName('COMPLETEDAT').AsDateTime:=Now();
          if not History.DataSet.Active then History.Open;
          History.AddItem(Self.DataSet,strTaskCompleted,Data.BuildLink(FDS.DataSet),'',nil,ACICON_STATUSCH);
          aProject := TProject.CreateEx(Self,Data,Connection);
          aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
          aProject.Open;
          if DataSet.FieldByName('USER').AsString<>DataSet.FieldByName('OWNER').AsString then
            begin
              Informed1 := DataSet.FieldByName('OWNER').AsString;
              aUser := TUser.CreateEx(Self,Data,Connection);
              aUser.SelectByAccountno(DataSet.FieldByName('OWNER').AsString);
              aUser.Open;
              if aUser.Count>0 then
                begin
                  if Assigned(aProject) then
                    aUser.History.AddItem(aProject.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKCLOSED)
                  else
                    aUser.History.AddItem(Self.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKCLOSED);
                end;
              aUser.Free;
            end;
          if aProject.Count>0 then
            begin
              aProject.History.Open;
              aProject.History.AddItem(aProject.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKCLOSED);
              Informed2 := Data.Users.GetLeaderAccountno;
              if (aProject.FieldByName('INFORMLEADER').AsString='Y') and (Informed1 <> Informed2) then
                begin //Inform Leader of
                  aUser := TUser.CreateEx(Self,Data,Connection);
                  aUser.SelectByAccountno(Informed2);
                  aUser.Open;
                  if aUser.Count>0 then
                    begin
                      if Assigned(aProject) then
                        aUser.History.AddItem(aProject.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKCLOSED)
                      else
                        aUser.History.AddItem(Self.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKCLOSED);
                    end;
                  aUser.Free;
                end
              else Informed2 := '';
              if (aProject.FieldByName('INFORMPMANAGER').AsString='Y') and (Informed2<>aProject.FieldByName('PMANAGER').AsString) and (Informed1<>aProject.FieldByName('PMANAGER').AsString) then
                begin //Inform Leader of
                  aUser := TUser.CreateEx(Self,Data,Connection);
                  aUser.SelectByAccountno(aProject.FieldByName('PMANAGER').AsString);
                  aUser.Open;
                  if aUser.Count>0 then
                    begin
                      if Assigned(aProject) then
                        aUser.History.AddItem(aProject.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKCLOSED)
                      else
                        aUser.History.AddItem(Self.DataSet,Format(strTaskSCompleted,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKCLOSED);
                    end;
                  aUser.Free;
                end;
            end;
          aProject.Free;
        end
      else if (FieldByName('COMPLETED').AsString='N') and (DataSet.State <> dsInsert) then
        begin
          if DataSet.FieldByName('PERCENT').AsInteger = 100 then
            DataSet.FieldByName('PERCENT').AsInteger:=0;
          DataSet.FieldByName('COMPLETEDAT').Clear;
          DataSet.FieldByName('CHECKED').AsString:='N';
          DataSet.FieldByName('DUEDATE').Clear;
          DataSet.FieldByName('STARTDATE').Clear;
          DataSet.FieldByName('BUFFERTIME').Clear;
          DataSet.FieldByName('PLANTIME').Clear;
          if not History.DataSet.Active then History.Open;
          History.AddItem(Self.DataSet,strTaskReopened,Data.BuildLink(FDS.DataSet),'',nil,ACICON_STATUSCH);
          aProject := TProject.CreateEx(Self,Data,Connection);
          aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
          aProject.Open;
          if aProject.Count>0 then
            begin
              aProject.History.Open;
              aProject.History.AddItem(aProject.DataSet,Format(strTaskSReopened,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKADDED);
              Informed2 := Data.Users.GetLeaderAccountno;
              if (aProject.FieldByName('INFORMLEADER').AsString='Y') then
                begin //Inform Leader of
                  aUser := TUser.CreateEx(Self,Data,Connection);
                  aUser.SelectByAccountno(Informed2);
                  aUser.Open;
                  if aUser.Count>0 then
                    begin
                      if Assigned(aProject) then
                        aUser.History.AddItem(aProject.DataSet,Format(strTaskSReopened,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKADDED)
                      else
                        aUser.History.AddItem(Self.DataSet,Format(strTaskSReopened,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKADDED);
                    end;
                  aUser.Free;
                end;
              if (aProject.FieldByName('INFORMPMANAGER').AsString='Y') and (Informed2<>aProject.FieldByName('PMANAGER').AsString) then
                begin //Inform Leader of
                  aUser := TUser.CreateEx(Self,Data,Connection);
                  aUser.SelectByAccountno(aProject.FieldByName('PMANAGER').AsString);
                  aUser.Open;
                  if aUser.Count>0 then
                    begin
                      if Assigned(aProject) then
                        aUser.History.AddItem(aProject.DataSet,Format(strTaskSReopened,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKADDED)
                      else
                        aUser.History.AddItem(Self.DataSet,Format(strTaskSReopened,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKADDED);
                    end;
                  aUser.Free;
                end;
            end;
          aProject.Free;
          aDeps := TDependencies.CreateEx(nil,DataModule);
          aDeps.SelectByLink(Data.BuildLink(DataSet));
          aDeps.Open;
          aDeps.First;
          while not aDeps.EOF do
            begin
              aParent := TTask.CreateEx(nil,DataModule);
              aParent.Select(aDeps.FieldByName('REF_ID').AsVariant);
              aParent.Open;
              if (aParent.Count>0) and (aParent.FieldByName('COMPLETED').AsString = 'Y') then
                begin
                  aParent.Edit;
                  aParent.FieldByName('COMPLETED').AsString:='N';
                  aParent.Post;
                end;
              aParent.Free;
              aDeps.Next;
            end;
          aDeps.Free;
        end;
      //Check Parent Task
      DataSet.EnableControls;
    end;
  if FCheckedChanged then
    begin
      FCheckedChanged:=False;
      DataSet.DisableControls;
      if not History.DataSet.Active then History.Open;
      History.AddItem(Self.DataSet,strTaskChecked,Data.BuildLink(FDS.DataSet),'',nil,ACICON_STATUSCH);
      if DataSet.FieldByName('USER').AsString<>DataSet.FieldByName('OWNER').AsString then
        begin
          aUser := TUser.CreateEx(Self,Data,Connection);
          aUser.SelectByAccountno(DataSet.FieldByName('USER').AsString);
          aUser.Open;
          if aUser.Count>0 then
            aUser.History.AddItem(Self.DataSet,Format(strTaskSChecked,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKCLOSED);
          aUser.Free;
        end;
      DataSet.EnableControls;
    end;
  FCompletedChanged:=False;
end;

procedure TTaskList.FDSDataChange(Sender: TObject; Field: TField);
var
  aParent: TTask;
  aProject: TProject;
  aUser: TUser;
  Informed1: String;
  Informed2: String;
  aDeps: TDependencies;
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then
    exit;
  if Field.FieldName='COMPLETED' then
    begin
      FCompletedChanged := True;
      DoCheckTask := True;
    end
  else if (Field.FieldName='CHECKED') and (Field.AsString='Y') then
    begin
      FCheckedChanged := True;
    end
  else if (Field.FieldName='SUMMARY') then
    begin
      FAddSummaryOnPost:=True;
    end
  else if (Field.FieldName='PROJECT') and (DataSet.State <> dsInsert) then
    begin
      DataSet.DisableControls;
      if not History.DataSet.Active then History.Open;
      aProject := TProject.CreateEx(Self,Data,Connection);
      aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
      aProject.Open;
      if aProject.Count>0 then
        begin
          aProject.History.Open;
          if (FDS.DataSet.FieldByName('SUMMARY').AsString<>'') and (DataSet.Tag<>111) then
            aProject.History.AddItem(aProject.DataSet,Format(strTaskAdded,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKADDED);
          History.AddItem(Self.DataSet,strProjectChanged,Data.BuildLink(aProject.DataSet),Field.AsString,aProject.DataSet,ACICON_EDITED);
        end;
      aProject.Free;
      DataSet.FieldByName('SEEN').AsString:='N';
      DataSet.EnableControls;
    end
  else if (Field.FieldName='PROJECT') and (DataSet.State = dsInsert) then
    begin
      FAddProjectOnPost := true;
    end
  else if (Field.FieldName='USER') then
    begin
      DataSet.DisableControls;
      DataSet.FieldByName('SEEN').AsString:='N';
      if (DataSet.FieldByName('USER').AsString<>DataSet.FieldByName('OWNER').AsString) and (FDS.DataSet.FieldByName('SUMMARY').AsString<>'') and (DataSet.Tag<>111) then
        begin
          aUser := TUser.CreateEx(Self,Data,Connection);
          aUser.SelectByAccountno(DataSet.FieldByName('USER').AsString);
          aUser.Open;
          if aUser.Count>0 then
            begin
              if not History.DataSet.Active then History.Open;
              History.AddItem(Self.DataSet,Format(strDelegated,[aUser.Text.AsString]),'','',nil,ACICON_TASKADDED,'',False);
              aProject := TProject.CreateEx(Self,Data,Connection);
              aProject.Select(FDS.DataSet.FieldByName('PROJECTID').AsVariant);
              aProject.Open;
              if aProject.Count>0 then
                aUser.History.AddItem(aProject.DataSet,Format(strTaskUDelegated,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',aProject.DataSet,ACICON_TASKADDED,'',False)
              else
                aUser.History.AddItem(Self.DataSet,Format(strTaskUDelegated,[FDS.DataSet.FieldByName('SUMMARY').AsString]),Data.BuildLink(FDS.DataSet),'',nil,ACICON_TASKADDED,'',False);
              aProject.Free;
            end;
          aUser.Free;
        end;
      DataSet.EnableControls;
    end
  else if (Field.FieldName='DUEDATE') then
    begin
      DataSet.DisableControls;
      FDueDateChanged := True;
      DataSet.FieldByName('SEEN').AsString:='N';
      DataSet.EnableControls;
    end
  else if (Field.FieldName='STARTDATE') then
    begin
      FStartDateChanged := True;
    end
  else if (Field.FieldName <> 'SEEN') and (Field.FieldName <> 'TIMESTAMPD') and (Field.FieldName <> 'CHANGEDBY') then
    begin
      DataSet.DisableControls;
      DataSet.FieldByName('SEEN').AsString:='N';
      DataSet.EnableControls;
    end;
end;
function TTaskList.GetownerName: string;
begin
  Result := '';
  if not FTempUsers.DataSet.Active then FTempUsers.Open;
  if FTempUsers.DataSet.Locate('ACCOUNTNO',DataSet.FieldByName('OWNER').AsString,[]) then
    Result := FTempUsers.FieldByName('NAME').AsString;
end;

function TTaskList.GetProject: TField;
begin
  result := FieldByName('PROJECT');
end;

function TTaskList.GetUserName: string;
begin
  Result := '';
  if not FTempUsers.DataSet.Active then FTempUsers.Open;
  if FTempUsers.DataSet.Locate('ACCOUNTNO',DataSet.FieldByName('USER').AsString,[]) then
    Result := FTempUsers.FieldByName('NAME').AsString;
end;
function TTaskList.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

procedure TTaskList.OpenItem(AccHistory: Boolean);
begin
  //dont add Element
end;

procedure TTaskList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TASKS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('COMPLETED',ftString,1,True);
            Add('ACTIVE',ftString,1,True);
            Add('NEEDSACTION',ftString,1,False);
            Add('CHECKED',ftString,1,True);
            Add('HASCHILDS',ftString,1,True);
            Add('SEEN',ftString,1,False);
            Add('SUMMARY',ftString,220,False);
            Add('GPRIORITY',ftLargeint,0,False);
            Add('LPRIORITY',ftInteger,0,False);
            Add('PLANTIME',ftFloat,0,False); //geplante Zeit
            Add('TIME',ftFloat,0,False);     //benötigte Zeit
            Add('PLANTASK',ftString,1,False);
            Add('BUFFERTIME',ftFloat,0,False);//Wartezeit (wann darf nächste Aufgabe frühestens starten)
            Add('CATEGORY',ftString,60,False);
            Add('PERCENT',ftInteger,0,False);
            Add('OWNER',ftString,20,True);
            Add('USER',ftString,20,False);
            Add('PARENT',ftLargeInt,0,False);
            Add('LPARENT',ftLargeInt,0,False);
            Add('PROJECTID',ftLargeInt,0,False);
            Add('PROJECT',ftString,260,False);
            Add('LINK',ftString,400,False);
            Add('ORIGID',ftLargeInt,0,False);
            Add('ORIGIDS',ftString,200,False);
            Add('CLASS',ftString,1,True);
            Add('DEPDONE',ftString,1,True);
            Add('PRIORITY',ftString,1,True);
            Add('STARTDATE',ftDateTime,0,False);
            Add('DUEDATE',ftDateTime,0,False);
            Add('EARLIEST',ftDateTime,0,False);
            Add('PLANED',ftDateTime,0,False);
            Add('UNPLANNED',ftString,1,False);
            Add('WORKSTATUS',ftString,4,False);
            Add('ORDERNO',ftString,20,False);
            Add('STARTEDAT',ftDateTime,0,False);
            Add('COMPLETEDAT',ftDateTime,0,False);
            Add('DESC',ftMemo,0,False);
            Add('CREATEDBY',ftString,4,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('COMPLETED','COMPLETED',[]);
            Add('ACTIVE','ACTIVE',[]);
            Add('CHECKED','CHECKED',[]);
            Add('OWNER','OWNER',[]);
            Add('USER','USER',[]);
            Add('PARENT','PARENT',[]);
            Add('LPARENT','LPARENT',[]);
            Add('PROJECTID','PROJECTID',[]);
            Add('CLASS','CLASS',[]);
            Add('WORKSTATUS','WORKSTATUS',[]);
            Add('STARTDATE','STARTDATE',[]);
            Add('DUEDATE','DUEDATE',[]);
          end;
      UpdateChangedBy:=False;
    end;
end;
procedure TTaskList.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDbInterface do
    begin
      DataSet.DisableControls;
      FieldByName('ACTIVE').AsString := 'Y';
      FieldByName('COMPLETED').AsString := 'N';
      FieldByName('DEPDONE').AsString := 'Y';
      FieldByName('CHECKED').AsString := 'N';
      FieldByName('HASCHILDS').AsString := 'N';
      FieldByName('CLASS').AsString := 'T';
      FieldByName('PERCENT').AsInteger := 0;
      FieldByName('PARENT').AsInteger := 0;
      FieldByName('PRIORITY').AsString := '0';
      if FieldByName('OWNER').IsNull then
        FieldByName('OWNER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
      FieldByName('GPRIORITY').AsString := '999999';
      if (not (Self is TProjectTasks)) then
        begin
          if FUserID = '' then
            FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString
          else
            FieldByName('USER').AsString := FUserID;
        end;
      DataSet.EnableControls;
    end;
end;

procedure TTaskList.SelectActiveByUser(AccountNo: string);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('USER')+'='+QuoteValue(AccountNo)+') and ('+QuoteField('COMPLETED')+'='+QuoteValue('N')+') and ('+QuoteField('ACTIVE')+'='+QuoteValue('Y')+') and ('+QuoteField('DEPDONE')+'='+QuoteValue('Y')+')';
    end;
end;

procedure TTaskList.SelectActive;
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('COMPLETED')+'='+QuoteValue('N')+') and ('+QuoteField('ACTIVE')+'='+QuoteValue('Y')+') and ('+QuoteField('DEPDONE')+'='+QuoteValue('Y')+')';
      SortFields:='SQL_ID';
      SortDirection:=sdAscending;
    end;
end;

procedure TTaskList.SelectActivewithoutDeps;
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('COMPLETED')+'='+QuoteValue('N')+') and ('+QuoteField('ACTIVE')+'='+QuoteValue('Y')+')';
      SortFields:='SQL_ID';
      SortDirection:=sdAscending;
    end;
end;

procedure TTaskList.SelectByUser(AccountNo: string);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('USER')+'='+QuoteValue(AccountNo)+') or ('+QuoteField('OWNER')+'='+QuoteValue(AccountNo)+')';
    end;
end;

procedure TTaskList.SelectUncompletedByUser(AccountNo: string);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('USER')+'='+QuoteValue(AccountNo)+') and ('+QuoteField('COMPLETED')+'='+QuoteValue('N')+') and ('+QuoteField('ACTIVE')+'='+QuoteValue('Y')+')';
    end;
end;

procedure TTaskList.SelectActiveByUserChangedSince(AccountNo: string; aDate: TdateTime
  );
begin
  SelectChangedSince(aDate);
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := Filter+' AND (('+QuoteField('USER')+'='+QuoteValue(AccountNo)+')) AND ('+QuoteField('COMPLETED')+'='+QuoteValue('N')+') and ('+QuoteField('ACTIVE')+'='+QuoteValue('Y')+') and ('+QuoteField('DEPDONE')+'='+QuoteValue('Y')+')';
    end;
end;

procedure TTaskList.SelectByDept(aDept: Variant);
var
  aUsers: TUser;
  aFilter: String;
  procedure RecourseUsers(aParent : Variant);
  var
    bUsers: TUser;
  begin
    bUsers := TUser.CreateEx(nil,DataModule);
    bUsers.SelectByParent(aParent);
    bUsers.Open;
    while not bUsers.EOF do
      begin
        RecourseUsers(bUsers.Id.AsVariant);
        with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
          begin
            with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
              Filter := Filter+' or (('+QuoteField('USER')+'='+QuoteValue(bUsers.FieldByName('ACCOUNTNO').AsString)+') and (('+Data.ProcessTerm(QuoteField('DUEDATE')+'='+QuoteValue(''))+') or ('+Data.ProcessTerm(QuoteField('PLANTIME')+'='+QuoteValue(''))+')))';
          end;
        bUsers.Next;
      end;
    bUsers.Free;
  end;

begin
  aUsers := TUser.CreateEx(nil,DataModule);
  aUsers.Select(aDept);
  aUsers.Open;
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('COMPLETED')+'<>'+QuoteValue('Y')+')';
      Filter := Filter+' and ('+QuoteField('PLANTASK')+'='+Data.ProcessTerm(QuoteValue('Y')+' or '+QuoteField('PLANTASK')+'='+Data.QuoteValue(''))+')';
      Filter := Filter+' and ('+QuoteField('ACTIVE')+'<>'+QuoteValue('N')+') and (';
      Filter := Filter+'('+QuoteField('USER')+'='+QuoteValue(aUsers.FieldByName('ACCOUNTNO').AsString)+')';
    end;
  RecourseUsers(aDept);
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := Filter+')';
      aFilter := Filter;
    end;
  aUsers.Free;
end;

procedure TTaskList.SelectByParent(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('PARENT')+'='+QuoteValue(aParent)+')';
    end;
end;
procedure TTaskList.SelectUncompletedByParent(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('PARENT')+'='+QuoteValue(aParent)+') AND ('+QuoteField('COMPLETED')+'='+QuoteValue('N')+')';
    end;
end;
constructor TTaskList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FAddProjectOnPost := false;
  FAddSummaryOnPost:=false;
  FDueDateChanged:=False;
  FStartDateChanged := False;
  DoCheckTask:=False;
  FCheckedChanged:=False;
  FCompletedChanged:=False;
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
  DataSet.AfterPost:=@DataSetAfterPost;
  DataSet.BeforeDelete:=@DataSetBeforeDelete;
  DataSet.AfterCancel:=@DataSetAfterCancel;
  DataSet.BeforePost:=@DataSetBeforePost;
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          SortFields:='SQL_ID';
          BaseSortFields:='SQL_ID';
          BaseSortDirection:=sdAscending;
          Limit := 500;
        end;
      if Assigned(Data.Users) and Data.Users.Active then
        SelectActiveByUser(data.Users.FieldByName('ACCOUNTNO').AsString);
    end;
  FTempUsers := TUser.CreateEx(aOwner,DM);
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FSnapshots := TTaskSnapshots.CreateEx(Self,DM,aConnection,DataSet);
  FDependencies := TDependencies.CreateEx(Self,DM,aConnection,DataSet);
  FDependencies.FTask:=Self;
end;

destructor TTaskList.Destroy;
begin
  FDependencies.Free;
  FSnapshots.Free;
  FDS.Free;
  FHistory.Free;
  FTempUsers.Free;
  inherited Destroy;
end;
procedure TTaskList.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(aDataSet,'STARTDATE',strStart);
  SetDisplayLabelName(aDataSet,'PLANTIME',strPlantime);
  SetDisplayLabelName(aDataSet,'TIME',strActtime);
  SetDisplayLabelName(aDataSet,'COMPLETEDAT',strCompletedAt);
  SetDisplayLabelName(aDataSet,'COMPLETED',strCompleted);
  SetDisplayLabelName(aDataSet,'STARTEDAT',strStarted);
  SetDisplayLabelName(aDataSet,'TARGET',strStarted);
  SetDisplayLabelName(aDataSet,'HASCHILDS',strHasChilds);
  SetDisplayLabelName(aDataSet,'BUFFERTIME',strBuffertime);
  SetDisplayLabelName(aDataSet,'PERCENT',strPercentDone);
  SetDisplayLabelName(aDataSet,'USER',strWorker);
  SetDisplayLabelName(aDataSet,'OWNER',strResponsable);
  SetDisplayLabelName(aDataSet,'WORKSTATUS',strWorkstatus);
end;
function TTaskList.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FHistory.CreateTable;
  FSnapshots.CreateTable;
  FDependencies.CreateTable;
  try
    if Data.ShouldCheckTable('TASKS',False) then
      begin
        Data.CreateTrigger('DEL_CHILD','TASKS','DELETE',
         'UPDATE '+Data.QuoteField('TASKS')
        +' SET '+Data.QuoteField('HASCHILDS')+'=(CASE WHEN EXISTS(SELECT * FROM '+Data.QuoteField('TASKS')+' AS '+Data.QuoteField('PAR')+' WHERE '+Data.QuoteField('TASKS')+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteField('PAR')+'.'+Data.QuoteField('PARENT')+') THEN '+Data.QuoteValue('Y')+' ELSE '+Data.QuoteValue('N')+' END)'
        +' WHERE $OLD$.'+Data.QuoteField('PARENT')+'='+Data.QuoteField('SQL_ID')+';');

        Data.CreateTrigger('INS_CHILD','TASKS','UPDATE',
        'UPDATE '+Data.QuoteField('TASKS')
       +' SET '+Data.QuoteField('HASCHILDS')+'=(CASE WHEN EXISTS(SELECT * FROM '+Data.QuoteField('TASKS')+' AS '+Data.QuoteField('PAR')+' WHERE '+Data.QuoteField('TASKS')+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteField('PAR')+'.'+Data.QuoteField('PARENT')+') THEN '+Data.QuoteValue('Y')+' ELSE '+Data.QuoteValue('N')+' END)'
       +' WHERE $NEW$.'+Data.QuoteField('PARENT')+'='+Data.QuoteField('SQL_ID')+';');

        Data.CreateTrigger('INS_COMPL','TASKS','UPDATE',
        'UPDATE '+Data.QuoteField('TASKS')
       +' SET '+Data.QuoteField('COMPLETED')+'=(CASE WHEN EXISTS(SELECT * FROM '+Data.QuoteField('TASKS')+' AS '+Data.QuoteField('PAR')+' WHERE '+Data.QuoteField('TASKS')+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteField('PAR')+'.'+Data.QuoteField('PARENT')+' AND '+Data.QuoteField('COMPLETED')+'='+Data.QuoteValue('N')+') THEN '+Data.QuoteValue('N')+' ELSE '+Data.QuoteValue('Y')+' END)'
       +' WHERE $NEW$.'+Data.QuoteField('PARENT')+'='+Data.QuoteField('SQL_ID')+';');

        Data.CreateTrigger('INS_DEPEND','TASKS','UPDATE',
        ' UPDATE '+Data.QuoteField('TASKS')
       +' SET '+Data.QuoteField('DEPDONE')+'=(CASE WHEN EXISTS(SELECT * FROM '+Data.QuoteField('DEPENDENCIES')+' inner join '+Data.QuoteField('TASKS')+' as '+Data.QuoteField('TP')+' on '+Data.QuoteField('DEPENDENCIES')+'.'+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteField('TP')+'.'+Data.QuoteField('SQL_ID')+' and '+Data.QuoteField('COMPLETED')+'='+Data.QuoteValue('N')+' WHERE '+Data.QuoteField('TASKS')+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteField('DEPENDENCIES')+'.'+Data.QuoteField('REF_ID')+') THEN '+Data.QuoteValue('N')+' ELSE '+Data.QuoteValue('Y')+' END)'
       +' WHERE '+Data.QuoteField('COMPLETED')+'='+Data.QuoteValue('N')+' AND '+Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y')+';'+LineEnding,'COMPLETED');
      end;
  except
  end;
end;
procedure TTaskList.CascadicPost;
begin
  inherited CascadicPost;
  FHistory.CascadicPost;
end;

procedure TTaskList.CascadicCancel;
begin
  FHistory.CascadicCancel;
  inherited CascadicCancel;
end;
function TTaskList.GetTextFieldName: string;
begin
  Result := 'SUMMARY';
end;
function TTaskList.GetNumberFieldName: string;
begin
  Result := 'SQL_ID';
end;
end.

