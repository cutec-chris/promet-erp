{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uCalendar;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,uBaseDbClasses,uBaseDbInterface,db,uIntfStrConsts,variants;
type

  { TCalendar }

  TCalendar = class(TBaseDbList)
  protected
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure SelectPlanedByUser(AccountNo : string);
    procedure SelectPlanedByUserAndTime(AccountNo : string;aStart,aEnd : TDateTime);
    procedure SelectPlanedByUseridAndTime(User : Variant;aStart,aEnd : TDateTime);
  end;
  TEventLinks = class(TLinks)
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
  end;

  { TEvent }

  TEvent = class(TCalendar)
  private
    FLinks: TEventLinks;
    function GetEnd: TDateTime;
    function GetStart: TDateTime;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure SelectById(aID : Integer);overload;
    property Links : TEventLinks read FLinks;
    function GetTimeInRange(aStart,aEnd : TDateTime) : Extended;
    property StartDate : TDateTime read GetStart;
    property EndDate : TDateTime read GetEnd;
  end;
  function TimeRangeOverlap(Range1Start, Range1Finish, Range2Start, Range2Finish : TDateTime) : TDateTime;
implementation
uses uBaseApplication,uData,math;

function TEvent.GetEnd: TDateTime;
begin
  Result := FieldByName('ENDDATE').AsDateTime;
end;

function TEvent.GetStart: TDateTime;
begin
  Result := FieldByName('STARTDATE').AsDateTime;
end;

constructor TEvent.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FLinks := TEventLinks.Create(Self,DM,aConnection);
end;
destructor TEvent.Destroy;
begin
  FLinks.Free;
  inherited Destroy;
end;
procedure TEvent.SelectById(aID: Integer);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(IntToStr(aID)));
        end;
    end;
end;
procedure TEventLinks.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('REF_ID').AsVariant:=(Parent as TEvent).Id.AsVariant;
end;
function TCalendar.GetTextFieldName: string;
begin
  Result:='SUMMARY';
end;
function TCalendar.GetNumberFieldName: string;
begin
  Result:='ID';
end;
procedure TCalendar.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CALENDAR';
      TableCaption := strCalendar;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID_ID',ftLargeInt,0,True);
            Add('ID',ftLargeint,0,True);
            Add('CLASS',ftString,1,False); //classification
            Add('SUMMARY',ftString,120,False);
            Add('PROJECT',ftString,260,False);
            Add('LOCATION',ftString,30,False);
            Add('CALENDART',ftString,15,False);
            Add('ICATEGORY',ftInteger,0,False);
            Add('CATEGORY',ftString,30,False);
            Add('STARTDATE',ftDateTime,0,False);
            Add('ENDDATE',ftDateTime,0,False);
            Add('ALLDAY',ftString,1,false);
            Add('ALARM',ftString,1,false);
            Add('ALARMADV',ftSmallInt,0,false);
            Add('ALARMADVTP',ftSmallInt,0,false);
            Add('DESCR',ftMemo,0,False);
            Add('SNOOZE',ftFloat,0,false);    //SnoozeTime
            Add('ROTATION',ftSmallInt,0,false);
            Add('ROTTO',ftDate,0,false);     //Rotation to
            Add('ROTCUS',ftInteger,0,false);  //Rotation Custom Interval
            Add('CRDATE',ftDate,0,False);
            Add('CHDATE',ftDate,0,False);
            Add('CREATEDBY',ftString,4,False);
            Add('CHANGEDBY',ftString,4,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('REF_ID_ID','REF_ID_ID',[]);
          end;
    end;
end;

procedure TCalendar.SelectPlanedByUser(AccountNo: string);
var
  aUser: TUser;
begin
  aUser := TUser.Create(nil,DataModule);
  aUser.SelectByAccountno(AccountNo);
  aUser.Open;
  if aUser.Count>0 then
    begin
      with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
        begin
          Filter := '('+QuoteField('REF_ID_ID')+'='+QuoteValue(aUser.Id.AsString)+') and ('+QuoteField('ICATEGORY')+'='+QuoteValue('8')+')';
        end;
    end
  else
    begin
      with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
        Filter := QuoteField('REF_ID_ID')+'='+QuoteValue('0');
    end;
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      SortFields:='STARTDATE';
      SortDirection:=sdDescending;
    end;
  aUser.Free;
end;

procedure TCalendar.SelectPlanedByUserAndTime(AccountNo: string; aStart,
  aEnd: TDateTime);
var
  aUser: TUser;
begin
  aUser := TUser.Create(nil,DataModule);
  aUser.SelectByAccountno(AccountNo);
  aUser.Open;
  if aUser.Count>0 then
    begin
      SelectPlanedByUseridAndTime(aUser.Id.AsVariant,aStart,aEnd);
    end
  else
    begin
      with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
        Filter := QuoteField('REF_ID_ID')+'='+QuoteValue('0');
    end;
  aUser.Free;
end;

procedure TCalendar.SelectPlanedByUseridAndTime(User: Variant; aStart,
  aEnd: TDateTime);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('REF_ID_ID')+'='+QuoteValue(User)+') and ('+QuoteField('ICATEGORY')+'='+QuoteValue('8')+')';
      Filter := Filter+' AND ('+Data.QuoteField('STARTDATE')+' >= '+Data.DateToFilter(aStart)+') AND (('+Data.QuoteField('ENDDATE')+' <= '+Data.DateToFilter(aEnd)+') OR ('+Data.QuoteField('ROTATION')+' > 0))';
    end;
end;

function TimeRangeOverlap(Range1Start, Range1Finish, Range2Start, Range2Finish : TDateTime) : TDateTime;
begin
  Result := Max(Min(Range1Finish, Range2Finish) - Max(Range1Start, Range2Start), 0);
end;

function TEvent.GetTimeInRange(aStart, aEnd: TDateTime): Extended;
begin
  Result := 0;
  Result := TimeRangeOverlap(StartDate,EndDate,aStart,aEnd);
  if (StartDate>=aStart) and (EndDate<=aEnd) and (FieldByName('ALLDAY').AsString='Y') then Result := 1;
end;

end.

