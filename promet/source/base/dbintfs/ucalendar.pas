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
unit uCalendar;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,uBaseDbClasses,uBaseDbInterface,db,uIntfStrConsts,variants;
type

  { TCalendar }

  TCalendar = class(TBaseDbList,IBaseHistory)
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FHistory: TBaseHistory;
    FDS: TDataSource;
    FRefID: Variant;
    function GetHistory: TBaseHistory;
  protected
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    procedure SetDisplayLabels(aDataSet : TDataSet);override;
    procedure SelectByUser(AccountNo : string);
    procedure SelectPlanedByUser(AccountNo : string);
    procedure SelectPlanedByUserAndTime(AccountNo : string;aStart,aEnd : TDateTime);
    procedure SelectPlanedByUseridAndTime(User : Variant;aStart,aEnd : TDateTime);
    function SelectFromLink(aLink: string): Boolean; override;
    property History : TBaseHistory read GetHistory;
    constructor Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    property RefId : Variant read FRefID write FRefID;
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
uses uBaseApplication,uData,math,uBaseERPDBClasses,Utils;

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

procedure TCalendar.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then
    exit;
  if (Field.FieldName='ENDDATE') then
    begin
      DataSet.DisableControls;
      if not History.DataSet.Active then History.Open;
      History.AddItem(Self.DataSet,Format(strDueDateChanged,[Field.AsString]),'','',nil,ACICON_DATECHANGED);
      DataSet.EnableControls;
    end
  else if (Field.FieldName='CATEGORY') then
    begin
      DataSet.DisableControls;
      if not History.DataSet.Active then History.Open;
      History.AddItem(Self.DataSet,Format(strCategoryChanged,[Field.AsString]),'','',nil,ACICON_ORERSTATUSCH);
      DataSet.EnableControls;
    end;
end;

function TCalendar.GetHistory: TBaseHistory;
begin
  Result := FHistory;
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
            Add('STATUS',ftString,4,False); //classification
            Add('SUMMARY',ftString,120,False);
            Add('PROJECT',ftString,260,False);
            Add('LOCATION',ftString,30,False);
            Add('CALENDART',ftString,15,False);
            Add('ICATEGORY',ftInteger,0,False);
            Add('CATEGORY',ftString,60,False);
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
            Add('ORIGID',ftString,200,False);
            Add('BUSYTYPE',ftString,2,False);
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

procedure TCalendar.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  FieldByName('REF_ID_ID').AsVariant:=RefId;
  FieldByName('ID').AsVariant:=Data.GetUniID;
end;

procedure TCalendar.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(aDataSet,'ENDDATE',strDue);
end;

procedure TCalendar.SelectByUser(AccountNo: string);
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
          Filter := '('+QuoteField('REF_ID_ID')+'='+QuoteValue(aUser.Id.AsString)+')';
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
      Filter := Filter+' AND (('+Data.QuoteField('STARTDATE')+' >= '+Data.DateToFilter(aStart)+') AND ('+Data.QuoteField('ENDDATE')+' <= '+Data.DateToFilter(aEnd)+')';
      Filter := Filter+' OR ('+Data.QuoteField('ENDDATE')+' >= '+Data.DateToFilter(aStart)+') AND ('+Data.QuoteField('ENDDATE')+' <= '+Data.DateToFilter(aEnd)+')';
      Filter := Filter+' OR ('+Data.QuoteField('STARTDATE')+' >= '+Data.DateToFilter(aStart)+') AND ('+Data.QuoteField('STARTDATE')+' <= '+Data.DateToFilter(aEnd)+')';
      Filter := Filter+' OR ('+Data.QuoteField('STARTDATE')+' < '+Data.DateToFilter(aStart)+') AND ('+Data.QuoteField('ENDDATE')+' > '+Data.DateToFilter(aEnd)+'))';
      Filter := Filter+' OR (('+Data.QuoteField('ROTATION')+' > 0) AND ('+Data.QuoteField('STARTDATE')+' <= '+Data.DateToFilter(aStart)+') AND ('+Data.QuoteField('ROTTO')+' >= '+Data.DateToFilter(aEnd)+'))';
    end;
end;

function TCalendar.SelectFromLink(aLink: string): Boolean;
begin
  Result := False;
  Select(0);
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))));
          Result := True;
        end;
    end;
end;

constructor TCalendar.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FHistory := TBaseHistory.Create(Self,DM,aConnection,DataSet);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;

destructor TCalendar.Destroy;
begin
  FDS.Free;
  FHistory.Free;
  inherited Destroy;
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

