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
*******************************************************************************}
unit uProjects;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDBClasses, db, uIntfStrConsts,
  uBaseERPDbClasses,uTask,Variants,uBaseDatasetInterfaces;
type
  { TProjectList }
  TProjectList = class(TBaseERPList)
  protected
    function GetDescriptionFieldName: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName: string;override;
    function GetStatusFieldName: string;override;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil;
      aMasterdata: TDataSet=nil); override;
    function GetTyp: string; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure SetDisplayLabels(aDataSet : TDataSet);override;
  published
    constructor Create(aOwner : TComponent);override;
    function SelectFromLink(aLink: string) : Boolean; override;
    procedure SelectFromParent(aParent : Variant);virtual;
  end;
  TProjectHistory = class(TBaseHistory)
  end;
  TProjectLinks = class(TLinks)
  private
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure Open;override;
  end;
  TProjectTasks = class(TTaskList)
  private
    FProject: TProjectList;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure Open;override;
    property Project : TProjectList read FProject;
  end;
  TProjectPositions = class(TBaseDBPosition)
  private
    FProject: TProjectList;
    OldRealPosPrice : real;
  protected
    function GetCurrency : string;override;
    procedure PosPriceChanged(aPosDiff,aGrossDiff :Extended);override;
    procedure RealPriceChanged(aRealDiff :Extended);
    procedure PosWeightChanged(aPosDiff :Extended);override;
    procedure DoModifyPosPrice; override;
    procedure DoDataChange(Sender: TObject; Field: TField); override;
    procedure DoInsert; override;
    procedure DoDelete; override;
    procedure DoEdit; override;
    procedure DoBeforeDelete; override;
    procedure DoPost; override;
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure SetDisplayLabels(aDataSet: TDataSet); override;
    property Project : TProjectList read FProject;
  end;
  TProjectQuestionEvent = function(Sender : TProjectList) : Boolean;
  TProject = class(TProjectList,IBaseHistory,IBaseStructure)
    procedure DataSetBeforePost(aDataSet: TDataSet);
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    PrioChanged,IDChanged : Boolean;
    FHistory: TProjectHistory;
    FImages: TImages;
    FLinks: TProjectLinks;
    FMeasurement: TMeasurement;
    FPositions: TProjectPositions;
    FStateChange: TNotifyEvent;
    FTasks: TProjectTasks;
    FDS : TDataSource;
    FStatus : string;
    FTarget: String;
    function GetHistory: TBaseHistory;
    function GetParentField: string;
    function GetStructureElements(aIndex : Integer) : TBaseDbDataSet;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure Open; override;
    procedure Recalculate;
    procedure Reorganize;
    function CreateTable : Boolean;override;
    procedure FillDefaults(aDataSet : TDataSet);override;
  published
    procedure CascadicPost;override;
    procedure CascadicCancel;override;
    property History : TProjectHistory read FHistory;
    property Images : TImages read FImages;
    property Links : TProjectLinks read FLinks;
    property Tasks : TProjectTasks read FTasks;
    property Positions : TProjectPositions read FPositions;
    property Measurements : TMeasurement read FMeasurement;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
    procedure Makesnapshot(aName : string);
    function Delete : Boolean; override;
    procedure GenerateThumbnail; override;
    procedure CheckNeedsAction;
    function DuplicateFromOtherProcess(bProject: TProject): Boolean;
  end;
implementation
uses uBaseSearch,uBaseApplication,Utils,uthumbnails,uData;
resourcestring
  strTargetChanged              = 'Zieldatum geändert von %s zu %s';
  strManagerChanged             = 'Projektleiter geändert zu %s';
  strPlanedPrice                = 'Planpreis';
  strRealPrice                  = 'Echtpreis';
  strWholeRealPrice             = 'Echtpreis gesamt';
  strInvoiceDate                = 'Rechnungsdatum';
function TProjectPositions.GetCurrency: string;
begin
  Result := Project.FieldByName('CURRENCY').AsString;
end;
procedure TProjectPositions.PosPriceChanged(aPosDiff, aGrossDiff: Extended);
begin
  if not ((Project.DataSet.State = dsEdit) or (Project.DataSet.State = dsInsert)) then
    Project.DataSet.Edit;
  Project.FieldByName('TARGETCOSTS').AsFloat := Project.FieldByName('TARGETCOSTS').AsFloat+aPosDiff;
end;
procedure TProjectPositions.RealPriceChanged(aRealDiff: Extended);
begin
  if not ((Project.DataSet.State = dsEdit) or (Project.DataSet.State = dsInsert)) then
    Project.DataSet.Edit;
  Project.FieldByName('COSTS').AsFloat := Project.FieldByName('COSTS').AsFloat+aRealDiff;
end;
procedure TProjectPositions.PosWeightChanged(aPosDiff: Extended);
begin
end;
procedure TProjectPositions.DoModifyPosPrice;
var
  tmp: Extended;
begin
  inherited DoModifyPosPrice;
  if (not DataSet.FieldByName('QUANTITY').IsNull) and (not TryStrToFloat(DataSet.FieldByName('QUANTITY').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('REALPRICE').IsNull) and (not TryStrToFloat(DataSet.FieldByName('REALPRICE').AsString,tmp)) then exit;
  DisableCalculation;
  try
    //Menge
    tmp := InternalRound(DataSet.FieldByName('REALPRICE').AsFloat*DataSet.FieldByName('QUANTITY').AsFloat);
    if not ((State = dsInsert) or (State = dsEdit)) then
      DataSet.Edit;
    if (GetPosTypDec = 1)
    or (GetPosTypDec = 2) then
      DataSet.FieldByName('REALPOSPRICE').AsFloat := 0
    else
      DataSet.FieldByName('REALPOSPRICE').AsFloat := InternalRound(tmp);
  finally
    EnableCalculation;
  end;
end;

procedure TProjectPositions.DoDataChange(Sender: TObject; Field: TField);
begin
  inherited DoDataChange(Sender, Field);
  if (Field.FieldName = 'REALPRICE') then
    begin
      DoModifyPosPrice;
    end
end;

procedure TProjectPositions.DoInsert;
begin
  inherited DoInsert;
  OldRealPosPrice:=0;
end;
procedure TProjectPositions.DoDelete;
begin
  inherited DoDelete;
  if (OldRealPosPrice <> 0) then
    begin
      RealPriceChanged(-OldRealPosPrice);
    end;
end;
procedure TProjectPositions.DoEdit;
begin
  inherited DoEdit;
  OldRealPosPrice:=DataSet.FieldByName('REALPRICE').AsFloat;
end;

procedure TProjectPositions.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  OldRealPosPrice:=DataSet.FieldByName('REALPRICE').AsFloat;
end;
procedure TProjectPositions.DoPost;
begin
  inherited DoPost;
  if (OldRealPosPrice <> DataSet.FieldByName('REALPRICE').AsFloat) then
    begin
      RealPriceChanged(DataSet.FieldByName('REALPRICE').AsFloat-OldRealPosPrice);
    end;
end;
procedure TProjectPositions.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
end;
procedure TProjectPositions.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROJECTPOS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            property REALPRICE: double read  write ;
            property REALPOSPRICE: double read  write ;
            property WORKSTATUS: string index 4 read  write ;
            property ORIGDATE: TDateTime read  write ;
          end;
      if Data.ShouldCheckTable(TableName) then
        DefineUserFields(aDataSet);
    end;
end;
procedure TProjectPositions.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(aDataSet,'SELLPRICE',strPlanedPrice);
  SetDisplayLabelName(aDataSet,'REALPRICE',strRealPrice);
  SetDisplayLabelName(aDataSet,'REALPOSPRICE',strWholeRealPrice);
  SetDisplayLabelName(aDataSet,'ORIGDATE',strInvoiceDate);
end;

constructor TProjectTasks.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  ActualFilter:='';
end;

procedure TProjectTasks.FillDefaults(aDataSet: TDataSet);
var aID : Variant;
begin
  inherited FillDefaults(aDataSet);
  aID := (Parent as TProject).Id.AsVariant;
  aDataSet.FieldByName('PROJECTID').AsVariant:=aID;
  aDataSet.FieldByName('PROJECT').AsVariant:=TBaseDBModule(DataModule).GetLinkDesc(TBaseDBModule(DataModule).BuildLink((Parent as TProject).DataSet));
end;
procedure TProjectTasks.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      if (Parent as TProject).DataSet.State = dsInsert then
        begin
          (Parent as TProject).DataSet.Post;
          (Parent as TProject).DataSet.Edit;
        end;
      with DataSet as IBaseDBFilter do
        begin
        if Assigned(Parent) then
          begin
            if Filter <> '' then
              begin
                if not TBaseDBDataSet(Parent).Id.IsNull then
                  Filter := Filter+' AND '+Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(TBaseDBDataSet(Parent).Id.AsString)
                else
                  Filter := Filter+' AND '+Data.QuoteField('PROJECTID')+'= 0';
              end
            else
              begin
              if not TBaseDBDataSet(Parent).Id.IsNull then
                Filter := Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(TBaseDBDataSet(Parent).Id.AsString)
              else
                Filter := Data.QuoteField('PROJECTID')+'= 0';
              end;
            SortFields:='GPRIORITY';
            SortDirection:=sdAscending;
          end;
        end;
    end;
  inherited Open;
end;

procedure TProject.DataSetBeforePost(aDataSet: TDataSet);
begin
  if PrioChanged then
    begin
      History.AddItem(Self.DataSet,Format(strPriorityChanged,[FieldByName('GPRIORITY').AsString]),'','',DataSet,ACICON_EDITED);
    end
  else if IDChanged then
    begin
      History.AddItem(Self.DataSet,Format(strNumberChanged,[FieldByName('ID').AsString]),'','',DataSet,ACICON_EDITED);
    end;
end;

procedure TProject.FDSDataChange(Sender: TObject; Field: TField);
var
  aMakeInactive: Boolean;
  aUsers: TUser;
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if (Dataset.State <> dsInsert)
  and ((Field.FieldName = 'STATUS')
   or  (Field.FieldName = 'TARGET')
   or  (Field.FieldName = 'PMANAGER')
   or  (Field.FieldName = 'ID')
   or  (Field.FieldName = 'GPRIORITY')
      )
  then
    begin
      if not History.DataSet.Active then
        History.Open;
      if (Field.FieldName = 'STATUS') then
        begin
          if FStatus=Field.AsString then exit;
          History.AddItem(Self.DataSet,Format(strStatusChanged,[FStatus,Field.AsString]),'','',DataSet,ACICON_STATUSCH);
          FStatus := Field.AsString;
          if Assigned(FStateChange) then
            FStateChange(Self);
          Makesnapshot(FStatus+' '+DateToStr(Now()));
          OpenItem(False);
        end;
      if (Field.FieldName = 'ID') then
        begin
          IDChanged :=True;
        end;
      if (Field.FieldName = 'GPRIORITY') then
        begin
          PrioChanged :=True;
        end;
      if (Field.FieldName = 'TARGET') then
        begin
          History.AddItem(Self.DataSet,Format(strTargetChanged,[FTarget,Field.AsString]),'','',DataSet,ACICON_DATECHANGED);
          FTarget := Field.AsString;
        end;
      if (Field.FieldName = 'PMANAGER') then
        begin
          aUsers := TUser.CreateEx(nil,DataModule);
          aUsers.SelectByAccountno(Field.AsString);
          aUsers.Open;
          if aUsers.Count>0 then
            History.AddItem(Self.DataSet,Format(strManagerChanged,[aUsers.FieldByName('NAME').AsString]),'','',DataSet,ACICON_STATUSCH)
          else
            History.AddItem(Self.DataSet,Format(strManagerChanged,['']),'','',DataSet,ACICON_STATUSCH);
          aUsers.Free;
        end;
    end;
end;
function TProject.GetHistory: TBaseHistory;
begin
  Result := History;
end;

function TProject.GetParentField: string;
begin
  Result := 'PARENT';
end;

function TProject.GetStructureElements(aIndex: Integer): TBaseDbDataSet;
begin
  Result := nil;
end;

function TProjectList.GetTyp: string;
begin
  Result := 'P';
end;

constructor TProject.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UpdateFloatFields:=True;
          UsePermissions:=False;
        end;
    end;
  FHistory := TProjectHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FPositions := TProjectPositions.CreateEx(Self,DataModule,aConnection,DataSet);
  FPositions.FProject := Self;
  FImages := TImages.CreateEx(Self,DataModule,aConnection,DataSet);
  FLinks := TProjectLinks.CreateEx(Self,DataModule,aConnection);
  with Self.DataSet as IBaseSubDataSets do
    RegisterSubDataSet(FLinks);
  FTasks := TProjectTasks.CreateEx(Self,DataModule,aConnection);
  with Self.DataSet as IBaseSubDataSets do
    RegisterSubDataSet(FTasks);
  FTasks.FProject := Self;
  FMeasurement := TMeasurement.CreateEx(Self,DataModule,aConnection,DataSet);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
  DataSet.BeforePost:=@DataSetBeforePost;
  PrioChanged:=False;
  IDChanged:=False;
end;
destructor TProject.Destroy;
begin
  FDS.Free;
  FMeasurement.Destroy;
  FLinks.Destroy;
  FPositions.Destroy;
  FImages.Destroy;
  FHistory.Destroy;
  FTasks.Destroy;
  inherited Destroy;
end;
procedure TProject.Open;
begin
  inherited Open;
  FStatus := Status.AsString;
  FTarget := DataSet.FieldByName('TARGET').AsString;
  FTasks.Open;
end;

procedure TProject.Recalculate;
var
  aReal: Double = 0;
  aPos: Double = 0;
  aGrossPos: Double = 0;
begin
  Positions.Open;
  Positions.First;
  while not Positions.EOF do
    begin
      aReal += Positions.FieldByName('REALPOSPRICE').AsFloat;
      aPos += Positions.FieldByName('POSPRICE').AsFloat;
      aGrossPos += Positions.FieldByName('GROSSPRICE').AsFloat;
      Positions.Next;
    end;
  if not CanEdit then DataSet.Edit;
  FieldByName('COSTS').AsFloat:=aReal;
  FieldByName('TARGETCOSTS').AsFloat:=aPos;
  if CanEdit then DataSet.Post;
end;

procedure TProject.Reorganize;
begin
  Recalculate;
  Tasks.Open;
  while not Tasks.EOF do
    begin
      Tasks.CheckChilds;
      Tasks.FixDependencys;
      Tasks.Next;
    end;
end;

function TProject.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FHistory.CreateTable;
  FImages.CreateTable;
  FLinks.CreateTable;
  FTasks.CreateTable;
  FPositions.CreateTable;
end;
procedure TProject.CascadicPost;
begin
  Recalculate;
  inherited CascadicPost;
  FImages.CascadicPost;
  FLinks.CascadicPost;
  if FTasks.CanEdit then
    FTasks.Post;
  FHistory.CascadicPost;
  FPositions.CascadicPost;
end;
procedure TProject.CascadicCancel;
begin
  FHistory.CascadicCancel;
  FImages.CascadicCancel;
  FLinks.CascadicCancel;
  FTasks.CascadicCancel;
  FPositions.CascadicCancel;
  inherited CascadicCancel;
end;
procedure TProject.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      aDataSet.DisableControls;
      FieldByName('STATUS').AsString := 'A';
      FieldByName('TYPE').AsString := 'P';
      FieldByName('START').AsDateTime := Now();
      FieldByName('CREATEDAT').AsDateTime := Now();
      FieldByName('GPRIORITY').AsInteger := 10000;
      if Data.Numbers.HasNumberSet('PROJECTS') then
        if FieldByName('ID').IsNull then
          FieldByName('ID').AsString := Data.Numbers.GetNewNumber('PROJECTS');
      FieldByName('CREATEDBY').AsString := Data.Users.IDCode.AsString;
      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
      aDataSet.EnableControls;
    end;
end;

procedure TProject.Makesnapshot(aName: string);
begin
  Tasks.Open;
  Tasks.First;
  while not Tasks.EOF do
    begin
      Tasks.MakeSnapshot(aName);
      Tasks.Next;
    end;
end;

function TProject.Delete: Boolean;
begin
  Result := False;
  Tasks.DataSet.DisableControls;
  Tasks.Open;
  while Tasks.Count>0 do
    Tasks.Delete;
  Tasks.DataSet.EnableControls;
  Links.Open;
  while Links.Count>0 do
    Links.Delete;
  Positions.Open;
  while Positions.Count>0 do
    Positions.Delete;
  Result := inherited Delete;
end;

procedure TProject.GenerateThumbnail;
var
  aThumbnail: TThumbnails;
begin
  aThumbnail := TThumbnails.CreateEx(nil,DataModule);
  aThumbnail.CreateTable;
  aThumbnail.SelectByRefId(Self.Id.AsVariant);
  aThumbnail.Open;
  if aThumbnail.Count=0 then
    Images.GenerateThumbnail(aThumbnail);
  aThumbnail.Free;
end;

procedure TProject.CheckNeedsAction;
var
  aTasks: TTaskList;
begin
  aTasks := TTaskList.CreateEx(nil,DataModule,Connection);
  aTasks.Filter(TBaseDBModule(DataModule).QuoteField('PROJECTID')+'='+TBaseDBModule(DataModule).QuoteValue(Self.Id.AsString)+' AND '+TBaseDBModule(DataModule).QuoteField('NEEDSACTION')+'='+TBaseDBModule(DataModule).QuoteValue('Y'));
  Edit;
  if aTasks.Count>0 then
    FieldByName('NEEDSACTION').AsString:='Y'
  else FieldByName('NEEDSACTION').AsString:='N';
  Post;
end;

function TProject.DuplicateFromOtherProcess(bProject: TProject): Boolean;
var
  cProject: TProject;
  aTask: TTask;
  aLink: String;
  bTask: TTask;
begin
  bProject.Tasks.Open;
  bProject.Tasks.First;
  Tasks.ImportFromXML(bProject.Tasks.ExportToXML);
  Tasks.First;
  cProject := TProject.CreateEx(nil,DataModule);
  cProject.Select(Id.AsVariant);
  cProject.Open;
  cProject.Tasks.Open;
  while not Tasks.DataSet.EOF do
    begin
      if bProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS',VarArrayOf([Tasks.FieldByName('SUMMARY').AsString,Tasks.FieldByName('WORKSTATUS').AsVariant]),[]) then
        begin
          Tasks.Dependencies.Open;
          while Tasks.Dependencies.Count>0 do
            Tasks.Dependencies.Delete;
          aTask := TTask.CreateEx(nil,DataModule); //Old Task
          aTask.Select(bProject.Tasks.Id.AsVariant);
          aTask.Open;
          aTask.Dependencies.Open;
          with aTask.Dependencies.DataSet do
            begin
              First;
              while not EOF do
                begin
                  aLink := FieldByName('LINK').AsString;
                  if pos('{',aLink) > 0 then
                    aLink := copy(aLink,0,pos('{',aLink)-1);
                  aLink := copy(aLink,7,length(aLink));
                  if bProject.Tasks.GotoBookmark(StrToInt64Def(aLink,0)) then
                    begin
                      if cProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS;PARENT',VarArrayOf([bProject.Tasks.FieldByName('SUMMARY').AsString,bProject.Tasks.FieldByName('WORKSTATUS').AsVariant,bProject.Tasks.FieldByName('PARENT').AsVariant]),[]) then
                        begin
                          bTask := TTask.CreateEx(nil,DataModule); //New Task
                          bTask.Select(Tasks.Id.AsVariant);
                          bTask.Open;
                          bTask.Dependencies.Open;
                          while bTask.Dependencies.Locate('LINK',FieldByName('LINK').AsString,[]) do
                            bTask.Dependencies.Delete;
                          bTask.Dependencies.Add(TBaseDBModule(DataModule).BuildLink(cProject.Tasks.DataSet));
                          bTask.Free;
                        end;
                    end;
                  Next;
                end;
            end;
          aTask.Free;
        end;
      Tasks.DataSet.Next;
    end;
  Tasks.DataSet.First;
  while not Tasks.DataSet.EOF do
    begin
      if not Tasks.FieldByName('PARENT').IsNull then
        begin
          if bProject.Tasks.GotoBookmark(Tasks.FieldByName('PARENT').AsVariant) then
            if cProject.Tasks.DataSet.Locate('SUMMARY;WORKSTATUS',VarArrayOf([bProject.Tasks.FieldByName('SUMMARY').AsString,bProject.Tasks.FieldByName('WORKSTATUS').AsVariant]),[]) then
              begin
                if not Tasks.CanEdit then Tasks.DataSet.Edit;
                Tasks.FieldByName('PARENT').AsVariant:=cProject.Tasks.Id.AsVariant;
                Tasks.DataSet.Post;
              end;
        end;
      Tasks.DataSet.Next;
    end;
  Reorganize;
  cProject.Free;
end;

procedure TProjectLinks.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('RREF_ID').AsVariant:=(Parent as TProject).Id.AsVariant;
end;
procedure TProjectLinks.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
        if Assigned(Parent) then
          begin
            if Filter <> '' then
              begin
                if not TBaseDBDataSet(Parent).Id.IsNull then
                  Filter := Filter+' AND '+Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(TBaseDBDataSet(Parent).Id.AsString)
                else
                  Filter := Filter+' AND '+Data.QuoteField('RREF_ID')+'= 0';
              end
            else
              begin
              if not TBaseDBDataSet(Parent).Id.IsNull then
                Filter := Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(TBaseDBDataSet(Parent).Id.AsString)
              else
                Filter := Data.QuoteField('RREF_ID')+'= 0';
              end;
          end;
        end;
    end;
  inherited Open;
end;
function TProjectList.GetDescriptionFieldName: string;
begin
  Result := 'DESC';
end;
function TProjectList.GetTextFieldName: string;
begin
  Result := 'NAME';
end;
function TProjectList.GetNumberFieldName: string;
begin
  Result:='ID';
end;
function TProjectList.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;
constructor TProjectList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          SortFields := 'TIMESTAMPD';
          SortDirection := sdDescending;
          UsePermissions:=True;
        end;
    end;
  UpdateFloatFields:=True;
end;

constructor TProjectList.Create(aOwner: TComponent);
begin
  CreateEx(aOwner,Data,nil,nil);
end;

procedure TProjectList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROJECTS';
      TableCaption := strProjects;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            property ID: string index 20 read  write ;
            property GPRIORITY: Integer read  write ;
            property TYPE: string index 1 read  write ;
            property NAME: string index 250 read  write ;
            property STATUS: string index 4 read  write ;
            property NEEDSACTION: string index 1 read  write ;
            property TREEENTRY: Int64 read  write ;
            property START: TDateTime read  write ;
            property END: TDateTime read  write ;
            property CRITPATH: double read  write ;//length of Critical Path in days
            property TARGET: TDateTime read  write ;
            property PLANTARGET: TDateTime read  write ;
            property TARGETCOSTS: double read  write ;
            property COSTS: double read  write ;
            property CURRENCY: string index 5 read  write ;
            property COLOR: string index 12 read  write ;
            property DESC: string read  write ;
            property PARENT: Int64 read  write ;
            property PMANAGER: string index 20 read  write ;
            property PINITED: string index 20 read  write ;
            property INFORMLEADER: string index 1 read  write ;
            property INFORMPMANAGER: string index 1 read  write ;
            property GROSSPLANNING: string index 1 read  write ;
            property ISTEMPLATE: string index 1 read  write ;
            property COSTCENTRE: string index 20 read  write ;//Kostenstelle
            property ACCOUNT: string index 20 read  write ; //Fibu Konto
            property ACCOUNTINGINFO: string read  write ; //Fibu Info
            property CATEGORY: string index 60 read  write ;
            property CLASS: string index 60 read  write ;
            property CHANGEDBY: string index 4 read  write ;
            property CREATEDBY: string index 4 read  write ;
            property CREATEDAT: TDateTime read  write ;
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            property ID','ID',[]);
            property NAME','NAME',[]);
            property STATUS','STATUS',[]);
            property TREEENTRY','TREEENTRY',[]);
          end;
      if Data.ShouldCheckTable(TableName) then
        DefineUserFields(aDataSet);
    end;
end;

procedure TProjectList.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(DataSet,'COSTS',strCosts);
  SetDisplayLabelName(DataSet,'TARGETCOSTS',strTargetCosts);
end;

function TProjectList.SelectFromLink(aLink: string): Boolean;
begin
  Result := False;
  Select(0);
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  if copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS.ID' then
    begin
      with BaseApplication as IBaseDbInterface do
        begin
          with DataSet as IBaseDBFilter do
            begin
              Filter := Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink)))+' OR '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink)));
              Result := True;
            end;
        end;
    end
  else if copy(aLink,0,pos('@',aLink)-1) = 'PROJECTS' then
    begin
      with BaseApplication as IBaseDbInterface do
        begin
          with DataSet as IBaseDBFilter do
            begin
              Filter := Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink)));
              Result := True;
            end;
        end;
    end
  else
    Result := inherited SelectFromLink(aLink);
end;
procedure TProjectList.SelectFromParent(aParent: Variant);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          if (VarIsNumeric(aParent) and (aParent = 0))
          or (VarIsStr(aParent) and (aParent = ''))
          or (aParent = Null)  then
            begin
              with DataSet as IBaseManageDb do
                Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0');
            end
          else
            begin
              with DataSet as IBaseManageDb do
                Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField('PARENT')+'='+Data.QuoteValue(Format('%d',[Int64(aParent)]));
            end;
          UsePermissions:=True;
        end;
    end;
end;

initialization
  RegisterdataSetClass('PROJECTS',TProject);
end.

