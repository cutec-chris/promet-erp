{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uProjects;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDBClasses, db, uBaseDbInterface, uIntfStrConsts,
  uBaseERPDbClasses,uTask,Variants;
type
  { TProjectList }
  TProjectList = class(TBaseERPList)
  protected
    function GetDescriptionFieldName: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName: string;override;
    function GetStatusFieldName: string;override;
  public
    constructor Create(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil;
      aMasterdata: TDataSet=nil); override;
    function GetTyp: string; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure SelectFromLink(aLink: string); override;
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
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure Open;override;
    property Project : TProjectList read FProject;
  end;

  { TProjectPositions }

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
  TProject = class(TProjectList,IBaseHistory)
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FHistory: TProjectHistory;
    FImages: TImages;
    FLinks: TProjectLinks;
    FPositions: TProjectPositions;
    FStateChange: TNotifyEvent;
    FTasks: TProjectTasks;
    FDS : TDataSource;
    FStatus : string;
    FTarget: String;
    function GetHistory: TBaseHistory;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure Open; override;
    procedure Recalculate;
    function CreateTable : Boolean;override;
    procedure CascadicPost;override;
    procedure CascadicCancel;override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    property History : TProjectHistory read FHistory;
    property Images : TImages read FImages;
    property Links : TProjectLinks read FLinks;
    property Tasks : TProjectTasks read FTasks;
    property Positions : TProjectPositions read FPositions;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
    procedure Delete; override;
  end;
implementation
uses uBaseSearch,uBaseApplication,Utils;
resourcestring
  strTargetChanged              = 'Zieldatum geändert von %s zu %s';
  strPlanedPrice                = 'Planpreis';
  strRealPrice                  = 'Echtpreis';
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
  OldRealPosPrice:=DataSet.FieldByName('REALPOSPRICE').AsFloat;
end;

procedure TProjectPositions.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  OldRealPosPrice:=DataSet.FieldByName('REALPOSPRICE').AsFloat;
end;
procedure TProjectPositions.DoPost;
begin
  inherited DoPost;
  if (OldRealPosPrice <> DataSet.FieldByName('REALPOSPRICE').AsFloat) then
    begin
      RealPriceChanged(DataSet.FieldByName('REALPOSPRICE').AsFloat-OldRealPosPrice);
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
            Add('REALPRICE',ftFloat,0,False);
            Add('REALPOSPRICE',ftFloat,0,False);
            Add('WORKSTATUS',ftString,4,False);
            Add('ORIGDATE',ftDateTime,0,False);
          end;
      DefineUserFields(aDataSet);
    end;
end;
procedure TProjectPositions.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(aDataSet,'SELLPRICE',strPlanedPrice);
  SetDisplayLabelName(aDataSet,'REALPRICE',strRealPrice);
  SetDisplayLabelName(aDataSet,'ORIGDATE',strInvoiceDate);
end;
procedure TProjectTasks.FillDefaults(aDataSet: TDataSet);
var aID : Variant;
begin
  inherited FillDefaults(aDataSet);
  aID := (Parent as TProject).Id.AsVariant;
  aDataSet.FieldByName('PROJECTID').AsVariant:=aID;
  aDataSet.FieldByName('PROJECT').AsVariant:=(Parent as TProject).Text.AsVariant;
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
                if not Parent.Id.IsNull then
                  Filter := Filter+' AND '+Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(Parent.Id.AsString)
                else
                  Filter := Filter+' AND '+Data.QuoteField('PROJECTID')+'= 0';
              end
            else
              begin
              if not Parent.Id.IsNull then
                Filter := Data.QuoteField('PROJECTID')+'='+Data.QuoteValue(Parent.Id.AsString)
              else
                Filter := Data.QuoteField('PROJECTID')+'= 0';
              end;
          end;
        end;
    end;
  inherited Open;
end;
procedure TProject.FDSDataChange(Sender: TObject; Field: TField);
var
  aMakeInactive: Boolean;
begin
  if not Assigned(Field) then exit;
  if (Dataset.State <> dsInsert)
  and ((Field.FieldName = 'STATUS')
   or  (Field.FieldName = 'TARGET')
      )
  then
    begin
      if not History.DataSet.Active then
        History.Open;
      if (Field.FieldName = 'STATUS') then
        begin
          History.AddItem(Self.DataSet,Format(strStatusChanged,[FStatus,Field.AsString]),'','',DataSet,ACICON_STATUSCH);
          FStatus := Field.AsString;
          if Assigned(FStateChange) then
            FStateChange(Self);
        end;
      if (Field.FieldName = 'TARGET') then
        begin
          History.AddItem(Self.DataSet,Format(strTargetChanged,[FTarget,Field.AsString]),'','',DataSet,ACICON_DATECHANGED);
          FTarget := Field.AsString;
        end;
    end;
end;
function TProject.GetHistory: TBaseHistory;
begin
  Result := History;
end;

function TProjectList.GetTyp: string;
begin
  Result := 'P';
end;

constructor TProject.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          SortFields := 'SQL_ID';
          BaseSortFields:='SQL_ID';
          SortDirection := sdAscending;
          UsePermissions:=False;
          UpdateFloatFields:=True;
        end;
    end;
  FHistory := TProjectHistory.Create(Self,DM,aConnection,DataSet);
  FPositions := TProjectPositions.Create(Self,DM,aConnection,DataSet);
  FPositions.FProject := Self;
  FImages := TImages.Create(Self,DM,aConnection,DataSet);
  FLinks := TProjectLinks.Create(Self,DM,aConnection);
  with Self.DataSet as IBaseSubDataSets do
    RegisterSubDataSet(FLinks);
  FTasks := TProjectTasks.Create(Self,DM,aConnection);
  with Self.DataSet as IBaseSubDataSets do
    RegisterSubDataSet(FTasks);
  FTasks.FProject := Self;
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;
destructor TProject.Destroy;
begin
  FDS.Free;
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
  FTasks.CascadicPost;
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
      FieldByName('STATUS').AsString := 'A';
      FieldByName('TYPE').AsString := 'P';
      FieldByName('START').AsDateTime := Now();
      if Data.Numbers.HasNumberSet('PROJECTS') then
        if FieldByName('ID').IsNull then
          FieldByName('ID').AsString := Data.Numbers.GetNewNumber('PROJECTS');
      FieldByName('CREATEDBY').AsString := Data.Users.FieldByName('IDCODE').AsString;
      FieldByName('CHANGEDBY').AsString := Data.Users.FieldByName('IDCODE').AsString;
    end;
end;

procedure TProject.Delete;
begin
  Tasks.Open;
  while Tasks.Count>0 do
    Tasks.Delete;
  Links.Open;
  while Links.Count>0 do
    Links.Delete;
  Positions.Open;
  while Positions.Count>0 do
    Positions.Delete;
  inherited Delete;
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
                if not Parent.Id.IsNull then
                  Filter := Filter+' AND '+Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(Parent.Id.AsString)
                else
                  Filter := Filter+' AND '+Data.QuoteField('RREF_ID')+'= 0';
              end
            else
              begin
              if not Parent.Id.IsNull then
                Filter := Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(Parent.Id.AsString)
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
  Result := 'DESCRIPTION';
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
constructor TProjectList.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          SortFields := 'TIMESTAMPD';
          SortDirection := sdDescending;
          UsePermissions:=True;
        end;
    end;
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
            Add('ID',ftString,20,False);
            Add('GPRIORITY',ftLargeint,0,False);
            Add('TYPE',ftString,1,False);
            Add('NAME',ftString,250,False);
            Add('STATUS',ftString,4,True);
            Add('TREEENTRY',ftLargeInt,0,false);
            Add('START',ftDate,0,false);
            Add('END',ftDate,0,false);
            Add('TARGET',ftDate,0,false);
            Add('PLANTARGET',ftDate,0,false);
            Add('TARGETCOSTS',ftFloat,0,false);
            Add('COSTS',ftFloat,0,false);
            Add('CURRENCY',ftString,5,False);
            Add('DESCRIPTION',ftMemo,0,false);
            Add('PARENT',ftLargeint,0,False);
            Add('CATEGORY',ftString,60,False);
            Add('CHANGEDBY',ftString,4,false);
            Add('CREATEDBY',ftString,4,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ID','ID',[]);
            Add('NAME','NAME',[]);
            Add('TREEENTRY','TREEENTRY',[]);
          end;
    end;
end;
procedure TProjectList.SelectFromLink(aLink: string);
begin
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
              Filter := ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))));
            end;
        end;
    end
  else
    inherited SelectFromLink(aLink);
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
end.

