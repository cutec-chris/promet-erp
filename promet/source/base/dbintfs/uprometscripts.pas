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
Created 08.08.2014
*******************************************************************************}
unit uprometscripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbClasses, uBaseDBInterface, db, genpascalscript
  ,uPSRuntime,uPSCompiler,uPSUtils;

type
  { TBaseScript }

  TBaseScript = class(TBaseDbList)
    procedure DataSetAfterOpen(aDataSet: TDataSet);
    procedure DataSetAfterScroll(ADataSet: TDataSet);
    function TPascalScriptUses(Sender: TPascalScript; const aName: tbtString
      ): Boolean;
  private
    aDS: TDataSet;
    FHistory: TBaseHistory;
    FLinks: TLinks;
    FRlFunc: TReadlnFunc;
    FScript: TScript;
    FSlFunc: TSleepFunc;
    FWrFunc: TWritelnFunc;
    FWriFunc: TWriteFunc;
    FDataSource: TDataSource;
    procedure SQLConn;
    procedure DoSetResults;
    procedure DoSetStatus(s : string);
  protected
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;

    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalReadln(var s: string);

    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;

    function InternalDataSet(SQL : string) : TDataSet;
    function InternalData : TBaseDBModule;
    function InternalHistory(Action: string; ParentLink: string; Icon: Integer=0;
      ObjectLink: string=''; Reference: string='';aCommission: string='';Source : string='';Date:TDateTime = 0) : Boolean;
    function InternalUserHistory(Action: string; UserName: string; Icon: Integer;
      ObjectLink: string; Reference: string; aCommission: string;
  Source: string; Date: TDateTime): Boolean;
    procedure InternalStorValue(aName,aId : string;aValue : Double);
    procedure InternalExecuteScript(aCommand, aClient: string);
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Write : TWriteFunc read FWriFunc write FWriFunc;
    property Writeln : TWritelnFunc read FWrFunc write FWRFunc;
    property Readln : TReadlnFunc read FRlFunc write FRlFunc;
    property Sleep : TSleepFunc read FSlFunc write FSlFunc;
    property Script : TScript read FScript;
    function Execute(Parameters : Variant) : Boolean;
    property History : TBaseHistory read FHistory;
    property Links : TLinks read FLinks;
    destructor Destroy;override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic

var
  Historyrun : Boolean;

implementation
uses uStatistic,uData,httpsend,Utils,variants,uPerson,uMasterdata,uProjects,uOrder,uBaseERPDBClasses,
  uBaseApplication,uSystemMessage;
function ProcessScripts : Boolean;//process Scripts that must be runned cyclic Result shows that it should be runned faster (debug)
var
  aScript: TBaseScript;
  aHistory: TBaseHistory;
  bScript: TBaseScript;
begin
  Result:=false;
  aScript := TBaseScript.Create(nil);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteValue('0')+' OR '+Data.QuoteField('STATUS')+'='+Data.QuoteValue('d'));
  while not aScript.EOF do
    begin
      if (aScript.FieldByName('STATUS').AsString<>'S') and ((aScript.FieldByName('RUNMASHINE').AsString='') or (pos(GetSystemName,aScript.FieldByName('RUNMASHINE').AsString)>0)) then
        if (aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now()) or (aScript.FieldByName('STATUS').AsString='d') or (aScript.FieldByName('STATUS').AsString='r') then
          begin
            bScript := TBaseScript.CreateEx(nil,aScript.DataModule,aScript.Connection);
            bScript.Select(aScript.Id.AsVariant);
            bScript.Open;
            Result := (aScript.FieldByName('STATUS').AsString='d');
            if bScript.Count=1 then
              bScript.Execute(Null);
            bScript.Free;
          end;
      aScript.Next;
    end;
  if Historyrun then
    begin
      aScript.Filter(Data.QuoteField('RUNONHISTORY')+'='+Data.QuoteValue('Y'));
      if (not aScript.EOF) then
        begin
          if aHistory.Count>0 then
            begin
              aHistory := TBaseHistory.Create(nil);
              while not aScript.EOF do
                begin
                  if aScript.FieldByName('STATUS').AsString<>'E' then
                    if aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now() then
                      begin
                        aHistory.Filter(Data.QuoteField('DATE')+'>'+Data.DateTimeToFilter(aScript.FieldByName('LASTRUN').AsDateTime));
                        aHistory.Last;
                        while not aHistory.DataSet.BOF do
                          begin
                            aHistory.Prior;
                            bScript := TBaseScript.CreateEx(nil,aScript.DataModule,aScript.Connection);
                            bScript.Select(aScript.Id.AsVariant);
                            bScript.Open;
                            if bScript.Count=1 then
                              bScript.Execute(VarArrayOf([aHistory.FieldByName('ACTION').AsString,aHistory.FieldByName('DATE').AsDateTime]));
                            bScript.Free;
                          end;
                      end;
                  aScript.Next;
                end;
              aHistory.Free;
            end;
        end
      else Historyrun:=False;
    end;
  aScript.Free;
end;
function TBaseScript.InternalDataSet(SQL: string): TDataSet;
begin
  Result := TBaseDBModule(DataModule).GetNewDataSet(SQL,Connection);
end;

function TBaseScript.InternalData: TBaseDBModule;
begin
  Result := uData.Data;
end;

function TBaseScript.InternalHistory(Action: string; ParentLink: string;
  Icon: Integer; ObjectLink: string; Reference: string; aCommission: string;
  Source: string; Date: TDateTime): Boolean;
var
  aHistory: TBaseHistory;
  aDataSetClass: TBaseDBDatasetClass;
  aDataSet: TBaseDBDataset;
begin
  Result := False;
  if TBaseDBModule(DataModule).DataSetFromLink(ParentLink,aDataSetClass) then
    begin
      aDataSet := aDataSetClass.CreateEx(nil,DataModule,Connection);
      TBaseDbList(aDataSet).SelectFromLink(ParentLink);
      aDataSet.Open;
      if aDataSet.Count>0 then
        begin
          aHistory := TBaseHistory.CreateEx(nil,DataModule,Connection,aDataSet.DataSet);
          aHistory.AddItemSR(aDataSet.DataSet,Action,ObjectLink,Reference,ObjectLink,Icon,aCommission,True,False);
          if Source<>'' then
            aHistory.FieldByName('SOURCE').AsString:=Source;
          aHistory.Post;
          aHistory.Free;
          result := True;
        end;
      aDataSet.Free;
    end;
end;
function TBaseScript.InternalUserHistory(Action: string; UserName: string;
  Icon: Integer; ObjectLink: string; Reference: string; aCommission: string;
  Source: string; Date: TDateTime): Boolean;
begin
  Result := False;
  if Data.Users.Locate('NAME',UserName,[loCaseInsensitive]) then
    begin
      Result := Data.Users.History.AddItemSR(Data.Users.DataSet,Action,ObjectLink,Reference,ObjectLink,Icon,aCommission,True,False);
      if Source<>'' then
        Data.Users.History.FieldByName('SOURCE').AsString:=Source;
      Data.Users.History.Post;
    end;
end;

procedure TBaseScript.InternalStorValue(aName, aId: string; aValue: Double);
var
  aVariable: TVariables;
begin
  aVariable := TVariables.Create(nil);
  aVariable.CreateTable;
  aVariable.Add(aName,aId,aValue);
  aVariable.Free;
end;

procedure TBaseScript.InternalExecuteScript(aCommand, aClient: string);
begin
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseApplication do
      TMessageHandler(GetMessageManager).SendCommand(aClient,aCommand);
end;

procedure TBaseScript.DataSetAfterOpen(aDataSet: TDataSet);
begin
  TPascalScript(FScript).OnUses:=@TPascalScriptUses;
end;

procedure TBaseScript.DataSetAfterScroll(ADataSet: TDataSet);
begin
  FScript.Source:=FieldByName('SCRIPT').AsString;
end;

procedure TBaseDbListPropertyTextR(Self: TBaseDbList; var T: TField); begin T := Self.Text; end;
procedure TBaseDbListPropertyNumberR(Self: TBaseDbList; var T: TField); begin T := Self.Number; end;
procedure TBaseDbListPropertyBookNumberR(Self: TBaseDbList; var T: TField); begin T := Self.BookNumber; end;
procedure TBaseDbListPropertyBarcodeR(Self: TBaseDbList; var T: TField); begin T := Self.Barcode; end;
procedure TBaseDbListPropertyTypR(Self: TBaseDbList; var T: string); begin T := Self.Typ; end;
procedure TBaseDbListPropertyMatchCodeR(Self: TBaseDbList; var T: TField); begin T := Self.Matchcode; end;
procedure TBaseDbListPropertyDescriptionR(Self: TBaseDbList; var T: TField); begin T := Self.Description; end;
procedure TBaseDbListPropertyComissionR(Self: TBaseDbList; var T: TField); begin T := Self.Commission; end;
procedure TBaseDbListPropertyStatusR(Self: TBaseDbList; var T: TField); begin T := Self.Status; end;
procedure TPersonPropertyContR(Self: TPerson; var T: TPersonContactData); begin T := Self.ContactData; end;
procedure TPersonPropertyAdressR(Self: TPerson; var T: TBaseDbAddress); begin T := Self.Address; end;
procedure TBaseDbListPropertyHistoryR(Self: TBaseDBList; var T: TBaseHistory); var Hist : IBaseHistory; begin if Supports(Self, IBaseHistory, Hist) then T := Hist.GetHistory; end;
procedure TUserPropertyFollowsR(Self: TUser; var T: TFollowers); begin T := Self.Follows; end;
procedure TUserPropertyOptionsR(Self: TUser; var T: TOptions); begin T := Self.Options; end;
procedure TBaseDBDatasetPropertyDataSetR(Self: TBaseDBDataset; var T: TDataSet); begin T := Self.DataSet; end;
procedure TBaseDBModulePropertyUsersR(Self: TBaseDBModule; var T: TUser); begin T := Self.Users; end;
procedure TBaseDBDatasetPropertyCountR(Self: TBaseDBDataSet; var T: Integer); begin T := Self.Count; end;
procedure TStoragePropertyJournalR(Self: TStorage; var T: TStorageJournal); begin T := Self.Journal; end;
procedure TMasterdataPropertyStorageR(Self: TMasterdata; var T: TStorage); begin T := Self.Storage; end;

function TBaseScript.TPascalScriptUses(Sender: TPascalScript;
  const aName: tbtString): Boolean;
var
  aScript: TBaseScript;
begin
  Result:=False;
  if aName = 'SYSTEM' then
    begin
      Result := True;
      try
        Sender.AddMethod(Self,@TBaseScript.InternalWriteln,'procedure Writeln(P1: string);');
        Sender.AddMethod(Self,@TBaseScript.InternalWrite,'procedure Write(P1: string);');
        Sender.AddMethod(Self, @TBaseScript.InternalParamStr,'function ParamStr(Param : Integer) : String;');
        Sender.AddMethod(Self, @TBaseScript.InternalParamCount,'function ParamCount : Integer;');
      except
        Result := False; // will halt compilation
      end;
    end
  else if aName = 'PROMET' then
    begin
      Result := True;
      try
        Sender.InternalUses(Sender.Compiler,'DB');
        Sender.InternalUses(Sender.Compiler,'DATEUTILS');
        Sender.AddMethod(Self,@TBaseScript.InternalDataSet,'function DataSet(SQL : string) : TDataSet;');
        Sender.AddMethod(Self,@TBaseScript.InternalHistory,'function History(Action : string;ParentLink : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Source : string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TBaseScript.InternalUserHistory,'function UserHistory(Action : string;User   : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Source : string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TBaseScript.InternalStorValue,'procedure StorValue(Name,Id : string;Value : Double);');
        Sender.AddMethod(Self,@TBaseScript.InternalExecuteScript,'procedure ExecuteScript(Name,Client : string);');
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TComponent'),TBaseDBDataset) do
          begin
            RegisterMethod('procedure Open;');
            RegisterMethod('procedure Close;');
            RegisterMethod('procedure Insert;');
            RegisterMethod('procedure Append;');
            RegisterMethod('procedure Delete;');
            //RegisterMethod('procedure First;');
            //RegisterMethod('procedure Last;');
            //RegisterMethod('procedure Next;');
            //RegisterMethod('procedure Prior;');
            RegisterMethod('procedure Post;');
            RegisterMethod('procedure Edit;');
            RegisterMethod('procedure Cancel;');
            //RegisterMethod('function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean;');
            //RegisterMethod('function EOF : Boolean;');
            //RegisterMethod('function FieldByName(const aFieldName : string) : TField;');
            RegisterMethod('procedure Filter(aFilter : string;aLimit : Integer);');
            RegisterProperty('ActualFilter','String',iptRW);
            RegisterProperty('ActualLimit','Integer',iptRW);
            RegisterProperty('DataSet','TDataSet',iptRW);
            RegisterProperty('Count','Integer',iptRW);
          end;
        with Sender.ClassImporter.Add(TBaseDBDataset) do
          begin
            RegisterVirtualMethod(@TBaseDBDataset.Open, 'OPEN');
            RegisterVirtualMethod(@TBaseDBDataset.Close, 'CLOSE');
            RegisterVirtualMethod(@TBaseDBDataset.Insert, 'INSERT');
            RegisterVirtualMethod(@TBaseDBDataset.Append, 'APPEND');
            RegisterVirtualMethod(@TBaseDBDataset.Delete, 'DELETE');
            RegisterVirtualMethod(@TBaseDBDataset.First, 'FIRST');
            RegisterVirtualMethod(@TBaseDBDataset.Last, 'LAST');
            RegisterVirtualMethod(@TBaseDBDataset.Next, 'NEXT');
            RegisterVirtualMethod(@TBaseDBDataset.Prior, 'PRIOR');
            RegisterVirtualMethod(@TBaseDBDataset.Post, 'POST');
            RegisterVirtualMethod(@TBaseDBDataset.Edit, 'EDIT');
            RegisterVirtualMethod(@TBaseDBDataset.Cancel, 'CANCEL');
            RegisterVirtualMethod(@TBaseDBDataset.Locate, 'LOCATE');
            RegisterVirtualMethod(@TBaseDBDataset.EOF, 'EOF');
            RegisterVirtualMethod(@TBaseDBDataset.FieldByName, 'FIELDBYNAME');
            RegisterVirtualMethod(@TBaseDBDataset.Filter, 'FILTER');
            RegisterPropertyHelper(@TBaseDBDatasetPropertyDataSetR,nil,'DATASET');
            RegisterPropertyHelper(@TBaseDBDatasetPropertyCountR,nil,'COUNT');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TBaseDbList) do
          begin
            RegisterProperty('Text','TField',iptR);
            RegisterProperty('Number','TField',iptR);
            RegisterProperty('BookNumber','TField',iptR);
            RegisterProperty('Barcode','TField',iptR);
            RegisterProperty('Description','TField',iptR);
            RegisterProperty('Commission','TField',iptR);
            RegisterProperty('Status','TField',iptR);
            RegisterProperty('Typ','string',iptR);
            RegisterProperty('MatchCode','TField',iptR);
            RegisterMethod('function SelectFromLink(aLink : string) : Boolean;');
            RegisterMethod('function SelectFromNumber(aNumber : string) : Boolean;');
          end;
        with Sender.ClassImporter.Add(TBaseDbList) do
          begin
            RegisterVirtualMethod(@TBaseDbList.SelectFromLink,'SELECTFROMLINK');
            RegisterVirtualMethod(@TBaseDbList.SelectFromNumber,'SELECTFROMNUMBER');
            RegisterPropertyHelper(@TBaseDbListPropertyTextR,nil,'TEXT');
            RegisterPropertyHelper(@TBaseDbListPropertyNumberR,nil,'NUMBER');
            RegisterPropertyHelper(@TBaseDbListPropertyBookNumberR,nil,'BOOKNUMBER');
            RegisterPropertyHelper(@TBaseDbListPropertyBarcodeR,nil,'BARCODE');
            RegisterPropertyHelper(@TBaseDbListPropertyDescriptionR,nil,'DESCRIPTION');
            RegisterPropertyHelper(@TBaseDbListPropertyComissionR,nil,'COMMISSION');
            RegisterPropertyHelper(@TBaseDbListPropertyStatusR,nil,'STATUS');
            RegisterPropertyHelper(@TBaseDbListPropertyTypR,nil,'TYP');
            RegisterPropertyHelper(@TBaseDbListPropertyMatchCodeR,nil,'MATCHCODE');
          end;
        //Object (Element)
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TObjects) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TObjects) do
          begin
            RegisterConstructor(@TObjects.Create,'CREATE');
          end;
        //Person
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TBaseDbAddress) do
          begin
            RegisterMethod('function ToString: ansistring;');
            RegisterMethod('procedure FromString(aStr : AnsiString);');
          end;
        with Sender.ClassImporter.Add(TBaseDbAddress) do
          begin
            RegisterVirtualMethod(@TBaseDbAddress.ToString,'TOSTRING');
            RegisterVirtualMethod(@TBaseDbAddress.FromString,'FROMSTRING');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbList'),TPersonContactData) do
          begin
          end;
        with Sender.ClassImporter.Add(TPersonContactData) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TPersonList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TPersonList) do
          begin
            RegisterConstructor(@TPersonList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TPersonList'),TPerson) do
          begin
            RegisterProperty('Address','TPersonAddress',iptR);
            RegisterProperty('ContactData','TPersonContactData',iptR);
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TPerson) do
          begin
            RegisterPropertyHelper(@TPersonPropertyAdressR,nil,'ADDRESS');
            RegisterPropertyHelper(@TPersonPropertyContR,nil,'CONTACTDATA');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        //Masterdata
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TStorageJournal) do
          begin
          end;
        with Sender.ClassImporter.Add(TStorageJournal) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TStorage) do
          begin
            RegisterProperty('Journal','TStorageJournal',iptR);
          end;
        with Sender.ClassImporter.Add(TStorage) do
          begin
            RegisterPropertyHelper(@TStoragePropertyJournalR,nil,'JOURNAL');
          end;

        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TMasterdataList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TMasterdataList) do
          begin
            RegisterConstructor(@TMasterdataList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TMasterdataList'),TMasterdata) do
          begin
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Storage','TStorage',iptR);
          end;
        with Sender.ClassImporter.Add(TMasterdata) do
          begin
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TMasterdataPropertyStorageR,nil,'STORAGE');
          end;
        //Projects
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TProjectList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TProjectList) do
          begin
            RegisterConstructor(@TProjectList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TProjectList'),TProject) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TProject) do
          begin
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        //Orders
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TOrderList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TOrderList) do
          begin
            RegisterConstructor(@TOrderList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TOrderList'),TOrder) do
          begin
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TOrder) do
          begin
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        //Small Gneral Datasets
        Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TFollowers);
        Sender.ClassImporter.Add(TFollowers);
        Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TOptions);
        Sender.ClassImporter.Add(TOptions);
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbList'),TUser) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Follows','TFollowers',iptR);
            RegisterProperty('Options','TOptions',iptR);
          end;
        with Sender.ClassImporter.Add(TUser) do
          begin
            RegisterConstructor(@TUser.Create,'CREATE');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TUserPropertyFollowsR,nil,'FOLLOWS');
            RegisterPropertyHelper(@TUserPropertyOptionsR,nil,'OPTIONS');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TActiveUsers) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TActiveUsers) do
          begin
            RegisterConstructor(@TActiveUsers.Create,'CREATE');
          end;

        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TComponent'),TBaseDBModule) do
          begin
            RegisterMethod('function GetConnection: TComponent;');
            //RegisterMethod('function GetSyncOffset: Integer;');
            //RegisterMethod('procedure SetSyncOffset(const AValue: Integer);');
            //RegisterMethod('function GetLimitAfterSelect: Boolean;');
            //RegisterMethod('function GetLimitSTMT: string;');

            RegisterMethod('function BuildLink(aDataSet : TDataSet) : string;');
            RegisterMethod('function GotoLink(const aLink : string) : Boolean;');
            RegisterMethod('function GetLinkDesc(aLink : string) : string;');
            RegisterMethod('function GetLinkLongDesc(aLink : string) : string;');
            RegisterMethod('function GetLinkIcon(aLink : string) : Integer;');

            RegisterMethod('function QuoteField(aField : string) : string;');
            RegisterMethod('function QuoteValue(aValue : string) : string;');
            RegisterMethod('function EscapeString(aValue : string) : string;');
            RegisterMethod('function DateToFilter(aValue : TDateTime) : string;');
            RegisterMethod('function DateTimeToFilter(aValue : TDateTime) : string;');
            //RegisterMethod('function ProcessTerm(aTerm : string) : string;');

            RegisterProperty('Users','TUser',iptR);
          end;
        with Sender.ClassImporter.Add(TBaseDBModule) do
          begin
            //RegisterVirtualMethod(@TBaseDBModule.GetConnection, 'GETCONNECTION');
            RegisterVirtualMethod(@TBaseDBModule.BuildLink, 'BUILDLINK');
            RegisterVirtualMethod(@TBaseDBModule.GotoLink, 'GOTOLINK');
            RegisterVirtualMethod(@TBaseDBModule.GetLinkDesc, 'GETLINKDESC');
            RegisterVirtualMethod(@TBaseDBModule.GetLinkLongDesc, 'GETLINKLONGDESC');
            RegisterVirtualMethod(@TBaseDBModule.GetLinkIcon, 'GETLINKICON');

            RegisterVirtualMethod(@TBaseDBModule.QuoteField, 'QUOTEFIELD');
            RegisterVirtualMethod(@TBaseDBModule.QuoteValue, 'QUOTEVALUE');
            RegisterVirtualMethod(@TBaseDBModule.EscapeString, 'ESCAPESTRING');
            RegisterVirtualMethod(@TBaseDBModule.DateToFilter, 'DATETOFILTER');
            RegisterVirtualMethod(@TBaseDBModule.DateTimeToFilter, 'DATETIMETOFILTER');
            RegisterVirtualMethod(@TBaseDBModule.ProcessTerm, 'PROCESSTERM');

            RegisterPropertyHelper(@TBaseDBModulePropertyUsersR,nil,'USERS');
          end;
        Sender.AddMethod(Self,@TBaseScript.InternalData,'function Data : TBaseDBModule');
      except
        Result := False; // will halt compilation
      end;
    end
  else
    begin
      aScript := TBaseScript.CreateEx(nil,DataModule);
      aScript.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue(aName)+')'));
      if aScript.Count>0 then
        if aScript.Locate('NAME',aName,[loCaseInsensitive]) then
          Result := Sender.Compiler.Compile(aScript.FieldByName('SCRIPT').AsString);
      aScript.Free;
    end;
end;

procedure TBaseScript.SQLConn;
var
  aSQL: String;
begin
  aSQL := ReplaceSQLFunctions(FieldByName('SCRIPT').AsString);
  aDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL,Connection);
end;

destructor TBaseScript.Destroy;
begin
  FLinks.Free;
  FHistory.Free;
  fScript.Destroy;
  FDataSource.Destroy;
  inherited Destroy;
end;

procedure TBaseScript.DoSetResults;
begin
  Edit;
  FieldByName('LASTRESULT').AsString:=Script.Results;
  Post;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
end;

function TBaseScript.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetNumberFieldName: string;
begin
  Result := 'SQL_ID';
end;

procedure TBaseScript.InternalWrite(const s: string);
begin
  if Assigned(FWriFunc) then FWriFunc(s);
end;

procedure TBaseScript.InternalWriteln(const s: string);
begin
  if Assigned(FWrFunc) then FWrFunc(s);
end;

procedure TBaseScript.InternalReadln(var s: string);
begin
  if Assigned(FRlFunc) then FRlFunc(s);
end;

function TBaseScript.InternalParamStr(Param: Integer): String;
begin
  Result:='';
  if not VarIsArray(FScript.Parameters) then exit;
  if Param<=VarArrayHighBound(FScript.Parameters,1) then
    Result:=FScript.Parameters[Param];
end;

function TBaseScript.InternalParamCount: Integer;
begin
  if not VarIsArray(FScript.Parameters) then exit;
  Result := VarArrayHighBound(FScript.Parameters,1)+1;
end;

constructor TBaseScript.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FScript := TPascalScript.Create;
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := DataSet;
  DataSet.AfterScroll:=@DataSetAfterScroll;
  dataSet.AfterOpen:=@DataSetAfterOpen;
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FLinks := TLinks.CreateEx(Self,DM,aConnection);
end;

procedure TBaseScript.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SCRIPTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,False);
            Add('PARENT',ftLargeint,0,False);
            Add('NAME',ftString,60,True);
            Add('STATUS',ftString,3,false);
            Add('SYNTAX',ftString,15,True);
            Add('RUNEVERY',ftInteger,0,False);
            Add('RUNMASHINE',ftString,150,False);
            Add('RUNONHISTORY',ftString,1,False);
            Add('LASTRUN',ftDateTime,0,False);
            Add('SCRIPT',ftMemo,0,false);
            Add('NOTE',ftMemo,0,false);
            Add('FOLDSTATE',ftString,200,false);
            Add('LASTRESULT',ftMemo,0,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('NAME','NAME',[ixUnique]);
    end;
end;
procedure TBaseScript.FillDefaults(aDataSet: TDataSet);
begin
  FieldByName('SYNTAX').AsString:='Pascal';
  FieldByName('SCRIPT').AsString:='begin'+LineEnding+'  '+LineEnding+'end.';
  inherited FillDefaults(aDataSet);
end;
function TBaseScript.Execute(Parameters: Variant): Boolean;
var
  aStartTime: TDateTime;
begin
  Result := False;
  if Count=0 then exit;
  OpenItem;
  if (pos(GetSystemName,FieldByName('RUNMASHINE').AsString)>0) or (trim(FieldByName('RUNMASHINE').AsString)='') then
    begin
      DoSetStatus('R');
      Edit;
      FieldByName('LASTRESULT').Clear;
      aStartTime:=Now();
      Post;
      Result := False;
      try
        if lowercase(FieldByName('SYNTAX').AsString) = 'sql' then
          begin
            try
              SQLConn;
              with aDS as IBaseDbFilter do
                DoExecSQL;
              with aDS as IBaseDbFilter do
                Script.Results:='Num Rows Affected: '+IntToStr(NumRowsAffected);
              DoSetResults;
              Result := True;
            except
              on e : Exception do
                begin
                  Script.Results := e.Message;
                  InternalWriteln('Error:'+e.Message);
                  DoSetResults;
                  Result := False;
                end;
            end;
          end
        else if lowercase(FieldByName('SYNTAX').AsString) = 'pascal' then
          begin
            Result := FScript.Execute(Parameters);
            if not Result then
              DoSetResults;
          end;
        if Result then
          begin
            DoSetStatus('N');
            Edit;
            FieldByName('LASTRUN').AsDateTime:=aStartTime;
            Post;
          end
        else
          begin
            DoSetStatus('E');
          end;
      except
      end;
    end
  else
    begin
      DoSetStatus('r');
      while FieldByName('STATUS').AsString='r' do
        begin
          sleep(500);
          DataSet.Refresh;
        end;
    end;
end;

initialization
  LoadedLibs := TList.Create;
  Historyrun:=True;
finalization
  LoadedLibs.Clear;
  LoadedLibs.Free;
end.

