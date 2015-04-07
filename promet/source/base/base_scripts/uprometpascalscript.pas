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
Created 07.04.2015
*******************************************************************************}
unit uprometpascalscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, genpascalscript, uprometscripts,uPSRuntime,uPSCompiler,uPSUtils,
  Utils,db,uBaseDBInterface;

type
  TPrometPascalScript = class(TBaseScript)
    function TPascalScriptUses(Sender: TPascalScript; const aName: tbtString
      ): Boolean;
    procedure DataSetAfterOpen(aDataSet: TDataSet);
    procedure DataSetAfterScroll(ADataSet: TDataSet);
  private
    FRlFunc: TStrInFunc;
    FScript: TScript;
    FSlFunc: TSleepFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;

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
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent
  =nil; aMasterdata: TDataSet=nil); override;
    property Sleep : TSleepFunc read FSlFunc write FSlFunc;
    property Script : TScript read FScript;
    function Execute(Parameters: Variant): Boolean; override;
    destructor Destroy;override;
  end;

implementation

uses uBaseDbClasses,uPerson,uMasterdata,uBaseERPDBClasses,uProjects,uMessages,
  uDocuments,utask,uOrder,uData,variants,uBaseApplication;

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
procedure TProjectsTasksR(Self: TProject; var T: TProjectTasks); begin T := Self.Tasks; end;
procedure TMessagePropertyContentR(Self : TMessage;var T : TMessageContent);begin T := Self.Content; end;
procedure TMessagePropertyDocumentsR(Self : TMessage;var T : TDocuments);begin T := Self.Documents; end;


function TPrometPascalScript.TPascalScriptUses(Sender: TPascalScript;
  const aName: tbtString): Boolean;
var
  aScript: TPrometPascalScript;
begin
  Result:=False;
  if aName = 'SYSTEM' then
    begin
      Result := True;
      try
        Sender.AddMethod(Self,@TPrometPascalScript.InternalWriteln,'procedure Writeln(P1: string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalWrite,'procedure Write(P1: string);');
        Sender.AddMethod(Self, @TPrometPascalScript.InternalParamStr,'function ParamStr(Param : Integer) : String;');
        Sender.AddMethod(Self, @TPrometPascalScript.InternalParamCount,'function ParamCount : Integer;');
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
        Sender.AddMethod(Self,@TPrometPascalScript.InternalDataSet,'function DataSet(SQL : string) : TDataSet;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalHistory,'function History(Action : string;ParentLink : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Source : string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalUserHistory,'function UserHistory(Action : string;User   : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Source : string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalStorValue,'procedure StorValue(Name,Id : string;Value : Double);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScript,'procedure ExecuteScript(Name,Client : string);');
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TComponent'),TBaseDBDataset) do
          begin
            RegisterMethod('procedure Open;');
            RegisterMethod('procedure Close;');
            RegisterMethod('procedure Insert;');
            RegisterMethod('procedure Append;');
            RegisterMethod('procedure Delete;');
            RegisterMethod('procedure First;');
            RegisterMethod('procedure Last;');
            RegisterMethod('procedure Next;');
            RegisterMethod('procedure Prior;');
            RegisterMethod('procedure Post;');
            RegisterMethod('procedure Edit;');
            RegisterMethod('procedure Cancel;');
            RegisterMethod('function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean;');
            RegisterMethod('function EOF : Boolean;');
            RegisterMethod('function FieldByName(const aFieldName : string) : TField;');
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
            RegisterMethod('function  ExportToXML : string;');
            RegisterMethod('procedure ImportFromXML(XML : string;OverrideFields : Boolean);');
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
            RegisterVirtualMethod(@TBaseDbList.ImportFromXML,'IMPORTFROMXML');
            RegisterVirtualMethod(@TBaseDbList.ExportToXML,'EXPORTTOXML');
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
        //Document
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TDocuments) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TDocuments) do
          begin
            RegisterConstructor(@TDocuments.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TDocuments'),TDocument) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TDocument) do
          begin
            RegisterConstructor(@TDocument.Create,'CREATE');
          end;
        //Messages
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TMessageContent) do
          begin
          end;
        with Sender.ClassImporter.Add(TMessageContent) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TMessageList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TMessageList) do
          begin
            RegisterConstructor(@TMessageList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TMessageList'),TMessage) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('Content','TMessageContent',iptR);
            RegisterProperty('Documents','TDocuments',iptR);
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TMessage) do
          begin
            RegisterConstructor(@TMessageList.Create,'CREATE');
            RegisterPropertyHelper(@TMessagePropertyContentR,nil,'CONTENT');
            RegisterPropertyHelper(@TMessagePropertyDocumentsR,nil,'DOCUMENTS');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
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
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TTaskList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TTaskList) do
          begin
            RegisterConstructor(@TTaskList.Create,'CREATE');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TTaskList'),TTask) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TTask) do
          begin
            RegisterConstructor(@TTask.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TProjectList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TProjectList) do
          begin
            RegisterConstructor(@TProjectList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TTaskList'),TProjectTasks) do
          begin
          end;
        with Sender.ClassImporter.Add(TProjectTasks) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TProjectList'),TProject) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Tasks','TProjectTasks',iptR);
          end;
        with Sender.ClassImporter.Add(TProject) do
          begin
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TProjectsTasksR,nil,'TASKS');
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
            RegisterMethod('function ProcessTerm(aTerm : string) : string;');

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
        Sender.AddMethod(Self,@TPrometPascalScript.InternalData,'function Data : TBaseDBModule');
      except
        Result := False; // will halt compilation
      end;
    end
  else
    begin
      aScript := TPrometPascalScript.CreateEx(nil,DataModule);
      aScript.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue(aName)+')'));
      if aScript.Count>0 then
        if aScript.Locate('NAME',aName,[loCaseInsensitive]) then
          Result := Sender.Compiler.Compile(aScript.FieldByName('SCRIPT').AsString);
      aScript.Free;
    end;
end;

procedure TPrometPascalScript.DataSetAfterOpen(aDataSet: TDataSet);
begin
  TPascalScript(FScript).OnUses:=@TPascalScriptUses;
end;

procedure TPrometPascalScript.DataSetAfterScroll(ADataSet: TDataSet);
begin
  FScript.Source:=FieldByName('SCRIPT').AsString;
end;

function TPrometPascalScript.InternalParamStr(Param: Integer): String;
begin
  Result:='';
  if not VarIsArray(FScript.Parameters) then exit;
  if Param<=VarArrayHighBound(FScript.Parameters,1) then
    Result:=FScript.Parameters[Param];
end;

function TPrometPascalScript.InternalParamCount: Integer;
begin
  if not VarIsArray(FScript.Parameters) then exit;
  Result := VarArrayHighBound(FScript.Parameters,1)+1;
end;

function TPrometPascalScript.InternalDataSet(SQL: string): TDataSet;
begin
  Result := TBaseDBModule(DataModule).GetNewDataSet(SQL,Connection);
end;

function TPrometPascalScript.InternalData: TBaseDBModule;
begin
  Result := uData.Data;
end;

function TPrometPascalScript.InternalHistory(Action: string; ParentLink: string;
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
function TPrometPascalScript.InternalUserHistory(Action: string; UserName: string;
  Icon: Integer; ObjectLink: string; Reference: string; aCommission: string;
  Source: string; Date: TDateTime): Boolean;
var
  aUsers: TUser;
begin
  Result := False;
  aUsers := TUser.Create(nil);
  if aUsers.Locate('NAME',UserName,[loCaseInsensitive]) then
    begin
      Result := aUsers.History.AddItemSR(aUsers.DataSet,Action,ObjectLink,Reference,ObjectLink,Icon,aCommission,True,False);
      if Source<>'' then
        aUsers.History.FieldByName('SOURCE').AsString:=Source;
      aUsers.History.Post;
    end;
  aUsers.Free;
end;

procedure TPrometPascalScript.InternalStorValue(aName, aId: string; aValue: Double);
var
  aVariable: TVariables;
begin
  aVariable := TVariables.Create(nil);
  aVariable.CreateTable;
  aVariable.Add(aName,aId,aValue);
  aVariable.Free;
end;

procedure TPrometPascalScript.InternalExecuteScript(aCommand, aClient: string);
begin
  {
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseApplication do
      TMessageHandler(GetMessageManager).SendCommand(aClient,aCommand);
  }
end;

constructor TPrometPascalScript.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FScript := TPascalScript.Create;
  DataSet.AfterScroll:=@DataSetAfterScroll;
  dataSet.AfterOpen:=@DataSetAfterOpen;
end;

function TPrometPascalScript.Execute(Parameters: Variant): Boolean;
var
  aStartTime: TDateTime;
begin
  aStartTime := Now();
  if lowercase(FieldByName('SYNTAX').AsString) = 'pascal' then
    begin
      Result := FScript.Execute(Parameters);
      if not Result then
        DoSetResults(FScript.Results);
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
    end
  else Inherited;
end;

destructor TPrometPascalScript.Destroy;
begin
  fScript.Destroy;
  inherited Destroy;
end;


end.

