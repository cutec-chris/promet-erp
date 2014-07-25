program sync_outlook_tasks;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Interfaces
  { you can add units after this }, FileUtil,Forms, uData,db,
  Dialogs,Controls,uBaseCustomApplication, pcmdprometapp,Variants,LConvEncoding,
  dateutils,Graphics,uSync,uPerson,uCalendar,utask,Windows,comobj, ActiveX,Utils,
  uBaseDBInterface,LazLogger,fpJSON,synautil,
  MapiServices,MapiDefs,MAPIUtil,MapiTags,MapiGuid;
type

  TSyncMSOApp = class(TBaseCustomApplication)
  private
    aConnection: TMapiConnection;
    TimeOffset : Integer;
  protected
    procedure DoRun; override;
    function GetSingleInstance : Boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;
  TGenericFolder = class(TInBox)
  private
    FEntryTyp : Cardinal;
  protected
    function OpenDefaultStoreEID(const DefaultStoreEID: TSBinary): Boolean; override;
    procedure PopulateContentItems; override;
  public
    constructor Create(const Connection: TMapiConnection;EntryTyp : Cardinal);
    property Folder;
  end;
resourcestring
  strCollect                                         = 'Zusammenführen';
  strCollectItems                                    = 'Sollen der MAPI Eintrag "%s" und der Promet-ERP Eintrag "%s" zusammengeführt werden ?';
  strSyncItems                                       = 'Sollen der MAPI Eintrag "%s" ins Promet-ERP übernommen werden ?';
const
 PR_IPM_APPOINTMENT_ENTRYID                   = (PT_BINARY) or (($36D0) shl 16);
 PR_IPM_CONTACT_ENTRYID                       = (PT_BINARY) or (($36D1) shl 16);
 PR_IPM_DRAFTS_ENTRYID                        = $36D70102;
 PR_IPM_JOURNAL_ENTRYID                       = $36D20102;
 PR_IPM_NOTE_ENTRYID                          = $36D30102;
 PR_IPM_TASK_ENTRYID                          = $36D40102;
 PSETID_Unknown : TGUID = '{00020329-0000-0000-C000-000000000046}';
 SyncType = 'MAPI (Outlook)';
var
  Application: TSyncMSOApp;
function EncodingIn(aIn : string) : string;
begin
  Result := ConvertEncoding(aIn,GuessEncoding(aIn),EncodingUTF8);
end;
function EncodingOut(aOut : string) : string;
begin
  Result := ConvertEncoding(aOut,GuessEncoding(aOut),'CP1250');
end;
procedure TSyncMSOApp.DoRun;
var
  InBox: TInbox;
  Folder : TGenericFolder;
  aFolder: TGenericFolder;
  aItem: TMapiMailItem;
  aID: Variant;
  tmp: String;
  SyncOut: Boolean;
  Collect: Boolean;
  DoSync: Boolean;
  MAPISynced : array of Cardinal;
  SQLSynced : array of LargeInt;
  aFound: Boolean;
  i: Integer;
  aPicture : TPicture;
  fe: String;
  ms: TMemoryStream;
  UserSelected: Boolean;
  tmp1: String;
  SyncItems : TSyncItems;
  aContact: TPerson;
  aCalendar: TCalendar;
  aTasks: TTaskList;
  aPattern : string;
  RTFIsUpdated : Bool;
  StreamIntf: IStream;
  OLEStream: TOleStream;
  SStream : TStringStream;
var
  ErrorInfo: PMAPIERROR;
  Errorresult : HRESULT;
  StreamInfo: TStatStg;
  MapiMessage: MapiDefs.IMessage;
  SentItemsProps: TSPropTagArray;
  SentItemsValues: PSPropValue;
  lpcValues: Cardinal;
  aEntryID: TSBinary;
  bItem: TMapiMailItem;
  OldSeen: String;
  DoDelete: Boolean;
  SyncOutF: Boolean;
  aJsonList: TJSONArray;
  aObj: TJSONObject;
  aStart,aEnd : TDateTime;
  aAllday: Boolean;
  sl: TStringList;
  aJsonOutList: TJSONArray;
  aField: TJSONData;
  aPrivate: Boolean;
  bFolder: TGenericFolder;
  aLastSync: TDateTime;
  aExt: TJSONData;
begin
  try
    if not Login then raise Exception.Create('Login failed !');
    Info('Datamodule open...');
    Info('Connecting to MAPI...');
    SyncItems := TSyncItems.Create(nil,Data);
    SyncItems.CreateTable;
    SyncItems.Open;
    aConnection := TMapiConnection.Create('',True);
    if UserSelected then
      begin
        //get Timeoffset
        with Application as IBaseDBInterface do
          TimeOffset := DBConfig.ReadInteger('MSO_TIMEOFFSET',-1);
        if TimeOffset = -1 then
          begin
            aFolder := TGenericFolder.Create(aConnection,PR_IPM_APPOINTMENT_ENTRYID);
            if aFolder.Folder.CreateMessage(IMapiMessage, 0, MapiMessage) = S_OK then
              begin
                aItem := TMapiMailItem.Create(aFolder, MapiMessage, False);
                aItem.LastModificationTime:=Now();
                TimeOffset :=  round((Now()-aItem.LastModificationTime)*24);
                Info('TimeOffset created:'+IntToStr(TimeOffset));
                aItem.CoMessage.SaveChanges(0);
                TimeOffset :=  round((Now()-aItem.LastModificationTime)*24);
                Info('TimeOffset changed:'+IntToStr(TimeOffset));
                aItem.Delete;
                with Application as IBaseDBInterface do
                  DBConfig.WriteInteger('MSO_TIMEOFFSET',TimeOffset);
              end;
            aFolder.Free;
          end;
        Info('TimeOffset:'+IntToStr(TimeOffset));
      end
    else TimeOffset:=0;
    if UserSelected then
      begin
        {
        // In PSETID_Task
        #define dispidTaskStatus					0x8101	//PT_LONG
        #define dispidTaskPercentComplete			0x8102	//PT_DOUBLE
        #define dispidTaskTeamTask					0x8103	//PT_BOOLEAN
        #define dispidTaskStartDate					0x8104	//PT_SYSTIME
        #define dispidTaskDueDate					0x8105	//PT_SYSTIME
        #define dispidTaskDateCompleted				0x810f	//PT_SYSTIME
        #define dispidTaskActualEffort				0x8110	//PT_LONG
        #define dispidTaskEstimatedEffort			0x8111	//PT_LONG
        #define dispidTaskState						0x8113	//PT_LONG
        #define dispidTaskRecurrenceState			0x8116	//PT_BINARY
        #define dispidTaskComplete					0x811c	//PT_BOOLEAN
        #define dispidTaskOwner						0x811f	//PT_STRING8
        #define dispidTaskDelegator					0x8121	//PT_STRING8
        #define dispidTaskOrdinal					0x8123	//PT_LONG
        #define dispidTaskIsRecurring				0x8126	//PT_BOOLEAN
        #define dispidTaskRole						0x8127	//PT_STRING8
        #define dispidTaskOwnership					0x8129	//PT_LONG
        #define dispidTaskDelegationState			0x812A	//PT_LONG}

        Info('Syncing Tasks');
        //Aufgaben syncronisieren
        aFolder := TGenericFolder.Create(aConnection,PR_IPM_TASK_ENTRYID);
        //Create Item List
        aJsonList := TJSONArray.Create;
        try
          aItem := aFolder.GetFirst;
          while Assigned(aItem) do
            begin
              aObj := TJSONObject.Create;
              aObj.Add('EXTERNAL_ID',EntryIdToString(aItem.EntryID));
              aObj.Add('TIMESTAMPD',Rfc822DateTime(IncHour(aItem.LastModificationTime,TimeOffset)));
              aObj.Add('SUMMARY',EncodingIn(aItem.PropertiesDirect[PR_SUBJECT,ptString]));
              SStream := TStringStream.Create('');
              try
                if aItem.CoMessage.OpenProperty(PR_BODY, IStream, STGM_READ, 0, IInterface(StreamIntf)) = S_OK then
                  begin
                    StreamIntf.Stat(StreamInfo, STATFLAG_NONAME);
                    OLEStream := TOleStream.Create(StreamIntf);
                    try
                      SSTream.CopyFrom(OLEStream,StreamInfo.cbSize);
                    finally
                      OLEStream.Free;
                    end;
                    if SStream.DataString <> '' then
                      aObj.Add('DESC',EncodingIn(SStream.DataString));
                  end;
              finally
                StreamIntf := nil;
              end;
              if not Assigned(aObj.Find('DESC')) then
                aObj.Add('DESC',EncodingIn(aItem.PropertiesDirect[PR_BODY,ptString]));
              aObj.Add('COMPLETED',Boolean(aItem.PropertiesDirect[aItem.GetPropertyDispId($811c, PT_BOOLEAN, False, @PSETID_Task),ptBoolean]));
              aStart := aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime];
              aEnd := aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime];
              if aStart <> -1 then
                begin
                  aStart := IncHour(aStart,TimeOffset);
                  aObj.Add('STARTDATE',Rfc822DateTime(aStart));
                end;
              if aEnd <> -1 then
                begin
                  aEnd := IncHour(aEnd,TimeOffset);
                  aObj.Add('DUEDATE',Rfc822DateTime(aEnd));
                end;
              aItem.Free;
              aJsonList.Add(aObj);
              aItem := aFolder.GetNext;
             end;

          aTasks := TTaskList.Create(nil,Data);
          aLastSync := SyncItems.LastSync(SyncType,aTasks.TableName);
          if aLastSync>0 then
            aTasks.SelectActiveByUserChangedSince(Data.Users.Accountno.AsString,aLastSync-1)
          else
            aTasks.SelectActiveByUser(Data.Users.Accountno.AsString);
          aTasks.Open;
          //Sync Item List
          aJsonOutList := SyncItems.SyncDataSet(aTasks,aJsonList,SyncType);
          aJsonList.Free;
          aItem := aFolder.GetFirst;
          bItem := nil;
          //change existing Items
          while Assigned(aItem) do
            begin
              i := 0;
              while i < aJsonOutList.Count do
                begin
                  DoDelete := False;
                  aField := SyncItems.GetField(aJsonOutList[i],'EXTERNAL_ID');
                  if Assigned(aField) and (aField.AsString = EntryIdToString(aItem.EntryID))
                  and (not Assigned(SyncItems.GetField(aJsonOutList[i],'item_not_found')))
                  then
                    begin
                      aField := SyncItems.GetField(aJsonOutList[i],'SUMMARY');
                      debugln('Updated:'+aItem.Subject);
                      if Assigned(aField) then
                        aItem.PropertiesDirect[PR_SUBJECT,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'DESC');
                      if Assigned(aField) then
                        if aItem.PropertiesDirect[PR_BODY,ptString] = '' then
                          aItem.PropertiesDirect[PR_BODY,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'COMPLETED');
                      if Assigned(aField) then
                        begin
                          aItem.PropertiesDirect[aItem.GetPropertyDispId($811c, PT_BOOLEAN, False, @PSETID_Task),ptBoolean] := aField.AsString = 'Y';
                          DoDelete := aField.AsString = 'Y';
                        end;
                      aField := SyncItems.GetField(aJsonOutList[i],'DUEDATE');
                      {if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString)
                      else aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime] := -1;
                      aField := SyncItems.GetField(aJsonOutList[i],'STARTDATE');
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString)
                      else aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime] := -1;}
                      try
                        if DoDelete then aItem.Delete; //Delete Done Tasks couse we cant set them correctly done at time
                        aItem.CoMessage.SaveChanges(0);
                      except
                      end;
                      //dont create changed items
                      TJSONObject(aJsonOutList.Items[i]).Add('item_is_synched',True);
                      break;
                    end
                  else inc(i);
                end;
              FreeAndNil(aItem);
              aItem := aFolder.GetNext;
            end;
          //Create new Items
          for i := 0 to aJsonOutList.Count-1 do
            begin
              if Assigned(aJsonOutList[i])
              and (not Assigned(SyncItems.GetField(aJsonOutList[i],'item_not_found')))
              and (not Assigned(SyncItems.GetField(aJsonOutList[i],'item_is_synched')))
              and ((not Assigned(SyncItems.GetField(aJsonOutList[i],'HASCHILDS'))) or (SyncItems.GetField(aJsonOutList[i],'HASCHILDS').AsString<>'Y'))
              and ((not Assigned(SyncItems.GetField(aJsonOutList[i],'COMPLETED'))) or (SyncItems.GetField(aJsonOutList[i],'COMPLETED').AsString<>'Y'))
              then
                begin
                  if aFolder.Folder.CreateMessage(IMapiMessage, 0, MapiMessage) = S_OK then
                    begin
                      aItem := TMapiMailItem.Create(aFolder, MapiMessage, False);
                      aItem.MessageClass:='IPM.Task';
                      aField := SyncItems.GetField(aJsonOutList[i],'SUMMARY');
                      if Assigned(aField) then
                        aItem.PropertiesDirect[PR_SUBJECT,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'DESC');
                      if Assigned(aField) then
                        if aItem.PropertiesDirect[PR_BODY,ptString] = '' then
                          aItem.PropertiesDirect[PR_BODY,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'COMPLETED');
                      if Assigned(aField) then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($811c, PT_BOOLEAN, False, @PSETID_Task),ptBoolean] := aField.AsString = 'Y';
                      {aField := SyncItems.GetField(aJsonOutList[i],'DUEDATE');
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'STARTDATE');
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString);}
                      try
                        aItem.CoMessage.SaveChanges(0);
                        bFolder := TGenericFolder.Create(aConnection,aFolder.FEntryTyp);
                        bItem := bFolder.GetFirst;
                        while Assigned(bItem) and (bItem.Subject<>aItem.Subject) do
                          bItem := bFolder.GetNext;
                        if Assigned(bItem) then
                          begin
                            aExt := SyncItems.GetField(TJSONObject(aJsonOutList[i]),'EXTERNAL_ID');
                            if Assigned(aExt) then
                              begin
                                 TJSONObject(aJsonOutList[i]).Delete(TJSONObject(aJsonOutList[i]).IndexOf(aExt));
                              end;
                            debugln('Created:'+bItem.Subject+' '+EntryIdToString(bItem.EntryID));
                            TJSONObject(aJsonOutList[i]).Add('EXTERNAL_ID',EntryIdToString(bItem.EntryID));
                          end;
                        bItem.Free;
                        bFolder.Free;
                      except
                      end;
                      aItem.Free;
                    end;
                  TJSONObject(aJsonOutList.Items[i]).Add('item_is_synched',True);
                end;
            end;
          //tell System the new external_id´s and changed rows
          if aJsonOutList.Count>0 then
            SyncItems.SyncDataSet(aTasks,aJsonOutList,SyncType,True);
          //Delete Items in Outlook
          aItem := aFolder.GetFirst;
          bItem := nil;
          //change existing Items
          while Assigned(aItem) do
            begin
              i := 0;
              while i < aJsonOutList.Count do
                begin
                  DoDelete := False;
                  aField := SyncItems.GetField(aJsonOutList[i],'EXTERNAL_ID');
                  if Assigned(aField) and (aField.AsString = EntryIdToString(aItem.EntryID))
                  and (Assigned(SyncItems.GetField(aJsonOutList[i],'item_not_found')))
                  then
                    begin
                      aField := SyncItems.GetField(aJsonOutList[i],'SUMMARY');
                      debugln('Deleted:'+aItem.Subject);
                    end;
                  inc(i);
                end;
              FreeAndNil(aItem);
              aItem := aFolder.GetNext;
            end;
          aJsonOutList.Free;
        finally
          aTasks.Free;
          aFolder.Free;
        end;
      end;
    SyncItems.Free;
    aConnection.Free;
    Info('->all done.');
  except
    on e : Exception do
      begin
        aConnection.Free;
        Error('->Failed ('+e.Message);
      end;
  end;
  Application.Terminate
end;
function TGenericFolder.OpenDefaultStoreEID(const DefaultStoreEID: TSBinary
  ): Boolean;
var DefaultMessageStore: IMsgStore;
    QueryProperties: TSPropTagArray;
    lppPropArray: PSPropValue;
    lpcValues: Cardinal;
    lpEntryID: pEntryID;
    cbEntryID: Cardinal;
    lpulObjectType: Cardinal;
    OpenResult: Cardinal;
    FInbox : IMapiFolder;
    lpClass: pChar;
    ulObjectType: Cardinal;
begin
  Result := False;
  pointer(FFolder) := nil;
  pointer(FInbox) := nil;
  pointer(DefaultMessageStore) := nil;
  OpenResult := Session.OpenMsgStore(0, DefaultStoreEID.cb,
      PEntryID(DefaultStoreEID.lpb), IMsgStore,
      MAPI_BEST_ACCESS or MDB_NO_DIALOG or MDB_WRITE, DefaultMessageStore);
  if OpenResult <> S_OK then
    raise EMapiProperty.Create('Unable to open Msg Store', Session, OpenResult)
  else try
    if DefaultMessageStore.GetReceiveFolder(sMapiQuotedIpmNote, 0, cbEntryID, lpEntryID, lpClass) = S_OK then
    try
      if DefaultMessageStore.OpenEntry(cbEntryID, lpEntryID, IMapiFolder, MAPI_MODIFY, ulObjectType, IInterface(FInbox)) <> S_OK then
        pointer(FInbox) := nil
      else
        begin
          QueryProperties.cValues := 1;
          QueryProperties.aulPropTag[0] := FEntryTyp;
          if FInbox.GetProps(@QueryProperties, 0, lpcValues, lppPropArray) = S_OK then
          try
            if DefaultMessageStore.OpenEntry(lppPropArray^.Value.bin.cb, PENTRYID(lppPropArray^.Value.bin.lpb), IMapiFolder, MAPI_BEST_ACCESS or MAPI_MODIFY, lpulObjectType, IInterface(FFolder)) <> S_OK then
              FFolder := nil
            else Result := True;
          finally
            MapiFreeBuffer(lppPropArray);
          end;
        end;
    finally
      MAPIFreeBuffer(lpEntryID);
    end;
  finally
    DefaultMessageStore := nil;
  end;
end;
procedure TGenericFolder.PopulateContentItems;
begin
  inherited PopulateContentItems;
end;
constructor TGenericFolder.Create(const Connection: TMapiConnection;
  EntryTyp: Cardinal);
begin
  FEntryTyp := EntryTyp;
  inherited Create(Connection);
end;
function TSyncMSOApp.GetSingleInstance: Boolean;
begin
  Result:=False;
end;
constructor TSyncMSOApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

begin
  Application:=TSyncMSOApp.Create(nil);
  Application.Run;
  Application.Free;
end.

