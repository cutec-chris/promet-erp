program sync_outlook_calendar;
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
    procedure WritelnMessage(s : string);
    procedure WriteMessage(s : string);
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
  aFolder: TGenericFolder;
  aItem: TMapiMailItem;
  i: Integer;
  UserSelected: Boolean;
  SyncItems : TSyncItems;
  aCalendar: TCalendar;
  aPattern : string;
  RTFIsUpdated : Bool;
  StreamIntf: IStream;
  OLEStream: TOleStream;
  SStream : TStringStream;
var
  StreamInfo: TStatStg;
  MapiMessage: MapiDefs.IMessage;
  bItem: TMapiMailItem;
  DoDelete: Boolean;
  aJsonList: TJSONArray;
  aObj: TJSONObject;
  aStart,aEnd : TDateTime;
  aAllday: Boolean;
  sl: TStringList;
  aJsonOutList: TJSONArray;
  aField: TJSONData;
  bFolder: TGenericFolder;
  aLastSync: TDateTime;
begin
  try
    if not Login then raise Exception.Create('Login failed !');
    WritelnMessage('Datamodule open...');
    WritelnMessage('Connecting to MAPI...');
    SyncItems := TSyncItems.Create(nil,Data);
    SyncItems.CreateTable;
    SyncItems.Open;
    aConnection := TMapiConnection.Create('',True);
    WritelnMessage('->OK');
    if UserSelected then
      begin
        //get Timeoffset
        with Application as IBaseDBInterface do
          TimeOffset := DBConfig.ReadInteger('MSO_TIMEOFFSET',-1);
        if TimeOffset = -1 then
          begin
            WritelnMessage('hole Timeoffset...');
            aFolder := TGenericFolder.Create(aConnection,PR_IPM_APPOINTMENT_ENTRYID);
            if aFolder.Folder.CreateMessage(IMapiMessage, 0, MapiMessage) = S_OK then
              begin
                aItem := TMapiMailItem.Create(aFolder, MapiMessage, False);
                aItem.LastModificationTime:=Now();
                TimeOffset :=  round((Now()-aItem.LastModificationTime)*24);
                WritelnMessage('TimeOffset erstellt:'+IntToStr(TimeOffset));
                aItem.CoMessage.SaveChanges(0);
                TimeOffset :=  round((Now()-aItem.LastModificationTime)*24);
                WritelnMessage('TimeOffset geändert:'+IntToStr(TimeOffset));
                aItem.Delete;
                with Application as IBaseDBInterface do
                  DBConfig.WriteInteger('MSO_TIMEOFFSET',TimeOffset);
              end;
            aFolder.Free;
          end;
        WritelnMessage('TimeOffset:'+IntToStr(TimeOffset));
      end
    else TimeOffset:=0;
    if UserSelected then
      begin
        {
        // In PSETID_Appointment
        #define dispidSendAsICAL					0x8200	//PT_BOOLEAN
        #define dispidBusyStatus					0x8205	//PT_LONG
        #define dispidLocation						0x8208	//PT_STRING8
        #define dispidApptStartWhole				0x820D	//PT_SYSTIME
        #define dispidApptEndWhole					0x820E	//PT_SYSTIME
        #define dispidApptDuration					0x8213	//PT_LONG
        #define dispidLabel							0x8214	//PT_LONG
        #define dispidAllDayEvent					0x8215	//PT_BOOLEAN
        #define dispidRecurrenceState				0x8216	//PT_BINARY
        #define dispidMeetingStatus					0x8217	//PT_LONG
        #define dispidResponseStatus				0x8218	//PT_LONG
        #define dispidRecurring						0x8223	//PT_BOOLEAN
        #define dispidRecurringBase					0x8228	//PT_SYSTIME
        #define dispidRequestSent					0x8229	//PT_BOOLEAN
        #define dispidRecurrenceType				0x8231	//PT_LONG
        #define dispidRecurrencePattern				0x8232	//PT_STRING8
        #define dispidTimeZoneData					0x8233	//PT_BINARY - official name dispidTimeZoneStruct
        #define dispidTimeZone						0x8234	//PT_STRING8
        #define dispidRecurrenceStart				0x8235	//PT_SYSTIME
        #define dispidRecurrenceEnd					0x8236	//PT_SYSTIME
        #define dispidAllAttendeesString			0x8238	//PT_STRING8
        #define dispidToAttendeesString				0x823B	//PT_STRING8
        #define dispidCCAttendeesString				0x823C	//PT_STRING8
        #define dispidNetMeetingType				0x8241	//PT_LONG
        #define dispidNetMeetingServer				0x8242	//PT_STRING8
        #define dispidNetMeetingOrganizerAlias		0x8243	//PT_STRING8
        #define dispidNetMeetingAutoStart			0x8244	//PT_BOOLEAN
        #define dispidConferenceServerAllowExternal 0x8246	//PT_BOOLEAN
        #define dispidNetMeetingDocPathName			0x8247	//PT_STRING8
        #define dispidNetShowURL					0x8248	//PT_STRING8
        #define dispidConferenceServerPassword		0x8249	//PT_STRING8
        // A counter proposal is when the recipient of the request has proposed a new time for the meeting
        #define dispidApptCounterProposal			0x8257	//PT_BOOLEAN}
        WritelnMessage('Syncing Calendar...');
        //Aufgaben syncronisieren
        aFolder := TGenericFolder.Create(aConnection,PR_IPM_APPOINTMENT_ENTRYID);
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
                      aObj.Add('DESCR',SStream.DataString);
                  end;
              finally
                StreamIntf := nil;
              end;
              if not Assigned(aObj.Find('DESCR')) then
                aObj.Add('DESCR',EncodingIn(aItem.PropertiesDirect[PR_BODY,ptString]));
              aObj.Add('LOCATION',EncodingIn(aItem.PropertiesDirect[PR_LOCATION,ptString]));
              aStart := aItem.PropertiesDirect[aItem.GetPropertyDispId($820D, PT_SYSTIME, False, @PSETID_Appointment),ptTime];
              aEnd := aItem.PropertiesDirect[aItem.GetPropertyDispId($820E, PT_SYSTIME, False, @PSETID_Appointment),ptTime];
              aAllday := Boolean(aItem.PropertiesDirect[aItem.GetPropertyDispId($8215, PT_BOOLEAN, False, @PSETID_Appointment),ptBoolean]);
              if not aAllday then
                begin
                  aStart := IncHour(aStart,TimeOffset);
                  aEnd := IncHour(aEnd,TimeOffset);
                end;
              aObj.Add('ALLDAY',aAllday);
              aObj.Add('STARTDATE',Rfc822DateTime(aStart));
              aObj.Add('ENDDATE',Rfc822DateTime(aEnd));
              if aItem.PropertiesDirect[aItem.GetPropertyDispId($8223, PT_BOOLEAN, False, @PSETID_Appointment),ptBoolean] then  //PR_ISRECURRING
                begin
                  aPattern := aItem.PropertiesDirect[aItem.GetPropertyDispId($8232, PT_STRING8, False, @PSETID_Appointment),ptString];
                  case aItem.PropertiesDirect[aItem.GetPropertyDispId($8231, PT_LONG, False, @PSETID_Appointment),ptInteger] of //RecurenceType    (1=Täglich=1,2=Wöchentlich=2,3=Monatlich=4,4=Jährlich=6)
                  1:aObj.Add('ROTATION',1);
                  2:aObj.Add('ROTATION',2);
                  3:aObj.Add('ROTATION',4);
                  4:aObj.Add('ROTATION',6);
                  end;
                  aObj.Add('ROTTO',Rfc822DateTime(aItem.PropertiesDirect[aItem.GetPropertyDispId($8236, PT_SYSTIME, False, @PSETID_Appointment),ptTime]));
                end;
              aItem.Free;
              aJsonList.Add(aObj);
              aItem := aFolder.GetNext;
             end;

          aCalendar := TCalendar.Create(nil,Data);
          aLastSync := SyncItems.LastSync(SyncType,aCalendar.TableName);
          aCalendar.SelectByUser(Data.Users.Accountno.AsString);
          aCalendar.Open;
          //Sync Item List
          aJsonOutList := SyncItems.SyncDataSet(aCalendar,aJsonList,SyncType);
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
                  if Assigned(aField) and (aField.AsString = EntryIdToString(aItem.EntryID)) then
                    begin
                      aField := SyncItems.GetField(aJsonOutList[i],'SUMMARY');
                      if Assigned(aField) then
                        aItem.PropertiesDirect[PR_SUBJECT,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'DESCR');
                      if Assigned(aField) then
                        if aItem.PropertiesDirect[PR_BODY,ptString] = '' then
                          aItem.PropertiesDirect[PR_BODY,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'LOCATION');
                      if Assigned(aField) then
                        aItem.PropertiesDirect[PR_LOCATION,ptString] := EncodingOut(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'ALLDAY');
                      if Assigned(aField) then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8215, PT_BOOLEAN, False, @PSETID_Appointment),ptBoolean] := aField.AsString = 'Y';
                      aItem.LastModificationTime := Now;
                      try
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
          {
          for i := 0 to aJsonOutList.Count-1 do
            begin
              if Assigned(aJsonOutList[i])
              and (not Assigned(SyncItems.GetField(aJsonOutList[i],'item_is_synched')))
              then
                begin
                  if aFolder.Folder.CreateMessage(IMapiMessage, 0, MapiMessage) = S_OK then
                    begin
                      aItem := TMapiMailItem.Create(aFolder, MapiMessage, False);
                      aItem.MessageClass:='IPM.Appointment';
                      aField := SyncItems.GetField(aJsonOutList[i],'SUMMARY');
                      if Assigned(aField) then
                        aItem.PropertiesDirect[PR_SUBJECT,ptString] := EncodingOut(aField.AsString);

                      try
                        aItem.CoMessage.SaveChanges(0);
                        bFolder := TGenericFolder.Create(aConnection,aFolder.FEntryTyp);
                        bItem := bFolder.GetFirst;
                        debugln('Created:'+bItem.Subject);
                        if Assigned(bItem) then
                          TJSONObject(aJsonOutList[i]).Add('EXTERNAL_ID',EntryIdToString(bItem.EntryID));
                        bItem.Free;
                        bFolder.Free;
                      except
                      end;
                      aItem.Free;
                    end;

                end;
            end;
          }
          //tell System the new external_id´s and changed rows
          if aJsonOutList.Count>0 then
            SyncItems.SyncDataSet(aCalendar,aJsonOutList,SyncType,True);
          aJsonOutList.Free;
        finally
          aCalendar.Free;
          aFolder.Free;
        end;
      end;
    SyncItems.Free;
    aConnection.Free;
    WritelnMessage('->all done.');
  except
    on e : Exception do
      begin
        aConnection.Free;
        WritelnMessage('->Failed ('+e.Message);
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
procedure TSyncMSOApp.WritelnMessage(s: string);
begin
  debugln(s);
end;
procedure TSyncMSOApp.WriteMessage(s: string);
begin
  debugln(s);
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

