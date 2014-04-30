program sync_mso;
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
    function SyncProperty(aItem : TMapiMailItem;MapiProp : Cardinal;MapiType : TMapiPropertyType;DbField : TField;SyncOut : Boolean;Collect : Boolean) : Boolean;
    function SyncStrProperty(aItem : TMapiMailItem;MapiProp : WideString;NameSpace : PGUID;MapiType : TMapiPropertyType;DbField : TField;SyncOut : Boolean;Collect : Boolean) : Boolean;
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
function NoValue(V : Variant) : Boolean;
begin
  Result := True;
  if VarisType(V,varstring) and (VarToStr(V) <> '') then
    Result := False
  else if VarisType(V,vardate) and (VarToDateTime(V) <> Now()) then
    Result := False;
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
function TSyncMSOApp.SyncProperty(aItem: TMapiMailItem; MapiProp: Cardinal;
  MapiType: TMapiPropertyType; DbField: TField; SyncOut: Boolean;
  Collect: Boolean): Boolean;
begin
  if Collect then
    begin
      if (not (NoValue(aItem.PropertiesDirect[Mapiprop,MapiType]) and DbField.IsNull)) then
        begin
          if NoValue(aItem.PropertiesDirect[MapiProp,MapiType]) then
            begin
              if MapiType = ptString then
                aItem.PropertiesDirect[MapiProp,MapiType] := EncodingOut(DbField.AsString)
              else if MapiType = ptTime then
                aItem.PropertiesDirect[MapiProp,MapiType] := DateUtils.IncHour(DbField.AsDateTime,-TimeOffset)//TODO:LocaltimeToGMT
              else
                aItem.PropertiesDirect[MapiProp,MapiType] := DbField.AsVariant;
            end
          else
            begin
              if (DbField.DataSet.State <> dsEdit) and (DbField.DataSet.State <> dsInsert) then
                DbField.DataSet.Edit;
              if MapiType = ptString then
                DbField.AsString:=EncodingIn(aItem.PropertiesDirect[MapiProp,MapiType])
              else if MapiType = ptTime then
                DbField.AsDateTime := IncHour(aItem.PropertiesDirect[MapiProp,MapiType],TimeOffset)//TODO:GMTToLocaltime
              else
                DbField.AsVariant:=aItem.PropertiesDirect[MapiProp,MapiType];
            end;
        end;
    end
  else if SyncOut then
    begin
      if not DbField.IsNull then
        begin
          if MapiType = ptString then
            aItem.PropertiesDirect[MapiProp,MapiType] := EncodingOut(DbField.AsString)
          else if MapiType = ptTime then
            aItem.PropertiesDirect[MapiProp,MapiType] := DateUtils.IncHour(DbField.AsDateTime,-TimeOffset)//TODO:LocaltimeToGMT
          else
            aItem.PropertiesDirect[MapiProp,MapiType] := DbField.AsVariant;
        end;
    end
  else
    begin
      if (DbField.DataSet.State <> dsEdit) and (DbField.DataSet.State <> dsInsert) then
        DbField.DataSet.Edit;
      if MapiType = ptString then
        begin
          if not (NoValue(aItem.PropertiesDirect[MapiProp,MapiType])) then
            DbField.AsString:=EncodingIn(aItem.PropertiesDirect[MapiProp,MapiType])
        end
      else if MapiType = ptTime then
        DbField.AsDateTime := IncHour(aItem.PropertiesDirect[MapiProp,MapiType],TimeOffset)//TODO:GMTToLocaltime
      else
        DbField.AsVariant:=aItem.PropertiesDirect[MapiProp,MapiType];
    end;
end;
function TSyncMSOApp.SyncStrProperty(aItem: TMapiMailItem;
  MapiProp: WideString; NameSpace: PGUID; MapiType: TMapiPropertyType;
  DbField: TField; SyncOut: Boolean; Collect: Boolean): Boolean;
var
  PropId: LongWord;
begin
//  PropId := aItem.GetPropertyId(MapiProp, PT_STRING8, True, NameSpace);
  PropId := aItem.GetPropertyId(MapiProp, PT_STRING8, False, NameSpace);
  if Collect then
    begin
      if (not (NoValue(aItem.PropertiesDirect[PropId,MapiType]) and DbField.IsNull)) then
        begin
          if NoValue(aItem.PropertiesDirect[PropId,MapiType]) then
            aItem.PropertiesDirect[PropId,MapiType] := DbField.AsVariant
          else
            DbField.AsVariant:=aItem.PropertiesDirect[PropId,MapiType];
        end;
    end
  else if SyncOut then
    aItem.PropertiesDirect[PropId,MapiType] := DbField.AsVariant
  else
    DbField.AsVariant:=aItem.PropertiesDirect[PropId,MapiType];
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
  function RoundToSecond(aDate : TDateTime) : TDateTime;
  begin
    Result := Round(aDate * SecsPerDay) / SecsPerDay;
  end;
  procedure SyncContactItem(CustomerCont : TPersonContactData;aSyncTyp : Cardinal;SyncItemtype : TMapiPropertyType;Itemtype : string;Itemtype2 : string = '';Itemtype3 : string = '');
  var
    bFound: Boolean;
    i: Integer;
    bSynced: Boolean;
  begin
    //find coresponding Entry
    with CustomerCont.DataSet do
      begin
        First;
        bSynced := False;
        while not EOF do
          begin
            Data.SetFilter(SyncItems,Data.QuoteField('LOCAL_ID')+'='+Data.QuoteValue(IntToStr(Data.GetBookmark(CustomerCont))));
            if (Data.Locate(SyncItems,'REMOTE_ID',VarArrayOf([aSyncTyp]),[]))
            or ((SyncItems.Count = 0) and ( //Nur wenn zu LocalID keine REMOTE_ID vorhanden
                 (trim(FieldByName('TYPE').AsString) = ItemType)
              or (trim(FieldByName('TYPE').AsString) = ItemType2)
              or (trim(FieldByName('TYPE').AsString) = ItemType3)
              ))
            then
              begin
                bFound := False;
                if (Data.Locate(SyncItems,'REMOTE_ID',VarArrayOf([aSyncTyp]),[])) then
                  for i := 0 to length(SQLSynced)-1 do
                    if SQLSynced[i] = Data.GetBookmark(CustomerCont) then
                      bFound := True;
                if not bFound then //We have an Entry to use
                  begin
                    if SyncOut or (Collect and (not NoValue(aItem.PropertiesDirect[aSyncTyp,SyncItemtype]))) then
                      begin
                        if SyncItemtype = ptString then
                          aItem.PropertiesDirect[aSyncTyp,SyncItemType] := EncodingOut(FieldByName('DATA').AsString)
                        else
                          aItem.PropertiesDirect[aSyncTyp,SyncItemType] := FieldByName('DATA').AsVariant;
                        bSynced := True;
                      end
                    else if not NoValue(aItem.PropertiesDirect[aSyncTyp,SyncItemtype]) then
                      begin
                        if State <> dsEdit then
                          Edit;
                        if (not SyncOut) or (Collect and (FieldByName('DATA').IsNull)) then
                          begin
                            if SyncItemtype = ptString then
                              FieldByName('DATA').AsString := EncodingIn(aItem.PropertiesDirect[aSyncTyp,SyncItemtype])
                            else
                              FieldByName('DATA').AsVariant := aItem.PropertiesDirect[aSyncTyp,SyncItemtype];
                          end;
                        bSynced := True;
                      end;
                    if bSynced then
                      begin
                        if not (Data.Locate(SyncItems,'REMOTE_ID',VarArrayOf([aSyncTyp]),[])) then
                          with SyncItems.DataSet do
                            begin
                              Insert;
                              FieldByName('SYNCTYPE').AsString:=SyncType;
                              FieldByName('REMOTE_ID').AsString:=IntToStr(aSyncTyp);
                              FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(CustomerCont);
                              FieldByName('TIMESTAMPD').AsDateTime:=Now();
                              Post;
                            end;
                        SetLength(MAPISynced,length(MAPISynced)+1);
                        MAPISynced[length(MAPISynced)-1] := aSyncTyp;
                        SetLength(SQLSynced,length(SQLSynced)+1);
                        SQLSynced[length(SQLSynced)-1] := Data.GetBookmark(CustomerCont);
                      end;
                    break;
                  end;
              end;
            Next;
          end;
        if (not bSynced) and (not SyncOut) and (not NoValue(aItem.PropertiesDirect[aSyncTyp,SyncItemType])) then
          begin
            Insert;
            FieldByName('TYPE').AsString:=Itemtype;
            if SyncItemType = ptString then
              FieldByName('DATA').AsString:=EncodingIn(aItem.PropertiesDirect[aSyncTyp,ptString])
            else
              FieldByName('DATA').AsVariant:=aItem.PropertiesDirect[aSyncTyp,SyncItemType];
            Post;
            with SyncItems.DataSet do
              begin
                Insert;
                FieldByName('SYNCTYPE').AsString:=SyncType;
                FieldByName('REMOTE_ID').AsString:=IntToStr(aSyncTyp);
                FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(CustomerCont);
                FieldByName('TIMESTAMPD').AsDateTime:=Now();
                Post;
              end;
          end;
{
        if SyncOut then
          begin
            Data.SetFilter(Data.SyncItems,'"REMOTE_ID"='+Data.QuoteValue(IntToStr(aSyncTyp)));
            if not Data.Locate(Data.CustomerCont,'SQL_ID',VarArrayOf([Data.SyncItems.FieldByName('LOCAL_ID').AsVariant]),[]) then
              if not NoValue(aItem.PropertiesDirect[aSyncTyp,SyncItemType]) then
                begin
                  if SyncItemType = ptString then
                    begin
                      aItem.PropertiesDirect[aSyncTyp,SyncItemType] := '';
                      Data.SyncItems.Delete;
                    end;

                end;
          end;
          }
      end;
  end;
  procedure SyncAddressItem(Addresses : TBaseDbAddress;aSyncCountry,aSyncCity,aSyncStreet,aSyncZip : Cardinal;Itemtype : string;Itemtype2 : string = '';Itemtype3 : string = '');
  var
    bFound: Boolean;
    i: Integer;
    bSynced: Boolean;
  begin
    //find coresponding Entry
    with Addresses.DataSet do
      begin
        First;
        bSynced := False;
        while not EOF do
          begin
            Data.SetFilter(SyncItems,'"LOCAL_ID"='+Data.QuoteValue(IntToStr(Data.GetBookmark(Addresses))));
            if (Data.Locate(SyncItems,'REMOTE_ID',VarArrayOf([aSyncStreet]),[]))
            or (trim(FieldByName('TYPE').AsString) = ItemType)
            or (trim(FieldByName('TYPE').AsString) = ItemType2)
            or (trim(FieldByName('TYPE').AsString) = ItemType3)
            then
              begin
                bFound := False;
                if (Data.Locate(SyncItems,'REMOTE_ID',VarArrayOf([aSyncStreet]),[])) then
                  for i := 0 to length(SQLSynced)-1 do
                    if SQLSynced[i] = Data.GetBookmark(Addresses) then
                      bFound := True;
                if not bFound then //We have an Entry to use
                  begin
                    if Syncout or Collect then
                      begin
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncCountry,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncCountry,ptString] := EncodingOut(FieldByName('COUNTRY').AsString);
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncCity,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncCity,ptString] := EncodingOut(FieldByName('CITY').AsString);
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncStreet,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncStreet,ptString] := EncodingOut(FieldByName('ADDRESS').AsString);
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncZip,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncZip,ptString] := EncodingOut(FieldByName('ZIP').AsString);
                        bSynced := True;
                      end
                    else if aItem.PropertiesDirect[aSyncStreet,ptString] <> '' then
                      begin
                        if State <> dsEdit then
                          Edit;
                        if (not SyncOut) or (Collect and (FieldByName('COUNTRY').IsNull)) then
                          FieldByName('COUNTRY').AsString := EncodingOut(aItem.PropertiesDirect[aSyncCountry,ptString]);
                        if (not SyncOut) or (Collect and (FieldByName('CITY').IsNull)) then
                          FieldByName('CITY').AsString := EncodingOut(aItem.PropertiesDirect[aSyncCity,ptString]);
                        if (not SyncOut) or (Collect and (FieldByName('ADDRESS').IsNull)) then
                          FieldByName('ADDRESS').AsString := EncodingOut(aItem.PropertiesDirect[aSyncStreet,ptString]);
                        if (not SyncOut) or (Collect and (FieldByName('ZIP').IsNull)) then
                          FieldByName('ZIP').AsString := EncodingOut(aItem.PropertiesDirect[aSyncZip,ptString]);
                        bSynced := True;
                      end;
                    if bSynced then
                      begin
                        if not (Data.Locate(SyncItems,'REMOTE_ID',VarArrayOf([aSyncStreet]),[])) then
                          with SyncItems.DataSet do
                            begin
                              Insert;
                              FieldByName('SYNCTYPE').AsString:=SyncType;
                              FieldByName('REMOTE_ID').AsString:=IntToStr(aSyncStreet);
                              FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(Addresses);
                              FieldByName('TIMESTAMPD').AsDateTime:=Now();
                              Post;
                            end;
                        SetLength(MAPISynced,length(MAPISynced)+1);
                        MAPISynced[length(MAPISynced)-1] := aSyncStreet;
                        SetLength(SQLSynced,length(SQLSynced)+1);
                        SQLSynced[length(SQLSynced)-1] := Data.GetBookmark(Addresses);
                      end;
                    break;
                  end;
              end;
            Next;
          end;
        if (not bSynced) and (not SyncOut) and (aItem.PropertiesDirect[aSyncStreet,ptString] <> '') then
          begin
            Insert;
            FieldByName('TYPE').AsString:=Itemtype;
            FieldByName('NAME').AsString:=EncodingIn(aItem.PropertiesDirect[PR_DISPLAY_NAME,ptString]);
            FieldByName('COUNTRY').AsString:=EncodingIn(aItem.PropertiesDirect[aSyncCountry,ptString]);
            FieldByName('CITY').AsString:=EncodingIn(aItem.PropertiesDirect[aSyncCity,ptString]);
            FieldByName('ADDRESS').AsString:=EncodingIn(aItem.PropertiesDirect[aSyncStreet,ptString]);
            FieldByName('ZIP').AsString:=EncodingIn(aItem.PropertiesDirect[aSyncZip,ptString]);
            Post;
            with SyncItems.DataSet do
              begin
                Insert;
                FieldByName('SYNCTYPE').AsString:=SyncType;
                FieldByName('REMOTE_ID').AsString:=IntToStr(aSyncStreet);
                FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(Addresses);
                FieldByName('TIMESTAMPD').AsDateTime:=Now();
                Post;
              end;
          end;
      end;
  end;
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
    if Application.HasOption('synccontacts') then
      begin
        WritelnMessage('Syncing Contacts...');
        //Kontakte syncronisieren
        aFolder := TGenericFolder.Create(aConnection,PR_IPM_CONTACT_ENTRYID);
        try
          aItem := aFolder.GetFirst;
          while Assigned(aItem) do
          begin
            SyncOut := False;
            Collect := False;
            DoSync := True;
            aID := 0;
            Data.SetFilter(SyncItems,Data.QuoteField('SYNCTYPE')+'='+Data.QuoteValue(Synctype)+' AND '+Data.QuoteField('REMOTE_ID')+'='+Data.QuoteValue(EntryIdToString(aItem.EntryID)));
            aContact := TPerson.Create(nil,Data);
            while SyncItems.Count > 0 do
              begin
                DoSync := (not SyncItems.DataSet.FieldByName('LOCAL_ID').IsNull) and (not SyncItems.DataSet.FieldByName('LOCAL_ID').AsInteger = 0);
                if DoSync then
                  begin
                    aContact.Select(SyncItems.DataSet.FieldByName('LOCAL_ID').AsVariant);
                    aContact.Open;
                    if aContact.Count > 0 then
                      begin
                        DoSync := DoSync and (not (RoundToSecond(IncHour(aItem.LastModificationTime,TimeOffset)) = RoundToSecond(aContact.Timestamp.AsDateTime)));
                        aID := aContact.Id.AsVariant;
                        break;
                      end
                    else
                      begin
                        SyncItems.Delete;
                      end;
                  end
                else break;
              end;
            if (aID = 0) and DoSync then
              begin
                tmp := StringReplace(EncodingIn(aItem.Subject),' ','*',[rfreplaceAll]);
                tmp1 := StringReplace(EncodingIn(aItem.PropertiesDirect[(PT_TSTRING) or ($802B shl 16),ptString]),' ','*',[rfreplaceAll]);
                if tmp1 = '' then
                  tmp1 := aItem.Subject;
                Data.SetFilter(aContact,Data.ProcessTerm('NAME='+Data.QuoteValue(tmp))+' OR '+Data.ProcessTerm('NAME='+Data.QuoteValue(tmp1)));
                aContact.DataSet.First;
                if tmp <> '' then
                  while not aContact.DataSet.EOF do
                    begin
                      if MessageDlg(strCollect,Format(strCollectItems,[aItem.Subject,aContact.DataSet.FieldByName('NAME').AsString]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
                        begin
                          Collect := True;
                          aID := aContact.Id.AsVariant;
                          with SyncItems.DataSet do
                            begin
                              Insert;
                              FieldByName('SYNCTYPE').AsString:=SyncType;
                              FieldByName('REMOTE_ID').AsString:=EntryIdToString(aItem.EntryID);
                              FieldByName('LOCAL_ID').AsVariant:=aContact.Id.AsVariant;
                              FieldByName('TIMESTAMPD').AsDateTime:=Now();
                              Post;
                            end;
                          break;
                        end;
                      aContact.DataSet.Next;
                    end;
                if (not Collect) and (aContact.Count > 0) then
                  begin
                    with SyncItems.DataSet do
                      begin
                        Insert;
                        FieldByName('SYNCTYPE').AsString:=SyncType;
                        FieldByName('REMOTE_ID').AsString:=EntryIdToString(aItem.EntryID);
                        FieldByName('LOCAL_ID').AsString := '0';
                        FieldByName('TIMESTAMPD').AsDateTime:=Now();
                        Post;
                      end;
                  end;
              end;
            if aID <> 0 then
              begin
                aContact.Select(aID);
                aContact.Open;
                if RoundToSecond(IncHour(aItem.LastModificationTime,TimeOffset)) < RoundToSecond(aContact.TimeStamp.AsDateTime) then
                  SyncOut := True;
              end;
            if (aID = 0) and DoSync then
              begin
    //            if MessageDlg(strCollect,Format(strSyncItems,[EncodingIn(aItem.Subject)]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
                  begin
                    aContact.Append;
                    aContact.Text.AsString := '';
                    aContact.DataSet.Post;
                    aID := aContact.Id.AsVariant;
                    SyncOut := False;
                    with SyncItems.DataSet do
                      begin
                        Insert;
                        FieldByName('SYNCTYPE').AsString:=SyncType;
                        FieldByName('REMOTE_ID').AsString:=EntryIdToString(aItem.EntryID);
                        FieldByName('LOCAL_ID').AsVariant:=aContact.DataSet.FieldByName('SQL_ID').AsVariant;
                        FieldByName('TIMESTAMPD').AsDateTime:=Now();
                        Post;
                      end;
                  end
    {
                else
                  begin
                    with SyncItems.DataSet do
                      begin
                        Insert;
                        FieldByName('SYNCTYPE').AsString:=SyncType;
                        FieldByName('REMOTE_ID').AsString:=EntryIdToString(aItem.EntryID);
                        FieldByName('LOCAL_ID').AsString := '0';
                        FieldByName('TIMESTAMPD').AsDateTime:=Now();
                        Post;
                      end;
                  end;
    }
              end;
            if (aID <> 0) and DoSync then
              begin
                aContact.CustomerCont.Open;
                SyncProperty(aItem,PR_SUBJECT,ptString,aContact.DataSet.FieldByName('NAME'),SyncOut,Collect);
                WritelnMessage('Syncing '+aContact.DataSet.FieldByName('NAME').AsString+' ... ');
                if Syncout and (not Collect) then
                  WritelnMessage('< ')
                else if Collect then
                  WritelnMessage('<>')
                else
                  WritelnMessage(' >');
                SyncProperty(aItem,PR_BODY,ptString,aContact.DataSet.FieldByName('INFO'),SyncOut,Collect);
                if SyncOut or Collect then
                  SyncProperty(aItem,PR_CUSTOMER_ID,ptString,aContact.DataSet.FieldByName('ACCOUNTNO'),True,False);
                //Addresses
                aContact.Address.Open;
                SyncAddressItem(aContact.Address,
                                PR_BUSINESS_ADDRESS_COUNTRY,
                                PR_BUSINESS_ADDRESS_CITY,
                                PR_BUSINESS_ADDRESS_STREET,
                                PR_BUSINESS_ADDRESS_POSTAL_CODE,'IAD','DAD','BAD');
                SyncAddressItem(aContact.Address,
                                PR_OTHER_ADDRESS_COUNTRY,
                                PR_OTHER_ADDRESS_CITY,
                                PR_OTHER_ADDRESS_STREET,
                                PR_OTHER_ADDRESS_POSTAL_CODE,'IAD','DAD','BAD');
                SyncAddressItem(aContact.Address,
                                PR_HOME_ADDRESS_COUNTRY,
                                PR_HOME_ADDRESS_CITY,
                                PR_HOME_ADDRESS_STREET,
                                PR_HOME_ADDRESS_POSTAL_CODE,'PAD','');
                //Contact Items
                SetLength(MAPISynced,0);
                SyncContactItem(aContact.CustomerCont,(PT_TSTRING) or ($8029 shl 16),ptString,'MAIL','MLB');
                SyncContactItem(aContact.CustomerCont,(PT_TSTRING) or ($8028 shl 16),ptString,'MAIL','MLB');
                SyncContactItem(aContact.CustomerCont,(PT_TSTRING) or ($8027 shl 16),ptString,'MAIL','MLB');
                SyncContactItem(aContact.CustomerCont,PR_HOME_TELEPHONE_NUMBER,ptString,'TELP','TEL');
                SyncContactItem(aContact.CustomerCont,PR_HOME2_TELEPHONE_NUMBER,ptString,'TELP','TEL');
                SyncContactItem(aContact.CustomerCont,PR_BUSINESS_TELEPHONE_NUMBER,ptString,'TELB');
                SyncContactItem(aContact.CustomerCont,PR_BUSINESS2_TELEPHONE_NUMBER,ptString,'TELB');
                SyncContactItem(aContact.CustomerCont,PR_MOBILE_TELEPHONE_NUMBER,ptString,'CEL');
                SyncContactItem(aContact.CustomerCont,PR_OTHER_TELEPHONE_NUMBER,ptString,'TEL');
                SyncContactItem(aContact.CustomerCont,PR_NICKNAME,ptString,'NICK');
                SyncContactItem(aContact.CustomerCont,PR_BIRTHDAY,ptTime,'BIR');
                SyncContactItem(aContact.CustomerCont,PR_BUSINESS_HOME_PAGE,ptString,'INT');
                SyncContactItem(aContact.CustomerCont,PR_PERSONAL_HOME_PAGE,ptString,'INT');

                if SyncOut or Collect then
                  begin
                    aContact.Images.Open;
                    aContact.Images.DataSet.First;
                    if aContact.Images.Count > 0 then
                      begin
                        aFound := False;
                        for i := 0 to aItem.Attachments.Count-1 do
                          begin
                            if aItem.Attachments[i].FileName = 'ContactPicture.jpg' then
                              aFound := True;
                          end;
                        if not aFound then
                          begin
                            ms := TMemoryStream.Create;
                            Data.BlobFieldToStream(aContact.Images.DataSet,'IMAGE',ms);
                            ms.Position:=0;
                            tmp := ms.ReadAnsiString;
                            aPicture := TPicture.Create;
                            aPicture.LoadFromStreamWithFileExt(ms,tmp);
                            ms.Free;
                            ms := TmemoryStream.Create;
                            aPicture.SaveToStreamWithFileExt(ms,'jpg');
                            ms.Position:=0;
                            aItem.Attachments.Add('ContactPicture.jpg',ms,True);
                            aItem.PropertiesDirect[(PT_BOOLEAN) or ($80BF shl 16),ptBoolean] := True; //HasPicture
                            ms.Free;
                          end;
                      end;
                  end
                else if (not SyncOut) and (aItem.Attachments.Count > 0) then
                  begin
                    aContact.Images.Open;
                    aContact.Images.DataSet.First;
                    for i := 0 to aItem.Attachments.Count-1 do
                      begin
                        if aItem.Attachments[i].FileName = 'ContactPicture.jpg' then
                          begin
                            if aContact.Images.DataSet.RecordCount = 0 then
                              aContact.Images.DataSet.Insert
                            else aContact.Images.DataSet.Edit;
                            aPicture := TPicture.Create;
                            ms := TMemoryStream.Create;
                            aItem.Attachments[i].Copy(ms);
                            ms.Position:=0;
                            aPicture.LoadFromStreamWithFileExt(ms,'.jpg');
                            ms.Free;
                            ms := TMemoryStream.Create;
                            try
                              ms.WriteAnsiString('.jpg');
                              aPicture.Graphic.SaveToStream(ms);
                              Data.StreamToBlobField(ms,aContact.Images.DataSet,'IMAGE');
                            finally
                              aPicture.Free;
                              ms.Free;
                            end;
                            aContact.Images.DataSet.Post;
                          end;
                      end;
                    aContact.Images.DataSet.Close;
                  end;

                if (aContact.DataSet.State <> dsEdit) then
                  aContact.DataSet.Edit;
                aContact.DataSet.Post;
                aItem.CoMessage.SaveChanges(0);
              end;
            aItem.Free;
            aItem := aFolder.GetNext;
          end;
        finally
          aFolder.Free;
          aContact.Free;
        end;
    end;
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
        //Termine syncronisieren
        aCalendar := TCalendar.Create(nil,Data);
        aFolder := TGenericFolder.Create(aConnection,PR_IPM_APPOINTMENT_ENTRYID);
        //Build Appointment List
        aJsonList := TJSONArray.Create;
        try
          aItem := aFolder.GetFirst;
          while Assigned(aItem) do
            begin
              aPrivate := Boolean(aItem.PropertiesDirect[aItem.GetPropertyDispId($8506, PT_BOOLEAN, False, @PSETID_Common),ptBoolean]);
              if not aPrivate then
                begin
                  aObj := TJSONObject.Create;
                  aObj.Add('EXTERNAL_ID',EntryIdToString(aItem.EntryID));
                  aObj.Add('PRIVATE',aPrivate);
                  aObj.Add('TIMESTAMPD',Rfc822DateTime(aItem.LastModificationTime));
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
                end;
              aItem := aFolder.GetNext;
            end;
          aCalendar.SelectByUser(Data.Users.Accountno.AsString);
          aCalendar.Open;

          aJsonOutList := SyncItems.SyncDataSet(aCalendar,aJsonList,SyncType);

          aJsonList.Free;
          aItem := aFolder.GetFirst;
          //change existing Items
          while Assigned(aItem) do
            begin
              i := 0;
              while i < aJsonOutList.Count-1 do
                begin
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
                      aJsonOutList.Delete(i);
                      break;
                    end
                  else inc(i);
                end;
              aItem.Free;
              aItem := aFolder.GetNext;
            end;
          //Create new Items
          for i := 0 to aJsonOutList.Count-1 do
            begin
              //TODO:add new Calendar entrys
              if Assigned(aJsonOutList[i]) then
                begin

                end;
            end;
          aJsonOutList.Free;
        finally
          aCalendar.Free;
          aFolder.Free;
        end;

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


        WritelnMessage('Syncing Tasks');
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
              aObj.Add('TIMESTAMPD',Rfc822DateTime(aItem.LastModificationTime));
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
          aLastSync := SyncItems.LastSync(SyncType);
          if aLastSync>0 then
            aTasks.SelectByUserChangedSince(Data.Users.Accountno.AsString,aLastSync)
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
                  if Assigned(aField) and (aField.AsString = EntryIdToString(aItem.EntryID)) then
                    begin
                      aField := SyncItems.GetField(aJsonOutList[i],'SUMMARY');
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
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString)
                      else aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime] := -1;
                      aField := SyncItems.GetField(aJsonOutList[i],'STARTDATE');
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString)
                      else aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime] := -1;
                      try
                        if DoDelete then aItem.Delete; //Delete Done Tasks couse we cant set them correctly done at time
                        aItem.CoMessage.SaveChanges(0);
                      except
                      end;
                      aJsonOutList.Delete(i);
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
              if Assigned(aJsonOutList[i]) and ((not Assigned(SyncItems.GetField(aJsonOutList[i],'HASCHILDS'))) or (SyncItems.GetField(aJsonOutList[i],'HASCHILDS').AsString<>'Y')) then
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
                      aField := SyncItems.GetField(aJsonOutList[i],'DUEDATE');
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString);
                      aField := SyncItems.GetField(aJsonOutList[i],'STARTDATE');
                      if Assigned(aField) and (aField.AsString<>'') then
                        aItem.PropertiesDirect[aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime] := DecodeRfcDateTime(aField.AsString);
                      try
                        aItem.CoMessage.SaveChanges(0);
                        bFolder := TGenericFolder.Create(aConnection,aFolder.FEntryTyp);
                        bItem := bFolder.GetFirst;
                        debugln(bItem.Subject);
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
          //tell System the new external_id´s
          if aJsonOutList.Count>0 then
            SyncItems.SyncDataSet(aTasks,aJsonOutList,SyncType);
          aJsonOutList.Free;
        finally
          aTasks.Free;
          aFolder.Free;
        end;
      end;
{
    //Nachrichten Syncronisieren
    InBox := TInBox.Create(aConnection);
    try
      MailItem := InBox.GetFirst;
      while Assigned(MailItem) do
      begin
        WritelnMessage('From: ' + MailItem.Sender + #13#10 +
                    'Subject: ' + MailItem.Subject);
        MailItem.Free;
        MailItem := InBox.GetNext;
      end;
    finally
      InBox.Free;
    end;
}
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

