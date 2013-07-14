unit uSyncMSO;

{$mode objfpc}

interface

uses
  Classes, windows, SysUtils, FileUtil, Forms, uData, Dialogs, MapiServices,
  MapiDefs, MapiTags, MapiUtil, MapiGUID, Controls, Math, db, Variants,
  Graphics, DateUtils, types, uBaseDbInterface, uIntfStrConsts, LConvEncoding,
  comobj, ActiveX;

const SyncType = 'MAPI (Outlook)';

type

  { TfSync }

  TfSync = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    aConnection: TMapiConnection;
    function SyncProperty(aItem : TMapiMailItem;MapiProp : Cardinal;MapiType : TMapiPropertyType;DbField : TField;SyncOut : Boolean;Collect : Boolean) : Boolean;
    function SyncStrProperty(aItem : TMapiMailItem;MapiProp : WideString;NameSpace : PGUID;MapiType : TMapiPropertyType;DbField : TField;SyncOut : Boolean;Collect : Boolean) : Boolean;
  public
    { public declarations }
    procedure WritelnMessage(s : string);
    procedure WriteMessage(s : string);
  end;

  { TGenericFolder }

  TGenericFolder = class(TInBox)
  private
    FEntryTyp : Cardinal;
  protected
    function OpenDefaultStoreEID(const DefaultStoreEID: TSBinary): Boolean; override;
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

var
  fSync: TfSync;
implementation
{$R *.lfm}
uses uPerson,uSync,uCalendar,LCLProc,uTask;
function NoValue(V : Variant) : Boolean;
begin
  Result := True;
  if VarisType(V,varstring) and (VarToStr(V) <> '') then
    Result := False
  else if VarisType(V,vardate) and (VarToDateTime(V) <> Now()) then
    Result := False;
end;

{ TfSync }

procedure TfSync.DataModuleCreate(Sender: TObject);
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
                          aItem.PropertiesDirect[aSyncTyp,SyncItemType] := UTF8ToSys(FieldByName('DATA').AsString)
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
                              FieldByName('DATA').AsString := SysToUTF8(aItem.PropertiesDirect[aSyncTyp,SyncItemtype])
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
              FieldByName('DATA').AsString:=SysToUTF8(aItem.PropertiesDirect[aSyncTyp,ptString])
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
                          aItem.PropertiesDirect[aSyncCountry,ptString] := UTF8ToSys(FieldByName('COUNTRY').AsString);
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncCity,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncCity,ptString] := UTF8ToSys(FieldByName('CITY').AsString);
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncStreet,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncStreet,ptString] := UTF8ToSys(FieldByName('ADDRESS').AsString);
                        if SyncOut or (Collect and (aItem.PropertiesDirect[aSyncZip,ptString] = '')) then
                          aItem.PropertiesDirect[aSyncZip,ptString] := UTF8ToSys(FieldByName('ZIP').AsString);
                        bSynced := True;
                      end
                    else if aItem.PropertiesDirect[aSyncStreet,ptString] <> '' then
                      begin
                        if State <> dsEdit then
                          Edit;
                        if (not SyncOut) or (Collect and (FieldByName('COUNTRY').IsNull)) then
                          FieldByName('COUNTRY').AsString := SysToUTF8(aItem.PropertiesDirect[aSyncCountry,ptString]);
                        if (not SyncOut) or (Collect and (FieldByName('CITY').IsNull)) then
                          FieldByName('CITY').AsString := SysToUTF8(aItem.PropertiesDirect[aSyncCity,ptString]);
                        if (not SyncOut) or (Collect and (FieldByName('ADDRESS').IsNull)) then
                          FieldByName('ADDRESS').AsString := SysToUTF8(aItem.PropertiesDirect[aSyncStreet,ptString]);
                        if (not SyncOut) or (Collect and (FieldByName('ZIP').IsNull)) then
                          FieldByName('ZIP').AsString := SysToUTF8(aItem.PropertiesDirect[aSyncZip,ptString]);
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
            FieldByName('NAME').AsString:=SysToUTF8(aItem.PropertiesDirect[PR_DISPLAY_NAME,ptString]);
            FieldByName('COUNTRY').AsString:=SysToUTF8(aItem.PropertiesDirect[aSyncCountry,ptString]);
            FieldByName('CITY').AsString:=SysToUTF8(aItem.PropertiesDirect[aSyncCity,ptString]);
            FieldByName('ADDRESS').AsString:=SysToUTF8(aItem.PropertiesDirect[aSyncStreet,ptString]);
            FieldByName('ZIP').AsString:=SysToUTF8(aItem.PropertiesDirect[aSyncZip,ptString]);
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
  with Application,Application as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
      if not HasOption('m','mandant') then
        raise Exception.Create(strMandantnotSelected);
      if not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user')) then
        raise Exception.Create(strLoginFailed);
      uData.Data := Data;
    end;
  SyncItems := TSyncItems.Create(nil,Data);
  SyncItems.CreateTable;
  SyncItems.Open;
  WritelnMessage('Datamodule open...');
  WriteMessage('Connecting to MAPI...');
  try
    aConnection := TMapiConnection.Create('',True);
    WritelnMessage('->OK');
    WriteMessage('Syncing Contacts...');
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
                    DoSync := DoSync and (not (roundto(IncHour(aItem.LastModificationTime,1),-4) = roundto(aContact.Timestamp.AsDateTime,-4)));
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
            tmp := StringReplace(SysToUTF8(aItem.Subject),' ','*',[rfreplaceAll]);
            tmp1 := StringReplace(SysToUTF8(aItem.PropertiesDirect[(PT_TSTRING) or ($802B shl 16),ptString]),' ','*',[rfreplaceAll]);
            if tmp1 = '' then
              tmp1 := aItem.Subject;
            with aContact.DataSet as IBaseDbFilter do
              Data.SetFilter(aContact,ProcessTerm('NAME='+Data.QuoteValue(tmp))+' OR '+ProcessTerm('NAME='+Data.QuoteValue(tmp1)));
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
            if roundto(IncHour(aItem.LastModificationTime,1),-4) < roundto(aContact.TimeStamp.AsDateTime,-4) then
              SyncOut := True;
          end;
        if (aID = 0) and DoSync then
          begin
            if MessageDlg(strCollect,Format(strSyncItems,[SysToUTF8(aItem.Subject)]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
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
          end;
        if (aID <> 0) and DoSync then
          begin
            aContact.CustomerCont.Open;
            SyncProperty(aItem,PR_SUBJECT,ptString,aContact.DataSet.FieldByName('NAME'),SyncOut,Collect);
            WriteMessage('Syncing '+aContact.DataSet.FieldByName('NAME').AsString+' ... ');
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

    if UserSelected then
      begin
        WriteMessage('Syncing Calendar...');
        //Termine syncronisieren
        aCalendar := TCalendar.Create(nil,Data);
        aFolder := TGenericFolder.Create(aConnection,PR_IPM_APPOINTMENT_ENTRYID);
        try
          aItem := aFolder.GetFirst;
          while Assigned(aItem) do
            begin
              SyncOut := False;
              Collect := False;
              DoSync := True;
              aID := 0;
              Data.SetFilter(SyncItems,Data.QuoteField('SYNCTYPE')+'='+Data.QuoteValue(Synctype)+' AND '+Data.QuoteField('REMOTE_ID')+'='+Data.QuoteValue(EntryIdToString(aItem.EntryID)));
              while SyncItems.Count > 0 do
                begin
                  DoSync := not SyncItems.DataSet.FieldByName('LOCAL_ID').IsNull;
                  aCalendar.Select(SyncItems.DataSet.FieldByName('LOCAL_ID').AsVariant);
                  aCalendar.Open;
                  if aCalendar.DataSet.RecordCount > 0 then
                    begin
                      DoSync := DoSync and (not (roundto(IncHour(aItem.LastModificationTime,1),-3) = roundto(aCalendar.Timestamp.AsDateTime,-3)));
                      aID := aCalendar.Id.AsVariant;
                      break;
                    end
                  else
                    begin
                      SyncItems.Delete;
                    end;
                end;
              if aID <> 0 then
                begin
                  aCalendar.Select(aID);
                  aCalendar.Open;
                  if roundto(IncHour(aItem.LastModificationTime,1),-3) < roundto(aCalendar.Timestamp.AsDateTime,-3) then
                    SyncOut := True;
                end;
              if (aID = 0) and DoSync then
                begin
                  aCalendar.Append;
                  aCalendar.DataSet.FieldByName('ID').AsInteger := 99799;
                  aCalendar.DataSet.FieldByName('REF_ID_ID').AsVariant:=Data.Users.Id.AsVariant;
                  aCalendar.DataSet.Post;
                  aCalendar.DataSet.Edit;
                  aCalendar.DataSet.FieldByName('ID').AsInteger:=Data.GetUniID(aCalendar.DataSet);
                  aID := aCalendar.Id.AsVariant;
                  SyncOut := False;
                  with SyncItems.DataSet do
                    begin
                      Insert;
                      FieldByName('SYNCTYPE').AsString:=SyncType;
                      FieldByName('REMOTE_ID').AsString:=EntryIdToString(aItem.EntryID);
                      FieldByName('LOCAL_ID').AsVariant:=aCalendar.Id.AsVariant;
                      FieldByName('TIMESTAMPD').AsDateTime:=Now();
                      Post;
                    end;
                end;
              if (aID <> 0) and DoSync then
                begin
                  if not aCalendar.CanEdit then aCalendar.DataSet.Edit;
                  SyncProperty(aItem,PR_SUBJECT,ptString,aCalendar.DataSet.FieldByName('SUMMARY'),SyncOut,Collect);
                  debugln('Syncing '+aCalendar.DataSet.FieldByName('SUMMARY').AsString+' ...');
                  SyncProperty(aItem,PR_BODY,ptString,aCalendar.DataSet.FieldByName('DESCR'),SyncOut,Collect);
                  if not SyncOut then
                    begin
                      SyncProperty(aItem,PR_BODY,ptString,aCalendar.DataSet.FieldByName('DESC'),SyncOut,Collect);
                      if (not SyncOut) and aCalendar.DataSet.FieldByName('DESC').IsNull then
                        begin
//                          RTFSync(aItem.CoMessage,RTF_SYNC_BODY_CHANGED,RtfIsUpdated);
//                          if RTFIsUpdated then
//                            SyncProperty(aItem,PR_BODY,ptString,aTasks.DataSet.FieldByName('DESC'),SyncOut,Collect);
                          SStream := TStringStream.Create('');
                          if aItem.CoMessage.OpenProperty(PR_BODY, IStream, STGM_READ, 0, IInterface(StreamIntf)) = S_OK then
                          try
                            StreamIntf.Stat(StreamInfo, STATFLAG_NONAME);
                            OLEStream := TOleStream.Create(StreamIntf);
                            try
                              SSTream.CopyFrom(OLEStream,StreamInfo.cbSize);
                            finally
                              OLEStream.Free;
                            end;
                          finally
                            StreamIntf := nil;
                          end;
                          aCalendar.DataSet.FieldByName('DESC').AsString := SStream.DataString;
                          SStream.Free;
                        end;
                    end;
                  SyncProperty(aItem,PR_LOCATION,ptString,aCalendar.DataSet.FieldByName('LOCATION'),SyncOut,Collect);
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
                  if Frac(aCalendar.DataSet.FieldByName('STARTDATE').AsFloat) = 0 then
                    begin
                      if not aCalendar.CanEdit then aCalendar.DataSet.Edit;
                      aCalendar.DataSet.FieldByName('STARTDATE').AsFloat := aCalendar.DataSet.FieldByName('STARTDATE').AsFloat+aCalendar.DataSet.FieldByName('STARTTIME').AsFloat;
                    end;
                  if Frac(aCalendar.DataSet.FieldByName('ENDDATE').AsFloat) = 0 then
                    begin
                      if not aCalendar.CanEdit then aCalendar.DataSet.Edit;
                      aCalendar.DataSet.FieldByName('ENDDATE').AsFloat := aCalendar.DataSet.FieldByName('ENDDATE').AsFloat+aCalendar.DataSet.FieldByName('ENDTIME').AsFloat;
                    end;
                  if not Syncout then //Securely dont Syncout times at time
                    begin
                      SyncProperty(aItem,aItem.GetPropertyDispId($820D, PT_SYSTIME, False, @PSETID_Appointment),ptTime,aCalendar.DataSet.FieldByName('STARTDATE'),SyncOut,Collect);
                      SyncProperty(aItem,aItem.GetPropertyDispId($820E, PT_SYSTIME, False, @PSETID_Appointment),ptTime,aCalendar.DataSet.FieldByName('ENDDATE'),SyncOut,Collect);
                    end;
                  if SyncOut then
                    aItem.PropertiesDirect[aItem.GetPropertyDispId($8215, PT_BOOLEAN, False, @PSETID_Appointment),ptBoolean] := aCalendar.DataSet.FieldByName('ALLDAY').AsString = 'Y'
                  else
                    begin
                      if aItem.PropertiesDirect[aItem.GetPropertyDispId($8215, PT_BOOLEAN, False, @PSETID_Appointment),ptBoolean] then
                        begin
                          aCalendar.DataSet.FieldByName('ALLDAY').AsString := 'Y';
                          aCalendar.DataSet.FieldByName('STARTDATE').AsFloat:=Trunc(aCalendar.DataSet.FieldByName('STARTDATE').AsFloat)+1;
                        end
                      else
                        aCalendar.DataSet.FieldByName('ALLDAY').AsString := 'N';
                    end;
                  //TODO: Alarm
                  //TODO:reccuring (dont work with standart props)
                  if SyncOut then
                    begin
  {                    case aCalendar.DataSet.FieldByName('ROTATION').AsInteger of
                      0:
                        begin
                          aItem.PropertiesDirect[(PT_BOOLEAN) or ($8014 shl 16),ptBoolean] := False;
                        end;
                      1:
                        begin
                          aItem.PropertiesDirect[(PT_LONG) or ($803A shl 16),ptInteger] := 1;
                          aItem.PropertiesDirect[(PT_I2) or ($8075 shl 16),ptInteger] := aCalendar.DataSet.FieldByName('ROTCUS').AsInteger; //DayInterval
                        end;
                      2:
                        begin
                          aItem.PropertiesDirect[(PT_LONG) or ($803A shl 16),ptInteger] := 2;
                          aItem.PropertiesDirect[(PT_I2) or ($808D shl 16),ptInteger] := aCalendar.DataSet.FieldByName('ROTCUS').AsInteger; //WeekInterval
                        end;
                      3,4:
                        begin
                          aItem.PropertiesDirect[(PT_LONG) or ($803A shl 16),ptInteger] := 3;
                          aItem.PropertiesDirect[(PT_I2) or ($8080 shl 16),ptInteger] := aCalendar.DataSet.FieldByName('ROTCUS').AsInteger; //MonthInterval
                        end;
                      5,6:
                        begin
                          aItem.PropertiesDirect[(PT_LONG) or ($803A shl 16),ptInteger] := 4;
                          aItem.PropertiesDirect[(PT_I2) or ($808F shl 16),ptInteger] := aCalendar.DataSet.FieldByName('ROTCUS').AsInteger; //YearInterval
                        end;
                      end;
                      if aCalendar.DataSet.FieldByName('ROTATION').AsInteger > 0 then
                        aItem.PropertiesDirect[(PT_BOOLEAN) or ($8014 shl 16),ptBoolean] := True;
                      }
                    end
                  else if (not SyncOut) or Collect then
                    begin
                      if aItem.PropertiesDirect[aItem.GetPropertyDispId($8223, PT_BOOLEAN, False, @PSETID_Appointment),ptBoolean] then  //PR_ISRECURRING
                        begin
//                          SyncProperty(aItem,aItem.GetPropertyDispId($8235, PT_SYSTIME, False, @PSETID_Appointment),ptTime,aCalendar.DataSet.FieldByName('STARTDATE'),SyncOut,Collect);
                          SyncProperty(aItem,aItem.GetPropertyDispId($8236, PT_SYSTIME, False, @PSETID_Appointment),ptTime,aCalendar.DataSet.FieldByName('ROTTO'),SyncOut,Collect);
                          aPattern := aItem.PropertiesDirect[aItem.GetPropertyDispId($8232, PT_STRING8, False, @PSETID_Appointment),ptString];
                          case aItem.PropertiesDirect[aItem.GetPropertyDispId($8231, PT_LONG, False, @PSETID_Appointment),ptInteger] of //RecurenceType    (1=Täglich=1,2=Wöchentlich=2,3=Monatlich=4,4=Jährlich=6)
                          1:
                            begin
                              aCalendar.DataSet.FieldByName('ROTATION').AsInteger := 1;
  //                            aCalendar.DataSet.FieldByName('ROTCUS').AsInteger := aItem.PropertiesDirect[(PT_I2) or ($8075 shl 16),ptInteger]; //DayInterval
                            end;
                          2:
                            begin
                              aCalendar.DataSet.FieldByName('ROTATION').AsInteger := 2;
  //                            aCalendar.DataSet.FieldByName('ROTCUS').AsInteger := (aItem.PropertiesDirect[(PT_I2) or ($808D shl 16),ptInteger]); //WeekInterval
                            end;
                          3:
                            begin
                              aCalendar.DataSet.FieldByName('ROTATION').AsInteger := 4;
  //                            aCalendar.DataSet.FieldByName('ROTCUS').AsInteger := (aItem.PropertiesDirect[(PT_I2) or ($8080 shl 16),ptInteger]); //MonthInterval
                            end;
                          4:
                            begin
                              aCalendar.DataSet.FieldByName('ROTATION').AsInteger := 6;
  //                            aCalendar.DataSet.FieldByName('ROTCUS').AsInteger := (aItem.PropertiesDirect[(PT_I2) or ($808F shl 16),ptInteger]); //YearInterval
                            end;
                          end;
                        end;
                    end;
                  if aCalendar.canEdit then
                    aCalendar.DataSet.Post;
                  if SyncOut then
                    aItem.CoMessage.SaveChanges(0);
                end;
              aItem.Free;
              aItem := aFolder.GetNext;
            end;
        finally
          aCalendar.Free;
          aFolder.Free;
        end;
        WriteMessage('Syncing Tasks...');
        //Aufgaben syncronisieren
        aTasks := TTaskList.Create(nil,Data);
        aFolder := TGenericFolder.Create(aConnection,PR_IPM_TASK_ENTRYID);
        try
          aItem := aFolder.GetFirst;
          while Assigned(aItem) do
            begin
              SyncOut := False;
              Collect := False;
              DoSync := True;
              aID := 0;
              Data.SetFilter(SyncItems,Data.QuoteField('SYNCTYPE')+'='+Data.QuoteValue(Synctype)+' AND '+Data.QuoteField('REMOTE_ID')+'='+Data.QuoteValue(EntryIdToString(aItem.EntryID)));
              while SyncItems.Count > 0 do
                begin
                  DoSync := not SyncItems.DataSet.FieldByName('LOCAL_ID').IsNull;
                  aTasks.Select(SyncItems.DataSet.FieldByName('LOCAL_ID').AsVariant);
                  aTasks.Open;
                  if aTasks.DataSet.RecordCount > 0 then
                    begin
                      DoSync := DoSync and (not (roundto(IncHour(aItem.LastModificationTime,1),-3) = roundto(aTasks.Timestamp.AsDateTime,-3)));
                      aID := aTasks.Id.AsVariant;
                      break;
                    end
                  else
                    begin
                      SyncItems.Delete;
                    end;
                end;
              if aID <> 0 then
                begin
                  aTasks.Select(aID);
                  aTasks.Open;
                  WritelnMessage('OETime:'+TimeToStr(IncHour(aItem.LastModificationTime,1))+' PETime:'+TimeToStr(aTasks.Timestamp.AsDateTime));
                  if IncHour(aItem.LastModificationTime,1) < aTasks.Timestamp.AsDateTime then
                    SyncOut := True;
                end;
              if (aID = 0) and DoSync then
                begin
                  aTasks.Append;
                  aTasks.DataSet.FieldByName('OWNER').AsVariant:=Data.Users.DataSet.FieldByName('ACCOUNTNO').AsVariant;
                  aTasks.DataSet.Post;
                  aTasks.DataSet.Edit;
                  aID := aTasks.Id.AsVariant;
                  SyncOut := False;
                  with SyncItems.DataSet do
                    begin
                      Insert;
                      FieldByName('SYNCTYPE').AsString:=SyncType;
                      FieldByName('REMOTE_ID').AsString:=EntryIdToString(aItem.EntryID);
                      FieldByName('LOCAL_ID').AsVariant:=aTasks.Id.AsVariant;
                      FieldByName('TIMESTAMPD').AsDateTime:=Now();
                      Post;
                    end;
                end;
              if (aID <> 0) and DoSync then
                begin
                  if not aTasks.CanEdit then aTasks.DataSet.Edit;
                  SyncProperty(aItem,PR_SUBJECT,ptString,aTasks.DataSet.FieldByName('SUMMARY'),SyncOut,Collect);
                  debugln('Syncing '+aTasks.DataSet.FieldByName('SUMMARY').AsString+' ...');
                  if not SyncOut then
                    begin
                      SyncProperty(aItem,PR_BODY,ptString,aTasks.DataSet.FieldByName('DESC'),SyncOut,Collect);
                      if (not SyncOut) and aTasks.DataSet.FieldByName('DESC').IsNull then
                        begin
//                          RTFSync(aItem.CoMessage,RTF_SYNC_BODY_CHANGED,RtfIsUpdated);
//                          if RTFIsUpdated then
//                            SyncProperty(aItem,PR_BODY,ptString,aTasks.DataSet.FieldByName('DESC'),SyncOut,Collect);
                          SStream := TStringStream.Create('');
                          if aItem.CoMessage.OpenProperty(PR_BODY, IStream, STGM_READ, 0, IInterface(StreamIntf)) = S_OK then
                          try
                            StreamIntf.Stat(StreamInfo, STATFLAG_NONAME);
                            OLEStream := TOleStream.Create(StreamIntf);
                            try
                              SSTream.CopyFrom(OLEStream,StreamInfo.cbSize);
                            finally
                              OLEStream.Free;
                            end;
                          finally
                            StreamIntf := nil;
                          end;
                          aTasks.DataSet.FieldByName('DESC').AsString := SStream.DataString;
                          SStream.Free;
                        end;
                    end;
                  if SyncOut then
                    begin
                      aItem.PropertiesDirect[aItem.GetPropertyDispId($811c, PT_BOOLEAN, False, @PSETID_Task),ptBoolean] := aTasks.DataSet.FieldByName('COMPLETED').AsString = 'Y';
                    end
                  else
                    begin
                      if aItem.PropertiesDirect[aItem.GetPropertyDispId($811c, PT_BOOLEAN, False, @PSETID_Task),ptBoolean] then
                        aTasks.DataSet.FieldByName('COMPLETED').AsString := 'Y'
                      else
                        aTasks.DataSet.FieldByName('COMPLETED').AsString := 'N';
                    end;
                  SyncProperty(aItem,aItem.GetPropertyDispId($8105, PT_SYSTIME, False, @PSETID_Task),ptTime,aTasks.DataSet.FieldByName('DUEDATE'),SyncOut,Collect);
                  SyncProperty(aItem,aItem.GetPropertyDispId($8104, PT_SYSTIME, False, @PSETID_Task),ptTime,aTasks.DataSet.FieldByName('STARTDATE'),SyncOut,Collect);
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
                end;
              if aTasks.canEdit then
                aTasks.DataSet.Post;
              aItem.CoMessage.SaveChanges(0);
              aItem.Free;
              aItem := aFolder.GetNext;
            end;
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
function TfSync.SyncProperty(aItem: TMapiMailItem; Mapiprop: Cardinal;
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
                aItem.PropertiesDirect[MapiProp,MapiType] := ConvertEncoding(DbField.AsString,GuessEncoding(DbField.AsString),'CP1250')
              else if MapiType = ptTime then
                aItem.PropertiesDirect[MapiProp,MapiType] := DateUtils.IncHour(DbField.AsDateTime,-1)//TODO:LocaltimeToGMT
              else
                aItem.PropertiesDirect[MapiProp,MapiType] := DbField.AsVariant;
            end
          else
            begin
              if (DbField.DataSet.State <> dsEdit) and (DbField.DataSet.State <> dsInsert) then
                DbField.DataSet.Edit;
              if MapiType = ptString then
                DbField.AsString:=ConvertEncoding(aItem.PropertiesDirect[MapiProp,MapiType],GuessEncoding(aItem.PropertiesDirect[MapiProp,MapiType]),EncodingUTF8)
              else if MapiType = ptTime then
                DbField.AsDateTime := IncHour(aItem.PropertiesDirect[MapiProp,MapiType],1)//TODO:GMTToLocaltime
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
            aItem.PropertiesDirect[MapiProp,MapiType] := ConvertEncoding(DbField.AsString,GuessEncoding(DbField.AsString),'CP1250')
          else if MapiType = ptTime then
            aItem.PropertiesDirect[MapiProp,MapiType] := DateUtils.IncHour(DbField.AsDateTime,-1)//TODO:LocaltimeToGMT
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
            DbField.AsString:=ConvertEncoding(aItem.PropertiesDirect[MapiProp,MapiType],GuessEncoding(aItem.PropertiesDirect[MapiProp,MapiType]),EncodingUTF8)
        end
      else if MapiType = ptTime then
        DbField.AsDateTime := IncHour(aItem.PropertiesDirect[MapiProp,MapiType],1)//TODO:GMTToLocaltime
      else
        DbField.AsVariant:=aItem.PropertiesDirect[MapiProp,MapiType];
    end;
end;

function TfSync.SyncStrProperty(aItem: TMapiMailItem; MapiProp: WideString;
  NameSpace : PGUID;MapiType: TMapiPropertyType; DbField: TField; SyncOut: Boolean;
  Collect: Boolean): Boolean;
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
procedure TfSync.WritelnMessage(s: string);
begin
  debugln(s);
end;
procedure TfSync.WriteMessage(s: string);
begin
  debugln(s);
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
constructor TGenericFolder.Create(const Connection: TMapiConnection;
  EntryTyp: Cardinal);
begin
  FEntryTyp := EntryTyp;
  inherited Create(Connection);
end;

end.

