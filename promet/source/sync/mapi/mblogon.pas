unit mblogon;

interface

{$IFDEF CPU64}
  //64 bit - align at 8 bytes
  {$A8}
{$ELSE}
  //32 bit
  {$A+}
{$ENDIF }

  uses Windows, MAPIDefs, MAPIX, MAPITags, SysUtils,
       edkmdb, edkguid, emsabTag,
       MAPIUtil;

  function HrMailBoxLogon(lplhSession : IMAPISession;
                          lpMDB : IMsgStore;
                          lpszMsgStoreDN : PAnsiChar;
                          lpszMailBoxDN : PAnsiChar;
                          var lppXManageStore : IExchangeManageStore;
                          var lppMailboxMDB : IMsgStore) : HResult; stdcall;

  function HrGetServerDN(Session:IMAPISession):AnsiString;
  //function HrGetServerDN(ProfName,Password:AnsiString):AnsiString;
  function HrGetMailboxDN(Session:IMAPISession):AnsiString;

implementation

function PosFromEnd(SubStr, Str:AnsiString):integer;
var ISubStr,IStr:integer;
    Equal:boolean;
    i,j:integer;
begin
  Result:=0;
  ISubStr:=Length(SubStr);
  IStr:=Length(Str);
  for i:=IStr-ISubStr+1 downto 1 do begin
    Equal:=true;
    for j:=1 to ISubStr do begin
      Equal:=Equal and (SubStr[j]=Str[i+j-1]);
      if not Equal then Break;
    end;
    if Equal then begin
      Result:=i;
      Break;
    end;
  end;
end;

  function HrMailBoxLogon(lplhSession : IMAPISession;
                          lpMDB : IMsgStore;
                          lpszMsgStoreDN : PAnsiChar;
                          lpszMailBoxDN : PAnsiChar;
                          var lppXManageStore : IExchangeManageStore;
                          var lppMailboxMDB : IMsgStore) : HResult; stdcall;
  var cbeid : ULONG;
      lpeid : PEntryID;
  begin
    Result:=lpMDB.QueryInterface(IID_IExchangeManageStore, lppXManageStore);
    if Result = S_OK then begin
      Result:=lppXManageStore.CreateStoreEntryID(lpszMsgStoreDN, lpszMailboxDN,
              OPENSTORE_USE_ADMIN_PRIVILEGE OR OPENSTORE_TAKE_OWNERSHIP, cbeid, lpeid);
      if Result = S_OK then begin
        Result:=lplhSession.OpenMsgStore(0,cbeid,lpeid,TGUID(nil^),MDB_NO_DIALOG or MAPI_BEST_ACCESS, lppMailboxMDB);
        MAPIFreeBuffer(lpeid);
      end;
    end;
  end;

{function HrGetServerDN(ProfName,Password:AnsiString):AnsiString;
const sptCols : record
        cValues : ULONG;
        aulPropTag : array[0..1] of ULONG;
      end =
      (cValues: 2; aulPropTag : (PR_SERVICE_UID, PR_SERVICE_NAME));

var res:HResult;
    ProfAdmin:IPROFADMIN;
    ServAdmin:IMsgServiceAdmin;
    Tbl:IMAPITable;
    pRow:PSRowSet;
    spv : TSPropValue;
    sres : TSRestriction;
    ProfSect:IProfSect;
    propVal:PSPropValue;
begin
  res:=MAPIAdminProfiles(0,ProfAdmin);
  if res = S_OK then begin
    res:=ProfAdmin.AdminServices(PAnsiChar(ProfName),PAnsiChar(Password),Application.Handle,0,ServAdmin);
    if res = S_OK then begin
      res:=ServAdmin.GetMsgServiceTable(0,Tbl);
      if res = S_OK then begin
        //res:=Tbl.SetColumns(PSPropTagArray(@sptCols),0);
        if res = S_OK then begin
          sres.rt:=RES_PROPERTY;
          sres.res.resProperty.relop:=RELOP_EQ;
          sres.res.resProperty.ulPropTag:=PR_SERVICE_NAME;
          sres.res.resProperty.lpProp:=@spv;
          spv.ulPropTag:=PR_SERVICE_NAME;
          spv.Value.lpszA:='MSEMS';
          res:=HrQueryAllRows(Tbl,@sptCols,@sres,nil,0,pRow);
          if res = S_OK then begin

            AdminProviders

            res:=ServAdmin.OpenProfileSection(PMAPIUID(pRow^.aRow[0].lpProps[0].Value.bin.lpb), TGUID(nil^),0,ProfSect);
            if res = S_OK then begin
              res:=HrGetOneProp(ProfSect,PR_PROFILE_HOME_SERVER_DN,propVal);
              if res = S_OK then begin
                Result:=propVal^.Value.lpszA;
                MAPIFreeBuffer(propVal);
              end;
            end;
            FreePRows(pRow);
          end;
        end;
      end;
    end;
  end;
end;}

function HrGetServerDN(Session:IMAPISession):AnsiString;
var cbeid:ULONG;
    lpeid:PEntryID;
    res:HResult;
    ulObjType:ULONG;
    MAPIProp:IMAPIProp;
    Prop:PSPropValue;
begin
  Result:='';
  res:=Session.QueryIdentity(cbeid,lpeid);
  if res = S_OK then begin
    res:=Session.OpenEntry(cbeid,lpeid,IMAPIProp,MAPI_BEST_ACCESS or MAPI_DEFERRED_ERRORS,ulObjType,IUnknown(MAPIProp));
    if res = S_OK then begin
      res:=HrGetOneProp(MAPIProp,PR_EMS_AB_HOME_MTA,Prop);
      if res = S_OK then begin
        Result:=Prop^.Value.lpszA;
        MAPIFreeBuffer(Prop);
      end;
    end;
    MAPIFreeBuffer(lpeid);
  end;
  //replace last "/cn=<some stuff>" with "/cn=NULL"
  if Result <> '' then Result:=Copy(Result, 1, PosFromEnd('/CN=',UpperCase(Result))-1);//+'NULL';
end;

function HrGetMailboxDN(Session:IMAPISession):AnsiString;
var cbeid:ULONG;
    lpeid:PEntryID;
    res:HResult;
    ulObjType:ULONG;
    MAPIProp:IMAPIProp;
    Prop:PSPropValue;
begin
  Result:='';
  res:=Session.QueryIdentity(cbeid,lpeid);
  if res = S_OK then begin
    res:=Session.OpenEntry(cbeid,lpeid,IMAPIProp,MAPI_BEST_ACCESS or MAPI_DEFERRED_ERRORS,ulObjType,IUnknown(MAPIProp));
    if res = S_OK then begin
      res:=HrGetOneProp(MAPIProp,PR_EMAIL_ADDRESS,Prop);
      if res = S_OK then begin
        Result:=Prop^.Value.lpszA;
        MAPIFreeBuffer(Prop);
      end;
    end;
    MAPIFreeBuffer(lpeid);
  end;
end;

end.
