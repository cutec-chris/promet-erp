program sync_owncloud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces, // this includes the LCL widgetset
  pcmdprometapp, uData, db, uBaseDBInterface, uBaseApplication,
  uBaseCustomApplication, uBaseDbClasses, uSync, uOrder, uPerson, uMasterdata,
  LConvEncoding,IniFiles,ZConnection,ZClasses,ZDataset,uProjects,uCalendar,
  uimpvcal,uImpVCard;
type
  TSyncDBApp = class(TBaseCustomApplication)
  private
    FTempDataSet : TDataSet;
    FTempNewCounter : Integer;
    FTempDataSetName : string;
    FDest : TBaseDBInterface;
    aGlobalTime : TDateTime;
    procedure WriteMessage(s : string);
    procedure WriteError(s : string);
    function RebuildTree(aConn: TZConnection; Parent, Left: Variant): Int64;
  protected
    procedure DoRun; override;
    function GetSingleInstance : Boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;
resourcestring
  strFailedToLoadConfig          = 'sync_owncloud.conf does not exists !';
  strConnectFailed               = 'Cant connect to owncloud DB';
function TSyncDBApp.GetSingleInstance: Boolean;
begin
  Result := False;
end;
procedure TSyncDBApp.DoRun;
var
  SyncDB: TSyncDB;
  aSyncOffs: Integer;
  aTable: TBaseDbdataSet;
  y: word;
  m: word;
  d: word;
  h: word;
  mm: word;
  s: word;
  ss: word;
  LoggedIn: Boolean;
  rmConn: TZConnection;
  rmQuerry: TZQuery;
  aConfig: TIniFile;
  aProjects: TProject;
  aProject: TProjectList;
  rmQuerryE: TZQuery;
  arec : variant;
  aLeft: Int64;
  aUsers: TUser;
  aUID: String;
  SyncItems : TSyncItems;
  SyncOut: Boolean;
  Collect: Boolean;
  DoSync: Boolean;
  aID: Integer;
  rmUser: TZQuery;
  aCalendar: TCalendar;
  aSL: TStringList;
  aContact: TPerson;
  Found: Boolean;
  LastModified: TDateTime;
begin
  aGlobalTime := Now();
  FTempDataSet := nil;
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not FileExists('sync_owncloud.conf') then
    raise Exception.Create(strFailedToLoadConfig);
  aConfig := TIniFile.Create('sync_owncloud.conf');
  with BaseApplication,BaseApplication as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
      if not HasOption('m','mandant') then
        raise Exception.Create(strMandantnotSelected);
      if not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user')) then
        raise Exception.Create(strLoginFailed);
      uData.Data := Data;
    end;
  DecodeDate(Now(),y,m,d);
  DecodeTime(Now(),h,mm,s,ss);
  rmConn := TZConnection.Create(Self);
  rmQuerry := TZQuery.Create(Self);
  rmQuerry.Connection:=rmConn;
  rmUser := TZQuery.Create(Self);
  rmUser.Connection:=rmConn;
  rmQuerryE := TZQuery.Create(Self);
  rmQuerryE.Connection:=rmConn;
  SyncItems := TSyncItems.Create(nil,Data);
  rmConn.Protocol:=aConfig.ReadString('database','protocol','sqlite-3');
  rmConn.HostName:=aConfig.ReadString('database','host','localhost');
  rmConn.Database:=aConfig.ReadString('database','database','owncloud.db');
  rmConn.Port:=aConfig.ReadInteger('database','port',0);
  rmConn.user:=aConfig.ReadString('database','user','');
  rmConn.Password:=aConfig.ReadString('database','password','');
  rmConn.Connect;
  if not rmConn.Connected then
    raise Exception.Create(strConnectFailed);
  WriteMessage('Connection successful');
  rmUser.SQL.Text:='select * from oc_users';
  rmUser.Open;
  aUsers := TUser.Create(nil,Data);
  aUsers.Open;
  while not aUsers.EOF do
    begin
      if aUsers.FieldByName('TYPE').AsString<>'G' then
        begin
          aUID := trim(aUsers.FieldByName('LOGINNAME').AsString);
          if aUID='' then
            aUID := copy(aUsers.FieldByName('NAME').AsString,pos(' ',aUsers.FieldByName('NAME').AsString)+1,length(aUsers.FieldByName('NAME').AsString));
          if (not rmUser.Locate('uid',aUID,[]))
          and (aUID <> 'Gast')
          and (aUID <> 'Administrator')
          then
            begin
              writeln('Adding User '+aUid);
              rmUser.Insert;
              rmUser.FieldByName('uid').AsString:=aUID;
              rmUser.FieldByName('displayname').AsString:=aUsers.FieldByName('NAME').AsString;
              rmUser.FieldByName('password').AsString:='$2a$08$qnPfPL3wi40AJt8Sp4ceTOlnMGe7scbPdChcT4QhWWOrdHKzSQTBm';//default
              rmUser.Post;
            end;

          writeln('Syncing User '+aUID);
          //Sync Calendars
          rmQuerry.SQL.Text:='select * from oc_clndr_calendars where userid='''+aUID+'''';
          rmQuerry.Open;
          while not rmQuerry.EOF do
            begin
              if rmQuerry.FieldByName('uri').AsString='defaultcalendar' then
                begin
                  rmQuerryE.SQL.Text:='select * from oc_clndr_objects where calendarid='''+rmQuerry.FieldByName('id').AsString+'''';
                  rmQuerryE.Open;
                  writeln('Syncing Calendar '+rmQuerry.FieldByName('displayname').AsString);
                  while not rmQuerryE.EOF do
                    begin
                      if rmQuerryE.FieldByName('objecttype').AsString='VEVENT' then
                        begin
                          SyncOut := False;
                          Collect := False;
                          DoSync := True;
                          aID := 0;
                          Data.SetFilter(SyncItems,Data.QuoteField('SYNCTYPE')+'='+Data.QuoteValue('OWNCLOUD')+' AND '+Data.QuoteField('REMOTE_ID')+'='+Data.QuoteValue(rmQuerryE.FieldByName('uri').AsString));
                          aCalendar := TCalendar.Create(nil,Data);
                          aCalendar.RefId:=aUsers.Id.AsVariant;
                          if SyncItems.Count > 0 then
                            begin
                              DoSync := (not SyncItems.DataSet.FieldByName('LOCAL_ID').IsNull) and (not SyncItems.DataSet.FieldByName('LOCAL_ID').AsInteger = 0);
                              aCalendar.Select(SyncItems.FieldByName('LOCAL_ID').AsVariant);
                              aCalendar.Open;
                            end
                          else
                            begin
                              SyncItems.Insert;
                              SyncItems.FieldByName('SYNCTYPE').AsString:='OWNCLOUD';
                              SyncItems.FieldByName('REMOTE_ID').AsString:=rmQuerryE.FieldByName('uri').AsString;
                            end;
                          if DoSync then
                            begin
                              writeln('Syncing Event '+rmQuerryE.FieldByName('summary').AsString);
                              aSL := TStringList.Create;
                              aSL.Text:=rmQuerryE.FieldByName('calendardata').AsString;
                              VCalImport(aCalendar,aSL);
                              aSL.Free;
                              with SyncItems do
                                begin
                                  Edit;
                                  FieldByName('USER_ID').AsVariant:=aUsers.Id.AsVariant;
                                  FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(aCalendar);
                                  FieldByName('TIMESTAMPD').AsDateTime:=Now();
                                  Post;
                                end;
                            end;
                          aCalendar.Free;
                        end;
                      rmQuerryE.Next;
                    end;
                end;
              rmQuerry.Next;
            end;

          //Sync contacts

          rmQuerry.SQL.Text:='select * from oc_contacts_addressbooks where userid='''+aUID+'''';
          rmQuerry.Open;
          while not rmQuerry.EOF do
            begin
              if rmQuerry.FieldByName('uri').AsString='contacts' then
                begin
                  rmQuerryE.SQL.Text:='select * from oc_contacts_cards where addressbookid='''+rmQuerry.FieldByName('id').AsString+'''';
                  rmQuerryE.Open;
                  writeln('Syncing Addressbook '+rmQuerry.FieldByName('displayname').AsString);
                  while not rmQuerryE.EOF do
                    begin
                      SyncOut := False;
                      Collect := False;
                      DoSync := True;
                      Found := False;
                      LastModified := ((rmQuerryE.FieldByName('lastmodified').AsInteger + 7200) / 86400) + 25569;
                      aID := 0;
                      Data.SetFilter(SyncItems,Data.QuoteField('SYNCTYPE')+'='+Data.QuoteValue('OWNCLOUD')+' AND '+Data.QuoteField('REMOTE_ID')+'='+Data.QuoteValue(rmQuerryE.FieldByName('uri').AsString));
                      aContact := TPerson.Create(nil,Data);
                      if SyncItems.Count > 0 then
                        begin
                          DoSync := (not SyncItems.DataSet.FieldByName('LOCAL_ID').IsNull) and (not SyncItems.DataSet.FieldByName('LOCAL_ID').AsInteger = 0);
                          aContact.Select(SyncItems.FieldByName('LOCAL_ID').AsVariant);
                          aContact.Open;
                          Found := True;
                          DoSync:=SyncItems.TimeStamp.AsDateTime<LastModified;
                          if aContact.TimeStamp.AsDateTime>SyncItems.TimeStamp.AsDateTime then
                            begin
                              DoSync:=True;
                              SyncOut:=True;
                            end;
                        end
                      else
                        begin
                          SyncItems.Insert;
                          SyncItems.FieldByName('SYNCTYPE').AsString:='OWNCLOUD';
                          SyncItems.FieldByName('REMOTE_ID').AsString:=rmQuerryE.FieldByName('uri').AsString;
                        end;
                      if DoSync then
                        begin
                          writeln('Syncing Contact '+rmQuerryE.FieldByName('fullname').AsString);
                          aSL := TStringList.Create;
                          aSL.Text:=rmQuerryE.FieldByName('carddata').AsString;
                          if not aContact.Active then
                            begin
                              aContact.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(rmQuerryE.FieldByName('fullname').AsString));
                              if aContact.Count=1 then
                                begin
                                  Found := True;
                                end;
                            end;
                          if DoSync and (not SyncOut) then
                            begin
                              VCardImport(aContact,aSL,Found);
                              with SyncItems do
                                begin
                                  Edit;
                                  FieldByName('USER_ID').AsVariant:=aUsers.Id.AsVariant;
                                  FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(aContact);
                                  FieldByName('TIMESTAMPD').AsDateTime:=Now();
                                  Post;
                                end;
                            end
                          else if DoSync then
                            begin
                              if VCardExport(aContact,aSL) then
                                begin
                                  rmQuerryE.Edit;
                                  rmQuerryE.FieldByName('carddata').AsString:=aSL.Text;
                                  rmQuerryE.FieldByName('lastmodified').AsInteger := ((Trunc(Now()) - 25569) * 86400) + Trunc(86400 * (Now() - Trunc(Now()))) - 7200;
                                  rmQuerryE.Post;
                                  with SyncItems do
                                    begin
                                      Edit;
                                      FieldByName('USER_ID').AsVariant:=aUsers.Id.AsVariant;
                                      FieldByName('LOCAL_ID').AsVariant:=Data.GetBookmark(aContact);
                                      FieldByName('TIMESTAMPD').AsDateTime:=Now();
                                      Post;
                                    end;
                                end;
                            end;
                          aSL.Free;
                        end;
                      aContact.Free;
                      rmQuerryE.Next;
                    end;
                end;
              rmQuerry.Next;
            end;
        end;
      aUsers.Next;
    end;

{
  aProjects := TProject.Create(nil,Data);
  with aProjects.DataSet as IBaseDbFilter do
    begin
      //Filter := ProcessTerm(Data.QuoteField('PARENT')+'='+Data.QuoteValue(''));
      SortFields:='SQL_ID';
      //SortDirection:=sdDescending;
      Limit:=0;
    end;
  aProjects.Open;
  while not aProjects.EOF do
    begin
      if not rmQuerry.Locate('identifier',aProjects.Number.AsString,[]) then
        begin
          rmQuerry.Insert;
        end
      else rmQuerry.Edit;
      if rmQuerry.FieldByName('updated_on').AsDateTime>aProjects.TimeStamp.AsDateTime then
        begin

        end
      else
        begin
          rmQuerry.FieldByName('name').AsString:=aProjects.text.AsString;
          rmQuerry.FieldByName('description').AsString:=aProjects.FieldByName('DESCRIPTION').AsString;
          rmQuerry.FieldByName('id').AsVariant:=aProjects.Id.AsVariant;
          rmQuerry.FieldByName('is_public').AsBoolean:=False;
          rmQuerry.FieldByName('created_on').AsDateTime:=aProjects.FieldByName('START').AsDateTime;
          rmQuerry.FieldByName('created_on').AsDateTime:=aProjects.FieldByName('START').AsDateTime;
          rmQuerry.FieldByName('updated_on').AsDateTime:=Now();
          rmQuerry.FieldByName('identifier').AsString := aProjects.Number.AsString;
          rmQuerry.FieldByName('updated_on').AsDateTime:=Now();
          if not aProjects.FieldByName('PARENT').IsNull then
            begin
              rmQuerry.FieldByName('parent_id').AsVariant:=aProjects.FieldByName('PARENT').AsVariant;
            end;
          if rmQuerry.State = dsInsert then
            begin
              if rmQuerry.RecordCount<1 then
                begin
                  rmQuerry.FieldByName('lft').AsInteger:=1;
                  rmQuerry.FieldByName('rgt').AsInteger:=2;
                  rmQuerry.Post;
                end
              else
                begin
                  rmQuerry.Post;
                  rmQuerryE.SQL.Text:='update projects set lft=(select max(rgt) from projects)+1,rgt=(select max(rgt) from projects)+2 where lft is NULL and rgt is NULL';
                  rmQuerryE.ExecSQL;
                end;
            end
          else rmQuerry.Post;
        end;
      aProjects.Next;
    end;
  rmQuerry.SQL.Text:='select * from "projects" where "parent_id" is null';
  rmQuerry.Open;
  rmQuerry.First;
  aLeft := 1;
  while not rmQuerry.EOF do
    begin
      aLeft := RebuildTree(rmConn,rmQuerry.FieldByName('id').AsVariant,aleft);
      rmQuerry.Next;
    end;

  aProjects.Free;
}
  rmQuerry.Free;
  rmQuerryE.Free;
  rmConn.Free;
  aConfig.Free;
  BaseApplication.Terminate;
end;
procedure TSyncDBApp.WriteMessage(s: string);
begin
  writeln(s);
end;
procedure TSyncDBApp.WriteError(s: string);
begin
  writeln('ERROR:'+s);
end;

function TSyncDBApp.RebuildTree(aConn: TZConnection; Parent, Left: Variant
  ): Int64;
var
  Right: Int64;
  aQuerry: TZQuery;
  aSQL: String;
begin
  if Left=Null then exit;
  Right := Left+1;
  if Parent=Null then exit;
  // get all children of this node
  aQuerry := TZQuery.Create(Self);
  aQuerry.Connection:=aConn;
  aSQL := 'select * from "projects" where "parent_id"='+Data.QuoteValue(IntTostr(Parent));
  aQuerry.SQL.Text:=aSQL;
  aQuerry.Open;
  while not aQuerry.EOF do
    begin
      // recursive execution of this function for each
      // child of this node
      // $right is the current right value, which is
      // incremented by the rebuild_tree function
      Right := RebuildTree(aConn,aQuerry.FieldByName('id').AsVariant,Right);
      aQuerry.Next;
    end;
  // we've got the left value, and now that we've processed
  // the children of this node we also know the right value
  aSQL := 'update "projects" set "lft"='+Data.QuoteValue(IntToStr(Left))+', "rgt"='+Data.QuoteValue(IntToStr(Right))+' where "id"='+Data.QuoteValue(IntToStr(Parent));
  aQuerry.SQL.Text:=aSQL;
  aQuerry.ExecSQL;
  aQuerry.Free;
  // return the right value of this node + 1
  Result := Right+1;
end;

constructor TSyncDBApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;
var
  Application: TSyncDBApp;
begin
  Application:=TSyncDBApp.Create(nil);
  Application.Run;
  Application.Free;
end.

