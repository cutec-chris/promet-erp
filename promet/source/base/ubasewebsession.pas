{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 04.08.2013
*******************************************************************************}
unit uBaseWebSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpWeb,uSessionDBClasses,HTTPDefs,fpHTTP,BlckSock;
type

  { TBaseWebSession }

  TBaseWebSession = class(TCustomSession)
  private
    SID : String;
    FSessionStarted: Boolean;
    FTerminated: Boolean;
    FSession : TSessions;
  protected
    Function GetSessionID : String; override;
    Function GetSessionVariable(VarName : String) : String; override;
    procedure SetSessionVariable(VarName : String; const AValue: String); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Terminate; override;
    Procedure UpdateResponse(AResponse : TResponse); override;
    Procedure InitSession(ARequest : TRequest; OnNewSession, OnExpired: TNotifyEvent); override;
    Procedure InitResponse(AResponse : TResponse); override;
    Procedure RemoveVariable(VariableName : String); override;
    procedure AddHistoryUrl(aUrl : string);
  end;
  TBaseSessionFactory = Class(TSessionFactory)
  private
  protected
    Procedure DoDoneSession(Var ASession : TCustomSession); override;
    Function SessionExpired(aSession : TSessions) : boolean;
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
    procedure DoCleanupSessions; override;
  end;

implementation
uses uData,uBaseDBInterface,md5,uUserAgents;
procedure TBaseSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  FreeAndNil(ASession);
end;
function TBaseSessionFactory.SessionExpired(aSession: TSessions): boolean;
Var
  L : TDateTime;
  T : Integer;
begin
  L:=aSession.FieldByName('LASTACCESS').AsDateTime;
  T:=aSession.FieldByName('TIMEOUT').AsInteger;
  Result:=((Now-L)>(T/(24*60)))
end;
function TBaseSessionFactory.DoCreateSession(ARequest: TRequest
  ): TCustomSession;
var
  S: TBaseWebSession;
begin
  S:=TBaseWebSession.Create(Nil);
  Result:=S;
end;
procedure TBaseSessionFactory.DoCleanupSessions;
var
  aSessions: TSessions;
  aFilter: String;
begin
  aSessions := TSessions.Create(Self,Data);
  aSessions.CreateTable;
  aFilter := Data.DateTimeToFilter(Now()-(DefaultTimeOutMinutes/(24*60)));
  aFilter := Data.QuoteField('LASTACCESS')+' < '+aFilter;
  with aSessions.DataSet as IBaseDbFilter do
    aFilter := aFilter+' and '+Data.ProcessTerm(Data.QuoteField('ISACTIVE')+'='+Data.QuoteValue(''));
  Data.SetFilter(aSessions,aFilter);
  with aSessions.DataSet do
    begin
      while not EOF do
        begin
          if SessionExpired(aSessions) then
            begin
              if not aSessions.CanEdit then
                aSessions.DataSet.Edit;
              aSessions.FieldByName('ISACTIVE').AsString := 'N';
              aSessions.DataSet.Post;
            end;
          Next;
        end;
    end;
  aSessions.Destroy;
end;
function TBaseWebSession.GetSessionID: String;
begin
  If (SID='') then
    SID:=inherited GetSessionID;
  Result:=SID;
end;
function TBaseWebSession.GetSessionVariable(VarName: String): String;
begin
  Result := '';
  if FSession.Count = 0 then exit;
  FSession.Variables.Select(Varname);
  FSession.Variables.Open;
  if FSession.Variables.Count > 0 then
    Result := FSession.Variables.FieldByName('VALUE').AsString;
end;
procedure TBaseWebSession.SetSessionVariable(VarName: String;
  const AValue: String);
begin
  if FSession.Count = 0 then exit;
  FSession.Variables.Select(Varname);
  FSession.Variables.Open;
  try
    if FSession.Variables.Count = 0 then
      FSession.Variables.DataSet.Insert
    else if not FSession.Variables.CanEdit then
      FSession.Variables.DataSet.Edit;
    FSession.Variables.FieldByName('VALUE').AsString := AValue;
    FSession.Variables.FieldByName('NAME').AsString := VarName;
    FSession.Variables.DataSet.Post;
  except
    FSession.Variables.DataSet.Cancel;
  end;
end;
constructor TBaseWebSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSession := TSessions.Create(Self,Data);
  TimeOutMinutes := 25;
end;
destructor TBaseWebSession.Destroy;
begin
  try
    FSession.Destroy;
  except
  end;
  inherited Destroy;
end;
procedure TBaseWebSession.Terminate;
begin
end;
procedure TBaseWebSession.UpdateResponse(AResponse: TResponse);
begin
end;
procedure TBaseWebSession.InitSession(ARequest: TRequest; OnNewSession,
  OnExpired: TNotifyEvent);
Var
  S : String;
  FExpired: Boolean = False;
  Sock: TBlockSocket;
begin
  // First initialize all session-dependent properties to their default, because
  // in Apache-modules or fcgi programs the session-instance is re-used
  SID := '';
  FSessionStarted := False;
  FTerminated := False;
  // If a exception occured during a prior request FIniFile is still not freed
//  if assigned(FIniFile) then FreeIniFile;
  If (SessionCookie='') then
    SessionCookie:='PWSID';
  S:=ARequest.CookieFields.Values[SessionCookie];
  // have session cookie ?
  If (S<>'') then
    begin
      FSession.Select(S);
      FSession.Open;
      if (FSession.Count > 0) and (SessionFactory as TBaseSessionFactory).SessionExpired(FSession) then
        begin
          if not FSession.CanEdit then
            FSession.DataSet.Edit;
          FSession.FieldByName('ISACTIVE').AsString := 'N';
          FSession.DataSet.Post;
          // Expire session.
          If Assigned(OnExpired) then
            OnExpired(Self);
          S:='';
          FExpired := True;
        end
      else if (FSession.Count > 0) then
        SID:=S
      else
        begin
          if ARequest.CookieFields.IndexOf(S) > -1 then
            ARequest.CookieFields.Delete(ARequest.CookieFields.IndexOf(S));
          S := '';
        end;
    end;
  If (S='') and (not FExpired) then
    begin
      FSession.Select(ARequest.RemoteAddress,ARequest.UserAgent);
      FSession.Open;
      if FSession.Count > 0 then
        begin
          if (SessionFactory as TBaseSessionFactory).SessionExpired(FSession) then
            begin
              if not FSession.CanEdit then
                FSession.DataSet.Edit;
              FSession.FieldByName('ISACTIVE').AsString := 'N';
              FSession.DataSet.Post;
              // Expire session.
              If Assigned(OnExpired) then
                OnExpired(Self);
              S:='';
              FExpired := True;
            end
          else
            begin
              S := FSession.FieldByName('SID').AsString;
              SID := S;
            end;
        end;
    end;
  If (S='') then
    begin
      If Assigned(OnNewSession) then
        OnNewSession(Self);
      GetSessionID;
      FSession.Select(0);
      FSession.Open;
      with FSession.DataSet do
        begin
          Insert;
          FieldByName('SID').AsString := SID;
          FieldByName('TIMEOUT').AsInteger := Self.TimeOutMinutes;
          FieldByName('STARTED').AsDateTime := Now();
          FieldByName('HOST').AsString := ARequest.RemoteAddress;
          FieldByName('CLIENT').AsString := MD5Print(MD5String(ARequest.UserAgent));
          Post;
        end;
      if ARequest.Referer <> '' then
        Self.Variables['REFERER'] := ARequest.Referer;
      if Self.Variables['HostName'] = '' then
        begin
          Sock := TBlockSocket.Create;
          Self.Variables['HostName'] := AnsiToUTF8(Sock.ResolveIPToName(ARequest.RemoteAddress));
          Sock.Free;
        end;
      Self.Variables['UserAgent'] := AnsiToUTF8(ARequest.UserAgent);
      Self.Variables['OS'] := OSFromUserAgent(AnsiToUTF8(ARequest.UserAgent));
//      Self.Variables['AGENT'] := AgentFromUserAgent(AnsiToUTF8(ARequest.UserAgent));
//      Self.Variables['TYP'] := TypeFromUserAgent(AnsiToUTF8(ARequest.UserAgent));
      Self.Variables['Host'] := AnsiToUTF8(ARequest.Host);
      FSessionStarted:=True;
    end;
  if not FSession.CanEdit then
    FSession.DataSet.Edit;
  FSession.FieldByName('LASTACCESS').AsDateTime := Now();
  FSession.DataSet.Post;
  if ARequest.GetFieldByName('X-Forwarded-For') <> '' then
    begin
      Self.Variables['Forwarded'] := AnsiToUTF8(ARequest.GetFieldByName('X-Forwarded-For'));
    end
  else if ARequest.GetFieldByName('HTTP_X_FORWARDED_FOR') <> '' then
    begin
      Self.Variables['Forwarded'] := AnsiToUTF8(ARequest.GetFieldByName('HTTP_X_FORWARDED_FOR'));
    end;
end;
procedure TBaseWebSession.InitResponse(AResponse: TResponse);
Var
  C : TCookie;
begin
  If FSessionStarted then
    begin
      C:=AResponse.Cookies.FindCookie(SessionCookie);
      If (C=Nil) then
        begin
        C:=AResponse.Cookies.Add;
        C.Name:=SessionCookie;
        end;
      C.Value:=SID;
      C.Path:=SessionCookiePath;
    end
  else If FTerminated then
    begin
      C:=AResponse.Cookies.Add;
      C.Name:=SessionCookie;
      C.Value:='';
    end;
end;
procedure TBaseWebSession.RemoveVariable(VariableName: String);
begin
  if FSession.Count = 0 then exit;
  FSession.Variables.Select(VariableName);
  FSession.Variables.Open;
  if FSession.Variables.Count > 0 then
    FSession.Variables.DataSet.Delete;
end;

procedure TBaseWebSession.AddHistoryUrl(aUrl: string);
begin
  if not FSession.History.DataSet.Active then
    begin
      FSession.History.Select(0);
      FSession.History.Open;
    end;
  with FSession.History.DataSet do
    begin
      try
        if FSession.History.CanEdit then
          Cancel;
        Insert;
        FieldByName('URL').AsString := aUrl;
        Post;
      except
        Cancel;
      end;
    end;
end;

end.

