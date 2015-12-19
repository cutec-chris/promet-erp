unit udavserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uhttpserver, dom, xmlread, xmlwrite,syncobjs;

type
  TDAVFile = class;

  { TDAVDirectoryList }

  TDAVDirectoryList = class(TList)
  private
    function Get(Index: Integer): TDAVFile;
    procedure Put(Index: Integer; AValue: TDAVFile);
  public
    constructor Create;
    property Files[Index: Integer]: TDAVFile read Get write Put; default;
    destructor Destroy;override;
  end;

  { TDAVFile }

  TDAVFile = class(TDAVDirectoryList)
  private
    FASet: TStringList;
    FCHS: string;
    FFath: string;
    FIsCal: Boolean;
    FIsCalU: Boolean;
    FIsDir: Boolean;
    FIsTodo: Boolean;
    FName: string;
    FPath: string;
    FProperties: TStringList;
    procedure SetName(AValue: string);
  public
    constructor Create(aName : string;aIsDir : Boolean = False);
    destructor Destroy;override;
    property Name : string read FName write SetName;
    property Properties : TStringList read FProperties;
    property IsDir : Boolean read FIsDir;
    property IsCalendar : Boolean read FIsCal write FIsCal;
    property IsCalendarUser : Boolean read FIsCalU write FIsCalU;
    property IsTodoList : Boolean read FIsTodo write FIsTodo;
    property CalendarHomeSet : string read FCHS write FCHS;
    property UserAdressSet : TStringList read FASet;
    property Path : string read FFath write FPath;
  end;
  TDAVGetDirectoryList = function(aDir : string;a3 : Integer;var aDirList : TDAVDirectoryList) : Boolean of object;
  TDAVGetCTag = function(aDir : string;var aCTag : Int64) : Boolean of object;
  TDAVFileEvent = function(aDir : string) : Boolean of object;
  TDAVFileStreamEvent = function(aDir : string;Stream : TStream;var eTag : string;var Result : Integer) : Boolean of object;
  TDAVFileStreamDateEvent = function(aDir : string;Stream : TStream;var FLastModified : TDateTime;var MimeType : string;var eTag : string) : Boolean of object;
  TDAVLoginEvent = function(aUser,aPassword : string) : Boolean of object;
  TDAVAccessEvent = procedure(Info : string) of object;

  { TDAVSocket }

  TDAVSocket = class(TTCPHttpThrd)
  private
    FStatus: Integer;
    FURI: string;
    FHeadersOut : TStringList;
  public
    property URI : string read FURI;
    property Status : Integer read FStatus write FStatus;
    property HeaderOut : TStringList read FHeadersOut write FHeadersOut;
    function ProcessHttpRequest(Request, aURI: string): integer; override;
  end;

  { TWebDAVServer }

  TWebDAVServer = class(TTCPHttpDaemon)
  private
    FAccess: TDAVAccessEvent;
    FCtag: TDAVGetCTag;
    FDelete: TDAVFileEvent;
    FGet: TDAVFileStreamDateEvent;
    FGetDirList: TDAVGetDirectoryList;
    FMkCol: TDAVFileEvent;
    FPost: TDAVFileStreamEvent;
    FPut: TDAVFileStreamEvent;
    FreadAllowed: TDAVFileEvent;
    FUserLogin: TDAVLoginEvent;
    FWriteAllowed: TDAVFileEvent;
    FCS : TCriticalSection;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    property OnGetDirectoryList : TDAVGetDirectoryList read FGetDirList write FGetDirList;
    property OnMkCol : TDAVFileEvent read FMkCol write FMkCol;
    property OnDelete : TDAVFileEvent read FDelete write FDelete;
    property OnPutFile : TDAVFileStreamEvent read FPut write FPut;
    property OnPostFile : TDAVFileStreamEvent read FPost write FPost;
    property OnGetFile : TDAVFileStreamDateEvent read FGet write FGet;
    property OnReadAllowed : TDAVFileEvent read FreadAllowed write FReadAllowed;
    property OnWriteAllowed : TDAVFileEvent read FWriteAllowed write FWriteAllowed;
    property OnUserLogin : TDAVLoginEvent read FUserLogin write FUserLogin;
    property OngetCTag : TDAVGetCTag read FCtag write FCtag;
    property OnAccess : TDAVAccessEvent read FAccess write FAccess;
  end;

  { TStreamOutput }

  TStreamOutput = class
  private
    Foutput: TStream;
  protected
    FSocket: TDAVSocket;
    procedure DoneInput;virtual;abstract;
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; virtual;abstract;
  public
    constructor Create(ASocket: TDAVSocket;aIn,aOut : TMemoryStream);virtual;
    property Output : TStream read Foutput write FOutput;
  end;
  TMemoryStreamOutput = class(TStreamOutput)
  end;
  TXmlOutput = class(TMemoryStreamOutput)
  private
    FIn: TMemoryStream;
    FOut : TMemoryStream;
    ADoc: TXMLDocument;
  protected
    procedure DoneInput; override;
    function BuildStatus(aStatus: Integer; Statusname: string): string;
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;virtual;abstract;
  public
    constructor Create(ASocket: TDAVSocket;aIn,aOut : TMemoryStream);override;
    destructor Destroy; override;
  end;
  TDAVOptionsOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVFindPropOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVMkColOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVDeleteOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVReportOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;

implementation

uses base64,Utils,uhttputil;

{ TStreamOutput }

constructor TStreamOutput.Create(ASocket: TDAVSocket; aIn, aOut: TMemoryStream);
begin
  FSocket := ASocket;
end;

{ TDAVOptionsOutput }

function TDAVOptionsOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
var
  aOptionsRes: TDOMElement;
  tmp: DOMString = 'D:options';
  aActivityCollection: TDOMElement = nil;
  aHref: TDOMElement;
  aDirList : TDAVDirectoryList;
  Path: string;
  aDepth: Integer;
  aUser: string;
begin
  Result := False;
  if FSocket.Parameters.Values['authorization'] <> '' then
    begin
      aUser := FSocket.Parameters.Values['authorization'];
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      TWebDAVServer(FSocket.Creator).Lock;
      if Assigned(TWebDAVServer(FSocket.Creator).OnUserLogin) then
        TWebDAVServer(FSocket.Creator).OnUserLogin(copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
      TWebDAVServer(FSocket.Creator).Unlock;
    end;
  aOptionsRes := aDocument.CreateElementNS('DAV:','D:options-response');
  if Assigned(aDocument.DocumentElement) then
    aActivityCollection := TDOMElement(aDocument.DocumentElement.FirstChild);
  if not Assigned(aActivityCollection) then
    aActivityCollection := aDocument.CreateElement('D:activity-collection-set');
  aOptionsRes.AppendChild(aActivityCollection);
  aHref := aDocument.CreateElement('D:href');
  Path := TDAVSocket(FSocket).URI;
  if copy(Path,0,1) <> '/' then Path := '/'+Path;
  if pos('trunk',Path) > 0 then
    Path := StringReplace(Path,'trunk','!svn/act',[]);
  aDepth := StrToIntDef(trim(FSocket.Parameters.Values['depth']),0);
  if Assigned(TWebDAVServer(FSocket.Creator).OnGetDirectoryList) then
    Result := TWebDAVServer(FSocket.Creator).OnGetDirectoryList(Path,aDepth,aDirList)
  else Result:=True;

  aHRef.AppendChild(aDocument.CreateTextNode(Path));
  aActivityCollection.AppendChild(aHref);
  if Assigned(aDocument.DocumentElement) then
    aDocument.DocumentElement.Free;
  aDocument.AppendChild(aOptionsRes);
  FSocket.Status:=200;
  TWebDAVServer(FSocket.Creator).Lock;
  if Assigned(TWebDAVServer(FSocket.Creator).OnReadAllowed) and (not TWebDAVServer(FSocket.Creator).OnReadAllowed(Path)) then
    begin
      FSocket.Status:=401;
      TDAVSocket(FSocket).HeaderOut.Add('WWW-Authenticate: Basic realm="Promet-ERP"');
      Result := True;
    end;
  TWebDAVServer(FSocket.Creator).Unlock;
end;

{ TDAVSocket }

function TDAVSocket.ProcessHttpRequest(Request, aURI: string): integer;
var
  Res : TMemoryStreamOutput = nil;

  procedure AddDAVheaders;
  begin
    HeaderOut.Add('DAV: 1,2, access-control, calendar-access');
    HeaderOut.Add('DAV: <http://apache.org/dav/propset/fs/1>');
    HeaderOut.Add('MS-Author-Via: DAV');
    HeaderOut.Add('Vary: Accept-Encoding');
  end;

begin
  if Assigned(TWebDAVServer(Creator).OnAccess) then
    TWebDAVServer(Creator).OnAccess('<'+Request+' '+aURI);
  FURI:=aURI;
  HeaderOut := TStringList.Create;
  HeaderOut.Add('Content-type: text/xml');
  HeaderOut.Add('Content-Charset: utf8');
  try
    Result := 500;
    case Request of
    'OPTIONS':
       begin
         AddDAVheaders;
         HeaderOut.Add('DAV: version-control,checkout,working-resource');
         HeaderOut.Add('DAV: 1, calendar-access, calendar-schedule, calendar-proxy');
         HeaderOut.Add('allow: GET, HEAD, POST, OPTIONS, MKCOL, DELETE, PUT, LOCK, UNLOCK, COPY, MOVE, PROPFIND, SEARCH, REPORT, MKCALENDAR, ACL');
         Res := TDAVOptionsOutput.Create(Self,InputData,OutputData);
       end;
    'REPORT':
       begin
         AddDAVheaders;
         HeaderOut.Add('DAV: version-control,checkout,working-resource');
         HeaderOut.Add('allow: GET, HEAD, POST, OPTIONS, MKCOL, DELETE, PUT, LOCK, UNLOCK, COPY, MOVE, PROPFIND, SEARCH, REPORT, MKCALENDAR, ACL');
         Res := TDAVReportOutput.Create(Self,InputData,OutputData);
       end;
    'PROPFIND':
       begin
         AddDAVheaders;
         Res := TDAVFindPropOutput.Create(Self,InputData,OutputData);
       end;
    'MKCOL':
       begin
         AddDAVheaders;
         Res := TDAVMkColOutput.Create(Self,InputData,OutputData);
       end;
    'DELETE':
       begin
         AddDAVheaders;
         Res := TDAVDeleteOutput.Create(Self,InputData,OutputData);
       end;
    end;
    if Assigned(Res) then
      begin
        Res.DoneInput;
        if Status<>0 then
          Result := Status;
        Headers.Clear;
        Headers.AddStrings(HeaderOut);
        if Assigned(TWebDAVServer(Creator).OnAccess) then
          TWebDAVServer(Creator).OnAccess('>'+IntToStr(Result));
      end;
  finally
    HeaderOut.Free;
  end;
end;

{ TWebDAVServer }

constructor TWebDAVServer.Create;
begin
  inherited Create;
  ThreadType:=TDAVSocket;
  FCS := TCriticalSection.Create;
end;

destructor TWebDAVServer.Destroy;
begin
  FCS.Destroy;
  inherited Destroy;
end;

procedure TWebDAVServer.Lock;
begin
  FCS.Enter;
end;

procedure TWebDAVServer.Unlock;
begin
  FCS.Leave;
end;

{ TDAVDirectoryList }

function TDAVDirectoryList.Get(Index: Integer): TDAVFile;
begin
  Result := TDAVFile(Items[Index]);
end;

procedure TDAVDirectoryList.Put(Index: Integer; AValue: TDAVFile);
begin
  Items[Index] := Pointer(AValue);
end;

constructor TDAVDirectoryList.Create;
begin
  inherited;
end;

destructor TDAVDirectoryList.Destroy;
begin
  inherited Destroy;
end;

{ TDAVFile }

procedure TDAVFile.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

constructor TDAVFile.Create(aName: string; aIsDir: Boolean);
begin
  FName := aName;
  FIsDir := aIsDir;
  FIsCal:=False;
  FIsTodo:=False;
  FProperties := TStringList.Create;
  FASet := TStringList.Create;
  FPath := '';
end;

destructor TDAVFile.Destroy;
begin
  FASet.Free;
  FProperties.Free;
  inherited Destroy;
end;

constructor TXmlOutput.Create(ASocket: TDAVSocket; aIn, aOut: TMemoryStream);
begin
  ADoc := TXMLDocument.Create;
  FIn := aIn;
  FOut := aOut;
  inherited Create(ASocket,aIn,aOut);
end;
procedure TXmlOutput.DoneInput;
var
  tmp: String;
begin
  FIn.Position:=0;
  try
    if Fin.Size>0 then
      ReadXMLFile(ADoc,FIn);
  except
  end;
  FOut.Clear;
  if HandleXMLRequest(ADoc) then
    begin
      WriteXML(ADoc,FOut);
      writeln('<'+TDAVSocket(FSocket).Parameters.Text);
      writeln('<'+MemoryStreamToString(Fin));
      writeln('>'+MemoryStreamToString(FOut));
      //Self.FBufferSize := FOut.Size;
      FOut.Position:=0;
      TDAVSocket(FSocket).HeaderOut.Add('ContentLength: '+IntToStr(FOut.Size));
      TDAVSocket(FSocket).OutputData := Self.FOut;
    end
  else
    begin
      TDAVSocket(FSocket).HeaderOut.Add('ContentLength: 0');
      TDAVSocket(FSocket).Status:=403;
      FOut.Clear;
      TDAVSocket(FSocket).OutputData := Self.FOut;
    end;
end;
function TXmlOutput.BuildStatus(aStatus: Integer;Statusname : string): string;
begin
  Result := 'HTTP/1.1 '+IntToStr(aStatus)+' '+StatusName;
end;
destructor TXmlOutput.Destroy;
begin
  FIn.Free;
  FOut.Free;
  inherited Destroy;
end;
function TDAVFindPropOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
var
  tmp: DOMString;
  aMSRes: TDOMElement;
  Path: string;
  aDirList : TDAVDirectoryList = nil;
  i: Integer;
  pfNode: TDOMElement;
  aNs : string = 'DAV:';
  aPrefix : string = 'D';
  aProperties: TStringList;
  aPropNode: TDOMElement;
  aUser: String;
  aDepth: Integer;
  tmp1: DOMString;
  a: Integer;
  Attr: TDOMNode;
  aAttrPrefix: String;
  aLocalName,aNSName: String;
  Attr1: TDOMAttr;
  tmp2: DOMString;

  function AddNS(anPrefix,aNS : string) : string;
  var
    a : Integer;
    aFound: Boolean;
  begin
    aFound:=False;
    for a := 0 to aDocument.DocumentElement.Attributes.Length-1 do
      begin
        Attr := aDocument.DocumentElement.Attributes[a];
        aAttrPrefix := copy(Attr.NodeName,0,pos(':',Attr.NodeName)-1);
        aLocalName := copy(Attr.NodeName,pos(':',Attr.NodeName)+1,length(Attr.NodeName));
        aNSName := Attr.NodeValue;
        if (aAttrPrefix = 'xmlns') and (aNSName=aNS) then
          begin
            aFound:=True;
            Result := aLocalName;
            exit;
          end;
      end;
    if not aFound then
      begin
        Attr := TDomElement(aDocument.DocumentElement).OwnerDocument.CreateAttribute('xmlns:'+anPrefix);
        Attr.NodeValue:=aNS;
        aDocument.DocumentElement.Attributes.setNamedItem(Attr);
        Result := anPrefix;
        writeln('New NS:'+anPrefix+'='+aNS);
      end;
  end;
  procedure CreateResponse(aPath : string;aParent : TDOMElement;Properties : TStrings;ns : string = 'DAV:';prefix : string = 'D';aFile : TDAVFile = nil);
  var
    aResponse: TDOMElement;
    aHref: TDOMElement;
    aPropStat: TDOMElement;
    aStatus: TDOMElement;
    aPropC: TDOMElement;
    aProp: TDOMElement;
    a: Integer;
    aLock: TDOMElement;
    aLockEntry: TDOMElement;
    aNotFoundProp : TStrings;
    aPropD: TDOMNode;
    aPropE: TDOMNode;
    aPropF: TDOMNode;
    b: Integer;
    aPropG: TDOMNode;
    bPrefix: String;
    aTextNode: TDOMText;
    function FindProp(aprop : string) : Integer;
    var
      b : Integer;
    begin
      b := 0;
      while b < aNotFoundProp.Count do
        begin
          if pos(lowercase(aProp),lowercase(aNotFoundProp.Names[b])) > 0 then
            begin
              Result := b;
              exit;
            end
          else inc(b);
        end;
      Result := -1;
    end;
    procedure RemoveProp(aProp : string);
    var
      b : Integer;
    begin
      b := 0;
      while b < aNotFoundProp.Count do
        begin
          if pos(lowercase(aProp),lowercase(aNotFoundProp.Names[b])) > 0 then
            aNotFoundProp.Delete(b)
          else inc(b);
        end;
    end;
  begin
    writeln('CreateResponse:'+aPath+' '+prefix);
    aNotFoundProp := TStringList.Create;
    aNotFoundProp.AddStrings(Properties);
    aResponse := aDocument.CreateElement(prefix+':response');
    aParent.AppendChild(aResponse);
    aHref := aDocument.CreateElement(prefix+':href');
    RemoveProp(':href');
    aResponse.AppendChild(aHref);
    if not (Assigned(aFile) and (not aFile.IsDir)) then
      if copy(aPath,length(aPath),1) <> '/' then aPath := aPath+'/';
    aHRef.AppendChild(aDocument.CreateTextNode(aPath));
    aPropStat := aDocument.CreateElement(prefix+':propstat');
    aResponse.AppendChild(aPropStat);
    aProp := aDocument.CreateElement(prefix+':'+'prop');
    aPropStat.AppendChild(aProp);
    if Assigned(aFile) then
      begin
        aPropC := nil;
        if (FindProp(':resourcetype') > -1)  then
          begin
            tmp := aNotFoundProp.ValueFromIndex[FindProp(':resourcetype')];
            aPropC := aDocument.CreateElement(tmp);
            RemoveProp(prefix+':resourcetype');
            aProp.AppendChild(aPropC);
          end;
        if aFile.IsCalendar then
          begin
            if (FindProp(':owner') > -1)  then
              begin
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':owner')]);
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement(prefix+':href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'user/'));
                RemoveProp(':owner');
              end;
            if (FindProp(':current-user-principal') > -1)  then
              begin
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':current-user-principal')]);
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement(prefix+':href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'user/'));
                RemoveProp(':current-user-principal');
              end;
            if (FindProp(':calendar-user-address-set') > -1)  then
              begin
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':calendar-user-address-set')]);
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement(prefix+':href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'user/'));
                RemoveProp(':calendar-user-address-set');
              end;
            if (FindProp(':calendar-home-set') > -1) then
              begin
                tmp := aNotFoundProp.ValueFromIndex[FindProp(':calendar-home-set')];
                AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':calendar-home-set')]);
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement(prefix+':href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aFile.CalendarHomeSet));
                RemoveProp(':calendar-home-set');
              end;

            if (aFile.UserAdressSet.Count>0) and (FindProp(':calendar-user-address-set') > -1) then
              begin
                tmp := aNotFoundProp.ValueFromIndex[FindProp(':calendar-user-address-set')];
                AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':calendar-user-address-set')]);
                aProp.AppendChild(apropD);
                for b := 0 to aFile.UserAdressSet.Count-1 do
                  begin
                    aHref := aDocument.CreateElement(prefix+':href');
                    aPropD.AppendChild(aHref);
                    aHRef.AppendChild(aDocument.CreateTextNode(aFile.UserAdressSet[b]));
                  end;
                RemoveProp(':calendar-user-address-set');
              end;
            if FindProp(':schedule-inbox-URL') > -1  then
              begin
                tmp := aNotFoundProp.ValueFromIndex[FindProp(':schedule-inbox-URL')];
                AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':schedule-inbox-URL')]);
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement(prefix+':href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'outbox/'));
                RemoveProp(':schedule-inbox-URL');
              end;
            if FindProp(':schedule-outbox-URL') > -1  then
              begin
                tmp := aNotFoundProp.ValueFromIndex[FindProp(':schedule-outbox-URL')];
                AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':schedule-outbox-URL')]);
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement(prefix+':href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'inbox/'));
                RemoveProp(':schedule-outbox-URL');
              end;

            if not aFile.IsCalendarUser then
              begin
                if Assigned(aPropC) then
                  aPropC.AppendChild(aDocument.CreateElement(AddNS('C','urn:ietf:params:xml:ns:caldav')+':calendar'));
                if FindProp(':supported-report-set') > -1 then
                  begin
                    aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':supported-report-set')]);
                    bPrefix := copy(aPropD.NodeName,0,pos(':',aPropD.NodeName)-1);
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(prefix+':supported-report'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement(prefix+':report'));
                    aPropG := aPropF.AppendChild(aDocument.CreateElement(bPrefix+':calendar-multiget'));
                    RemoveProp(':supported-report-set');
                  end;
                if FindProp(':current-user-privilege-set') > -1 then
                  begin
                    tmp := aNotFoundProp.ValueFromIndex[FindProp(':current-user-privilege-set')];
                    aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':current-user-privilege-set')]);
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(prefix+':privilege'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement(prefix+':read'));
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(prefix+':privilege'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement(prefix+':read-acl'));
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(prefix+':privilege'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement(prefix+':read-current-user-privilege-set'));
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(prefix+':privilege'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement(prefix+':write'));
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(prefix+':privilege'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement(prefix+':write-acl'));
                    RemoveProp(':current-user-privilege-set');
                  end;
                if FindProp(':supported-calendar-component-set') > -1 then
                  begin
                    tmp := aNotFoundProp.ValueFromIndex[FindProp(':supported-calendar-component-set')];
                    AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                    aPropD := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':supported-calendar-component-set')]);
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement(aPrefix+':comp'));
                    TDOMElement(aPropE).SetAttribute('name','VEVENT');
                    if aFile.IsTodoList then
                      begin
                        aPropE := aPropD.AppendChild(aDocument.CreateElement(aPrefix+':comp'));
                        TDOMElement(aPropE).SetAttribute('name','VTODO');
                      end;
                    RemoveProp(':supported-calendar-component-set');
                  end;
              end;
          end;
        if aFile.IsDir then
          begin
            if Assigned(aPropC) then
              aPropC.AppendChild(aDocument.CreateElement(prefix+':collection'));
            if not aFile.IsCalendar then
              begin
                if (FindProp('getcontenttype') > -1)  then
                  begin
                    aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp('getcontenttype')]);
                    RemoveProp('getcontenttype');
                    aPropC.AppendChild(aDocument.CreateTextNode('httpd/unix-directory'));
                    aProp.AppendChild(apropC);
                  end;
              end;
          end;
        for a := 0 to aFile.Properties.Count-1 do
          begin
            if (FindProp(aFile.Properties.Names[a]) > -1) then
              begin
                if (aFile.Properties.Names[a] = 'getcontenttype')
                then
                  aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(aFile.Properties.Names[a])])
                else if pos(':',aFile.Properties.Names[a])=-1 then
                  aPropC := aDocument.CreateElement(prefix+':'+aFile.Properties.Names[a])
                else
                  begin
                    tmp := aNotFoundProp.ValueFromIndex[FindProp(aFile.Properties.Names[a])];
                    case copy(aFile.Properties.Names[a],0,pos(':',aFile.Properties.Names[a])-1) of
                    'C':
                      begin
                        AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                        aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(aFile.Properties.Names[a])]);
                      end;
                    'CS':
                      begin
                        AddNS(copy(tmp,0,pos(':',tmp)-1),'http://calendarserver.org/ns/');
                        aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(aFile.Properties.Names[a])]);
                      end;
                    else
                      aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(aFile.Properties.Names[a])]);
                    end;
                  end;
                RemoveProp(aFile.Properties.Names[a]);
                aPropC.AppendChild(aDocument.CreateTextNode(aFile.Properties.ValueFromIndex[a]));
                aProp.AppendChild(aPropC);
              end;
          end;
      end
    else //root dir
      begin
        if (FindProp(':getcontenttype') > -1)  then
          begin
            aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':getcontenttype')]);
            aPropC.AppendChild(aDocument.CreateTextNode('httpd/unix-directory'));
            aProp.AppendChild(apropC);
          end;
        if (FindProp('resourcetype') > -1)  then
          begin
            aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp('resourcetype')]);
            RemoveProp('resourcetype');
            aProp.AppendChild(aPropC);
            aPropC.AppendChild(aDocument.CreateElement(prefix+':collection'));
          end;
      end;
    if (FindProp(':supportedlock') > -1)  then
      begin
        aLock := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':supportedlock')]);
        aLockEntry := aDocument.CreateElement(prefix+':lockentry');
        aLock.AppendChild(aLockEntry);
        aLockEntry.AppendChild(aDocument.CreateElement(prefix+':lockscope').AppendChild(aDocument.CreateElement(prefix+':exclusive')));
        aLockEntry.AppendChild(aDocument.CreateElement(prefix+':locktype').AppendChild(aDocument.CreateElement(prefix+':write')));
        aProp.AppendChild(aLock);
      end;
    aStatus := aDocument.CreateElement(prefix+':status');
    aPropStat.AppendChild(aStatus);
    aTextNode := aDocument.CreateTextNode(BuildStatus(200,'OK'));
    aStatus.AppendChild(atextNode);
    if aNotFoundProp.Count>0 then
      begin
        aPropStat := aDocument.CreateElement(prefix+':propstat');
        aResponse.AppendChild(aPropStat);
        aProp := aDocument.CreateElement(prefix+':'+'prop');
        aPropStat.AppendChild(aProp);
        for a := 0 to aNotFoundProp.Count-1 do
          begin
            if FindProp(aNotFoundProp.ValueFromIndex[a])>-1 then
              tmp := aNotFoundProp.ValueFromIndex[FindProp(aNotFoundProp.ValueFromIndex[a])]
            else tmp := '';
            case copy(aNotFoundProp.ValueFromIndex[a],0,pos(':',aNotFoundProp.ValueFromIndex[a])-1) of
            'C':
              begin
                AddNS(copy(tmp,0,pos(':',tmp)-1),'urn:ietf:params:xml:ns:caldav');
                aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[a]);
              end;
            'CS':
              begin
                AddNS(copy(tmp,0,pos(':',tmp)-1),'http://calendarserver.org/ns/');
                aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[a]);
              end;
            else
              aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[a]);
            end;
            aProp.AppendChild(aPropC);
            writeln('Property not found:'+aNotFoundProp.ValueFromIndex[a]);
          end;
        aStatus := aDocument.CreateElement(prefix+':status');
        aPropStat.AppendChild(aStatus);
          aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(404,'Not Found')));
      end;
    aNotFoundProp.Free;
  end;

begin
  Result := False;
  writeln('***PROPFIND:'+HTTPDecode(TDAVSocket(FSocket).URI));
  aProperties := TStringList.Create;
  if FSocket.Parameters.Values['Authorization'] <> '' then
    begin
      aUser := FSocket.Parameters.Values['Authorization'];
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      TWebDAVServer(FSocket.Creator).Lock;
      if Assigned(TWebDAVServer(FSocket.Creator).OnUserLogin) then
        TWebDAVServer(FSocket.Creator).OnUserLogin(copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
      TWebDAVServer(FSocket.Creator).Unlock;
    end;
  if Assigned(aDocument.DocumentElement) then
    begin
      if trim(copy(aDocument.DocumentElement.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1)) <> '' then
        aPrefix := trim(copy(aDocument.DocumentElement.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1));
      aMSRes := aDocument.CreateElement(aPrefix+':multistatus');
      for a := 0 to aDocument.DocumentElement.Attributes.Length-1 do
        begin
          Attr := aDocument.DocumentElement.Attributes[a];
          aAttrPrefix := copy(Attr.NodeName,0,pos(':',Attr.NodeName)-1);
          aLocalName := copy(Attr.NodeName,pos(':',Attr.NodeName)+1,length(Attr.NodeName));
          aNSName := Attr.NodeValue;
          if (aAttrPrefix = 'xmlns') and (aLocalName<>'') then
            begin
              Attr1 := aDocument.DocumentElement.OwnerDocument.CreateAttribute('xmlns:'+aLocalName);
              Attr1.NodeValue:=aNSName;
              aMSRes.Attributes.setNamedItem(Attr1);
              writeln('Old NS:'+aLocalName+'='+aNSName);
            end
          else if (aLocalName = 'xmlns') and (aNSName<>'') then
            begin
              Attr1 := aDocument.DocumentElement.OwnerDocument.CreateAttribute('xmlns:'+aNSName);
              Attr1.NodeValue:=aPrefix;
              aMSRes.Attributes.setNamedItem(Attr1);
              writeln('Old NS:'+aNSName+'=');
            end;

        end;
      aPropNode := TDOMElement(aDocument.DocumentElement.FirstChild);
      for i := 0 to aPropNode.ChildNodes.Count-1 do
        begin
          tmp := aPropNode.ChildNodes.Item[i].NodeName;
          tmp := copy(tmp,pos(':',tmp)+1,length(tmp));
          tmp1 := copy(aPropNode.ChildNodes.Item[i].NodeName,0,pos(':',aPropNode.ChildNodes.Item[i].NodeName)-1);
          if aPropNode.ChildNodes.Item[i].NamespaceURI<>'' then
            tmp := aPropNode.ChildNodes.Item[i].NamespaceURI+':'+tmp
          else
            begin
              if pos(':',tmp)=0 then
                tmp := tmp1+':'+tmp;
            end;
          aProperties.Values[lowercase(tmp)]:=aPropNode.ChildNodes.Item[i].NodeName;
          writeln('Wanted:'+tmp+'='+aPropNode.ChildNodes.Item[i].NodeName);
        end;
      aDocument.DocumentElement.Free;
    end
  else
    aMSRes := aDocument.CreateElement(aPrefix+':multistatus');
  aDocument.AppendChild(aMSRes);
  Path := HTTPDecode(TDAVSocket(FSocket).URI);
  if copy(Path,0,1) <> '/' then Path := '/'+Path;
  aDepth := StrToIntDef(trim(FSocket.Parameters.Values['depth']),0);
  TWebDAVServer(FSocket.Creator).Lock;
  if Assigned(TWebDAVServer(FSocket.Creator).OnGetDirectoryList) then
    Result := TWebDAVServer(FSocket.Creator).OnGetDirectoryList(Path,aDepth,aDirList)
  else Result:=True;
  TWebDAVServer(FSocket.Creator).Unlock;
  if Assigned(aDirList) and (aDirList.Count > 0) then
    begin
      Createresponse(Path,aMSres,aProperties,aNS,aPrefix);
      if copy(Path,length(Path),1) <> '/' then
        Path := Path+'/';
      for i := 0 to aDirList.Count-1 do
        begin
          if aDirList[i].Path='' then
            Createresponse(Path+aDirList[i].Name,aMSres,aProperties,aNs,aPrefix,aDirList[i])
          else
            Createresponse(aDirList[i].Path+'/'+aDirList[i].Name,aMSres,aProperties,aNs,aPrefix,aDirList[i]);
        end;
    end
  else if Assigned(aDirList) and (aDirList is TDAVFile) then
    begin
      Createresponse(Path,aMSres,aProperties,aNs,aPrefix,TDAVFile(aDirList));
    end;
  TDAVSocket(FSocket).Status:=207;
  TWebDAVServer(FSocket.Creator).Lock;
  if Assigned(TWebDAVServer(FSocket.Creator).OnReadAllowed) and (not TWebDAVServer(FSocket.Creator).OnReadAllowed(Path)) then
    begin
      TDAVSocket(FSocket).Status:=403;
      TDAVSocket(FSocket).HeaderOut.Add('WWW-Authenticate: Basic realm="Promet-ERP"');
      Result := True;
    end;
  TWebDAVServer(FSocket.Creator).Unlock;
  aProperties.Free;
end;
function TDAVReportOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
var
  aProperties: TStringList;
  aUser: string;
  aPrefix: String;
  aItems: TStringList;
  aPropNode: TDOMElement;
  i: Integer;
  aMSRes: TDOMElement;
  aDepth: Integer;
  bProperties: TStringList;
  tmp: DOMString;
  tmp1: String;
  a: Integer;
  Attr: TDOMNode;
  aAttrPrefix: String;
  aLocalName: String;
  Attr1: TDOMAttr;
  aNSName: String;
  tmp2: DOMString;
  Path: string;
  aDirList : TDAVDirectoryList;
  aNode: TDOMNode;
  aFilter : string = '';

  procedure CreateResponse(aPath : string;aParent : TDOMElement;Properties : TStrings;ns : string = 'DAV:';prefix : string = 'D');
  var
    aResponse: TDOMElement;
    aHref: TDOMElement;
    aPropStat: TDOMElement;
    aStatus: TDOMElement;
    aPropC: TDOMElement;
    aProp: TDOMElement;
    a: Integer;
    aLock: TDOMElement;
    aLockEntry: TDOMElement;
    aNotFoundProp : TStrings;
    aPropD: TDOMNode;
    aPropE: TDOMNode;
    aPropF: TDOMNode;
    b: Integer;
    aPropG: TDOMNode;
    aStream : TStringStream;
    FLastModified : TDateTime;
    FMimeType,FeTag : string;
    function FindProp(aprop : string) : Integer;
    var
      b : Integer;
    begin
      b := 0;
      while b < aNotFoundProp.Count do
        begin
          if pos(lowercase(aProp),lowercase(aNotFoundProp.Names[b])) > 0 then
            begin
              Result := b;
              exit;
            end
          else inc(b);
        end;
      Result := -1;
    end;
    procedure RemoveProp(aProp : string);
    var
      b : Integer;
    begin
      b := 0;
      while b < aNotFoundProp.Count do
        begin
          if pos(lowercase(aProp),lowercase(aNotFoundProp.Names[b])) > 0 then
            aNotFoundProp.Delete(b)
          else inc(b);
        end;
    end;
  begin
    writeln('CreateResponse:'+aPath+' '+prefix);
    aNotFoundProp := TStringList.Create;
    aNotFoundProp.AddStrings(Properties);
    aResponse := aDocument.CreateElement(prefix+':response');
    aParent.AppendChild(aResponse);
    aHref := aDocument.CreateElement(prefix+':href');
    aResponse.AppendChild(aHref);
    aHRef.AppendChild(aDocument.CreateTextNode(aPath));
    aPropStat := aDocument.CreateElement(prefix+':propstat');
    aResponse.AppendChild(aPropStat);
    aProp := aDocument.CreateElement(prefix+':'+'prop');
    aPropStat.AppendChild(aProp);

    aStream := TStringStream.Create('');
    if Assigned(TWebDAVServer(FSocket.Creator).FGet) then
      TWebDAVServer(FSocket.Creator).FGet(aPath,aStream,FLastModified,FMimeType,FeTag);
    if (FindProp(':getetag') > -1) and (FeTag<>'')  then
      begin
        aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':getetag')]);
        aPropC.AppendChild(aDocument.CreateTextNode(FeTag));
        aProp.AppendChild(apropC);
        removeProp(':getetag');
      end;
    if (FindProp(':calendar-data') > -1) and (aStream.DataString<>'')  then
      begin
        aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[FindProp(':calendar-data')]);
        aPropC.AppendChild(aDocument.CreateTextNode(aStream.DataString));
        aProp.AppendChild(apropC);
        removeProp(':calendar-data');
      end;

    aStream.Free;
    aStatus := aDocument.CreateElement(prefix+':status');
    aPropStat.AppendChild(aStatus);
    aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(200,'OK')));
    if aNotFoundProp.Count>0 then
      begin
        aPropStat := aDocument.CreateElement(prefix+':propstat');
        aResponse.AppendChild(aPropStat);
        aProp := aDocument.CreateElement(prefix+':'+'prop');
        aPropStat.AppendChild(aProp);
        for a := 0 to aNotFoundProp.Count-1 do
          begin
            aPropC := aDocument.CreateElement(aNotFoundProp.ValueFromIndex[a]);
            aProp.AppendChild(aPropC);
            writeln('Property not found:'+aNotFoundProp.ValueFromIndex[a]);
          end;
        aStatus := aDocument.CreateElement(prefix+':status');
        aPropStat.AppendChild(aStatus);
        aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(404,'Not Found')));
      end;
    aNotFoundProp.Free;
  end;
  procedure RecourseFilter(aNode : TDOMNode);
  var
    b: Integer;
  begin
    if pos(':vevent',lowercase(aNode.NodeName))>0 then

    for b := 0 to aNode.ChildNodes.Count-1 do
      RecourseFilter(aNode.ChildNodes[i]);
  end;

begin
  aProperties := TStringList.Create;
  bProperties := TStringList.Create;
  writeln('***REPORT:'+HTTPDecode(TDAVSocket(FSocket).URI));
  aItems := TStringList.Create;
  if FSocket.Parameters.Values['authorization'] <> '' then
    begin
      aUser := FSocket.Parameters.Values['authorization'];
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      TWebDAVServer(FSocket.Creator).Lock;
      if Assigned(TWebDAVServer(FSocket.Creator).OnUserLogin) then
        TWebDAVServer(FSocket.Creator).OnUserLogin(copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
      TWebDAVServer(FSocket.Creator).Unlock;
    end;
  Path := TDAVSocket(FSocket).URI;
  if copy(Path,0,1) <> '/' then Path := '/'+Path;
  if Assigned(aDocument.DocumentElement) then
    begin
      if trim(copy(aDocument.DocumentElement.FirstChild.NodeName,0,pos(':',aDocument.DocumentElement.FirstChild.NodeName)-1)) <> '' then
        aPrefix := trim(copy(aDocument.DocumentElement.FirstChild.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1));
      aMSRes := aDocument.CreateElement(aPrefix+':multistatus');
      for a := 0 to aDocument.DocumentElement.Attributes.Length-1 do
        begin
          Attr := aDocument.DocumentElement.Attributes[a];
          aAttrPrefix := copy(Attr.NodeName,0,pos(':',Attr.NodeName)-1);
          aLocalName := copy(Attr.NodeName,pos(':',Attr.NodeName)+1,length(Attr.NodeName));
          aNSName := Attr.NodeValue;
          if (aAttrPrefix = 'xmlns') and (aLocalName<>'') then
            begin
              Attr1 := aDocument.DocumentElement.OwnerDocument.CreateAttribute('xmlns:'+aLocalName);
              Attr1.NodeValue:=aNSName;
              aMSRes.Attributes.setNamedItem(Attr1);
            end;
        end;
      aPropNode := TDOMElement(aDocument.DocumentElement.FindNode(aPrefix+':prop'));
      if Assigned(aPropNode) then
      for i := 0 to aPropNode.ChildNodes.Count-1 do
        begin
          tmp := aPropNode.ChildNodes.Item[i].NodeName;
          tmp := copy(tmp,pos(':',tmp)+1,length(tmp));
          tmp1 := copy(aPropNode.ChildNodes.Item[i].NodeName,0,pos(':',aPropNode.ChildNodes.Item[i].NodeName)-1);
          if aPropNode.ChildNodes.Item[i].NamespaceURI<>'' then
            tmp := aPropNode.ChildNodes.Item[i].NamespaceURI+':'+tmp
          else
            begin
              for a := 0 to aDocument.DocumentElement.Attributes.Length-1 do
                begin
                  Attr := aDocument.DocumentElement.Attributes[a];
                  aAttrPrefix := copy(Attr.NodeName,0,pos(':',Attr.NodeName)-1);
                  aLocalName := copy(Attr.NodeName,pos(':',Attr.NodeName)+1,length(Attr.NodeName));
                  if (aAttrPrefix = 'xmlns') and (aLocalName = tmp1) then
                    begin
                      case lowercase(Attr.NodeValue) of
                      'dav:':tmp := 'D:'+tmp;
                      'urn:ietf:params:xml:ns:caldav':tmp := 'C:'+tmp;
                      'http://calendarserver.org/ns/':tmp := 'CS:'+tmp;
                      end;
                    end;
                  if (aAttrPrefix = 'xmlns') then
                    begin
                      Attr1 := aDocument.DocumentElement.OwnerDocument.CreateAttribute('xmlns:'+aLocalName);
                      Attr1.Value:=Attr.NodeValue;
                      aMSRes.Attributes.setNamedItemNS(Attr1);
                    end;
                end;
              if pos(':',tmp)=0 then
                tmp := tmp1+':'+tmp;
            end;
          aProperties.Values[lowercase(tmp)]:=aPropNode.ChildNodes.Item[i].NodeName;
          writeln('Wanted:'+tmp+'='+aPropNode.ChildNodes.Item[i].NodeName);
        end;
      aPropNode := TDOMElement(aDocument.DocumentElement);
      for i := 0 to aPropNode.ChildNodes.Count-1 do
        begin
          tmp := aPropNode.ChildNodes.Item[i].NodeName;
          if pos(':href',lowercase(tmp)) > 0 then
            aItems.Add(aPropNode.ChildNodes.Item[i].FirstChild.NodeValue);
          if pos(':filter',lowercase(tmp)) > 0 then
            begin
              RecourseFilter(aPropNode.ChildNodes.Item[i]);
            end;
        end;
      if aItems.Count=0 then
        begin //we report all ??!
          aDirList := TDAVDirectoryList.Create;
          if TWebDAVServer(TDAVSocket(FSocket).Creator).FGetDirList(Path,1,aDirList) then
            for i := 0 to aDirList.Count-1 do
              begin
                aItems.Add(Path+aDirList[i].Name);
              end;
          aDirList.Free;
        end;
      aDocument.DocumentElement.Free;
    end
  else
    aMSRes := aDocument.CreateElement(aPrefix+':multistatus');
  aDocument.AppendChild(aMSRes);
  aDepth := StrToIntDef(trim(FSocket.Parameters.Values['depth']),0);
  for i := 0 to aItems.Count-1 do
    begin
      bProperties.Assign(aProperties);
      CreateResponse(aItems[i],aMSRes,bProperties);
    end;
  aProperties.Free;
  bProperties.Free;
  aItems.Free;
  TDAVSocket(FSocket).Status:=207;
  Result:=True;
end;
function TDAVDeleteOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
begin
  Result := False;
  TWebDAVServer(FSocket.Creator).Lock;
  if Assigned(TWebDAVServer(FSocket.Creator).OnDelete) then
    Result := TWebDAVServer(FSocket.Creator).OnDelete(HTTPDecode(TDAVSocket(FSocket).URI));
  TWebDAVServer(FSocket.Creator).Unlock;
end;
function TDAVMkColOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
begin
  Result := False;
  TWebDAVServer(FSocket.Creator).Lock;
  if Assigned(TWebDAVServer(FSocket.Creator).OnMkCol) then
    Result := TWebDAVServer(FSocket.Creator).OnMkCol(HTTPDecode(TDAVSocket(FSocket).URI));
  TWebDAVServer(FSocket.Creator).Unlock;
end;


end.

