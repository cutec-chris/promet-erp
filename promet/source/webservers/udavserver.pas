unit udavserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uhttpserver, dom, xmlread, xmlwrite;

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
  TDAVGetDirectoryList = function(aDir : string;aDepth : Integer;var aDirList : TDAVDirectoryList) : Boolean of object;
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
  public
    constructor Create; override;
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

  { TXmlOutput }

  TXmlOutput = class(TMemoryStreamOutput)
  private
    FIn: TMemoryStream;
    FOut : TMemoryStream;
    ADoc: TXMLDocument;
  protected
    procedure DoneInput; override;
    function BuildStatus(aStatus : Integer) : string;
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;virtual;abstract;
  public
    constructor Create(ASocket: TDAVSocket;aIn,aOut : TMemoryStream);override;
    destructor Destroy; override;
  end;

  { TDAVOptionsOutput }

  TDAVOptionsOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;

implementation

uses base64;

const
  HTTPTexts: array[0..14] of string =
    ('', 'OK', 'No Content', 'Partial Content', 'Moved Permanently', 'Found',
     'Not Modified', 'Bad Request', 'Forbidden',
     'Not Found', 'Precondition Failed', 'Request Too Long', 'Internal Error',
     'Method Not Implemented', 'Method Not Allowed');

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
  if FSocket.Headers.Values['Authorization'] <> '' then
    begin
      aUser := FSocket.Headers.Values['Authorization'];
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      if Assigned(TWebDAVServer(FSocket.Creator).OnUserLogin) then
        TWebDAVServer(FSocket.Creator).OnUserLogin(copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
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
  aDepth := StrToIntDef(FSocket.Headers.Values['Depth'],0);
  if Assigned(TWebDAVServer(FSocket.Creator).OnGetDirectoryList) then
    Result := TWebDAVServer(FSocket.Creator).OnGetDirectoryList(Path,aDepth,aDirList)
  else Result:=True;

  aHRef.AppendChild(aDocument.CreateTextNode(Path));
  aActivityCollection.AppendChild(aHref);
  if Assigned(aDocument.DocumentElement) then
    aDocument.DocumentElement.Free;
  aDocument.AppendChild(aOptionsRes);
  FSocket.Status:=200;
  if Assigned(TWebDAVServer(FSocket.Creator).OnReadAllowed) and (not TWebDAVServer(FSocket.Creator).OnReadAllowed(Path)) then
    begin
      FSocket.Status:=401;
      TDAVSocket(FSocket).HeaderOut.Add('WWW-Authenticate: Basic realm="Promet-ERP"'+#13#10);
      Result := True;
    end;
end;

{ TDAVSocket }

function TDAVSocket.ProcessHttpRequest(Request, aURI: string): integer;
var
  Res : TMemoryStreamOutput = nil;

  procedure AddDAVheaders;
  begin
    HeaderOut.Add('DAV: 1,2, access-control, calendar-access'+#13#10);
    HeaderOut.Add('DAV: <http://apache.org/dav/propset/fs/1>'+#13#10);
    HeaderOut.Add('MS-Author-Via: DAV'+#13#10);
    HeaderOut.Add('Vary: Accept-Encoding'+#13#10);
  end;

begin
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
         HeaderOut.Add('DAV: version-control,checkout,working-resource'+#13#10);
         HeaderOut.Add('DAV: 1, calendar-access, calendar-schedule, calendar-proxy'+#13#10);
         HeaderOut.Add('allow: GET, HEAD, POST, OPTIONS, MKCOL, DELETE, PUT, LOCK, UNLOCK, COPY, MOVE, PROPFIND, SEARCH, REPORT, MKCALENDAR, ACL'+#13#10);
         Res := TDAVOptionsOutput.Create(Self,InputData,OutputData);
       end;
    end;
    if Assigned(Res) then
      begin
        Res.DoneInput;
        if Status<>0 then
          Result := Status;
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
end;

{ TDAVDirectoryList }

function TDAVDirectoryList.Get(Index: Integer): TDAVFile;
begin

end;

procedure TDAVDirectoryList.Put(Index: Integer; AValue: TDAVFile);
begin

end;

constructor TDAVDirectoryList.Create;
begin

end;

destructor TDAVDirectoryList.Destroy;
begin
  inherited Destroy;
end;

{ TDAVFile }

procedure TDAVFile.SetName(AValue: string);
begin

end;

constructor TDAVFile.Create(aName: string; aIsDir: Boolean);
begin

end;

destructor TDAVFile.Destroy;
begin
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
    ReadXMLFile(ADoc,FIn);
  except
  end;
  FOut.Clear;
  if HandleXMLRequest(ADoc) then
    begin
      WriteXML(ADoc,FOut);
      //Self.FBufferSize := FOut.Size;
      FOut.Position:=0;
      TDAVSocket(FSocket).HeaderOut.Add('ContentLength='+IntToStr(FOut.Size));
      TDAVSocket(FSocket).OutputData := Self.FOut;
    end
  else
    begin
      TDAVSocket(FSocket).HeaderOut.Add('ContentLength=0');
      TDAVSocket(FSocket).Status:=403;
      FOut.Clear;
      TDAVSocket(FSocket).OutputData := Self.FOut;
    end;
end;
function TXmlOutput.BuildStatus(aStatus: Integer): string;
begin
  Result := 'HTTP/1.1 '+IntToStr(aStatus)+' '+HTTPTexts[aStatus];
end;
destructor TXmlOutput.Destroy;
begin
  FIn.Free;
  FOut.Free;
  inherited Destroy;
end;


end.

