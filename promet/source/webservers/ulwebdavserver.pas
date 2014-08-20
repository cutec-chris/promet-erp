unit ulwebdavserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lexthttp, lStrBuffer, dom, xmlread, xmlwrite;
type
  TLXMLURIHandler = class(TURIHandler)
  protected
    function CreateDocument(ASocket: TLHTTPServerSocket; var ContentType: String): TXMLDocument; virtual; abstract;
    procedure DestroyDocument(ADocument: TXMLDocument); virtual; abstract;
  end;
  TLWebDAVURIHandler = class(TLXMLURIHandler)
  protected
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
    function CreateDocument(ASocket: TLHTTPServerSocket; var ContentType: String): TXMLDocument; override;
    procedure DestroyDocument(ADocument: TXMLDocument); override;
  public
    constructor Create;
  end;
  TLWebDAVFileHandler = class(TURIHandler)
  protected
  public
    constructor Create;
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
  end;
  TLFile = class;
  TLDirectoryList = class(TList)
  private
    function Get(Index: Integer): TLFile;
    procedure Put(Index: Integer; AValue: TLFile);
  public
    constructor Create;
    property Files[Index: Integer]: TLFile read Get write Put; default;
    destructor Destroy;override;
  end;

  { TLFile }

  TLFile = class(TLDirectoryList)
  private
    FASet: TStringList;
    FCHS: string;
    FIsCal: Boolean;
    FIsCalU: Boolean;
    FIsDir: Boolean;
    FIsTodo: Boolean;
    FName: string;
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
  end;
  TLGetDirectoryList = function(aDir : string;aDepth : Integer;var aDirList : TLDirectoryList) : Boolean of object;
  TLGetCTag = function(aDir : string;var aCTag : Int64) : Boolean of object;
  TLFileEvent = function(aDir : string) : Boolean of object;
  TLFileStreamEvent = function(aDir : string;Stream : TStream;var eTag : string) : Boolean of object;
  TLFileStreamDateEvent = function(aDir : string;Stream : TStream;var FLastModified : TDateTime;var MimeType : string;var eTag : string) : Boolean of object;
  TLLoginEvent = function(aUser,aPassword : string) : Boolean of object;
  TLWebDAVServer = class(TLHTTPServer)
  private
    FCtag: TLGetCTag;
    FDelete: TLFileEvent;
    FGet: TLFileStreamDateEvent;
    FGetDirList: TLGetDirectoryList;
    FMkCol: TLFileEvent;
    FPost: TLFileStreamEvent;
    FPut: TLFileStreamEvent;
    FreadAllowed: TLFileEvent;
    FUserLogin: TLLoginEvent;
    FWriteAllowed: TLFileEvent;
  protected
    URIHandler: TLWebDAVURIHandler;
    FileHandler: TLWebDAVFileHandler;
    procedure RegisterHandlers;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnGetDirectoryList : TLGetDirectoryList read FGetDirList write FGetDirList;
    property OnMkCol : TLFileEvent read FMkCol write FMkCol;
    property OnDelete : TLFileEvent read FDelete write FDelete;
    property OnPutFile : TLFileStreamEvent read FPut write FPut;
    property OnPostFile : TLFileStreamEvent read FPost write FPost;
    property OnGetFile : TLFileStreamDateEvent read FGet write FGet;
    property OnReadAllowed : TLFileEvent read FreadAllowed write FReadAllowed;
    property OnWriteAllowed : TLFileEvent read FWriteAllowed write FWriteAllowed;
    property OnUserLogin : TLLoginEvent read FUserLogin write FUserLogin;
    property OngetCTag : TLGetCTag read FCtag write FCtag;
  end;
  TLWebDAVServerSocket = class(TLHTTPServerSocket)
  end;
  TXmlOutput = class(TMemoryStreamOutput)
  private
    FIn: TStringStream;
    FOut : TMemoryStream;
    ADoc: TXMLDocument;
  protected
    procedure DoneInput; override;
    function BuildStatus(aStatus : TLHTTPStatus) : string;
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; override;
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;virtual;abstract;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;
  end;
  TFileStreamInput = class(TStreamOutput)
  private
    FEvent: TLFileStreamEvent;
  protected
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; override;
    procedure DoneInput; override;
  public
    property Event : TLFileStreamEvent read FEvent write FEvent;
  end;
  TFileStreamOutput = class(TStreamOutput)
  private
    FEvent: TLFileStreamDateEvent;
  protected
    procedure DoneInput; override;
  public
    property Event : TLFileStreamDateEvent read FEvent write FEvent;
  end;
  TDAVOptionsOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVReportOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVFindPropOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVLockOutput = class(TXmlOutput)
  protected
  end;
  TDAVMkColOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
  TDAVDeleteOutput = class(TXmlOutput)
  protected
    function HandleXMLRequest(aDocument : TXMLDocument) : Boolean;override;
  end;
implementation
uses lHTTPUtil,lMimeTypes,Base64;

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
    procedure RemoveProp(aProp : string);
    var
      b: Integer;
    begin
      b := 0;
      while b < aNotFoundProp.Count do
        begin
          if pos(aProp,aNotFoundProp[b]) > 0 then
            aNotFoundProp.Delete(b)
          else inc(b);
        end;
    end;
  begin
    aNotFoundProp := TStringList.Create;
    aNotFoundProp.AddStrings(Properties);
    aResponse := aDocument.CreateElement('D:response');
    aParent.AppendChild(aResponse);
    aHref := aDocument.CreateElement('D:href');
    aResponse.AppendChild(aHref);
    aHRef.AppendChild(aDocument.CreateTextNode(aPath));
    aPropStat := aDocument.CreateElement('D:propstat');
    aResponse.AppendChild(aPropStat);
    aProp := aDocument.CreateElementNS(ns,prefix+':'+'prop');
    aProp.Prefix:=prefix;
    aPropStat.AppendChild(aProp);

    aStream := TStringStream.Create('');
    if Assigned(TLWebDAVServer(Socket.Creator).FGet) then
      TLWebDAVServer(Socket.Creator).FGet(aPath,aStream,FLastModified,FMimeType,FeTag);
    if (aNotFoundProp.IndexOf('D:getetag') > -1) and (FeTag<>'')  then
      begin
        aPropC := aDocument.CreateElement('D:getetag');
        aPropC.AppendChild(aDocument.CreateTextNode(FeTag));
        aProp.AppendChild(apropC);
        while aNotFoundProp.IndexOf('D:getetag') > -1 do
          aNotFoundProp.Delete(aNotFoundProp.IndexOf('D:getetag'));
      end;
    if (aNotFoundProp.IndexOf('C:calendar-data') > -1) and (aStream.DataString<>'')  then
      begin
        aPropC := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:calendar-data');
        aPropC.AppendChild(aDocument.CreateTextNode(aStream.DataString));
        aProp.AppendChild(apropC);
        while aNotFoundProp.IndexOf('C:calendar-data') > -1 do
          aNotFoundProp.Delete(aNotFoundProp.IndexOf('C:calendar-data'));
      end;

    aStream.Free;
    aStatus := aDocument.CreateElement('D:status');
    aPropStat.AppendChild(aStatus);
    aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(hsOK)));
    if aNotFoundProp.Count>0 then
      begin
        aPropStat := aDocument.CreateElement('D:propstat');
        aResponse.AppendChild(aPropStat);
        aProp := aDocument.CreateElementNS(ns,prefix+':'+'prop');
        aProp.Prefix:=prefix;
        aPropStat.AppendChild(aProp);
        for a := 0 to aNotFoundProp.Count-1 do
          begin
            case copy(aNotFoundProp[a],0,pos(':',aNotFoundProp[a])-1) of
            'C':aPropC := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav',aNotFoundProp[a]);
            'CS':aPropC := aDocument.CreateElementNS('http://calendarserver.org/ns/',aNotFoundProp[a]);
            else
              aPropC := aDocument.CreateElement(aNotFoundProp[a]);
            end;
            aProp.AppendChild(aPropC);
            writeln('Property not found:'+aNotFoundProp[a]);
          end;
        aStatus := aDocument.CreateElement('D:status');
        aPropStat.AppendChild(aStatus);
          aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(hsNotFound)));
      end;
    aNotFoundProp.Free;
  end;

begin
  aProperties := TStringList.Create;
  bProperties := TStringList.Create;
  aItems := TStringList.Create;
  if FSocket.Parameters[hpAuthorization] <> '' then
    begin
      aUser := FSocket.Parameters[hpAuthorization];
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      if Assigned(TLWebDAVServer(FSocket.Creator).OnUserLogin) then
        TLWebDAVServer(FSocket.Creator).OnUserLogin(copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
    end;
  if Assigned(aDocument.DocumentElement) then
    begin
      if trim(copy(aDocument.DocumentElement.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1)) <> '' then
        aPrefix := trim(copy(aDocument.DocumentElement.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1));
      aPropNode := TDOMElement(aDocument.DocumentElement.FindNode('D:prop'));
      for i := 0 to aPropNode.ChildNodes.Count-1 do
        begin
          aProperties.Add(aPropNode.ChildNodes.Item[i].NodeName);
        end;
      aPropNode := TDOMElement(aDocument.DocumentElement);
      for i := 0 to aPropNode.ChildNodes.Count-1 do
        begin
          if pos(':href',aPropNode.ChildNodes.Item[i].NodeName) > 0 then
            aItems.Add(aPropNode.ChildNodes.Item[i].FirstChild.NodeValue);
        end;
      aDocument.DocumentElement.Free;
    end;
  aMSRes := aDocument.CreateElementNS('DAV:','D:multistatus');
  aDocument.AppendChild(aMSRes);
  aDepth := StrToIntDef(TLHTTPServerSocket(FSocket).Parameters[hpDepth],0);
  for i := 0 to aItems.Count-1 do
    begin
      bProperties.Assign(aProperties);
      CreateResponse(aItems[i],aMSRes,bProperties);
    end;
  aProperties.Free;
  bProperties.Free;
  aItems.Free;
  TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsMultiStatus;
  Result:=True;
end;

procedure TFileStreamOutput.DoneInput;
var
  FModified : TDateTime;
  FMimeType,FeTag : string;
begin
  TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsNotFound;
  if Assigned(Event) then
    if Event(HTTPDecode(TLHTTPServerSocket(FSocket).FRequestInfo.Argument),FStream,FModified,FMimeType,FEtag) then
      begin
        TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsOK;
        TLHTTPServerSocket(FSocket).FHeaderOut.ContentLength := FStream.Size;
        TLHTTPServerSocket(FSocket).FResponseInfo.LastModified:=LocalTimeToGMT(FModified);
      end;
  FStream.Position:=0;
  TLHTTPServerSocket(FSocket).StartResponse(Self);
end;
function TFileStreamInput.HandleInput(ABuffer: pchar; ASize: integer
  ): integer;
begin
  Result := FStream.Write(ABuffer^,ASize);
end;

procedure TFileStreamInput.DoneInput;
var
  FeTag : string;
begin
  TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsNotAllowed;
  if Assigned(Event) then
    if Event(HTTPDecode(TLHTTPServerSocket(FSocket).FRequestInfo.Argument),FStream,FeTag) then
      TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsOK;
  TMemoryStream(FStream).Clear;
  TLHTTPServerSocket(FSocket).StartResponse(Self);
end;
constructor TLWebDAVFileHandler.Create;
begin
  Methods := [hmHead, hmGet, hmPost, hmPut];
end;
function TLWebDAVFileHandler.HandleURI(ASocket: TLHTTPServerSocket
  ): TOutputItem;
begin
  Result := nil;
  case ASocket.FRequestInfo.RequestType of
  hmHead,hmGet:
    begin
      if Assigned(TLWebDAVServer(ASocket.Creator).OnReadAllowed)
      and (not TLWebDAVServer(ASocket.Creator).OnReadAllowed(HTTPDecode(TLHTTPServerSocket(ASocket).FRequestInfo.Argument))) then
        begin
          ASocket.FResponseInfo.Status:=hsUnauthorized;
          exit;
        end;
      Result := TFileStreamOutput.Create(ASocket,TmemoryStream.Create,True);
      TFileStreamOutput(Result).Event:=TLWebDAVServer(ASocket.Creator).FGet;
    end;
  hmPut:
    begin
      if Assigned(TLWebDAVServer(ASocket.Creator).OnWriteAllowed)
      and (not TLWebDAVServer(ASocket.Creator).OnWriteAllowed(HTTPDecode(TLHTTPServerSocket(ASocket).FRequestInfo.Argument))) then
        begin
          ASocket.FResponseInfo.Status:=hsUnauthorized;
          exit;
        end;
      Result := TFileStreamInput.Create(ASocket,TmemoryStream.Create,True);
      TFileStreamInput(Result).Event:=TLWebDAVServer(ASocket.Creator).FPut;
    end;
  hmPost:
    begin
      if Assigned(TLWebDAVServer(ASocket.Creator).OnWriteAllowed)
      and (not TLWebDAVServer(ASocket.Creator).OnWriteAllowed(HTTPDecode(TLHTTPServerSocket(ASocket).FRequestInfo.Argument))) then
        begin
          ASocket.FResponseInfo.Status:=hsUnauthorized;
          exit;
        end;
      Result := TFileStreamInput.Create(ASocket,TmemoryStream.Create,True);
      TFileStreamInput(Result).Event:=TLWebDAVServer(ASocket.Creator).FPost;
    end;
  end;
end;
function TDAVDeleteOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
begin
  Result := False;
  if Assigned(TLWebDAVServer(FSocket.Creator).OnDelete) then
    Result := TLWebDAVServer(FSocket.Creator).OnDelete(HTTPDecode(TLHTTPServerSocket(FSocket).FRequestInfo.Argument));
end;
function TDAVMkColOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
begin
  Result := False;
  if Assigned(TLWebDAVServer(FSocket.Creator).OnMkCol) then
    Result := TLWebDAVServer(FSocket.Creator).OnMkCol(HTTPDecode(TLHTTPServerSocket(FSocket).FRequestInfo.Argument));
end;

function TLDirectoryList.Get(Index: Integer): TLFile;
begin
  Result := TLFile(Items[Index]);
end;

procedure TLDirectoryList.Put(Index: Integer; AValue: TLFile);
begin
  Items[Index] := Pointer(AValue);
end;
constructor TLDirectoryList.Create;
begin
  inherited;
end;
destructor TLDirectoryList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Files[i].Free;
  inherited Destroy;
end;

procedure TLFile.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;
constructor TLFile.Create(aName: string;aIsDir : Boolean = False);
begin
  inherited Create;
  FName := aName;
  FIsDir := aIsDir;
  FIsCal:=False;
  FIsTodo:=False;
  FProperties := TStringList.Create;
  FASet := TStringList.Create;
end;

destructor TLFile.Destroy;
begin
  FASet.Free;
  FProperties.Free;
  inherited Destroy;
end;

function TDAVOptionsOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
var
  aOptionsRes: TDOMElement;
  tmp: DOMString = 'D:options';
  aActivityCollection: TDOMElement = nil;
  aHref: TDOMElement;
  aDirList : TLDirectoryList;
  Path: string;
  aDepth: Integer;
begin
  Result := False;
  aOptionsRes := aDocument.CreateElementNS('DAV:','D:options-response');
  if Assigned(aDocument.DocumentElement) then
    aActivityCollection := TDOMElement(aDocument.DocumentElement.FirstChild);
  if not Assigned(aActivityCollection) then
    aActivityCollection := aDocument.CreateElement('D:activity-collection-set');
  aOptionsRes.AppendChild(aActivityCollection);
  aHref := aDocument.CreateElement('D:href');
  Path := TLHTTPServerSocket(FSocket).FRequestInfo.Argument;
  if copy(Path,0,1) <> '/' then Path := '/'+Path;
  if pos('trunk',Path) > 0 then
    Path := StringReplace(Path,'trunk','!svn/act',[]);
  aDepth := StrToIntDef(TLHTTPServerSocket(FSocket).Parameters[hpDepth],0);
  if Assigned(TLWebDAVServer(FSocket.Creator).OnGetDirectoryList) then
    Result := TLWebDAVServer(FSocket.Creator).OnGetDirectoryList(Path,aDepth,aDirList)
  else Result:=True;

  aHRef.AppendChild(aDocument.CreateTextNode(Path));
  aActivityCollection.AppendChild(aHref);
  if Assigned(aDocument.DocumentElement) then
    aDocument.DocumentElement.Free;
  aDocument.AppendChild(aOptionsRes);
  TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsOK;
end;
function TDAVFindPropOutput.HandleXMLRequest(aDocument: TXMLDocument): Boolean;
var
  tmp: DOMString;
  aMSRes: TDOMElement;
  Path: string;
  aDirList : TLDirectoryList = nil;
  i: Integer;
  pfNode: TDOMElement;
  aNs : string = 'DAV:';
  aPrefix : string = 'D';
  aProperties: TStringList;
  aPropNode: TDOMElement;
  aUser: String;
  aDepth: Integer;

  procedure CreateResponse(aPath : string;aParent : TDOMElement;Properties : TStrings;ns : string = 'DAV:';prefix : string = 'D';aFile : TLFile = nil);
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
    procedure RemoveProp(aProp : string);
    var
      b: Integer;
    begin
      b := 0;
      while b < aNotFoundProp.Count do
        begin
          if pos(aProp,aNotFoundProp[b]) > 0 then
            aNotFoundProp.Delete(b)
          else inc(b);
        end;
    end;
  begin
    aNotFoundProp := TStringList.Create;
    aNotFoundProp.AddStrings(Properties);
    aResponse := aDocument.CreateElement('D:response');
    aParent.AppendChild(aResponse);
    aHref := aDocument.CreateElement('D:href');
//    RemoveProp(':href');
    aResponse.AppendChild(aHref);
    if not (Assigned(aFile) and (not aFile.IsDir)) then
      if copy(aPath,length(aPath),1) <> '/' then aPath := aPath+'/';
    aHRef.AppendChild(aDocument.CreateTextNode(aPath));
    aPropStat := aDocument.CreateElement('D:propstat');
    aResponse.AppendChild(aPropStat);
    aProp := aDocument.CreateElementNS(ns,prefix+':'+'prop');
    aProp.Prefix:=prefix;
    aPropStat.AppendChild(aProp);
    if Assigned(aFile) then
      begin
        aPropC := nil;
        if (aNotFoundProp.IndexOf(prefix+':'+'resourcetype') > -1)  then
          begin
            while aNotFoundProp.IndexOf(prefix+':'+'resourcetype') > -1 do
              aNotFoundProp.Delete(aNotFoundProp.IndexOf(prefix+':'+'resourcetype'));
            aPropC := aDocument.CreateElement(prefix+':'+'resourcetype');
            aProp.AppendChild(aPropC);
          end;
        if aFile.IsCalendar then
          begin
            if (aNotFoundProp.IndexOf('D:owner') > -1)  then
              begin
                aPropD := aDocument.CreateElement('D:owner');
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement('D:href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'user/'));
                while aNotFoundProp.IndexOf('D:owner') > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf('D:owner'));
              end;
            if (aNotFoundProp.IndexOf('C:schedule-inbox-URL') > -1)  then
              begin
                aPropD := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:schedule-inbox-URL');
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement('D:href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'outbox/'));
                while aNotFoundProp.IndexOf('C:schedule-inbox-URL') > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf('C:schedule-inbox-URL'));
              end;
            if (aNotFoundProp.IndexOf('C:schedule-outbox-URL') > -1)  then
              begin
                aPropD := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:schedule-outbox-URL');
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement('D:href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aPath+'inbox/'));
                while aNotFoundProp.IndexOf('C:schedule-outbox-URL') > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf('C:schedule-outbox-URL'));
              end;
            if (aNotFoundProp.IndexOf('C:calendar-home-set') > -1) then
              begin
                aPropD := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:calendar-home-set');
                aProp.AppendChild(apropD);
                aHref := aDocument.CreateElement('D:href');
                aPropD.AppendChild(aHref);
                aHRef.AppendChild(aDocument.CreateTextNode(aFile.CalendarHomeSet));
                while aNotFoundProp.IndexOf('C:calendar-home-set') > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf('C:calendar-home-set'));
              end;

            if (aFile.UserAdressSet.Count>0) and (aNotFoundProp.IndexOf('C:calendar-user-address-set') > -1) then
              begin
                aPropD := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:calendar-user-address-set');
                aProp.AppendChild(apropD);
                for b := 0 to aFile.UserAdressSet.Count-1 do
                  begin
                    aHref := aDocument.CreateElement('D:href');
                    aPropD.AppendChild(aHref);
                    aHRef.AppendChild(aDocument.CreateTextNode(aFile.UserAdressSet[b]));
                  end;
                while aNotFoundProp.IndexOf('C:calendar-user-address-set') > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf('C:calendar-user-address-set'));
              end;

            if not aFile.IsCalendarUser then
              begin
                if Assigned(aPropC) then
                  aPropC.AppendChild(aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:calendar'));
                if aNotFoundProp.IndexOf('D:supported-report-set') > -1 then
                  begin
                    aPropD := aDocument.CreateElement('D:supported-report-set');
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement('D:supported-report'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement('D:report'));
                    aPropG := aPropF.AppendChild(aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:calendar-multiget'));
                    {
                    aPropD := aDocument.CreateElement('D:supported-report-set');
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement('D:supported-report'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement('D:report'));
                    aPropG := aPropF.AppendChild(aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:calendar-query'));
                    }
                    {
                    aPropD := aDocument.CreateElement('D:supported-report-set');
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement('D:report'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement('D:expand-property'));
                    aPropD := aDocument.CreateElement('D:supported-report-set');
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement('D:report'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement('D:principal-property-search'));
                    aPropD := aDocument.CreateElement('D:supported-report-set');
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElement('D:report'));
                    aPropF := aPropE.AppendChild(aDocument.CreateElement('D:principal-search-property-set'));
                    }
                    while aNotFoundProp.IndexOf('D:supported-report-set') > -1 do
                      aNotFoundProp.Delete(aNotFoundProp.IndexOf('D:supported-report-set'));
                  end;

                if aNotFoundProp.IndexOf('C:supported-calendar-component-set') > -1 then
                  begin
                    //<C:supported-calendar-component-set><C:comp name="VEVENT"/></C:supported-calendar-component-set>
                    aPropD := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:supported-calendar-component-set');
                    aProp.AppendChild(aPropD);
                    aPropE := aPropD.AppendChild(aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:comp'));
                    TDOMElement(aPropE).SetAttribute('name','VEVENT');
                    if aFile.IsTodoList then
                      begin
                        aPropE := aPropD.AppendChild(aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav','C:comp'));
                        TDOMElement(aPropd).SetAttribute('name','VTODO');
                      end;
                    while aNotFoundProp.IndexOf('C:supported-calendar-component-set') > -1 do
                      aNotFoundProp.Delete(aNotFoundProp.IndexOf('C:supported-calendar-component-set'));
                  end;
              end;
          end;
        if aFile.IsDir then
          begin
            if Assigned(aPropC) then
              aPropC.AppendChild(aDocument.CreateElement('D:collection'));
            if not aFile.IsCalendar then
              begin
                if (aNotFoundProp.IndexOf('D:getcontenttype') > -1)  then
                  begin
                    aPropC := aDocument.CreateElement('D:getcontenttype');
                    while aNotFoundProp.IndexOf('D:getcontenttype') > -1 do
                      aNotFoundProp.Delete(aNotFoundProp.IndexOf('D:getcontenttype'));
                    aPropC.AppendChild(aDocument.CreateTextNode('httpd/unix-directory'));
                    aProp.AppendChild(apropC);
                  end;
              end;
          end;
        for a := 0 to aFile.Properties.Count-1 do
          begin
            if (aNotFoundProp.IndexOf(prefix+':'+aFile.Properties.Names[a]) > -1)
            or (aNotFoundProp.IndexOf(aFile.Properties.Names[a]) > -1)
            then
              begin
                while aNotFoundProp.IndexOf(prefix+':'+aFile.Properties.Names[a]) > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf(prefix+':'+aFile.Properties.Names[a]));
                while aNotFoundProp.IndexOf(aFile.Properties.Names[a]) > -1 do
                  aNotFoundProp.Delete(aNotFoundProp.IndexOf(aFile.Properties.Names[a]));
                if (aFile.Properties.Names[a] = 'getcontenttype')
                then
                  aPropC := aDocument.CreateElement('D:'+aFile.Properties.Names[a])
                else if pos(':',aFile.Properties.Names[a])=-1 then
                  aPropC := aDocument.CreateElement(prefix+':'+aFile.Properties.Names[a])
                else
                  begin
                    case copy(aFile.Properties.Names[a],0,pos(':',aFile.Properties.Names[a])-1) of
                    'C':aPropC := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav',aFile.Properties.Names[a]);
                    'CS':aPropC := aDocument.CreateElementNS('http://calendarserver.org/ns/',aFile.Properties.Names[a]);
                    else
                      aPropC := aDocument.CreateElement(aFile.Properties.Names[a]);
                    end;
                  end;
                aPropC.AppendChild(aDocument.CreateTextNode(aFile.Properties.ValueFromIndex[a]));
                aProp.AppendChild(aPropC);
              end;
          end;
      end
    else //root dir
      begin
        if (aNotFoundProp.IndexOf('D:getcontenttype') > -1)  then
          begin
            aPropC := aDocument.CreateElement('D:getcontenttype');
            aPropC.AppendChild(aDocument.CreateTextNode('httpd/unix-directory'));
            aProp.AppendChild(apropC);
          end;
        if (aNotFoundProp.IndexOf(prefix+':'+'resourcetype') > -1)  then
          begin
            aPropC := aDocument.CreateElement(prefix+':'+'resourcetype');
    //        RemoveProp(':resourcetype');
            aProp.AppendChild(aPropC);
            aPropC.AppendChild(aDocument.CreateElement('D:collection'));
          end;
      end;
    if (aNotFoundProp.IndexOf('D:supportedlock') > -1)  then
      begin
        aLock := aDocument.CreateElement('D:supportedlock');
        aLockEntry := aDocument.CreateElement('D:lockentry');
        aLock.AppendChild(aLockEntry);
        aLockEntry.AppendChild(aDocument.CreateElement('D:lockscope').AppendChild(aDocument.CreateElement('D:exclusive')));
        aLockEntry.AppendChild(aDocument.CreateElement('D:locktype').AppendChild(aDocument.CreateElement('D:write')));
        aProp.AppendChild(aLock);
      end;
    aStatus := aDocument.CreateElement('D:status');
    aPropStat.AppendChild(aStatus);
    aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(hsOK)));
    if aNotFoundProp.Count>0 then
      begin
        aPropStat := aDocument.CreateElement('D:propstat');
        aResponse.AppendChild(aPropStat);
        aProp := aDocument.CreateElementNS(ns,prefix+':'+'prop');
        aProp.Prefix:=prefix;
        aPropStat.AppendChild(aProp);
        for a := 0 to aNotFoundProp.Count-1 do
          begin
            case copy(aNotFoundProp[a],0,pos(':',aNotFoundProp[a])-1) of
            'C':aPropC := aDocument.CreateElementNS('urn:ietf:params:xml:ns:caldav',aNotFoundProp[a]);
            'CS':aPropC := aDocument.CreateElementNS('http://calendarserver.org/ns/',aNotFoundProp[a]);
            else
              aPropC := aDocument.CreateElement(aNotFoundProp[a]);
            end;
            aProp.AppendChild(aPropC);
            writeln('Property not found:'+aNotFoundProp[a]);
          end;
        aStatus := aDocument.CreateElement('D:status');
        aPropStat.AppendChild(aStatus);
          aStatus.AppendChild(aDocument.CreateTextNode(BuildStatus(hsNotFound)));
      end;
    aNotFoundProp.Free;
  end;

begin
  Result := False;
  aProperties := TStringList.Create;
  if FSocket.Parameters[hpAuthorization] <> '' then
    begin
      aUser := FSocket.Parameters[hpAuthorization];
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      if Assigned(TLWebDAVServer(FSocket.Creator).OnUserLogin) then
        TLWebDAVServer(FSocket.Creator).OnUserLogin(copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
    end;
  if Assigned(aDocument.DocumentElement) then
    begin
      if trim(copy(aDocument.DocumentElement.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1)) <> '' then
        aPrefix := trim(copy(aDocument.DocumentElement.NodeName,0,pos(':',aDocument.DocumentElement.NodeName)-1));
      aPropNode := TDOMElement(aDocument.DocumentElement.FirstChild);
      for i := 0 to aPropNode.ChildNodes.Count-1 do
        begin
          aProperties.Add(aPropNode.ChildNodes.Item[i].NodeName);
        end;
      aDocument.DocumentElement.Free;
    end;
  aMSRes := aDocument.CreateElementNS('DAV:','D:multistatus');
  aDocument.AppendChild(aMSRes);
  Path := HTTPDecode(TLHTTPServerSocket(FSocket).FRequestInfo.Argument);
  if copy(Path,0,1) <> '/' then Path := '/'+Path;
  aDepth := StrToIntDef(TLHTTPServerSocket(FSocket).Parameters[hpDepth],0);
  if Assigned(TLWebDAVServer(FSocket.Creator).OnGetDirectoryList) then
    Result := TLWebDAVServer(FSocket.Creator).OnGetDirectoryList(Path,aDepth,aDirList)
  else Result:=True;
  if Assigned(aDirList) and (aDirList.Count > 0) then
    begin
      Createresponse(Path,aMSres,aProperties,aNS,aPrefix);
      if copy(Path,length(Path),1) <> '/' then
        Path := Path+'/';
      for i := 0 to aDirList.Count-1 do
        Createresponse(Path+aDirList[i].Name,aMSres,aProperties,aNs,aPrefix,aDirList[i]);
    end
  else if Assigned(aDirList) and (aDirList is TLFile) then
    begin
      Createresponse(Path,aMSres,aProperties,aNs,aPrefix,TLFile(aDirList));
    end;
  TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsMultiStatus;
  if Assigned(TLWebDAVServer(FSocket.Creator).OnReadAllowed) and (not TLWebDAVServer(FSocket.Creator).OnReadAllowed(Path)) then
    begin
      TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsUnauthorized;
      AppendString(TLHTTPServerSocket(FSocket).FHeaderOut.ExtraHeaders, 'WWW-Authenticate: Basic realm="Promet-ERP"'+#13#10);
      Result := True;
    end;
  aProperties.Free;
end;
constructor TXmlOutput.Create(ASocket: TLHTTPSocket);
begin
  ADoc := TXMLDocument.Create;
  FIn := TStringStream.Create('');
  FOut := TMemoryStream.Create;
  inherited Create(ASocket, FOut, False);
end;
procedure TXmlOutput.DoneInput;
var
  tmp: String;
begin
  FIn.Position:=0;
  try
    if Fin.DataString <> '' then
      ReadXMLFile(ADoc,FIn);
  except
  end;
  FOut.Clear;
  if HandleXMLRequest(ADoc) then
    begin
      WriteXML(ADoc,FOut);
      Self.FBufferSize := FOut.Size;
      FOut.Position:=0;
      TLHTTPServerSocket(FSocket).FHeaderOut.ContentLength := FOut.Size;
      TLHTTPServerSocket(FSocket).Startmemoryresponse(TMemoryOutput(Self));
    end
  else
    begin
      TLHTTPServerSocket(FSocket).FHeaderOut.ContentLength := 0;
      TLHTTPServerSocket(FSocket).FResponseInfo.Status:=hsForbidden;
      FOut.Clear;
      TLHTTPServerSocket(FSocket).Startmemoryresponse(TMemoryOutput(Self));
    end;
end;
function TXmlOutput.BuildStatus(aStatus: TLHTTPStatus): string;
begin
  Result := 'HTTP/1.1 '+IntToStr(HTTPStatusCodes[aStatus])+' '+HTTPTexts[aStatus];
end;
function TXmlOutput.HandleInput(ABuffer: pchar; ASize: integer): integer;
begin
  FIn.WriteString(ABuffer);
  Result := ASize;
end;
destructor TXmlOutput.Destroy;
begin
  FIn.Free;
  FOut.Free;
  inherited Destroy;
end;
function TLWebDAVURIHandler.HandleURI(ASocket: TLHTTPServerSocket
  ): TOutputItem;
  procedure AddDAVheaders;
  begin
    AppendString(ASocket.FHeaderOut.ExtraHeaders, 'DAV: 1,2, access-control, calendar-access'+#13#10);
    AppendString(ASocket.FHeaderOut.ExtraHeaders, 'DAV: <http://apache.org/dav/propset/fs/1>'+#13#10);
    AppendString(ASocket.FHeaderOut.ExtraHeaders, 'MS-Author-Via: DAV'+#13#10);
    AppendString(ASocket.FHeaderOut.ExtraHeaders, 'Vary: Accept-Encoding'+#13#10);
  end;
begin
  ASocket.FResponseInfo.ContentType:='text/xml';
  ASocket.FResponseInfo.ContentCharset:='utf-8';
  Result := nil;
  if ASocket.FRequestInfo.RequestType = hmOptions then
    begin
      AddDAVheaders;
      AppendString(ASocket.FHeaderOut.ExtraHeaders,'DAV: version-control,checkout,working-resource'+#13#10);
      AppendString(ASocket.FHeaderOut.ExtraHeaders,'DAV: 1, calendar-access, calendar-schedule, calendar-proxy'+#13#10);
      AppendString(ASocket.FHeaderOut.ExtraHeaders,'allow: GET, HEAD, POST, OPTIONS, MKCOL, DELETE, PUT, LOCK, UNLOCK, COPY, MOVE, PROPFIND, SEARCH, REPORT, MKCALENDAR, ACL'+#13#10);
      Result := TDAVOptionsOutput.Create(ASocket);
    end
  else if ASocket.FRequestInfo.RequestType = hmReport then
    begin
      AddDAVheaders;
      AppendString(ASocket.FHeaderOut.ExtraHeaders,'DAV: version-control,checkout,working-resource'+#13#10);
      AppendString(ASocket.FHeaderOut.ExtraHeaders,'allow: GET, HEAD, POST, OPTIONS, MKCOL, DELETE, PUT, LOCK, UNLOCK, COPY, MOVE, PROPFIND, SEARCH, REPORT, MKCALENDAR, ACL'+#13#10);
      Result := TDAVReportOutput.Create(ASocket);
    end
  else if ASocket.FRequestInfo.RequestType = hmPropFind then
    begin
      AddDAVheaders;
      Result := TDAVFindPropOutput.Create(ASocket);
    end
  else if (ASocket.FRequestInfo.RequestType = hmLock)
       or (ASocket.FRequestInfo.RequestType = hmUnLock) then
    begin
      AddDAVheaders;
      Result := TDAVLockOutput.Create(ASocket);
    end
  else if (ASocket.FRequestInfo.RequestType = hmMkCol) then
    begin
      AddDAVheaders;
      Result := TDAVMkColOutput.Create(ASocket);
    end
  else if (ASocket.FRequestInfo.RequestType = hmDelete) then
    begin
      AddDAVheaders;
      Result := TDAVDeleteOutput.Create(ASocket);
    end
  ;
  if Assigned(Result) then
    begin
      try
        ASocket.FResponseInfo.ContentType := ASocket.FResponseInfo.ContentType;
      finally
      end;
    end;
end;
function TLWebDAVURIHandler.CreateDocument(ASocket: TLHTTPServerSocket;
  var ContentType: String): TXMLDocument;
begin
  Result := TXMLDocument.Create;
//  Result.Encoding:='utf-8';
end;
procedure TLWebDAVURIHandler.DestroyDocument(ADocument: TXMLDocument);
begin
  if Assigned(ADocument) then
    ADocument.Free;
end;
constructor TLWebDAVURIHandler.Create;
begin
  Methods := [hmOptions,hmPropfind,hmLock,hmUnlock,hmMkCol,hmDelete,hmReport];
end;
procedure TLWebDAVServer.RegisterHandlers;
begin
  URIHandler := TLWebDAVURIHandler.Create;
  FileHandler := TLWebDAVFileHandler.Create;
  Self.RegisterHandler(URIHandler);
  Self.RegisterHandler(FileHandler);
end;
constructor TLWebDAVServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SocketClass := TLWebDAVServerSocket;
  RegisterHandlers;
end;
destructor TLWebDAVServer.Destroy;
begin
  URIHandler.Destroy;
  FileHandler.Destroy;
  inherited Destroy;
end;
end.

