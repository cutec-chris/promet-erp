unit udavserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uhttpserver;

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

implementation

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

end.

