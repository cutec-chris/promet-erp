program checkin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Classes, SysUtils, pcmdprometapp, CustApp ,uBaseCustomApplication,
  uBaseDBInterface,uWiki,uData,LConvEncoding, uDocuments,Utils,FileUtil
  { you can add units after this };

resourcestring
  strParametersMissing           = 'A parameter is missing !';
  strmandantnotFound             = 'Mandant not found !';

type

  { TChangeWikiPage }

  TChangeWikiPage = class(TBaseCustomApplication)
    function aDocumentCheckCheckinFiles(aFiles: TStrings; Directory: string;
      var Desc: string): Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TChangeWikiPage }

function TChangeWikiPage.aDocumentCheckCheckinFiles(aFiles: TStrings;
  Directory: string; var Desc: string): Boolean;
begin
end;

procedure TChangeWikiPage.DoRun;
var
  ErrorMsg: String;
  tmp: String;
  aWiki: TWikiList;
  aPage: TStringList;
  aDocument: TDocument;
  aDir: String;
  FileList: TStrings;
  i: Integer;
begin
  // parse parameters
  if HasOption('h','help') or (ParamCount < 2) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  StopOnException := True;
  if not Login then raise Exception.Create('Login failed !');
  aDocument := TDocument.Create(Self,Data);
  if pos('@',Params[ParamCount-1]) > 0 then
    begin
      aDocument.SelectByLink(Params[ParamCount-1]);
      aDocument.Open;
    end;
  aDir := ChompPathDelim(CleanAndExpandDirectory(Params[ParamCount]));
  if rpos(DirectorySeparator,aDir) > 0 then
    aDir := copy(aDir,0,rpos(DirectorySeparator,aDir)-1);
  aDocument.OnCheckCheckinFiles:=@aDocumentCheckCheckinFiles;
  writeln('collecting files... ('+aDir+')');
  FileList := aDocument.CollectCheckInFiles(aDir);
  i := 0;
  while i < FileList.Count do
    begin
      if FileList.ValueFromIndex[i] = 'N' then
        FileList.Delete(i)
      else inc(i);
    end;
  for i := 0 to FileList.Count-1 do
    writeln(FileList[i]);
  if FileList.Count > 0 then
    begin
      writeln('checking in files...');
      if not aDocument.CheckinFiles(FileList,aDir) then
        begin
          ExitCode:=1;
          writeln('failed to checkin Files to '+aDir);
        end;
    end
  else writeln('nothing to do...');
  FileList.Free;
  aDocument.Free;
  DoExit;
  // stop program loop
  Terminate;
end;

constructor TChangeWikiPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TChangeWikiPage.Destroy;
begin
  inherited Destroy;
end;

procedure TChangeWikiPage.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExtractFileName(ExeName),' Link File/Dir');
  writeln('  Link: an Link to the Document');
  writeln('  File/Dir: The File to Checkin');
end;

var
  Application: TChangeWikiPage;
begin
  Application:=TChangeWikiPage.Create(nil);
  Application.Run;
  Application.Free;
end.

