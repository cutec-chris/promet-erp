unit uCDMenueMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  Ipfilebroker, IpHtml,ProcessUtils,Utils,LCLIntf;

type

  { TSimpleIpHtml }

  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

  { TfMain }

  TfMain = class(TForm)
    ipHTML: TIpHtmlPanel;
    procedure FormCreate(Sender: TObject);
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure ipHTMLHotClick(Sender: TObject);
  private
    { private declarations }
    HTMLPath : string;
  public
    { public declarations }
    procedure OpenHTMLFile(const Filename: string);
  end;

var
  fMain: TfMain;

implementation
{$R *.lfm}
uses Process
  {$ifdef WINDOWS}
  ,Windows
  {$endif}
  ;
{ TfMain }

procedure TfMain.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  PicCreated: boolean;
  MURL: String;
begin
  try
    MURL := URL;
    if not FileExistsUTF8(MURL) then
      MURL := AppendPathDelim(HTMLPath)+URL;
    if FileExistsUTF8(MURL) then
      begin
        PicCreated := False;
        if Picture=nil then
          begin
            Picture:=TPicture.Create;
            PicCreated := True;
          end;
        Picture.LoadFromFile(MURL);
    end;
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end;

procedure TfMain.ipHTMLHotClick(Sender: TObject);
var
  NodeA: TIpHtmlNodeA;
  tmp: String;
begin
  if IpHtml.HotNode is TIpHtmlNodeA then
    begin
      NodeA:=TIpHtmlNodeA(IpHtml.HotNode);
      if ExtractFileExt(NodeA.HRef) = '.html' then
        OpenURL(NodeA.Href)
      else
        begin
          fMain.Hide;
          Application.ProcessMessages;
          tmp := NodeA.HRef;
          while pos(';',tmp) > 0 do
            begin
              ExecVisualProcess(copy(tmp,0,pos(';',tmp)-1),ExtractFilePath(Application.Exename),true);
              tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
            end;
          ExecVisualProcess(tmp,ExtractFilePath(Application.Exename),true);
          fMain.Show;
        end;
    end;
end;

{$IFDEF Windows}
function GetArch: string;
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
      Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL; // Result from IsWow64Process
  IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
    Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
      raise SysUtils.Exception.Create('IsWindows64: bad process handle');
    // Return result of function
    if IsWow64Result then
      Result := 'win64';
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := 'win32';
end;
{$ENDIF Windows}

{$IF DEFINED(DARWIN) or DEFINED(Linux) or DEFINED(Unix)}
function GetArch: string;
{
Detect if we are running on a 64 bit or 32 bit operating system/kernel
independently of bitness of this program, or actual hardware.
Adapted from Lazarus forum post by Shebuka:
http://www.lazarus.freepascal.org/index.php/topic,13109.msg71741.html#msg71741
}
var
  InfoProcess: TProcess;
  path: string;
  InfoResult: TStringList;
  ResultLine: string;
  NumRows, i: integer;
begin
  result := 'noarch'; //fail by default

  path := 'sh -c "uname -m"';
  InfoProcess := Tprocess.Create(nil);
  try
    InfoProcess.CommandLine := path;
    InfoProcess.Options := InfoProcess.Options + [poUsePipes, poWaitOnExit];
    InfoProcess.Execute;
  except
    InfoProcess.Free;
    exit; //result already was false...
  end;

  InfoResult := TStringList.Create;
  try
    InfoResult.LoadFromStream(InfoProcess.Output);
  except
    InfoResult.Free;
    exit; //result already was false...
  end;

  InfoProcess.Terminate(0);
  InfoProcess.Free;

  NumRows := InfoResult.Count;
  // Should be just one result; we'll take the first non-empty line
  for i := 0 to NumRows - 1 do
  begin
    ResultLine:=trim(Inforesult[i]);
    if ResultLine<>'' then
    begin
      Result := ResultLine;
      //Note: we're no only testing x86 architecture!
      //Don't know if s390 s390x are 64 bit, but not a big chance to encounter mainframes for now
      Break; //we're done.
    end;
  InfoResult.Free;
  end;
end;
{$ENDIF} //Darwin, Linux, Unix
procedure TfMain.FormCreate(Sender: TObject);
begin
{$ifdef WINDOWS}
  OpenHTMLFile(AppendPathDelim('prometheus')+'cdmenue.'+GetArch+'.html');
{$endif}
{$ifdef LINUX}
  OpenHTMLFile(AppendPathDelim('prometheus')+'cdmenue.linux-'+GetArch+'.html');
{$ELSE}
  OpenHTMLFile(AppendPathDelim('prometheus')+'cdmenue.unix-'+GetArch+'.html');
{$ENDIF}
end;

procedure TfMain.OpenHTMLFile(const Filename: string);
var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
begin
  try
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
    try
      HTMLPath := ExtractFilePath(UTF8ToSys(Filename));
      NewHTML:=TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      NewHTML.OnGetImageX:=@HTMLGetImageX;
      NewHTML.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    IpHtml.SetHtml(NewHTML);
  except
  end;
end;

initialization

end.
