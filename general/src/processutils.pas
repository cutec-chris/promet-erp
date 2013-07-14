unit ProcessUtils; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, AsyncProcess,UTF8Process, FileUtil
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;

type
  TLineWriteEvent = procedure(Line : string) of object;
  TCharWriteEvent = procedure(c : char) of object;

  { TExtendedProcess }

  TExtendedProcess = class(TAsyncProcess)
    procedure ExtendedProcessReadData(Sender: TObject);
    procedure ExtendedProcessTerminate(Sender: TObject);
  private
    FActive: Boolean;
    FOnCharWritten: TCharWriteEvent;
    FOnDone: TNotifyEvent;
    FOnLineWritten: TLineWriteEvent;
    FDataLine : string;
  public
    constructor Create(Cmdln : string;Autorun : Boolean = True;Dir : string = '');
    property    OnLineWritten : TLineWriteEvent read FOnLineWritten write FOnLineWritten;
    property    OnCharWritten : TCharWriteEvent read FOnCharWritten write FOnCharWritten;
    procedure   Writeln(str : string);
    property    OnDone : TNotifyEvent read FOnDone write FOnDone;
    procedure   Start;
  end;

procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
procedure ExecVisualProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;

implementation
uses ClipBrd;

{ TExtendedProcess }

procedure TExtendedProcess.ExtendedProcessReadData(Sender: TObject);
var
  tmp : string;
  len: LongWord;
  i: Integer;
begin
  len := NumBytesAvailable;
  if len > 0 then
    begin
      setlength(tmp,len);
      setlength(tmp,Output.Read(tmp[1],len));
      if Assigned(FOnCharWritten) then
        for i := 1 to length(tmp) do
          FOnCharWritten(tmp[i]);
      FDataLine := FdataLine+tmp;
      while pos(lineending,FDataLine) > 0 do
        begin
          if Assigned(FOnLineWritten) then
            FOnLineWritten(copy(FDataLine,0,pos(LineEnding,FDataLine)-1));
          FDataLine := copy(FDataLine,pos(LineEnding,FDataLine)+length(LineEnding),length(FDataLine));
        end;
    end;
end;

procedure TExtendedProcess.ExtendedProcessTerminate(Sender: TObject);
begin
  if Assigned(FOnDone) then
    FOnDone(Self);
end;

constructor TExtendedProcess.Create(Cmdln: string;Autorun : Boolean;Dir: string);
begin
  inherited Create(nil);
  FActive := False;
  FDataLine := '';
  Options:= [poUsePipes, poNoConsole, poStdErrToOutPut];
  ShowWindow := swoNone;
  CommandLine := Cmdln;
  OnTerminate :=@ExtendedProcessTerminate;
  OnreadData :=@ExtendedProcessReadData;

  if Dir <> '' then
    CurrentDirectory := Dir;
  if Autorun then
    begin
      try
        Execute;
      except
        ExtendedProcessTerminate(self);
        exit;
      end;
    end
end;

procedure TExtendedProcess.Writeln(str: string);
var
  tmp : string;
begin
  Input.Write(str[1],length(str));
  tmp := #10;
  Input.Write(tmp[1],1);
end;

procedure TExtendedProcess.Start;
begin
  if Active then exit;
  Execute;
end;

function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
var
  process : TProcessUTF8;
  tmps: tstringlist;
  err : string = '';
begin
  Process := TProcessUTF8.Create(nil);
  Process.Options:= [poUsePipes, poWaitOnExit, poNoConsole, poStdErrToOutPut, poNewProcessGroup];
//  Process.ShowWindow := swoHide;
  Process.CommandLine := CommandLine;
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  try
    Process.Execute;
  except
    on e : exception do
      err := err+#13+e.Message;
  end;
  tmps := TStringList.Create;
  tmps.LoadFromStream(Process.Output);
  Process.Free;
  Result := tmps.Text;
  tmps.Free;
  if err <> '' then
    Result := 'errors:'+err+#13+Result;
end;
procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
var
{$IFDEF MSWINDOWS}
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  Res: Boolean;
{$ELSE}
  process : TProcessUTF8;
{$ENDIF}
  aDir: String;
begin
  aDir := GetCurrentDirUTF8;
  if CurDir <> '' then
    ChDir(CurDir);
{$IFDEF MSWINDOWS}
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE
  end;
  Res := CreateProcess(NIL, PChar(UTF8ToSys(CommandLine)), NIL, NIL, FALSE,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS, NIL,
                          PChar(UTF8ToSys(CurDir)),
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
//  Clipboard.AsText:=CommandLine;
  if Res and Waitfor then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
{$ELSE}
  Process := TProcessUTF8.Create(nil);
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  Process.CommandLine := CommandLine;
  if Waitfor then
    Process.Options := [poNoConsole,poWaitOnExit]
  else
    Process.Options := [poNoConsole];
//  Process.ShowWindow := swoHide;
  Process.Execute;
  if Waitfor then Process.Free;
{$ENDIF}
  ChDir(aDir);
end;

procedure ExecVisualProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
var
  process : TProcessUTF8;
begin
  Process := TProcessUTF8.Create(nil);
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  Process.CommandLine := CommandLine;
  if Waitfor then
    Process.Options := [poWaitOnExit]
  else
    Process.Options := [];
  Process.Execute;
  if Waitfor then Process.Free;
end;


end.
