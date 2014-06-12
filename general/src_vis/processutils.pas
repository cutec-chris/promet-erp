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

procedure ExecVisualProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);

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

