unit uDocumentProcess;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uDocuments, Process;
type
  TDocExecuteThread = class(TThread)
  private
    FCmd: string;
    FLanguage: string;
    FDoDelete: Boolean;
    FUseStarter: Boolean;
    FTempID: String;
    FDocument : TDocument;
    FExit : Integer;
    FDocActionId : Variant;
    FMimeId : Variant;
    procedure DoReturn;
  public
    property OwnCmd : string read FCmd;
    property Language : string read FLanguage write fLanguage;
    function DoExecuteDocumentCommands(bCmd: string; UseStarter: Boolean): Integer;
    function DoAfterExecutedCommands(aDoDelete: Boolean; ExStatus: Integer; TempId: string) : Boolean;
    constructor Create(Document : TDocument;Cmd : string;DoDelete : Boolean;UseStarter : Boolean;TempID : string;aDocActionId : Variant;aMimeId : Variant);
    procedure Execute;override;
  end;
var
  ProcessList : TList;
implementation
uses uBaseApplication, SecureUtils, uIntfStrConsts,uData,Utils;
resourcestring
  strFailedExecuteProcess       = 'Möglicherweise ist das auführen von "%s" fehlgeschlagen, Rückgabewert: %d';
  strNoValidCommand             = 'Sie haben einen ungültigen befehl in den Dateiaktionen angegeben. Gültige Befehle müssen mit exec: oder mkdir: beginnen.';
  strCantDeleteTempDirectory    = 'Das temporäre Verzeichnis konnte nicht gelöscht werden !';
  strCheckinFailed              = 'Holen der Dateien fehlgeschlagen, das Temporäre Verzeichnis wurde nicht gelöscht !';
procedure TDocExecuteThread.DoReturn;
begin
  DoAfterExecutedCommands(FDoDelete,FExit,FTempID);
  ProcessList.Remove(Self);
  FDocument.Destroy;
end;
function TDocExecuteThread.DoExecuteDocumentCommands(bCmd : string;UseStarter : Boolean) : Integer;
var
  Proc: TProcess;
  ACmd: String;
begin
  while bCmd <> '' do
    begin
      if pos(lineending,bCmd) > 0 then
        begin
          ACmd := copy(FCmd,0,pos(lineending,FCmd)-1);
          FCmd := copy(FCmd,pos(lineending,FCmd)+length(lineending),length(FCmd));
        end
      else
        begin
          ACmd := bCmd;
          bCmd := '';
        end;
      if copy(Uppercase(ACmd),0,5) = 'EXEC:' then
        begin
          ACmd := StringReplace(copy(ACmd,6,length(ACmd)),#13,'',[rfReplaceAll]);
          ACmd := StringReplace(ACmd,#10,'',[rfReplaceAll]);
          Proc := TProcess.Create(BaseApplication);
          if UseStarter then
            {$IFNDEF DARWIN}
            Proc.CommandLine := AppendPathDelim(ExtractFilePath(BaseApplication.Exename))+'pstarter'+ExtractFileExt(BaseApplication.Exename)+' '+Language+' '+ACmd
            {$ELSE}
            //TODO:add Language
            Proc.CommandLine := FileUtil.CleanAndExpandDirectory(BaseApplication.Location+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator)+'pstarter.app/Contents/MacOS/pstarter '+'de'+' '+ACmd
            {$ENDIF}
          else
            Proc.CommandLine := ACmd;
          Proc.Options := [poNoConsole, poNewProcessGroup, poWaitOnExit];
          Result := 0;
          try
            Proc.Execute;
          except
            Result := -1;
          end;
          if Result <> -1 then
            Result := Proc.ExitStatus;
          Proc.Free;
          if Result > 1 then
            raise Exception.Create(Format(strFailedExecuteProcess,[ACmd,Result]));
        end
      else if copy(Uppercase(ACmd),0,6) = 'MKDIR:' then
        begin
          ACmd := StringReplace(copy(ACmd,7,length(ACmd)),#13,'',[rfReplaceAll]);
          ForceDirectories(uniToSys(ACmd));
        end
      else raise Exception.Create(strNoValidCommand);
    end;
end;
function TDocExecuteThread.DoAfterExecutedCommands(aDoDelete : Boolean;ExStatus : Integer;TempId : string) : Boolean;
var
  Rec: String;
  OldFilter: String;
  filename : string;
  DelOK: Boolean;
  aRec: String;
  FileList: TStrings;
  aDocAction: TDocumentActions;
  aMime: TMimeTypes;
label DelRetry;
begin
  FileName := FDocument.GetIDCheckoutPath('',TempId);
  //Collect Checkin Documents
  FileList := FDocument.CollectCheckInFiles(FileName);
  if FDocument.CheckCheckinFiles(FileList,FileName) then
    begin
      //Checkin
      if FileList.Count > 0 then
        Result := FDocument.CheckinFiles(FileList,FileName)
      else Result := True;
    end;
  FileList.Free;
  //Remove Temp Stuff
  if aDoDelete and Result then
    begin
      if DirectoryExists(UniToSys(filename)) then
        begin
        DelRetry:
          DelOK := RemoveDir(filename);
        end;
    end
  else if aDoDelete then
    raise Exception.Create(strCheckinFailed)
    ;
  if FUseStarter and (ExStatus=1) then //Dont Use Starter again
    begin
      aDocAction := TDocumentActions.Create(nil);
      aDocAction.Select(FDocActionId);
      aDocAction.Open;
      if aDocAction.Count>0 then
        begin
          aDocAction.Edit;
          aDocAction.FieldByName('USESTARTER').AsString:='N';
          aDocAction.Post;
        end
      else
        begin
          aMime := TMimeTypes.Create(nil);
          aMime.Select(FMimeId);
          aMime.Open;
          if aMime.Count>0 then
            begin
              aMime.Edit;
              aMime.FieldByName('USESTARTER').AsString:='N';
              aMime.Post;
            end;
          aMime.Free;
        end;
      aDocAction.Free;
    end;
end;
constructor TDocExecuteThread.Create(Document: TDocument; Cmd: string;
  DoDelete: Boolean; UseStarter: Boolean; TempID: string;
  aDocActionId: Variant; aMimeId: Variant);
begin
  FDocActionId := aDocActionId;
  FMimeId := aMimeId;
  FCmd := Cmd;
  FDoDelete := DoDelete;
  FUseStarter := UseStarter;
  FTempID := TempID;
  FreeOnTerminate := True;
  FLanguage := 'de';
  FDocument := Document;
  ProcessList.Add(Self);
  if not BaseApplication.HasOption('disablethreads') then
    inherited Create(False)
  else
    Execute;
end;

procedure TDocExecuteThread.Execute;
begin
  FExit := DoExecuteDocumentCommands(FCmd,FUseStarter);
  Self.Synchronize(@DoReturn);
end;
initialization
  ProcessList := TList.Create
finalization
  ProcessList.Destroy;
end.
