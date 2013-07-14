unit ubackupmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, XMLPropStorage, uAppconsts, Processutils,Utils,FileUtil,
  Clipbrd, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    bSync: TButton;
    cbFullBackup: TCheckBox;
    deSource: TDirectoryEdit;
    deTarget: TDirectoryEdit;
    lLastBackupDir: TLabel;
    lFile: TLabel;
    lFileStatus: TLabel;
    lStatus: TLabel;
    lSourceDirectory: TLabel;
    lTargetDirectory: TLabel;
    mOutput: TMemo;
    pbOverall: TProgressBar;
    pbFile: TProgressBar;
    Properties: TXMLPropStorage;
    procedure bSyncClick(Sender: TObject);
    procedure deTargetChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ProcessBlockWritten(Line: string);
    procedure ProcessDone(Sender: TObject);
  private
    { private declarations }
    Process: TExtendedProcess;
    Fileprogress: Boolean;
    procedure ProcessLine(Line : string);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not DirectoryExistsUTF8(GetConfigDir(vAppname)) then
    ForceDirectoriesUTF8(GetConfigDir(vAppname));
  Properties.FileName := GetConfigDir(vAppname)+'backupcfg.xml';
  Properties.Restore;
  deSource.Directory:=Properties.StoredValue['SOURCEDIR'];
  deTarget.Directory:=Properties.StoredValue['TARGETDIR'];
  deTargetChange(nil);
end;

procedure TForm1.ProcessBlockWritten(Line: string);
begin
  while pos(#10,Line) > 0 do
    begin
      Processline(copy(Line,0,pos(#10,Line)-1));
      mOutput.Lines.Add(copy(Line,0,pos(#10,Line)-1));
      Line := copy(Line,pos(#10,Line)+1,length(Line));
    end;
  mOutput.Lines.Add(Line);
  ProcessLine(Line);
end;

procedure TForm1.ProcessDone(Sender: TObject);
begin
  bSync.Enabled := True;
  pbOverall.Position := 0;
  pbFile.Position := 0;
  lFile.Caption := '';
  lFileStatus.Caption := '';
  deTargetChange(nil);
end;

procedure TForm1.ProcessLine(Line: string);
var
  nline: String;
begin
  if trim(Line) = '' then exit;
  if pos('files to consider',Line) > 0 then
    begin
      Fileprogress := True;
      pbOverall.Max:=StrToIntDef(copy(trim(Line),0,pos(' ',trim(Line))-1),0);
    end
  else if copy(Line,0,4) = 'sent' then
    FileProgress := false;
  if pos('(xfer#',Line) > 0 then
    begin
      Line := copy(Line,pos('(xfer#',Line)+6,length(Line));
      pbOverall.Position:=StrToIntDef(copy(Line,0,pos(',',Line)-1),0);
    end;
  if Fileprogress then
    begin
      if copy(Line,0,1) = ' ' then
        begin
          nline := copy(trim(Line),0,pos(' ',trim(Line))-1);
          Line := copy(trim(Line),pos(' ',trim(Line))+1,length(trim(Line)));
          if pos(nline,line) > 0 then
            nLine := copy(Line,pos(nline,line)-1,length(line))
          else nLine := '';
          pbFile.Position:=StrToIntDef(copy(Line,0,pos('%',Line)-1),0);
          lFileStatus.Caption := trim(copy(Line,pos(' ',Line)+1,length(Line)));
          if nLine <> '' then ProcessLine(nline);
        end
      else
        begin
          pbFile.Position := 0;
          lFile.Caption:=Line;
        end;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Properties.StoredValue['SOURCEDIR'] := deSource.Directory;
  Properties.StoredValue['TARGETDIR'] := deTarget.Directory;
  Properties.Save;
end;

procedure TForm1.bSyncClick(Sender: TObject);
var
  Cmdln: String;
  i: Integer;
begin
  FileProgress := false;
  CmdLn := '--archive --verbose --progress --delete';
  if not ((lLastBackupDir.Caption <> '') and (not cbFullBackup.Checked)) then
    Cmdln := CmdLn+' "'+deSource.Text+'" "'+AppendPathDelim(deTarget.Text)+ValidateFileName(DateToStr(Date)+'_'+TimeToStr(Time))+'_full"'
  else
    Cmdln := CmdLn+' "'+deSource.Text+'" "'+AppendPathDelim(deTarget.Text)+ValidateFileName(DateToStr(Date)+'_'+TimeToStr(Time))+'"';
  if (lLastBackupDir.Caption <> '') and (not cbFullBackup.Checked) then
    CmdLn := CmdLn+' --link-dest="'+lLastBackupDir.Caption+'"';
  for i := 0 to 26 do
    CmdLn := StringReplace(CmdLn,chr(ord('a')+i)+':\','/cygdrive/'+chr(ord('A')+i)+'/',[rfReplaceAll,rfIgnoreCase]);
//  Cmdln := '';
  if not Assigned(Process) then
    Process.Free;
  mOutput.Text:=AppendPathDelim(AppendPathDelim(ExtractFileDir(Application.ExeName))+'tools')+'rsync.exe '+CmdLn;
  Process := TExtendedProcess.Create(AppendPathDelim(AppendPathDelim(ExtractFileDir(Application.ExeName))+'tools')+'rsync.exe '+CmdLn,'');
  Process.OnBlockWritten:=@ProcessBlockWritten;
  Process.OnDone:=@ProcessDone;
  bSync.Enabled := False;
  Process.Resume;
end;

procedure TForm1.deTargetChange(Sender: TObject);
var
  Info : TSearchRec;
begin
  lLastBackupDir.Caption := '';
  if DirectoryExistsUTF8(deTarget.Text) then
    begin
      if FindFirstUTF8(AppendPathDelim(deTarget.Text)+'*',faDirectory,Info)=0 then
        begin
          repeat
            if Info.Name[1] <> '.' then
              lLastBackupDir.Caption := AppendPathDelim(deTarget.Text)+Info.Name;
          until FindNextUTF8(info)<>0;
        end;
      FindCloseUTF8(Info);
    end;
end;

initialization
  {$I ubackupmain.lrs}

end.
