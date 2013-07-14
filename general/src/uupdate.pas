unit uUpdate;

{$mode objfpc}{$H+}

interface 

uses
  Forms,Controls, Classes, SysUtils, FileUtil,Clipbrd,Dialogs,
  uGeneralStrConsts,uMashineID,Utils,http,md5,ProcessUtils;

const
  ST_ERROR = 1;
  ST_MESSAGE = 2;
type
  TUpdateProtokoll = (upFile,upFTP,upHTTP);
  TStatusEvent = procedure(StatusTyp : Integer;Stat : string) of object;
  TProgressEvent = procedure(Pos : Integer;Max : Integer) of object;
  TInfoEvent = procedure(Filename : string;FromRev,ToRev : Integer;FromVer,ToVer : string) of object;

  { TUpdate }

  TUpdate = class
  private
    ferror: boolean;
    foninfo: tinfoevent;
    fonprogress: tprogressevent;
    fonstatus: tstatusevent;
    FPath : string;
    FProtokoll : TUpdateProtokoll;
    FTargetPath : string;
    FIndex : TStringList;
    FProgress,
    FMax : Integer;
    procedure GetFile(FileName : string;Timeout : LongInt = 120000);
  public
    constructor Create(url,targetpath : string);
    procedure TransmitID;
    procedure Prepare;
    procedure PatchAll;
    procedure WaitForFiles;
    function UpdateAvalible : Boolean;
    procedure PatchFile(Filename : string;Revision : Integer = -1);
    function CheckFile(Filename : string;Revision : Integer = -1) : Integer;
    property OnStatus : TStatusEvent read FOnStatus write FOnStatus;
    property Onprogress : TProgressEvent read FOnProgress write FOnprogress;
    property OnInfo : TInfoEvent read FOninfo write FOninfo;
    property Error : Boolean read FError write fError;
  end;

  //Call this from the Target App
  function DoUpdate(Lang : string;Paths : TStrings;productname : string;mail : string;ShowMessagewhenAvalible,ShowMessagewehnNoNet,RestartApp : Boolean) : Boolean;

implementation

function DoUpdate(Lang : string;Paths : TStrings;productname : string;mail : string;ShowMessagewhenAvalible,ShowMessagewehnNoNet,RestartApp : Boolean) : boolean;
var
  Cmd : string;
  mDoUpdate : Boolean;
  i: integer;
  mUpdate : TUpdate;
  Batchf : TextFile;
  http: THTTP;
  Cmd1: String;
begin
  Result := False;
  ForceDirectories(AppendPathDelim(ProgramDirectory)+'update');
  mUpdate := TUpdate.Create('http://update.ullihome.de/update/',AppendPathDelim(ProgramDirectory)+'update');
  mUpdate.Prepare;
  if mUpdate.FIndex.Text = '' then
    begin
      if ShowMessagewehnNoNet then
        begin
          ShowMessage(strNoNetavalible);
        end;
      if pos('FILE://',UpperCase(Paths.Text)) = 0 then
        exit;
    end
  else
    begin
      try
        http := THTTP.Create;
        http.Timeout := 2500;
        {$IFDEF WINDOWS}
        http.url := 'http://update.ullihome.de/cgi-bin/check.pl?s='+IntToStr(round(Date))+'%3b'+IntToStr(CreateMachineID)+'%3b'+IntToStr(CreateuserID)+'%3b'+HTTPEncode(mail)+'%3b'+HTTPEncode(productname)+'%3bwindows%3b%0d';
        {$ELSE}
        http.url := 'http://update.ullihome.de/cgi-bin/check.pl?s='+IntToStr(round(Date))+'%3b'+IntToStr(CreateMachineID)+'%3b'+IntToStr(CreateuserID)+'%3b'+HTTPEncode(mail)+'%3b'+HTTPEncode(productname)+'%3bunix%3b%0d';
        {$ENDIF}
        http.Post;
        while http.Active do Application.Processmessages;
      finally
        http.Free;
      end;
    end;
  if (mUpdate.UpdateAvalible and (not mUpdate.Error)) then
    begin
      mUpdate.PatchAll;
    end
  else
    begin
    end;
  mUpdate.Free;
  if not FileExists(AppendPathDelim(ProgramDirectory)+'update'+DirectorySeparator+'update'+ExtractFileExt(ParamStr(0))) then
    begin
      exit;
    end;
  if RestartApp then
    Cmd := '"'+AppendPathDelim(ProgramDirectory)+'update'+DirectorySeparator+'update'+ExtractFileExt(ParamStr(0))+'" '+Lang+' "'+ParamStr(0)+'" "'+ProgramDirectory+'"'
  else
    Cmd := '"'+AppendPathDelim(ProgramDirectory)+'update'+DirectorySeparator+'update'+ExtractFileExt(ParamStr(0))+'" '+Lang+' "'+AppendPathDelim(ProgramDirectory)+'update'+DirectorySeparator+'bspatch'+ExtractFileExt(ParamStr(0))+'" "'+ProgramDirectory+'"';
  Cmd1 := AppendPathDelim(ProgramDirectory)+'update'+DirectorySeparator+'update'+ExtractFileExt(ParamStr(0))+' '+Lang+' "'+ParamStr(0)+'" "'+ProgramDirectory+'"';
  mDoUpdate := false;
  for i := 0 to Paths.Count-1 do
    begin
      mUpdate := TUpdate.Create(Paths.Strings[i],ProgramDirectory);
      mUpdate.Prepare;
      mDoUpdate := mDoUpdate or (mUpdate.UpdateAvalible and (not mUpdate.Error));
      mUpdate.Free;
      if mDoUpdate then
        begin
          Cmd := cmd + ' "' + Paths[i]+'"';
          Cmd1 := cmd1 + ' "' + Paths[i]+'"';
        end;
    end;
  AssignFile(BatchF,AppendPathDelim(ProgramDirectory)+'update'+Directoryseparator+'updateclean.bat');
  Rewrite(BatchF);
  writeln(BatchF,'@echo off');
  writeln(BatchF,'cd ..');
  writeln(BatchF,'del *.exe');
  writeln(BatchF,cmd1);
  CloseFile(BatchF);
  if mDoUpdate then
    begin
      if ShowMessagewhenAvalible then
        begin
          if MessageDlg(strUpdate,strAnNewUpdateisAvalible,mtInformation,[mbYes,mbNo],0) = mrYes then
            begin
              ExecVisualProcess(Cmd,AppendPathDelim(ProgramDirectory)+'update',False);
              Result := true;
            end;
        end
      else
        begin
          ExecVisualProcess(Cmd,AppendPathDelim(ProgramDirectory)+'update',False);
          Result := true;
        end;
    end;
end;

{ TUpdate }

procedure tupdate.getfile(filename: string;Timeout : LongInt = 120000);
var
  http : THTTP;
  
  function HTTPEncode(s : string) : string;
  begin
    Result := StringReplace(s,' ','%20',[rfReplaceAll]);
  end;
begin
  case FProtokoll of
  upFile:
    begin
      if (not FileExists(FTargetPath+filename)) or (Uppercase(Filename) = 'UPDATE2.IDX') then
        FileUtil.CopyFile(FPath+filename,FTargetPath+filename);
    end;
  upHTTP:
    begin
      if (not FileExists(FTargetPath+filename)) or (Uppercase(Filename) = 'UPDATE2.IDX') then
        begin
          try
            http := THTTP.Create;
            http.Timeout := Timeout;
            http.url := 'http://'+HTTPEncode(Fpath+'/'+filename);
            http.Get;
            while http.Active do Application.Processmessages;
            if Http.Data.Size > 0 then
              http.Data.SaveToFile(FTargetPath+filename);
          finally
            http.Free;
          end;
        end;
    end;
  end;
end;

constructor TUpdate.Create(url,TargetPath: string);
begin
  FError := False;
  FIndex := TStringList.Create;
  if copy(Uppercase(url),0,7)= 'HTTP://' then
    begin
      FProtokoll := upHTTP;
      FPath := copy(url,8,length(url));
    end;
  if copy(Uppercase(url),0,7)= 'FILE://' then
    begin
      FProtokoll := upFile;
      FPath := AppendPathDelim(copy(url,8,length(url)));
    end;
  FTargetPath := AppendPathDelim(TargetPath);
end;

procedure TUpdate.TransmitID;
begin

end;

procedure tupdate.prepare;
begin
  if Assigned(FOnStatus) then
    FOnStatus(ST_MESSAGE,strGettingUpdateFile);
  case Fprotokoll of
  upHTTP:
    begin
      try
        GetFile('update2.idx',2500);
        if FileExists(FTargetPath+'update2.idx') then
          FIndex.LoadFromFile(FTargetPath+'update2.idx');
//        DeleteFile(FTargetPath+'update2.idx');
      except
        FError := True;
        if Assigned(FOnStatus) then
          FOnStatus(ST_ERROR,strUpdateSourcenotthere);
      end;
    end;
  upFile:
    begin
      if FileExists(Fpath+'update2.idx') then
        FIndex.LoadFromFile(Fpath+'update2.idx')
      else
        begin
          FError := True;
          if Assigned(FOnStatus) then
            FOnStatus(ST_ERROR,strUpdateSourcenotthere);
        end;
    end;
  end;
end;

procedure TUpdate.PatchAll;
var
  i: integer;
  f : TextFile;
begin
  FProgress := 0;
  FMax := 0;
  Assignfile(f,'update.log');
  Rewrite(f);
  Closefile(f);
  for i := 0 to FIndex.Count-1 do
    if copy(FIndex[i],1,1) = '"' then
      inc(Fmax,CheckFile(copy(Findex[i],2,length(FIndex[i])-2)));
  if Assigned(FOnProgress) then
    FOnProgress(0,Fmax);
  for i := 0 to FIndex.Count-1 do
    if copy(FIndex[i],1,1) = '"' then
      if CheckFile(copy(Findex[i],2,length(FIndex[i])-2)) > 0 then
        PatchFile(copy(Findex[i],2,length(FIndex[i])-2));
end;

procedure tupdate.waitforfiles;
var
  i : Integer;
  fs : TFileStream;
  Exc : Boolean;
label NewStep;
begin
  if Assigned(FOnStatus) then
    FOnStatus(ST_MESSAGE,strWaitingforFileAccess);
  NewStep:
  sleep(10);
  for i := 0 to FIndex.Count-1 do
    begin
      if FileExists(FTargetPath+copy(Findex[i],2,length(FIndex[i])-2)) then
        begin
          Exc := False;
          try
            fs := TFileStream.Create(FTargetPath+copy(Findex[i],2,length(FIndex[i])-2),fmOpenWrite);
            fs.Free;
          except
            Exc := True;
          end;
          if Exc then
            goto NewStep;
          end;
    end;
end;

function tupdate.updateavalible: boolean;
var
  i: integer;
  Found : Boolean = False;
  lastcrc2:string = '';
  lastcrc: string = '';
  tmp: string;
begin
  Result := False;
  for i := 0 to FIndex.Count-1 do
    if copy(FIndex[i],1,1) = '"' then
      begin
        tmp := FIndex[i];
        if (i > 0) and (Lastcrc <> Lastcrc2) and Found then
          begin
            Result := True;
            break;
          end;
        Found := False;
        if FileExists(FTargetPath+copy(Findex[i],2,length(FIndex[i])-2)) or DirectoryExists(FTargetPath+copy(Findex[i],2,length(FIndex[i])-2)) then
          begin
            Lastcrc2 := MD5Print(MD5File(FTargetPath+copy(Findex[i],2,length(FIndex[i])-2)));
//            if MD5Print(MD5File('')) = LastCRC2 then Found := False;
          end
        else
          Found := True;
      end
    else
      begin
        tmp := copy(Findex[i],pos(';',FIndex[i])+1,length(FIndex[i]));
        if copy(tmp,0,pos(';',tmp)-1) = '' then
          begin
            Result := False;
            exit;
          end;
        lastcrc := copy(tmp,0,pos(';',tmp)-1);
        if lastcrc = Lastcrc2 then Found := True;
      end;
  if (i > 0) and (Lastcrc <> Lastcrc2) and Found then
    begin
      Result := True;
    end;
end;

function TUpdate.CheckFile(Filename: string; Revision: Integer) : Integer;
var
  actualrev: integer;
  targetrev:integer;
  i: integer;
  tmp: string;
begin
  Result := 0;
  ActualRev := -2;
  Targetrev := -1;
  ForceDirectories(FTargetpath+ExtractFileDir(FileName));
  if not FileExists(FTargetPath+Filename) then
    Actualrev := -1
  else
    begin
      i := FIndex.IndexOf('"'+Filename+'"')+1;
      if i = -1 then
        exit;
      while (i < FIndex.Count) and (FIndex[i][1] = '>') do
        begin
          tmp := copy(Findex[i],pos(';',FIndex[i])+1,length(FIndex[i]));
          if copy(tmp,0,pos(';',tmp)-1) = MD5Print(MD5File(FTargetPath+Filename)) then
            begin
              ActualRev := StrToInt(copy(Findex[i],2,pos(';',FIndex[i])-2));
              break;
            end;
          inc(i);
        end;
    end;
  if Revision = -1 then
    begin
      i := FIndex.IndexOf('"'+Filename+'"')+1;
      if i = 0 then
        exit;
      while (i < FIndex.Count) and (FIndex[i][1] = '>') do
        inc(i);
      if FIndex[i-1][1] <> '>' then
        exit;
      TargetRev := StrToInt(copy(Findex[i-1],2,pos(';',FIndex[i-1])-2));
    end
  else
    Targetrev := Revision;
  if (Actualrev > TargetRev) or (Actualrev = -2) or (TargetRev = -1) then
    begin
      if ActualRev = -2 then
        begin
          FOnStatus(ST_MESSAGE,Format(strFilenotinrepositore,[Filename]));
        end;
      exit;
    end;
  if ActualRev = TargetRev then
    exit;
{  PatchSize := 0;
  i := FIndex.IndexOf('"'+Filename+'"')+1;
  if i = -1 then
    exit;
  PrevSize := fsize;
  while (i < FIndex.Count) and (FIndex[i][1] = '>') do
    begin
      tmp := copy(Findex[i],pos(';',FIndex[i])+1,length(FIndex[i]));
      if StrToInt(copy(Findex[i],2,pos(';',FIndex[i])-2)) > ActualRev then
        begin
          inc(PatchSize,StrToInt(copy(tmp,0,pos(';',tmp)-1))-PrevSize);
          PrevSize := StrToInt(copy(tmp,0,pos(';',tmp)-1));
        end;
      inc(i);
    end;}
  Result := TargetRev-Actualrev;//PatchSize;
end;

procedure TUpdate.PatchFile(Filename: string; Revision: Integer);
var
  actualrev: integer;
  targetrev:integer;
  actualver : string;
  targetver : string;
  crc : string;
  i: integer;
  tmp: string;
  rev: longint;
  TargetCRC : string;
  f : TextFile;
  ExecDir : string;
  oldprogress : integer;
  PatchSize: Integer;
//  PrevSize: LongInt;
begin
  Assignfile(f,'update.log');
  if FileExists('update.log') then
    Append(f)
  else
    Rewrite(f);
  ActualRev := -2;
  Targetrev := -1;
  oldprogress := fprogress;
  writeln(f,'File:'+Filename+' Revision:'+IntToStr(revision));
  ForceDirectories(FTargetpath+ExtractFileDir(FileName));
  if not FileExists(FTargetPath+Filename) then
    Actualrev := -1
  else
    begin
      crc := MD5Print(MD5File(FTargetPath+Filename));
      i := FIndex.IndexOf('"'+Filename+'"')+1;
      if i = -1 then
        exit;
      while (i < FIndex.Count) and (FIndex[i][1] = '>') do
        begin
          tmp := copy(Findex[i],pos(';',FIndex[i])+1,length(FIndex[i]));
          if copy(tmp,0,pos(';',tmp)-1) = crc then
            begin
              ActualRev := StrToInt(copy(Findex[i],2,pos(';',FIndex[i])-2));
              tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
              ActualVer := tmp;
              break;
            end;
          inc(i);
        end;
    end;
  if Revision = -1 then
    begin
      i := FIndex.IndexOf('"'+Filename+'"')+1;
      if i = 0 then
        exit;
      while (i < FIndex.Count) and (FIndex[i][1] = '>') do
        inc(i);
      if FIndex[i-1][1] <> '>' then
        exit;
      TargetRev := StrToInt(copy(Findex[i-1],2,pos(';',FIndex[i-1])-2));
      tmp := copy(Findex[i-1],pos(';',FIndex[i-1])+1,length(FIndex[i-1]));
      TargetCRC := copy(tmp,0,pos(';',tmp)-1);
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      TargetVer := tmp;
    end
  else
    Targetrev := Revision;
  if (Actualrev > TargetRev) or (Actualrev = -2) or (TargetRev = -1) then
    begin
      if Assigned(FOnStatus) then
        begin
          if Assigned(FOnStatus) then
            FOnStatus(ST_MESSAGE,Format(strUpdatingFile,[Filename]));
          if (Actualrev > TargetRev) then
            begin
              writeln(f,strFileIsNewer);
              FOnStatus(ST_MESSAGE,strFileIsNewer);
            end;
          if (Actualrev = -2) then
            begin
              writeln(f,strActualRevisionnotFound);
              FOnStatus(ST_MESSAGE,strActualRevisionnotFound);
            end;
          if (TargetRev = -1) then
            begin
              writeln(f,strTargetRevisionnotFound);
              FOnStatus(ST_MESSAGE,strTargetRevisionnotFound);
            end;
        end;
      exit;
    end;
  if ActualRev = TargetRev then
    exit;
  PatchSize := CheckFile(Filename,TargetRev);
  writeln(f,Format(strUpdatingFile,[Filename]));
  if Assigned(FOnStatus) then
    FOnStatus(ST_MESSAGE,Format(strUpdatingFile,[Filename]));
  writeln(f,Format(strRevisions,[ActualRev,TargetRev]));
  if Assigned(FOnStatus) then
    FOnStatus(ST_MESSAGE,Format(strRevisions,[ActualRev,TargetRev]));
  if Assigned(FOnInfo) then
    FOnInfo(Filename,ActualRev,TargetRev,ActualVer,TargetVer);
  i := FIndex.IndexOf('"'+Filename+'"')+1;
//  PrevSize := fsize;
  for rev := ActualRev+1 to TargetRev do
    begin
      writeln(f,Format(strGettingRevision,[rev]));
      if Assigned(FOnStatus) then
        FOnStatus(ST_MESSAGE,Format(strGettingRevision,[rev]));
      GetFile(ExtractFilename(FileName)+'.'+IntToStr(rev)+'.up2');
      if not FileExists(FTargetPath+ExtractFileName(Filename)+'.'+IntToStr(rev)+'.up2') then
        begin
          if Assigned(FOnStatus) then
            FOnStatus(ST_MESSAGE,strErrorGettingFile);
          exit;
        end;
      writeln(f,strPatchingFile);
      if Assigned(FOnStatus) then
        FOnStatus(ST_MESSAGE,strPatchingFile);
      if rev = 0 then
        Fileutil.CopyFile(FTargetPath+ExtractFileName(Filename)+'.0.up2',FTargetPath+Filename)
      else
        begin
          ExecDir := AppendPathDelim(ProgramDirectory);
          if not (FileExists(ExecDir+'bspatch'+ExtractFileExt(ParamStr(0)))) then
            begin
              writeln(f,strPatchExedontexists+' "'+ExecDir+'bspatch'+ExtractFileExt(ParamStr(0))+'"');
              ExecDir := AppendPathDelim(ExecDir+'update');
              if not FileExists(ExecDir+'bspatch'+ExtractFileExt(ParamStr(0))) then
                begin
                  writeln(f,strPatchExedontexists+' "'+ExecDir+'bspatch'+ExtractFileExt(ParamStr(0))+'"');
                  ExecDir := '';
                  if Assigned(FOnStatus) then
                    FOnStatus(ST_ERROR,strPatchExedontexists+' bspatch'+ExtractFileExt(ParamStr(0)));
                end;
            end;
          if Execdir <> '' then
           begin
             writeln(f,ExecDir+'bspatch'+ExtractFileExt(ParamStr(0))+' "'+FTargetPath+Filename+'" "'+FTargetPath+Filename+'" "'+Ftargetpath+ExtractFileName(Filename)+'.'+IntToStr(rev)+'.up2"');
             ExecProcess('"'+ExecDir+'bspatch'+ExtractFileExt(ParamStr(0))+'" "'+FTargetPath+Filename+'" "'+FTargetPath+Filename+'" "'+Ftargetpath+ExtractFileName(Filename)+'.'+IntToStr(rev)+'.up2"',ExecDir,True);
           end;
        end;
      while (i < FIndex.Count) and (FIndex[i][1] = '>') do
        begin
          tmp := copy(Findex[i],pos(';',FIndex[i])+1,length(FIndex[i]));
          if StrToInt(copy(Findex[i],2,pos(';',FIndex[i])-2)) <> rev then
            inc(i)
          else
            break;
        end;
//      inc(Fprogress,StrToInt(copy(tmp,0,pos(';',tmp)-1))-PrevSize);
//      PrevSize := StrToInt(copy(tmp,0,pos(';',tmp)-1));
      if Assigned(FOnProgress) then
        FOnProgress(FProgress,FMax);
    end;
  FProgress := oldProgress+PatchSize;
  if Assigned(FOnProgress) then
    FOnProgress(FProgress,FMax);
  if TargetCRC <> MD5Print(MD5File(FTargetPath+Filename)) then
    begin
      writeln(f,strFailedPatchingFile);
//      writeln(f,'TargetSize:'+IntToStr(TargetSize)+' ActualSize:'+IntToStr(filesize(FTargetPath+Filename)));
      if Assigned(FOnStatus) then
        FOnStatus(ST_ERROR,strFailedPatchingFile);
      //TODO:check if this is realy so good idea :p
      SysUtils.DeleteFile(FTargetPath+Filename);
    end
  else
    begin
      for rev := ActualRev+1 to TargetRev do
        SysUtils.DeleteFile(FTargetPath+ExtractFileName(Filename)+'.'+IntToStr(rev)+'.up2');
      writeln(f,strDone);
    end;
  if Assigned(FOnStatus) then
    FOnStatus(ST_MESSAGE,strDone);
  CloseFile(f);
end;

end.

