unit UtilsVis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,UTF8Process,Graphics,Utils
  {$IFDEF MSWINDOWS}
  ,Registry,Windows
  {$ENDIF}
  ;

type
  TProcessinfoTyp = (piOpen,piPrint);

function GetProcessforExtension(InfoTyp : TProcessinfoTyp;Extension : string) : string;
function TextCut(aCanvas: TCanvas; Len: Integer; Text: String): String;

implementation

function GetProcessforExtension(InfoTyp : TProcessinfoTyp;Extension : string) : string;
var
{$ifdef MSWINDOWS}
  reg : TRegistry;
  ot : string;
  FileClass: string;
  chrResult: array[0..1023] of Char;
  wrdReturn: DWORD;
{$else}
  SRec : TSearchRec;
  res : Integer;
  f : TextFile;
  tmp : string;
  mime : string;
  apps : string;
{$endif}
begin
{$ifdef WINDOWS}
  case InfoTyp of
  piOpen:ot := 'open';
  piPrint:ot := 'print';
  end;
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_CLASSES_ROOT;
  FileClass := '';
  if Reg.OpenKeyReadOnly(ExtractFileExt('.'+Extension)) then
  begin
    FileClass := Reg.ReadString('');
    Reg.CloseKey;
  end;
  if FileClass <> '' then begin
    if Reg.OpenKeyReadOnly(FileClass + '\Shell\'+ot+'\Command') then
    begin
      wrdReturn := ExpandEnvironmentStrings(PChar(StringReplace(Reg.ReadString(''),'%1','%s',[rfReplaceAll])), chrResult, 1024);
      if wrdReturn = 0 then
        Result := StringReplace(Reg.ReadString(''),'%1','%s',[rfReplaceAll])
      else
        Result := Trim(chrResult);
      Reg.CloseKey;
    end;
  end;
  Reg.Free;
  if (Result = '') and (lowercase(Extension)='lnk') then  result := 'explorer';
{$ELSE}
  apps := '';
  mime := GetMimeTypeforExtension(Extension);
//  /usr/share/mime-info *.keys
  Res := FindFirst ('/usr/share/mime-info/*.keys', faAnyFile, SRec);
  while Res = 0 do
    begin
      AssignFile(f,'/usr/share/mime-info/'+SRec.Name);
      Reset(f);
      while not eof(f) do
        begin
          readln(f,tmp);
// nicht eingerueckt ist der mime typ
          if not ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) then
//eingerÃckt die eigenschaften
            if ((copy(tmp,length(tmp)-2,1) = '*')
            and (copy(tmp,0,length(tmp)-2) = copy(mime,0,length(tmp)-2)))
            or (trim(tmp) = trim(mime)) then
              begin
                readln(f,tmp);
                while (not eof(f)) and ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) do
                  begin
                    tmp := StringReplace(trim(tmp),#9,'',[rfReplaceAll]);
//open referenziert gleich das program
                    if lowercase(copy(tmp,0,5)) = 'open=' then
                      begin
                        Result := copy(tmp,6,length(tmp));
                        if pos('%f',Result) = 0 then
                          Result := Result+' "%s"'
                        else
                          Stringreplace(Result,'%f','%s',[rfReplaceAll]);
                        SysUtils.FindClose(SRec);
                        exit;
                      end
//das referenziert ein kÃrzel das isn der application registry steht
                    else if lowercase(copy(tmp,0,49)) = 'short_list_application_ids_for_novice_user_level=' then
                      begin
                        apps := copy(tmp,50,length(tmp));
                        break;
                      end;
                    readln(f,tmp);
                  end;
              end;
          if apps <> '' then break;
        end;
      CloseFile(f);
      Res := FindNext(SRec);
      if apps <> '' then break;
    end;
  SysUtils.FindClose(SRec);
  Result := apps;
  if apps <> '' then
    begin
      while pos(',',apps) > 0 do
        begin
          Res := FindFirst ('/usr/share/application-registry/*.applications', faAnyFile, SRec);
          while Res = 0 do
            begin
              AssignFile(f,'/usr/share/application-registry/'+SRec.Name);
              Reset(f);
              while not eof(f) do
                begin
                  readln(f,tmp);
                  if not ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) then
    //eingerÃckt die eigenschaften
                    if trim(tmp) = copy(apps,0,pos(',',apps)-1) then
                      begin
                        readln(f,tmp);
                        while (not eof(f)) and ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) do
                          begin
                            tmp := StringReplace(trim(tmp),#9,'',[rfReplaceAll]);
                            if lowercase(copy(tmp,0,8)) = 'command=' then
                              begin
                                Result := copy(tmp,9,length(tmp));
                                if FindFilenameOfCmd(Result) <> '' then
                                  begin
                                    if pos('%f',Result) = 0 then
                                      Result := Result+' "%s"'
                                    else
                                      Stringreplace(Result,'%f','%s',[rfReplaceAll]);

                                    CloseFile(f);
                                    exit;
                                  end;
                              end;
                            readln(f,tmp);
                          end;
                      end;
                end;
              CloseFile(f);
              Res := FindNext(SRec);
            end;
          apps := copy(apps,pos(',',apps)+1,length(apps));
        end;
    end;
  if Result='' then
    Result:=FindFilenameOfCmd('xdg-open')+' "%s"'; // Portland OSDL/FreeDesktop standard on Linux
  if Result='' then
    Result:=FindFilenameOfCmd('kfmclient')+' "%s"'; // KDE command
  if Result='' then
    Result:=FindFilenameOfCmd('gnome-open')+' "%s"'; // GNOME command
{$endif}
end;

function TextCut(aCanvas: TCanvas; Len: Integer; Text: String): String;
var
  k: Integer;
begin
  Result := '';
  if Len < 0 then exit;
  if Len <= aCanvas.TextWidth(Copy(Text, 1, 1) + '...') then exit;
  Result := Text;
  with aCanvas do
    begin
      if TextWidth(Text) > Len then
        begin
          for k := Length(Text) downto 1 do
            if TextWidth(Copy(Text, 1, k) + '...') > Len then Continue
              else
            begin
              Result := Copy(Text, 1, k) + '...';
              Exit;
            end;
        end;
    end;
end;

end.

