program linksender;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Classes, SysUtils, pprometdbintfs, CustApp,uprometipc,uProcessManager,usimpleprocess,FileUtil
  { you can add units after this };

type

  { TOpenLink }

  TOpenLink = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
  end;

{ TOpenLink }

procedure TOpenLink.DoRun;
var
  ErrorMsg: String;
  sl: TStringList;
  aLink: String;
begin
  if ParamCount>0 then
    begin
      if FileExists(ParamStr(1)) and (lowercase(ExtractFileExt(ParamStr(1))) = '.plink') then
        begin
          if not ProcessExists('prometerp'+ExtractFileExt(ExeName),'') then
            begin
              ExecProcess(AppendPathDelim(ExpandFileName(AppendPathdelim(Location) + '..'))+'prometerp'+ExtractFileExt(ExeName));
            end;
          sl := TStringList.Create;
          sl.LoadFromFile(ParamStr(1));
          SendIPCMessage('OpenLink('+sl[0]+')');
          sl.Free;
        end
      else if pos('@',ParamStr(1))>0 then
        begin
          if not ProcessExists('prometerp'+ExtractFileExt(ExeName),'') then
            begin
              ExecProcess(AppendPathDelim(ExpandFileName(AppendPathdelim(Location) + '..'))+'prometerp'+ExtractFileExt(ExeName));
            end;
          aLink := ParamStr(1);
          if pos(':',aLink)>0 then
            aLink := copy(aLink,pos(':',aLink)+1,length(aLink));
          SendIPCMessage('OpenLink('+aLink+')');
        end;
    end;
  // stop program loop
  Terminate;
end;

var
  Application: TOpenLink;

{$R *.res}

begin
  Application:=TOpenLink.Create(nil);
  Application.Title:='LinkSender';
  Application.Run;
  Application.Free;
end.

