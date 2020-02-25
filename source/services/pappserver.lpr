program pappserver;
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, general_nogui, memds, LazFileUtils,
  ubasedbclasses, uPrometORM, httproute, fphttpapp, fpwebfile, HTTPDefs,
  uapiv2handling, pprometdbintfs2, uData;

begin
  Application.Port := 8085;
  Application.Threaded := true;
  Application.Initialize;
  write('connecting...');
  Data.ConfigPath := Application.GetOptionValue('config-path');
  if Data.ConfigPath = '' then Data.ConfigPath:=AppendPathDelim(GetAppConfigDir(True))+'prometerp';
  Data.ConfigPath := AppendPathDelim(Data.ConfigPath);
  Data.Mandant := Application.GetOptionValue('mandant');
  if Data.Mandant = '' then Data.Mandant := 'Standard';
  try
    Data.Connect;
  except
    on e : exception do
      begin
        writeln(e.Message);
        exit;
      end;
  end;
  writeln('done.');
  if Application.HasOption('u','user') then
    begin
      GlobalUser := TUser.Create(nil);
      if not Data.Load(GlobalUser,'NAME='+Application.GetOptionValue('u','user')) then
        begin
          writeln('User not found...');
          FreeAndNil(GlobalUser);
        end;
      if not GlobalUser.CheckUserPasswort(Application.GetOptionValue('p','password')) then
        begin
          writeln('Password incorrect...');
          FreeAndNil(GlobalUser);
        end;
    end;
  if DirectoryExistsUTF8('web') then
    RegisterFileLocation('*','web');
  Application.Run;
end.

