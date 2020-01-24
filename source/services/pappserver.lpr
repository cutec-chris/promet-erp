program pappserver;
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, general_nogui, LazFileUtils, ubasedbclasses,
  uPrometORM, httproute, fphttpapp, fpwebfile, HTTPDefs, uapiv2handling;

begin
  Application.Port := 8085;
  Application.Threaded := true;
  Application.Initialize;
  {
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
  }
  Data.Select(TUser,'NAME=Jemand','NAME');

  if DirectoryExistsUTF8('web') then
    RegisterFileLocation('*','web');
  Application.Run;
end.

