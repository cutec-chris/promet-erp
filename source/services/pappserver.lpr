program pappserver;
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, general_nogui, memds, LazFileUtils,
  ubasedbclasses, uPrometORM, httproute, fphttpapp, fpwebfile, HTTPDefs,
  uapiv2handling;

var
  aUsers: TMemDataset;
  aUser : TUser;
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
  aUsers := Data.Select(TUser,'NAME=Jemand','NAME,SQL_ID');
  aUsers.First;
  while not aUsers.EOF do
    begin
      writeln(aUsers.FieldByName('NAME').AsString);
      aUsers.Next;
    end;
  aUsers.Free;
  aUsers := Data.Select(TUser,'NAME=Gast','NAME,SQL_ID');
  aUsers.First;
  while not aUsers.EOF do
    begin
      writeln(aUsers.FieldByName('NAME').AsString);
      aUsers.Next;
    end;
  aUser := TUser.Create;
  Data.Load(aUser,aUsers.FieldByName('SQL_ID').AsLargeInt);
  aUsers.Free;
  aUser.Free;
  if DirectoryExistsUTF8('web') then
    RegisterFileLocation('*','web');
  Application.Run;
end.

