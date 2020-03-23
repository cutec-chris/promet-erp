program avamm;

uses {$IFDEF UNIX}cthreads,{$ENDIF} pscript2, perp2, pwiki2, udata, Interfaces,
  Forms, LazFileUtils, SysUtils, Dialogs, lclgui;

begin
  Application.Initialize;
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
        showmessage(e.Message);
        exit;
      end;
  end;
  if not Assigned(GlobalUser) then //Login
    begin

    end;
  Application.Run;
end.

