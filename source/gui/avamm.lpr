{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 25.03.2020
*******************************************************************************}
program avamm;

uses {$IFDEF UNIX}cthreads,{$ENDIF} pscript2, perp2, pwiki2, udata, Interfaces,
  Forms, LazFileUtils, SysUtils, Dialogs, lclgui, uPassword, Controls, umain;

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
    GlobalUser := nil;
  end;
{
  if not Assigned(GlobalUser) then //Login
    begin
      if fPassword.Execute = mrYes then
        begin

        end
      else exit;
    end;
}
  Application.CreateForm(TfMain,fMain);
  Application.Run;
end.

