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

info@cu-tec.de
*******************************************************************************}
program local_appbase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring,cthreads,{$ENDIF}
  ubasehttpapplication, pfcgiprometapp,
  uBaseApplication,umain, uwebserver;
begin
  Application.DefaultModule:='main';
  with BaseApplication as IBaseApplication do
    begin
      with Application as IBaseApplication do
        begin
          AppVersion:={$I ../base/version.inc};
          AppRevision:={$I ../base/revision.inc};
        end;
      SetConfigName('appconfig');
      RestoreConfig;
      Login;
    end;
  Application.Initialize;
  Application.CreateForm(TfWebServer,fWebServer);
  Application.Port:=8086;
  Application.DefaultModule:='appbase';
  Application.Run;
  Application.DoExit;
end.

