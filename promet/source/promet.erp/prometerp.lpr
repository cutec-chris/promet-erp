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
Created 01.06.2006
*******************************************************************************}
program prometerp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, lazreport, turbopoweripro, uMain,FileUtil,
  Process
  { you can add units after this }
  ,uBaseVisualApplication, pphones, zvdatetimectrls, general, Utils;

{$R *.res}

var
  PlainName: String;
  FindRec: TSearchRec;
  NewName: TFilename;
  aProcess: TProcess;
  i: Integer;
begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  {$ifdef WINDOWS}
  PlainName := copy(Application.Exename,0,length(Application.Exename)-length(ExtractFileExt(Application.Exename)));
  IF FindFirst(UniToSys(PlainName+'*'+ExtractFileExt(Application.Exename)), faDirectory, FindRec) = 0 THEN
    REPEAT
      IF ((FindRec.Name <> '.') AND (FindRec.Name <> '..')) THEN
        begin
          if FindRec.Name <> ExtractFileName(Application.ExeName) then
            NewName := FindRec.Name;
        end;
    UNTIL FindNext(FindRec) <> 0;
  FindClose(FindRec);
  if (NewName <> ExtractFileName(Application.ExeName)) and (NewName<>'') and (pos('_',NewName)=0) then
    begin
      aProcess := Tprocess.Create(nil);
      aProcess.Options:=[];
      aProcess.CommandLine:=AppendPathDelim(Application.Location)+NewName;
      for i :=  1 to Paramcount do
        aProcess.CommandLine := aProcess.CommandLine+' '+ParamStr(i);
      aProcess.Execute;
      aProcess.Free;
      Application.Terminate;
      exit;
    end;
  {$endif}
  Application.Title:='Promet-ERP';
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  if fMain.DoCreate then
    Application.Run;
end.
