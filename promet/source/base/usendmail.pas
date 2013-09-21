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
*******************************************************************************}
unit uSendMail;
{$mode delphi}{$H+}
interface
uses
  Classes, SysUtils,UTF8Process,process
  {$IFDEF WINDOWS}
  ,MAPI,windows,Forms
  {$ENDIF}
  ;

function DoSendMail(const Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: String) : Integer;

implementation
{$IFDEF WINDOWS}
function DoSendMail(const Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: String) : Integer;
var
  message: TMapiMessage;
  lpSender,
  lpRecepient: TMapiRecipDesc;
  FileAttach: TMapiFileDesc;
  SM: TFNMapiSendMail;
  MAPIModule: HModule;
begin
  FillChar(message, SizeOf(message), 0);
  with message do
  begin
    if (Subject<>'') then
    begin
      lpszSubject := PChar(Subject)
    end;
    if (Body<>'') then
    begin
      lpszNoteText := PChar(Body)
    end;
    if (SenderEMail<>'') then
    begin
      lpSender.ulRecipClass := MAPI_ORIG;
      if (SenderName='') then
      begin
        lpSender.lpszName := PChar(SenderEMail)
      end
      else
      begin
        lpSender.lpszName := PChar(SenderName)
      end;
      lpSender.lpszAddress := PChar('SMTP:'+SenderEMail);
      lpSender.ulReserved := 0;
      lpSender.ulEIDSize := 0;
      lpSender.lpEntryID := nil;
      lpOriginator := @lpSender;
    end;
    if (RecepientEMail<>'') then
    begin
      lpRecepient.ulRecipClass := MAPI_TO;
      if (RecepientName='') then
      begin
        lpRecepient.lpszName := PChar(RecepientEMail)
      end
      else
      begin
        lpRecepient.lpszName := PChar(RecepientName)
      end;
      lpRecepient.lpszAddress := PChar('SMTP:'+RecepientEMail);
      lpRecepient.ulReserved := 0;
      lpRecepient.ulEIDSize := 0;
      lpRecepient.lpEntryID := nil;
      nRecipCount := 1;
      lpRecips := @lpRecepient;
    end
    else
    begin
      lpRecips := nil
    end;
    if (FileName='') then
    begin
      nFileCount := 0;
      lpFiles := nil;
    end
    else
    begin
      FillChar(FileAttach, SizeOf(FileAttach), 0);
      FileAttach.nPosition := Cardinal($FFFFFFFF);
      FileAttach.lpszPathName := PChar(FileName);
      nFileCount := 1;
      lpFiles := @FileAttach;
    end;
  end;
  MAPIModule := LoadLibrary(PChar(MAPIDLL));
  if MAPIModule=0 then
  begin
    Result := -1
  end
  else
  begin
    try
      @SM := GetProcAddress(MAPIModule, 'MAPISendMail');
      if @SM<>nil then
      begin
        Result := SM(0, Application.MainForm.Handle, message, MAPI_DIALOG or
                     MAPI_LOGON_UI, 0);
      end
      else
      begin
        Result := 1
      end;

    finally
      FreeLibrary(MAPIModule);
    end;
  end;
  if Result<>0 then
  begin
  end;
end;
{$ELSE}
function DoSendMail(const Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: String) : Integer;
var
  aProc: TProcessUTF8;
  NoTB: Boolean = False;
begin
  aProc := TProcessUTF8.Create(nil);
  aProc.CommandLine:='thunderbird --version';
  try
    aProc.Execute;
  except
    NoTB := True;
  end;
  if not NoTB then
    begin
      aProc.CommandLine:='thunderbird -compose "subject='+Subject+',body='+Body+',attachment=file://'+FileName+'"';
      aProc.Execute;
    end
  else
    begin
      aProc.CommandLine:='xdg-open "mailto:?subject='+Subject+' &body='+Body+' &attach=file://'+FileName+'"';
      aProc.Execute;
    end;
  aProc.Free;
end;
{$ENDIF}

end.

