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
  Classes, SysUtils,UTF8Process,process,Dialogs,Utils
  {$IFDEF WINDOWS}
  ,MAPI,windows,Forms
  {$ENDIF}
  ;

function DoSendMail(Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: String) : Integer;

implementation
{$IFDEF WINDOWS}
function DoSendMail(Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: String) : Integer;
var
  message: TMapiMessage;
  lpSender : TMapiRecipDesc;
  MailName : array[0..30] of string;
  MailAddr : array[0..30] of string;
  lpRecepient : array[0..30] of TMapiRecipDesc;
  FileAttach: TMapiFileDesc;
  SM: TFNMapiSendMail;
  MAPIModule: HModule;
  tmpMail: String;
begin
  FillChar(message, SizeOf(message), 0);
  FillChar(lpRecepient, SizeOf(lpRecepient), 0);
  with message do
  begin
    if (Subject<>'') then
    begin
      Subject:=UniToSys(Subject);
      lpszSubject := PChar(Subject)
    end;
    if (Body<>'') then
    begin
      lpszNoteText := PChar(UniToSys(Body))
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
    tmpMail := RecepientEMail;
    if pos('@',tmpMail)>0 then
      tmpMail:=tmpMail+',';
    lpRecips := nil;
    nRecipCount:=0;
    while pos(',',tmpMail)>0 do
      begin
        lpRecepient[nRecipCount].ulRecipClass := MAPI_TO;
        MailName[nRecipCount] := copy(tmpMail,0,pos(',',tmpMail)-1);
        if (RecepientName='') then
        begin
          lpRecepient[nRecipCount].lpszName := PChar(UniToSys(MailName[nRecipCount]))
        end
        else
        begin
          lpRecepient[nRecipCount].lpszName := PChar(RecepientName)
        end;
        MailAddr[nRecipCount] := 'SMTP:'+copy(tmpMail,0,pos(',',tmpMail)-1);
        lpRecepient[nRecipCount].lpszAddress := PChar(MailAddr[nRecipCount]);
        lpRecepient[nRecipCount].ulReserved := 0;
        lpRecepient[nRecipCount].ulEIDSize := 0;
        lpRecepient[nRecipCount].lpEntryID := nil;
        nRecipCount := nRecipCount+1;
        lpRecips := @lpRecepient;
        tmpMail := copy(tmpMail,pos(',',tmpMail)+1,length(tmpMail));
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
      FileName := UniToSys(FileName);
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
      except
        on e : Exception do
          begin
            Result := 2;
            Showmessage(e.Message);
          end;
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
function DoSendMail(Subject, Body, FileName, SenderName, SenderEMail,
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

