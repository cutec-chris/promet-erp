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
unit uinformation;

{$mode objfpc}

interface

uses
  Classes, SysUtils, uSpeaker;

implementation

function HandleTalk(Speaker : TSpeaker;language : string;var sentence : string;var canhandle : Boolean) : Boolean;
begin
  Result:=False;
  canhandle:=(pos('$checksite(',sentence)>0);
  if pos('$getdescription(de)',sentence)>0 then
    begin
      sentence:='Webseiten oder Feeds beobachten.';
      result := true;
      canhandle:=true;
      exit;
    end;
  if not canhandle then exit;

end;

procedure Chron;
begin

end;

procedure AddSentences;
begin
end;

initialization
  RegisterToSpeaker(@HandleTalk,@AddSentences);
  RegisterChron(@Chron);

end.

