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
unit uprometmodules;

{
Erstell mir eine Aufgabe Fertigungsauftrag jetzt da wenn ein Fertigungsauftrag für Artikel 1000 angelegt wird

Wann ist mein nächster Termin?
Was steht Dienstag in meinem Kalender?
Wo ist meine nächste Besprechung?
Wann ist die Besprechung mit Franzi?
Erstelle einen Termin für morgen um 14 Uhr.
Plane eine Besprechung heute um 11 Uhr im Tagungsraum.
Verschiebe meinen heutigen Termin von 9 Uhr auf 11 Uhr.
Füge Marcell zu meinem Termin heute um 11 Uhr hinzu.

Sende eine Mail an Peer wegen Urlaub.
Schreibe eine Mail an Nik mit dem Inhalt: Kommst Du heute wieder zu spät?
Zeige neue Mails von Nik.
Zeige Mails von gestern zum Urlaub

Wo ist Aileen?
Wo ist mein Chef?
Wer ist in der Nähe?
Ist meine Oma zu Hause?
Benachrichtige mich, wenn Oma zuhause ankommt.
Benachrichtige Oma, wenn ich das Büro verlasse.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uSpeaker;

implementation

function HandleTalk(Speaker : TSpeaker;language : string;var sentence : string;var canhandle : Boolean) : Boolean;
begin
  Result:=False;
  canhandle:=(pos('$weather(',sentence)>0);
  if pos('$getdescription(de)',sentence)>0 then
    begin
      sentence:='Nach Kalendereinträgen oder Mail fragen.';
      result := true;
      canhandle:=true;
      exit;
    end;
  if not canhandle then exit;

end;

procedure AddSentences;
begin
end;

initialization
  RegisterToSpeaker(@HandleTalk,@AddSentences);

end.

