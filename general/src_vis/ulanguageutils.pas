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
Created 12.06.2014
*******************************************************************************}
unit uLanguageUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PoTranslator,Translations,FileUtil,Forms,LCLProc,TypInfo,
  LCLIntf,LResources, DbCtrls, LCLStrConsts,uGeneralStrConsts;

procedure LoadLanguage(lang : string);
procedure TranslateNavigator(nav : TDBCustomNavigator);

implementation

procedure LoadLanguage(lang: string);
var
  Info : TSearchRec;
  po: TPOFile;
  units: TStringList;
  id: String;
  i: Integer;
  a: Integer;
  Comp: TComponent;
  aFilename: String;
Begin
  If FindFirstUTF8(ProgramDirectory+'languages'+Directoryseparator+'*.'+lowercase(copy(lang,0,2))+'.po',faAnyFile,Info)=0 then
    repeat
      po := TPOFile.Create(ProgramDirectory+'languages'+Directoryseparator+Info.Name);
      units := TStringList.Create;
      for i := 0 to po.Items.Count-1 do
        begin
          {$IF FPC_FULLVERSION>=20601}
          id := copy(TPoFileItem(po.Items[i]).IdentifierLow,0,pos('.',TPoFileItem(po.Items[i]).IdentifierLow)-1);
          {$ELSE}
          id := lowercase(copy(TPoFileItem(po.Items[i]).Identifier,0,pos('.',TPoFileItem(po.Items[i]).Identifier)-1));
          {$ENDIF}
          if units.IndexOf(id) = -1 then
            units.Add(id);
        end;
      for i := 0 to units.Count-1 do
        Translations.TranslateUnitResourceStrings(units[i],ProgramDirectory+'languages'+Directoryseparator+Info.Name);
      units.Free;
      for i := 0 to po.Items.Count-1 do
        begin
          {$IF FPC_FULLVERSION>=20601}
          id := copy(TPoFileItem(po.Items[i]).IdentifierLow,0,pos('.',TPoFileItem(po.Items[i]).IdentifierLow)-1);
          {$ELSE}
          id := lowercase(copy(TPoFileItem(po.Items[i]).Identifier,0,pos('.',TPoFileItem(po.Items[i]).Identifier)-1));
          {$ENDIF}
          for a := 0 to Screen.FormCount-1 do
            if UTF8UpperCase(Screen.Forms[a].ClassName) = UTF8UpperCase(id) then
              begin
                {$IF FPC_FULLVERSION>=20601}
                id := copy(TPoFileItem(po.Items[i]).IdentifierLow,0,pos('.',TPoFileItem(po.Items[i]).IdentifierLow)-1);
                {$ELSE}
                id := lowercase(copy(TPoFileItem(po.Items[i]).Identifier,0,pos('.',TPoFileItem(po.Items[i]).Identifier)-1));
                {$ENDIF}
                if Assigned(Screen.Forms[a].FindComponent(copy(id,0,pos('.',id)-1))) then
                  begin
                    try
                      Comp := Screen.Forms[a].FindComponent(copy(id,0,pos('.',id)-1));
                      id := copy(id,pos('.',id)+1,length(id));
                      SetStrProp(Comp,id,TPoFileItem(po.Items[i]).Translation);
                    except
                    end;
                  end;
              end;
        end;
      po.Free;
    until FindNextUTF8(info)<>0;
  FindCloseUTF8(Info);
  aFilename := ProgramDirectory+'languages'+Directoryseparator+'forms.'+lowercase(copy(lang,0,2))+'.po';
  if FileExists(aFilename) then
    LRSTranslator:=TPoTranslator.Create(aFilename);
end;
procedure TranslateNavigator(nav : TDBCustomNavigator);
begin
  nav.Hints.Clear;
  nav.Hints.Add(rsFirstRecordHint+' '+strRecord);
  nav.Hints.Add(rsPriorRecordHint+' '+strRecord);
  nav.Hints.Add(rsNextRecordHint+' '+strRecord);
  nav.Hints.Add(rsLastRecordHint+' '+strRecord);
  nav.Hints.Add(strSNew);
  nav.Hints.Add(strDelete);
  nav.Hints.Add(strEdit);
  nav.Hints.Add(strSave);
  nav.Hints.Add(strCancelEdit);
  nav.Hints.Add(strRefresh);
end;


end.

