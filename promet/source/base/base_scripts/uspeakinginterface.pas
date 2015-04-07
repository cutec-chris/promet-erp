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
Created 30.10.2014
*******************************************************************************}
unit uspeakinginterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uprometpascalscript,uBaseDbDataSet,variants,uBaseDbClasses;

type

  { TSpeakingInterface }

  TSpeakingInterface = class(TPrometPascalScript)
  public
    function CheckSentence(aSentence : string) : Boolean;
  end;

implementation
uses uData,genpascalscript;
{ TSpeakingInterface }

function TSpeakingInterface.CheckSentence(aSentence: string): Boolean;
begin
  Result:=False;
  FilterEx(Data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue('CmdLn.*')),0,'TIMESTAMPD','DESC');
  First;
  while not EOF do
    begin
      if (Script is TPascalScript) and TPascalScript(Script).Compile then
        with Script as TPascalScript do
          begin
            try
            if Assigned(Runtime) then
              if Runtime.RunProcPN([aSentence],'CHECKSENTENCE') = True then
                begin
                  Result := True;
                  exit;
                end;

            except
              Result := false;
            end;
          end;
      Next;
    end;
end;

end.

