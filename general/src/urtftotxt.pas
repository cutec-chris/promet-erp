unit uRTFtoTXT;
{$mode DELPHI}
interface
uses sysutils,math,Utils;
function RTF2Plain (const aSource: string): string;
implementation
function HexToInt(HexNum: string): LongInt;
begin
  Result:=StrToInt('$' + HexNum);
end;
{Convert RTF enabled text to plain.}
function RTF2Plain (const aSource: string): string;
var
   Source: string;
   NChar: Integer;
function ProcessGroupRecursevly: string;
  procedure SkipStar;
  var
     BracesOpened: Integer;
     Escaped: Boolean;
  begin
       BracesOpened:=1;
       Escaped:=false;
       while BracesOpened>0
             do begin
                Inc (NChar);
                case Source [NChar] of
                     '{': if Escaped
                             then Escaped:=false
                             else Inc (BracesOpened);
                     '}': if Escaped
                             then Escaped:=false
                             else Dec (BracesOpened);
                     '\': Escaped:=not Escaped;
                     else Escaped:=false;
                end;
             end;
  end;
  function UnicodeCharCode2ANSIChar (aCode: LongInt): Char;
  type
      TUnicode2ANSITable=array [$0410..$044f] of Char;
  const
       Unicode2ANSITable: TUnicode2AnsiTable=('À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
                                               'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');
  begin
       if (Low (Unicode2ANSITable)<=aCode) and (aCode<=High (Unicode2ANSITable))
          then UnicodeCharCode2ANSIChar:=Unicode2ANSITable [aCode]
          else UnicodeCharCode2ANSIChar:='?';
  end;
var
   Control, NumericValue, TextValue: string;
   tmp: String;
begin
     Result:='';
     Inc (NChar);
     while NChar<=Length (Source)
           do case Source [NChar] of
                   '{': Result:=Result+ProcessGroupRecursevly;
                   '}': begin
                             Inc (NChar);
                             Break;
                        end;
                   '\': begin
                             Inc (NChar);
                             case Source [NChar] of
                                  '''': begin
                                             Result:=Result+Chr (HexToInt (Copy (Source, NChar+1, 2)));
                                             Inc (NChar, 3);
                                        end;
                                  '~': Result:=Result+#$20;
                                  '*': SkipStar;
                                  'a'..'z': begin
                                                 Control:='';
                                                 while Source [NChar] in ['a'..'z']
                                                       do begin
                                                          Control:=Control+Source [NChar];
                                                          Inc (NChar);
                                                       end;
                                                 if Source [NChar]='-'
                                                    then begin
                                                         NumericValue:=Source [NChar];
                                                         Inc (NChar);
                                                    end
                                                    else NumericValue:='';
                                                  while Source [NChar] in ['0'..'9']
                                                        do begin
                                                           NumericValue:=NumericValue+Source [NChar];
                                                           Inc (NChar);
                                                        end;
                                                  if Source [NChar]='{' then
                                                    tmp := ProcessGroupRecursevly;
                                                  TextValue:='';
                                                  if not (Source [NChar] in ['a'..'z', '{', '}', '\'])
                                                     then begin
                                                          Inc (NChar);
                                                          while not (Source [NChar] in ['{', '}', '\'])
                                                                do begin
                                                                   TextValue:=TextValue+Source [NChar];
                                                                   Inc (NChar);
                                                                end;
                                                     end;
                                                  if (Control='line') or (Control='par')
                                                     then Result:=Result+#$0D#$0A
                                                     else if Control='tab'
                                                             then Result:=Result+#$09
                                                             else if Control='u'
                                                                     then Result:=Result+SysToUni(UnicodeCharCode2ANSIChar (StrToInt (NumericValue)))
                                                                     else if Control='colortbl'
                                                                             then TextValue:='';
                                                 if Length (TextValue)>0
                                                    then if (not ((TextValue [Length (TextValue)]=';') and (Source [NChar]='}')))
                                                            then begin
                                                                 Result:=Result+TextValue;
                                                                 TextValue:='';
                                                            end;
                                            end;
                                  else begin
                                       Result:=Result+Source [NChar];
                                       Inc (NChar);
                                  end;
                             end;
                   end;
                   else begin
                        Result:=Result+Source [NChar];
                        Inc (NChar);
                   end;
           end;
end;
function InitSource: Boolean;
var
   BracesCount: Integer;
   Escaped: Boolean;
begin
     if Copy (aSource, 1, 5)<>'{\rtf'
        then InitSource:=false
        else begin
             Source:='';
             BracesCount:=0;
             Escaped:=false;
             NChar:=1;
             while (NChar<=Length (aSource)) and (BracesCount>=0)
                   do begin
                      if not (aSource [NChar] in [#$0D, #$0A])
                         then begin
                              Source:=Source+aSource [NChar];
                              case aSource [NChar] of
                                   '{': if not Escaped
                                           then Inc (BracesCount)
                                           else Escaped:=false;
                                   '}': if not Escaped
                                           then Dec (BracesCount)
                                           else Escaped:=false;
                                   '\': Escaped:=true;
                                   else Escaped:=false;
                              end;
                         end;
                      Inc (NChar);
                   end;
             InitSource:=BracesCount=0;
        end;
end;
begin
  if InitSource then
    begin
      NChar:=1;
      Result:=ProcessGroupRecursevly;
    end
  else Result:=aSource;
end;
end.
