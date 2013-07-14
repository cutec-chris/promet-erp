{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Functions to handle different byte orders                                                       }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclBitArithmetic.pas.                                                     }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2004-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclIniFiles;

interface
uses
  SysUtils, IniFiles;

type
  TYclISOMemIniFile = class(TMemIniFile)
  public
    function ReadDate(const Section, Name: String; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: String; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: String; Default: Double): Double; override;
    function ReadTime(const Section, Name: String; Default: TDateTime): TDateTime; override;
    function ReadHex(const Section, Name: String; Default: Cardinal): Cardinal;
    procedure WriteDate(const Section, Name: String; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: String; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: String; Value: Double); override;
    procedure WriteTime(const Section, Name: String; Value: TDateTime); override;
    procedure WriteHex(const Section, Name: String; Value: Cardinal);
  end;

  TYclISOIniFile = class(TIniFile)
  public
    function ReadDate(const Section, Name: String; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: String; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: String; Default: Double): Double; override;
    function ReadTime(const Section, Name: String; Default: TDateTime): TDateTime; override;
    function ReadHex(const Section, Name: String; Default: Cardinal): Cardinal; 
    procedure WriteDate(const Section, Name: String; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: String; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: String; Value: Double); override;
    procedure WriteTime(const Section, Name: String; Value: TDateTime); override;
    procedure WriteHex(const Section, Name: String; Value: Cardinal);
  end;

implementation
uses
  YclTextDatas;
    
function TYclISOMemIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOMemIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateTimeDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOMemIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  Result := ISOStrToFloatDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOMemIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToTimeDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOMemIniFile.ReadHex(const Section, Name: String; Default: Cardinal): Cardinal;
begin
  Result := StrToIntDef('$' + ReadString(Section, Name, ''), Default);
end;

procedure TYclISOMemIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateToStr(Value));
end;

procedure TYclISOMemIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateTimeToStr(Value));
end;

procedure TYclISOMemIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section, Name, ISOFloatToStr(Value));
end;

procedure TYclISOMemIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISOTimeToStr(Value));
end;

procedure TYclISOMemIniFile.WriteHex(const Section, Name: String; Value: Cardinal);
begin
  WriteString(Section, Name, Format('%.x', [Value]));
end;

// *******************************************************************************************

function TYclISOIniFile.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOIniFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToDateTimeDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOIniFile.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  Result := ISOStrToFloatDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOIniFile.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := ISOStrToTimeDef(ReadString(Section, Name, ''), Default);
end;

function TYclISOIniFile.ReadHex(const Section, Name: String; Default: Cardinal): Cardinal;
begin
  Result := StrToIntDef('$' + ReadString(Section, Name, ''), Default);
end;

procedure TYclISOIniFile.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateToStr(Value));
end;

procedure TYclISOIniFile.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISODateTimeToStr(Value));
end;

procedure TYclISOIniFile.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section, Name, ISOFloatToStr(Value));
end;

procedure TYclISOIniFile.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  WriteString(Section, Name, ISOTimeToStr(Value));
end;

procedure TYclISOIniFile.WriteHex(const Section, Name: String; Value: Cardinal);
begin
  WriteString(Section, Name, Format('%.x', [Value]));
end;

end.
