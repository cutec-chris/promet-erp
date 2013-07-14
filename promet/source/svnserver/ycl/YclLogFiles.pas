{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Log file functions                                                                              }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclLogFiles.pas.                                                          }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclLogFiles;

interface
uses
  SysUtils, Classes;
  
procedure LogFileAdd(const Msg: String; const Filename: String = '');
procedure LogFileAddFmt(const Fmt: String; const Args: array of const; const Filename: String = '');
procedure LogFileClear(const Filename: String = '');

implementation

type
  TLogFileItem = class(TObject)
  private
    FLogFileName: String;
    FLogFile: Text;
    procedure Open(CreateNew: Boolean);
    procedure Close;
  public
    constructor Create(const LogFileName: String; CreateNew: Boolean);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Msg: String);
  end;

  TLogFileList = class(TObject)
  private
    FList: TStringList;
  protected
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    function GetItem(Filename: String; CreateNew: Boolean): TLogFileItem;
  end;

// *******************************************************************************************

constructor TLogFileItem.Create(const LogFileName: String; CreateNew: Boolean);
begin
  inherited Create;
  FLogFileName := LogFileName;
  Open(CreateNew);
end;

destructor TLogFileItem.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TLogFileItem.Open(CreateNew: Boolean);
begin
  AssignFile(FLogFile, FLogFileName);
  if FileExists(FLogFileName) and not CreateNew then
    Append(FLogFile)
  else
    ReWrite(FLogFile);
end;

procedure TLogFileItem.Close;
begin
  System.Close(FLogFile);
end;

procedure TLogFileItem.Clear;
begin
  Close;
  Open(True);
end;

procedure TLogFileItem.Add(const Msg: String);
var
  Current: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Current := Now;
  DecodeDate(Current, Year, Month, Day);
  DecodeTime(Current, Hour, Min, Sec, MSec);
  WriteLn(FLogFile, Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d,%.3d : %s',
      [Year, Month, Day, Hour, Min, Sec, MSec, Msg]));
  Flush(FLogFile);
end;

// *******************************************************************************************

constructor TLogFileList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TLogFileList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy
end;

procedure TLogFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList.Objects[i].Free;
  FList.Clear;
end;

function TLogFileList.GetItem(Filename: String; CreateNew: Boolean): TLogFileItem;
var
  Index: Integer;
begin
  if Filename = '' then
    Filename := ChangeFileExt(ParamStr(0), '.log');
  Index := FList.IndexOf(Filename);
  if Index < 0 then begin
    Result := TLogFileItem.Create(Filename, CreateNew);
    try
      FList.AddObject(Filename, Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  else begin
    Result := FList.Objects[Index] as TLogFileItem;
    if CreateNew then
      Result.Clear;
  end;
end;

var
  LogFileList: TLogFileList = Nil;

function GetLogFileItem(Filename: String; CreateNew: Boolean): TLogFileItem;
begin
  if not Assigned(LogFileList) then
    LogFileList := TLogFileList.Create;
  Result := LogFileList.GetItem(Filename, CreateNew);
end;

procedure LogFileAdd(const Msg: String; const Filename: String = '');
var
  Item: TLogFileItem;
begin
  Item := GetLogFileItem(Filename, False);
  Item.Add(Msg);
end;

procedure LogFileAddFmt(const Fmt: String; const Args: array of const; const Filename: String = '');
begin
  LogFileAdd(Format(Fmt, Args), Filename);
end;

procedure LogFileClear(const Filename: String = '');
begin
  GetLogFileItem(Filename, True);  // True löscht die Datei
end;

initialization
finalization
  LogFileList.Free;

// *******************************************************************************************

//  History:
//  2005-03-23, Peter J. Haas
//   - Ycl version

end.
