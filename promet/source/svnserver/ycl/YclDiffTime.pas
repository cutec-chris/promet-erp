{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  CPU Timestamp                                                                                   }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclDiffTime.pas.                                                          }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclDiffTime;

interface
uses
  Windows, SysUtils;
  
type
  {$IFDEF SUPPORTS_INT64}
  TDiffTime = Int64;
  {$ELSE SUPPORTS_INT64~}
  TDiffTime = Comp;
  {$ENDIF ~SUPPORTS_INT64}

// read Pentium time stamp counter
function RDTSC: TDiffTime;

// CPU Speed in MHz
function GetCPUSpeed: Extended;

// Time -> String
function DiffTimeCyclesToStr(ATime: TDiffTime): String;

// Time in Microseconds
function DiffTimeCyclesToTime(ATime: TDiffTime): Extended;

implementation

// read Pentium time stamp counter
function RDTSC: TDiffTime;
asm
        DB      $0F, $31  // opcode for RDTSC
        {$IFNDEF SUPPORTS_INT64 ~}
        PUSH    EDX
        PUSH    EAX
        FILD    Comp([ESP])
        ADD     ESP, 8
        {$ENDIF ~SUPPORTS_INT64}
end;

function CalcCPUSpeed: Extended;
const
  DelayTime = 100;  // measure time in ms
var
  TimerHi, TimerLo: DWord;
  PriorityClass, Priority: Integer;
  Freq, Ticks1, Ticks2: Int64;
begin
  try
    if QueryPerformanceFrequency(Freq) then begin
      PriorityClass := GetPriorityClass(GetCurrentProcess);
      Priority := GetThreadPriority(GetCurrentThread);
      SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
      SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
      Sleep(0);
      QueryPerformanceCounter(Ticks1);
      asm
        DB      $0F, $31  // opcode for RDTSC
        MOV     TimerLo, EAX
        MOV     TimerHi, EDX
      end;
      Sleep(DelayTime);
      QueryPerformanceCounter(Ticks2);
      asm
        DB      $0F, $31  // opcode for RDTSC
        SUB     EAX, TimerLo
        SBB     EDX, TimerHi
        MOV     TimerLo, EAX
        MOV     TimerHi, EDX
      end;
      SetThreadPriority(GetCurrentThread, Priority);
      SetPriorityClass(GetCurrentProcess, PriorityClass);
      Result := (TimerLo * Freq) / (Ticks2 - Ticks1);
    end
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

var
  CPUSpeed: Extended = 0;
  IsCalculated: Boolean = False;

function GetCPUSpeed: Extended;
begin
  if not IsCalculated then begin
    IsCalculated := True;
    CPUSpeed := CalcCPUSpeed;
  end;
  Result := CPUSpeed;
end;

function DiffTimeCyclesToTime(ATime: TDiffTime): Extended;
begin
  Result := ATime / GetCPUSpeed;
end;

function DiffTimeCyclesToStr(ATime: TDiffTime): String;
begin
  {$IFDEF SUPPORTS_INT64}
  Result := IntToStr(ATime);
  {$ELSE SUPPORTS_INT64~}
  Result := Format('%.0f', [ATime]);
  {$ENDIF ~SUPPORTS_INT64}
end;

end.
