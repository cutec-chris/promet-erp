unit uTrayHint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, ExtCtrls
  {$IFDEF MSWINDOWS}
  ,CommCtrl,Windows
  {$ENDIF}
  ;
  
{$IFDEF MSWINDOWS}
const
  TTS_BALLOON    = $40;
  TTM_SETTITLE = (WM_USER + 32);
{$ENDIF}

type

  { TBalloonTip }

  TBalloonTip = class
    procedure FTimerTimer(Sender: TObject);
  private
    FTimer : TTimer;
    hWndTip: THandle;
{$IFDEF MSWINDOWS}
    ti: TOOLINFO;
//    function NewWinProc(hWnd: HWND; Msg: WORD; wParam: WORD; lParam: LONGINT):LONGINT;
{$ENDIF}
  public
    constructor Create(hWnd: THandle; Icon: integer; Title: string; Text: String;Time : Integer);
    destructor Destroy;override;
  end;

implementation

{$IFDEF MSWINDOWS}
var
  OldWinProc: TFarProc;
{$ENDIF}

{ TBalloonTip }

procedure TBalloonTip.FTimerTimer(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SendMessage(hWndTip, TTM_TRACKACTIVATE, 0, Integer(@ti));
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function NewWinProc(hWnd: HWND; Msg: WORD; wParam: WORD; lParam: LONGINT):
LONGINT;
var
   MessageProcessed: Boolean;
begin
   MessageProcessed := False;
  case Msg of
  WM_LBUTTONDOWN:
    begin
      MessageProcessed := True;
      SendMessage(hWnd, TTM_TRACKACTIVATE, 0, 0{Integer(@ti)});
    end;
  end;
   if not MessageProcessed then
      Result := CallWindowProc(WNDPROC(OldWinProc),hWnd,Msg,wParam,lParam)
   else
      Result := 0;
end;
{$ENDIF}

constructor TBalloonTip.Create(hWnd: THandle; Icon: integer; Title: string;
  Text: String; Time: Integer);
{$IFDEF MSWINDOWS}
const
  TOOLTIPS_CLASS = 'tooltips_class32';
  TTS_ALWAYSTIP = $01;
  TTS_NOPREFIX = $02;
  TTS_BALLOON = $40;
  TTF_TRANSPARENT = $0100;
  TTM_ADDTOOL = $0400 + 50;
  TTM_SETTITLE = (WM_USER + 32);
  ICC_WIN95_CLASSES = $000000FF;
var
  r : Trect;
{$ENDIF}
begin
  FTimer := TTimer.Create(nil);
  FTimer.Interval := Time;
  FTimer.OnTimer :=@FTimerTimer;
{$IFDEF MSWINDOWS}
  hWndTip := CreateWindow(TOOLTIPS_CLASS, nil,WS_POPUP or TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP, 0, 0, 0, 0, hWnd, 0, HInstance, nil);
  if hWndTip <> 0 then
    begin
//      OldWinProc := Pointer(SetWindowLong(hWndTip,GWL_WNDPROC,integer(@NewWinProc)));
      SetWindowPos(hWndTip, HWND_TOPMOST, 0, 0, 0, 0, {SWP_NOACTIVATE or }SWP_NOMOVE or SWP_NOSIZE);
      ti.cbSize := SizeOf(ti);
      ti.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS or TTF_IDISHWND;
      ti.hwnd := hWnd;
      ti.lpszText := PChar(UTF8Decode(Text));
      Windows.GetClientRect(hWnd, @ti.Rect);
      SendMessage(hWndTip, TTM_ADDTOOL, 1, Integer(@ti));
      SendMessage(hWndTip, TTM_SETTITLE, Icon mod 4, Integer(PChar(Title)));
      SendMessage(hWndTip, TTM_TRACKACTIVATE, 1, Integer(@ti));
    end;
{$ENDIF}
  FTimer.Enabled:=True;
end;

destructor TBalloonTip.Destroy;
begin
{$IFDEF MSWINDOWS}
  SendMessage(hWndTip, TTM_TRACKACTIVATE, 0, Integer(@ti));
{$ENDIF}
  FTimer.Free;
  inherited Destroy;
end;

end.

