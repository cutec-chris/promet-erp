unit scrollingcontrol;

//This is a very tiny wrapper, not for general purpose
//22.6.2010 Theo

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
LCLType, LCLIntf, Classes, SysUtils, Forms, Controls, StdCtrls, messages;

type

{ TScrollingControl }

  TScrollingControl = class(TCustomControl)
  private
    fCanShowH: boolean;
    fCanShowV: boolean;
    FScrollBars: TScrollStyle;
    fLargeStep: integer;
    fSmallStep: integer;
    function GetHScrollPosition: integer;
    function GetVScrollPosition: integer;
    procedure SetCanShowH(const AValue: boolean);
    procedure SetCanShowV(const AValue: boolean);
    procedure SetHScrollPosition(const AValue: integer);
    procedure SetScrollBars(const AValue: TScrollStyle);
    procedure DoSetScrollBars(const AValue: TScrollStyle; Force: Boolean = true);
    procedure SetVScrollPosition(const AValue: integer);
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetVScrollPos(NewPos: Longint);
    procedure SetHScrollPos(NewPos: Longint);
  protected
    FVScrollInfo: TScrollInfo;
    FHScrollInfo: TScrollInfo;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure VScroll(var Msg: TWMScroll); virtual;
    procedure HScroll(var Msg: TWMScroll); virtual;
    procedure UpdateHScrollInfo;
    procedure UpdateVScrollInfo;
    property HScrollInfo: TScrollInfo read fHScrollInfo write fHScrollInfo;
    property VScrollInfo: TScrollInfo read fVScrollInfo write fVScrollInfo;
    property CanShowV: boolean read fCanShowV write SetCanShowV;
    property CanShowH: boolean read fCanShowH write SetCanShowH;
    property SmallStep: integer read fSmallStep write fSmallStep;
    property LargeStep: integer read fLargeStep write fLargeStep;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property VScrollPosition: integer read GetVScrollPosition write SetVScrollPosition;
    property HScrollPosition: integer read GetHScrollPosition write SetHScrollPosition;
  end;

implementation

uses Math;

{ TScrollingControl }

procedure TScrollingControl.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TScrollingControl.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
  if {fWantTabs and}(GetKeyState(VK_CONTROL) >= 0) then Msg.Result := Msg.Result or DLGC_WANTTAB;
end;

procedure TScrollingControl.WMHScroll(var Msg: TWMScroll);
begin
  HScroll(Msg);
end;

procedure TScrollingControl.WMVScroll(var Msg: TWMScroll);
begin
  VScroll(Msg);
end;

procedure ZeroMemory(Destination: Pointer; Length: Cardinal);
begin
  FillChar(Destination^, Length, 0);
end;

procedure TScrollingControl.SetVScrollPos(NewPos: Integer);
begin
  FVScrollInfo.nPos := Max(0, Min(fVScrollInfo.nMax - fVScrollInfo.nPage, NewPos));
  SetScrollInfo(Handle, SB_VERT, fVScrollInfo, True);
  Invalidate;
end;


procedure TScrollingControl.VScroll(var Msg: TWMScroll);
var si: TScrollInfo;
var newPos: Longint;
begin
 if CanShowV then begin
  {$IFDEF LCLqt}
    newpos := Msg.Pos;
  {$ELSE}
    ZeroMemory(@si, sizeof(si));
    si.cbSize := sizeof(si);
    si.fMask := SIF_TRACKPOS; //MSDN against 65k limit
    {$IFDEF LCLGtk2}
    newpos := Msg.Pos;
    {$ELSE}
    if GetScrollInfo(Handle, SB_VERT, si) then newPos := si.nTrackPos else  newpos := Msg.Pos;
    {$ENDIF}
  {$ENDIF}
    case Msg.ScrollCode of
      SB_THUMBPOSITION,
	SB_THUMBTRACK:
	begin
	  SetVScrollPos(NewPos);
	end;
      SB_LINEDOWN: SetVScrollPos(FVScrollInfo.nPos + fSmallStep);
      SB_PAGEDOWN: SetVScrollPos(FVScrollInfo.nPos + fLargeStep);
      SB_LINEUP: SetVScrollPos(FVScrollInfo.nPos - fSmallStep);
      SB_PAGEUP: SetVScrollPos(FVScrollInfo.nPos - fLargeStep);
      SB_TOP: SetVScrollPos(0);
      SB_BOTTOM: SetVScrollPos(FVScrollInfo.nMax);
    end;
  end;
end;


procedure TScrollingControl.SetHScrollPos(NewPos: Integer);
begin
  fHScrollInfo.nPos := Max(0, Min(fHScrollInfo.nMax - fHScrollInfo.nPage, NewPos));
  SetScrollInfo(Handle, SB_HORZ, fHScrollInfo, True);
  Invalidate;
end;


procedure TScrollingControl.HScroll(var Msg: TWMScroll);
var si: TScrollInfo;
var newPos: Longint;
begin
{$IFDEF LCLqt}
  newpos := Msg.Pos;
{$ELSE}
  ZeroMemory(@si, sizeof(si));
  si.cbSize := sizeof(si);
  si.fMask := SIF_TRACKPOS; //MSDN against 65k limit
 {$IFDEF LCLGtk2}
  newpos := Msg.Pos;
  {$ELSE}
  if GetScrollInfo(Handle, SB_HORZ, si) then newPos := si.nTrackPos else newpos := Msg.Pos;
 {$ENDIF}
{$ENDIF}
  case Msg.ScrollCode of
    SB_THUMBPOSITION,
      SB_THUMBTRACK:
      begin
        SetHScrollPos(NewPos);
      end;
    SB_LINEDOWN: SetHScrollPos(FHScrollInfo.nPos + fSmallStep);
    SB_PAGEDOWN: SetHScrollPos(FHScrollInfo.nPos + fLargeStep);
    SB_LINEUP: SetHScrollPos(FHScrollInfo.nPos - fSmallStep);
    SB_PAGEUP: SetHScrollPos(FHScrollInfo.nPos - fLargeStep);
    SB_LEFT: SetHScrollPos(0);
    SB_RIGHT: SetVScrollPos(FHScrollInfo.nMax);
  end;
end;


procedure TScrollingControl.SetScrollBars(const AValue: TScrollStyle);
begin
  DoSetScrollBars(AValue, False);
end;

procedure TScrollingControl.DoSetScrollBars(const AValue: TScrollStyle; Force: Boolean);
begin
  if (FScrollBars <> AValue) or Force then
  begin
    case AValue of
      ssBoth: begin
          ShowScrollBar(Handle, SB_HORZ, True);
          ShowScrollBar(Handle, SB_VERT, True);
        end;

      ssNone: begin
          ShowScrollBar(Handle, SB_HORZ, False);
          ShowScrollBar(Handle, SB_VERT, False);
        end;

      ssVertical: begin
          ShowScrollBar(Handle, SB_HORZ, False);
          ShowScrollBar(Handle, SB_VERT, True);
        end;

      ssHorizontal: begin
          ShowScrollBar(Handle, SB_HORZ, True);
          ShowScrollBar(Handle, SB_VERT, False);
        end;

      ssAutoBoth: begin
          ShowScrollBar(Handle, SB_HORZ, CanShowH);
          ShowScrollBar(Handle, SB_VERT, CanShowV);
        end;

      ssAutoHorizontal: begin
          ShowScrollBar(Handle, SB_HORZ, CanShowH);
          ShowScrollBar(Handle, SB_VERT, False);
        end;

      ssAutoVertical: begin
          ShowScrollBar(Handle, SB_HORZ, False);
          ShowScrollBar(Handle, SB_VERT, CanShowV);
        end;
    end;
    FScrollBars := AValue;
    UpdateHScrollInfo;
    UpdateVScrollInfo;
  end;
end;

procedure TScrollingControl.SetVScrollPosition(const AValue: integer);
begin
  fVScrollInfo.nPos := AValue;
  UpdateVScrollInfo;
end;

procedure TScrollingControl.SetCanShowH(const AValue: boolean);
begin
  if HandleAllocated then
    if (AValue <> fCanShowH) and (fScrollBars in [ssAutoBoth, ssAutoHorizontal]) then
    begin
      fCanShowH := AValue;
      DoSetScrollBars(fScrollbars);
    end;
  fCanShowH := AValue;
end;

function TScrollingControl.GetVScrollPosition: integer;
begin
  Result := VScrollInfo.nPos;
end;

function TScrollingControl.GetHScrollPosition: integer;
begin
  Result := HScrollInfo.nPos;
end;

procedure TScrollingControl.SetCanShowV(const AValue: boolean);
begin
  if HandleAllocated then
    if (AValue <> fCanShowV) and (fScrollBars in [ssAutoBoth, ssAutoVertical]) then
    begin
      fCanShowV := AValue;
      DoSetScrollBars(fScrollbars);
    end;
  fCanShowV := AValue;
end;

procedure TScrollingControl.SetHScrollPosition(const AValue: integer);
begin
  fHScrollInfo.nPos := AValue;
  UpdateHScrollInfo;
end;


procedure TScrollingControl.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
{$IFOPT R+}{$DEFINE RangeCheckOn}{$R-}{$ENDIF}
    WindowClass.Style := WindowClass.Style and not Cardinal(ClassStylesOff);
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[bsSingle] or WS_CLIPCHILDREN;
{$IFDEF RangeCheckOn}{$R+}{$ENDIF}
    if true (*NewStyleControls and Ctl3D {and (BorderStyle = bsSingle)} *) then begin
      Style := Style and not Cardinal(WS_BORDER);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;


procedure TScrollingControl.CreateWnd;
begin
  inherited CreateWnd;
//  if not (csLoading in componentstate) then
  begin
  DoSetScrollBars(fScrollBars);
  UpdateHScrollInfo;
  UpdateVScrollInfo;
  end;
end;

constructor TScrollingControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fScrollBars := ssAutoBoth;
  fVScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL and not SIF_TRACKPOS;
  fVScrollInfo.nMax := 1000;
  fVScrollInfo.nMin := 0;
  fVScrollInfo.nPage := 100;
  fVScrollInfo.nPos := 0;
  fHScrollInfo := FVScrollInfo;
  fLargeStep := 50;
  fSmallStep := 12;
end;

destructor TScrollingControl.Destroy;
begin
  inherited Destroy;
end;

procedure TScrollingControl.UpdateHScrollInfo;
begin
  if (fScrollBars in [ssBoth, ssHorizontal]) or ((fScrollBars in [ssAutoBoth, ssAutoHorizontal]) and CanShowH) then
  begin
    SetHScrollPos(HScrollInfo.nPos);
  end;
end;

procedure TScrollingControl.UpdateVScrollInfo;
begin
  if (fScrollBars in [ssBoth, ssVertical]) or ((fScrollBars in [ssAutoBoth, ssAutoVertical]) and CanShowV) then
  begin
    SetVScrollPos(VScrollInfo.nPos);
  end;

end;


end.
