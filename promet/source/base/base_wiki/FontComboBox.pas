//  FontComboBox  ver 1.01   Date: 12-16-1998
//  created by Jimmy Theo
//  email: Theo@elang.stts.ac.id
//
//  usage : Just drop this compoments to a form and list of all the fonts
//          that installed on the system will appears in Combobox
//        * there is also preperty TTOnly which will display TrueType Fonts only
//        * there is also preperty UseItemFont that will display the name of font
//          in they own font, with the size,and effect same with the
//          FontComboBox.font property
//
// this is unit was based on FontComb.pas created by Hardy Yau, CIS 102144,712
// use this unit anyway U want, No Fee. No Guarantee, ENJOY !
/////////////////////// History ///////////////////////////////////////////////////
// ver 1.01
// - fix itemheight's bug when selecting larger font size
// - added some featues by Milos Dragovic <dragomil@EUnet.yu>
//   UpdateAllForms property : If it's set to TRUE, selecting a font will
//                            automatically change the font property for all
//                            forms in the application (actually, all forms
//                            directly owned by application, i.e. created with
//                            application.createform... or form.create(application))


unit FontComboBox;

interface

uses
  LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LCLType, LCLProc, LCLIntf;

type
  TFontComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    FBitmap: TBitmap;
    FCanvas: TControlCanvas;
    FTTOnly: Boolean;
    FUseItemFont: Boolean;
{added}FUpdateAllForms : boolean;
    function  IsTrueType(Index: Integer): Boolean;
    procedure DrawTT(Background: TColor);
    procedure SetTTOnly(Value : boolean);
    procedure SetUseItemFont(Value : boolean);
  protected
    { Protected declarations }
    procedure CreateWnd; override;
{added}procedure CMFontChanged(var Message); message CM_FONTCHANGED;
{added}procedure Change; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  published
    { Published declarations }
    property Color;
//    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property Items;
    property ParentColor;
//    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property TTonly : boolean read FTTonly write SetTTonly;
    property UseItemFont : boolean read FUseItemFont write SetUseItemFont;
{added}property UpdateAllForms : boolean read FUpdateAllForms write FUpdateAllForms;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Download', [TFontComboBox]);
end;

constructor TFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted := True;
  Style := csOwnerDrawFixed;
  ItemHeight := font.size*2;
  {Create a bitmap for drawing}
  FBitmap := TBitmap.Create;
  FBitmap.Height := 12;
  FBitmap.Width := 12;
  {Create a Canvas for checking True Type property}
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
{added}DropDownCount := 16;
end;

procedure TFontComboBox.SetTTOnly(value : boolean);
var I:longint;
begin
  FTTonly := Value;
  if FTTonly = True then
  begin
   Items.clear;
   Items.Assign(Screen.Fonts);
   I := 0;
   repeat
   if Items[I] = 'Default' then
   begin
     Items.delete(I);
   end else inc(I);
   until I = Items.count;
   ItemIndex := 0;
   I := 0;
   if FTTonly then
     repeat
       if not IsTrueType(I) then
       begin
          Items.delete(I);
       end else inc(I);
     until I = Items.count;
  end else
  begin
   Items.clear;
   Items.Assign(Screen.Fonts);
   I := 0;
   repeat
    if Items[I] = 'Default' then
    begin
      Items.delete(I);
    end else inc(I);
   until I = Items.count;
   ItemIndex := 0;
  end;
end;

procedure TFontComboBox.SetUseItemFont(value : boolean);
begin
 FUseItemFont := Value;
 SetTTOnly(FTTOnly);
end;

destructor TFontComboBox.Destroy;
begin
  FBitmap.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TFontComboBox.CreateWnd;
var I:longint;
begin
  inherited CreateWnd;
  Items.Assign(Screen.Fonts);
  I := 0;
  repeat
   if Items[I] = 'Default' then
   begin
        Items.delete(I);
   end else inc(I);
  until I = Items.count;
  ItemIndex := 0;
end;
procedure TFontComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  lf: TLogFont;
  oldFont, newFont: HFont;
begin
NewFont := 0;
OldFont := 0;
if FUseItemFont then
begin
  with lf do begin
    lfHeight := Rect.Bottom-Rect.Top;// Font.Height;
    lfWidth := 0;
    lfEscapement := 0;
    if fsBold in Font.Style then
       lfWeight := FW_BOLD
    else
       lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in Font.Style);
    lfUnderline := Byte(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    lfCharSet := DEFAULT_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, Items[Index]);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(Canvas.Handle, newFont);
end;
  { Rect.Left = 3 means drawing on the Static portion of the ComboBox }
  with Canvas do begin
    FillRect(Rect);
    if IsTrueType(Index) and (Rect.Left <> 3) then begin
       DrawTT(Brush.Color);
       Draw(Rect.Left+2, Rect.Top+2, FBitmap);
    end;
    if (Rect.Left <> 3) then
       TextOut(Rect.Left+16, Rect.Top, Items[Index])
    else
       TextOut(Rect.Left, Rect.Top, Items[Index])
  end;
if FUseItemFont then
begin
  SelectObject(Canvas.Handle, oldFont);
  DeleteObject(newFont);
end;
end;

const TMPF_TRUETYPE = 4;

function TFontComboBox.IsTrueType(Index: Integer): Boolean;
var
  Metrics: TTextMetric;
  lf: TLogFont;
  oldFont, newFont: HFont;
begin
  with lf do begin
    lfHeight := 10;
    lfWidth := 10;
    lfEscapement := 0;
    lfWeight := FW_REGULAR;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := DEFAULT_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, Items[Index]);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(FCanvas.Handle, newFont);
  GetTextMetrics(FCanvas.Handle, Metrics);
  Result := (Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
  SelectObject(FCanvas.Handle, oldFont);
  DeleteObject(newFont);
end;

procedure TFontComboBox.DrawTT(Background: TColor);
  procedure DrawT(OrgX, OrgY: Integer; Color: TColor);
  begin
   with FBitmap.Canvas do begin
     Brush.Style := bsSolid;
     Pen.Color := Color;
     MoveTo(OrgX,OrgY);
     LineTo(OrgX+7,OrgY);
     LineTo(OrgX+7,OrgY+3);
     MoveTo(OrgX,OrgY);
     LineTo(OrgX,OrgY+3);
     MoveTo(OrgX+1,OrgY);
     LineTo(OrgX+1,OrgY+1);
     MoveTo(OrgX+6,OrgY);
     LineTo(OrgX+6,OrgY+1);
     MoveTo(OrgX+3,OrgY);
     LineTo(OrgX+3,OrgY+8);
     MoveTo(OrgX+4,OrgY);
     LineTo(OrgX+4,OrgY+8);
     MoveTo(OrgX+1,OrgY+8);
     LineTo(OrgX+6,OrgY+8);
   end;
  end;
begin
  with FBitmap.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := background;
    FillRect(Rect(0,0,12,12));
    DrawT(0,0,clGray);
    DrawT(4,3,clBlack);
  end;
end;

procedure TFontComboBox.CMFontChanged(var Message);
begin // without this, changing the font of the component had no effect
  ItemHeight := font.size*2;
  RecreateWnd(Self);// on it's appearance (height wasn't adjusted)
  repaint;
end;

procedure TFontComboBox.Change;
var i : integer;
begin
  inherited Change;
  if FUpdateAllForms then // Changing font on all forms
     for i := 0 to Application.ComponentCount-1 do
       if Application.components[i] is TCustomForm then
          (Application.components[i] as TCustomForm).Font.Name := Text;
end;
end.
