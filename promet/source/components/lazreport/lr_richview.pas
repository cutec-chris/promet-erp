{*****************************************}
{                                         }
{  Copyright (c) 2012 by C.Ulrich         }
{                                         }
{*****************************************}
unit lr_richview;

interface

uses
  Classes, SysUtils, LResources,
  Graphics, Controls, Forms, Dialogs,Buttons,
  StdCtrls, Menus, LCLType, ExtCtrls,LR_Class, IpHtml, LR_Desgn;


type
  TUnprotectedHTML = class(TIpHtml)
  end;
  TfrRichViewObject = class(TComponent)  // fake component
  end;
  TfrRichTextView = class(TfrMemoView)
  private
    FHTML : TUnprotectedHtml;
  protected
    procedure SetHTML;
    procedure ShowContent;
    function CalcHeight: Integer; override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy;override;
    procedure Draw(aCanvas: TCanvas); override;
  end;
  TfrRichViewForm = class(TfrObjEditorForm)
    Image1: TImage;
    Panel1: TPanel;
  end;

procedure register;
var
  frRichViewForm : TfrRichViewForm;
implementation
{$R *.lfm}
uses LR_Var, LR_Flds, LR_Const, LR_Utils, InterfaceBase;
resourcestring
  sInsRichtext = 'Insert Richview object';
procedure register;
begin
  RegisterComponents('LazReport', [TfrRichViewObject]);
end;
procedure TfrRichTextView.SetHTML;
var
  aStream: TStringStream;
begin
  Memo1.Assign(Memo);
  aStream := TStringStream.Create('<html><body>'+Memo1.Text+'</body></html>');
  FHTML.LoadFromStream(aStream);
  aStream.Free;
end;

procedure TfrRichTextView.ShowContent;
var
  SavX: Integer;
  SavY: Integer;
begin
  SavX:=X;
  SavY:=Y;
  FHTML.Render(Canvas,Rect(0,0,DRect.Right-DRect.Left,DRect.Bottom-DRect.Top),False,DRect.TopLeft);
  X := SavX;
  Y := SavY;
end;

function TfrRichTextView.CalcHeight: Integer;
var
  aRect: TRect;
begin
  aRect := FHTML.GetPageRect(Canvas,DRect.Right-DRect.Left,0);
  Result := aRect.Bottom-aRect.Top;
end;

constructor TfrRichTextView.Create(AOwnerPage:TfrPage);
begin
  inherited Create(AOwnerPage);
  FHTML := TUnprotectedHTML.Create;
  BeginUpdate;
  try
    BaseName := 'RichView';
  Finally
    EndUpdate;
  end;
end;

destructor TfrRichTextView.Destroy;
begin
  FHTML.Free;
  inherited Destroy;
end;

procedure TfrRichTextView.Draw(aCanvas: TCanvas);
var
  NeedWrap: Boolean;
  newdx: Integer;
  OldScaleX, OldScaleY: Double;
  IsVisible: boolean;
begin
  BeginDraw(aCanvas);
  if ((Flags and flAutoSize) <> 0) and (Memo.Count > 0) and  (DocMode <> dmDesigning) then
  begin
    newdx := CalcHeight;

    if Alignment=Classes.taRightJustify then
    begin
      x := x + dx - newdx;
      dx := newdx;
    end
    else
      dx := newdx;
  end;
  Streaming := False;
  if Memo1.Text <> Memo.Text then
    begin
      SetHTML;
    end;
  newdx := CalcHeight;

  OldScaleX := ScaleX;
  OldScaleY := ScaleY;
  ScaleX := 1;
  ScaleY := 1;
  CalcGaps;
  ScaleX := OldScaleX;
  ScaleY := OldScaleY;
  RestoreCoord;

  CalcGaps;

  if Flags and flHideDuplicates <> 0 then
    IsVisible := (flIsDuplicate and Flags = 0)
  else
    IsVisible := true;

  if IsVisible then
  begin
    if not Exporting then ShowBackground;
    if not Exporting then ShowFrame;
    if Memo1.Count > 0 then
      ShowContent;
  end;

  RestoreCoord;
end;

initialization
  if not assigned(frRichViewForm) then
  begin
    frRichViewForm := TfrRichViewForm.Create(nil);
    frRegisterObject(TfrRichTextView, frRichViewForm.Image1.Picture.Bitmap,
                         sInsRichtext, nil);
  end;
finalization
  frRichViewForm.Free;
end.
