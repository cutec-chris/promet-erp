unit uscreenshotmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, ExtDlgs, StdCtrls;

type

  { TfScreenshot }

  TfScreenshot = class(TForm)
    Label1: TLabel;
    Scr: TImage;
    SavePictureDialog1: TSavePictureDialog;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ScrDblClick(Sender: TObject);
    procedure ScrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    FSaveTo: string;
    { private declarations }
      FSelecting: Boolean;
      FSelRect: TRect;
      FSelX: Integer;
      FSelY: Integer;
      procedure SetSaveTo(AValue: string);
  public
    { public declarations }
    property SaveTo : string read FSaveTo write SetSaveTo;
  end; 

var
  fScreenshot: TfScreenshot;

implementation
{$R *.lfm}
uses LCLIntf,Math;
procedure TfScreenshot.FormCreate(Sender: TObject);
var
  FScreenDC: HDC;
  FScreen: TBitmap;
begin
  sleep(100);
  FScreenDC := GetDC(0);
  FScreen := TBitmap.Create;
  try
    FScreen.LoadFromDevice(FScreenDC);
    Scr.Picture.Assign(FScreen);
  finally
    FScreen.Free;
  end;
  BoundsRect := Screen.DesktopRect;
end;

procedure TfScreenshot.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  FBitmap: TBitmap;
  aDir: String;
  i : Integer = 1;
begin
  aDir := GetCurrentDir;
  SavePictureDialog1.InitialDir := aDir;
  while FileExists(AppendPathDelim(aDir)+'screen_'+IntToStr(i)+'.png') do
    inc(i);
 if FSaveTo <> '' then
   begin
     FBitmap := TBitmap.Create;
     FBitmap.Height := FSelRect.Bottom-FSelRect.Top-2;
     FBitmap.Width := FSelRect.Right-FSelRect.Left-2;
     FBitmap.Canvas.CopyRect(Rect(-1,-1,FBitmap.Width+1,FBitmap.Height+1),Scr.Picture.Bitmap.Canvas,FSelRect);
     Scr.Picture.Assign(FBitmap);
     Scr.Picture.Jpeg.CompressionQuality:=90;
     Scr.Picture.SaveToFile(FSaveTo);
     FBitmap.Free;
   end
 else
   begin
     SavePictureDialog1.FileName:=AppendPathDelim(aDir)+'screen_'+IntToStr(i)+'.png';
      if SavePictureDialog1.Execute then
        begin
          FBitmap := TBitmap.Create;
          FBitmap.Height := FSelRect.Bottom-FSelRect.Top-2;
          FBitmap.Width := FSelRect.Right-FSelRect.Left-2;
          FBitmap.Canvas.CopyRect(Rect(-1,-1,FBitmap.Width+1,FBitmap.Height+1),Scr.Picture.Bitmap.Canvas,FSelRect);
          Scr.Picture.Assign(FBitmap);
          Scr.Picture.SaveToFile(SavePictureDialog1.FileName);
          FBitmap.Free;
        end;
   end;
end;

procedure TfScreenshot.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ScrDblClick(Self);
end;

procedure TfScreenshot.ScrDblClick(Sender: TObject);
begin
  Close;
end;

procedure TfScreenshot.ScrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelX := X;
  FSelY := Y;
  FSelecting := True;
end;

procedure TfScreenshot.ScrMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Scale: Single;
  W: Integer;
  H: Integer;
begin
  if FSelecting then
  begin
    Scr.Canvas.DrawFocusRect(FSelRect);
    Scale := Scr.Width / Scr.Height;
    W := X - FSelX;
    H := Y - FSelY;
    {
    if (W <> 0) and (H <> 0) then
      if Abs(W) / Abs(H) > Scale then
        H := Round(Abs(W) / Scale) * Sign(H)
      else
        W := Round(Abs(H) * Scale) * Sign(W);
    }
    FSelRect := Bounds(Min(FSelX, FSelX + W), Min(FSelY, FSelY + H), Abs(W), Abs(H));
    Scr.Canvas.DrawFocusRect(FSelRect);
  end;
end;

procedure TfScreenshot.ScrMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelecting := False;
end;

procedure TfScreenshot.Timer1Timer(Sender: TObject);
begin
  Label1.Visible:=not Label1.Visible;
end;

procedure TfScreenshot.SetSaveTo(AValue: string);
begin
  if FSaveTo=AValue then Exit;
  FSaveTo:=AValue;
end;

end.

