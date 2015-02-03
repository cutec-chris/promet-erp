unit uImageFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, Buttons, ExtCtrls,
  ExtDlgs, db, uBaseDbClasses, uPrometFramesInplaceDB, Clipbrd, ActnList,
  uExtControls;
type
  TfImageFrame = class(TPrometInplaceDBFrame)
    acAddImage: TAction;
    acCopy: TAction;
    acPaste: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    dnNavigator: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    Images: TDatasource;
    iPreview: TDBImage;
    OpenPictureDialog: TOpenPictureDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pImage: TPanel;
    pToolbar: TPanel;
    sbAddImage: TSpeedButton;
    sbClipboardToImage: TSpeedButton;
    sbImageToClipboard: TSpeedButton;
    sbImage: TScrollBox;
    procedure acAddImageExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure AValueAfterScroll(aDataSet: TDataSet);
    procedure iPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure iPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure iPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure sbAddImageClick(Sender: TObject);
    procedure sbClipboardToImageClick(Sender: TObject);
    procedure sbImageToClipboardClick(Sender: TObject);
  private
    { private declarations }
    StartX,
    StartY,
    MoveX,
    MoveY: Integer;
    IsMoved: Boolean;
    FScale : real;
    procedure DoScalePreview;
  public
    { public declarations }
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    destructor Destroy;override;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation
{$R *.lfm}
procedure TfImageFrame.FrameEnter(Sender: TObject);
begin
  Images.DataSet.AfterScroll:=@AValueAfterScroll;
  Images.DataSet.Open;
end;
procedure TfImageFrame.acAddImageExecute(Sender: TObject);
begin
  if OpenpictureDialog.Execute then
    begin
      if not FDataSet.CanEdit then
        FDataSet.Insert;
      iPreview.Picture.LoadFromFile(OpenPictureDialog.FileName);
    end;
end;
procedure TfImageFrame.acCopyExecute(Sender: TObject);
begin
  Clipboard.Assign(iPreview.Picture.Bitmap);
end;
procedure TfImageFrame.acPasteExecute(Sender: TObject);
begin
  if not FDataSet.CanEdit then FDataSet.Insert;
  Clipboard.AssignTo(iPreview.Picture);
end;
procedure TfImageFrame.AValueAfterScroll(aDataSet: TDataSet);
begin
  if iPreview.Picture.Width<=0 then exit;
  FScale := pImage.Width/iPreview.Picture.Width;
  DoScalePreview;
  iPreview.Top:=0;
  iPreview.Left:=0;
end;
procedure TfImageFrame.iPreviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      IsMoved := True;
      StartX  := X;
      StartY  := Y;
      MoveX   := X;
      MoveY   := Y;
      sbImage.DoubleBuffered := True;
    end;
end;
procedure TfImageFrame.iPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if isMoved then
    begin
      if (X < MoveX) and ((iPreview.Left + iPreview.Width + 25) > sbImage.Width) then
        iPreview.Left := iPreview.Left + (X - StartX);

      if (X > MoveX) and (iPreview.Left < 0) then
        iPreview.Left := iPreview.Left + (X - StartX);

      if (Y < MoveY) and ((iPreview.Top + iPreview.Height + 25) > sbImage.Height) then
        iPreview.Top := iPreview.Top + (Y - StartY);

      if (Y > MoveY) and (iPreview.Top < 0) then
        iPreview.Top := iPreview.Top + (Y - StartY);
      MoveX := X;
      MoveY := Y;
    end;
end;
procedure TfImageFrame.iPreviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      IsMoved := False;
      sbImage.DoubleBuffered := False;
    end;
end;
procedure TfImageFrame.FrameExit(Sender: TObject);
begin
  DataSet.DataSet.AfterScroll:=nil;
//  Images.DataSet.Close;
end;
procedure TfImageFrame.sbAddImageClick(Sender: TObject);
begin
  if OpenpictureDialog.Execute then
    begin
      iPreview.Picture.LoadFromFile(OpenPictureDialog.FileName);
    end;
end;
procedure TfImageFrame.sbClipboardToImageClick(Sender: TObject);
begin
  Clipboard.AssignTo(iPreview.Picture);
end;
procedure TfImageFrame.sbImageToClipboardClick(Sender: TObject);
begin
  Clipboard.Assign(iPreview.Picture.Bitmap);
end;
procedure TfImageFrame.DoScalePreview;
begin
  iPreview.Height:=round(iPreview.Picture.Height*FScale);
  iPreview.Width:=round(iPreview.Picture.Width*FScale);
end;
procedure TfImageFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  Images.DataSet := FDataSet.DataSet;
end;
destructor TfImageFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TfImageFrame.SetRights(Editable: Boolean);
begin
  acAddImage.Enabled := Editable;
  acPaste.Enabled:= Editable;
  ArrangeToolBar(pToolbar,ActionList1,'Images');
end;

end.

