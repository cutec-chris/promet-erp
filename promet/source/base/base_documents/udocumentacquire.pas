{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

info@cu-tec.de
*******************************************************************************}
unit uDocumentAcquire;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs
  {$IFDEF WINDOWS}
  ,DelphiTwain,Windows
  {$ELSE}
  ,sanescanner,sanetool
  {$ENDIF}
  ,ExtCtrls, Buttons, ComCtrls, StdCtrls,uIntfStrConsts,uOCR,LCLType,
  uBaseApplication,uBaseDbClasses;
type

  { TfAcquire }

  TfAcquire = class(TForm)
    bAbort: TBitBtn;
    bCloseandUse: TBitBtn;
    bPreviewScan: TBitBtn;
    bScan: TBitBtn;
    cbType: TComboBox;
    cbOCR: TCheckBox;
    cbSource: TComboBox;
    Image: TImage;
    Label1: TLabel;
    lDevice: TLabel;
    lbDevices: TListBox;
    Panel1: TPanel;
    pSearching: TPanel;
    tbResolution: TTrackBar;
    lResolution: TLabel;
    udPage: TUpDown;
    procedure bScanClick(Sender: TObject);
    procedure bPreviewScanClick(Sender: TObject);
    procedure cbSourceChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lbDevicesSelectionChange(Sender: TObject; User: boolean);
    procedure ScannerAcquireError(Sender: TObject; const Index: Integer;
      ErrorCode, Additional: Integer);
    {$IFDEF WINDOWS}
    procedure ScannerAcquireProgress(Sender: TObject; const Index: Integer;
      const xImage: HBitmap; const Current, Total: Integer);
    procedure ScannerTwainAcquire(Sender: TObject; const Index: Integer;
      xImage: Graphics.TBitmap; var Cancel: Boolean);
    {$ENDIF}
    {$IFDEF LINUX}
    procedure ScannerScanProgress(SaneClient: TSaneClient; Info: TScanData);
    procedure ScannerFinishedScan(Sender: TObject);
    {$ENDIF}
    procedure tbResolutionChange(Sender: TObject);
    procedure udPageChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { private declarations }
    {$IFDEF WINDOWS}
    Scanner : TDelphiTwain;
    {$ENDIF}
    {$IFDEF LINUX}
    Scanner : TScanner;
    Bitmap : TBitmap;
    FScanData : TScanData;
    {$ENDIF}
    Preview : Boolean;
    FirstActivate : Boolean;
    procedure EndScan;
  public
    { public declarations }
    Images : array of TPicture;
    Texts : TOCRPages;
    procedure OCRDone(Sender : TObject);
    function Execute : Boolean;
    function GetScannercount : Integer;
  end;
var
  fAcquire: TfAcquire;
implementation
{$R *.lfm}
uses ubaseconfig;
resourcestring
  strScanNext                   = 'Nächstes Bild scannen';
  strScan                       = 'Scannen';
  strResolution                 = 'Auflösung';
  strSearchingDevices           = 'Suche Geräte';
procedure TfAcquire.bScanClick(Sender: TObject);
var
  xHeight,
  xWidth : Extended;
{$ifdef linux}
  opt: TSaneOption;
{$ENDIF}
begin
  if lbDevices.ItemIndex = -1 then exit;
  bScan.Enabled := False;
  Preview := False;
{$IFDEF WINDOWS}
  Scanner.Source[lbDevices.ItemIndex].LoadSource;
  Scanner.Source[lbDevices.ItemIndex].TransferMode := ttmMemory;
  Scanner.Source[lbDevices.ItemIndex].SetIXResolution(tbResolution.Position);
  Scanner.Source[lbDevices.ItemIndex].SetIYResolution(tbResolution.Position);
  Scanner.Source[lbDevices.ItemIndex].GetIPhysicalHeight(xHeight,rcGet);
  Scanner.Source[lbDevices.ItemIndex].GetIPhysicalHeight(xWidth,rcGet);
//  Scanner.Source[lbDevices.ItemIndex].SetImagelayoutFrame(0,0,xWidth,xHeight);
  Scanner.Source[lbDevices.ItemIndex].EnableSource(False,False);
{$ENDIF}
{$IFDEF LINUX}
  if Scanner.IsOpen then Scanner.Close;
  Scanner.Open(lbDevices.ItemIndex);
  if cbSource.Visible then
    begin
      opt := Scanner.OptionByName('source');
      if Assigned(opt) then
        opt.WriteString(cbSource.Text);
    end;
  Scanner.StdOptions.MaximizeScanRect;
  Scanner.StdOptions.Resolution.WriteInteger(tbResolution.Position);
  Scanner.ScanImage;
{$ENDIF}
  bScan.Caption := strScanNext;
end;
procedure TfAcquire.bPreviewScanClick(Sender: TObject);
var
  xHeight,
  xWidth : Extended;
{$IFDEF LINUX}
  opt: TSaneOption;
{$endif}
begin
  if lbDevices.ItemIndex = -1 then exit;
  bPreviewScan.Enabled := False;
  bScan.Enabled := False;
  Preview := True;
{$IFDEF WINDOWS}
  Scanner.Source[lbDevices.ItemIndex].TransferMode := ttmMemory;
  Scanner.Source[lbDevices.ItemIndex].SetIXResolution(50);
  Scanner.Source[lbDevices.ItemIndex].SetIYResolution(50);
  Scanner.Source[lbDevices.ItemIndex].GetIPhysicalHeight(xHeight,rcGet);
  Scanner.Source[lbDevices.ItemIndex].GetIPhysicalHeight(xWidth,rcGet);
//  Scanner.Source[lbDevices.ItemIndex].SetImagelayoutFrame(0,0,xWidth,xHeight);
  Scanner.Source[lbDevices.ItemIndex].LoadSource;
  Scanner.Source[lbDevices.ItemIndex].EnableSource(False,False);
{$ENDIF}
{$IFDEF LINUX}
  if Scanner.IsOpen then Scanner.Close;
  Scanner.Open(lbDevices.ItemIndex);
  opt := Scanner.OptionByName('source');
  if Assigned(opt) then
    opt.WriteString(cbSource.Text);
  Scanner.StdOptions.Resolution.WriteInteger(Scanner.StdOptions.Resolution.Range.min+10);
  if Scanner.StdOptions.ScanRectAvailable and Scanner.StdOptions.ScanRectModifiable then
    Scanner.StdOptions.MaximizeScanRect;
  Scanner.ScanImage;
{$ENDIF}
end;
procedure TfAcquire.cbSourceChange(Sender: TObject);
begin
with Application as IBaseConfig do
  begin
    Config.WriteString('SCANNERSOURCE',cbSource.Text);
  end;
end;
procedure TfAcquire.FormActivate(Sender: TObject);
var
  i: Integer;
begin
  if not FirstActivate then exit;
  pSearching.Visible := True;
  pSearching.Align := alClient;
  fAcquire.Update;
  fAcquire.Invalidate;
  Application.Processmessages;
{$IFDEF WINDOWS}
  lbDevices.Items.Clear;
  Scanner.EnumerateDevices;
  for i := 0 to Scanner.SourceCount-1 do
    lbDevices.Items.Add(Scanner.Source[i].Manufacturer+' '+Scanner.Source[i].ProductName);
{$ENDIF}
{$IFDEF LINUX}
  if lbDevices.Count = 0 then
    for i := 0 to Scanner.DeviceList.Count-1 do
      begin
        lbDevices.Items.Add(SaneDevArray[i].Vendor+' '+SaneDevArray[i].Model);
      end;
{$ENDIF}
  if lbDevices.ItemIndex = -1 then
    if lbDevices.Items.Count = 1 then
      lbDevices.ItemIndex := 0;
  with Application as IBaseConfig do
    begin
      if Config.ReadString('SCANNER','') <> '' then
        lbDevices.ItemIndex := lbDevices.Items.IndexOf(Config.ReadString('SCANNER',''));
    end;
  pSearching.Visible := False;
  FirstActivate := False;
end;

procedure TfAcquire.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
{$IFDEF LINUX}
  if Scanner.IsOpen then
    begin
      Scanner.Close;
    end;
{$ENDIF}
end;
procedure TfAcquire.FormCreate(Sender: TObject);
begin
  Texts := TOCRPages.Create;
  with Application as IBaseConfig do
    tbResolution.Position := Config.ReadInteger('SCANNERRES',200);
{$IFDEF WINDOWS}
  Scanner := TDelphiTwain.Create(Self);
  Scanner.LoadLibrary;
  Scanner.LoadSourceManager;
  Scanner.EnumerateDevices;
  Scanner.OnAcquireProgress :=@ScannerAcquireProgress;
  Scanner.OnTwainAcquire :=@ScannerTwainAcquire;
  Scanner.OnAcquireError:=@ScannerAcquireError;
  Scanner.Info.Manufacturer := 'CUTEC';
  Scanner.Info.ProductName := 'PrometERP';
  Scanner.Info.ProductFamily := '';
{$ENDIF}
{$IFDEF LINUX}
  Scanner := TScanner.Create(Self);
  Scanner.OnScanProgress:=@ScannerScanProgress;
  Scanner.OnFinishedScan:=@ScannerFinishedScan;
  Bitmap := TBitmap.Create;
{$ENDIF}
end;
procedure TfAcquire.FormDestroy(Sender: TObject);
begin
  Texts.Free;
  if length(Images) > 0 then
    while length(Images) > 0 do
      begin
        Images[length(Images)-1].Free;
        Setlength(Images,length(Images)-1);
      end;
{$IFDEF WINDOWS}
  Scanner.Free;
{$ENDIF}
{$IFDEF LINUX}
  Scanner.Free;
{$ENDIF}
end;
procedure TfAcquire.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfAcquire.FormShow(Sender: TObject);
begin

end;

procedure TfAcquire.lbDevicesSelectionChange(Sender: TObject; User: boolean);
{$IFDEF LINUX}
var
  i: Integer;
  opt: TSaneOption;
{$ENDIF}
begin
  with Application as IBaseConfig do
    Config.WriteString('SCANNER',lbDevices.Items[lbDevices.ItemIndex]);
  {$IFDEF LINUX}
  Scanner.Open(Scanner.DeviceList[lbDevices.ItemIndex]);
  try
    opt := Scanner.OptionByName('source');
    if Assigned(opt) then
      begin
        cbSource.Items.Assign(opt.StringItems);
        cbSource.Hint := opt.Description;
        cbSource.Text:=opt.ReadString;
        with Application as IBaseConfig do
          begin
            if Config.ReadString('SCANNERSOURCE','') <> '' then
              cbSource.Text := Config.ReadString('SCANNERSOURCE',cbSource.Text);
          end;
      end;
    cbSource.Visible:=Assigned(opt);
  except
    cbSource.Visible:=False;
  end;
  Scanner.Close;
  {$ENDIF}
end;
procedure TfAcquire.ScannerAcquireError(Sender: TObject; const Index: Integer;
  ErrorCode, Additional: Integer);
begin
  bScan.Enabled := True;
  bScan.Caption := strScan;
  bPreviewScan.Enabled := True;
end;
{$IFDEF WINDOWS}
procedure TfAcquire.ScannerAcquireProgress(Sender: TObject;
  const Index: Integer; const xImage: HBitmap; const Current, Total: Integer);
{var
  Bitmap : Graphics.TBitmap;}
begin
{  Bitmap := Graphics.TBitmap.Create;
  try
  Bitmap.Handle := xImage;
  Bitmap.SaveToFile('c:\test.bmp');
  Self.Image.Picture.LoadFromFile('c:\test.bmp');
  except
  end;
  Bitmap.free; }
end;
procedure TfAcquire.ScannerTwainAcquire(Sender: TObject; const Index: Integer;
  xImage: Graphics.TBITMAP; var Cancel: Boolean);
begin
  Image.Picture.Bitmap.Assign(xImage);
  Image.Height:=(Image.Width*Image.Picture.Height) div Image.Picture.Width;

  if not Preview then
    begin
      Setlength(Images,length(Images)+1);
      Images[length(Images)-1] := TPicture.Create;
      Images[length(Images)-1].Bitmap.Assign(xImage);
      if cbOCR.Checked then
        begin
          StartOCR(Texts,Images[length(Images)-1]);
          uOCR.OnallprocessDone := @OCRDone;
        end
      else
        bCloseAndUse.Enabled := True;
      udPage.Max:=length(Images)-1;
      udPage.Position:=udPage.Max;
    end;
  bPreviewScan.Enabled := True;
  bScan.Enabled := True;
end;
{$ENDIF}
{$IFDEF LINUX}
procedure TfAcquire.ScannerFinishedScan(Sender: TObject);
var
  Unpaper: TUnPaperProcess;
begin
  bScan.Enabled := True;
  bScan.Caption := strScan;
  bPreviewScan.Enabled := True;
  with BaseApplication as IBaseApplication do
    begin
      Bitmap.SaveToFile(GetInternalTempDir+DirectorySeparator+'test.bmp');
      Image.Picture.LoadFromFile(GetInternalTempDir+DirectorySeparator+'test.bmp');
      if not Preview then
        begin
          Setlength(Images,length(Images)+1);
          Images[length(Images)-1] := TPicture.Create;
          Images[length(Images)-1].LoadFromFile(GetInternalTempDir+DirectorySeparator+'test.bmp');
          try
            Unpaper := TUnPaperProcess.Create(Images[length(Images)-1]);
            while Unpaper.Running do Application.ProcessMessages;
          except
          end;
          if cbOCR.Checked then
            begin
              uOCR.OnallprocessDone := @OCRDone;
              StartOCR(Texts,Images[length(Images)-1]);
            end
          else
            bCloseAndUse.Enabled := True;
        end;
      DeleteFile(GetInternalTempDir+DirectorySeparator+'test.bmp');
    end;
  EndScan;
  Scanner.Close;
end;
procedure TfAcquire.ScannerScanProgress(SaneClient: TSaneClient; Info: TScanData
  );
var
  Ratio: double;
begin
  Label1.Caption:= IntToStr(Info.AcquiredData);
  DrawScanData(Info,Bitmap);
end;
{$ENDIF}
procedure TfAcquire.tbResolutionChange(Sender: TObject);
begin
  lresolution.Caption := strResolution+' '+IntToStr(tbResolution.Position)+' dpi';
  with Application as IBaseConfig do
    Config.WriteInteger('SCANNERRES',tbResolution.Position);
end;
procedure TfAcquire.udPageChanging(Sender: TObject; var AllowChange: Boolean);
var
  Ratio: double;
begin
  AllowChange := True;
  Image.Picture.Clear;
  if length(Images)>udPage.Position then
    begin
      Image.Picture.Assign(Images[udPage.Position]);
      Ratio := Images[udPage.Position].Height / Images[udPage.Position].Width;
      Image.Height := round(Image.Width*Ratio);
    end;
end;

procedure TfAcquire.EndScan;
{$IFDEF LINUX}
var
  opt: TSaneOption;
{$ENDIF}
begin
  {$IFDEF LINUX}
  opt := Scanner.OptionByName('power-save-time');
  if Assigned(opt) then
    opt.WriteInteger(25);
  {$ENDIF}
end;

procedure TfAcquire.OCRDone(Sender: TObject);
begin
  bCloseAndUse.Enabled := True;
end;
function TfAcquire.Execute: Boolean;
var
  i: Integer;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfAcquire,fAcquire);
      Self := fAcquire;
    end;
  bScan.Caption := strScan;
  Image.Picture.Clear;
  bCloseAndUse.Enabled := False;
  if length(Images) > 0 then
    while length(Images) > 0 do
      begin
        Images[length(Images)-1].Free;
        Setlength(Images,length(Images)-1);
      end;
  FirstActivate := True;
  Result := False;
  Result := Showmodal = mrOK;
  {$IFDEF LINUX}
//  if Scanner.IsOpen then Scanner.Close;
  {$ENDIF}
end;
function TfAcquire.GetScannercount: Integer;
begin
{$IFDEF WINDOWS}
  Result := Scanner.SourceCount;
{$ENDIF}
{$IFDEF LINUX}
  try
    Result := Scanner.DeviceList.Count;
  except
    Result := 0;
  end;
{$ENDIF}
end;
initialization
end.

