unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PReport, ExtCtrls, StdCtrls, Buttons, PRJpegImage, ExtDlgs, PdfDoc, ShellApi;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    PRPage1: TPRPage;
    PRLayoutPanel1: TPRLayoutPanel;
    PRJpegImage1: TPRJpegImage;
    PRLayoutPanel2: TPRLayoutPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    CheckBox1: TCheckBox;
    OpenPictureDialog1: TOpenPictureDialog;
    PReport1: TPReport;
    PRLabel1: TPRLabel;
    SaveDialog1: TSaveDialog;
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure PRPage1PrintPage(Sender: TObject; ACanvas: TPRCanvas);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  PRJpegImage1.Stretch := CheckBox1.Checked;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  SaveDialog1.FileName := ChangeFileExt(ExtractFileName(OpenPictureDialog1.FileName), '.pdf');
  if SaveDialog1.Execute then
    with PReport1 do
    begin
      FileName := SaveDialog1.FileName;
      BeginDoc;
      Print(PRPage1);
      EndDoc;
      ShellExecute(Self.Handle, 'Open', PChar(FileName), '', '', SW_SHOW);
    end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    PRJpegImage1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    PRLabel1.Caption := OpenPictureDialog1.FileName;
    PRJpegImage1.Repaint;
  end;
end;

procedure TForm1.PRPage1PrintPage(Sender: TObject; ACanvas: TPRCanvas);
var
  Dest: TPRDestination;
begin
  // create a new destination for the current page.
  Dest := PReport1.CreateDestination;

  // setting the properties for the destination object.
  with Dest do
  begin
    DestinationType := dtXYZ;
    Left := -10;
    Top := -10;
    Zoom := 1;
  end;

  // set the destination object as the open-action.
  PReport1.OpenAction := Dest;
end;

end.
