unit UMultiSizeDocument;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PReport, PdfDoc, Menus, ComCtrls, PdfTypes, LResources;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    CreatePDF1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    PReport1: TPReport;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ScrollBox1: TScrollBox;
    PRPage1: TPRPage;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ScrollBox2: TScrollBox;
    PRPage2: TPRPage;
    ScrollBox3: TScrollBox;
    PRPage3: TPRPage;
    TabSheet4: TTabSheet;
    ScrollBox4: TScrollBox;
    PRPage4: TPRPage;
    PRLayoutPanel1: TPRLayoutPanel;
    PRLayoutPanel2: TPRLayoutPanel;
    PRLayoutPanel3: TPRLayoutPanel;
    PRLayoutPanel4: TPRLayoutPanel;
    PRText1: TPRText;
    PRText2: TPRText;
    PRText3: TPRText;
    PRText4: TPRText;
    procedure CreatePDF1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.CreatePDF1Click(Sender: TObject);
var
  APage: TPRPage;
  i: integer;
begin
  if not SaveDialog1.Execute then Exit;
  with PReport1 do
  begin
    FileName := SaveDialog1.FileName;
    BeginDoc;
    for i := 0 to PageControl1.PageCount do
    begin
      APage := TPRPage(Self.FindComponent('PRPage' + IntToStr(i)));
      if APage <> nil then
        Print(APage);
    end;
    EndDoc;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PRPage1.Visible := false;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage(POWER_PDF_VERSION_STR + #13#10 + POWER_PDF_COPYRIGHT);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$i UMultiSizeDocument.lrs}

end.
