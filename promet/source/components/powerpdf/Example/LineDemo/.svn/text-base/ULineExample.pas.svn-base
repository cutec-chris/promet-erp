unit ULineExample;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, PReport, PdfDoc, Menus, ComCtrls;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    CreatePDF1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    PReport1: TPReport;
    PRPage1: TPRPage;
    PRLayoutPanel1: TPRLayoutPanel;
    PRText1: TPRText;
    PRText2: TPRText;
    SaveDialog1: TSaveDialog;
    PRRect1: TPRRect;
    PRText3: TPRText;
    PRText4: TPRText;
    PRRect2: TPRRect;
    PRText5: TPRText;
    PRRect3: TPRRect;
    PRRect4: TPRRect;
    PRText6: TPRText;
    PRText7: TPRText;
    PRRect5: TPRRect;
    PRRect6: TPRRect;
    PRText8: TPRText;
    PRRect7: TPRRect;
    PRText9: TPRText;
    PRText10: TPRText;
    PRRect8: TPRRect;
    PRText11: TPRText;
    PRRect9: TPRRect;
    PRRect10: TPRRect;
    PRText12: TPRText;
    PRText13: TPRText;
    PRRect11: TPRRect;
    PRRect12: TPRRect;
    PRText14: TPRText;
    PRRect13: TPRRect;
    PRText15: TPRText;
    PRText16: TPRText;
    PRRect14: TPRRect;
    PRText17: TPRText;
    PRRect15: TPRRect;
    PRRect16: TPRRect;
    PRText18: TPRText;
    PRText19: TPRText;
    PRRect17: TPRRect;
    PRRect19: TPRRect;
    PRText21: TPRText;
    PRText22: TPRText;
    PRRect20: TPRRect;
    PRText23: TPRText;
    PRRect21: TPRRect;
    PRRect22: TPRRect;
    PRText24: TPRText;
    PRText25: TPRText;
    PRRect23: TPRRect;
    PRRect25: TPRRect;
    PRText27: TPRText;
    PRRect26: TPRRect;
    PRText28: TPRText;
    PRRect18: TPRRect;
    PRText20: TPRText;
    PRRect24: TPRRect;
    PRText26: TPRText;
    PRRect27: TPRRect;
    PRText29: TPRText;
    PRRect28: TPRRect;
    PRText30: TPRText;
    PRRect29: TPRRect;
    PRText31: TPRText;
    PRRect30: TPRRect;
    PRText32: TPRText;
    procedure CreatePDF1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CreatePDF1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    with PReport1 do
    begin
      FileName := SaveDialog1.FileName;
      BeginDoc;
      Print(PRPage1);
      EndDoc;
    end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage(POWER_PDF_VERSION_STR + #13#10 + POWER_PDF_COPYRIGHT);
end;

end.
