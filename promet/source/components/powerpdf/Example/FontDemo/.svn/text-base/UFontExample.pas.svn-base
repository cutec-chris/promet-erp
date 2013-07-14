unit UFontExample;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, PReport, PdfDoc, Menus, ComCtrls{$IFDEF FPC}, LResources{$ENDIF};

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
    SaveDialog1: TSaveDialog;
    PRRect1: TPRRect;
    PRText3: TPRText;
    PRText4: TPRText;
    PRText5: TPRText;
    PRText6: TPRText;
    PRText7: TPRText;
    PRText8: TPRText;
    PRText9: TPRText;
    PRText10: TPRText;
    PRText11: TPRText;
    PRText12: TPRText;
    PRText13: TPRText;
    PRText14: TPRText;
    PRText15: TPRText;
    PRText16: TPRText;
    PRText17: TPRText;
    PRText18: TPRText;
    PRText19: TPRText;
    PRText20: TPRText;
    PRText21: TPRText;
    PRText22: TPRText;
    PRText23: TPRText;
    PRText24: TPRText;
    PRText25: TPRText;
    PRText26: TPRText;
    PRRect2: TPRRect;
    PRText27: TPRText;
    PRText28: TPRText;
    PRText29: TPRText;
    PRText30: TPRText;
    PRText31: TPRText;
    PRText32: TPRText;
    PRText33: TPRText;
    PRText34: TPRText;
    PRText35: TPRText;
    PRText36: TPRText;
    PRText37: TPRText;
    PRText38: TPRText;
    PRText39: TPRText;
    PRText40: TPRText;
    PRRect3: TPRRect;
    PRText41: TPRText;
    PRText42: TPRText;
    PRText43: TPRText;
    PRText44: TPRText;
    PRText45: TPRText;
    PRText46: TPRText;
    PRText47: TPRText;
    PRText48: TPRText;
    PRText49: TPRText;
    PRText50: TPRText;
    PRLabel1: TPRLabel;
    PRLabel3: TPRLabel;
    PRLabel2: TPRLabel;
    PRLabel4: TPRLabel;
    PRLabel5: TPRLabel;
    PRLabel6: TPRLabel;
    PRLabel7: TPRLabel;
    PRLabel8: TPRLabel;
    PRLabel9: TPRLabel;
    PRLabel10: TPRLabel;
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
{$IFNDEF FPC}{$R *.DFM}{$ENDIF}

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

{$IFDEF FPC}
initialization
  {$I UFontExample.lrs}
{$ENDIF}

end.
