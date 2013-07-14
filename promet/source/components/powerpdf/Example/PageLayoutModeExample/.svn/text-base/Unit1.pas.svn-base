unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PdfDoc, PReport, ShellAPI;

type
  TForm1 = class(TForm)
    PRPage1: TPRPage;
    PRLayoutPanel1: TPRLayoutPanel;
    PRLabel1: TPRLabel;
    PRLabel2: TPRLabel;
    PRLabel3: TPRLabel;
    PRLabel4: TPRLabel;
    PRLabel5: TPRLabel;
    PRLabel6: TPRLabel;
    PRLabel7: TPRLabel;
    PRLabel8: TPRLabel;
    PRLabel9: TPRLabel;
    PRLabel10: TPRLabel;
    PRLabel11: TPRLabel;
    PRLabel12: TPRLabel;
    PRLabel13: TPRLabel;
    PRLabel14: TPRLabel;
    PRLabel15: TPRLabel;
    PRLabel16: TPRLabel;
    PRLabel17: TPRLabel;
    PRLabel18: TPRLabel;
    PRLabel19: TPRLabel;
    PRLabel20: TPRLabel;
    PRLabel21: TPRLabel;
    PRLabel22: TPRLabel;
    PRLabel23: TPRLabel;
    PRLabel24: TPRLabel;
    PRLabel25: TPRLabel;
    PRLabel26: TPRLabel;
    PRLabel27: TPRLabel;
    Button1: TButton;
    PReport1: TPReport;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    PRPage2: TPRPage;
    PRLayoutPanel2: TPRLayoutPanel;
    PRLabel28: TPRLabel;
    PRLabel29: TPRLabel;
    PRLabel30: TPRLabel;
    PRLabel31: TPRLabel;
    PRLabel32: TPRLabel;
    PRLabel33: TPRLabel;
    PRLabel34: TPRLabel;
    PRLabel35: TPRLabel;
    PRLabel36: TPRLabel;
    PRLabel37: TPRLabel;
    PRLabel38: TPRLabel;
    PRLabel39: TPRLabel;
    PRLabel40: TPRLabel;
    PRLabel41: TPRLabel;
    PRLabel42: TPRLabel;
    PRLabel43: TPRLabel;
    PRLabel44: TPRLabel;
    PRLabel45: TPRLabel;
    PRLabel46: TPRLabel;
    PRLabel47: TPRLabel;
    PRLabel48: TPRLabel;
    PRLabel49: TPRLabel;
    PRLabel50: TPRLabel;
    PRLabel51: TPRLabel;
    PRLabel52: TPRLabel;
    PRLabel53: TPRLabel;
    PRLabel54: TPRLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with PReport1 do
  begin
    BeginDoc;
    PReport1.PageLayout := TPRPageLayout(RadioGroup1.ItemIndex);
    PReport1.PageMode := TPRPageMode(RadioGroup2.ItemIndex);
    Print(PRPage1);
    Print(PRPage2);
    Print(PRPage1);
    Print(PRPage2);
    EndDoc;
  end;
  ShellExecute(Self.Handle, 'Open', 'default.pdf', '', '', SW_SHOW);
end;

end.
