unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PReport, PdfDoc, ComCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    PRPage1: TPRPage;
    PReport1: TPReport;
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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EdtLeft: TEdit;
    UpDown1: TUpDown;
    EdtTop: TEdit;
    UpDown2: TUpDown;
    EdtRight: TEdit;
    UpDown3: TUpDown;
    EdtBottom: TEdit;
    UpDown4: TUpDown;
    EdtZoom: TEdit;
    UpDown5: TUpDown;
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
    procedure Button1Click(Sender: TObject);
    procedure PRPage1PrintPage(Sender: TObject; ACanvas: TPRCanvas);
    procedure RadioGroup1Click(Sender: TObject);
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
    Print(PRPage1);
    EndDoc;
  end;
  ShellExecute(Self.Handle, 'Open', 'default.pdf', '', '', SW_SHOW);
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
    DestinationType := TPRDestinationType(RadioGroup1.ItemIndex);
    Left := StrToInt(EdtLeft.Text);
    Top := StrToInt(EdtTop.Text);
    Right := StrToInt(EdtRight.Text);
    Bottom := StrToInt(EdtBottom.Text);
    Zoom := StrToInt(EdtZoom.Text) / 100;
  end;

  // set the destination object as the open-action.
  PReport1.OpenAction := Dest;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: begin
         EdtTop.Enabled := true;
         EdtLeft.Enabled := true;
         EdtZoom.Enabled := true;
         EdtRight.Enabled := false;
         EdtBottom.Enabled := false;
       end;
    1, 5:
       begin
         EdtTop.Enabled := false;
         EdtLeft.Enabled := false;
         EdtZoom.Enabled := false;
         EdtRight.Enabled := false;
         EdtBottom.Enabled := false;
       end;
    2, 6:
       begin
         EdtTop.Enabled := true;
         EdtLeft.Enabled := false;
         EdtZoom.Enabled := false;
         EdtRight.Enabled := false;
         EdtBottom.Enabled := false;
       end;
    3, 7:
       begin
         EdtTop.Enabled := false;
         EdtLeft.Enabled := true;
         EdtZoom.Enabled := false;
         EdtRight.Enabled := false;
         EdtBottom.Enabled := false;
       end;
    4:
       begin
         EdtTop.Enabled := true;
         EdtLeft.Enabled := true;
         EdtZoom.Enabled := false;
         EdtRight.Enabled := true;
         EdtBottom.Enabled := true;
       end;
  end;

end;

end.
