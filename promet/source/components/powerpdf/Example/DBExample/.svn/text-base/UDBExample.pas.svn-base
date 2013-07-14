unit UDBExample;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PReport, ExtCtrls, PdfDoc, Menus, ComCtrls, Db, DBTables, PdfTypes;

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
    SaveDialog1: TSaveDialog;
    PRPage1: TPRPage;
    PRLayoutPanel1: TPRLayoutPanel;
    PRGridPanel1: TPRGridPanel;
    PRText1: TPRText;
    Table1: TTable;
    PRLayoutPanel2: TPRLayoutPanel;
    PRText2: TPRText;
    PRRect1: TPRRect;
    PRText3: TPRText;
    PRText4: TPRText;
    PRText5: TPRText;
    PRText6: TPRText;
    TxtCustNo: TPRText;
    TxtCompany: TPRText;
    TxtAddr: TPRText;
    TxtCity: TPRText;
    TxtState: TPRText;
    PRRect2: TPRRect;
    Table1CustNo: TFloatField;
    Table1Company: TStringField;
    Table1Addr1: TStringField;
    Table1City: TStringField;
    Table1State: TStringField;
    procedure CreatePDF1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PRGridPanel1BeforePrintChild(Sender: TObject;
      ACanvas: TPRCanvas; ACol, ARow: Integer; Rect: TRect);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CreatePDF1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    TxtCustNo.Printable := true;
    TxtCompany.Printable := true;
    TxtAddr.Printable := true;
    TxtCity.Printable := true;
    TxtState.Printable := true;
    try
      with PReport1 do
      begin
        FileName := SaveDialog1.FileName;

        // starting printing document.
        BeginDoc;

        Table1.Open;
        while not Table1.Eof do
          Print(PRPage1);

        // save document.
        EndDoc;
        Table1.Close;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PRPage1.Visible := false;
end;

procedure TForm1.PRGridPanel1BeforePrintChild(Sender: TObject;
  ACanvas: TPRCanvas; ACol, ARow: Integer; Rect: TRect);
begin
  with Table1 do
    if not Table1.Eof then
    begin
      // setting text from current record.
      TxtCustNo.Text := Table1CustNo.AsString;
      TxtCompany.Text := Table1Company.AsString;
      TxtAddr.Text := Table1Addr1.AsString;
      TxtCity.Text := Table1City.AsString;
      TxtState.Text := Table1State.AsString;

      // move next current record.
      Table1.Next;
    end
    else
    begin
      TxtCustNo.Printable := false;
      TxtCompany.Printable := false;
      TxtAddr.Printable := false;
      TxtCity.Printable := false;
      TxtState.Printable := false;
    end;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage(POWER_PDF_VERSION_STR + #13#10 + POWER_PDF_COPYRIGHT);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
