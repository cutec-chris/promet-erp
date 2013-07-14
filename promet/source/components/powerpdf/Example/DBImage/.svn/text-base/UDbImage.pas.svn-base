{*
 * << P o w e r P d f >> -- UDbImage.pas
 * << DBImage sample program >>
 *
 * Copyright (c) 1999-2001 Takezou. <takeshi_kanno@est.hi-ho.ne.jp>
 *
 * 2000.09.10 create.
 * 2001.09.08 change .
 *}
unit UDbImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PReport, ExtCtrls, Menus, ComCtrls, Db, DBTables,

  // to use outline, PdfDoc and PdfTypes must be inclueded.
  PdfDoc, PdfTypes;

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
    PRRect1: TPRRect;
    PRRect2: TPRRect;
    PRText2: TPRText;
    PRText3: TPRText;
    PRText4: TPRText;
    PRImage1: TPRImage;
    Table1: TTable;
    Table1Common_Name: TStringField;
    Table1SpeciesName: TStringField;
    Table1Length_In: TFloatField;
    Table1Graphic: TGraphicField;
    PRRect3: TPRRect;
    procedure CreatePDF1Click(Sender: TObject);
    procedure PRPage1PrintPage(Sender: TObject; ACanvas: TPRCanvas);
    procedure FormCreate(Sender: TObject);
    procedure PRGridPanel1BeforePrintChild(Sender: TObject;
      ACanvas: TPRCanvas; ACol, ARow: Integer; Rect: TRect);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    FPage: integer;
    FOutline: TPROutLineEntry;
  public
    { Public }
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
    try
      with PReport1 do
      begin
        FileName := SaveDialog1.FileName;
        BeginDoc;
        FPage := 0;
        Table1.Open;
        while not Table1.Eof do
          Print(PRPage1);
        StatusBar1.Panels[0].Text := 'writing document..';
        StatusBar1.Repaint;
        EndDoc;
        StatusBar1.Panels[0].Text := 'end..';
        Table1.Close;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.PRPage1PrintPage(Sender: TObject; ACanvas: TPRCanvas);
begin
  // creating outlines (the destination set to the top of the page).
  with PReport1 do
  begin
    // if the first page, create open-action. 
    if PageNumber = 1 then
    begin
      OpenAction := CreateDestination;
      with OpenAction do
      begin
        DestinationType := dtXYZ;
        Zoom := 1;
      end;
    end;
    FOutline := OutlineRoot.AddChild;
    FOutline.Dest := CreateDestination;
    FOutline.Dest.Top := 0;
    FOutline.Title := 'Page ' + IntToStr(PReport1.PageNumber);
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
      inc(FPage);
      // setting status bar text.
      StatusBar1.Panels[0].Text := 'creating page ' + IntToStr(FPage);
      StatusBar1.Repaint;

      // setting text from current record.
      PRText2.Text := Table1Common_Name.AsString;
      PRText3.Text := Table1SpeciesName.AsString;
      PRText4.Text := 'Length_In: ' + FormatFloat('##0.#0', Table1Length_In.AsFloat);

      // creating outline entry
      with PReport1 do
      begin
        with FOutline.AddChild do
        begin
          // create destination and set the properties.
          Dest := CreateDestination;
          Dest.Top := Rect.Top;
          Dest.Left := Rect.Left;
          Title := PRText2.Text;
        end;
      end;

      // setting image..
      PRImage1.Picture.Assign(Table1Graphic);
      Table1.Next;
    end
    else
    begin
      PRText2.Text := '';
      PRText3.Text := '';
      PRText4.Text := '';
      PRRect2.Printable := false;
      PRImage1.Printable := false;
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
