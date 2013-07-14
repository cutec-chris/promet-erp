{*
 *
 *  BatchPdf.dpr
 *
 *  To run this program, open terminal window and run this program with
 *  filename parameter
 *
 *  ex) ./BatchPdf batchpdf.pdf
 *}

program BatchPdf;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Classes,
  PdfDoc in '../../PdfDoc.pas',
  PdfFonts in '../../PdfFonts.pas',
  PdfTypes in '../../PdfTypes.pas';

type
  TCustRecord = record
    CustNo: string;
    Company: string;
    Addr: string;
    Phone: string;
  end;

const
  CUSTFILE_NAME = 'Customer.dat';

var
  FDoc: TPdfDoc;
  FFileName: string;
  FOutFile: TFileStream;
  FPage: integer;
  FFile: TextFile;

  procedure TextOut(X, Y: Single; S: string);
  begin
    with FDoc.Canvas do
    begin
      BeginText;
      MoveTextPoint(X, Y);
      ShowText(S);
      EndText;
    end;
  end;

  procedure DrawLine(X1, Y1, X2, Y2, Width: Single);
  begin
    with FDoc.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
      Stroke;
    end;
  end;

  procedure WriteHeader;
  var
    s: string;
    w: integer;
  begin
    // writing the headline of the pages
    with FDoc.Canvas do
    begin
      // setting font
      SetFont('Arial-BoldItalic', 16);

      // printing text.
      TextOut(90, 770, 'Customer.DB');

      SetFont('Arial-BoldItalic', 9);
      S := FormatDateTime('YYYY/MM/DD', Date);
      w := Round(TextWidth(S));

      // writing header text.
      TextOut(530 - w, 770, S);

      SetRGBStrokeColor($00008800);
      DrawLine(90, 765, 530, 765, 1.5);
    end;
  end;

  procedure WriteFooter;
  var
    w: Single;
    s: string;
  begin
    with FDoc.Canvas do
    begin
      // Setting font and print text with center align
      SetFont('Times-Roman', 8);

      DrawLine(90, 70, 530, 70, 1.5);

      s := 'Page ' + IntToStr(FPage);
      w := TextWidth(s);

      TextOut((PageWidth - w) / 2, 55, S);
    end;
  end;

  function StringToCustRecord(s: string): TCustRecord;
  var
    FPos1, FPos2: integer;
  begin
    // initializing TCustRecord.
    with Result do
    begin
      CustNo := '';
      Company := '';
      Addr := '';
      Phone := '';
    end;

    // makeing TCustRecord from string(each element is separated by #09 charactar).
    FPos1 := Pos(#09, s);
    if FPos1 > 0 then
    begin
      Result.CustNo := Copy(S, 1, FPos1-1);
      s[FPos1] := ' ';
    end;
    FPos2 := Pos(#09, s);
    if FPos2 > 0 then
    begin
      Result.Company := Copy(S, FPos1+1, FPos2-FPos1-1);
      s[FPos2] := ' ';
    end;
    FPos1 := Pos(#09, s);
    if FPos1 > 0 then
    begin
      Result.Addr := Copy(S, FPos2+1, FPos1-FPos2-1);
      s[FPos1] := ' ';
    end;
    Result.Phone := Copy(S, FPos1+1, Length(s) - FPos1);
  end;

  procedure WriteRow(YPos: Single);
  var
    i: integer;
    s: string;
    CustRecord: TCustRecord;
  begin
    // printing the detail lines
    with FDoc.Canvas do
    begin
      if not EOF(FFile) then
      begin
        Readln(FFile, s);
        CustRecord := StringToCustRecord(s);

        TextOut(95, YPos - 15, CustRecord.CustNo);

        i := MeasureText(CustRecord.Company, 130);
        TextOut(135, YPos - 15, Copy(CustRecord.Company, 1, i));

        i := MeasureText(CustRecord.Addr, 175);
        TextOut(275, YPos - 15, Copy(CustRecord.Addr, 1, i));

        TextOut(455, YPos - 15, CustRecord.Phone);
      end;
    end;
  end;

  procedure WritePage;
  var
    i: integer;
    XPos, YPos: Single;
  begin
    with FDoc.Canvas do
    begin
      // writing the outline
      SetLineWidth(1.5);
      Rectangle(90, 80, 440, 680);
      Stroke;

      // writing the horizontal lines.
      YPos := 760;
      SetLineWidth(0.75);
      for i := 0 to 32 do
      begin
        YPos := YPos - 20;
        MoveTo(90, YPos);
        LineTo(530, YPos);
        Stroke;
      end;

      // writing the header of the text.
      SetLineWidth(1);
      SetFont('Times-Roman', 10.5);

      XPos := 90;
      TextOut(XPos + 5, 745, 'NO.');

      XPos := 130;
      DrawLine(XPos, 760, XPos, 80, 1);
      TextOut(XPos + 5, 745, 'Company');

      XPos := 270;
      DrawLine(XPos, 760, XPos, 80, 1);
      TextOut(XPos + 5, 745, 'Address');

      XPos := 450;
      DrawLine(XPos, 760, XPos, 80, 1);
      TextOut(XPos + 5, 745, 'Phone');

      XPos := 530;
      DrawLine(XPos, 760, XPos, 80, 1);

      // setting the font for the detail lines.
      SetFont('Arial', 10.5);
    end;

    // printing the detail lines with 20 dot height.
    YPos := 740;
    for i := 0 to 32 do
    begin
      WriteRow(YPos);
      YPos := YPos - 20;
    end;
  end;

begin
  if ParamCount > 0 then
    FFileName := ParamStr(1)
  else
  begin
    Writeln('Usage: bachpdf <pdf-filename>');
    Halt(2);
  end;

  // create output-filestream.
  FOutFile := TFileStream.Create(FFileName, fmCreate);

  FPage := 1;

  // open Custmer.dat
  Writeln('opening datafile..');
  AssignFile(FFile, CUSTFILE_NAME);
  Reset(FFile);

  // create TPdfDoc object.
  Writeln('BatchPdf creating document..');
  FDoc := TPdfDoc.Create;
  with FDoc do
  try
    // create a new document.
    NewDoc;

    // printind page from the result of the query.
    while not EOF(FFile) do
    begin
      AddPage;
      WriteHeader;
      WritePage;
      WriteFooter;
      inc(FPage);
    end;

    // save the pdf-document to the file-stream.
    Writeln('BatchPdf saving document..');
    FDoc.SaveToStream(FOutFile);
  finally
    FDoc.Free;
  end;

  FOutFile.Free;
  CloseFile(FFile);
  Writeln('BatchPdf end..');
end.
