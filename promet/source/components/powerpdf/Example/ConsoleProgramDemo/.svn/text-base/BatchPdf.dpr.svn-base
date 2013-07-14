{*
 *
 *  BatchPdf.dpr
 *
 *  To run this program, open command-prompt window and run this program with
 *  filename parameter
 *
 *  ex) BatchPdf.exe batchpdf.pdf
 *}

program BatchPdf;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Classes,
  PdfDoc,
  PdfTypes,
  PdfFonts,
  DB,
  DBTables;

var
  FDoc: TPdfDoc;
  FFileName: string;
  FOutFile: TFileStream;
  FPage: integer;
  FQuery: TQuery;

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

  procedure WriteRow(YPos: Single);
  var
    i: integer;
    s: string;
  begin
    // printing the detail lines
    with FDoc.Canvas do
    begin
      if not FQuery.Eof then
      begin
        TextOut(95, YPos - 15, FQuery.FieldByName('CustNo').AsString);

        s := FQuery.FieldByName('Company').AsString;

        // calculate the number of the charactor which can be contained in the
        // width of the frame.
        i := MeasureText(s, 130);
        TextOut(135, YPos - 15, Copy(s, 1, i));

        s := FQuery.FieldByName('State').AsString +
             FQuery.FieldByName('City').AsString +
             FQuery.FieldByName('Addr1').AsString;

        i := MeasureText(s, 175);
        TextOut(275, YPos - 15, Copy(s, 1, i));

        TextOut(455, YPos - 15, FQuery.FieldByName('Phone').AsString);

        FQuery.Next;
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

  // create TQuery object and set properties.
  FQuery := TQuery.Create(nil);
  with FQuery do
  try
    Writeln('BatchPdf opening database..');
    DatabaseName := 'DBDEMOS';
    SQL.Text := 'SELECT CustNo, Company, State, City, Addr1, Phone from Customer';
    Open;

    // create TPdfDoc object.
    Writeln('BatchPdf creating document..');
    FDoc := TPdfDoc.Create;
    with FDoc do
    try
      // create a new document.
      NewDoc;

      // printind page from the result of the query.
      while not FQuery.Eof do
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

    Close;
    Writeln('BatchPdf end..');
  finally
    Free;
    FOutFile.Free;
  end;

end.
