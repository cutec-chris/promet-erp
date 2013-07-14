{
test_write_formatting.pas

Demonstrates how to write an Excel 8+ file using the fpspreadsheet library

Adds formatting to the file

AUTHORS: Felipe Monteiro de Carvalho
}
program test_write_formatting;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, fpspreadsheet, xlsbiff8, fpsopendocument,
  laz_fpspreadsheet, fpsconvencoding;

var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  MyDir: string;
  MyCell: PCell;

procedure WriteFirstWorksheet();
begin
  MyWorksheet := MyWorkbook.AddWorksheet('Worksheet1');

  // Write some cells
  MyWorksheet.WriteUTF8Text(1, 0, 'Border');// A2

  MyWorksheet.WriteUTF8Text(1, 1, '[]');    // B2
  MyCell := MyWorksheet.GetCell(1, 1);
  MyCell^.Border := [];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(1, 3, '[N]');// D2
  MyCell := MyWorksheet.GetCell(1, 3);
  MyCell^.Border := [cbNorth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(1, 5, '[W]');// F2
  MyCell := MyWorksheet.GetCell(1, 5);
  MyCell^.Border := [cbWest];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(1, 7, '[E]');// H2
  MyCell := MyWorksheet.GetCell(1, 7);
  MyCell^.Border := [cbEast];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(1, 9, '[S]');// J2
  MyCell := MyWorksheet.GetCell(1, 9);
  MyCell^.Border := [cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(3, 1, '[N,W]');// B4
  MyCell := MyWorksheet.GetCell(3, 1);
  MyCell^.Border := [cbNorth, cbWest];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(3, 3, '[N,E]');// D4
  MyCell := MyWorksheet.GetCell(3, 3);
  MyCell^.Border := [cbNorth, cbEast];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(3, 5, '[N,S]');// F4
  MyCell := MyWorksheet.GetCell(3, 5);
  MyCell^.Border := [cbNorth, cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(3, 7, '[W,E]');// H4
  MyCell := MyWorksheet.GetCell(3, 7);
  MyCell^.Border := [cbWest, cbEast];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(3, 9, '[W,S]');// J4
  MyCell := MyWorksheet.GetCell(3, 9);
  MyCell^.Border := [cbWest, cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(3, 11, '[E,S]');// L4
  MyCell := MyWorksheet.GetCell(3, 11);
  MyCell^.Border := [cbEast, cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(5, 1, '[N,W,E]');// B6
  MyCell := MyWorksheet.GetCell(5, 1);
  MyCell^.Border := [cbNorth, cbWest, cbEast];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(5, 3, '[N,W,S]');// D6
  MyCell := MyWorksheet.GetCell(5, 3);
  MyCell^.Border := [cbNorth, cbWest, cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(5, 5, '[N,E,S]');// F6
  MyCell := MyWorksheet.GetCell(5, 5);
  MyCell^.Border := [cbNorth, cbEast, cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(5, 7, '[W,E,S]');// H6
  MyCell := MyWorksheet.GetCell(5, 7);
  MyCell^.Border := [cbWest, cbEast, cbSouth];
  MyCell^.UsedFormattingFields := [uffBorder];

  MyWorksheet.WriteUTF8Text(5, 9, '[N,W,E,S]');// J6
  MyCell := MyWorksheet.GetCell(5, 9);
  MyCell^.Border := [cbNorth, cbWest, cbEast, cbSouth];
  MyCell^.BackgroundColor := scGreen;
  MyCell^.UsedFormattingFields := [uffBorder, uffBold, uffBackgroundColor];
end;

procedure WriteSecondWorksheet();
begin
  MyWorksheet := MyWorkbook.AddWorksheet('Worksheet2');

  // Write some cells

  // Line 1

  MyWorksheet.WriteUTF8Text(1, 1, 'Relatório');
  MyCell := MyWorksheet.GetCell(1, 1);
  MyCell^.Border := [cbNorth, cbWest, cbSouth];
  MyCell^.BackgroundColor := scGrey20pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor, uffBold];

  MyWorksheet.WriteUTF8Text(1, 2, ' ');
  MyCell := MyWorksheet.GetCell(1, 2);
  MyCell^.Border := [cbNorth, cbEast, cbSouth];
  MyCell^.BackgroundColor := scGrey20pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor];

  // Line 2

  MyWorksheet.WriteUTF8Text(2, 1, 'Compras');
  MyCell := MyWorksheet.GetCell(2, 1);
  MyCell^.Border := [cbWest];
  MyCell^.BackgroundColor := scGrey10pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor];

  MyWorksheet.WriteUTF8Text(2, 2, 'R$ 20');
  MyCell := MyWorksheet.GetCell(2, 2);
  MyCell^.Border := [cbEast];
  MyCell^.BackgroundColor := scGrey10pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor];

  // Line 3

  MyWorksheet.WriteUTF8Text(3, 1, 'Total:');
  MyCell := MyWorksheet.GetCell(3, 1);
  MyCell^.Border := [cbWest, cbSouth];
  MyCell^.BackgroundColor := scGrey10pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor];

  MyWorksheet.WriteUTF8Text(3, 2, 'R$ 20');
  MyCell := MyWorksheet.GetCell(3, 2);
  MyCell^.Border := [cbEast, cbSouth];
  MyCell^.BackgroundColor := scGrey10pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor];
end;

begin
  MyDir := ExtractFilePath(ParamStr(0));

  // Create the spreadsheet
  MyWorkbook := TsWorkbook.Create;

  WriteFirstWorksheet();

  WriteSecondWorksheet();

  // Save the spreadsheet to a file
//  MyWorkbook.WriteToFile(MyDir + 'test3.xls', sfExcel8, False);
  MyWorkbook.WriteToFile(MyDir + 'test3.odt', sfOpenDocument, False);
  MyWorkbook.Free;
end.

