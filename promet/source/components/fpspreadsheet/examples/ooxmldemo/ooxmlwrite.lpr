{
ooxmlwrite.dpr

Demonstrates how to write an OOXML file using the fpspreadsheet library

AUTHORS: Felipe Monteiro de Carvalho
}
program ooxmlwrite;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, fpspreadsheet, fpsallformats, laz_fpspreadsheet;

var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  MyDir: string;
  i: Integer;
  a: TStringList;
  MyCell: PCell;
begin
  // Open the output file
  MyDir := ExtractFilePath(ParamStr(0));

  // Create the spreadsheet
  MyWorkbook := TsWorkbook.Create;
  MyWorksheet := MyWorkbook.AddWorksheet('My Worksheet');

  // Write some number cells
  MyWorksheet.WriteNumber(0, 0, 1.0);
  MyWorksheet.WriteNumber(0, 1, 2.0);
  MyWorksheet.WriteNumber(0, 2, 3.0);
  MyWorksheet.WriteNumber(0, 3, 4.0);

// Uncommend this to test large XLS files
  for i := 2 to 2{20} do
  begin
    MyWorksheet.WriteUTF8Text(i, 0, ParamStr(0));
    MyWorksheet.WriteUTF8Text(i, 1, ParamStr(0));
    MyWorksheet.WriteUTF8Text(i, 2, ParamStr(0));
    MyWorksheet.WriteUTF8Text(i, 3, ParamStr(0));
  end;

  // Test for Bold
  MyCell := MyWorksheet.GetCell(2, 0);
  MyCell^.UsedFormattingFields := [uffBold];
  MyCell := MyWorksheet.GetCell(2, 1);
  MyCell^.UsedFormattingFields := [uffBold];
  MyCell := MyWorksheet.GetCell(2, 2);
  MyCell^.UsedFormattingFields := [uffBold];
  MyCell := MyWorksheet.GetCell(2, 3);
  MyCell^.UsedFormattingFields := [uffBold];

  // Creates a new worksheet
  MyWorksheet := MyWorkbook.AddWorksheet('My Worksheet 2');

  // Write some string cells
  MyWorksheet.WriteUTF8Text(0, 0, 'First');
  MyWorksheet.WriteUTF8Text(0, 1, 'Second');
  MyWorksheet.WriteUTF8Text(0, 2, 'Third');
  MyWorksheet.WriteUTF8Text(0, 3, 'Fourth');

  // Save the spreadsheet to a file
  MyWorkbook.WriteToFile(MyDir + 'test.xlsx', sfOOXML);
  MyWorkbook.Free;
end.

