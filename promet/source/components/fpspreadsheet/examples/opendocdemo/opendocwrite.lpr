{
opendocwrite.dpr

Demonstrates how to write an OpenDocument file using the fpspreadsheet library

AUTHORS: Felipe Monteiro de Carvalho
}
program opendocwrite;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, fpspreadsheet, fpsallformats,
  laz_fpspreadsheet;

var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  MyDir: string;
begin
  MyDir := ExtractFilePath(ParamStr(0));

  // Create the spreadsheet
  MyWorkbook := TsWorkbook.Create;
  MyWorksheet := MyWorkbook.AddWorksheet('My Worksheet');

  // Write some cells
  MyWorksheet.WriteNumber(0, 0, 1.0);// A1
  MyWorksheet.WriteNumber(0, 1, 2.0);// B1
  MyWorksheet.WriteNumber(0, 2, 3.0);// C1
  MyWorksheet.WriteNumber(0, 3, 4.0);// D1
  MyWorksheet.WriteUTF8Text(4, 2, 'Total:');// C5
  MyWorksheet.WriteNumber(4, 3, 10);        // D5
  // Add some formatting
  MyWorksheet.WriteUsedFormatting(0, 0, [uffBold]);

  // Creates a new worksheet
  MyWorksheet := MyWorkbook.AddWorksheet('My Worksheet 2');

  // Save the spreadsheet to a file
  MyWorkbook.WriteToFile(MyDir + 'test.ods',
    sfOpenDocument);
  MyWorkbook.Free;
end.

