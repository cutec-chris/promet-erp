{
excel2write.dpr

Demonstrates how to write an Excel 2.x file using the fpspreadsheet library

AUTHORS: Felipe Monteiro de Carvalho
}
program excel2write;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, fpspreadsheet, xlsbiff2, laz_fpspreadsheet;

var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  MyRPNFormula: TsRPNFormula;
  MyDir: string;
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

  // Write the formula E1 = ABS(A1)
  SetLength(MyRPNFormula, 2);
  MyRPNFormula[0].ElementKind := fekCell;
  MyRPNFormula[0].Col := 0;
  MyRPNFormula[0].Row := 0;
  MyRPNFormula[1].ElementKind := fekABS;
  MyWorksheet.WriteRPNFormula(0, 4, MyRPNFormula);

  // Write the formula F1 = ROUND(A1, 0)
  SetLength(MyRPNFormula, 3);
  MyRPNFormula[0].ElementKind := fekCell;
  MyRPNFormula[0].Col := 0;
  MyRPNFormula[0].Row := 0;
  MyRPNFormula[1].ElementKind := fekNum;
  MyRPNFormula[1].DoubleValue := 0.0;
  MyRPNFormula[2].ElementKind := fekROUND;
  MyWorksheet.WriteRPNFormula(0, 5, MyRPNFormula);

  // Write some string cells
  MyWorksheet.WriteUTF8Text(1, 0, 'First');
  MyWorksheet.WriteUTF8Text(1, 1, 'Second');
  MyWorksheet.WriteUTF8Text(1, 2, 'Third');
  MyWorksheet.WriteUTF8Text(1, 3, 'Fourth');

  // Save the spreadsheet to a file
  MyWorkbook.WriteToFile(MyDir + 'test' + STR_EXCEL_EXTENSION, sfExcel2);
  MyWorkbook.Free;
end.

