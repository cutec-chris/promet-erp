{
excel8write.dpr

Demonstrates how to write an Excel 8+ file using the fpspreadsheet library

AUTHORS: Felipe Monteiro de Carvalho
}
program excel8write;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, fpspreadsheet, xlsbiff8,
  laz_fpspreadsheet;

const
  Str_First = 'First';
  Str_Second = 'Second';
  Str_Third = 'Third';
  Str_Fourth = 'Fourth';
  Str_Worksheet1 = 'Meu Relatório';
  Str_Worksheet2 = 'My Worksheet 2';
  Str_Total = 'Total:';
var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  MyRPNFormula: TsRPNFormula;
  MyDir: string;
  i: Integer;
  lCell: PCell;
begin
  MyDir := ExtractFilePath(ParamStr(0));

  // Create the spreadsheet
  MyWorkbook := TsWorkbook.Create;
  MyWorksheet := MyWorkbook.AddWorksheet(Str_Worksheet1);

  // Write some cells
  MyWorksheet.WriteNumber(0, 0, 1.0);// A1
  MyWorksheet.WriteNumber(0, 1, 2.0);// B1
  MyWorksheet.WriteNumber(0, 2, 3.0);// C1
  MyWorksheet.WriteNumber(0, 3, 4.0);// D1
  MyWorksheet.WriteUTF8Text(4, 2, Str_Total);// C5
  MyWorksheet.WriteNumber(4, 3, 10);        // D5

  // D6 number with background color
  MyWorksheet.WriteNumber(5, 3, 10);
  lCell := MyWorksheet.GetCell(5,3);
  lCell^.BackgroundColor := scPURPLE;
  lCell^.UsedFormattingFields := [uffBackgroundColor];

{ Uncommend this to test large XLS files
  for i := 2 to 20 do
  begin
    MyWorksheet.WriteAnsiText(i, 0, ParamStr(0));
    MyWorksheet.WriteAnsiText(i, 1, ParamStr(0));
    MyWorksheet.WriteAnsiText(i, 2, ParamStr(0));
    MyWorksheet.WriteAnsiText(i, 3, ParamStr(0));
  end;
}

  // Write the formula E1 = A1 + B1
  SetLength(MyRPNFormula, 3);
  MyRPNFormula[0].ElementKind := fekCell;
  MyRPNFormula[0].Col := 0;
  MyRPNFormula[0].Row := 0;
  MyRPNFormula[1].ElementKind := fekCell;
  MyRPNFormula[1].Col := 1;
  MyRPNFormula[1].Row := 0;
  MyRPNFormula[2].ElementKind := fekAdd;
  MyWorksheet.WriteRPNFormula(0, 4, MyRPNFormula);

  // Write the formula F1 = ABS(A1)
  SetLength(MyRPNFormula, 2);
  MyRPNFormula[0].ElementKind := fekCell;
  MyRPNFormula[0].Col := 0;
  MyRPNFormula[0].Row := 0;
  MyRPNFormula[1].ElementKind := fekABS;
  MyWorksheet.WriteRPNFormula(0, 5, MyRPNFormula);

  //MyFormula.FormulaStr := '';

  // Creates a new worksheet
  MyWorksheet := MyWorkbook.AddWorksheet(Str_Worksheet2);

  // Write some string cells
  MyWorksheet.WriteUTF8Text(0, 0, Str_First);
  MyWorksheet.WriteUTF8Text(0, 1, Str_Second);
  MyWorksheet.WriteUTF8Text(0, 2, Str_Third);
  MyWorksheet.WriteUTF8Text(0, 3, Str_Fourth);
  MyWorksheet.WriteTextRotation(0, 0, rt90DegreeClockwiseRotation);
  MyWorksheet.WriteUsedFormatting(0, 1, [uffBold]);

  // Save the spreadsheet to a file
  MyWorkbook.WriteToFile(MyDir + 'test2.xls', sfExcel8, False);
  MyWorkbook.Free;
end.

