{
test_write_formula.pas

Demonstrates how to write an formula using the fpspreadsheet library

AUTHORS: Felipe Monteiro de Carvalho
}
program test_write_formula;

{$mode delphi}{$H+}

uses
  Classes, SysUtils,
  fpspreadsheet, xlsbiff5, xlsbiff8, fpsopendocument,
  laz_fpspreadsheet, fpsconvencoding;

var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  MyDir: string;
  MyCell: PCell;

procedure WriteFirstWorksheet();
var
  MyFormula: TsFormula;
  MyRPNFormula: TsRPNFormula;
begin
  MyWorksheet := MyWorkbook.AddWorksheet('Worksheet1');

  // Write some cells
  MyWorksheet.WriteUTF8Text(0, 1, 'Text Formula');// B1
  MyWorksheet.WriteUTF8Text(0, 2, 'RPN');// C1

  // =Sum(E2:e5)
  MyWorksheet.WriteUTF8Text(1, 0, '=Sum(E2:e5)'); // A2
  //
  MyFormula.FormulaStr := '=Sum(DE:e5)';
  MyFormula.DoubleValue := 0.0;
  MyWorksheet.WriteFormula(1, 1, MyFormula);    // B2
  //
  SetLength(MyRPNFormula, 2);
  MyRPNFormula[0].ElementKind := fekCellRange;
  MyRPNFormula[0].Row := 1;
  MyRPNFormula[0].Row2 := 4;
  MyRPNFormula[0].Col := 4;
  MyRPNFormula[0].Col2 := 4;
  MyRPNFormula[1].ElementKind := fekOpSUM;
  MyWorksheet.WriteRPNFormula(1, 2, MyRPNFormula);   // C2

  // Write the formula =ABS(E1)
  MyWorksheet.WriteUTF8Text(2, 0, '=ABS(E1)'); // A3
  //
  SetLength(MyRPNFormula, 2);
  MyRPNFormula[0].ElementKind := fekCell;
  MyRPNFormula[0].Col := 4;
  MyRPNFormula[0].Row := 0;
  MyRPNFormula[1].ElementKind := fekABS;
  MyWorksheet.WriteRPNFormula(2, 2, MyRPNFormula);

  // Write the formula =4+5
  MyWorksheet.WriteUTF8Text(3, 0, '=4+5'); // A4
  //
  SetLength(MyRPNFormula, 3);
  MyRPNFormula[0].ElementKind := fekNum;
  MyRPNFormula[0].DoubleValue := 4.0;
  MyRPNFormula[1].ElementKind := fekNum;
  MyRPNFormula[1].DoubleValue := 5.0;
  MyRPNFormula[2].ElementKind := fekAdd;
  MyWorksheet.WriteRPNFormula(3, 2, MyRPNFormula);
end;

procedure WriteSecondWorksheet();
begin
{  MyWorksheet := MyWorkbook.AddWorksheet('Worksheet2');

  // Write some cells

  // Line 1

  MyWorksheet.WriteUTF8Text(1, 1, 'Relatório');
  MyCell := MyWorksheet.GetCell(1, 1);
  MyCell^.Border := [cbNorth, cbWest, cbSouth];
  MyCell^.BackgroundColor := scGrey20pct;
  MyCell^.UsedFormattingFields := [uffBorder, uffBackgroundColor, uffBold];}
end;

begin
  MyDir := ExtractFilePath(ParamStr(0));

  // Create the spreadsheet
  MyWorkbook := TsWorkbook.Create;

  WriteFirstWorksheet();

  WriteSecondWorksheet();

  // Save the spreadsheet to a file
  MyWorkbook.WriteToFile(MyDir + 'test_formula.xls', sfExcel8, False);
//  MyWorkbook.WriteToFile(MyDir + 'test_formula.odt', sfOpenDocument, False);
  MyWorkbook.Free;
end.

