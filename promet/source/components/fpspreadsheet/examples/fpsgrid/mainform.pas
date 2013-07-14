unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, fpspreadsheetgrid, fpspreadsheet;

type

  { TForm1 }

  TForm1 = class(TForm)
    buttonPopulateGrid: TButton;
    sWorksheetGrid1: TsWorksheetGrid;
    procedure buttonPopulateGridClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.buttonPopulateGridClick(Sender: TObject);
var
  lWorksheet: TsWorksheet;
begin
  lWorksheet := TsWorksheet.Create;
  try
    lWorksheet.WriteUTF8Text(2, 2, 'Algo');
    sWorksheetGrid1.LoadFromWorksheet(lWorksheet);
  finally
    lWorksheet.Free;
  end;
end;

initialization
  {$I mainform.lrs}

end.

