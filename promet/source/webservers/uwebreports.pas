unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_DBSet, LR_BarC, LR_RRect, LR_Shape,
  LR_E_TXT, LR_E_HTM, lr_e_pdf;

type

  { TfWebReports }

  TfWebReports = class(TDataModule)
    frBarCodeObject1: TfrBarCodeObject;
    frHTMExport1: TfrHTMExport;
    frReport1: TfrReport;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    frTextExport1: TfrTextExport;
    frTNPDFExport1: TfrTNPDFExport;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fWebReports: TfWebReports;

implementation

{$R *.lfm}

end.

