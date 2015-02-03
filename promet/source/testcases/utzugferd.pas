unit utzugferd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,uzugferd,uOrder,uData;

type

  { TZugferdTest }

  TZugferdTest= class(TTestCase)
  published
    procedure DocumentExists;
    procedure CreateOrder;
    procedure ImportDocument;
    procedure CheckDocument;
    procedure DestroyOrder;
  end;

implementation

var
  aOrder: TOrder;

{ TZugferdTest }

procedure TZugferdTest.DocumentExists;
begin
  Check(FileExists('../../ZUGFeRD-invoice.xml'),'Invoice not existent');
end;

procedure TZugferdTest.CreateOrder;
begin
  aOrder := TOrder.Create(nil);
end;

procedure TZugferdTest.ImportDocument;
begin
  Check(ImportZugferdInvoice(aOrder,'../../ZUGFeRD-invoice.xml'),'Import failed');
end;

procedure TZugferdTest.CheckDocument;
begin
end;

procedure TZugferdTest.DestroyOrder;
begin
  aOrder.Free;
end;

initialization

  RegisterTest(TZugferdTest);
end.

