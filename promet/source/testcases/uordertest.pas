unit uordertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,uOrder,uBaseERPDBClasses;

type

  { OrderTest }

  OrderTest= class(TTestCase)
  published
    procedure CreateOrder;
    procedure FillPosition1;
    procedure CheckVatCalculation;
    procedure CheckQuantityCalculation;
    procedure FillPosition2;
    procedure CheckSumCalculation;
    procedure CheckPost;
    procedure ConvertAU;
    procedure ConvertLI;
    procedure Free;
  end;

implementation
uses uData;
var
  aOrder : TOrder;
procedure OrderTest.CreateOrder;
begin
  aOrder := TOrder.Create(nil,Data);
  aOrder.Insert;
end;

procedure OrderTest.FillPosition1;
begin
  aOrder.Positions.Insert;
  aOrder.Positions.FieldByName('SHORTTEXT').AsString:='Position 1';
  Check(aOrder.Positions.FieldByName('POSNO').AsString = '1');
end;

procedure OrderTest.CheckVatCalculation;
begin
  aOrder.Positions.FieldByName('SELLPRICE').AsString:='5';
  Check(aOrder.Positions.FieldByName('GROSSPRICE').AsString = StringReplace('5.95','.',DecimalSeparator,[rfReplaceAll]),'MwSt from 5 is:'+aOrder.Positions.FieldByName('GROSSPRICE').AsString);
end;

procedure OrderTest.CheckQuantityCalculation;
begin
  aOrder.Positions.FieldByName('QUANTITY').AsString:='5';
  Check(aOrder.Positions.FieldByName('POSPRICE').AsFloat = 25);
end;

procedure OrderTest.FillPosition2;
begin
  aOrder.Positions.Insert;
  aOrder.Positions.FieldByName('SHORTTEXT').AsString:='Position 2';
  Check(aOrder.Positions.FieldByName('POSNO').AsString = '2');
  aOrder.Positions.FieldByName('SELLPRICE').AsString:='5';
  aOrder.Positions.FieldByName('QUANTITY').AsString:='2';
end;

procedure OrderTest.CheckSumCalculation;
begin
  aOrder.DataSet.Post;
  Check(aOrder.FieldByName('NETPRICE').AsFloat = 35);
  aOrder.CascadicPost;
end;

procedure OrderTest.CheckPost;
begin
  Check(aOrder.Post=prSuccess,'Order post failed');
end;

procedure OrderTest.ConvertAU;
begin
  aOrder.ChangeStatus('AU');
  Check(aOrder.Count=2,'Order Count='+IntToStr(aOrder.Count));
  Check(aOrder.Post=prSuccess,'Order post failed');
end;

procedure OrderTest.ConvertLI;
begin
  aOrder.ChangeStatus('LI');
  Check(aOrder.Count=3,'Order Count='+IntToStr(aOrder.Count));
  Check(aOrder.Post=prSuccess,'Order post failed');
end;

procedure OrderTest.Free;
begin
  aOrder.Free;
end;



initialization

  RegisterTest(OrderTest);
end.

