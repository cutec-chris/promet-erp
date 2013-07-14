unit utransaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uData, uOrder;

type

  TTransactionTest= class(TTestCase)
  public
  published
    procedure GetConnection;
    procedure StartTransaction;
    procedure CreateOrders;
    procedure Fill1Order;
    procedure Fill2Order;
    procedure GetOrderList;
    procedure SaveOrders;
    procedure Rollback;
    procedure FreeOrders;
    procedure FreeConnection;
  end;

implementation
var
  aTransaction : TComponent;
  bTransaction : TComponent;
  aOrder,
  bOrder : TOrder;
  aOrderList : TOrderList;

procedure TTransactionTest.GetConnection;
begin
  aTransaction := Data.GetNewConnection;
  bTransaction := Data.GetNewConnection;
end;

procedure TTransactionTest.StartTransaction;
begin
  Data.StartTransaction(aTransaction);
  Data.StartTransaction(bTransaction);
end;

procedure TTransactionTest.CreateOrders;
begin
  aOrder := TOrder.Create(nil,Data,aTransaction);
  bOrder := TOrder.Create(nil,Data,bTransaction);
end;

procedure TTransactionTest.Fill1Order;
begin
  aOrder.Insert;
  aOrder.Positions.Insert;
  aOrder.Positions.Ident.AsString:='TEST';
end;

procedure TTransactionTest.Fill2Order;
begin
  bOrder.Insert;
  bOrder.Positions.Insert;
  bOrder.Positions.Ident.AsString:='TEST2';
end;

procedure TTransactionTest.GetOrderList;//Blocking Test
begin
  aOrderList := TOrderList.Create(nil,Data);
  aOrderList.Open;
end;

procedure TTransactionTest.SaveOrders;
begin
  aOrder.Positions.DataSet.Post;
  aOrder.DataSet.Post;
//  bOrder.Positions.DataSet.Post;
//  bOrder.DataSet.Post;
end;

procedure TTransactionTest.Rollback;
begin
  Data.Rollback(aTransaction);
  Data.Rollback(bTransaction);
end;

procedure TTransactionTest.FreeOrders;
begin
  aOrder.Free;
  bOrder.Free;
  aOrderList.Free;
end;

procedure TTransactionTest.FreeConnection;
begin
  aTransaction.Free;
  bTransaction.Free;
end;

initialization

  RegisterTest(TTransactionTest);
end.
