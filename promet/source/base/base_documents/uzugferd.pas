{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 25.07.2014
*******************************************************************************}
unit uzugferd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uOrder,XMLRead,DOM,uBaseDbClasses;

function ImportZugferdInvoice(aOrder : TOrder;aFile : string) : Boolean;

implementation

function ImportZugferdInvoice(aOrder: TOrder; aFile: string): Boolean;
var
  Doc: TXMLDocument;
  DocumentNode: TDOMNode;
  aTmp: TDOMNode;
  aTmpS: DOMString;
  i: Integer;
  a: Integer;

  procedure CombineFields(aData : TBaseDbDataSet;aNode : TDOMNode;aZField : string;aDBField : string);
  var
    aTmp: TDOMNode;
  begin
    aTmp := aNode.FindNode(aZField);
    if Assigned(aTmp) and Assigned(aOrder.FieldByName(aDBField)) then
      aData.FieldByName(aDBField).AsString:=aTmp.FirstChild.NodeValue;
  end;
  procedure CombineDateFields(aData : TBaseDbDataSet;aNode : TDOMNode;aZField : string;aDBField : string);
  var
    aTmp: TDOMElement;
    aVal: DOMString;
  begin
    aTmp := TDOMElement(DocumentNode.FindNode(aZField));
    if Assigned(aTmp) and Assigned(aOrder.FieldByName(aDBField)) then
      begin
        aVal := aTmp.FirstChild.NodeValue;
        if aTmp.GetAttribute('format')='102' then
          aData.FieldByName(aDBField).AsDateTime:=EncodeDate(StrToInt(copy(aVal,0,4)),StrToInt(copy(aVal,5,2)),StrToInt(copy(aVal,7,2)));
      end;
  end;

begin
  ReadXMLFile(Doc, aFile);
  DocumentNode := Doc.DocumentElement.FindNode('rsm:HeaderExchangedDocument');
  if not aOrder.Active then
    aOrder.Insert
  else
    aOrder.Edit;
  CombineFields(aOrder,DocumentNode,'ID','COMMISSION');
  CombineDateFields(aOrder,DocumentNode,'IssueDateTime','ODATE');
  for i := 0 to DocumentNode.ChildNodes.Count-1 do
    if DocumentNode.ChildNodes[i].NodeName='IncludedNote' then
      begin
        aTmp := DocumentNode.ChildNodes[i].FindNode('SubjectCode');
        if Assigned(aTmp) then
          aTmpS := aTmp.FirstChild.NodeValue
        else aTmpS := '';
        aOrder.History.AddItem(aOrder.DataSet,DocumentNode.ChildNodes[i].FindNode('Content').FirstChild.NodeValue,'',aTmpS,nil,0);
      end;
  DocumentNode := Doc.DocumentElement.FindNode('rsm:SpecifiedSupplyChainTradeTransaction');
  for a := 0 to DocumentNode.ChildNodes.Count-1 do
    case DocumentNode.ChildNodes[a].NodeName of
    'ApplicableSupplyChainTradeAgreement'://Kunde
      begin
        for i := 0 to DocumentNode.ChildNodes[a].ChildNodes.Count-1 do
          begin
            aTmp := DocumentNode.ChildNodes[a].ChildNodes[i];
            case aTmp.NodeName of
            'SellerTradeParty':
              begin
                aOrder.Address.Append;
                CombineFields(aOrder.Address,aTmp,'NAME','Name');
                CombineFields(aOrder.Address,aTmp,'ZIP','PostcodeCode');
                CombineFields(aOrder.Address,aTmp,'ADDRESS','LineOne');
                CombineFields(aOrder.Address,aTmp,'CITY','CityName');
                CombineFields(aOrder.Address,aTmp,'COUNTRY','CountryID');
                aOrder.Address.Post;
              end;
            end;
          end;
      end;
    'ApplicableSupplyChainTradeSettlement':
      begin
        aTmp := DocumentNode.ChildNodes[a].ChildNodes[i];
        CombineFields(aOrder,aTmp,'CURRENCY','InvoiceCurrencyCode');

      end;
    end;
  Doc.Free;
end;

end.

