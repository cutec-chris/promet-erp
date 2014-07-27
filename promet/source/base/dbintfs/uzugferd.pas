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
  Classes, SysUtils,uOrder,XMLRead,DOM,uBaseDbClasses,uBaseERPDBClasses,uData;

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
  aTmp1: TDOMNode;
  aDiscount: Extended;

  function CombineFields(aData : TBaseDbDataSet;aNode : TDOMNode;aDBField : string;aZField : string) : Boolean;
  var
    aTmp: TDOMNode;
  begin
    Result := False;
    if not Assigned(aNode) then exit;
    aTmp := aNode.FindNode(aZField);
    if Assigned(aTmp) and Assigned(aData.FieldByName(aDBField)) then
      begin
        aData.FieldByName(aDBField).AsString:=aTmp.FirstChild.NodeValue;
        result := True;
      end;
  end;
  function CombineDateFields(aData : TBaseDbDataSet;aNode : TDOMNode;aDBField : string;aZField : string) : Boolean;
  var
    aTmp: TDOMElement;
    aVal: DOMString;
  begin
    Result := False;
    if not Assigned(aNode) then exit;
    aTmp := TDOMElement(DocumentNode.FindNode(aZField));
    if Assigned(aTmp) and Assigned(aData.FieldByName(aDBField)) then
      begin
        aVal := aTmp.FirstChild.NodeValue;
        if aTmp.GetAttribute('format')='102' then
          begin
            aData.FieldByName(aDBField).AsDateTime:=EncodeDate(StrToInt(copy(aVal,0,4)),StrToInt(copy(aVal,5,2)),StrToInt(copy(aVal,7,2)));
            Result := True;
          end;
      end;
  end;

begin
  Result := True;
  ReadXMLFile(Doc, aFile);
  DocumentNode := Doc.DocumentElement.FindNode('rsm:HeaderExchangedDocument');
  if not aOrder.Active then
    begin
      aOrder.Insert;
      aOrder.Post;
    end;
  aOrder.Edit;
  aOrder.Status.AsString:='ER';
  CombineFields(aOrder,DocumentNode,'COMMISSION','ID');
  CombineDateFields(aOrder,DocumentNode,'ODATE','IssueDateTime');
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
                CombineFields(aOrder.Address,aTmp.FindNode('PostalTradeAddress'),'ZIP','PostcodeCode');
                CombineFields(aOrder.Address,aTmp.FindNode('PostalTradeAddress'),'ADDRESS','LineOne');
                CombineFields(aOrder.Address,aTmp.FindNode('PostalTradeAddress'),'CITY','CityName');
                CombineFields(aOrder.Address,aTmp.FindNode('PostalTradeAddress'),'COUNTRY','CountryID');
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
    'IncludedSupplyChainTradeLineItem':
      begin
        aTmp := DocumentNode.ChildNodes[a];
        aOrder.Positions.Append;
        //aOrder.Positions.DisableCalculation;
        aTmp1 := aTmp.FindNode('AssociatedDocumentLineDocument');
        if Assigned(aTmp1) then
          begin
            CombineFields(aOrder.Positions,aTmp1,'POSNO','LineID');
            if CombineFields(aOrder.Positions,aTmp1.FindNode('IncludedNote'),'SHORTTEXT','Content') then
              aOrder.Positions.FieldByName('POSTYP').AsString:='TX';
          end;
        aTmp1 := aTmp.FindNode('SpecifiedSupplyChainTradeAgreement');
        if Assigned(aTmp1) then
          begin
            aOrder.Positions.FieldByName('POSTYP').AsString:='MD';
            if aOrder.Positions.FieldByName('SHORTTEXT').AsString<>'' then
              aOrder.Positions.FieldByName('TEXT').AsString := aOrder.Positions.FieldByName('SHORTTEXT').AsString;
            aTmp1 := aTmp1.FindNode('GrossPriceProductTradePrice');
            CombineFields(aOrder.Positions,aTmp1,'SELLPRICE','ChargeAmount');
            if CombineFields(aOrder.Positions,aTmp1,'QUANTITY','BasisQuantity') then
              aOrder.Positions.FieldByName('QUANTITYU').AsString:=TDOMElement(aTmp1.FindNode('BasisQuantity')).GetAttribute('unitCode');
            if Assigned(aTmp1) then
              begin
                aTmp1 := aTmp1.FindNode('AppliedTradeAllowanceCharge');
                if Assigned(aTmp1) then
                  begin
                    if Assigned(aTmp1.FindNode('Reason')) and (aTmp1.FindNode('Reason').FirstChild.NodeValue='Rabatt') then
                      begin
                        aDiscount := StrToFloat(aTmp1.FindNode('ActualAmount').FirstChild.NodeValue);
                        if aOrder.Positions.FieldByName('SELLPRICE').AsFloat>0 then
                          aDiscount:=(aDiscount/aOrder.Positions.FieldByName('SELLPRICE').AsFloat)*100
                        else aDiscount:=0;
                        aOrder.Positions.FieldByName('DISCOUNT').AsFloat:=aDiscount;
                      end;
                  end;
              end;
            CombineFields(aOrder.Positions,aTmp.FindNode('SpecifiedTradeProduct'),'IDENT','BuyerAssignedID');
            CombineFields(aOrder.Positions,aTmp.FindNode('SpecifiedTradeProduct'),'MANUFACNR','SellerAssignedID');
            CombineFields(aOrder.Positions,aTmp.FindNode('SpecifiedTradeProduct'),'SHORTTEXT','Name');
            CombineFields(aOrder.Positions,aTmp.FindNode('SpecifiedTradeProduct'),'TEXT','Description');
          end;
        aTmp1 := aTmp.FindNode('SpecifiedSupplyChainTradeSettlement');
        if Assigned(aTmp1) then
          begin
            aTmp1 := aTmp1.FindNode('ApplicableTradeTax');
            if Assigned(aTmp1) then
              begin
                if Assigned(aTmp1.FindNode('ApplicablePercent')) then
                  begin
                    if Data.Vat.Locate('VALUE',aTmp1.FindNode('ApplicablePercent').FirstChild.NodeValue,[]) then
                      aOrder.Positions.FieldByName('VAT').AsInteger:=Data.Vat.FieldByName('ID').AsInteger;
                  end;
              end;
          end;
        aOrder.Positions.Post;
        //aOrder.Positions.EnableCalculation;
      end;
    end;
  aOrder.Post;
  Doc.Free;
end;

end.

