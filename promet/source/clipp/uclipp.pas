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
Created 04.12.2013
*******************************************************************************}
unit uclipp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbDataSet,uBaseDbClasses,uIntfStrConsts,uBaseDBInterface,
  db,Clipbrd, ComCtrls;
type
  TClipp = class(TBaseDBList)
  protected
    procedure DefineFields(aDataSet: TDataSet); override;
  public
    procedure AddFromClipboard;
    procedure RestoreToClipboard;
  end;

procedure AddToMainTree(MainNode : TTreeNode = nil);

implementation

uses uMainTreeFrame,uData;

procedure AddToMainTree(MainNode : TTreeNode = nil);
var
  aClip: TTreeNode;
  Clipp: TClipp;
  Node1: TTreeNode;
begin
  Clipp := TClipp.Create(nil,Data);
  Clipp.CreateTable;
  Clipp.Free;
  //Clipboard
  aClip := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,strClipboard,TTreeEntry.Create);
  TTreeEntry(aClip.Data).Typ := etClipboard;
  Data.SetFilter(Data.Tree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('Z')+'))',0,'','ASC',False,True,True);
  Data.Tree.DataSet.First;
  while not Data.Tree.dataSet.EOF do
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(aClip,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
      TTreeEntry(Node1.Data).DataSource := Data.Tree;
      TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
      TTreeEntry(Node1.Data).Typ := etDir;
      fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
      Data.Tree.DataSet.Next;
    end;
end;

procedure TClipp.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CLIPBOARD';
      TableCaption := strClipboard;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,True);
            Add('TREEENTRY',ftLargeint,0,False);
            Add('DESCRIPTION',ftMemo,0,False);
            Add('DATA',ftMemo,0,False);
            Add('CREATEDBY',ftString,4,False);
            Add('CHANGEDBY',ftString,4,False);
          end;
    end;
end;

procedure TClipp.AddFromClipboard;
begin

end;

procedure TClipp.RestoreToClipboard;
begin

end;

end.

