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
Created 01.06.2006
*******************************************************************************}
unit uOverviewFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, db,uOrder,uBaseVisualcontrols,
  uPrometFramesInplace;
type
  TfOrderOverviewFrame = class(TPrometInplaceFrame)
    Order: TDatasource;
    tvOrderView: TTreeView;
    procedure FrameEnter(Sender: TObject);
    procedure tvOrderViewDblClick(Sender: TObject);
  private
    { private declarations }
    FOpenLink : string;
    procedure DoOpenLink(Data : PtrInt);
  public
    { public declarations }
    ParentOrder : TOrder;
    procedure SetRights(Editable : Boolean);override;
  end; 
implementation
uses uData,uOrderFrame;
{$R *.lfm}
procedure TfOrderOverviewFrame.FrameEnter(Sender: TObject);
var
  Rec: TBookmark;
  Node: TTreeNode = nil;
  aParent: TTreeNode=nil;
  aLastNode: TTreeNode = nil;
  OrderType: TDataSet;
  oRec: TBookmark;
  uNode: TTreeNode;
  ActID: String;
begin
  tvOrderview.BeginUpdate;
  tvOrderView.Items.Clear;
  with Order.DataSet do
    begin
      ActID := FieldByName('ORDERNO').AsString;
      Rec := GetBookmark;
      DisableControls;
      First;
      OrderType := ParentOrder.OrderType.DataSet;
      oRec := OrderType.GetBookmark;
      while not EOF do
        begin
          Node := nil;
          if OrderType.Locate('STATUS',FieldByName('STATUS').AsString, [loCaseInsensitive]) then
            if OrderType.FieldByName('ISDERIVATE').AsString = 'Y' then
              Node := tvOrderView.Items.AddChild(aParent,FieldByName('STATUS').AsString+' '+FieldByName('ORDERNO').AsString);
          if not Assigned(Node) then
            begin
              Node := tvOrderView.Items.AddChild(nil,FieldByName('STATUS').AsString+' '+FieldByName('ORDERNO').AsString);
              aParent := Node;
            end;
          Node.ImageIndex:=25;
          Node.Selectedindex := Node.ImageIndex;
          if not (FieldByName('DATE').IsNull or (OrderType.FieldByName('ISDERIVATE').AsString = 'Y')) then
            aLastNode := Node;
          if FieldByName('ORDERNO').AsString = ActID then
            tvOrderView.Selected := Node;
          Node.Expanded:=True;
          Next;
        end;
      if Assigned(aLastNode) then
        begin
          aLastNode.ImageIndex:=26;
          aLastNode.SelectedIndex:=26;
        end;
      EnableControls;
      GotoBookmark(Rec);
      FreeBookmark(Rec);
      OrderType.GotoBookmark(oRec);
      OrderType.FreeBookmark(oRec);
    end;
  tvOrderview.EndUpdate;
end;

procedure TfOrderOverviewFrame.tvOrderViewDblClick(Sender: TObject);
var
  aLink: String;
  bLink: String;
begin
  if not Assigned(tvOrderView.Selected) then exit;
  if not Assigned(Owner) then exit;
  bLink := Data.BuildLink(ParentOrder.DataSet);
  if ParentOrder.DataSet.Locate('ORDERNO',copy(tvOrderView.Selected.Text,pos(' ',tvOrderView.Selected.Text)+1,length(tvOrderView.Selected.Text)),[]) then
    begin
      aLink := Data.BuildLink(ParentOrder.DataSet);
      TfOrderFrame(owner).pcHeader.PageIndex:=0;
      if aLink <> bLink then
        begin
          FOpenLink := aLink;
          Application.QueueAsyncCall(@DoOpenLink,PtrInt(nil));
        end;
    end;
end;
procedure TfOrderOverviewFrame.DoOpenLink(Data: PtrInt);
begin
  TfOrderFrame(owner).OpenFromLink(FOpenLink);
end;
procedure TfOrderOverviewFrame.SetRights(Editable: Boolean);
begin
end;

end.

