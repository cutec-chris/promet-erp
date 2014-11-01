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
*******************************************************************************}
unit uArticleStorageFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, db,
  uFilterFrame,uPrometFramesInplace, uExtControls;
type
  TfArticleStorageFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    Datasource: TDatasource;
    dnNavigator: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    pToolbar: TPanel;
    Panel2: TPanel;
    procedure FListFListFilterChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FList: TfFilter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation
{$R *.lfm}
procedure TfArticleStorageFrame.FListFListFilterChanged(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FList.gList.Columns.Count-1 do
    if FList.gList.Columns[i].FieldName = 'PLACE' then
      FList.gList.Columns[i].ReadOnly:=False;
end;
constructor TfArticleStorageFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TfFilter.Create(Self);
  with FList do
    begin
      Editable:=false;
      Parent := Self;
      Align := alClient;
      FList.OnFilterChanged:=@FListFListFilterChanged;
      Show;
    end;
end;
destructor TfArticleStorageFrame.Destroy;
begin
  FList.DataSet := nil;
  FList.Free;
  inherited Destroy;
end;

procedure TfArticleStorageFrame.SetRights(Editable: Boolean);
begin
  dnNavigator.Enabled := Editable;
  ArrangeToolBar(pToolbar,nil,'Storage');
end;

end.

