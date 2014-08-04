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
unit ucalcframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  DBGrids, Menus, db, uPrometFramesInplace;

type

  { TfCalcPositionFrame }

  TfCalcPositionFrame = class(TPrometInplaceFrame)
    cbPQuantityU2: TDBComboBox;
    eCommonPrice: TDBEdit;
    eDiscont: TDBEdit;
    eIdent2: TDBEdit;
    ePosNo3: TDBEdit;
    ePosPrice: TDBEdit;
    ePQuantity2: TDBEdit;
    ePurchase: TDBEdit;
    eQuantity: TDBEdit;
    eSellPrice: TDBEdit;
    eShortText3: TDBEdit;
    eWholePrice: TDBEdit;
    gPrices: TDBGrid;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    lAddVAT: TLabel;
    lBP: TLabel;
    lCommonPrice: TLabel;
    lDiscont: TLabel;
    lIdent2: TLabel;
    lPosNo3: TLabel;
    lPosPrice: TLabel;
    lPQuantity2: TLabel;
    lQuantity: TLabel;
    lQuantityUnit2: TLabel;
    lSEP: TLabel;
    lShortText3: TLabel;
    lSP: TLabel;
    lWholePrice: TLabel;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pmImage: TPopupMenu;
    Position: TDatasource;
    procedure FrameEnter(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetRights(Editable : Boolean);override;
  end; 

implementation
{$R *.lfm}
uses uPositionFrame,uBaseERPDBClasses;
procedure TfCalcPositionFrame.FrameEnter(Sender: TObject);
begin
  with TfPosition(Owner).DataSet as TBaseDbPosition do
    begin
      Position.DataSet := DataSet;
    end;
end;

procedure TfCalcPositionFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

