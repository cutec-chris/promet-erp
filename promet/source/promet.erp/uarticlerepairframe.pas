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
unit uarticlerepairframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, uExtControls, db,
  uMasterdata,uPrometFramesInplace;
type
  TFArticlerepairFrame = class(TPrometInplaceFrame)
    Assembly: TDatasource;
    Part: TDatasource;
    dnAssembly: TDBNavigator;
    dnPart: TDBNavigator;
    gAssemblys: TExtDBGrid;
    gParts: TExtDBGrid;
  private
    FMasterdata: TMasterdata;
    procedure SetMasterdata(const AValue: TMasterdata);
    { private declarations }
  public
    { public declarations }
    property Masterdata : TMasterdata read FMasterdata write SetMasterdata;
    procedure SetRights(Editable : Boolean);override;
  end; 
implementation
{$R *.lfm}
procedure TFArticlerepairFrame.SetMasterdata(const AValue: TMasterdata);
begin
  if FMasterdata=AValue then exit;
  FMasterdata:=AValue;
  Assembly.DataSet := AValue.Assembly.DataSet;
  AValue.Assembly.Parts.Open;
  Part.DataSet := AValue.Assembly.Parts.DataSet;
end;

procedure TFArticlerepairFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

