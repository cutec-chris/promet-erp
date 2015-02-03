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
unit utextpositionframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls, db,
  uPrometFramesInplace;

type
  TfTextPositionFrame = class(TPrometInplaceFrame)
    cbTexttyp: TDBComboBox;
    Position: TDatasource;
    eShortText: TDBMemo;
    eTenderPos: TDBEdit;
    lLongtext: TLabel;
    lShortText: TLabel;
    lTenderPos: TLabel;
    lTexttype: TLabel;
    mLongText: TDBMemo;
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
procedure TfTextPositionFrame.FrameEnter(Sender: TObject);
begin
  with TfPosition(Owner).DataSet as TBaseDbPosition do
    begin
      Position.DataSet := DataSet;
    end;
end;

procedure TfTextPositionFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

