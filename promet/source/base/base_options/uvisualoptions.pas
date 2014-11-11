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
Created 23.10.2014
*******************************************************************************}
unit uvisualoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, StdCtrls, db,
  uOptionsFrame, uBaseDBClasses;

type

  { TfVisualOptions }

  TfVisualOptions = class(TOptionsFrame)
    cbHideTree: TCheckBox;
    rbLeft: TRadioButton;
    rbRight: TRadioButton;
    procedure cbHideTreeChange(Sender: TObject);
    procedure rbLeftChange(Sender: TObject);
  private
    { private declarations }
    aConnection: TComponent;
    aUserFieldDefs: TUserfielddefs;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation

{$R *.lfm}
uses uData,uBaseDBInterface;

procedure TfVisualOptions.rbLeftChange(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    DBConfig.WriteBoolean('TBLEFT',rbLeft.Checked);
end;

procedure TfVisualOptions.cbHideTreeChange(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    DBConfig.WriteBoolean('HIDETREE',cbHideTree.Checked);
end;

constructor TfVisualOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  with Application as IBaseDbInterface do
    cbHideTree.Checked := DBConfig.ReadBoolean('HIDETREE',false);
  with Application as IBaseDbInterface do
    rbLeft.Checked := DBConfig.ReadBoolean('TBLEFT',true);
end;

destructor TfVisualOptions.Destroy;
begin
  try
    aConnection.Destroy;
  except
  end;
  inherited Destroy;
end;

procedure TfVisualOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
end;

procedure TfVisualOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfVisualOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

