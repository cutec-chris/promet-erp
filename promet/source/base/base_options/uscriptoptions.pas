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
unit uScriptOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, StdCtrls, DBGrids,
  uOptionsFrame;

type
  TfScriptOptions = class(TOptionsFrame)
    DBNavigator2: TDBNavigator;
    gProcesses: TDBGrid;
    lProcesses: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation

{$R *.lfm}

procedure TfScriptOptions.StartTransaction;
begin
  inherited StartTransaction;
end;

procedure TfScriptOptions.CommitTransaction;
begin
  inherited CommitTransaction;
end;

procedure TfScriptOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
end;

end.

