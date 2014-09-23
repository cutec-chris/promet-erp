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
  Classes, SysUtils, db, FileUtil, Forms, Controls, DbCtrls, StdCtrls, DBGrids,
  uOptionsFrame,uprometscripts;

type
  TfScriptOptions = class(TOptionsFrame)
    Scripts: TDataSource;
    DBNavigator2: TDBNavigator;
    gProcesses: TDBGrid;
    lProcesses: TLabel;
  private
    { private declarations }
    aConnection: TComponent;
    FScripts: TBaseScript;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
uses uData;
{$R *.lfm}

constructor TfScriptOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  FScripts := TBaseScript.Create(nil,Data,aConnection);
  Scripts.DataSet := FScripts.DataSet;
end;

destructor TfScriptOptions.Destroy;
begin
  FScripts.Free;
  aConnection.Free;
  inherited Destroy;
end;

procedure TfScriptOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  FScripts.Open;
end;

procedure TfScriptOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  Data.CommitTransaction(aConnection);
end;

procedure TfScriptOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
  Data.RollbackTransaction(aConnection);
end;

end.

