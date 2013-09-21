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
unit uProcessOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, ExtCtrls, StdCtrls,
  DbCtrls, uOptionsFrame, db;

type

  { TfProcessOptions }

  TfProcessOptions = class(TOptionsFrame)
    Clients: TDatasource;
    DBGrid1: TDBGrid;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    gProcesses1: TDBGrid;
    lProcesses1: TLabel;
    Panel1: TPanel;
    ProcessParameters: TDatasource;
    Label1: TLabel;
    lProcesses: TLabel;
    Processes: TDatasource;
    gProcesses: TDBGrid;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
  private
    { private declarations }
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation

uses uData, uBaseDbInterface;
{$R *.lfm}
procedure TfProcessOptions.StartTransaction;
begin
  inherited StartTransaction;
  Clients.DataSet := Data.ProcessClient.DataSet;
  Processes.DataSet := Data.ProcessClient.Processes.DataSet;
  Data.ProcessClient.Open;
  Data.ProcessClient.Processes.Open;
  ProcessParameters.DataSet := Data.ProcessClient.Processes.Parameters.DataSet;
  Data.ProcessClient.Processes.Parameters.DataSet.Open;
end;
procedure TfProcessOptions.CommitTransaction;
begin
  if (Processes.State = dsEdit) or (Processes.State = dsInsert) then
    Processes.DataSet.Post;
  inherited CommitTransaction;
end;
procedure TfProcessOptions.RollbackTransaction;
begin
  if (Processes.State = dsEdit) or (Processes.State = dsInsert) then
    Processes.DataSet.Cancel;
  inherited RollbackTransaction;
end;

end.
