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
  Buttons, ActnList, uOptionsFrame, uprometscripts;

type
  TfScriptOptions = class(TOptionsFrame)
    acEdit: TAction;
    acExecute: TAction;
    ActionList1: TActionList;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Scripts: TDataSource;
    DBNavigator2: TDBNavigator;
    gProcesses: TDBGrid;
    lProcesses: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure acEditExecute(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure FScriptsDataSetBeforeScroll(DataSet: TDataSet);
    procedure FScriptsWriteln(const s: string);
  private
    { private declarations }
    aConnection: TComponent;
    FScripts: TBaseScript;
    aScript: TBaseScript;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
uses uData,uScriptEditor;
{$R *.lfm}

procedure TfScriptOptions.acEditExecute(Sender: TObject);
var
  aRec: TBookmark;
begin
  fScriptEditor.Execute(FScripts.FieldByName('NAME').AsString,aConnection);
  aRec := Scripts.DataSet.GetBookmark;
  Scripts.DataSet.Refresh;
  Scripts.DataSet.GotoBookmark(aRec);
end;

procedure TfScriptOptions.acExecuteExecute(Sender: TObject);
var
  aRec: TBookmark;
begin
  FScripts.Post;
  aScript := TBaseScript.Create(nil);
  aScript.Select(FScripts.Id.AsVariant);
  aScript.Open;
  aScript.Writeln:=@FScriptsWriteln;
  if aScript.Count>0 then
    aScript.Execute(Null);
  aScript.DataSet.Refresh;
  aRec := FScripts.DataSet.GetBookmark;
  FScripts.DataSet.Refresh;
  FScripts.DataSet.GotoBookmark(aRec);
  while aScript.DataSet.FieldByName('STATUS').AsString='R' do
    begin
      Application.ProcessMessages;
      aScript.DataSet.Refresh;
    end;
  aScript.Post;
  aScript.Free;
  aRec := FScripts.DataSet.GetBookmark;
  FScripts.DataSet.Refresh;
  FScripts.DataSet.GotoBookmark(aRec);
end;

procedure TfScriptOptions.FScriptsDataSetBeforeScroll(DataSet: TDataSet);
var
  aRec: TBookmark;
begin
  aRec := Scripts.DataSet.GetBookmark;
end;

procedure TfScriptOptions.FScriptsWriteln(const s: string);
begin
  aScript.Edit;
  aScript.FieldByName('LASTRESULT').AsString:=aScript.FieldByName('LASTRESULT').AsString+lineending+s;
  aScript.Post;
end;
constructor TfScriptOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  FScripts := TBaseScript.CreateEx(nil,Data,aConnection);
  FScripts.CreateTable;
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
  //Data.StartTransaction(aConnection);
  FScripts.Open;
end;

procedure TfScriptOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  //Data.CommitTransaction(aConnection);
end;

procedure TfScriptOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
  //Data.RollbackTransaction(aConnection);
end;

end.

