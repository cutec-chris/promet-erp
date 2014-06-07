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
Created 07.06.2014
*******************************************************************************}
unit uPasswords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ComCtrls,uPasswordSave;

type

  { TfPasswords }

  TfPasswords = class(TForm)
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    ToolBar1: TToolBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    DataSet: TPasswordSave;
    function Execute : Boolean;
    procedure SetLanguage;
  end;

var
  fPasswords: TfPasswords;

implementation
uses uData;
{$R *.lfm}

{ TfPasswords }

procedure TfPasswords.FormDestroy(Sender: TObject);
begin
end;

procedure TfPasswords.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DataSet.Free;
end;

function TfPasswords.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfPasswords,fPasswords);
      Self := fPasswords;
    end;
  SetLanguage;
  DataSet := TPasswordSave.Create(nil,Data,nil,Data.Users.DataSet);
  DataSet.CreateTable;
  DataSet.Open;
  Datasource1.DataSet := DataSet.DataSet;
  Show;
end;

procedure TfPasswords.SetLanguage;
begin

end;

end.

