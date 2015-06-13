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
Created 01.06.2012
*******************************************************************************}
unit utableedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, uIntfStrConsts, db,
  memds, FileUtil, Translations, md5, ComCtrls, ExtCtrls, DbCtrls, Grids,
  uFilterFrame,uBaseDbClasses,uBaseDatasetInterfaces;

type
  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acDelete: TAction;
    ActionList1: TActionList;
    Datasource: TDatasource;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miNewMandant: TMenuItem;
    miOptions: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    Properties: TXMLPropStorage;
    procedure acDeleteExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FFilter : TfFilter;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCreate;
  end;
  TOwnDataSet = class(TBaseDBDataset)
    procedure DefineFields(aDataSet: TDataSet); override;
  end;

var
  fMain: TfMain;

implementation
{$R *.lfm}
uses uBaseApplication, uData, uBaseDbInterface, uOrder, uDataImport,uDataImportCSV;
resourcestring
  strTablenameMissing         = 'Sie müssen mit dem Kommandozielenparameter --tablename="TABELLENNAME" einen Tabellennamen zum Editieren angeben !';

procedure TOwnDataSet.DefineFields(aDataSet: TDataSet);
begin

end;

procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Tableedit');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;

procedure TfMain.acLoginExecute(Sender: TObject);
var
  i: Integer;
  aDataSet: TBaseDBDataset;
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  if Application.GetOptionValue('t','tablename') = '' then
    Showmessage(strTablenameMissing)
  else
    begin
      FFilter := TfFilter.Create(nil);
      FFilter.Parent:=Self;
      FFilter.Align := alClient;
      FFIlter.FilterType:='TABLE:'+Data.QuoteField(Application.GetOptionValue('t','tablename'));
      aDataSet := TOwnDataSet.Create(nil);
      DataSource.DataSet := Data.GetNewDataSet('select * from '+Data.QuoteField(Application.GetOptionValue('t','tablename')),nil,nil,aDataSet);
      aDataSet.DataSet := DataSource.DataSet;
      DataSource.DataSet.Open;
      with aDataSet.DataSet as IBaseManageDB do
         TableName := Application.GetOptionValue('t','tablename');
      FFilter.DataSet := aDataSet;
      FFilter.Editable:=True;
      FFilter.gList.ReadOnly:=False;
//      FFilter.AddToolbarAction(acDelete);
      for i := 0 to FFilter.gList.Columns.Count-1 do
        begin
          if FFilter.gList.Columns[i].Width > Width-50 then
            FFilter.gList.Columns[i].Width := Width-50;
          FFilter.gList.Columns[i].ReadOnly:=False;
        end;
    end;
end;

procedure TfMain.acDeleteExecute(Sender: TObject);
begin
  Datasource.DataSet.Delete;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with Application as IBaseApplication do
    begin
      SaveConfig;
      DoExit;
    end;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
end;

constructor TfMain.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfMain.Destroy;
begin
  inherited Destroy;
end;

initialization

end.
