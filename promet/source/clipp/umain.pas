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
Created 04.12.2013
*******************************************************************************}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList,Clipbrd, Menus,process,uclipp,uMainTreeFrame;

type
  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acAdd: TAction;
    acRestore: TAction;
    ActionList2: TActionList;
    eSearch: TEdit;
    eName: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbFormats: TListBox;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    tvMain: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    bNew: TToolButton;
    brestore: TToolButton;
    procedure acAddExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acRestoreExecute(Sender: TObject);
    procedure fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    GlobalStream : TMemoryStream;
    DataSet : TClipp;
    procedure DoCreate;
    procedure RefreshView;
  end;

var
  fMain: TfMain;

implementation
uses uBaseApplication, uData, uBaseDbInterface,
  uDocuments,uFilterFrame,uIntfStrConsts,uPrometFrames,uBaseDbClasses,
  uWikiFrame;
{$R *.lfm}

procedure TfMain.acAddExecute(Sender: TObject);
var
  aFormat: LongWord;
  i: Integer;
  aMime: String;
  aProc: TProcess;
  aExt: String;
  aStream: TFileStream;
  aMStream: TMemoryStream;
  aFStream: TFileStream;
  aOK: Boolean;
  aName: String;
begin
  if InputQuery(strName,strName,aName) then
    begin
      DataSet.Insert;
      DataSet.FieldByName('NAME').AsString:=aName;
      DataSet.AddFromClipboard;
      RefreshView;
      DataSet.Post;
    end;
end;

procedure TfMain.acLoginExecute(Sender: TObject);
var
  WikiFrame: TfWikiFrame;
  Node: TTreeNode;
  miNew: TMenuItem;
  aDocuments: TDocuments;
  aStat: TTreeNode;
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  uClipp.AddToMainTree;
  if fMainTreeFrame.tvMain.Items.Count>0 then
    fMainTreeFrame.tvMain.Items[0].Expanded:=True;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.acRestoreExecute(Sender: TObject);
begin
  DataSet.RestoreToClipboard;
end;

procedure TfMain.fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
begin
  FreeAndnil(DataSet);
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(GlobalStream);
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  uMainTreeFrame.fMainTreeFrame := TfMainTree.Create(Self);
  fMainTreeFrame.Parent := tvMain;
  fMainTreeFrame.Align:=alClient;
  fMainTreeFrame.SearchOptions:='CLIPP';
  fMainTreeFrame.OnSelectionChanged:=@fMainTreeFrameSelectionChanged;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
  if Assigned(Data) then
    begin
    end;
end;

procedure TfMain.Panel2Click(Sender: TObject);
begin

end;

procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Clipp');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;

procedure TfMain.RefreshView;
begin
  Image1.Picture.Clear;
  Memo1.Lines.Clear;
  eName.Text:='';
end;

end.

