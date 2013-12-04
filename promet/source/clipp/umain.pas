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
  ExtCtrls, ComCtrls, ActnList,Clipbrd, Menus,process;

type
  TfMain = class(TForm)
    acCloseTab: TAction;
    acLogin: TAction;
    acLogout: TAction;
    acNewStatistic: TAction;
    acAdd: TAction;
    acRestore: TAction;
    ActionList2: TActionList;
    Image1: TImage;
    lbFormats: TListBox;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    miView: TMenuItem;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    GlobalStream : TMemoryStream;
    procedure DoCreate;
  end;

var
  fMain: TfMain;

implementation
uses LCLIntf,LCLType,uBaseApplication, uData, uBaseDbInterface,
  uDocuments,uFilterFrame,uIntfStrConsts,uPrometFrames,uBaseDbClasses,
  uWikiFrame,uMainTreeFrame;
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
begin
  lbFormats.Clear;
  FreeAndNil(GlobalStream);
  GlobalStream := TMemoryStream.Create;
  for i := 0 to Clipboard.FormatCount-1 do
    begin
      aMStream := TMemoryStream.Create;
      try
        aOK := Clipboard.GetFormat(Clipboard.Formats[i],aMStream);
      except
        aOK := false;
      end;
      if aOK  then
        begin
          aFormat := Clipboard.Formats[i];
          aMime := ClipboardFormatToMimeType(aFormat);
          lbFormats.Items.Add(aMime);

          aMStream.Position:=0;
          GlobalStream.WriteAnsiString(aMime);
          GlobalStream.WriteDWord(aFormat);
          GlobalStream.WriteDWord(aMStream.Size);
          GlobalStream.CopyFrom(aMStream,0);
          aMStream.Position:=0;
        end;
      aMStream.Free;
    end;
  Image1.Picture.Clear;
  if Clipboard.HasPictureFormat then
    Image1.Picture.LoadFromClipboardFormat(Clipboard.FindPictureFormatID);
  Memo1.Text := Clipboard.AsText;
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
  //Add Search Node
  aDocuments := TDocuments.Create(Self,Data);
  aDocuments.CreateTable;
  aDocuments.Destroy;
  //Projects
  aStat := fMainTreeFrame.tvMain.Items.AddChildObject(nil,strStatistics,TTreeEntry.Create);
  TTreeEntry(aStat.Data).Typ := etStatistic;
  if fMainTreeFrame.tvMain.Items.Count>0 then
    fMainTreeFrame.tvMain.Items[0].Expanded:=True;
//  pcPages.AddTabClass(TfFilter,strProjectList,@AddProjectList,Data.GetLinkIcon('PROJECTS@'),True);
//  aFrame := TfProjectDispoFrame.Create(Self);
//  pcPages.AddTab(aFrame);
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.acRestoreExecute(Sender: TObject);
var
  aFormat: Cardinal;
  aMStream: TMemoryStream;
  aSize: Cardinal;
  aMime: String;
begin
  Clipboard.Clear;
  GlobalStream.Position:=0;
  while GlobalStream.Position<GlobalStream.Size do
    begin
      aMime := GlobalStream.ReadAnsiString;
      aFormat := GlobalStream.ReadDWord;
      if Clipboard.FindFormatID(aMime) = 0 then
        aFormat := RegisterClipboardFormat(aMime);
      aSize := GlobalStream.ReadDWord;
      aMStream := TMemoryStream.Create;
      aMStream.CopyFrom(GlobalStream,aSize);
      aMStream.Position:=0;
      Clipboard.AddFormat(aFormat,aMStream);
      aMStream.Free;
    end;
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

end.

