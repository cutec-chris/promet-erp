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
unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ButtonPanel,uOptionsFrame;

resourcestring
  strGeneralOptions                             = 'Globale Einstellungen';
  strAutomationOptions                          = 'Automation/Synchronisation';
  strMasterdataOptions                          = 'Stammdaten';
  strPersonalOptions                            = 'Persönliche Einstellungen';

type
  { TfOptions }

  TfOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    pFrame: TPanel;
    pRight: TPanel;
    pHeader: TPanel;
    Splitter1: TSplitter;
    tvMain: TTreeView;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tvMainSelectionChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FormsList: TList;
    procedure RegisterOptionsFrame(aFrame : TOptionsFrame;aName : string;aCategory : string = '');
    destructor Destroy;override;
    procedure SetLanguage;
  end; 

var
  fOptions: TfOptions;

implementation

{$R *.lfm}

{ TfOptions }

procedure TfOptions.tvMainSelectionChanged(Sender: TObject);
begin
  if pFrame.ControlCount > 0 then
    pFrame.RemoveControl(pFrame.Controls[0]);
  pHeader.Visible:=False;
  if tvMain.Selected = nil then exit;
  if tvMain.Selected.Data = nil then
    begin
      if tvMain.Selected.Count > 0 then
        tvMain.Selected := tvMain.Selected.Items[0];
      exit;
    end;
  with TOptionsFrame(tvMain.Selected.Data) do
    begin
      Parent := pFrame;
      Align := alClient;
      Show;
      pHeader.Caption:=tvMain.Selected.Text;
      pHeader.Visible:=True;
      if not InTransaction then StartTransaction;
      pFrame.Caption:='';
    end;
end;

procedure TfOptions.OKButtonClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := tvMain.Items.GetFirstNode;
  while Assigned(aNode) do
    begin
      if Assigned(aNode.Data) then
        if TOptionsFrame(aNode.Data).InTransaction then
          TOptionsFrame(aNode.Data).CommitTransaction;
      aNode := aNode.GetNext;
    end;
end;
procedure TfOptions.CancelButtonClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := tvMain.Items.GetFirstNode;
  while Assigned(aNode) do
    begin
      if Assigned(aNode.Data) then
        if TOptionsFrame(aNode.Data).InTransaction then
          TOptionsFrame(aNode.Data).RollbackTransaction;
      aNode := aNode.GetNext;
    end;
end;
procedure TfOptions.FormCreate(Sender: TObject);
begin
  FormsList := TList.Create;
end;
procedure TfOptions.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FormsList.Count-1 do
    TOptionsFrame(FormsList[i]).Destroy;
  FormsList.Free;
end;
procedure TfOptions.RegisterOptionsFrame(aFrame: TOptionsFrame;aName : string;aCategory : string = '');
var
  aParentNode: TTreeNode;
  aNode: TTreeNode;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfOptions,fOptions);
      Self := fOptions;
    end;
  aParentNode := nil;
  if aCategory <> '' then
    begin
      aParentNode := tvMain.Items.FindTopLvlNode(aCategory);
      if not Assigned(aParentNode) then
        aParentNode := tvMain.Items.Add(nil,aCategory);
    end;
  aNode := tvMain.Items.AddChildObject(aParentNode,aName,aFrame);
  FormsList.Add(aFrame);
  tvMain.FullExpand;
end;
destructor TfOptions.Destroy;
begin
  inherited Destroy;
end;
procedure TfOptions.SetLanguage;
begin

end;

end.

