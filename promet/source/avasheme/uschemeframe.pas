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
Created 13.03.2016
*******************************************************************************}
unit uschemeframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  DbCtrls, EditBtn, ComCtrls, ActnList, uExtControls, uPrometFrames,
  uPrometFramesInplace, usimplegraph,Graphics;

type

  { TfShemeFrame }

  TfShemeFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acClose: TAction;
    acCopy: TAction;
    acDelete: TAction;
    acDeleteThumb: TAction;
    acExport: TAction;
    acImport: TAction;
    acPaste: TAction;
    acPrint: TAction;
    acRights: TAction;
    acSave: TAction;
    acSetTreeDir: TAction;
    acShowTreeDir: TAction;
    acStartTimeRegistering: TAction;
    ActionList1: TActionList;
    bChangeNumber: TSpeedButton;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    bExecute: TSpeedButton;
    cbStatus: TComboBox;
    eNumber: TDBEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pNav1: TPanel;
    sbMenue: TSpeedButton;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
  private
    { private declarations }
    FGraph: TEvsSimpleGraph;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetLanguage; override;
  end;

procedure AddToMainTree(aAction : TAction;Node : TTreeNode);

implementation

uses uData,uBaseDBInterface,uMainTreeFrame;

procedure AddToMainTree(aAction: TAction; Node: TTreeNode);
var
  Node1: TTreeNode;
begin
  if (Data.Users.Rights.Right('SCHEME') > RIGHT_NONE) then
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
      Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('S')+'))';
      Data.Tree.DataSet.Filtered:=True;
      Data.Tree.DataSet.First;
      while not Data.Tree.dataSet.EOF do
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
          TTreeEntry(Node1.Data).DataSource := Data.Tree;
          TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
          TTreeEntry(Node1.Data).Typ := etDir;
          fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
          Data.Tree.DataSet.Next;
        end;
      Data.Tree.DataSet.Filtered:=False;
    end;
end;

{$R *.lfm}

{ TfShemeFrame }

constructor TfShemeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraph := TEvsSimpleGraph.Create(Self);
  FGraph.Parent:= Self;
  FGraph.Align := alClient;
  FGraph.Color := clWhite;
  FGraph.ShowGrid:=True;
  FGraph.DoubleBuffered:=True;
  FGraph.HorzScrollBar.Tracking:=True;
  FGraph.VertScrollBar.Tracking:=True;

  //FGraph.OnObjectDblClick := @goDblClick;
  //FGraph.OnDblClick := @sgDblClick;
  FGraph.FixedScrollBars := True;
end;

procedure TfShemeFrame.SetLanguage;
begin

end;

end.

