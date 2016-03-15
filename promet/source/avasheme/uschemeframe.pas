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
  DbCtrls, EditBtn, ComCtrls, ActnList, uExtControls, db, uPrometFrames,
  uPrometFramesInplace, usimplegraph,Graphics,uBaseDbClasses,variants,Utils;

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
    ActionList: TActionList;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bExecute: TSpeedButton;
    bMenue1: TSpeedButton;
    cbStatus: TComboBox;
    ClipboardBitmap: TAction;
    ClipboardMetafile: TAction;
    ClipboardNative: TAction;
    CoolBar1: TToolBar;
    EditAlign: TAction;
    EditBringToFront: TAction;
    EditCopy: TAction;
    EditCut: TAction;
    EditDelete: TAction;
    EditInvertSelection: TAction;
    EditLockLinks: TAction;
    EditLockNodes: TAction;
    EditMakeAllSelectable: TAction;
    EditPaste: TAction;
    EditProperties: TAction;
    EditSelectAll: TAction;
    EditSendToBack: TAction;
    EditSize: TAction;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel4: TExtRotatedLabel;
    FormatAlignBottom: TAction;
    FormatAlignLeft: TAction;
    FormatAlignRight: TAction;
    FormatAlignTop: TAction;
    FormatBold: TAction;
    FormatCenter: TAction;
    FormatItalic: TAction;
    FormatUnderline: TAction;
    FormatVCenter: TAction;
    HelpAbout: TAction;
    HelpUsage: TAction;
    ImageList: TImageList;
    LinkAddPoint: TAction;
    LinkGrow: TAction;
    LinkRemovePoint: TAction;
    LinkReverse: TAction;
    LinkRotateCCW: TAction;
    LinkRotateCW: TAction;
    LinkShrink: TAction;
    ObjectsBezier: TAction;
    ObjectsEllipse: TAction;
    ObjectsHexagon: TAction;
    ObjectsLink: TAction;
    ObjectsNone: TAction;
    ObjectsPentagon: TAction;
    ObjectsRectangle: TAction;
    ObjectsRhomboid: TAction;
    ObjectsRoundRect: TAction;
    ObjectsTriangle: TAction;
    OptionsConfirmDeletion: TAction;
    OptionsConfirmHookLink: TAction;
    Panel2: TPanel;
    Panel7: TPanel;
    pToolbar: TPanel;
    Scheme: TDataSource;
    eName: TDBEdit;
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
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TSpeedButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ViewActualSize: TAction;
    ViewFixScrolls: TAction;
    ViewGrid: TAction;
    ViewPan: TAction;
    ViewTransparent: TAction;
    ViewWholeGraph: TAction;
    ViewZoomIn: TAction;
    ViewZoomOut: TAction;
    procedure acCancelExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure goDblClick(Graph: TEvsSimpleGraph; GraphObject: TEvsGraphObject);
    procedure ObjectsLinkExecute(Sender: TObject);
    procedure ObjectsNoneExecute(Sender: TObject);
    procedure ObjectsPentagonExecute(Sender: TObject);
    procedure ObjectsRectangleExecute(Sender: TObject);
    procedure ObjectsRhomboidExecute(Sender: TObject);
    procedure ObjectsRoundRectExecute(Sender: TObject);
    procedure ObjectsTriangleExecute(Sender: TObject);
    procedure SchemeStateChange(Sender: TObject);
    procedure ViewPanExecute(Sender: TObject);
  private
    { private declarations }
    FEditable: Boolean;
    FGraph: TEvsSimpleGraph;
    procedure FGraphObjectChange(Graph: TEvsSimpleGraph;
      GraphObject: TEvsGraphObject);
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
    procedure DoOpen;override;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure New;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure SetLanguage; override;
  end;

procedure AddToMainTree(aAction : TAction;Node : TTreeNode);

implementation

uses uData,uBaseDBInterface,uMainTreeFrame,uscheme,uIntfStrConsts,uSchemenodeproperties,
  uschemelinkproperties;

resourcestring
  strNewScheme                     = 'Neues Schema';

procedure AddToMainTree(aAction: TAction; Node: TTreeNode);
var
  Node1: TTreeNode;
begin
  if (Data.Users.Rights.Right('SCHEME') > RIGHT_NONE) then
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
      Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('D')+'))';
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

procedure TfShemeFrame.ViewPanExecute(Sender: TObject);
begin
  FGraph.SnapToGrid:=False;
  FGraph.CommandMode := cmPan;
end;

procedure TfShemeFrame.ObjectsNoneExecute(Sender: TObject);
begin
  if not FEditable then
    FGraph.CommandMode := cmViewOnly
  else
    FGraph.CommandMode := cmEdit;
end;

procedure TfShemeFrame.ObjectsLinkExecute(Sender: TObject);
begin
  FGraph.CommandMode := cmInsertLink;
  FGraph.DefaultLinkClass := TEvsGraphLink;
end;

procedure TfShemeFrame.acSaveExecute(Sender: TObject);
var
  aMS: TMemoryStream;
begin
  if Assigned(FConnection) then
    begin
      FDataSet.Edit;
      aMS := TMemoryStream.Create;
      FGraph.SaveToStream(aMS);
      aMS.Position:=0;
      Data.StreamToBlobField(aMS,FDataSet.DataSet,'DATA');
      FDataSet.CascadicPost;
    end;
end;

procedure TfShemeFrame.goDblClick(Graph: TEvsSimpleGraph;
  GraphObject: TEvsGraphObject);
begin
  if FGraph.SelectedObjects.Count > 0 then begin
    if GraphObject.IsNode then
      TfNodeProperties.Execute(FGraph.SelectedObjects)
    else TfLinkProperties.Execute(FGraph.SelectedObjects);
  end;
end;

procedure TfShemeFrame.acCancelExecute(Sender: TObject);
begin
  DataSet.CascadicCancel;
  DoOpen;
end;

procedure TfShemeFrame.ObjectsPentagonExecute(Sender: TObject);
begin
  FGraph.DefaultNodeClass := TEvsPentagonalNode;
  FGraph.CommandMode := cmInsertNode;
end;

procedure TfShemeFrame.ObjectsRectangleExecute(Sender: TObject);
begin
  FGraph.DefaultNodeClass := TEvsRectangularNode;
  FGraph.CommandMode := cmInsertNode;
end;

procedure TfShemeFrame.ObjectsRhomboidExecute(Sender: TObject);
begin
  FGraph.DefaultNodeClass := TEvsRhomboidalNode;
  FGraph.CommandMode := cmInsertNode;
end;

procedure TfShemeFrame.ObjectsRoundRectExecute(Sender: TObject);
begin
  FGraph.DefaultNodeClass := TEvsRoundRectangularNode;
  FGraph.CommandMode := cmInsertNode;
  FGraph.SnapToGrid:=True;
end;

procedure TfShemeFrame.ObjectsTriangleExecute(Sender: TObject);
begin
  FGraph.DefaultNodeClass := TEvsTriangularNode;
  FGraph.CommandMode := cmInsertNode;
end;

procedure TfShemeFrame.SchemeStateChange(Sender: TObject);
var
  aEnabled: Boolean;
begin
  aEnabled := DataSet.CanEdit or DataSet.Changed;
  acSave.Enabled := aEnabled;
  acCancel.Enabled:= aEnabled;
end;

procedure TfShemeFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  acSave.Enabled:=False;
  acCancel.Enabled:=False;
  if not Assigned(AValue) then exit;
  Scheme.DataSet := AValue.DataSet;
end;

procedure TfShemeFrame.FGraphObjectChange(Graph: TEvsSimpleGraph;
  GraphObject: TEvsGraphObject);
begin
  DataSet.Edit;
end;

procedure TfShemeFrame.DoOpen;
var
  aType: Char;
  aFound: Boolean;
  tmp: String;
  aMS: TMemoryStream;
begin
  TSchemeList(DataSet).OpenItem;
  FEditable := ((Data.Users.Rights.Right('PROJECTS') > RIGHT_READ));

  cbStatus.Items.Clear;
  cbStatus.Text := '';
  aType := 'D';
  if not Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      Data.SetFilter(Data.States,'');
      aFound := Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]);
    end
  else aFound := True;
  if aFound then
    begin
      cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
      cbStatus.Text := Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')';
    end
  else cbStatus.Text:=FDataSet.FieldByName('STATUS').AsString;
  tmp := trim(Data.States.FieldByName('DERIVATIVE').AsString);
  if (length(tmp) = 0) or (tmp[length(tmp)] <> ';') then
    tmp := tmp+';';
  if tmp <> ';' then
    begin
      while pos(';',tmp) > 0 do
        begin
          if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,copy(tmp,0,pos(';',tmp)-1)]),[loCaseInsensitive]) then
            cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
        end;
    end
  else
    begin
      Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType));
      with Data.States.DataSet do
        begin
          First;
          while not eof do
            begin
              if cbStatus.Items.IndexOf(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')') = -1 then
                cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
              Next;
            end;
        end;
    end;
  if DataSet.State<>dsInsert then
    begin
      aMS := TMemoryStream.Create;
      Data.BlobFieldToStream(FDataSet.DataSet,'DATA',aMS);
      aMS.Position:=0;
      FGraph.LoadFromStream(aMS);
      FDataSet.CascadicPost;
    end;
end;

function TfShemeFrame.SetRights: Boolean;
begin
  FEditable := ((Data.Users.Rights.Right('SCHEMES') > RIGHT_READ));
  Result := FEditable;
  acDelete.Enabled:=FEditable and (Data.Users.Rights.Right('PROJECTS') > RIGHT_WRITE);
  acPaste.Enabled:=FEditable;
  acRights.Enabled:=Data.Users.Rights.Right('PROJECTS') >= RIGHT_PERMIT;
  eName.Enabled:=FEditable;
  cbStatus.Enabled:=FEditable;
  eName.Enabled:=FEditable;

  FGraph.Enabled := FEditable;
end;

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

  FGraph.OnObjectDblClick:=@goDblClick;
  //FGraph.OnDblClick := @sgDblClick;
  FGraph.FixedScrollBars := True;
  FGraph.OnObjectChange:=@FGraphObjectChange;
end;

procedure TfShemeFrame.New;
begin
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  TabCaption := strNewScheme;
  DataSet := TSchemeList.CreateEx(Self,Data,FConnection);
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
end;

function TfShemeFrame.OpenFromLink(aLink: string): Boolean;
begin
  Result := False;
  if not ((copy(aLink,0,pos('@',aLink)-1) = 'SCHEME')
  or (copy(aLink,0,pos('@',aLink)-1) = 'SCHEME.ID')) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  DataSet := TSchemeList.CreateEx(Self,Data,FConnection);
  if TbaseDbList(DataSet).SelectFromLink(aLink) then
    FDataSet.Open;
  if FDataSet.Count > 0 then
    begin
      TabCaption := TBaseDbList(FDataSet).Text.AsString;
      DoOpen;
      Result := True;
    end;
end;

procedure TfShemeFrame.SetLanguage;
begin

end;

end.

