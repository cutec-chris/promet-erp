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
  uPrometFramesInplace, usimplegraph, Graphics, Menus, ExtDlgs, uBaseDbClasses,
  variants, Utils;

type

  { TfShemeFrame }

  TfShemeFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acClose: TAction;
    acCopy: TAction;
    acDelete: TAction;
    acExport: TAction;
    acImport: TAction;
    acPaste: TAction;
    acPrint: TAction;
    acRights: TAction;
    acSave: TAction;
    acSetTreeDir: TAction;
    acShowTreeDir: TAction;
    acStartTimeRegistering: TAction;
    acEdit: TAction;
    acDeleteElement: TAction;
    acProperties: TAction;
    acOpen: TAction;
    acExportToImage: TAction;
    ZoomIn: TAction;
    ZoomOut: TAction;
    Bevel11: TBevel;
    Bevel4: TBevel;
    bSave1: TSpeedButton;
    EditLock: TAction;
    ActionList: TActionList;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel10: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    bExecute: TSpeedButton;
    bMenue2: TSpeedButton;
    bMenue4: TSpeedButton;
    cbActive: TDBCheckBox;
    cbStatus: TComboBox;
    cbVersion: TComboBox;
    ClipboardBitmap: TAction;
    ClipboardMetafile: TAction;
    ClipboardNative: TAction;
    CoolBar1: TToolBar;
    CoolBar2: TToolBar;
    EditAlign: TAction;
    EditBringToFront: TAction;
    EditInvertSelection: TAction;
    EditMakeAllSelectable: TAction;
    EditProperties: TAction;
    EditSelectAll: TAction;
    EditSendToBack: TAction;
    EditSize: TAction;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel4: TExtRotatedLabel;
    ExtRotatedLabel5: TExtRotatedLabel;
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
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LinkAddPoint: TAction;
    LinkGrow: TAction;
    LinkRemovePoint: TAction;
    LinkReverse: TAction;
    LinkRotateCCW: TAction;
    LinkRotateCW: TAction;
    LinkShrink: TAction;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miCopy: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miStartTimeregistering: TMenuItem;
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
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    pmAction: TPopupMenu;
    pmContext: TPopupMenu;
    pToolbar: TPanel;
    SavePictureDialog1: TSavePictureDialog;
    sbMenue1: TSpeedButton;
    sbMenue2: TSpeedButton;
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
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TSpeedButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
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
    procedure acCancelExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acDeleteElementExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acExportToImageExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure EditBringToFrontExecute(Sender: TObject);
    procedure EditLockExecute(Sender: TObject);
    procedure EditSendToBackExecute(Sender: TObject);
    procedure FGraphMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGraphObjectMouseEnter(Graph: TEvsSimpleGraph;
      GraphObject: TEvsGraphObject);
    procedure FGraphObjectMouseLeave(Graph: TEvsSimpleGraph;
      GraphObject: TEvsGraphObject);
    procedure FormatAlignBottomExecute(Sender: TObject);
    procedure FormatAlignLeftExecute(Sender: TObject);
    procedure FormatAlignRightExecute(Sender: TObject);
    procedure FormatAlignTopExecute(Sender: TObject);
    procedure FormatVCenterExecute(Sender: TObject);
    procedure goDblClick(Graph: TEvsSimpleGraph; GraphObject: TEvsGraphObject);
    procedure ObjectsBezierExecute(Sender: TObject);
    procedure ObjectsLinkExecute(Sender: TObject);
    procedure ObjectsNoneExecute(Sender: TObject);
    procedure ObjectsPentagonExecute(Sender: TObject);
    procedure ObjectsRectangleExecute(Sender: TObject);
    procedure ObjectsRhomboidExecute(Sender: TObject);
    procedure ObjectsRoundRectExecute(Sender: TObject);
    procedure ObjectsTriangleExecute(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
    procedure SchemeStateChange(Sender: TObject);
    procedure ZoomInExecute(Sender: TObject);
    procedure ZoomOutExecute(Sender: TObject);
  private
    { private declarations }
    FEditable: Boolean;
    FGraph: TEvsSimpleGraph;
    procedure FGraphObjectChange(Graph: TEvsSimpleGraph;
      GraphObject: TEvsGraphObject);
    function OnCalcText(Sender: TObject; aCanvas: TCanvas; const aText: string;
      const Rect: TRect): string;
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
    procedure DoOpen;override;
    function SetRights : Boolean;
  protected
    function ForEachCallback(GraphObject: TEvsGraphObject; UserData: integer
      ): boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure New;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure SetLanguage; override;
  end;

procedure AddToMainTree(aAction : TAction;Node : TTreeNode);

implementation

uses uData,uBaseDBInterface,uMainTreeFrame,uscheme,uIntfStrConsts,uSchemenodeproperties,
  uschemelinkproperties,Dialogs,uscriptimport,uthumbnails;

resourcestring
  strNewScheme                     = 'Neues Schema';
const
  // ForEachObject Actions
  FEO_DELETE             = 00;
  FEO_SELECT             = 01;
  FEO_INVERTSELECTION    = 02;
  FEO_SENDTOBACK         = 03;
  FEO_BRINGTOFRONT       = 04;
  FEO_MAKESELECTABLE     = 05;
  FEO_SETFONTFACE        = 06;
  FEO_SETFONTSIZE        = 07;
  FEO_SETFONTBOLD        = 08;
  FEO_SETFONTITALIC      = 09;
  FEO_SETFONTUNDERLINE   = 10;
  FEO_RESETFONTBOLD      = 11;
  FEO_RESETFONTITALIC    = 12;
  FEO_RESETFONTUNDERLINE = 13;
  FEO_SETALIGNMENTLEFT   = 14;
  FEO_SETALIGNMENTCENTER = 15;
  FEO_SETALIGNMENTRIGHT  = 16;
  FEO_SETLAYOUTTOP       = 17;
  FEO_SETLAYOUTCENTER    = 18;
  FEO_SETLAYOUTBOTTOM    = 19;
  FEO_REVERSEDIRECTION   = 20;
  FEO_ROTATE90CW         = 21;
  FEO_ROTATE90CCW        = 22;
  FEO_GROW25             = 23;
  FEO_SHRINK25           = 24;

procedure AddToMainTree(aAction: TAction; Node: TTreeNode);
var
  Node1: TTreeNode;
begin
  if (Data.Users.Rights.Right('SCHEME') > RIGHT_VIEW) then
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

function TfShemeFrame.OnCalcText(Sender: TObject; aCanvas: TCanvas; const aText: string;
  const Rect: TRect): string;
var
  tmpText: String;
begin
  tmpText := aText;
  if pos('[Link:',aText)>0 then
    tmpText :=copy(aText,0,pos('[Link:',aText)-1);
  Result := MinimizeText(aCanvas,tmpText,Rect);
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
  aThumb: TThumbnails;
  aStream: TMemoryStream;
begin
  FDataSet.Edit;
  aMS := TMemoryStream.Create;
  FGraph.SaveToStream(aMS);
  aMS.Position:=0;
  Data.StreamToBlobField(aMS,FDataSet.DataSet,'DATA');
  Save;
  aThumb := TThumbnails.Create(nil);
  try
    aThumb.SelectByRefId(DataSet.Id.AsVariant);
    aThumb.Open;
    while aThumb.Count>0 do
      aThumb.Delete;
    aThumb.Append;
    aThumb.FieldByName('REF_ID_ID').AsVariant:=DataSet.Id.AsVariant;
    FGraph.SaveAsBitmap(GetTempDir+'ptemp.bmp');
    aStream := TMemoryStream.Create;
    if uthumbnails.GenerateThumbNail(GetTempDir+'ptemp.bmp',GetTempDir+'ptemp.bmp',aStream,'') then
      begin
        if aStream.Size>0 then
          Data.StreamToBlobField(aStream,aThumb.DataSet,'THUMBNAIL');
        aThumb.Post;
      end;
    aStream.Free;
    DeleteFile(GetTempDir+'ptemp.bmp');
  finally
    aThumb.Free;
  end;
end;

procedure TfShemeFrame.EditBringToFrontExecute(Sender: TObject);
var
  vCntr : Integer;
begin
  for vCntr := 0 to FGraph.SelectedObjects.Count -1 do
     FGraph.SelectedObjects[vCntr].BringToFront;
end;

procedure TfShemeFrame.EditLockExecute(Sender: TObject);
var
  vCntr: Integer;
begin
//  for vCntr := 0 to FGraph.SelectedObjects.Count -1 do
//     FGraph.SelectedObjects[vCntr].LoadFromStream();:=True;
end;

procedure TfShemeFrame.EditSendToBackExecute(Sender: TObject);
var
  vCntr : Integer;
begin
  for vCntr := 0 to FGraph.SelectedObjects.Count -1 do
     FGraph.SelectedObjects[vCntr].SendToBack;
end;

procedure TfShemeFrame.FGraphMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  GraphObject: TEvsGraphObject;
  aLink: String;
begin
  if (not acedit.Checked) and Assigned(FGraph.ObjectAtCursor) then
    begin
      GraphObject := FGraph.ObjectAtCursor;
      if pos('[Link:',GraphObject.Text)>0 then
        begin
          aLink := copy(GraphObject.Text,pos('[Link:',GraphObject.Text)+6,length(GraphObject.Text));
          aLink := copy(aLink,0,length(aLink)-1);
          Data.GotoLink(aLink);
        end;
    end;
end;

procedure TfShemeFrame.FGraphObjectMouseEnter(Graph: TEvsSimpleGraph;
  GraphObject: TEvsGraphObject);
begin
  if pos('[Link:',GraphObject.Text)>0 then
    Screen.Cursor:=crHandPoint;
end;

procedure TfShemeFrame.FGraphObjectMouseLeave(Graph: TEvsSimpleGraph;
  GraphObject: TEvsGraphObject);
begin
  if pos('[Link:',GraphObject.Text)>0 then
    Screen.Cursor:=crHandFlat;
end;

procedure TfShemeFrame.FormatAlignBottomExecute(Sender: TObject);
begin
  FormatAlignTop.Checked := True;
  FGraph.ForEachObject(@ForEachCallback, FEO_SETLAYOUTBOTTOM, True);
end;

procedure TfShemeFrame.FormatAlignLeftExecute(Sender: TObject);
begin
  FGraph.ForEachObject(@ForEachCallback, FEO_SETALIGNMENTLEFT, True);
end;

procedure TfShemeFrame.FormatAlignRightExecute(Sender: TObject);
begin
  FGraph.ForEachObject(@ForEachCallback, FEO_SETALIGNMENTRIGHT, True);
end;

procedure TfShemeFrame.FormatAlignTopExecute(Sender: TObject);
begin
  FGraph.ForEachObject(@ForEachCallback, FEO_SETLAYOUTTOP, True);
end;

procedure TfShemeFrame.FormatVCenterExecute(Sender: TObject);
begin
  FGraph.ForEachObject(@ForEachCallback, FEO_SETLAYOUTCENTER, True);
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

procedure TfShemeFrame.ObjectsBezierExecute(Sender: TObject);
begin
  FGraph.CommandMode := cmInsertLink;
  FGraph.DefaultLinkClass := TEVSBezierLink;
end;

procedure TfShemeFrame.acCancelExecute(Sender: TObject);
begin
  Abort;
end;

procedure TfShemeFrame.acCopyExecute(Sender: TObject);
begin
  FGraph.CopyToClipboard(True);
end;

procedure TfShemeFrame.acEditExecute(Sender: TObject);
begin
  FGraph.LockLinks := not acEdit.Checked;
  FGraph.LockNodes := not acEdit.Checked;
  if acEdit.Checked then
    begin
      if not FEditable then
        FGraph.CommandMode := cmViewOnly
      else
        FGraph.CommandMode := cmEdit;
    end
  else
    begin
      FGraph.ClearSelection;
      FGraph.CommandMode := cmPan;
    end;
end;

procedure TfShemeFrame.acExportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icExport,'D',FDataSet) then
    DataSet.DataSet.Refresh;
end;

procedure TfShemeFrame.acExportToImageExecute(Sender: TObject);
var
  aPic: TPicture;
begin
  if SavePictureDialog1.Execute then
    begin
      FGraph.SaveAsBitmap(GetTempDir+'ptemp.bmp');
      aPic := TPicture.Create;
      aPic.LoadFromFile(GetTempDir+'ptemp.bmp');
      DeleteFile(GetTempDir+'ptemp.bmp');
      aPic.SaveToFile(SavePictureDialog1.FileName);
      aPic.Free;
    end;
end;

procedure TfShemeFrame.acImportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icImport,'D',FDataSet) then
    DataSet.DataSet.Refresh;
end;

procedure TfShemeFrame.acOpenExecute(Sender: TObject);
var
  GraphObject: TEvsGraphObject;
  aLink: String;
begin
  GraphObject := FGraph.ObjectAtCursor;
  if not Assigned(GraphObject) then exit;
  if pos('[Link:',GraphObject.Text)>0 then
    begin
      aLink := copy(GraphObject.Text,pos('[Link:',GraphObject.Text)+6,length(GraphObject.Text));
      aLink := copy(aLink,0,length(aLink)-1);
      Data.GotoLink(aLink);
    end;
end;

procedure TfShemeFrame.acPasteExecute(Sender: TObject);
begin
  FGraph.PasteFromClipboard;
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

procedure TfShemeFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfShemeFrame.SchemeStateChange(Sender: TObject);
var
  aEnabled: Boolean;
begin
  aEnabled := DataSet.CanEdit or DataSet.Changed;
  acSave.Enabled := aEnabled;
  acCancel.Enabled:= aEnabled;
end;

procedure TfShemeFrame.ZoomInExecute(Sender: TObject);
begin
  if FGraph.Zoom<200 then
    FGraph.Zoom := FGraph.Zoom+10;
end;

procedure TfShemeFrame.ZoomOutExecute(Sender: TObject);
begin
  if FGraph.Zoom>10 then
    FGraph.Zoom := FGraph.Zoom-10;
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
  try
    if Assigned(FDataSet) then
      DataSet.Edit;
  except
  end;
end;

procedure TfShemeFrame.DoOpen;
var
  aType: Char;
  aFound: Boolean;
  tmp: String;
  aMS: TMemoryStream;
begin
  inherited;
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

      FGraph.LockNodes:=True;
      FGraph.LockLinks:=True;
      acEdit.Checked:=not FGraph.LockNodes;
      FGraph.CommandMode := cmPan;
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

function TfShemeFrame.ForEachCallback(GraphObject: TEvsGraphObject;
  UserData: integer): boolean;
var
  RotateOrg: TPoint;
begin
  Result := True;
  case UserData of
    FEO_DELETE:
      Result := GraphObject.Delete;
    FEO_SELECT:
      GraphObject.Selected := True;
    FEO_INVERTSELECTION:
      GraphObject.Selected := not GraphObject.Selected;
    FEO_SENDTOBACK:
      GraphObject.SendToBack;
    FEO_BRINGTOFRONT:
      GraphObject.BringToFront;
    FEO_MAKESELECTABLE:
      GraphObject.Options := GraphObject.Options + [goSelectable];
    FEO_SETFONTBOLD:
      GraphObject.Font.Style := GraphObject.Font.Style + [fsBold];
    FEO_SETFONTITALIC:
      GraphObject.Font.Style := GraphObject.Font.Style + [fsItalic];
    FEO_SETFONTUNDERLINE:
      GraphObject.Font.Style := GraphObject.Font.Style + [fsUnderline];
    FEO_RESETFONTBOLD:
      GraphObject.Font.Style := GraphObject.Font.Style - [fsBold];
    FEO_RESETFONTITALIC:
      GraphObject.Font.Style := GraphObject.Font.Style - [fsItalic];
    FEO_RESETFONTUNDERLINE:
      GraphObject.Font.Style := GraphObject.Font.Style - [fsUnderline];
    FEO_SETALIGNMENTLEFT:
      if GraphObject is TEvsGraphNode then
        TEvsGraphNode(GraphObject).Alignment := taLeftJustify;
    FEO_SETALIGNMENTCENTER:
      if GraphObject is TEvsGraphNode then
        TEvsGraphNode(GraphObject).Alignment := taCenter;
    FEO_SETALIGNMENTRIGHT:
      if GraphObject is TEvsGraphNode then
        TEvsGraphNode(GraphObject).Alignment := taRightJustify;
    FEO_SETLAYOUTTOP:
      if GraphObject is TEvsGraphNode then
        TEvsGraphNode(GraphObject).Layout := tlTop;
    FEO_SETLAYOUTCENTER:
      if GraphObject is TEvsGraphNode then
        TEvsGraphNode(GraphObject).Layout := tlCenter;
    FEO_SETLAYOUTBOTTOM:
      if GraphObject is TEvsGraphNode then
        TEvsGraphNode(GraphObject).Layout := tlBottom;
    FEO_REVERSEDIRECTION:
      if GraphObject is TEvsGraphLink then
        TEvsGraphLink(GraphObject).Reverse;
    FEO_ROTATE90CW:
      if GraphObject is TEvsGraphLink then
        with TEvsGraphLink(GraphObject) do
        begin
          RotateOrg := CenterOfPoints(Polyline);
          Rotate(+Pi / 2, RotateOrg);
        end;
    FEO_ROTATE90CCW:
      if GraphObject is TEvsGraphLink then
        with TEvsGraphLink(GraphObject) do
        begin
          RotateOrg := CenterOfPoints(Polyline);
          Rotate(-Pi / 2, RotateOrg);
        end;
    FEO_GROW25:
      if GraphObject is TEvsGraphLink then
        TEvsGraphLink(GraphObject).Scale(1.25);
    FEO_SHRINK25:
      if GraphObject is TEvsGraphLink then
        TEvsGraphLink(GraphObject).Scale(0.75);
  else
    Result := False;
  end;
end;

procedure TfShemeFrame.acDeleteElementExecute(Sender: TObject);
begin
  FGraph.ForEachObject(@ForEachCallback, FEO_DELETE, True);
end;

procedure TfShemeFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      Application.ProcessMessages;
      FDataSet.CascadicCancel;
      DataSet.Delete;
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
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
  FGraph.SnapToGrid:=False;
  FGraph.OnObjectDblClick:=@goDblClick;
  FGraph.OnMouseUp:=@FGraphMouseUp;
  FGraph.OnObjectMouseEnter:=@FGraphObjectMouseEnter;
  FGraph.OnObjectMouseLeave:=@FGraphObjectMouseLeave;
  //FGraph.FixedScrollBars := True;
  FGraph.OnObjectChange:=@FGraphObjectChange;
  FGraph.PopupMenu := pmContext;
  FGraph.OnCalcText:=@OnCalcText;
end;

destructor TfShemeFrame.Destroy;
begin
  FGraph.OnObjectChange:=nil;
  inherited Destroy;
end;

procedure TfShemeFrame.New;
begin
  inherited;
  TabCaption := strNewScheme;
  DataSet := TSchemeList.CreateEx(Self,Data,FConnection);
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
end;

function TfShemeFrame.OpenFromLink(aLink: string): Boolean;
begin
  inherited;
  Result := False;
  if not ((copy(aLink,0,pos('@',aLink)-1) = 'SCHEME')
  or (copy(aLink,0,pos('@',aLink)-1) = 'SCHEME.ID')) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
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

