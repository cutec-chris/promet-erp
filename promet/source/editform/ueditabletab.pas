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
Created 18.03.2013
*******************************************************************************}
unit uEditableTab;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Buttons, ExtCtrls,
  StdCtrls, DbCtrls, ObjectInspector, JvDesignSurface, JvDesignImp, PropEdits,
  typinfo, JvDesignUtils, DBZVDateTimePicker, uPrometFrames, Dialogs,GraphPropEdits,
  uIntfStrConsts;

type
  TUnlockedDesigner = class(TJvDesignSurface);

  { TEditableFrame }

  TEditableFrame = class(TFrame)
    bDelete: TSpeedButton;
    ButtonButton: TToolButton;
    ImageButton: TToolButton;
    ImageButton1: TToolButton;
    ImageButton2: TToolButton;
    ImageButton3: TToolButton;
    ImageButton4: TToolButton;
    ImageButton5: TToolButton;
    ImageList1: TImageList;
    JvDesignPanel1: TJvDesignPanel;
    JvDesignSurface: TJvDesignSurface;
    LabelButton: TToolButton;
    PropertyGridPanel: TPanel;
    PanelButton: TToolButton;
    bClose: TSpeedButton;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    procedure aButtonClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure ButtonButtonClick(Sender: TObject);
    procedure EditableFrameInitIDEFileDialog(AFileDialog: TFileDialog);
    procedure EditableFrameStoreIDEFileDialog(AFileDialog: TFileDialog);
    procedure JvDesignPanel1GetAddClass(Sender: TObject; var ioClass: string);
    procedure JvDesignPanel1Paint(Sender: TObject);
    procedure JvDesignPanel1SelectionChange(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure JvDesignPanel1SurfaceBeforeClearSelection(Sender: TObject);
    procedure WriterFindAncestor(Writer: TWriter; Component: TComponent;
      const aName: string; var Ancestor, RootAncestor: TComponent);
  private
    { private declarations }
    FTab : TTabSheet;
    TheObjectInspector: TObjectInspectorDlg;
    ThePropertyEditorHook: TPropertyEditorHook;
    PropertyGrid: TOIPropertyGrid;
    StickyClass: Boolean;
    DesignClass: String;
    FEnterButton : TBitBtn;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupTabEditor(aFrame : TTabSheet);
  end;

implementation
uses uBaseVisualControls,uExtControls,uData,Utils,IDEDialogs;
procedure TEditableFrame.aButtonClick(Sender: TObject);
var
  aFrame: TEditableFrame = nil;
  i: Integer;
  aCreated: Boolean = False;
  aFEnterButton: TBitBtn;
  aFTab: TTabSheet;
begin
  aFEnterButton := TBitBtn(Sender);
  aFTab := TTabSheet(TBitBtn(Sender).Parent.Parent);
  if not Assigned(aFTab) then exit;
  aFEnterButton.Parent.Parent:=nil;
  for i := 0 to TTabSheet(aFTab).ControlCount-1 do
    if TTabSheet(aFTab).Controls[i] is TEditableFrame then
      begin
        aFrame := TEditableFrame(TTabSheet(aFTab).Controls[i]);
        break;
      end;
  if not Assigned(aFrame) then
    begin
      aFrame := TEditableFrame.Create(nil);
      aCreated := True;
    end;
  Self := aFrame;
  FEnterButton := aFEnterButton;
  FTab := aFTab;
  i := 0;
  while i < FTab.ControlCount do
    begin
      if (FTab.Controls[i] is TFrame) then
        FTab.Controls[i].Visible:=False;
      if ((FTab.Controls[i] is TWinControl) or (FTab.Controls[i] is TGraphicControl))
      and (not (FTab.Controls[i] is TFrame))
      and (FTab.Controls[i] <> Sender)
      then
        TWinControl(FTab.Controls[i]).Parent := JvDesignPanel1
      else inc(i);
    end;
  aFrame.Parent := FTab;
  aFrame.Align := alClient;
  aFrame.Show;
  JvDesignSurface.Active:=True;
  if aCreated then
    while (ThePropertyEditorHook.LookupRoot is TWincontrol) and (Assigned(TWinControl(ThePropertyEditorHook.LookupRoot).Parent)) do
      begin
        ThePropertyEditorHook.LookupRoot := TWinControl(ThePropertyEditorHook.LookupRoot).Parent;
        //debugln(TComponent(ThePropertyEditorHook.LookupRoot).Name);
        if ThePropertyEditorHook.LookupRoot is TPrometMainFrame then break;
      end;
end;

procedure TEditableFrame.bDeleteClick(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Data.SetFilter(Data.Forms,Data.QuoteField('TYPE')+'='+Data.QuoteValue(TExtMenuPageControl(FTab.PageControl).TabTypes));
      if Data.Forms.DataSet.Locate('NAME',FTab.Caption,[]) then
        begin
          Data.Forms.Delete;
        end;
      fTab.Free;
    end;
end;

procedure TEditableFrame.ButtonButtonClick(Sender: TObject);
const
  cClasses: array[0..9] of string = ( '','TButton','TLabel', 'TPanel','TImage','TDBEdit','TDBComboBox','TDBMemo','TDBZVDateTimePicker','TDBCheckBox');
var
  aNewControl: TControl;
begin
  StickyClass := False;
  DesignClass := cClasses[TToolButton(Sender).ImageIndex];
  aNewControl := TControlClass(GetClass(DesignClass)).Create(fTab);
  aNewControl.Parent := JvDesignPanel1;
  aNewControl.Name:=DesignUniqueName(aNewControl,DesignClass);
  FEnterButton.Click;
end;

procedure TEditableFrame.EditableFrameInitIDEFileDialog(AFileDialog: TFileDialog
  );
begin

end;

procedure TEditableFrame.EditableFrameStoreIDEFileDialog(
  AFileDialog: TFileDialog);
begin

end;

procedure TEditableFrame.JvDesignPanel1GetAddClass(Sender: TObject;
  var ioClass: string);
var
  aNewControl: TControl;
begin
  ioClass := '';//DesignClass;
  if not StickyClass then
  begin
    DesignClass := '';
//    SelectButton.Down  := true;
  end;
end;
procedure TEditableFrame.JvDesignPanel1Paint(Sender: TObject);
begin
  with JvDesignPanel1 do
     DesignPaintGrid(Canvas, ClientRect, Color);
end;

procedure TEditableFrame.JvDesignPanel1SelectionChange(Sender: TObject);
var
  Selection: TPersistentSelectionList;
  i: Integer;
begin
  Selection:=TPersistentSelectionList.Create;
  for i := low(JvDesignSurface.Selected) to high(JvDesignSurface.Selected) do
    Selection.Add(TWincontrol(JvDesignSurface.Selected[i]));
  PropertyGrid.Selection:=Selection;
  Selection.Free;
end;

procedure TEditableFrame.bCloseClick(Sender: TObject);
var
  i: Integer;
  Stream: TMemoryStream;
  Writer: TWriter;
  aControl: TControl;
begin
  randomize;
  fTab.Name:='C'+ValidateFileName(copy(fTab.Name,0,50))+IntToStr(Random(255));
  i := 0;
  while i < FTab.ComponentCount do
    begin
      if (FTab.Components[i] is TWinControl) and (not TWinControl(FTab.Components[i]).Visible) then
        FTab.Components[i].Free
      else inc(i);
    end;
  for i := 0 to FTab.ComponentCount-1 do
    begin
      try
        if copy(fTab.Components[i].Name,0,50)='' then
          fTab.Components[i].Name:='C'+copy(fTab.Components[i].Name,0,50)+IntToStr(Random(255))
        else
          fTab.Components[i].Name:=copy(fTab.Components[i].Name,0,50)+IntToStr(Random(255));
      except
      end;
    end;
  JvDesignSurface.Active:=False;
  //Self := TEditableFrame(FTab);
  i := 0;
  while i < JvDesignPanel1.ControlCount do
    begin
      aControl :=JvDesignPanel1.Controls[i];
      if (aControl is TWinControl)
      or (aControl is TGraphicControl)
      then
        begin
          aControl.Parent := FTab;
        end
      else
        inc(i);
    end;
  Hide;
  if FTab is TTabSheet then
    if FTab.PageControl is TExtMenuPageControl then
      begin
        Data.SetFilter(Data.Forms,Data.QuoteField('TYPE')+'='+Data.QuoteValue(TExtMenuPageControl(FTab.PageControl).TabTypes));
        if Data.Forms.DataSet.Locate('NAME',FTab.Caption,[]) then
          begin
            FEnterButton.Parent.Parent:=nil;
            Stream := TMemoryStream.Create;
            Stream.WriteComponent(FTab);
            Stream.Position:=0;
            Data.StreamToBlobField(Stream,Data.Forms.DataSet,'FORM');
            Stream.Free;
          end;
      end;
  FEnterButton.Parent.Parent := FTab;
end;

procedure TEditableFrame.JvDesignPanel1SurfaceBeforeClearSelection(
  Sender: TObject);
begin
  PropertyGrid.Selection:=nil;
end;

procedure TEditableFrame.WriterFindAncestor(Writer: TWriter;
  Component: TComponent; const aName: string; var Ancestor,
  RootAncestor: TComponent);
begin
  //debugln(Name);
end;

constructor TEditableFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  InitIDEFileDialog:=@EditableFrameInitIDEFileDialog;
  StoreIDEFileDialog:=@EditableFrameStoreIDEFileDialog;
  GlobalDesignHook := TPropertyEditorHook.Create;
  ThePropertyEditorHook:=TPropertyEditorHook.Create;

  PropertyGrid:=TOIPropertyGrid.CreateWithParams(Self,ThePropertyEditorHook
      ,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      {, tkArray, tkRecord}, tkInterface, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord],
      25);
  with PropertyGrid do begin
    Name:='PropertyGrid';
    Parent:=PropertyGridPanel;
    Align:=alClient;
  end;
  ThePropertyEditorHook.LookupRoot:=JvDesignPanel1;
end;

destructor TEditableFrame.Destroy;
begin
  FreeAndNil(GlobalDesignHook);
  ThePropertyEditorHook.Free;
  PropertyGrid.Free;
  inherited Destroy;
end;

procedure TEditableFrame.SetupTabEditor(aFrame: TTabSheet);
var
  aButton: TBitBtn;
  aPanel: TPanel;
begin
  aPanel := TPanel.Create(aFrame);
  aPanel.Left:=aFrame.Width-22;
  aPanel.Top := 0;
  aPanel.Width := 22;
  aPanel.Height := 22;
  aPanel.Align:=alRight;
  aPanel.BevelInner:=bvNone;
  aPanel.BevelOuter:=bvNone;
  aPanel.Parent:=aFrame;
  aButton := TBitBtn.Create(aFrame);
  aButton.Parent := aPanel;
  aButton.OnClick:=@aButtonClick;
  aButton.Glyph := nil;
  aButton.Align := alClient;
  fVisualControls.Images.GetBitmap(67,aButton.Glyph);
  aPanel.Parent:=aFrame;//TWinControl(aFrame.Controls[0]);
  //TWinControl(aFrame.Controls[0]).SendToBack;
end;

initialization
  RegisterClass(TSpeedButton);
  RegisterClass(TButton);
  RegisterClass(TLabel);
  RegisterClass(TPanel);
  RegisterClass(TImage);
  RegisterClass(TDbEdit);
  RegisterClass(TDBComboBox);
  RegisterClass(TDBMemo);
  RegisterClass(TDBZVDateTimePicker);
  RegisterClass(TDBCheckBox);
  {$R *.lfm}
end.

