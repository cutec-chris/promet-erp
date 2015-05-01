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
Created 03.12.2011
*******************************************************************************}
unit ugridview;
{$mode objfpc}{$H+}

{.$define gridvisible}
{.$define slowdebug}
{.$define debug}

interface
uses
  Classes, SysUtils, Utils,  Forms, Controls, DBGrids, ExtCtrls,
  Buttons, ComCtrls, uExtControls, db, Grids, ActnList, Menus, uBaseDBClasses,
  uBaseDbInterface, StdCtrls, Graphics, types, Clipbrd, LMessages,
  ubasevisualapplicationtools, ZVDateTimePicker, Dialogs, DbCtrls, EditBtn,uBaseDatasetInterfaces;
type
  TUnprotectedGrid = class(TCustomGrid);

  { TInplaceMemo }

  TInplaceMemo = class(TMemo)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FSetValue: Boolean;
  protected
    procedure RealSetText(const Value: TCaption); override;
    procedure ShowControl(AControl: TControl); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure Change;override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
    procedure msg_SetBounds(var Msg: TGridMessage); message GM_SETBOUNDS;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure WMPaste(var Message: TLMPaste); message LM_PASTE;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
    property SetValue : Boolean read FSetValue write FSetValue;
  end;
  TRowObject = class
  private
    function GetStringRec: string;
  public
    Rec : Int64;
    Childs : Char;
    Level : Integer;
    Seen : string;
    Dependencies : Boolean;
    ShouldStart : TDateTime;
    NeedsAction : string;
    RefreshHeight : Boolean;
    Extends : TPoint;
    property StringRec : string read GetStringRec;
    constructor Create;
  end;
  TColumnWidthHelper = record
    Index : integer;
    MaxWidth : integer;
  end;

  TCellChangedEvent = procedure(Sender : TObject;NewCell,OldCell : TPoint) of object;
  TCellButtonClickEvent = procedure(Sender : TObject;Cell : TPoint;Field : TColumn) of object;
  TSetupPositionEvent = procedure(Sender : TObject;Columns : TGridColumns)  of object;
  TGetCellTextEvent = procedure(Sender : TObject;aCol : TColumn;aRow : Integer;var NewText : string;aFont : TFont) of object;
  TGetRowHeightEvent = procedure(Sender : TObject;aCol : TColumn;aRow : Integer;var aHeight : Integer;var aWidth : Integer) of object;
  TSetCellTextEvent = procedure(Sender : TObject;aCol : TColumn;aRow : Integer;var NewText : string) of object;
  TFieldEvent = procedure(Field : TColumn) of object;
  TSearchKey = function(Sender : TObject;X,Y : Integer;Field : TColumn;var Key : Word;Shift : TShiftState;SearchString : string) : Boolean of object;
  TGridDrawColumnCellEvent = function(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState) : Boolean of object;
  TCellFontEvent = function(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState;var aColor : TColor;var Style : TFontStyles) : Boolean of object;

  { TfGridView }

  TfGridView = class(TFrame)
    acCopyLink: TAction;
    acFilter: TAction;
    acOpen: TAction;
    acSearch: TAction;
    acCopyToClipboard: TAction;
    acChangeRows: TAction;
    acResetFilter: TAction;
    ActionList: TActionList;
    ActionList1: TActionList;
    bRowEditor: TSpeedButton;
    dgFake: TDBGrid;
    FDataSource: TDatasource;
    FilterImage: TImage;
    gHeader: TExtStringgrid;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    miOpen: TMenuItem;
    pConfig: TPanel;
    pFilter: TPanel;
    Panel2: TPanel;
    bEditRows: TSpeedButton;
    pmPopup: TPopupMenu;
    pmHeader: TPopupMenu;
    sbGrids: TPanel;
    gList: TExtStringgrid;
    procedure acChangeRowsExecute(Sender: TObject);
    procedure acCopyLinkExecute(Sender: TObject);
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acResetFilterExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure deDateAcceptDate(Sender: TObject; var ADate: TDateTime;
      var AcceptDate: Boolean);
    procedure dgFakeTitleClick(Column: TColumn);
    procedure DoAsyncRefresh(Data: PtrInt);
    procedure FDataSourceDataChange(Sender: TObject; Field: TField);
    procedure FDataSourceStateChange(Sender: TObject);
    procedure fGridViewEnter(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure gHeaderColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure gHeaderDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure gHeaderEditingDone(Sender: TObject);
    procedure gHeaderEnter(Sender: TObject);
    procedure gHeaderGetCellWidth(aCol: Integer; var aNewWidth: Integer);
    procedure gHeaderHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gHeaderKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure gHeaderPickListSelect(Sender: TObject);
    procedure gHeaderSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure gHeaderSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure gListEnterEdit(Sender: TObject);
    procedure gListExit(Sender: TObject);
    procedure gListResize(Sender: TObject);
    procedure gListSelection(Sender: TObject; aCol, aRow: Integer);
    procedure mInplaceEditingDone(Sender: TObject);
    procedure mInplaceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure mInplaceResize(Sender: TObject);
    procedure gListButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure gListCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure gListClick(Sender: TObject);
    procedure gListColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure gListDblClick(Sender: TObject);
    procedure gListDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure gListEditingDone(Sender: TObject);
    procedure gListGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure gListHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gListMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure gListSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure gListSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure gListSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure SearchKeyTimerTimer(Sender: TObject);
  private
    FFEditPrefix: string;
    procedure SetEditPrefix(AValue: string);
  private
    ColumnWidthHelper : TColumnWidthHelper;
    FAddRow: TNotifyEvent;
    FAfterInsert: TNotifyEvent;
    FApplyAutoFilter: Boolean;
    FAutoFilterChanged: TNotifyEvent;
    FbaseFilter: string;
    FBeforeInsert: TNotifyEvent;
    FBeforInsert: TNotifyEvent;
    FCBToggle: TFieldEvent;
    fDefaultRows: string;
    FDelete: TNotifyEvent;
    FFilterCell: TFilterCellTextEvent;
    FGetRowHeight: TGetRowHeightEvent;
    FInvertedDrawing: Boolean;
    FNumberField: string;
    FOnDrawCellBack: TGridDrawColumnCellEvent;
    FOnGetFontCell: TCellFontEvent;
    FWordwrap: Boolean;
    FWorkStatus: string;
    WasEditing: Boolean;
    { private declarations }
    FEntered : Boolean;
    FExpandSignSize : Integer;
    FAutoFilter : string;
    FActAutoFilter : string;
    FBaseName: string;
    FButtonClick: TCellButtonClickEvent;
    FCellChanged: TCellChangedEvent;
    FCellChanging: TNotifyEvent;
    FCheckIdent: TNotifyEvent;
    FDefaultRowHeight: Boolean;
    FDragDrop: TDragDropEvent;
    FDragOver: TDragOverEvent;
    FExpField: string;
    FGetCText: TGetCellTextEvent;
    FGetEdit: TGetEditEvent;
    FHCField: string;
    FIdentField: string;
    FOnDrawCell: TGridDrawColumnCellEvent;
    FreadOnly: Boolean;
    FSearchKey: TSearchKey;
    FSetCText: TSetCellTextEvent;
    FSetEdit: TSetEditEvent;
    FSetupPosition: TSetupPositionEvent;
    FSortDirection: TSortDirection;
    FSortField: string;
    FActSortDirection: TSortDirection;
    FActSortField: string;
    FInpStringList : TStringList;
    FDataSet : TBaseDBDataSet;
    FSTextField: string;
    FTextField: string;
    FTreeField: string;
    OldRow : Integer;
    OldCol : Integer;
    mInplace : TInplaceMemo;
    FIgnoreSettext : Boolean;
    InEdit: Boolean;
    FDontUpdate : Integer;
    FEditable : Boolean;
    FirstFocused : Boolean;
    aOldSize : Integer;
    FSearchKeyCol: TColumn;
    FSearchKeyKey: Word;
    FSearchKeyVal: String;
    FSearchKeyRect: TRect;
    SearchKeyTimer : TTimer;
    FDisableEdit : Boolean;
    procedure ClearFilters;
    function GetFilterRow: Boolean;
    procedure SetExpField(AValue: string);
    procedure SetFilterRow(AValue: Boolean);
    procedure SetHasChilds(aCol,aRow : Integer;Expanded : char);
    function HasChilds(aCol,aRow : Integer) : Char;
    procedure SetHCField(AValue: string);
    procedure SetLevel(aCol,aRow,aLevel : Integer);
    function GetLevel(aCol,aRow : Integer) : Integer;
    procedure FieldModified(aField : TField);
    function GetActualCell: string;
    function GetActualField: TColumn;
    function GetColumns: TDBGridColumns;
    function GetCount: Integer;
    function  GetRowHeight(aRow : Integer) : TPoint;
    procedure SetBaseFilter(AValue: string);
    procedure SetDataSet(const AValue: TBaseDBDataSet);
    procedure SetDefaultRowHeight(AValue: Boolean);
    procedure SetDefaultRows(AValue: string);
    procedure SetDragDrop(AValue: TDragDropEvent);
    procedure SetDragOver(AValue: TDragOverEvent);
    procedure SetIdentField(AValue: string);
    procedure SetNumberField(AValue: string);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetShortTextField(AValue: string);
    procedure SetSortDirecion(AValue: TSortDirection);
    procedure SetSortField(AValue: string);
    procedure SetTextField(AValue: string);
    procedure SetTreeField(AValue: string);
    procedure TryToMakeEditorVisible;
    procedure UpdateTitle;
    procedure DoSetEdit(Data : PtrInt);
    procedure DoSetEditDD(Data : PtrInt);
    procedure DoInvalidate(Data : PtrInt);
    procedure CleanList(AddRows : Integer);
    procedure CleanRow(aRow : Integer;aIdentCol : Integer);
    procedure Asyncrefresh;
    property FEditPrefix : string read FFEditPrefix write SetEditPrefix;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property  BaseName : string read FBaseName write FBaseName;
    function FindRow(aTreeBM : LargeInt) : Integer;
    function SyncActiveRow(Bookmark : Int64;DoInsert,UpdateData : Boolean;UpdateRowHeight : Boolean = False;DoGroup : Boolean = True;AddNotFound : Boolean = False;aIdentField : Integer = -1) : Boolean;
    procedure SyncDataSource(UpdateHeader : Boolean = True;DoGroup : Boolean = True);
    procedure SetupHeader;
    procedure SetEdited;
    procedure Append;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure EditingDone; override;
    procedure Insert(SetCol : Boolean = False);
    procedure InsertAfter(SetCol : Boolean = False);
    procedure Delete;
    procedure First;
    procedure Post;
    procedure Refresh(RefreshDS : Boolean = True);
    procedure RenumberRows(aIndex : Integer = 0;aOffset : Integer = 0);
    procedure CalculateRowHeights;
    procedure AutoInsert;
    procedure SelectCol(aColName : string);
    procedure ResetEditor;
    procedure SetFocus;override;
    procedure SetRights(Editable : Boolean);
    procedure SetChild(Dorefresh : Boolean = True);
    procedure UnSetChild;
    property BaseFilter : string read FbaseFilter write SetBaseFilter;
    property ApplyAutoFilter : Boolean read FApplyAutoFilter write FApplyAutoFilter;
    function GotoActiveRow : Boolean;
    function GotoRow(aBookmark : LargeInt) : Boolean;
    function GotoRowNumber(aRow : Integer) : Boolean;
    function GotoDataSetRow : Boolean;
    procedure SetActive;
    property ActualCell : string read GetActualCell;
    property ActualColumn : TColumn read GetActualField;
    property Count : Integer read GetCount;
    property UseDefaultRowHeight : Boolean read FDefaultRowHeight write SetDefaultRowHeight;
    property Columns: TDBGridColumns read GetColumns;
    property DefaultRows : string read fDefaultRows write SetDefaultRows;
    property SortDirection : TSortDirection read FSortDirection write SetSortDirecion;
    property WordWrap : Boolean read FWordwrap write FWordwrap;
    property InvertedDrawing : Boolean read FInvertedDrawing write FInvertedDrawing;

    property SortField : string read FSortField write SetSortField;
    property NumberField : string read FNumberField write SetNumberField;
    property TextField : string read FTextField write SetTextField;
    property FilterRow : Boolean read GetFilterRow write SetFilterRow;
    property ShortTextField : string read FSTextField write SetShortTextField;
    property TreeField : string read FTreeField write SetTreeField;
    property HasChildsField : string read FHCField write SetHCField;
    property ExpandedField : string read FExpField write SetExpField;
    property IdentField : string read FIdentField write SetIdentField;
    property WorkStatusField : string read FWorkStatus write FWorkStatus;

    property DataSet : TBaseDBDataSet read FDataSet write SetDataSet;
    property OnCellChanging : TNotifyEvent read FCellChanging write FCellChanging;
    property OnCellChanged : TCellChangedEvent read FCellChanged write FCellChanged;
    property OnCheckIdent : TNotifyEvent read FCheckIdent write FCheckIdent;
    property OnCellButtonClick : TCellButtonClickEvent read FButtonClick write FButtonClick;
    property OnSetupPosition : TSetupPositionEvent read FSetupPosition write FSetupPosition;
    property OnDragOver : TDragOverEvent read FDragOver write SetDragOver;
    property OnDragDrop : TDragDropEvent read FDragDrop write SetDragDrop;
    property OnGetCellText : TGetCellTextEvent read FGetCText write FGetCText;
    property OngetRowHeight : TGetRowHeightEvent read FGetRowHeight write FGetRowHeight;
    property OnSetCellText : TSetCellTextEvent read FSetCText write FSetCText;
    property OnGetEditText : TGetEditEvent read FGetEdit write FGetEdit;
    property OnSetEditText : TSetEditEvent read FSetEdit write FSetEdit;
    property OnFilterCell  : TFilterCellTextEvent read FFilterCell write FFilterCell;
    property OnCheckBoxColumnToggle : TFieldEvent read FCBToggle write FCBToggle;
    property OnSearchKey : TSearchKey read FSearchKey write FSearchKey;
    property BeforeInsert : TNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert : TNotifyEvent read FAfterInsert write FAfterInsert;
    property OnDrawColumnCell : TGridDrawColumnCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetCellFont : TCellFontEvent read FOnGetFontCell write FOnGetFontCell;
    property OnAddRow : TNotifyEvent read FAddRow write FAddRow;
    property ReadOnly : Boolean read FreadOnly write SetReadOnly;
    property OnDelete : TNotifyEvent read FDelete write fDelete;
    property OnAutoFilterChanged : TNotifyEvent read FAutoFilterChanged write FAutoFilterChanged;
    property AutoFilter : string read FAutoFilter write FAutoFilter;
  end;
implementation
{$R *.lfm}
uses uRowEditor,LCLType,LCLProc,LCLIntf,Themes,uIntfStrConsts,
  uData,uBaseVisualApplication,Math;
{ TRowObject }

function TRowObject.GetStringRec: string;
begin
  if Assigned(Self) then
    Result := IntToStr(Rec);
end;

constructor TRowObject.Create;
begin
  Rec := 0;
  Childs:=' ';
  RefreshHeight := True;
  Dependencies:=False;
  NeedsAction:='na';
end;

procedure TfGridView.deDateAcceptDate(Sender: TObject; var ADate: TDateTime;
  var AcceptDate: Boolean);
begin
  TUnprotectedGrid(gList).SetEditText(gList.Col, gList.Row, DateToStr(ADate));
end;

procedure TfGridView.dgFakeTitleClick(Column: TColumn);
begin
  if (Column.Field.DataType = ftMemo)
  or (Column.Field.DataType = ftWideMemo)
  or (Column.Field.DataType = ftBlob)
  then exit;
  FDisableEdit:=True;
  Post;
  if SortField = Column.FieldName then
    begin
      if SortDirection = sdAscending then
        SortDirection := sdDescending
      else if SortDirection = sdDescending then
        begin
          SortDirection := sdIgnored;
          SortField := '';
        end
      else
        SortDirection := sdAscending;
    end
  else
    begin
      SortField := Column.FieldName;
      SortDirection := sdAscending;
    end;
  UpdateTitle;
  acFilter.Execute;
  FDisableEdit:=False;
end;

procedure TfGridView.DoAsyncRefresh(Data: PtrInt);
begin
  Refresh(False);
end;

procedure TfGridView.acFilterExecute(Sender: TObject);
begin
  SetBaseFilter(FbaseFilter);
end;

procedure TfGridView.acOpenExecute(Sender: TObject);
begin

end;

procedure TfGridView.acResetFilterExecute(Sender: TObject);
begin
  ClearFilters;
end;

procedure TfGridView.acSearchExecute(Sender: TObject);
begin
  if Assigned(OnCellButtonClick) then
    OnCellButtonClick(Self,Point(gList.Selection.Left,gList.Selection.Top),dgFake.Columns[gList.Col-1]);
end;

procedure TfGridView.acCopyLinkExecute(Sender: TObject);
var
  aLinks : string;
  Stream: TStringStream;
begin
  if GotoActiveRow then
    begin
      aLinks := Data.BuildLink(DataSet.DataSet)+';';
      Stream := TStringStream.Create(aLinks);
      Clipboard.AddFormat(LinkClipboardFormat,Stream);
      Stream.Free;
    end;
end;

procedure TfGridView.acChangeRowsExecute(Sender: TObject);
begin
  fRowEditor.Execute(FBaseName,FDataSource,dgFake,FBaseName);
  Asyncrefresh;
end;

procedure TfGridView.acCopyToClipboardExecute(Sender: TObject);
begin
  if (gList.Col>0)
  and (gList.Row>0) then
    Clipboard.AsText := gList.Cells[gList.Col,gList.Row];
end;

procedure TfGridView.FDataSourceDataChange(Sender: TObject; Field: TField);
begin
  FieldModified(Field);
end;
procedure TfGridView.FDataSourceStateChange(Sender: TObject);
begin
end;
procedure TfGridView.fGridViewEnter(Sender: TObject);
begin
  if FEntered then exit;
  try
    FEntered := True;
    SyncDataSource;
    if Assigned(DataSet) then
      begin
        DataSet.First;
        GotoDataSetRow;
      end;
  except
  end;
end;
procedure TfGridView.FrameResize(Sender: TObject);
var
  aMult: Extended;
  i: Integer;
begin
  if not Visible then exit;
  if aOldSize = 0 then aOldSize := TPanel(Sender).Width;
  if aOldSize = 0 then exit;
  if Abs(aOldSize-TPanel(Sender).Width) = 0 then exit;//< (gList.Columns.Count*4) then exit;
  aMult := (TPanel(Sender).Width / aOldSize);
  for i := 0 to dgFake.Columns.Count-1 do
    begin
      dgFake.Columns[i].Width:=round(dgFake.Columns[i].Width*aMult);
      if gList.Columns.Count>i then
        gList.Columns[i].Width:=dgFake.Columns[i].Width;
      if gHeader.Columns.Count>i then
        gHeader.Columns[i].Width:=dgFake.Columns[i].Width;
    end;
  aOldSize := TScrollBox(Sender).Width;
end;
procedure TfGridView.gHeaderColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin
  if IsColumn then
    begin
      dgFake.Columns[sIndex-1].Index:=tIndex-1;
      //gList.Columns[sIndex-1].Index:=tIndex-1;
      fRowEditor.SetGridSizes(FBaseName,dgFake.DataSource,dgFake,FBaseName);
      AsyncRefresh;
    end;
end;
procedure TfGridView.gHeaderDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if aRow < 1 then
    begin
      gHeader.DefaultDrawCell(aCol,aRow,aRect,aState);
      exit;
    end;
  with TStringGrid(Sender),Canvas do
    begin
      Pen.Color:=TStringGrid(Sender).Color;
      Rectangle(aRect);
      if Cells[aCol,aRow] <> '' then
        TextOut(aRect.Left + 2, aRect.Top + 2, Cells[aCol,aRow])
      else
        TextOut(aRect.Left + 2, aRect.Top + 2, strNoFilter)
    end;
end;

procedure TfGridView.gHeaderEditingDone(Sender: TObject);
begin
  FAutoFilter := BuildAutoFilter(dgFake,gHeader);
  if Assigned(FAutoFilterChanged) then
    FAutoFilterChanged(Self);
  acFilter.Execute;
  gHeader.Options:=gHeader.Options-[goAlwaysShowEditor];
end;

procedure TfGridView.gHeaderEnter(Sender: TObject);
begin
  gHeader.Options:=gHeader.Options+[goAlwaysShowEditor];
end;

procedure TfGridView.gHeaderGetCellWidth(aCol: Integer; var aNewWidth: Integer);
begin
  ColumnWidthHelper.Index := aCol-1;
  if ColumnWidthHelper.Index < 0 then Exit;
  ColumnWidthHelper.MaxWidth := -1;
  gList.Repaint;
  aNewWidth := 8 + ColumnWidthHelper.MaxWidth;
end;

procedure TfGridView.gHeaderHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if Index = 0 then exit;
  if IsColumn then
    dgFake.OnTitleClick(dgFake.Columns[Index-1]);
end;
procedure TfGridView.gHeaderKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN)
  or (Key = VK_DOWN) then
    begin
      FAutoFilter := BuildAutoFilter(dgFake,gHeader);
      if Assigned(FAutoFilterChanged) then
        FAutoFilterChanged(Self);
      acFilter.Execute;
      if gList.CanFocus then
        gList.SetFocus;
      Key := 0;
    end;
end;

procedure TfGridView.gHeaderPickListSelect(Sender: TObject);
begin
  FAutoFilter := BuildAutoFilter(dgFake,gHeader,FFilterCell);
  if Assigned(FAutoFilterChanged) then
    FAutoFilterChanged(Self);
  acFilter.Execute;
end;

procedure TfGridView.gHeaderSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  gList.Col := aCol;
end;
procedure TfGridView.gHeaderSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FAutoFilter := BuildAutoFilter(dgFake,gHeader,FFilterCell);
  if Assigned(FAutoFilterChanged) then
    FAutoFilterChanged(Self);
end;

procedure TfGridView.gListEnterEdit(Sender: TObject);
begin
  if gList.Editor is TCustomComboBox then
    begin
      Application.ProcessMessages;
      if not (goAlwaysShowEditor in gList.Options) then
        TCustomComboBox(gList.Editor).DroppedDown:=True;
    end;
end;

procedure TfGridView.gListExit(Sender: TObject);
begin
  if not Visible then exit;
  if not Assigned(DataSet) then exit;
  if DataSet.CanEdit and gList.EditorMode then
    begin
      if DataSet.Changed then
        begin
          Post
        end
      else if (FDataSource.DataSet.State = dsInsert) and (not FDataSet.Changed) then
        begin
          CleanRow(gList.RowCount-1,-2);
          gList.RowCount:=gList.RowCount-1;
          FDataSource.DataSet.Cancel;
        end
      else FDataSource.DataSet.Cancel;
    end;
end;

procedure TfGridView.gListResize(Sender: TObject);
begin
  dgFake.Width:=gList.Width;
end;

procedure TfGridView.gListSelection(Sender: TObject; aCol, aRow: Integer);
begin
  {$IFDEF DEBUG}
  debugln('Selection(',IntToStr(aCol),IntToStr(aRow),')');
  {$ENDIF}
end;

procedure TfGridView.mInplaceEditingDone(Sender: TObject);
begin
  TRowObject(gList.Objects[0,gList.Row]).RefreshHeight:=True;
  mInplaceResize(Sender);
end;
procedure TfGridView.mInplaceKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aHeight: Integer;
  aLength: Integer;
  i: Integer;
  aLeave: Boolean = False;
  aRect: Classes.TRect;
  bKey: Word;
  aCur: objpas.Integer;
begin
  if Assigned(FSearchKey) then
    begin
      case Key of
      VK_PRIOR,
      VK_NEXT,
      VK_UP,
      VK_DOWN,
      VK_RETURN,
      VK_ESCAPE:
          begin
            aRect := gList.CellRect(gList.Col,gList.Row);
            bKey := Key;
            if FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],bKey,Shift,'') then
              begin
                Key:=0;
                aLeave:=True;
                SearchKeyTimer.Enabled:=False;
              end;
            case Key of
            VK_DOWN,VK_UP,VK_ESCAPE:Key := bKey;
            end;
          end;
      end;
    end;
  if (Key = VK_RETURN)
  or (Key = VK_BACK)
  or (Key = VK_DELETE)
  then
    begin
      mInplaceResize(Self);
    end
  else if (Key = VK_TAB) then
    aLeave := True
  else if (Key = VK_LEFT) and (mInplace.SelStart = 0) then
    begin
      aLeave := True
    end
  else if (Key = VK_UP) and (mInplace.SelStart <= length(mInplace.Lines[0])) then
    begin
      aLeave := True
    end
  else if (Key = VK_DOWN) or (Key = VK_RIGHT) then
    begin
      aLength := 0;
      for i := 0 to mInplace.Lines.Count-1 do
        inc(aLength,UTF8length(mInplace.Lines[i])+UTF8length(lineending));
      dec(aLength,UTF8length(lineending));
      if ((Key = VK_DOWN) and (mInplace.SelStart >= aLength-(length(mInplace.Lines[mInplace.Lines.Count-1])-1)))
      or ((Key = VK_RIGHT) and (mInplace.SelStart >= aLength-1))
      then
        aLeave := True;
    end
  else if (Key = VK_ESCAPE) then
    begin
      TUnprotectedGrid(gList).KeyDown(Key,Shift);
    end
  else if Key <> 0 then
    begin
      if Assigned(FSearchKey) then
        begin
          FSearchKeyRect := gList.CellRect(gList.Col,gList.Row);
          if gHeader.Visible then
            FSearchKeyRect.Bottom:=FSearchKeyRect.Bottom+gHeader.Height;
          FSearchKeyCol := dgFake.Columns[gList.Col-1];
          FSearchKeyKey := Key;
          FSearchKeyVal := gList.Cells[gList.Col,gList.Row]+chr(key);
          SearchKeyTimer.Enabled := True;
        end;
    end;
  if aLeave then
    begin
      gList.EditorMode:=False;
      TUnprotectedGrid(gList).KeyDown(Key,Shift);
      gList.SetFocus;
    end;
end;
procedure TfGridView.mInplaceResize(Sender: TObject);
var
  aHeight: Integer;
  aEdit: Boolean;
  aCur: objpas.Integer;
begin
  aHeight := ((mInplace.Lines.Count+1)*gList.Canvas.GetTextHeight('A'))+3;
  if gList.DefaultRowHeight > aHeight then
    aHeight := gList.DefaultRowHeight;
  if aHeight > (gList.Height-(gList.FixedRows*gList.DefaultRowHeight)) then
    begin
      aHeight := (gList.Height-(gList.FixedRows*gList.DefaultRowHeight));
      if mInplace.ScrollBars<>ssAutoBoth then
        begin
          aCur := mInplace.SelStart;
          mInplace.ScrollBars:=ssAutoVertical;
          mInplace.SelStart:=aCur;
          if mInplace.Visible then
            mInplace.SetFocus;
        end;
    end
  else
    begin
      TRowObject(gList.Objects[0,gList.Row]).Extends := GetRowHeight(gList.Row);
      TRowObject(gList.Objects[0,gList.Row]).Extends.y:=TRowObject(gList.Objects[0,gList.Row]).Extends.y+(gList.DefaultRowHeight);
      gList.RowHeights[gList.Row] := TRowObject(gList.Objects[0,gList.Row]).Extends.Y;
      TRowObject(gList.Objects[0,gList.Row]).RefreshHeight:=True;
      TryToMakeEditorVisible;
      mInplace.BoundsRect:=gList.CellRect(gList.Col,gList.Row);
      mInplace.Width:=mInplace.Width-1;
    end;
end;
procedure TfGridView.gListButtonClick(Sender: TObject; aCol, aRow: Integer
  );
begin
  if (dgFake.Columns.Count < aCol) or (aCol = -1) then exit;
  if Assigned(FButtonClick) then
    begin
      FButtonClick(Self,Point(aCol,aRow),dgFake.Columns[aCol-1]);
      if (not Focused) and CanFocus and Visible then
        begin
          SetFocus;
        end;
    end;
end;
procedure TfGridView.gListCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if (aCol > 0) then
    begin
      if not GotoActiveRow then exit;
      if (HasChildsField <> '') and (DataSet.FieldByName(HasChildsField).AsString='Y') then exit;
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      if aState = cbChecked then
        dgFake.Columns[aCol-1].Field.AsString:=dgFake.Columns[aCol-1].ValueChecked
      else
        dgFake.Columns[aCol-1].Field.AsString:=dgFake.Columns[aCol-1].ValueUnChecked;
      if Assigned(FCBToggle) then
        FCBToggle(dgFake.Columns[aCol-1]);
      TStringGrid(Sender).InvalidateCell(0,OldRow);
      TStringGrid(Sender).InvalidateCell(0,aRow);
    end;
end;
procedure TfGridView.gListClick(Sender: TObject);
var
  aKey : Word = VK_ESCAPE;
  aRect: Classes.TRect;
begin
  aRect := gList.CellRect(gList.Col,gList.Row);
  if gList.Col>0 then
    if Assigned(FSearchKey) then
      FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
end;
procedure TfGridView.gListColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
var
  i: Integer;
  aIndex: Integer;
  aCol: Integer;
begin
  if FDataSource.DataSet.ControlsDisabled then exit;
  if not FEditable then exit;
  FDisableEdit:=True;
  if IsColumn then
    begin
      dgFake.Columns[sIndex-1].Index:=tIndex-1;
      fRowEditor.SetGridSizes(FBaseName,dgFake.DataSource,dgFake,FBaseName);
      Asyncrefresh;
    end
  else if (sIndex <> tIndex) then
    begin
      if DataSet.CanEdit and gList.EditorMode then
        begin
          if DataSet.Changed then
            Post;
        end;
      for i := 0 to dgFake.Columns.Count-1 do
        if dgFake.Columns[i].FieldName = IdentField then
          begin
            aCol := i+1;
            break;
          end;
      if GotoRowNumber(sIndex) then
        begin
          gList.Row := sIndex;
          if FDataSet.CanEdit then
            FDataSet.DataSet.Post;
          gList.Row:=tIndex;
          //Renumber rows
          aIndex := tIndex;
          if sIndex < aIndex then
            aIndex := sIndex;
          RenumberRows(aIndex);
        end;
    end;
  FDisableEdit:=False;
  Refresh;
end;
procedure TfGridView.gListDblClick(Sender: TObject);
begin
  if Assigned(Self.OnDblClick) then
    begin
      Post;
      if gList.EditorMode then
        gList.EditorMode:=False;
      OnDblClick(Sender);
    end
  else if not gList.EditorMode then
    gList.EditorMode:=True;
end;
procedure TfGridView.gListDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
const
  aTextStyle : TTextStyle = (Alignment:taLeftJustify;
                             Layout : tlTop;
                             SingleLine : False;
                             Clipping  : True;
                             ExpandTabs:False;
                             ShowPrefix:False;
                             Wordbreak:false;
                             Opaque:True;
                             SystemFont:False;
                             RightToLeft:False);
  aTextStyleW : TTextStyle = (Alignment:taLeftJustify;
                             Layout : tlTop;
                             SingleLine : False;
                             Clipping  : True;
                             ExpandTabs:False;
                             ShowPrefix:False;
                             Wordbreak:True;
                             Opaque:True;
                             SystemFont:False;
                             RightToLeft:False);
var
  aColor: TColor;
  aText: string = '';
  bRect: TRect;
  aLevel: Integer;
  i: Integer;
  asCol: Integer;
  aStrlevel: String;
  aDrawed: Boolean;
  aFontStyle : TFontStyles;
  cRect: TRect;
  aNewHeight: Integer;
  UpdatedARow: Boolean;
  procedure DrawExpandSign(MidX, MidY: integer; CollapseSign: boolean);
  const
    PlusMinusDetail: array[Boolean {Hot}, Boolean {Expanded}] of TThemedTreeview =
    (
      (ttGlyphClosed, ttGlyphOpened),
      (ttHotGlyphClosed, ttHotGlyphOpened)
    );
  var
    HalfSize, ALeft, ATop, ARight, ABottom: integer;
    Points: PPoint;
    Details: TThemedElementDetails;
    R: TRect;
    FExpandSignColor : TColor = clWindowFrame;
    FExpandSignType: TTreeViewExpandSignType = tvestTheme;
  begin
    with TStringGrid(Sender).Canvas do
    begin
      Pen.Color := FExpandSignColor;
      Pen.Style := psSolid;
      HalfSize := FExpandSignSize shr 1;
      if ((FExpandSignSize and 1) = 0) then
        dec(HalfSize);
      ALeft := MidX - HalfSize;
      ATop := MidY - HalfSize;
      ARight := ALeft + FExpandSignSize;
      ABottom := ATop + FExpandSignSize;
      case FExpandSignType of
      tvestTheme:
        begin
          // draw a themed expand sign. Todo: track hot
          R := Rect(ALeft, ATop, ARight + 1, ABottom + 1);
          Details := ThemeServices.GetElementDetails(PlusMinusDetail[False, CollapseSign]);
          ThemeServices.DrawElement(TStringGrid(Sender).Canvas.Handle, Details, R, nil);
        end;
      tvestPlusMinus:
        begin
          // draw a plus or a minus sign
          R := Rect(ALeft, ATop, ARight, ABottom);
          Rectangle(R);
          MoveTo(R.Left + 2, MidY);
          LineTo(R.Right - 2, MidY);
          if not CollapseSign then
          begin
            MoveTo(MidX, R.Top + 2);
            LineTo(MidX, R.Bottom - 2);
	  end;
        end;
      tvestArrow:
        begin
          // draw an arrow. down for collapse and right for expand
          R := Rect(ALeft, ATop, ARight, ABottom);
          GetMem(Points, SizeOf(TPoint) * 3);
          if CollapseSign then
          begin
            // draw an arrow down
            Points[0] := Point(R.Left, MidY);
            Points[1] := Point(R.Right - 1, MidY);
            Points[2] := Point(MidX, R.Bottom - 1);
          end else
          begin
            // draw an arrow right
            Points[0] := Point(MidX - 1, ATop);
            Points[1] := Point(R.Right - 2, MidY);
            Points[2] := Point(MidX - 1, R.Bottom - 1);
          end;
          Polygon(Points, 3, False);
          FreeMem(Points);
        end;
      end;
    end;
  end;
  procedure DrawIndicator(ACanvas: TCanvas; R: TRect;
    Opt: TDataSetState; MultiSel: boolean);
  var
    dx,dy, x, y: Integer;
    procedure CenterY;
    begin
      y := R.Top + (R.Bottom-R.Top) div 2;
    end;
    procedure CenterX;
    begin
      X := R.Left + (R.Right-R.Left) div 2;
    end;
    procedure DrawEdit(clr: Tcolor);
    begin
      ACanvas.Pen.Color := clr;
      CenterY;
      CenterX;
      ACanvas.MoveTo(X-2, Y-Dy);
      ACanvas.LineTo(X+3, Y-Dy);
      ACanvas.MoveTo(X, Y-Dy);
      ACanvas.LineTo(X, Y+Dy);
      ACanvas.MoveTo(X-2, Y+Dy);
      ACanvas.LineTo(X+3, Y+Dy);
    end;
  begin
    dx := 6;
    dy := 6;
    case Opt of
      dsBrowse:
        begin //
          ACanvas.Brush.Color:=clBlack;
          ACanvas.Pen.Color:=clBlack;
          CenterY;
          x:= R.Left+3;
          if MultiSel then begin
            ACanvas.Polyline([point(x,y-dy),  point(x+dx,y),point(x,y+dy), point(x,y+dy-1)]);
            ACanvas.Polyline([point(x,y-dy+1),point(x+dx-1,y),point(x, y+dy-1), point(x,y+dy-2)]);
            CenterX;
            Dec(X,3);
            ACanvas.Ellipse(Rect(X-2,Y-2,X+2,Y+2));
          end else
            ACanvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
         end;
      dsEdit:
        DrawEdit(clBlack);
      dsInsert:
        DrawEdit(clGreen);
      else
      if MultiSel then begin
        ACanvas.Brush.Color:=clBlack;
        ACanvas.Pen.Color:=clBlack;
        CenterX;
        CenterY;
        ACanvas.Ellipse(Rect(X-3,Y-3,X+3,Y+3));
      end;
    end;
  end;
begin
  try
  with Sender as TStringGrid do
    begin
      if (((gdFixed in AState)
       and (aCol > 0))
      or  (not Assigned(FDataSource))
      or  ((aCol > 0) and (Self.Columns[aCol-1].ButtonStyle=cbsCheckboxColumn))
      ) then
        begin
          aColor := TStringGrid(Sender).Columns[aCol-1].Color;
          if (AltColorStartNormal and Odd(ARow-FixedRows)) {(1)} or
             (not AltColorStartNormal and Odd(ARow)) {(2)} then
            AColor := AlternateColor;
          if (gdSelected in AState) then
            begin
              aColor := SelectedColor;
              TStringGrid(Sender).Canvas.Font.Color:=clHighlightText;
            end;
          with TStringGrid(Sender).Canvas do
            begin
              TStringGrid(Sender).Canvas.Brush.Color:=aColor;
              FillRect(aRect);
            end;
          for i := 0 to dgFake.Columns.Count-1 do
            if dgFake.Columns[i].FieldName = IdentField then
              asCol := i+1;
          if (HasChildsField = '')
          or (gdFixed in AState)
          or (HasChilds(asCol,aRow) = ' ')
          then
            gList.DefaultDrawCell(aCol,aRow,aRect,aState);
          if (gdSelected in aState) and gList.Focused then
            TStringGrid(Sender).Canvas.DrawFocusRect(arect);
          exit;
        end;
      if Assigned(gList.Objects[0,aRow]) and TRowObject(gList.Objects[0,aRow]).RefreshHeight and (not gList.EditorMode) and gList.Canvas.HandleAllocated then
        begin
          gList.BeginUpdate;
          UpdatedARow := False;
          for i := gList.TopRow to gList.TopRow+gList.VisibleRowCount-1 do
            begin
              UpdatedARow := i=aRow;
              if Assigned(gList.Objects[0,i]) and TRowObject(gList.Objects[0,i]).RefreshHeight then
                begin
                  TRowObject(gList.Objects[0,i]).Extends := GetRowHeight(i);
                  if TRowObject(gList.Objects[0,i]).Extends.Y <> RowHeights[i] then
                    begin
                      RowHeights[i] := TRowObject(gList.Objects[0,i]).Extends.Y;
                    end;
                  TRowObject(gList.Objects[0,i]).RefreshHeight := False;
                end;
            end;
          if not UpdatedARow then
            begin
              TRowObject(gList.Objects[0,aRow]).Extends := GetRowHeight(i);
              if TRowObject(gList.Objects[0,aRow]).Extends.Y <> RowHeights[i] then
                begin
                  RowHeights[i] := TRowObject(gList.Objects[0,aRow]).Extends.Y;
                end;
              TRowObject(gList.Objects[0,aRow]).RefreshHeight := False;
            end;
          gList.EndUpdate;
          Application.QueueAsyncCall(@DoInvalidate,0);
        end;
      if (aCol = 0) then
        begin
          gList.DefaultDrawCell(aCol,aRow,aRect,aState);
          //Canvas.Brush.Color:=FixedColor;
          //Canvas.Pen.Color:=FixedColor;
          //Canvas.Rectangle(aRect.Left+2,aRect.Top+2,aRect.Right-2,aRect.Bottom-2);
          if (TStringGrid(Sender).Row = aRow) then
            DrawIndicator(TStringGrid(Sender).Canvas,aRect,FDataSource.State,False);
          exit;
        end;
      aDrawed := False;
      aColor := TStringGrid(Sender).Columns[aCol-1].Color;
      if (not (gdFixed in AState)) and (AlternateColor<>AColor) then
        begin
          if (AltColorStartNormal and Odd(ARow-FixedRows)) {(1)} or
             (not AltColorStartNormal and Odd(ARow)) {(2)} then
            AColor := AlternateColor;
        end;
      if Assigned(FOnDrawCell) then
        begin
          if (gdSelected in AState) then
            begin
              aColor := SelectedColor;
              TStringGrid(Sender).Canvas.Font.Color:=clHighlightText;
            end;
          TStringGrid(Sender).Canvas.Brush.Color:=aColor;
          TStringGrid(Sender).Canvas.FillRect(aRect);
          bRect :=aRect;
          if (dgFake.Columns[aCol-1].FieldName = IdentField) and (TreeField <> '') then
            begin
              aLevel := GetLevel(asCol,aRow);
              inc(bRect.Left,10*aLevel);
              if (HasChilds(asCol,aRow) = '+')
              or (HasChilds(asCol,aRow) = '-')
              then
                DrawExpandSign(bRect.Left+((FExpandSignSize+4) div 2),bRect.Top+((bRect.Bottom-bRect.Top) div 2),HasChilds(asCol,aRow) = '-');
              inc(bRect.Left,(FExpandSignSize+4));
            end;
          aDrawed := FOnDrawCell(gList,bRect,aRow,dgFake.Columns[aCol-1],aState);
        end;
      if not aDrawed then
        begin
          if (gdSelected in AState) then
            begin
              aColor := SelectedColor;
              TStringGrid(Sender).Canvas.Font.Color:=clHighlightText;
            end;
          if Assigned(FOnGetFontCell) then
            FOnGetFontCell(gList,aRect,aRow,dgFake.Columns[aCol-1],aState,aColor,aFontStyle);
          with TStringGrid(Sender).Canvas do
            begin
              if not Assigned(FOnDrawCell) then
                begin
                  TStringGrid(Sender).Canvas.Brush.Color:=aColor;
                  TStringGrid(Sender).Canvas.FillRect(aRect);
                end;
              bRect := aRect;
              for i := 0 to dgFake.Columns.Count-1 do
                if dgFake.Columns[i].FieldName = IdentField then
                  asCol := i+1;
              aText := gList.Cells[aCol,aRow];
              if (dgFake.Columns[aCol-1].FieldName = IdentField) and (TreeField <> '') then
                begin
                  aLevel := GetLevel(asCol,aRow);
                  inc(bRect.Left,10*aLevel);
                  if (HasChilds(asCol,aRow) = '+')
                  or (HasChilds(asCol,aRow) = '-')
                  then
                    DrawExpandSign(bRect.Left+((FExpandSignSize+4) div 2),bRect.Top+((bRect.Bottom-bRect.Top) div 2),HasChilds(asCol,aRow) = '-');
                  inc(bRect.Left,(FExpandSignSize+4));
                end;
              if  (TStringGrid(Sender).RowHeights[aRow] = TStringGrid(Sender).DefaultRowHeight)
              and (pos(#10,aText) > 0)
              then
                aText := copy(aText,0,pos(#10,aText)-1);
              if Assigned(FGetCText) then
                FGetCText(Self,dgFake.Columns[aCol-1],aRow,aText,Canvas.Font);
              TStringGrid(Sender).Canvas.Brush.Style:=bsClear;
              {$ifdef DEBUG}
              if Assigned(TRowObject(gList.Objects[0,aRow])) then
                aText := '['+IntToStr(TRowObject(gList.Objects[0,aRow]).Rec)+'] '+aText;
              {$endif}
              aTextStyle.Alignment:=dgFake.Columns[aCol-1].Alignment;
              dec(bRect.Right,1);
              if not FWordWrap then
                TextRect(bRect,bRect.Left+3,bRect.Top,aText,aTextStyle)
              else
                TextRect(bRect,bRect.Left+3,bRect.Top,aText,aTextStyleW);
              dec(aRect.Right,1);
              dec(aRect.Bottom,1);
              if (gdSelected in aState) and gList.Focused then
                TStringGrid(Sender).Canvas.DrawFocusRect(arect);
            end;
        end;
      if (aCol-1) = ColumnWidthHelper.Index then
        ColumnWidthHelper.MaxWidth := Max(ColumnWidthHelper.MaxWidth, TStringGrid(Sender).Canvas.TextWidth(atext));
    end;
  except
  end;
end;
procedure TfGridView.gListEditingDone(Sender: TObject);
var
  ct: String = '';
  Value: String;
  aRow: Integer;
  oDate: TDateTime;
  aRect: TGridRect;
  aStart: LongInt;
  aStop: LongInt;
begin
  if (gList.Col<gList.FixedCols) or (gList.Row<gList.FixedRows) or (gList.Col>=gList.ColCount) or (gList.Row>=gList.RowCount) then exit;
  if WasEditing or gList.EditorMode then
    begin
      BeginUpdate;
      gList.BeginUpdate;
      try
        try
          aStart := gList.Selection.Top;
          aStop := gList.Selection.Bottom;
          if aStart>aStop then
            begin
              aStart := gList.Selection.Bottom;
              aStop := gList.Selection.Top;
            end;
          Value := gList.Cells[gList.Col,gList.Row];
          if (gList.Selection.Bottom-gList.Selection.Top = 1) or (dgFake.Columns[gList.Col-1].Field.FieldName<>IdentField) then
            for aRow := aStop downto aStart do
              begin
                if GotoRowNumber(aRow) then
                  begin
                    if Value <> '' then
                      begin
                        if dgFake.Columns[gList.Col-1].Field.DataType = ftDateTime then
                          begin
                            if TryStrToDateTime(Value,oDate) and (dgFake.Columns[gList.Col-1].Field.AsDateTime<>oDate) then
                              begin
                                if not ((dgFake.DataSource.DataSet.State = dsEdit) or (dgFake.DataSource.DataSet.State = dsInsert)) then
                                  dgFake.DataSource.DataSet.Edit;
                                if ((pos('END',dgFake.Columns[gList.Col-1].Field.FieldName)>0) or (pos('DUE',dgFake.Columns[gList.Col-1].Field.FieldName)>0)) and (Trunc(oDate) = oDate) then
                                  oDate := oDate+0.99999;
                                dgFake.Columns[gList.Col-1].Field.AsDateTime:=oDate;
                              end
                          end
                        else if (dgFake.Columns[gList.Col-1].Field.FieldName <> TextField) and (dgFake.Columns[gList.Col-1].Field.AsString<>Value) then
                          begin
                            if not ((dgFake.DataSource.DataSet.State = dsEdit) or (dgFake.DataSource.DataSet.State = dsInsert)) then
                              dgFake.DataSource.DataSet.Edit;
                            if Assigned(FSetCText) then
                              FSetCText(Sender,dgFake.Columns[gList.Col-1],aRow,Value);
                            dgFake.Columns[gList.Col-1].Field.AsString:=Value
                          end;
                      end
                    else if (not dgFake.Columns[gList.Col-1].Field.IsNull) then
                      begin
                        if (not ((dgFake.DataSource.DataSet.State = dsEdit) or (dgFake.DataSource.DataSet.State = dsInsert))) then
                          dgFake.DataSource.DataSet.Edit;
                        dgFake.Columns[gList.Col-1].Field.Clear;
                      end;
                    if Assigned(FSetCText) then
                      FSetCText(Sender,dgFake.Columns[gList.Col-1],aRow,Value);
                    gList.Cells[gList.Col,aRow] := ct+Value;
                  end
                {$ifdef DEBUG}
                else debugln('gotorownumber failed');
                {$ENDIF}
              end;
          aRect := gList.Selection;
        finally
          gList.EndUpdate;
          EndUpdate;
        end;
      except
      end;
      gList.Invalidate;
      WasEditing:=False;
    end;
end;
procedure TfGridView.gListGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  if InEdit then exit;
  Value := gList.Cells[ACol,ARow];
  GotoRowNumber(aRow);
  if Assigned(FGetEdit) then
    FGetEdit(Sender,ACol,ARow,Value);
end;
procedure TfGridView.gListHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  aWidth: Integer;
  i: Integer;
begin
  if not IsColumn then exit;
  if Sender = gHeader then
    gList.Columns[Index-1].Width:=gHeader.Columns[Index-1].Width;
  if (dgFake.Columns.Count > Index-1) and (gList.Columns.Count > Index-1) then
    dgFake.Columns[Index-1].Width:=gList.Columns[Index-1].Width;
  if Assigned(Sender) then
    begin
      fRowEditor.SetGridSizes(FBaseName,dgFake.DataSource,dgFake,FBaseName);
      Asyncrefresh;
    end;
end;
procedure TfGridView.gListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aBm: Int64;
  i: Integer = 0;
  DoInsert: Boolean = False;
  DoSync: Boolean = False;
//  ch : TUTF8Char;
//  c: Char;
  aRect: TRect;
  WasInsert: Boolean;
  aSetEdit: Boolean = false;
  aKey : Word = VK_ESCAPE;
begin
  BeginUpdate;
  if Assigned(FSearchKey) then
    begin
      case Key of
      VK_PRIOR,
      VK_NEXT,
      VK_UP,
      VK_DOWN,
      VK_RETURN,
      VK_ESCAPE:
          begin
            aRect := gList.CellRect(gList.Col,gList.Row);
            if FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],Key,Shift,'') then
              begin
                Key := 0;
                SearchKeyTimer.Enabled:=False;
              end;
          end;

      end;
    end;
  if Key = VK_RETURN then
    begin
      if (not gList.EditorMode) then
        begin
          if Assigned(Self.OnDblClick) and (dgFake.Columns[gList.Col-1].FieldName = IdentField) then
            begin
              Post;
              OnDblClick(Sender);
              Key := 0;
            end;
        end
      else
        begin
          if Assigned(FSearchKey) then
            FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
          Application.QueueAsyncCall(@DoSetEditDD,0);
        end;
    end
  else if Key = VK_DOWN then
    begin
      if (FDataSet.CanEdit) then
        begin
          try
            if Assigned(FCheckIdent) then FCheckIdent(Self);
            WasInsert := FDataSet.State=dsInsert;
            if (FDataSource.DataSet.State = dsInsert) and (not FDataSet.Changed) then
              begin
                Key := 0;
                exit;
              end
            else
              begin
                if FDataSet.CanEdit then
                  DataSet.DataSet.Post;
                aBm := DataSet.GetBookmark;
                if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
                  TRowObject(gList.Objects[0,gList.Row]).Rec := aBM;
                if aBm = TRowObject(gList.Objects[0,gList.Row]).Rec then
                  SyncActiveRow(aBm,false,true);
                if WasInsert and (gList.Row < gList.RowCount-1) then
                  gListColRowMoved(gList,False,gList.Row,gList.Row);
              end;
          except
            EndUpdate;
            raise;
          end;
          gList.EditorMode:=False;
        end
      else if (gList.Row = gList.RowCount-1) and (goEditing in gList.Options) and FEditable then
        begin
          aBm := DataSet.GetBookmark;
          if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
            TRowObject(gList.Objects[0,gList.Row]).Rec := aBM;
          if FDataSet.DataSet.State<>dsInsert then
            Append;
        end;
      if gList.CanFocus then
        gList.SetFocus;
    end
  else if Key = VK_UP then
    begin
      if (FDataSource.DataSet.State = dsInsert) and (not FDataSet.Changed) then
        begin
          EditingDone;
          CleanRow(gList.Row,-2);
          gList.DeleteColRow(False,gList.Row);
          if gList.RowCount = gList.FixedRows then
            begin
              CleanList(1);
            end;
          FDataSource.DataSet.Cancel;
          Key := 0;
        end
      else if (FDataSet.Changed) then
        begin
          try
            if FDataSet.CanEdit then
              DataSet.DataSet.Post;
            aBm := DataSet.GetBookmark;
            if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
              TRowObject(gList.Objects[0,gList.Row]).Rec := aBM;
            if aBm = TRowObject(gList.Objects[0,gList.Row]).Rec then
              SyncActiveRow(aBm,false,true);
          except
            EndUpdate;
            raise;
            exit;
          end;
          FIgnoreSettext := True;
        end;
    end
  else if (Key = VK_LEFT)
       or (Key = VK_RIGHT) then
    begin
      if Assigned(FSearchKey) then
        FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
      if gList.EditorMode then
        Application.QueueAsyncCall(@DoSetEdit,0);
    end
  else if Key = VK_INSERT then
    begin
      Insert(True);
      gList.EditorMode:=True;
      if gList.CanFocus then
        gList.SetFocus;
      Key := 0;
    end
  else if Key = VK_ESCAPE then
    begin
      if (FDataSource.State = dsInsert) or (FDataSource.State = dsEdit) then
        begin
          gList.EditorMode:=False;
          if (FDataSource.DataSet.State = dsInsert) then
            begin
              FDataSource.DataSet.Cancel;
              gList.DeleteRow(gList.Row);
              if gList.RowCount = gList.FixedRows then
                begin
                  gList.RowCount:=gList.FixedRows+1;
                  gList.Objects[0,gList.FixedRows] := TRowObject.Create;
                end;
            end
          else
            begin
              FDataSource.DataSet.Cancel;
              DoSync := True;
            end;
        end;
      try
        if gList.Visible and gList.CanFocus and gList.IsControlVisible and gList.Enabled then
          gList.SetFocus;
      except //badly sometimes the lcl is not able to detect whenever focus can be set or not (during form hide @ example)
      end;
      Key := 0;
    end
  else if Key = VK_TAB then
    begin
      if (gList.Col=gList.ColCount-1)
      and (gList.Row=gList.RowCount-1) then
        begin
          if ((FDataSource.DataSet.State = dsEdit) or (FDataSource.DataSet.State = dsInsert)) and (FDataSet.Changed) then
            begin
              try
                if Assigned(FCheckIdent) then FCheckIdent(Self);
                FDataSource.DataSet.Post;
                aBm := DataSet.GetBookmark;
                SyncActiveRow(aBm,false,false);
              except
                EndUpdate;
                raise;
                exit;
              end;
            end;
          if (gList.Row = gList.RowCount-1) and (goEditing in gList.Options) and FEditable then
            begin
              Append;
            end;
          gList.Col:=1;
          if gList.CanFocus then
            gList.SetFocus;
          if Assigned(FSearchKey) then
            FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
          if gList.EditorMode then
            Application.QueueAsyncCall(@DoSetEdit,0);
        end;
    end
  else if (Key in [VK_DELETE]) and (ssCtrl in Shift) then
    begin
      if not gList.EditorMode then
        begin
          if Assigned(OnDelete) then
            OnDelete(Self);
          Key := 0;
        end;
    end
  {$IFDEF WINDOWS}
  else if Key in [VK_A..VK_Z,VK_0..VK_9] then
    begin
    end
  {$ENDIF}
  ;
  if FDataSource.DataSet.Active
  and (DoInsert or DoSync) and FEditable then
    begin
      aBm := DataSet.GetBookmark;
      if (gList.RowCount = gList.FixedRows+1) and (FDataSource.DataSet.RecordCount = 0) then
        begin
          gList.RowCount:=gList.FixedRows;
        end
      else
        for i := gList.FixedRows to gList.RowCount-1 do
          if Assigned(gList.Objects[0,i]) and (TRowObject(gList.Objects[0,i]).Rec = aBm) then
            break;
      gList.EditorMode:=False;
      if i=gList.Row then
        SyncActiveRow(aBm,DoInsert,DoSync)
      else GotoActiveRow;
      if gList.CanFocus then
        gList.SetFocus;
    end;
  EndUpdate;
end;
procedure TfGridView.gListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aBm: LargeInt;
begin
  if not Assigned(FDataSource) then exit;
  if  (TStringGrid(Sender).MouseToCell(Point(X,Y)).Y <> OldRow)
  and (TStringGrid(Sender).MouseToCell(Point(X,Y)).Y > 0)
  and ((FDataSource.State = dsEdit) or (FDataSource.State = dsInsert))
  then
    begin
      aBm := DataSet.GetBookmark;
      if TRowObject(gList.Objects[0,OldRow]).Rec = 0 then
        TRowObject(gList.Objects[0,OldRow]).Rec := aBM;
      if (FDataSource.DataSet.State = dsInsert) and (not FDataSet.Changed) then
        begin
          gList.DeleteColRow(False,gList.Row);
          if gList.RowCount = gList.FixedRows then
            begin
              gList.RowCount:=gList.FixedRows+1;
              gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
            end;
          FDataSource.DataSet.Cancel;
        end;
    end;
end;
procedure TfGridView.gListMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  CanSelect: Boolean;
  aBm: LargeInt;
begin
  if  (WheelDelta = 120)
  and ((FDataSource.State = dsEdit) or (FDataSource.State = dsInsert))
  then
    begin
      if (FDataSource.DataSet.State = dsInsert) and (not FDataSet.Changed) then
        begin
          CleanRow(gList.RowCount-1,-2);
          gList.RowCount:=gList.RowCount-1;
          FDataSource.DataSet.Cancel;
        end;
    end;
end;
procedure TfGridView.gListSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  NewVisible: Boolean;
  aBm: LargeInt;
  akey : Word = VK_ESCAPE;
  aRect: TRect;
  ct: String;
  WasInsert: Boolean;
begin
  if aRow>=gList.RowCount then exit;
  if aRow < gList.FixedRows then exit;
  BeginUpdate;
  try
    WasEditing := gList.EditorMode;
    if not (OldRow < TStringGrid(Sender).RowCount) then
      OldRow := TStringGrid(Sender).RowCount-1;
    if (OldRow <> aRow)  then
      begin
        gList.EditorMode:=False;
        {$ifdef debug}
        debugln('RowChanged '+IntToStr(Oldrow)+'->'+IntToStr(aRow)+' '+TRowObject(gList.Objects[0,aRow]).StringRec);
        {$endif}
        if Assigned(OnCellChanging) then
          OnCellChanging(Self);
        TStringGrid(Sender).InvalidateCell(0,OldRow);
        if aRow < TStringGrid(Sender).RowCount then
          TStringGrid(Sender).InvalidateCell(0,aRow);
        if Assigned(FDataSet) and (FDataSet.CanEdit) and (not FDataSet.DataSet.ControlsDisabled) then
          begin
            if FDataSet.Changed then
              begin
                WasInsert := FDataSet.State=dsInsert;
                if FDataSet.CanEdit then
                  FDataSet.DataSet.Post;
                aBm := DataSet.GetBookmark;
                if (OldRow>-1) and Assigned(gList.Objects[0,OldRow]) and (TRowObject(gList.Objects[0,OldRow]).Rec<>aBm) and (TRowObject(gList.Objects[0,OldRow]).Rec=0) then
                  TRowObject(gList.Objects[0,OldRow]).Rec:=aBm;
                if WasInsert then
                  gListColRowMoved(gList,False,OldRow,OldRow);
                GotoRow(TRowObject(gList.Objects[0,aRow]).Rec);
              end
          end;
        OldRow := aRow;
        if Assigned(DataSet) and (DataSet.State<>dsInsert) then
          gList.EditorMode:=False;
      end
    else if (OldCol < aCol) then
      begin
        if Assigned(FCheckIdent) then FCheckIdent(Self);
      end;
    if ((OldCol < aCol) or (OldRow <> aRow)) and Assigned(FSearchKey) then
      begin
        aRect := gList.CellRect(gList.Col,gList.Row);
        if gList.Col>1 then
          FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
      end;
    if Assigned(OnCellChanged) then
      OnCellChanged(Self,Point(aCol,aRow),Point(OldCol,OldRow));
    OldCol := aCol;
  except
  end;
  EndUpdate;
end;
procedure TfGridView.gListSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if TextField <> '' then
    if (dgFake.Columns[aCol-1].FieldName = TextField) and (Editor <> mInplace) then
      begin
        Editor:=mInplace;
        exit;
      end;
end;
procedure TfGridView.gListSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  aBm: Int64;
  tmp: string;
  sl: TStringList;
  aKey : Word = 0;
  oDate: TDateTime;
begin
  if FDisableEdit then exit;
  if FIgnoreSettext then
    begin
      FIgnoreSettext := False;
      exit;
    end;
  if copy(Value,length(Value),1) = Decimalseparator then exit;
  {$ifdef DEBUG}
  debugln('SetEditText ',IntToStr(ACol)+' ',IntToStr(aRow)+' '+TRowObject(gList.Objects[0,aRow]).StringRec+' "',Value+'"');
  {$endif}
  If InEdit then exit;
  InEdit := True;
  if not Assigned(dgFake.Columns[aCol-1].Field) then
    begin
      gList.Cells[aCol,aRow] := '';
      InEdit := False;
      exit;
    end;
  if (FDataSource.State <> dsInsert) and  Assigned(gList.Objects[0,aRow]) and (TRowObject(gList.Objects[0,aRow]).Rec <> 0) and (not DataSet.GotoBookmark(TRowObject(gList.Objects[0,aRow]).Rec)) then
    begin
      tmp := gList.Cells[aCol,aRow];
      aBm := DataSet.GetBookmark;
      SyncActiveRow(aBm,False,True);
      gList.Cells[aCol,aRow] := tmp;
    end;
  if (gList.Cells[aCol,aRow] <> Value) and (FDataSource.State <> dsInsert) and (FDataSource.State <> dsEdit) then
    begin
      FDataSource.DataSet.Edit;
      if FDataSource.DataSet.State = dsInsert then
        begin
          aBm := DataSet.GetBookmark;
          SyncActiveRow(aBm,false,True);
          gList.Cells[aCol,aRow] := Value;
        end;
    end;
  if dgFake.Columns[aCol-1].Field.FieldName <> TextField then
    begin
      try
        tmp := Value;
        if (gList.Columns[aCol-1].PickList.Count > 0) then
          begin
            if (FDataSource.State <> dsEdit) and (FDataSource.State <> dsInsert) then
              FDataSource.DataSet.Edit;
            if (pos(' ',Value) > 0) then
              tmp := copy(Value,0,pos(' ',Value)-1);
            if dgFake.Columns[aCol-1].Field.Size > 0 then
              tmp := copy(tmp,0,dgFake.Columns[aCol-1].Field.Size);
            if Assigned(FSetEdit) then
              FSetEdit(Sender,ACol,ARow,tmp);
            if Assigned(FSetCText) then
              FSetCText(Sender,dgFake.Columns[aCol-1],aRow,tmp);
            gList.Cells[aCol,aRow] := tmp;
          end
        else
          begin
            if Assigned(FSetEdit) then
              FSetEdit(Sender,ACol,ARow,tmp);
            if Assigned(FSetCText) then
              begin
                FSetCText(Sender,dgFake.Columns[aCol-1],aRow,tmp);
              end;
          end;
        if (tmp <> dgFake.Columns[aCol-1].Field.AsString) or (tmp = '') then
          begin
            if tmp <> '' then
              begin
                if (FDataSource.State <> dsEdit) and (FDataSource.State <> dsInsert) then
                  FDataSource.DataSet.Edit;
                if dgFake.Columns[aCol-1].Field.DataType = ftDateTime then
                  begin
                    if TryStrToDateTime(tmp,oDate) then
                      begin
                        if ((pos('END',dgFake.Columns[gList.Col-1].Field.FieldName)>0) or (pos('DUE',dgFake.Columns[gList.Col-1].Field.FieldName)>0)) and (Trunc(oDate) = oDate) then
                          oDate := oDate+0.99999;
                        dgFake.Columns[aCol-1].Field.AsDateTime:=oDate;
                      end;
                    {$ifdef DEBUG}
                    debugln('SetEditDate:'+tmp+' '+dgFake.Columns[aCol-1].Field.AsString);
                    {$endif}
                  end
                else
                  begin
                    dgFake.Columns[aCol-1].Field.AsString:=tmp;
                    {$ifdef debug}
                    debugln('SetEditText:'+tmp);
                    {$endif}
                  end;
              end
            else
              begin
                if (FDataSource.State <> dsEdit) and (FDataSource.State <> dsInsert) then
                  FDataSource.DataSet.Edit;
                if not (dgFake.Columns[aCol-1].Field.AsString='') then
                  dgFake.Columns[aCol-1].Field.Clear;
              end;
            if Assigned(FSearchKey) then
              begin
                FSearchKeyRect := gList.CellRect(gList.Col,gList.Row);
                if gHeader.Visible then
                  FSearchKeyRect.Bottom:=FSearchKeyRect.Bottom+gHeader.Height;
                FSearchKeyCol := dgFake.Columns[aCol-1];
                FSearchKeyKey := aKey;
                FSearchKeyVal := tmp;
                SearchKeyTimer.Enabled := True;
              end;
          end;
      except
      end;
    end
  else
    begin
      if FDataSet.DataSet.RecordCount=0 then
        FDataSet.DataSet.Edit;
      BeginUpdate;
      tmp :=Value;
      FInpStringList.Text := tmp;
      if DataSet.DataSet.FieldDefs.IndexOf(ShortTextField)>=0 then
        begin
          if FInpStringList.Count > 0 then
            begin
              if DataSet.FieldByName(ShortTextField).AsString<>FInpStringList[0] then
                begin
                  if (FDataSource.State <> dsEdit) and (FDataSource.State <> dsInsert) then
                    FDataSource.DataSet.Edit;
                  DataSet.FieldByName(ShortTextField).AsString:=FInpStringList[0];
                end;
              FInpStringList.Delete(0);
            end
          else if DataSet.FieldByName(ShortTextField).AsString<>'' then
            begin                   ;
              if (FDataSource.State <> dsEdit) and (FDataSource.State <> dsInsert) then
                FDataSource.DataSet.Edit;
              DataSet.FieldByName(ShortTextField).Clear;
            end
        end;
      tmp :=FInpStringList.Text;
      if Assigned(DataSet.FieldByName(TextField)) then
        begin
          if DataSet.FieldByName(TextField).AsString<>FInpStringList.Text then
            begin
              if (FDataSource.State <> dsEdit) and (FDataSource.State <> dsInsert) then
                FDataSource.DataSet.Edit;
              DataSet.FieldByName(TextField).AsString:=FInpStringList.Text;
              dataSet.Change;
              TRowObject(gList.Objects[0,aRow]).RefreshHeight:=True;
            end;
        end;
      EndUpdate;
    end;
  InEdit := False;
end;
procedure TfGridView.SearchKeyTimerTimer(Sender: TObject);
var
  aCol: objpas.Integer;
begin
  SearchKeyTimer.Enabled:=false;
  if (not Assigned(DataSet)) or (not DataSet.DataSet.Active) then exit;
  aCol := gList.Col-1;
  if (dgFake.Columns.Count>aCol) and (FSearchKeyCol = dgFake.Columns[aCol]) then
    begin
      if Assigned(FSearchKey) then
        FSearchKey(Self,FSearchKeyRect.Left,FSearchKeyRect.Bottom,FSearchKeyCol,FSearchKeyKey,[],FSearchKeyVal);
    end;
end;

procedure TfGridView.SetEditPrefix(AValue: string);
begin
  if FFEditPrefix=AValue then Exit;
  FFEditPrefix:=AValue;
end;

procedure TfGridView.ClearFilters;
var
  a: Integer;
begin
  gList.Selection := Rect(0,0,0,0);
  for a := 0 to gList.Columns.Count-1 do
    begin
      gHeader.Cells[a+1,1] := '';
    end;
  FAutoFilter := BuildAutofilter(dgFake,gHeader);
  if Assigned(FAutoFilterChanged) then
    FAutoFilterChanged(Self);
  acFilter.Execute;
end;
procedure TfGridView.SetHasChilds(aCol, aRow: Integer; Expanded: char);
var
  ct: String;
begin
  if aCol <= 0 then exit;
  if Assigned(TRowObject(gList.Objects[0,aRow])) then
    TRowObject(gList.Objects[0,aRow]).Childs:=Expanded;
end;
procedure TfGridView.SetExpField(AValue: string);
begin
  if FExpField=AValue then Exit;
  FExpField:=AValue;
end;
function TfGridView.GetFilterRow: Boolean;
begin
  Result := gHeader.Visible;
end;
procedure TfGridView.SetFilterRow(AValue: Boolean);
begin
  gHeader.Visible:=AValue;
  pFilter.Visible:=AValue;
  if AValue then gList.FixedRows:=0
  else gList.FixedRows:=1;
end;
function TfGridView.HasChilds(aCol, aRow : Integer): Char;
var
  ct: String;
begin
  Result := ' ';
  if Assigned(TRowObject(gList.Objects[0,aRow])) then
    Result := TRowObject(gList.Objects[0,aRow]).Childs;
end;
procedure TfGridView.SetHCField(AValue: string);
begin
  if FHCField=AValue then Exit;
  FHCField:=AValue;
end;
procedure TfGridView.SetLevel(aCol, aRow, aLevel: Integer);
var
  ct: String;
  at: String = '';
begin
  if aCol <= 0 then exit;
  if Assigned(TRowObject(gList.Objects[0,aRow])) then
    TRowObject(gList.Objects[0,aRow]).Level:=aLevel;
end;
function TfGridView.GetLevel(aCol, aRow: Integer): Integer;
var
  aStrlevel: String;
begin
  Result := 0;
  if Assigned(gList.Objects[0,aRow]) then
    Result := TRowObject(gList.Objects[0,aRow]).Level;
end;
procedure TfGridView.FieldModified(aField: TField);
var
  i: Integer;
  tmp: String;
begin
  if aField = nil then exit;
  if FDontUpdate > 0 then exit;
  if gList.Row>gList.RowCount-1 then exit;
  if not Assigned(gList.Objects[0,gList.Row]) then exit;
  if (TRowObject(gList.Objects[0,gList.Row]).Rec <> DataSet.GetBookmark)
  and ((DataSet.State <> dsInsert)
  and (TRowObject(gList.Objects[0,gList.Row]).Rec <> 0)
  and (not GotoDataSetRow))
  then exit;
  for i := 0 to dgFake.Columns.Count-1 do
    if dgFake.Columns[i].Field = aField then
      if not (InEdit and ((gList.Col-1) = i)) then
        begin
          if ((aField.FieldName = TextField)
           or (aField.FieldName = ShortTextField))
          and (ShorttextField <> '') then
            begin
              if dgFake.Columns[i].Field.AsString = '' then
                gList.Cells[i+1,gList.Row] := dgFake.DataSource.DataSet.FieldByName(ShorttextField).AsString
              else
                gList.Cells[i+1,gList.Row] := dgFake.DataSource.DataSet.FieldByName(ShortTextField).AsString+lineending+dgFake.Columns[i-1].Field.AsString;
            end
          else
            begin
              tmp := aField.AsString;
              //if not (gList.Editormode) then
              gList.Cells[i+1,gList.Row] := tmp;
              {$IFDEF DEBUG}
              debugln('FieldModified:'+tmp);
              {$ENDIF}
            end;
          break;
        end;
end;
function TfGridView.GetActualCell: string;
begin
  Result := gList.Cells[gList.Col,gList.Row];
end;
function TfGridView.GetActualField: TColumn;
begin
  Result := dgFake.Columns[gList.Col-1];
end;
function TfGridView.GetRowHeight(aRow: Integer): TPoint;
var
  aText: string;
  aHeight : Integer = 1;
  i: Integer;
  r: Classes.TRect;
  TextWidth: Integer;
  DontCall: Boolean = False;
  aStyle: TFontStyles;
begin
  Result.Y := gList.DefaultRowHeight;
  Result.X := 0;
  if aRow<0 then exit;
  if UseDefaultRowHeight then exit;
  r.Top:=0;
  r.Bottom:=0;
  for i := 0 to dgFake.Columns.Count-1 do
    if Assigned(dgFake.Columns[i].Field) and (dgFake.Columns[i].Field.FieldName = TextField) then
      begin
        if not (i < gList.ColCount) then exit;
        if gList.ColCount > i+1 then
          begin
            aText := gList.Cells[i+1,aRow];
            if aText='' then exit;
            if Assigned(FGetCText) then
              FGetCText(Self,dgFake.Columns[i],aRow,atext,nil);
            TextWidth := gList.CellRect(i+1,aRow).Right-gList.CellRect(i+1,aRow).Left;
            Result.X := TextWidth;
            if FWordWrap then
              begin
                if Assigned(FGetRowHeight) then
                  begin
                    FGetRowHeight(Self,dgFake.Columns[i],aRow,Result.Y,TextWidth);
                    DontCall := True;
                  end;
                if gList.Canvas.HandleAllocated then
                  begin
                    r := gList.CellRect(i+1,aRow);
                    r.Right:=r.Left+TextWidth;
                    aStyle := gList.Canvas.Font.Style;
                    gList.Canvas.Font.Style:=[fsBold];
                    DrawText(gList.Canvas.Handle,
                      PChar(UniToSys(aText)),
                      Length(aText),
                      r,
                      DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
                    gList.Canvas.Font.Style := aStyle;
                  end;
              end;
          end;
        break;
      end;
  if r.Bottom = 0 then
    begin
      if copy(aText,length(aText),1) = #10 then
        aText := copy(aText,0,length(atext)-2);
      while pos(#10,aText) > 0 do
        begin
          inc(aHeight,1);
          //if Canvas.TextFitInfo(copy(atext,0,pos(#10,aText)-1),TextWidth)>length(copy(atext,0,pos(#10,aText)-1)) then
          //  inc(aHeight,1);
          aText := copy(aText,pos(#10,aText)+1,length(aText));
        end;
      aHeight := ((aHeight)*(gList.DefaultRowHeight-4));
      if gList.DefaultRowHeight > aHeight then
        aHeight := gList.DefaultRowHeight;
      Result.Y := aHeight;
    end
  else
    begin
      Result.Y := r.Bottom-r.Top;
      if Result.Y < gList.DefaultRowHeight then
        Result.Y := gList.DefaultRowHeight;
    end;
  if Result.Y>(gList.Height-(gList.FixedRows*gList.DefaultRowHeight)) then
    Result.Y:=(gList.Height-(gList.FixedRows*gList.DefaultRowHeight));
  if Assigned(FGetRowHeight) and (not DontCall) then
    FGetRowHeight(Self,dgFake.Columns[i],aRow,Result.Y,Result.X);
end;
procedure TfGridView.SetBaseFilter(AValue: string);
var
  aFilter: String;
begin
  if (FbaseFilter=AValue)
  and ((FAutoFilter=FActAutoFilter) and FApplyAutoFilter)
  and (FSortDirection=FActSortDirection)
  and (FSortField=FActSortField)
  then Exit;
  if Assigned(FDataSet) then
    begin
      with FDataSet.DataSet as IBaseDBFilter do
        aFilter := Filter;
      {
      if aFirstFilter = '' then
        aFirstFilter := aFilter
      else aFilter := aFirstFilter;
      }
      if pos(') AND ('+FActAutoFilter+')',aFilter) > 0 then
        begin
          System.Delete(aFilter,pos(') AND ('+FActAutoFilter+')',aFilter),length(aFilter));
          System.Delete(aFilter,1,1);
          FActAutoFilter:='';
        end
      else if pos('('+FActAutoFilter+')',aFilter) > 0 then
        begin
          System.Delete(aFilter,pos('('+FActAutoFilter+')',aFilter),length(aFilter));
          FActAutoFilter:='';
        end;
      if pos(') AND ('+FbaseFilter+')',aFilter) > 0 then
        begin
          System.Delete(aFilter,pos(') AND ('+FbaseFilter+')',aFilter),length(aFilter));
          System.Delete(aFilter,1,1);
        end
      else if pos('('+FbaseFilter+')',aFilter) > 0 then
        begin
          System.Delete(aFilter,pos('('+FbaseFilter+')',aFilter),length(aFilter));
        end;
    end;
  FbaseFilter:=AValue;
  FActSortDirection := FSortDirection;
  FActSortField := FSortField;
  if not Assigned(FDataSet) then exit;
  if (AValue <> '') and (aFilter <> '') then
    aFilter := '('+aFilter+') AND ('+AValue+')'
  else if (aValue <> '') then
    aFilter := '('+AValue+')';
  if (FAutoFilter <> '') and FApplyAutoFilter then
    begin
      aFilter := '('+aFilter+') AND ('+FAutoFilter+')';
      FActAutoFilter:=FAutoFilter;
    end;
  aFilter := StringReplace(aFilter,'() AND ','',[rfReplaceAll]);
  with FDataSet.DataSet as IBaseDBFilter do
    begin
      Filter := aFilter;
      if FSortField <> '' then
        SortFields:=FSortField
      else SortFields := BaseSortFields;
//      if FSortDirection <> sdIgnored then
      SortDirection:=FSortDirection;
    end;
  FDataSet.Open;
  SyncDataSource(gList.Columns.Count = 0);
end;
function TfGridView.GetCount: Integer;
begin
  Result := gList.RowCount-gList.FixedRows;
end;
function TfGridView.GetColumns: TDBGridColumns;
begin
  Result := dgFake.Columns;
end;
procedure TfGridView.SetDataSet(const AValue: TBaseDBDataSet);
begin
  FDataSet := AValue;
  if not Assigned(fDataSet) then
    begin
      FDataSource.DataSet := nil;
      exit;
    end;
  try
    if Assigned(FDataSet) and Assigned(FDataSet.DataSet) then
      FDataSet.DataSet.AfterScroll:=nil;
  except
  end;
  FDatasource.DataSet := FDataset.DataSet;
//  fRowEditor.GetGridSizes(FBaseName,FDataSource,dgFake,FDefaultRows);
  SyncDataSource;
end;
procedure TfGridView.SetDefaultRowHeight(AValue: Boolean);
begin
  if FDefaultRowHeight=AValue then Exit;
  FDefaultRowHeight:=AValue;
  CalculateRowHeights;
end;
procedure TfGridView.SetDefaultRows(AValue: string);
begin
  if fDefaultRows=AValue then Exit;
  fDefaultRows:=AValue;
  SyncDataSource;
end;
procedure TfGridView.SetDragDrop(AValue: TDragDropEvent);
begin
  if FDragDrop=AValue then Exit;
  FDragDrop:=AValue;
  gList.OnDragDrop:=AValue;
end;
procedure TfGridView.SetDragOver(AValue: TDragOverEvent);
begin
  if FDragOver=AValue then Exit;
  FDragOver:=AValue;
  gList.OnDragOver:=AValue;
end;
procedure TfGridView.SetIdentField(AValue: string);
begin
  if FIdentField=AValue then Exit;
  FIdentField:=AValue;
end;

procedure TfGridView.SetNumberField(AValue: string);
begin
  if FNumberField=AValue then Exit;
  FNumberField:=AValue;
end;

procedure TfGridView.SetReadOnly(AValue: Boolean);
begin
  if FreadOnly=AValue then Exit;
  FreadOnly:=AValue;
  if AValue then
    gList.Options := gList.Options-[goEditing]
  else gList.Options := gList.Options-[goEditing];
end;
procedure TfGridView.SetShortTextField(AValue: string);
begin
  if FSTextField=AValue then Exit;
  FSTextField:=AValue;
end;
procedure TfGridView.SetSortDirecion(AValue: TSortDirection);
begin
  if FSortDirection=AValue then Exit;
  FSortDirection:=AValue;
end;
procedure TfGridView.SetSortField(AValue: string);
begin
  if FSortField=AValue then Exit;
  FSortField:=AValue;
  if not Assigned(FDataSet) then exit;
  with FDataSet.DataSet as IBaseDbFilter do
    begin
      UseBaseSorting:=FSortField='';
    end;
end;
procedure TfGridView.SetTextField(AValue: string);
begin
  if FTextField=AValue then Exit;
  FTextField:=AValue;
end;
procedure TfGridView.SetTreeField(AValue: string);
begin
  if FTreeField=AValue then Exit;
  FTreeField:=AValue;
  if Assigned(FDataSet) then
    SyncDataSource;
end;
procedure TfGridView.CalculateRowHeights;
var
  i: Integer;
begin
  if gList.EditorMode then
    gList.EditorMode:=False;
  gList.BeginUpdate;
  for i := gList.FixedRows to gList.RowCount-1 do
    begin
      if Assigned(gList.Objects[0,i]) then
        TRowObject(gList.Objects[0,i]).Extends := GetRowHeight(i);
      //if TRowObject(gList.Objects[0,i]).Extends.Y<>gList.RowHeights[i] then
      //  gList.RowHeights[i] := TRowObject(gList.Objects[0,i]).Extends.Y;
      if Assigned(gList.Objects[0,i]) then
        TRowObject(gList.Objects[0,i]).RefreshHeight:=True;
    end;
  gList.EndUpdate;
  Application.QueueAsyncCall(@DoInvalidate,0);
end;
procedure TfGridView.AutoInsert;
var
  aKey: word;
begin
  try
    if Visible and gList.CanFocus and (not gList.Focused) then
      gList.SetFocus;
    aKey := VK_DOWN;
    TUnprotectedGrid(gList).KeyDown(aKey,[]);
    SelectCol(IdentField);
  except
  end;
end;
procedure TfGridView.SelectCol(aColName: string);
var
  i: Integer;
begin
  for i := 0 to dgFake.Columns.Count-1 do
    if TColumn(dgFake.Columns[i]).Fieldname = aColName then
      gList.Col:=i+1;
end;
procedure TfGridView.ResetEditor;
var
  OldShowEditor: TGridOptions;
  OldEditorMode: Boolean;
  i: Integer;
  aBookmark: db.LargeInt;
begin
  OldShowEditor := gList.Options;
  OldEditorMode := gList.EditorMode;
  gList.EditorMode := False;
  gList.Options:=OldShowEditor;
  gList.EditorMode := OldEditorMode;
  aBookmark := DataSet.GetBookmark;
  for i := gList.FixedRows to gList.RowCount-1 do
    if TRowObject(gList.Objects[0,i]).Rec = aBookmark then
      begin
        gList.Row:=i;
        break;
      end;
  gList.Invalidate;
end;
procedure TfGridView.SetFocus;
begin
  try
    if CanFocus and Visible then
      inherited;
    if  Visible
    and gList.CanFocus
    and (not gList.Focused)
    then
      begin
        gList.Invalidate;
        gList.SetFocus;
      end;
  except
  end;
end;
procedure TfGridView.SetRights(Editable: Boolean);
begin
  if not Editable then
    gList.Options:=gList.Options-[goEditing]
  else
    gList.Options:=gList.Options+[goEditing];
  FEditable := Editable;
end;
procedure TfGridView.SetChild(Dorefresh : Boolean = True);
var
  OneRowAheadID: LargeInt = 0;
  i: Integer;
  aCol: Integer = -1;
  OwnParentID: String;
  aLevel: Integer;
  aRec: LargeInt;
  Bookmark: LargeInt;
  NewParentID: variant;
  aStrlevel: String;
  aRow: LongInt;
  aTop: Integer;
  SourceLevel: objpas.Integer;
  NewWorkStatus: String='';
begin
  for i := 0 to dgFake.Columns.Count-1 do
    if dgFake.Columns[i].FieldName = IdentField then
      begin
        aCol := i+1;
        break;
      end;
  aTop := gList.TopRow;
  FDataSet.DataSet.DisableControls;
  SourceLevel := GetLevel(aCol,gList.Selection.Top);
  for aRow := gList.Selection.Top to gList.Selection.Bottom do
    begin
      if GotoRowNumber(aRow) then
        begin
          if FDataSet.DataSet.FieldDefs.IndexOf(TreeField) = -1 then exit;
          if FDataSet.CanEdit then
            FDataSet.DataSet.Post;
          gList.Row:=aRow;
          aLevel := GetLevel(aCol,gList.Row);
          if aLevel = SourceLevel then
            begin
              if gList.Row > gList.FixedRows then
                if OneRowAheadID=0 then
                  OneRowAheadID := TRowObject(gList.Objects[0,gList.Row-1]).Rec;
              if aLevel = 0 then
                aLevel := 1;
              if OneRowAheadID <> 0 then
                begin
                  NewParentID := OneRowAheadID;
                  OwnParentID := FDataSet.FieldByName(TreeField).AsString;
                  aRec := DataSet.GetBookmark;
                  if DataSet.GotoBookmark(OneRowAheadID) and (FDataSet.FieldByName(TreeField).AsString <> '') and (FDataSet.FieldByName(TreeField).AsString <> '0') then
                    begin
                      {$ifdef slowdebug}
                      Application.ProcessMessages;
                      {$endif}
                      if OwnParentID <> FDataSet.FieldByName(TreeField).AsString then
                        begin
                          NewParentID := FDataSet.FieldByName(TreeField).AsVariant;
                          aLevel := GetLevel(aCol,gList.Row-1);
                        end
                      else
                        begin
                          inc(aLevel);
                        end;
                      if WorkStatusField<>'' then
                        NewWorkStatus := FDataSet.FieldByName(WorkStatusField).AsString;
                    end;
                  if DataSet.GetBookmark = NewParentID then
                    begin
                      if DataSet.DataSet.FieldDefs.IndexOf(HasChildsField)>0 then
                        begin
                          if not DataSet.CanEdit then
                            DataSet.DataSet.Edit;
                          DataSet.FieldByName(HasChildsField).AsString:='Y';
                          if DataSet.CanEdit then
                            DataSet.DataSet.Post;
                        end;
                    end;
                  DataSet.GotoBookmark(aRec);
                  if not FDataSet.CanEdit then
                    FDataSet.DataSet.Edit;
                  FDataSet.FieldByName(TreeField).AsVariant:=NewParentID;
                  if NewWorkStatus<>'' then
                    FDataSet.FieldByName(WorkStatusField).AsString:=NewWorkStatus;
                  Bookmark := aRec;
                  FDataSet.DataSet.Refresh;
                  DataSet.FreeBookmark(aRec);
                end;
            end;
    end;
  end;
  FDataSet.DataSet.EnableControls;
//  if Dorefresh then
    SyncDataSource;
  for i := 1 to gList.RowCount-1 do
    if TRowObject(gList.Objects[0,i]).Rec = Bookmark then
      begin
        gList.Row:=i;
        break;
      end;
  gList.TopRow:=aTop;
end;
procedure TfGridView.UnSetChild;
var
  OneRowAheadID: LargeInt=0;
  i: Integer;
  aCol: Integer = -1;
  aLevel: Integer;
  OwnParentID: String;
  aRec: LargeInt;
  NewParentID: String;
  aStrlevel: String;
  Bookmark: LargeInt;
  bRec: LargeInt;
  aRow: LongInt;
  aTop: Integer;
  SourceLevel: objpas.Integer;
begin
  for i := 0 to dgFake.Columns.Count-1 do
    if dgFake.Columns[i].FieldName = IdentField then
      begin
        aCol := i+1;
        break;
      end;
  FDataSet.DataSet.DisableControls;
  aTop := gList.TopRow;
  SourceLevel := GetLevel(aCol,gList.Selection.Top);
  for aRow := gList.Selection.Top to gList.Selection.Bottom do
    begin
      if GotoRowNumber(aRow) then
        begin
          gList.Row := aRow;
          if FDataSet.DataSet.FieldDefs.IndexOf(TreeField) = -1 then exit;
          if FDataSet.CanEdit then
            FDataSet.DataSet.Post;
          if gList.Row > 1 then
            OneRowAheadID := TRowObject(gList.Objects[0,gList.Row-1]).Rec;
          aLevel := GetLevel(aCol,gList.Row);
          if alevel = SourceLevel then
            begin
              if aLevel = 0 then
                aLevel := 1;
              OwnParentID := FDataSet.FieldByName(TreeField).AsString;
              if OwnParentID <> '' then
                begin
                  aRec := DataSet.GetBookmark;
                  if DataSet.GotoBookmark(StrToInt64(OwnParentID)) then
                    begin
                      if (FDataSet.FieldByName(TreeField).AsString <> '') and (FDataSet.FieldByName(TreeField).AsString <> '0') then
                        begin
                          if OwnParentID <> FDataSet.FieldByName(TreeField).AsString then
                            begin
                              NewParentID := FDataSet.FieldByName(TreeField).AsString;
                              aLevel := GetLevel(aCol,gList.Row-1);
                            end
                          else
                            begin
                              NewParentID := '';
                            end;
                        end;
                    end;
                  DataSet.GotoBookmark(aRec);
                  if not FDataSet.CanEdit then
                    FDataSet.DataSet.Edit;
                  FDataSet.FieldByName(TreeField).AsString:=NewParentID;
                  if FDataSet.CanEdit then
                    FDataSet.DataSet.Post;
                  DataSet.GotoBookmark(StrToInt64(OwnParentID));
                  if not DataSet.DataSet.Locate(TreeField,OwnParentID,[]) then
                    begin
                      if DataSet.DataSet.FieldDefs.IndexOf(HasChildsField)>0 then
                        begin
                          if not DataSet.CanEdit then
                            DataSet.DataSet.Edit;
                          DataSet.FieldByName(HasChildsField).AsString:='N';
                          if DataSet.CanEdit then
                            DataSet.DataSet.Post;
                        end;
                    end;
                  Bookmark := aRec;
                  DataSet.FreeBookmark(aRec);
                end;
            end;
        end;
    end;
  FDataSet.DataSet.EnableControls;
  SyncDataSource;
  for i := 1 to gList.RowCount-1 do
    if TRowObject(gList.Objects[0,i]).Rec = Bookmark then
      begin
        gList.Row:=i;
        break;
      end;
  gList.TopRow:=aTop;
end;
function TfGridView.GotoActiveRow: Boolean;
begin
  Result := (gList.RowCount>gList.Row)
       and Assigned(gList.Objects[0,gList.Row])
       and ((TRowObject(gList.Objects[0,gList.Row]).Rec = 0) or (DataSet.State = dsInsert) or ((DataSet.State = dsEdit) and (not DataSet.Changed)) or ((TRowObject(gList.Objects[0,gList.Row]).Rec <> 0) and (DataSet.GotoBookmark(TRowObject(gList.Objects[0,gList.Row]).Rec))))
end;
function TfGridView.GotoRow(aBookmark: LargeInt): Boolean;
begin
  Result := False;
  if aBookmark=0 then exit;
  Result := (TRowObject(gList.Objects[0,gList.Row]).Rec <> 0) and ((DataSet.State = dsInsert) or (DataSet.GotoBookmark(aBookMark)));
end;
function TfGridView.GotoRowNumber(aRow: Integer): Boolean;
begin
  Result := False;
  if (aRow<gList.FixedRows) or (aRow>=gList.RowCount) then exit;
  if (aRow = gList.Row) then
    Result := GotoActiveRow
  else
    Result := (TRowObject(gList.Objects[0,aRow]).Rec <> 0)
         and ((DataSet.State = dsInsert)
           or (DataSet.GotoBookmark(TRowObject(gList.Objects[0,aRow]).Rec)));
end;
function TfGridView.GotoDataSetRow: Boolean;
var
  aBm: LargeInt;
  i: Integer;
begin
  aBm := FDataSet.GetBookmark;
  for i := gList.FixedRows to gList.RowCount-1 do
    if TRowObject(gList.Objects[0,i]).Rec = aBm then
      begin
        gList.Row:=i;
        break;
      end;
end;
procedure TfGridView.SetActive;
begin
  if gList.CanFocus then
    gList.SetFocus;
end;
procedure TfGridView.TryToMakeEditorVisible;
begin
  while (gList.CellRect(0,gList.Row).Bottom > gList.Height) and (gList.TopRow < gList.Row) do
    gList.TopRow:=gList.TopRow+1;
end;
procedure TfGridView.UpdateTitle;
var
  i: Integer;
  tmp: string;
begin
  for i := 0 to dgFake.Columns.Count-1 do
    begin
      gHeader.Columns[i].ButtonStyle:=cbsAuto;
      if SortField = TColumn(dgFake.Columns[i]).FieldName then
        begin
          if SortDirection = sdAscending then
            begin
              gList.Columns[i].Title.ImageIndex := 0;
              gHeader.Columns[i].Title.ImageIndex := 0;
            end
          else if SortDirection = sdDescending then
            begin
              gList.Columns[i].Title.ImageIndex := 1;
              gHeader.Columns[i].Title.ImageIndex := 1;
            end
          else
            begin
              gList.Columns[i].Title.ImageIndex := -1;
              gHeader.Columns[i].Title.ImageIndex := -1;
            end;
        end
      else
        begin
          gList.Columns[i].Title.ImageIndex := -1;
          gHeader.Columns[i].Title.ImageIndex := -1;
        end;
    end;
end;
procedure TfGridView.DoSetEdit(Data: PtrInt);
var
  aKey : Word = VK_ESCAPE;
  aRect: Classes.TRect;
begin exit;
  gList.EditorMode:=True;
  aRect := gList.CellRect(gList.Col,gList.Row);
  if gList.Col>1 then
    if Assigned(FSearchKey) then
      FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
end;
procedure TfGridView.DoSetEditDD(Data: PtrInt);
var
  aKey : Word = VK_ESCAPE;
  aRect: Classes.TRect;
begin
  gList.EditorMode:=True;
  aRect := gList.CellRect(gList.Col,gList.Row);
  if gList.Col>1 then
    if Assigned(FSearchKey) then
      FSearchKey(Self,aRect.Left,aRect.Bottom,dgFake.Columns[gList.Col-1],aKey,[],'');
  if gList.Editor is TComboBox then
    TComboBox(gList.Editor).DroppedDown:=True;
end;
procedure TfGridView.DoInvalidate(Data: PtrInt);
begin
  try
    if Assigned(Self) then
      gList.Invalidate;
  except
  end;
end;
procedure TfGridView.CleanList(AddRows: Integer);
var
  i: Integer;
  aCol: Integer = -1;
begin
  for i := 0 to dgFake.Columns.Count-1 do
    if dgFake.Columns[i].FieldName = IdentField then
      begin
        aCol := i+1;
        break;
      end;
  for i := 0 to gList.RowCount-1 do
    CleanRow(i,aCol);
  gList.RowCount:=gList.FixedRows+AddRows;
  for i := gList.FixedRows to gList.RowCount-1 do
    gList.Objects[0,i] := TRowObject.Create;
end;
procedure TfGridView.CleanRow(aRow: Integer; aIdentCol: Integer);
var
  i: Integer;
begin
  if aIdentCol=-2 then
    begin
      aIdentCol:=-1;
      for i := 0 to dgFake.Columns.Count-1 do
        if dgFake.Columns[i].FieldName = IdentField then
          begin
            aIdentCol := i+1;
            break;
          end;
    end;
  if Assigned(gList.Objects[0,aRow]) then
    gList.Objects[0,aRow].Free;
  for i := 1 to gList.ColCount-1 do
    if Assigned(gList.Objects[i,aRow]) then
      begin
        gList.Objects[i,aRow].Free;
        gList.Objects[i,aRow] := nil;
      end;
end;

procedure TfGridView.Asyncrefresh;
begin
  Application.QueueAsyncCall(@DoAsyncRefresh,0);
end;

constructor TfGridView.Create(AOwner: TComponent);
var
  Details: TThemedElementDetails;
begin
  inherited Create(AOwner);
  InEdit := False;
  FDisableEdit:=False;
  FApplyAutoFilter := True;
  FreadOnly:=False;
  FWordwrap:=False;
  FEntered := False;
  FDataSet := nil;
  FDontUpdate := 0;
  FInvertedDrawing:=False;
  FSortDirection:=sdIgnored;
  FInpStringList := TStringList.Create;
  FDefaultRowHeight := False;
  mInplace := TInplaceMemo.Create(Self);
  mInplace.ScrollBars:=ssAutoVertical;
  mInplace.OnEditingDone:=@mInplaceEditingDone;
  mInplace.OnKeyDown:=@mInplaceKeyDown;
  mInplace.BorderStyle:=bsNone;
  mInplace.OnResize:=@mInplaceResize;
  mInplace.WordWrap:=False;
  FIgnoreSettext := False;
  Self.OnEnter:=@fGridViewEnter;
  {$ifdef gridvisible}
  dgFake.Visible := True;
  {$endif}
  Details := ThemeServices.GetElementDetails(ttGlyphOpened);
  FExpandSignSize := ThemeServices.GetDetailSize(Details).cx;
  FirstFocused := False;
  gHeader.CachedEditing:=False;
  SearchKeyTimer := TTimer.Create(Self);
  SearchKeyTimer.Interval:=200;
  SearchKeyTimer.OnTimer:=@SearchKeyTimerTimer;
  gHeader.Visible:=False;
  //gList.ScrollSyncControl := gHeader;
end;
destructor TfGridView.Destroy;
begin
  try
    CleanList(0);
    SearchKeyTimer.Enabled:=False;
    SearchKeyTimer.Free;
    FInpStringList.Destroy;
  except
  end;
  inherited Destroy;
  Self := nil;
end;
function TfGridView.FindRow(aTreeBM: LargeInt): Integer;
var
  i: Integer;
begin
  Result := -1;
  if not Assigned(gList) then exit;
  try
  for i := gList.FixedRows to gList.RowCount-1 do
    if TRowObject(gList.Objects[0,i]).Rec=aTreeBM then
      begin
        Result := i;
        break;
      end;
  except
  end;
end;
function TfGridView.SyncActiveRow(Bookmark : Int64;DoInsert,UpdateData : Boolean;UpdateRowHeight : Boolean;DoGroup : Boolean = True;AddNotFound : Boolean = False;aIdentField : Integer = -1) : Boolean;
var
  i: Integer;
  aRow: Integer = -1;
  aCol: Integer = -1;
  aLevel: Integer;
  NewLevel: Integer;
  a: Integer;
  Found: Boolean;
  aTreeBM : Variant;
  aNewHeight: Integer;

  function ParentExpanded(aID : string) : Boolean;
  begin
    Result := True;
  end;
begin
  Result := False;
  {$ifdef slowdebug}
  Application.ProcessMessages;
  {$endif}
  {$ifdef debug}
  debugln('SyncActiveRow '+IntToStr(Bookmark));
  {$endif}
  gList.OnSelectCell:=nil;
  aCol := aIdentField;
  if aCol = -1 then
    for i := 0 to dgFake.Columns.Count-1 do
      if dgFake.Columns[i].FieldName = IdentField then
        begin
          aCol := i+1;
          break;
        end;
  if not DoGroup then AddNotFound:=True;
  if Bookmark = -1 then Bookmark := 0;
  if (((gList.Row >= gList.RowCount) and (gList.RowCount = gList.FixedRows))
  or (Bookmark = 0) or (not Assigned(gList.Objects[0,gList.Row])) or (not (TRowObject(gList.Objects[0,gList.Row]).Rec = Bookmark)))
  and ((TreeField = '') or (not DoGroup) or (FDataSet.FieldByName(TreeField).IsNull) or (FDataSet.FieldByName(TreeField).AsString='0')) then
    begin
      if DoInsert then
        begin
          gList.RowCount:=gList.RowCount+1;
          gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
          {$ifdef slowdebug}
          Application.ProcessMessages;
          {$endif}
          gList.Row := gList.RowCount-1;
          UpdateData := True;
        end
      else if gList.RowCount = gList.FixedRows then
        begin
          gList.RowCount:=gList.FixedRows+1;
          gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
          UpdateData := True;
        end
      else if gList.Row < gList.RowCount then
        begin
          if Assigned(gList.Objects[0,gList.Row]) then
            TRowObject(gList.Objects[0,gList.Row]).Rec := Bookmark;
          {$ifdef debug}
          debugln('Sync Bookmark=',IntToStr(Bookmark));
          {$endif}
        end;
      Result := True;
    end
  else if (not ((TreeField = '') or (FDataSet.FieldByName(TreeField).IsNull) or (FDataSet.FieldByName(TreeField).AsString='0'))) and (FIdentField <> '') then
    begin //Subentry
      aRow := gList.Row;
      UpdateData := not DoInsert;
      aTreeBM := FDataSet.FieldByName(TreeField).AsVariant;
      Found := False;
      for i := gList.FixedRows to gList.RowCount-1 do
        if TRowObject(gList.Objects[0,i]).Rec=aTreeBM then
          begin
            Found := True;
            if  (HasChilds(aCol,i) = '+') then
              UpdateData := False
            else //Add row after it
              begin
                SetHasChilds(aCol,i,'-');
                if DoInsert and ParentExpanded(FDataSet.FieldByName(TreeField).AsString) then
                  begin
                    NewLevel := GetLevel(aCol,i)+1;
                    a := i+1;
                    while (a < gList.RowCount) and (GetLevel(aCol,a)>=NewLevel) do
                      inc(a);
                    gList.RowCount:=gList.RowCount+1;
                    gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
                    gList.MoveColRow(False,gList.RowCount-1,a);
                    gList.Row := a;
                    SetLevel(aCol,a,NewLevel);
                    inc(aRow);
                    UpdateData := True;
                    Result := True;
                    break;
                  end
              end;
          end;
      if (not Found) and AddNotFound and ((gList.RowCount <= gList.Row) or (TRowObject(gList.Objects[0,gList.Row]).Rec <> 0)) then
          if (gList.RowCount <= gList.Row) or (TRowObject(gList.Objects[0,gList.Row]).Rec <> Bookmark) then
            begin
              gList.RowCount:=gList.RowCount+1;
              gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
              gList.Row := gList.RowCount-1;
              UpdateData := True;
              Found := True;
              Result := Found;
            end;
    end;
  if Result and Assigned(gList.Objects[0,gList.Row]) and (TRowObject(gList.Objects[0,gList.Row]).Rec = 0) then
    TRowObject(gList.Objects[0,gList.Row]).Rec := Bookmark;
  if UpdateData then
    begin
      for i := 1 to gList.ColCount-1 do
        if (dgFake.Columns.Count>=(i-2)) and  Assigned(dgFake.Columns[i-1].Field) then
          begin
            if (dgFake.Columns[i-1].Field.FieldName = IdentField) and (aCol > -1) then
              aLevel := Getlevel(aCol,gList.Row);
            if (HasChildsField <> '')
            and (FDataSet.FieldByName(HasChildsField).AsString='Y') then
              SetHasChilds(aCol,gList.Row,'.');
            if (dgFake.Columns[i-1].Field.FieldName = TextField) and (ShortTextField <> '') then
              begin
                if dgFake.Columns[i-1].Field.AsString = '' then
                  gList.Cells[i,gList.Row] := dgFake.DataSource.DataSet.FieldByName(ShortTextField).AsString
                else
                  gList.Cells[i,gList.Row] := dgFake.DataSource.DataSet.FieldByName(ShortTextField).AsString+lineending+dgFake.Columns[i-1].Field.AsString;
              end
            else
              gList.Cells[i,gList.Row] := dgFake.Columns[i-1].Field.AsString;
            if (dgFake.Columns[i-1].Field.FieldName = IdentField) and (aCol > -1) then
              SetLevel(aCol,gList.Row,aLevel);
          end;
      gList.InvalidateCell(0,gList.Row);
      if Assigned(FAddRow) then fAddRow(gList);
    end;
  if Result and UpdateRowHeight then
    begin
      TRowObject(gList.Objects[0,gList.Row]).Extends := GetRowHeight(gList.Row);
      aNewHeight := TRowObject(gList.Objects[0,gList.Row]).Extends.Y;
      if aNewHeight <> gList.RowHeights[gList.Row] then
        gList.RowHeights[gList.Row] := aNewHeight;
      TRowObject(gList.Objects[0,gList.Row]).RefreshHeight:=True;
    end;
  if aRow > -1 then
    gList.Row := aRow;
  gList.OnSelectCell:=@gListSelectCell;
  {$ifdef slowdebug}
  Application.ProcessMessages;
  {$endif}
end;
procedure TfGridView.SyncDataSource(UpdateHeader : Boolean = True;DoGroup : Boolean = True);
var
  CanSelect: Boolean;
  i: Integer;
  AllDone: Boolean;
  runs: Integer;
  NotDone : TStringList;
  aTime: DWORD;
  aCol: Integer;
  aCnt: Integer;
  aOldCol: Integer;
  procedure AddTasks(aLevel : Integer = 0);
  var
    aRow: Integer;
    aRec: db.LargeInt;
    aParBM: String;
    Bm: Int64;
  begin
    Bm := DataSet.GetBookmark;
    if ((not ((TreeField = '') or (FDataSet.FieldByName(TreeField).IsNull) or (FDataSet.FieldByName(TreeField).AsString='0'))) and (FIdentField <> '')) then
      begin
        aRow := FindRow(FDataSet.FieldByName(TreeField).AsVariant);
        if aRow = -1 then
          begin
            aRec := FDataSet.GetBookmark;
            aParBM := FDataSet.FieldByName(TreeField).AsString;
            if FDataSet.DataSet.Locate('SQL_ID',aParBM,[]) and (aLevel < 1000) then
              AddTasks(aLevel+1);
            FDataSet.GotoBookmark(aRec);
          end;
      end;
    if FindRow(Bm) = -1 then
      if not SyncActiveRow(Bm,True,True,False,DoGroup,(SortField<>''),aCol) then
        SyncActiveRow(Bm,True,True,False,DoGroup,True,aCol);
    DataSet.GotoBookmark(Bm);
  end;

begin
  gList.EditorMode:=False;
  if (not Assigned(FDataSource.DataSet))
  or (not FDataSource.DataSet.Active) then exit;
  {$ifndef slowdebug}
  gList.BeginUpdate;
  {$endif}
  try
    gList.Selection:=Rect(-1,-1,-1,-1);
    if UpdateHeader then
      SetupHeader;
    NotDone := TStringList.Create;
    {$ifndef debug}
    aTime := GetTickCount;
  //  Self.Visible:=False;
    {$else}
    debugln('SyncDataSource');
    {$endif}
    for i := 0 to dgFake.Columns.Count-1 do
      if dgFake.Columns[i].FieldName = IdentField then
        begin
          aCol := i+1;
          break;
        end;
    with FDataSource.DataSet do
      begin
        {$ifndef debug}
        DisableControls;
        {$endif}
        try
          CleanList(0);
          AllDone := True;
          if DataSet.CanEdit then
            begin
              if not ReadOnly then
                begin
                  try
                    DataSet.Post;
                  except
                    DataSet.Cancel;
                  end;
                end
              else DataSet.Cancel;
            end;
          if not FInvertedDrawing then
            begin
              First;
              while not EOF do
                begin
                  AddTasks;
                  Next;
                end;
            end
          else
            begin
              Last;
              while not BOF do
                begin
                  AddTasks;
                  Prior;
                end;
            end;
          if gList.RowCount = gList.FixedRows then
            begin
              gList.RowCount:=gList.FixedRows+1;
              gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
            end;
          gListSelectCell(gList,0,1,CanSelect);
          if Assigned(OnCellChanging) then
            OnCellChanging(Self);
        finally
          {$ifndef debug}
          begin
            while ControlsDisabled do
              EnableControls;
          end;
          {$endif}
        end;
      end;
    NotDone.Free;
    if not FEditable then
      gList.Row:=gList.FixedRows;
    OldRow := gList.Row;
  {$ifndef slowdebug}
  finally
    gList.EndUpdate;
  end;
  aOldCol := gList.Col;
  try
    if gList.ColCount>0 then
      gList.Col := gList.ColCount-1;
    gList.Col := aOldCol;
  except
    gList.Selection:=Rect(-1,-1,-1,-1);
  end;
  {$else}
  debugln('SyncDataSourceEnd='+IntToStr(GetTickCount-aTime));
  {$endif}
end;
procedure TfGridView.SetupHeader;
var
  i: Integer;
  sl : TStringList;
begin
  sl := TStringList.Create;
  for i := 0 to dgFake.Columns.Count-1 do
    begin
      if gHeader.Columns.Count>i then
        if gHeader.Cells[i,1] <> '' then
          sl.Values[dgFake.Columns[i].FieldName]:=gHeader.Cells[i,1];
    end;
  fRowEditor.GetGridSizes(FBaseName,dgFake.DataSource,dgFake,FDefaultRows,False,FBaseName);
  if Assigned(FSetupPosition) then FSetupPosition(Self,gList.Columns);
  gList.Columns.Assign(dgFake.Columns);
  for i := 0 to gList.Columns.Count-1 do
    begin
      if gList.Columns[i].ButtonStyle=cbsCheckboxColumn then
        begin
          gList.Columns[i].ValueChecked:='Y';
          gList.Columns[i].ValueUnChecked:='N';
        end
      ;
    end;
  gHeader.Columns.Assign(gList.Columns);
  for i := 0 to dgFake.Columns.Count-1 do
    begin
      if gHeader.Columns.Count>i then
        begin
          gHeader.Cells[i,1]:=sl.Values[dgFake.Columns[i].FieldName];
          if gHeader.Columns[i].PickList.Count>0 then
            gHeader.OnPickListSelect:=@gHeaderPickListSelect;
        end;
    end;
  sl.Free;
  UpdateTitle;
  gHeader.AutoFillColumns:=gList.AutoFillColumns;
end;
procedure TfGridView.SetEdited;
begin
  WasEditing:=True;
  gListEditingDone(gList);
end;
procedure TfGridView.Append;
var
  DoInsert: Boolean = True;
  DoSync : Boolean = False;
  aBm: Int64 = 0;
  i: Integer;
  aTree: String = '';
  asCol: Integer = -1;
  aLevel: Integer;
  aHasChilds: Char;
begin
  {$ifdef DEBUG}
  debugln('Append');
  {$endif}
  if FDataSet.State = dsInsert then
    FDataSet.Post;
  gList.EditorMode:=False;
  if GotoActiveRow then
    begin
      aBm := DataSet.GetBookmark;
      if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
        TRowObject(gList.Objects[0,gList.Row]).Rec := aBM;
      if aBm = TRowObject(gList.Objects[0,gList.Row]).Rec then
        SyncActiveRow(aBm,false,true);
      if TreeField <> '' then
        aTree := DataSet.FieldByName(TreeField).AsString;
      for i := 0 to dgFake.Columns.Count-1 do
        if dgFake.Columns[i].FieldName = IdentField then
          begin
            asCol := i+1;
            break;
          end;
      if asCol > -1 then
        begin
          aLevel := GetLevel(asCol,gList.Row);
          aHasChilds := HasChilds(asCol,gList.Row);
        end;
    end;
  if Assigned(FBeforeInsert) then
    begin
      FBeforeInsert(Self);
    end;
  FDataSet.DataSet.DisableControls;
  FDataset.DisableChanges;
  try
    FDataSource.DataSet.Append;
    aBm :=0;
    if (gList.RowCount = gList.FixedRows+1) and Assigned(gList.Objects[0,gList.FixedRows]) and (TRowObject(gList.Objects[0,gList.FixedRows]).Rec = 0) then
      begin
        CleanList(0);
      end
    else
      for i := gList.FixedRows to gList.RowCount-1 do
        if Assigned(gList.Objects[0,i]) and (TRowObject(gList.Objects[0,i]).Rec = aBm) then
          break;
    SyncActiveRow(aBm,DoInsert,DoSync);
    if Assigned(FAfterInsert) then
      FAfterInsert(Self);
    if TreeField <> '' then
      DataSet.FieldByName(TreeField).AsString:=aTree;
    OldRow:=gList.RowCount;
  finally
    FDataSet.EnableChanges;
    FDataSet.DataSet.EnableControls;
  end;
  if asCol > -1 then
    begin
      if aLevel > 0 then
        SetLevel(asCol,gList.Row,aLevel);
      if (aHasChilds <> ' ')
      and (aHasChilds <> '.') then
        SetHasChilds(asCol,gList.Row,aHasChilds);
      FEditPrefix := gList.Cells[asCol,gList.Row];
    end;
  if gList.CanFocus then
    gList.SetFocus;
  gList.EditorMode:=True;
end;

procedure TfGridView.BeginUpdate;
begin
  inc(FDontUpdate);
end;

procedure TfGridView.EndUpdate;
begin
  dec(FDontUpdate);
end;

procedure TfGridView.EditingDone;
begin
  inherited EditingDone;
//  if Assigned(FDataSet) and (gList.Row > 0) and Assigned(gList.Objects[0,gList.Row]) and (DataSet.GetBookmark=TRowObject(gList.Objects[0,gList.Row]).Rec) then
//    SyncActiveRow(DataSet.GetBookmark,False,True,True);
  gList.EditorMode:=False;
end;
procedure TfGridView.Insert(SetCol : Boolean);
var
  DoInsert: Boolean = False;
  DoSync : Boolean = False;
  aBm: Int64;
  i: Integer;
  newIndex: Integer;
  aTree: String;
  asCol: Integer = -1;
  aLevel: Integer;
  aHasChilds: Char;
  aPosNo: objpas.Integer;
begin
  if not GotoActiveRow then exit;
  aBm := DataSet.GetBookmark;
  if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
    TRowObject(gList.Objects[0,gList.Row]).Rec := aBM;
  if aBm = TRowObject(gList.Objects[0,gList.Row]).Rec then
    SyncActiveRow(aBm,false,true);
  if TreeField <> '' then
    aTree := DataSet.FieldByName(TreeField).AsString;
  for i := 0 to dgFake.Columns.Count-1 do
    if dgFake.Columns[i].FieldName = IdentField then
      begin
        asCol := i+1;
        break;
      end;
  if asCol > -1 then
    begin
      aLevel := GetLevel(asCol,gList.Row);
      aHasChilds := HasChilds(asCol,gList.Row);
    end;
  if (DataSet.State=dsInsert) and (not DataSet.Changed) then exit;
  if Assigned(FBeforeInsert) then
    begin
      FBeforeInsert(Self);
    end;
  aPosNo := -1;
  if NumberField<>'' then
    begin
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      aPosNo := DataSet.FieldByName(NumberField).AsInteger;
      RenumberRows(gList.Row,1);
    end;
  FDataSource.DataSet.Insert;
  FDataSet.DisableChanges;
  try
    if Assigned(FAfterInsert) then
      FAfterInsert(Self);
    if TreeField <> '' then
      begin
        DataSet.FieldByName(TreeField).AsString:=aTree;
      end;
    if (NumberField <> '') and (aPosNo>-1) then
      begin
        DataSet.FieldByName(NumberField).AsInteger:=aPosNo;
      end;
  finally
    FDataSet.EnableChanges;
  end;
  aBm := DataSet.GetBookmark;
  FDataSet.DataSet.DisableControls;
  try
    gList.EditorMode:=False;
    if (gList.RowCount = gList.FixedRows+1) and (TRowObject(gList.Objects[0,gList.FixedRows]).Rec = 0) then
      begin
        //CleanList(0);
      end
    else
      begin
        for i := gList.FixedRows to gList.RowCount-1 do
          if TRowObject(gList.Objects[0,i]).Rec = aBm then
            break;
        newIndex := gList.Row;
        if NewIndex < 1 then NewIndex := gList.FixedRows;
        gList.InsertColRow(False,NewIndex);
        gList.Objects[0,newIndex]:= TRowObject.Create;
        gList.Row:=NewIndex;
      end;
    SyncActiveRow(aBm,DoInsert,DoSync);
  finally
    FDataSet.DataSet.EnableControls;
  end;
  if asCol > -1 then
    begin
      if aLevel > 0 then
        SetLevel(asCol,gList.Row,aLevel);
      FEditPrefix := gList.Cells[asCol,gList.Row];
      if SetCol then
        begin
          if gList.CanFocus then
            gList.SetFocus;
          gList.Col:=asCol;
        end;
    end;
  if gList.CanFocus then
    gList.SetFocus;
end;

procedure TfGridView.InsertAfter(SetCol: Boolean);
var
  DoInsert: Boolean = False;
  DoSync : Boolean = True;
  aBm: Int64;
  i: Integer;
  newIndex: Integer;
  aTree: String;
  asCol: Integer = -1;
  aLevel: Integer;
  aHasChilds: Char;
  aPosNo: Integer;
begin
  SetFocus;
  SearchKeyTimer.Enabled:=false;
  if (DataSet.State=dsInsert) and (DataSet.Changed) then
    DataSet.Post;
  aBm := DataSet.GetBookmark;
  if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
    TRowObject(gList.Objects[0,gList.Row]).Rec := aBM;
  if aBm = TRowObject(gList.Objects[0,gList.Row]).Rec then
    SyncActiveRow(aBm,false,true)
  else if not GotoActiveRow then exit;
  if TreeField <> '' then
    aTree := DataSet.FieldByName(TreeField).AsString;
  for i := 0 to dgFake.Columns.Count-1 do
    if dgFake.Columns[i].FieldName = IdentField then
      begin
        asCol := i+1;
        break;
      end;
  if asCol > -1 then
    begin
      aLevel := GetLevel(asCol,gList.Row);
      aHasChilds := HasChilds(asCol,gList.Row);
    end;
  if not ((DataSet.State=dsInsert) and (not DataSet.Changed)) then
    begin
      if Assigned(FBeforeInsert) then
        begin
          FBeforeInsert(Self);
        end;
      aBm := DataSet.GetBookmark;
      gList.EditorMode:=False;
      if (gList.RowCount = gList.FixedRows+1) and (TRowObject(gList.Objects[0,gList.FixedRows]).Rec = 0) then
        begin
          //CleanList(0);
        end
      else
        begin
          for i := gList.FixedRows to gList.RowCount-1 do
            if TRowObject(gList.Objects[0,i]).Rec = aBm then
              break;
          newIndex := gList.Row+1;
          if NewIndex < 1 then NewIndex := gList.FixedRows;
          if NewIndex>gList.RowCount then
            begin
              gList.RowCount:=gList.RowCount+1;
              gList.Objects[0,gList.RowCount-1] := TRowObject.Create;
              gList.Row:=gList.RowCount;
            end
          else
            begin
              gList.InsertColRow(False,NewIndex);
              gList.Objects[0,newIndex]:= TRowObject.Create;
              gList.Row:=NewIndex;
            end;
        end;
      aPosNo := -1;
      if (NumberField<>'') and (DataSet.Count>0) then
        begin
          if not DataSet.CanEdit then DataSet.DataSet.Edit;
          aPosNo := DataSet.FieldByName(NumberField).AsInteger+1;
          RenumberRows(gList.Row+1,1);
        end;
      FDataSet.DisableChanges;
      try
        if gList.Row = gList.RowCount-1 then
          FDataSource.DataSet.Append
        else
          FDataSource.DataSet.Insert;
        if Assigned(FAfterInsert) then
          FAfterInsert(Self);
        if TreeField <> '' then
          begin
            DataSet.FieldByName(TreeField).AsString:=aTree;
          end;
        if (NumberField <> '') and (aPosNo>-1) then
          begin
            DataSet.FieldByName(NumberField).AsInteger:=aPosNo;
          end;
        aBm := 0;
      finally
        FDataSet.EnableChanges;
      end;
      FDataSet.DataSet.DisableControls;
      try
        SyncActiveRow(aBm,DoInsert,DoSync);
      finally
        FDataSet.DataSet.EnableControls;
      end;
  end;
  if asCol > -1 then
    begin
      if aLevel > 0 then
        SetLevel(asCol,gList.Row,aLevel);
      FEditPrefix := gList.Cells[asCol,gList.Row];
      if SetCol then
        begin
          if gList.CanFocus then
            gList.SetFocus;
          gList.Col:=asCol;
        end;
    end;
  if gList.CanFocus then
    gList.SetFocus;
end;

procedure TfGridView.Delete;
var
  aOldRow: Integer;
  aRow: Integer;
begin
  for aRow := gList.Selection.Bottom+1 downto gList.Selection.Top+1 do
    begin
      if GotoRowNumber(aRow-1) then
        FDataSet.Delete;
    end;
  if gList.RowCount = gList.FixedRows then
    begin
      CleanList(1);
    end;
  aOldRow := gList.Row;
  try
    gList.Clean(gList.Selection,[]);
  except
  end;
  gList.Row:=aOldRow;
  Refresh;
  GotoActiveRow;
end;
procedure TfGridView.First;
var
  CanSelect: Boolean;
begin
  OldRow := 0;
  gListSelectCell(gList,0,1,CanSelect);
end;
procedure TfGridView.Post;
begin
  if Assigned(FDataSet) and FDataSet.CanEdit then
    begin
      FDataSet.DataSet.Post;
      if TRowObject(gList.Objects[0,gList.Row]).Rec = 0 then
        TRowObject(gList.Objects[0,gList.Row]).Rec := FDataSet.GetBookmark;
      EditingDone;
    end;
end;
procedure TfGridView.Refresh(RefreshDS : Boolean);
var
  aRec: LargeInt;
  aTopRow: Integer;
  aCol: Integer;
  OldIgnore: Boolean;
begin
  if not Assigned(FDataSet) then exit;
  if not FDataSet.DataSet.Active then exit;
  gList.BeginUpdate;
  try
    OldIgnore := FDisableEdit;
    FDisableEdit:=True;
    if gList.EditorMode then gList.EditorMode:=False;
    if DataSet.State = dsInsert then exit;
    aTopRow := gList.TopRow;
    aCol := gList.Col;
    GotoActiveRow;
    aRec := FDataSet.GetBookmark;
    if RefreshDS then
      FDataSet.DataSet.Refresh;
    SyncDataSource;
    FDataSet.GotoBookmark(aRec);
    GotoDataSetRow;
    gList.TopRow := aTopRow;
  finally
    gList.EndUpdate;
  end;
  FDisableEdit:=OldIgnore;
  if InvertedDrawing then
    begin
      while (not glist.IsCellVisible(0,gList.RowCount-1)) and (glist.IsCellVisible(0,gList.Row)) do
        gList.TopRow:=gList.TopRow+1;
    end;
  try
    gList.Col:=aCol;
  except
  end;
end;

procedure TfGridView.RenumberRows(aIndex: Integer;aOffset : Integer = 0);
var
  aPosno: objpas.Integer = -1;
  i: objpas.Integer;
  aNumCol: objpas.Integer;
  aRow: objpas.Integer;
begin
  aRow := gList.Row;
  if not FEditable then exit;
  if aIndex <= gList.FixedRows then
    begin
      aIndex := gList.FixedRows;
      aPosNo := 0+aOffset;
    end;
  if NumberField<>'' then
    begin
      if aIndex=gList.FixedRows then
        begin
          if not GotoRowNumber(gList.FixedRows) then exit;
          if aPosNo=-1 then
            aPosno := DataSet.FieldByName(NumberField).AsInteger+aOffset;
        end
      else
        begin
          if not GotoRowNumber(aIndex-1) then exit;
          if aPosNo=-1 then
            aPosno := DataSet.FieldByName(NumberField).AsInteger+aOffset;
        end;
      aNumCol:=-1;
      for i := 0 to dgFake.Columns.Count-1 do
        if dgFake.Columns[i].FieldName = NumberField then
          begin
            aNumCol := i+1;
            break;
          end;
      {$ifndef DEBUG}
      FDataSet.DataSet.DisableControls;
      {$endif}
      try
        for i := aIndex to gList.RowCount-1 do
          begin
            inc(aPosNo,1);
            if not GotoRowNumber(i) then exit;
            if not DataSet.CanEdit then
              DataSet.DataSet.Edit;
            DataSet.FieldByName(NumberField).AsInteger := aPosno;
            if aNumCol>-1 then
              gList.Cells[aNumCol,i]:=IntToStr(aPosNo);
            if DataSet.CanEdit then
              DataSet.DataSet.Post;
          end;
      finally
        {$ifndef DEBUG}
        FDataSet.DataSet.EnableControls;
        {$endif}
      end;
    end;
  gList.Row:=aRow;
end;

procedure TInplaceMemo.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
end;

procedure TInplaceMemo.ShowControl(AControl: TControl);
begin
  inherited ShowControl(AControl);
end;

procedure TInplaceMemo.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
end;

procedure TInplaceMemo.Change;
var
  aText: TCaption;
begin
  inherited Change;
  aText := Text;
  if (FGrid<>nil) and Visible then
    begin
      TUnprotectedGrid(FGrid).SetEditText(FCol, FRow, aText);
    end;
end;
procedure TInplaceMemo.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_IMPLEMENTED;
end;

procedure TInplaceMemo.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid:=FGrid;
  Msg.Options:=EO_AUTOSIZE or EO_IMPLEMENTED;
end;

procedure TInplaceMemo.msg_SetBounds(var Msg: TGridMessage);
begin
  with Msg.CellRect do
    SetBounds(Left, Top, (Right-Left)-1, Bottom-Top);
end;
procedure TInplaceMemo.msg_SetValue(var Msg: TGridMessage);
var
  aClear: Boolean;
  aText: TCaption;
begin
  if FSetValue then
    begin
      WordWrap:=True;
      aText := Text;
      aClear := length(trim(aText))<2;
      Text:=Msg.Value;
      if aClear then
        SelStart:=length(Msg.Value)+1;
    end;
end;
procedure TInplaceMemo.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;
procedure TInplaceMemo.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TInplaceMemo.WMPaste(var Message: TLMPaste);
var
  SaveClipboard: string;
  tabPos: SizeInt;
  OldText: String;
begin
  SaveClipboard := Clipboard.AsText;
  OldText := SaveClipboard;
  SaveClipboard:=trim(SaveClipboard);
  while (copy(SaveClipboard,length(SaveClipboard)-1,1) = #10)
     or (copy(SaveClipboard,length(SaveClipboard)-1,1) = #13)
     or (copy(SaveClipboard,length(SaveClipboard)-1,1) = ' ') do
   SaveClipboard:=copy(SaveClipboard,0,length(SaveClipboard)-2);
  tabPos := pos(#9,Saveclipboard);
  if tabPos <= 4 then
    SaveClipboard:=copy(SaveClipboard,pos(#9,Saveclipboard)+1,length(SaveClipboard));
  SaveClipboard:=trim(SaveClipboard);
  Clipboard.AsText := SaveClipboard;
  Text := StringReplace(Text,OldText,SaveClipboard,[]);
  inherited;
  EditingDone;
end;

constructor TInplaceMemo.Create(AOwner: TComponent);
begin
  FSetValue:=True;
  inherited Create(AOwner);
  WordWrap:=True;
end;

procedure TInplaceMemo.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
    FGrid.EditingDone;
end;

initialization
end.

