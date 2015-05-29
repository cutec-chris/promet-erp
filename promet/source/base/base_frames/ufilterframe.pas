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
unit uFilterFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, LR_DBSet, LR_Class,
   Forms, StdCtrls, ExtCtrls, Buttons, ComCtrls, DBGrids, Grids,
  Controls, EditBtn, Spin, Menus, ActnList, md5, db, Variants, uIntfStrConsts,
  Dialogs, uExtControls, Graphics, LCLType, uTimeLine, ClipBrd, DbCtrls, math,
  uBaseDbClasses, uBaseDBInterface, uPrometFrames, uBaseApplication,ugridview,uBaseDatasetInterfaces;
type
  TOnGetCellTextEvent = procedure(Sender: TObject; DataCol: Integer;
              Column: TColumn;var aText : string) of object;

  { TfFilter }

  TfFilter = class(TPrometMainFrame)
    acSaveFilter: TAction;
    acFilterRights: TAction;
    acDeleteFilter: TAction;
    acFilter: TAction;
    acOpen: TAction;
    acCopyLink: TAction;
    acImport: TAction;
    acExport: TAction;
    acPrint: TAction;
    acDefaultFilter: TAction;
    acSaveLink: TAction;
    acCopyFilterLink: TAction;
    acInformwithexternMail: TAction;
    acInformwithinternMail: TAction;
    acDelete: TAction;
    acChangeRows: TAction;
    acResetFilter: TAction;
    ActionList: TActionList;
    bEditFilter: TSpeedButton;
    bEditFilter1: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    bExecute: TSpeedButton;
    bFilter: TBitBtn;
    cbFilter: TComboBox;
    cbMaxResults: TCheckBox;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    miAdmin: TMenuItem;
    MenuItem5: TMenuItem;
    pBottom: TPanel;
    PHistory: TfrDBDataSet;
    History: TDatasource;
    List: TDatasource;
    eFilterEdit: TSynMemo;
    LinkSaveDialog: TSaveDialog;
    pmHeader: TPopupMenu;
    sbSave1: TSpeedButton;
    sbSave2: TSpeedButton;
    SynSQLSyn1: TSynSQLSyn;
    Users: TDatasource;
    MandantDetails: TDatasource;
    DBNavigator1: TDBNavigator;
    eFilterIn: TEdit;
    ExtRotatedLabel1: TLabel;
    ExtRotatedLabel2: TLabel;
    ExtRotatedLabel3: TLabel;
    FilterImage: TImage;
    PList: TfrDBDataSet;
    gHeader: TExtStringgrid;
    gList: TExtDBGrid;
    DblClickTimer: TIdleTimer;
    ImageList1: TImageList;
    lFilterEdit: TLabel;
    lFilterIn: TLabel;
    MenuItem1: TMenuItem;
    miImport: TMenuItem;
    miCopyMailAddress: TMenuItem;
    miOpen: TMenuItem;
    miExport: TMenuItem;
    Panel1: TPanel;
    pFilterOpt: TPanel;
    pNav: TPanel;
    Panel5: TPanel;
    pmAction: TPopupMenu;
    Panel3: TPanel;
    Panel4: TPanel;
    pmPopup: TPopupMenu;
    pNav1: TPanel;
    pRight: TPanel;
    pToolbar: TPanel;
    pTop: TPanel;
    Report: TfrReport;
    sbGrids: TPanel;
    sbSave: TSpeedButton;
    sbDelete: TSpeedButton;
    sbSavePublic: TSpeedButton;
    seMaxresults: TSpinEdit;
    SpeedButton2: TSpeedButton;
    tbMenue1: TToolButton;
    tbToolBar: TToolBar;
    ToolBar: TToolBar;
    procedure acChangeRowsExecute(Sender: TObject);
    procedure acCopyFilterLinkExecute(Sender: TObject);
    procedure acCopyLinkExecute(Sender: TObject);
    procedure acDefaultFilterExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteFilterExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acInformwithexternMailExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acResetFilterExecute(Sender: TObject);
    procedure acSaveFilterExecute(Sender: TObject);
    procedure acFilterRightsExecute(Sender: TObject);
    procedure acSaveLinkExecute(Sender: TObject);
    procedure bEditRowsClick(Sender: TObject);
    procedure bFilterKeyPress(Sender: TObject; var Key: char);
    procedure cbFilterSelect(Sender: TObject);
    procedure DatasetAfterScroll(aDataSet: TDataSet);
    procedure DblClickTimerTimer(Sender: TObject);
    procedure DoAsyncRefresh(Data: PtrInt);
    procedure DoAsyncResize(Data: PtrInt);
    procedure eFilterEditChange(Sender: TObject);
    function fSearchOpenUserItem(aLink: string): Boolean;
    procedure gHeaderColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure gHeaderDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure gHeaderGetCellWidth(aCol: Integer; var aNewWidth: Integer);
    procedure gHeaderHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gHeaderHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gHeaderKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gHeaderSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure gHeaderSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure gListColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
    procedure gListColumnSized(Sender: TObject);
    procedure bEditFilterClick(Sender: TObject);
    procedure gListDblClick(Sender: TObject);
    procedure gListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure gListGetCellHint(Sender: TObject; Column: TColumn;
      var AText: String);
    procedure gListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure gListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gListTitleClick(Column: TColumn);
    procedure ListStateChange(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
    procedure sbGridsResize(Sender: TObject);
    procedure seMaxresultsChange(Sender: TObject);
    procedure tbMenueClick(Sender: TObject);
    procedure TCustomComboBoxEnter(Sender: TObject);
    procedure TimeLineSetMarker(Sender: TObject);
    procedure TWinControlKeyPress(Sender: TObject; var Key: char);
  private
    FSearcheMail : string;
    aOldSize : Integer;
    FbaseFilter: string;
    fDefaultRows: string;
    FDestroyDataSet: Boolean;
    FDistinct: Boolean;
    FEditable: Boolean;
    FFilter: string;
    FFilterType: string;
    FGlobalFilter: Boolean;
    FOnClose: TCloseEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOnFilterChanged: TNotifyEvent;
    FOnGetCellText: TOnGetCellTextEvent;
    FOnScrolled: TNotifyEvent;
    FOnViewDetails: TNotifyEvent;
    FSortable: Boolean;
    FSortDirection: TSortDirection;
    FSortField: string;
    FTimelineField: string;
    FStdFilter: string;
    FDefaultSorting : string;
    FDefaultSortDirection : TSortDirection;
    FFirstMove : Boolean;
    HintY: LongInt;
    aSelectedIndex : LongInt;
    function GetAutoFiltered: Boolean;
    function GetFilterIn: string;
    procedure SetBaseFilter(const AValue: string);
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetDefaultRows(const AValue: string);
    procedure SetDistinct(const AValue: Boolean);
    procedure SetEditable(const AValue: Boolean);
    procedure SetFilter(const AValue: string);
    procedure SetFilterIn(const AValue: string);
    procedure SetFilterType(const AValue: string);
    procedure SetSortDirecion(const AValue: TSortDirection);
    procedure SetSortField(const AValue: string);
    procedure SetStdFilter(const AValue: string);
    procedure UpdateTitle;
    procedure SetupHeader;
    procedure DoFilterFocus;
    procedure DoUpdateTimeLine;
    procedure DoUpdateDSCount;
    procedure Asyncrefresh;
    { private declarations }
  public
    { public declarations }
    FAutoFilter : string;
    TimeLine : TTimeLine;
    ColumnWidthHelper : TColumnWidthHelper;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParseForms(Filter : string);
    property Filter : string read FFilter write SetFilter;
    property FilterIn : string read GetFilterIn write SetFilterIn;
    property DefaultFilter : string read FStdFilter write SetStdFilter;
    property FilterType : string read FFilterType write SetFilterType;
    procedure ClearFilter;
    property DefaultRows : string read fDefaultRows write SetDefaultRows;
    property SortDirection : TSortDirection read FSortDirection write SetSortDirecion;
    property SortField : string read FSortField write SetSortField;
    property BaseFilter : string read FbaseFilter write SetBaseFilter;
    property GlobalFilter : Boolean read FGlobalFilter write FGlobalFilter;
    property OnViewDetails : TNotifyEvent read FOnViewDetails write FOnViewDetails;
    property OnScrolled : TNotifyEvent read FOnScrolled write FOnScrolled;
    property OnFilterChanged : TNotifyEvent read FOnFilterChanged write FOnFilterChanged;
    procedure SetActive;
    procedure DoBeforeClose;
    function ShowHint(var HintStr: string;var CanShow: Boolean; var HintInfo: THintInfo) : Boolean;override;
    property Editable : Boolean read FEditable write SetEditable;
    property Sortable : Boolean read FSortable write FSortable;
    property DestroyDataSet : Boolean read FDestroyDataSet write FDestroyDataSet;
    property OnGetCellText : TOnGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnDrawColumnCell : TDrawColumnCellEvent read FOnDrawColumnCell write FOnDrawColumnCell;
    property AutoFiltered : Boolean read GetAutoFiltered;
    function AddToolbarAction(aAction: TAction): TToolButton;
    procedure AddToolbarToggle(aAction : TAction);
    procedure AddContextAction(aAction : TAction);
    function AddFilter(FieldName,Value : string) : Boolean;
    procedure ClearFilters;
    procedure DoRefresh;override;
    procedure SetRights;
    function GetLink(onlyOne: Boolean=True): string;
    procedure ShowFrame;override;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    procedure Open;
  end;
  function AddTab(aPage : TTabSheet;DataSet : TBaseDBDataset) : TfFilter;
implementation
{$R *.lfm}
uses uRowEditor,uSearch, uBaseVisualApplicationTools, uBaseVisualApplication ,
  uFilterTabs,uFormAnimate,uData, uBaseVisualControls,uscriptimport,
  uSelectReport,uOrder,uBaseERPDBClasses,uNRights,LCLProc,
  uPerson,uSendMail,Utils,LCLIntf;
resourcestring
  strRecordCount                            = '%d Einträge';
  strFullRecordCount                        = 'von %d werden %d Einträge angezeigt';

function AddTab(aPage : TTabSheet;DataSet : TBaseDBDataset): TfFilter;
begin
  Result := TfFilter.Create(aPage);
  Result.Parent := aPage;
  Result.Align:=alClient;
  Result.Dataset := DataSet;
  if Assigned(aPage.PageControl.OnChange) then aPage.PageControl.OnChange(nil);
  {$IFDEF MAINAPP}
  result.gList.AlternateColor:=fPersonaloptions.fAlternativeRowColor.Color;
  {$ENDIF}
  Result.Show;
  Result.Open;
end;
//Format:
//%Name:DATE%
//%Name:TEXT%
//%Name:DROPDOWN(Entry 1, Entry 2, Entry 3)%
//%SEARCH%
procedure TfFilter.bEditFilterClick(Sender: TObject);
var
  Animate: TAnimationController;
begin
  Animate := TAnimationController.Create(pTop);
  bEditFilter.Enabled:=False;
  Application.ProcessMessages;
  cbFilter.Style := csDropDownList;
  if bEditFilter.Down then
    begin
      if Data.Users.Rights.Right('EDITFILTER') > RIGHT_READ then
        begin
          Animate.AnimateControlHeight(183);
          cbFilter.Style := csDropDown;
        end
      else
        Animate.AnimateControlHeight(79);
    end
  else
    Animate.AnimateControlHeight(40);
  bEditFilter.Enabled:=True;
  Animate.Free;
end;
procedure TfFilter.gListDblClick(Sender: TObject);
begin
  DblClickTimer.Enabled:=True;
end;
procedure TfFilter.gListDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
const
  memowidth = 60; // 10 Zeichen
var
  s: string;
begin
  if not Assigned(Column) then exit;
  if not Assigned(Column.Field) then exit;
  with (Sender as TDBGrid) do
    begin
      if Column.ButtonStyle = cbsCheckBoxColumn then
        DefaultDrawColumnCell(Rect,DataCol,Column,State)
      else
        begin
          Canvas.FillRect(Rect);
          s := copy(Column.Field.DisplayText, 1, memowidth);
          if Assigned(FOnGetCellText) then
            FOnGetCellText(Sender, DataCol,Column,s);
          if Assigned(FOnDrawColumnCell) then
            FOnDrawColumnCell(Sender,Rect,DataCol,Column,State)
          else
            Canvas.TextOut(Rect.Left+2, Rect.Top+2, s);
          if (DataSet is TBaseERPList) and (TBaseERPList(DataSet).GetStatusFieldName=Column.FieldName) and (TBaseERPList(DataSet).GetStatusIcon>-1) then
            begin
              Canvas.FillRect(Rect);
              uBaseVisualControls.fVisualControls.StatusImages.Draw(Canvas,Rect.Left,Rect.Top,TBaseERPList(DataSet).GetStatusIcon);
              Canvas.TextOut(Rect.Left+2+uBaseVisualControls.fVisualControls.StatusImages.Width, Rect.Top+2, s);
            end;
          if DataCol = ColumnWidthHelper.Index then
            if Assigned(Column.Field) then
              ColumnWidthHelper.MaxWidth := Max(ColumnWidthHelper.MaxWidth, TDBGrid(Sender).Canvas.TextWidth(s));
        end;
    end;
end;
procedure TfFilter.gListGetCellHint(Sender: TObject; Column: TColumn;
  var AText: String);
begin
  AText:=' ';
end;
procedure TfFilter.gListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_UP) and TDBGrid(Sender).DataSource.DataSet.BOF then
    if gHeader.CanFocus then
      begin
        gHeader.Col := gList.SelectedIndex+1;
        gHeader.SetFocus;
        gHeader.EditorMode:=True;
      end;
  if (Key = VK_RETURN) then
    begin
//      if not gList.DataSource.DataSet.EOF then
//        gList.DataSource.DataSet.MoveBy(-1);
      acOpen.Execute;
      Key := 0;
    end;
  if (Key = VK_UP) then
    dec(aSelectedIndex);
  if (Key = VK_DOWN) then
    dec(aSelectedIndex);
  if (Shift = [ssCtrl]) and (Key = ord('A')) then
    begin
      with TdbGrid(Sender).DataSource.DataSet do
        begin
          DisableControls;
          First;
          try
            while not EOF do
              begin
                TdbGrid(Sender).SelectedRows.CurrentRowSelected := True;
                Next;
              end;
          finally
            EnableControls;
          end;
        end;
    end;
end;
procedure TfFilter.gListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  gc: TPoint;
begin
  gc:= gList.MouseCoord(Mouse.CursorPos.x,Mouse.CursorPos.y);
  if gc.y <> HintY then
    begin
      Application.CancelHint;
      gList.ShowHint:=False;
      gList.ShowHint:=True;
      HintY := gc.y;
    end;
end;
procedure TfFilter.gListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  aSelectedIndex:=gList.MouseCoord(0,Y+1).Y;
end;

procedure TfFilter.gListTitleClick(Column: TColumn);
begin
  if not Assigned(Column.Field) then exit;
  if not Sortable then exit;
  if (Column.Field.DataType = ftMemo)
  or (Column.Field.DataType = ftWideMemo)
  or (Column.Field.DataType = ftBlob)
  then exit;
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
  DoUpdateTimeLine;
end;

procedure TfFilter.ListStateChange(Sender: TObject);
begin
end;

procedure TfFilter.RefreshTimerTimer(Sender: TObject);
begin
  if Assigned(FDataSet) then
    DoRefresh;
end;

procedure TfFilter.ReportGetValue(const ParName: String; var ParValue: Variant);
begin
  if Uppercase(ParName)='FILTER' then
    ParValue:=cbFilter.Text;
  if Uppercase(ParName)='FILTERVAR' then
    ParValue:=BuildAutofilter(gList,gHeader);
end;

procedure TfFilter.sbGridsResize(Sender: TObject);
var
  aMult: Extended;
  i: Integer;
begin
  if not Visible then exit;
  if aOldSize = 0 then aOldSize := TPanel(Sender).Width;
  if aOldSize = 0 then exit;
  if Abs(aOldSize-TPanel(Sender).Width) = 0 then exit;//< (gList.Columns.Count*4) then exit;
  aMult := (TPanel(Sender).Width / aOldSize);
  for i := 0 to gList.Columns.Count-1 do
    begin
      gList.Columns[i].Width:=round(gList.Columns[i].Width*aMult);
      gHeader.Columns[i].Width:=gList.Columns[i].Width;
    end;
  aOldSize := TScrollBox(Sender).Width;
end;
procedure TfFilter.seMaxresultsChange(Sender: TObject);
begin
  if not cbMaxResults.Enabled then exit;
  acFilter.Execute;
end;
procedure TfFilter.tbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfFilter.TCustomComboBoxEnter(Sender: TObject);
begin
  Application.ProcessMessages;
  TCustomComboBox(Sender).DroppedDown:=True;
end;

procedure TfFilter.TimeLineSetMarker(Sender: TObject);
var
  a: Integer;
begin
  if FTimeLineField <> '' then
    begin
      for a := 0 to gList.Columns.Count-1 do
        if gList.Columns[a].FieldName = FTimeLineField then
          begin
            gHeader.Cells[a+1,1] := '<'+DateToStr(TimeLine.MarkerDate);
            FAutoFilter := BuildAutofilter(gList,gHeader);
            acFilter.Execute;
            break;
          end;
    end;
end;
procedure TfFilter.TWinControlKeyPress(Sender: TObject; var Key: char);
var
  aControl: TControl;
  i: Integer;
begin
  if Key <> #13 then exit;
  Key := #0;
  i := 0;
  while i < ToolBar.ControlCount do
    begin
      if ToolBar.Controls[i] = TWinControl(Sender).Parent then
        begin
          inc(i);
          break;
        end;
      inc(i);
    end;
  if i < ToolBar.ControlCount then
    begin
      aControl := ToolBar.Controls[i];
      for i := 0 to aControl.ComponentCount-1 do
        if (aControl.Components[i] is TEdit)
        or (aControl.Components[i] is TComboBox)
        or (aControl.Components[i] is TDateEdit)
        then if TWinControl(aControl.Components[i]).CanFocus then
          begin
            TWinControl(aControl.Components[i]).SetFocus;
            exit;
          end;

    end
  else if bFilter.CanFocus then
    begin
      bFilter.SetFocus;
      bFilter.ExecuteDefaultAction;
    end;
end;
function TfFilter.GetFilterIn: string;
begin
  with FDataSet.DataSet as IBaseDBFilter do
    Result := FilterTables;
end;

function TfFilter.GetAutoFiltered: Boolean;
begin
  Result := FAutoFilter<>'';
end;

procedure TfFilter.eFilterEditChange(Sender: TObject);
begin
  ParseForms(eFilterEdit.lines.Text);
end;

function TfFilter.fSearchOpenUserItem(aLink: string): Boolean;
var
  aUser: TUser;
  aCont: TPerson;
  aFile: String;
  sl: TStringList;
begin
  if pos('USERS',aLink)>0 then
    begin
      aUser := TUser.Create(nil);
      aUser.SelectFromLink(aLink);
      aUser.Open;
      if aUser.Count>0 then
        begin
          FSearcheMail := trim(aUser.FieldByName('EMAIL').AsString);
        end;
      aUser.Free;
    end
  else
    begin
      aCont := TPerson.Create(nil);
      aCont.SelectFromLink(aLink);
      aCont.Open;
      if aCont.Count>0 then
        begin
          aCont.ContactData.Open;
          if aCont.ContactData.Locate('TYPE;ACTIVE',VarArrayOf(['EM','Y']),[loPartialKey]) then
            FSearcheMail := aCont.ContactData.FieldByName('DATA').AsString;
        end;
      aCont.Free;
    end;
end;

procedure TfFilter.gHeaderColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin
  if IsColumn then
    begin
      gList.Columns[sIndex-1].Index:=tIndex-1;
      fRowEditor.SetGridSizes('FILTER'+FFilterType,gList.DataSource,gList,cbFilter.Text);
      AsyncRefresh;
    end;
end;
procedure TfFilter.gHeaderDrawCell(Sender: TObject; aCol, aRow: Integer;
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
      Canvas.FillRect(aRect);
      if Cells[aCol,aRow] <> '' then
        TextOut(aRect.Left + 2, aRect.Top + 2, Cells[aCol,aRow])
      else
        TextOut(aRect.Left + 2, aRect.Top + 2, strNoFilter)
    end;
end;
procedure TfFilter.gHeaderGetCellWidth(aCol: Integer; var aNewWidth: Integer);
begin
  ColumnWidthHelper.Index := aCol-1;
  if ColumnWidthHelper.Index < 0 then Exit;
  ColumnWidthHelper.MaxWidth := -1;
  TDBGrid(gList).Repaint;
  aNewWidth := 8 + ColumnWidthHelper.MaxWidth;
end;
procedure TfFilter.gHeaderHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if Index = 0 then exit;
  if IsColumn then
    gList.OnTitleClick(gList.Columns[Index-1]);
end;
procedure TfFilter.gHeaderHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  aWidth: Integer;
  i: Integer;
begin
  if not IsColumn then exit;
  if Sender = gHeader then
    gList.Columns[Index-1].Width:=gHeader.Columns[Index-1].Width;
  Application.QueueAsyncCall(@DoAsyncResize,0);
end;
procedure TfFilter.gHeaderKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN)
  or (Key = VK_DOWN) then
    begin
      acFilter.Execute;
      if gList.CanFocus then
        gList.SetFocus;
      Key := 0;
    end;
end;
procedure TfFilter.gHeaderSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  gList.SelectedIndex:=aCol-1;
  gHeader.EditorMode:=True;
end;
procedure TfFilter.gHeaderSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FAutoFilter := BuildAutoFilter(gList,gHeader);
end;
procedure TfFilter.bFilterKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    bFilter.Action.Execute;
end;

procedure TfFilter.acSaveFilterExecute(Sender: TObject);
begin
  if (cbFilter.Text = strNoSelectFilter) or (cbFilter.Text = '') then
    begin
      Showmessage(strEnterAnFilterName);
      exit;
    end;
  if lowercase(copy(trim(eFilterEdit.Lines.Text),0,6)) = 'select' then
    begin
      if Data.Users.Rights.Right('EDITFILTER') < RIGHT_PERMIT then
        begin
          Showmessage(strNoRightsToSaveComplexFilers);
          exit;
        end;
    end;
  with Application as IBaseDbInterface do
    begin
      with Data.Filters.DataSet as IBaseDBFilter,Data.Filters.DataSet as IbaseManageDB do
        Data.SetFilter(Data.Filters,'',0,'','ASC',False,True,True);
      if Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive,loPartialKey]) then
        Data.Filters.DataSet.Edit
      else
        begin
          Data.Filters.DataSet.Append;
          cbFilter.Items.Add(cbFilter.text);
        end;
      with Data.Filters.DataSet do
        begin
          FieldByName('TYPE').AsString := FilterType;
          FieldByName('NAME').AsString := cbFilter.Text;
          FieldByName('FILTER').AsString := eFilterEdit.Lines.Text;
          FieldByName('FILTERIN').AsString := eFilterIn.Text;
          with DataSet.DataSet as IBaseDbFilter do
            begin
              FieldByName('SORTFIELD').AsString := SortFields;
              if SortDirection =sdDescending then
                FieldByName('SORTDIR').AsString := 'DESC'
              else
                FieldByName('SORTDIR').AsString := 'ASC';
            end;
          Post;
        end;
    end;
end;
procedure TfFilter.acFilterRightsExecute(Sender: TObject);
begin
  if (cbFilter.Text = strNoSelectFilter) or (cbFilter.Text = '') then
    begin
      Showmessage(strEnterAnFilterName);
      exit;
    end;
  with Application as IBaseDbInterface do
    begin
      if Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive,loPartialKey]) then
        fNRights.Execute(data.Filters.Id.AsVariant);
    end;
end;

procedure TfFilter.acSaveLinkExecute(Sender: TObject);
var
  sl: TStringList;
  tmp: String;
begin
  if LinkSaveDialog.Execute then
    begin
      sl := TStringList.Create;
      sl.Add(Data.BuildLink(gList.DataSource.DataSet));
      tmp := LinkSaveDialog.FileName;
      if lowercase(ExtractFileExt(tmp)) <> '.plink' then
        tmp := tmp+'.plink';
      sl.SaveToFile(tmp);
      sl.Free;
    end;
end;

procedure TfFilter.bEditRowsClick(Sender: TObject);
begin

end;

procedure TfFilter.acDeleteFilterExecute(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    begin
      with Data.Filters.DataSet as IBaseDBFilter,Data.Filters.DataSet as IbaseManageDB do
        Data.SetFilter(Data.Filters,'',0,'','ASC',False,True,True);
      if Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive,loPartialKey]) then
        if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
          begin
            Data.Filters.DataSet.Delete;
            cbFilter.Items.Delete(cbFilter.ItemIndex);
          end;
    end;
end;
procedure TfFilter.acExportExecute(Sender: TObject);
begin
  fScriptImport.Execute(icExport,FilterType);
end;
procedure TfFilter.acCopyLinkExecute(Sender: TObject);
var
  aLinks : string = '';
  Stream: TStringStream;
  i: Integer;
begin
  if gList.SelectedRows.Count > 0 then
    begin
      for i := 0 to gList.SelectedRows.Count-1 do
        begin
          gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
          with Application as IBaseDbInterface do
            aLinks := aLinks+Data.BuildLink(gList.DataSource.DataSet)+';';
        end;
      gList.SelectedRows.Clear;
    end
  else
    with Application as IBaseDbInterface do
      aLinks := aLinks+Data.BuildLink(gList.DataSource.DataSet)+';';
  Stream := TStringStream.Create(aLinks);
  Clipboard.AddFormat(LinkClipboardFormat,Stream);
  Stream.Free;
end;

procedure TfFilter.acCopyFilterLinkExecute(Sender: TObject);
var
  aLinks: String;
  Stream: TStringStream;
begin
  if Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive]) then
    begin
      aLinks := Data.BuildLink(Data.Filters.DataSet)+';';
      Stream := TStringStream.Create(aLinks);
      Clipboard.AddFormat(LinkClipboardFormat,Stream);
      Stream.Free;
    end;
end;

procedure TfFilter.acChangeRowsExecute(Sender: TObject);
begin
  fRowEditor.Execute     ('FILTER'+FFilterType,List,gList,cbFilter.Text);
  fRowEditor.GetGridSizes('FILTER'+FFilterType,List,gList,FDefaultRows,not FEditable,cbFilter.Text);
  SetupHeader;
  UpdateTitle;
end;

procedure TfFilter.acDefaultFilterExecute(Sender: TObject);
begin
  if (cbFilter.Text = strNoSelectFilter) or (cbFilter.Text = '') then
    begin
      with Application as IBaseDbInterface do
        DBConfig.WriteString('DEFAULTFILTER'+FFilterType,'');
      exit;
    end;
  with Application as IBaseDbInterface do
    begin
      DBConfig.WriteString('DEFAULTFILTER'+FFilterType,cbFilter.Text);
    end;
end;

procedure TfFilter.acDeleteExecute(Sender: TObject);
var
  aLinks : string = '';
  Stream: TStringStream;
  i: Integer;
begin
  if gList.SelectedRows.Count > 0 then
    begin
      if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
        begin
          for i := 0 to gList.SelectedRows.Count-1 do
            begin
              gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
              gList.DataSource.DataSet.Delete;
            end;
          gList.SelectedRows.Clear;
        end;
    end
  else
    with Application as IBaseDbInterface do
      if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
        begin
          gList.DataSource.DataSet.Delete;
        end;
end;

procedure TfFilter.acFilterExecute(Sender: TObject);
var
  aFilter: String;
  tmp,adata,aname: String;
  aControl: TControl;
  Rec: LongInt = 0;
  aFullCount: Integer;
begin
  if (not Assigned(DataSet)) or (not Assigned(DataSet.DataSet)) then exit;
  if DataSet.DataSet.Active then
    Rec := DataSet.GetBookmark;
  try
    aFilter := eFilterEdit.Lines.Text;
    tmp := aFilter;
    tmp := StringReplace(tmp,'@PERMISSIONJOIN@','&PERMISSIONJOIN&',[rfReplaceAll]);
    tmp := StringReplace(tmp,'@PERMISSIONWHERE@','&PERMISSIONWHERE&',[rfReplaceAll]);
    tmp := StringReplace(tmp,'@DEFAULTORDER@','&DEFAULTORDER&',[rfReplaceAll]);
    tmp := StringReplace(tmp,'@AUTOFILTER@','&AUTOFILTER&',[rfReplaceAll]);
    aFilter := '';
    while pos('@',tmp) > 0 do
      begin
        aFilter := aFilter+copy(tmp,0,pos('@',tmp)-1);
        tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
        adata := copy(tmp,0,pos('@',tmp)-1);
        if adata = '' then break;
        tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
        aname := copy(adata,0,pos(':',adata)-1);
        if aname = '' then
          aname := adata;
        if (ToolBar.FindChildControl('TBC'+MD5Print(MD5String(aname))) <> nil) then
          with Application as IBaseDbInterface do
            begin
              aControl := ToolBar.FindChildControl('TBC'+MD5Print(MD5String(aname)));
              aControl := TWinControl(aControl).FindChildControl('TBE'+MD5Print(MD5String(aname)));
              if aControl is TEdit then
                aFilter := aFilter+TEdit(aControl).Text
              else if aControl is TDateEdit then
                aFilter := aFilter+Data.DateTimeToFilter(TDateEdit(aControl).Date)
              else if aControl is TComboBox then
                aFilter := aFilter+TComboBox(aControl).Text;
            end;
      end;
    aFilter := aFilter+tmp;
    aFilter := StringReplace(aFilter,'&PERMISSIONJOIN&','@PERMISSIONJOIN@',[rfReplaceAll]);
    aFilter := StringReplace(aFilter,'&PERMISSIONWHERE&','@PERMISSIONWHERE@',[rfReplaceAll]);
    aFilter := StringReplace(aFilter,'&DEFAULTORDER&','@DEFAULTORDER@',[rfReplaceAll]);
    if lowercase(copy(trim(aFilter),0,6)) = 'select' then
      begin
        //TODO:Security Risk if eFilterEdit.Lines.Text changed !!!
        with DataSet.DataSet as IBaseDBFilter do
          begin
            if FAutoFilter <> '' then
              FullSQL:=StringReplace(aFilter,'&AUTOFILTER&',' AND ('+FAutoFilter+')',[])
            else
              FullSQL:=StringReplace(aFilter,'&AUTOFILTER&','',[]);
            if cbMaxResults.Checked then
              Limit := seMaxResults.Value
            else Limit := 0;
            if SortField <> '' then
              SortFields := SortField
            else SortFields := BaseSortFields;
            SortDirection := FSortDirection;
            DataSet.Open;
          end;
      end
    else
      begin
        with DataSet.DataSet as IBaseDBFilter do
          aFilter := Data.ProcessTerm(aFilter);
        if pos('$',aFilter) > 0 then
          begin
            aFilter := StringReplace(aFilter,'$NOW()',Data.DateTimeToFilter(Now()),[rfReplaceAll,rfIgnoreCase]);
          end;
        if FBaseFilter <> '' then
          begin
            if aFilter = '' then
              aFilter := FBaseFilter
            else
              aFilter := '('+FBaseFilter+') and ('+aFilter+')';
          end;
        if FautoFilter <> '' then
          begin
            if aFilter = '' then
              aFilter := FAutoFilter
            else
              aFilter := '('+aFilter+') and ('+FAutoFilter+')';
          end;
        with DataSet.DataSet as IBaseDBFilter do
          begin
            Filter := aFilter;
            if cbMaxResults.Checked then
              Limit := seMaxResults.Value
            else Limit := 0;
            if SortField <> '' then
              SortFields := SortField
            else SortFields := BaseSortFields;
            SortDirection := FSortDirection;
            Distinct := FDistinct;
            DataSet.Open;
          end;
      end;
    if Rec <> 0 then
      begin
        DataSet.GotoBookmark(Rec);
        DataSet.FreeBookmark(Rec);
      end;
    DoUpdateDSCount;
    if FAutoFilter <> '' then
      DoUpdateTimeLine;
    if acFilter.ActionComponent = bFilter then
      DoFilterFocus;
    if Assigned(FOnFilterChanged) then
      FOnFilterChanged(Self);
  except
    on e : Exception do
      begin
        pBottom.Caption:=e.Message;
        Rec := 0;
        raise;
      end;
  end;
end;
procedure TfFilter.acImportExecute(Sender: TObject);
begin
  fScriptImport.Execute(icImport,FilterType);
  List.DataSet.Refresh;
end;

procedure TfFilter.acInformwithexternMailExecute(Sender: TObject);
var
  i: Integer;
  aLink: String;
  aFile: String;
  sl: TStringList;
begin
  fSearch.SetLanguage;
  i := 0;
  while i < fSearch.cbSearchType.Count do
    begin
      if  (fSearch.cbSearchType.Items[i] <> strUsers)
      and (fSearch.cbSearchType.Items[i] <> strCustomers) then
        fSearch.cbSearchType.Items.Delete(i)
      else
        inc(i);
    end;
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenUserItem;
  FSearcheMail:='';
  fSearch.Execute(True,'LISTU',strSearchFromMailSelect);
  fSearch.SetLanguage;
  aLink := GetLink;
  if (aLink<>'') then
    begin
      with BaseApplication as IBaseApplication do
        aFile := GetInternalTempDir+ValidateFileName(Data.GetLinkDesc(aLink))+'.plink';
      sl := TStringList.Create;
      sl.Add(aLink);
      sl.SaveToFile(aFile);
      sl.Free;
      DoSendMail(Data.GetLinkDesc(aLink),Data.GetLinkLongDesc(aLink), aFile,'','','',FSearcheMail);
    end;
end;

procedure TfFilter.acOpenExecute(Sender: TObject);
var
  aLink: String;
begin
  if Assigned(FOnViewDetails) then
    FOnViewDetails(Self)
  else
    Data.GotoLink(Data.BuildLink(DataSet.DataSet));
end;
procedure TfFilter.acPrintExecute(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  fSelectReport.Report := Report;
  fSelectReport.SetLanguage;
  MandantDetails.DataSet := Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  if Supports(FDataSet, IBaseHistory, Hist) then
    History.DataSet := Hist.GetHistory.DataSet;
  Users.DataSet := Data.Users.DataSet;
  List.DataSet.DisableControls;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      if TableName = 'ORDERS' then fSelectReport.ReportType := 'ORDL'
      else if TableName = 'CUSTOMERS' then fSelectReport.ReportType := 'CUSL'
      else if TableName = 'MASTERDATA' then fSelectReport.ReportType := 'ARTL'
      else fSelectReport.ReportType := copy(TableName,0,3)+copy(TableName,length(Tablename)-1,1);
    end;
  fSelectReport.Execute;
  List.DataSet.EnableControls;
end;

procedure TfFilter.acResetFilterExecute(Sender: TObject);
begin
  ClearFilters;
end;

procedure TfFilter.cbFilterSelect(Sender: TObject);
var
  aControl: TControl;
  i: Integer;
begin
  try
    with Application as IBaseDBInterface do
      begin
        eFilterIn.Text:='';
        if cbFilter.ItemIndex=0 then
          begin
            Filter := DefaultFilter;
            SortDirection := FDefaultSortDirection;
            SortField := FDefaultSorting;
          end
        else
          begin
            if not Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive]) then
              with Data.Filters.DataSet,Data.Filters.DataSet as IBaseDBFilter do
                begin
                  Filter := '';
                  Open;
                  if not Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive]) then
                    begin
                      raise Exception.CreateFmt(strFilterNotFound,[cbFilter.Text]);
                      exit;
                    end;
                end;
            FilterIn := Data.Filters.FieldByName('FILTERIN').AsString;
            eFilterIn.Text:=FilterIn;
            Filter := Data.Filters.FieldByName('FILTER').AsString;
            SortField := Data.Filters.FieldByName('SORTFIELD').AsString;
            if Data.Filters.FieldByName('SORTDIR').AsString = 'DESC' then
              SortDirection := sdDescending
            else if Data.Filters.FieldByName('SORTDIR').AsString = 'ASC' then
              SortDirection := sdAscending
            else
              SortDirection := sdIgnored;
            FDataSet.Open;
          end;
        if Parent is TTabSheet then
          begin
            TTabSheet(Parent).Caption:=cbFilter.Text;
            if cbFilter.Text = '' then
              TTabSheet(Parent).Caption:=TfFilterTabs(TTabSheet(Parent).PageControl.Parent).TabNames;
          end;
        DoFilterFocus;
        UpdateTitle;
        if bFilter.Focused or gList.Focused then
          acFilter.Execute;
      end;
  except
    on e : Exception do
      begin
        pBottom.Caption:=e.Message;
      end;
  end;
end;
procedure TfFilter.DatasetAfterScroll(aDataSet: TDataSet);
begin
  if Assigned(FOnScrolled) then
    begin
      try
        FOnScrolled(Self);
      except
      end;
    end;
end;
procedure TfFilter.DblClickTimerTimer(Sender: TObject);
begin
  DblClickTimer.Enabled:=False;
  acOpen.Execute;
end;
procedure TfFilter.DoAsyncRefresh(Data: PtrInt);
begin
  fRowEditor.GetGridSizes('FILTER'+FFilterType,List,gList,FDefaultRows,not FEditable,cbFilter.Text);
  gHeader.Columns.Assign(gList.Columns);
  UpdateTitle;
end;
procedure TfFilter.DoAsyncResize(Data: PtrInt);
begin
  fRowEditor.SetGridSizes('FILTER'+FFilterType,gList.DataSource,gList,cbFilter.Text);
  Asyncrefresh;
end;
procedure TfFilter.gListColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer
  );
begin
  fRowEditor.SetGridSizes('FILTER'+FFilterType,gList.DataSource,gList,cbFilter.Text);
end;
procedure TfFilter.gListColumnSized(Sender: TObject);
begin
  fRowEditor.SetGridSizes('FILTER'+FFilterType,List,gList,cbFilter.Text);
end;
procedure TfFilter.SetFilterType(const AValue: string);
begin
  if FFilterType=AValue then exit;
  FFilterType:=AValue;
//  pTop.Visible := (FFilterType <> '');
  cbFilter.Text := strNoSelectFilter;
  cbFilter.Clear;
  cbFilter.Items.Add(strNoSelectFilter);
  cbFilter.Text:=strNoSelectFilter;
  with Application as IBaseDBInterface do
    begin
      with Data.Filters.DataSet do
        begin
          with Data.Filters.DataSet as IBaseDBFilter,Data.Filters.DataSet as IbaseManageDB do
            Data.SetFilter(Data.Filters,'',0,'','ASC',False,True,True);
          First;
          while not EOF do
            begin
              if Data.Filters.DataSet.FieldByName('TYPE').AsString=AValue then
                cbFilter.Items.Add(FieldbyName('NAME').AsString);
              Next;
            end;
        end;
    end;
end;
procedure TfFilter.SetSortDirecion(const AValue: TSortDirection);
begin
  if FSortDirection=AValue then exit;
  FSortDirection:=AValue;
end;
procedure TfFilter.SetSortField(const AValue: string);
begin
  if FSortField=AValue then exit;
  FSortField:=AValue;
  with FDataSet.DataSet as IBaseDbFilter do
    begin
      UseBaseSorting:=FSortField='';
    end;
end;
procedure TfFilter.SetStdFilter(const AValue: string);
begin
  if FStdFilter=AValue then exit;
  FStdFilter:=AValue;
  Filter := FStdFilter;
end;
procedure TfFilter.UpdateTitle;
var
  i: Integer;
  tmp: string;
begin
  for i := 0 to gList.Columns.Count-1 do
    begin
      gHeader.Columns[i].ButtonStyle:=cbsAuto;
      gHeader.Columns[i].Width:=TColumn(gList.Columns[i]).Width;
      if SortField = TColumn(gList.Columns[i]).FieldName then
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
  for i := 0 to gHeader.Columns.Count-1 do
    gHeader.Columns[i].ReadOnly:=false;
end;

procedure TfFilter.SetupHeader;
begin
  gHeader.Columns.Assign(gList.Columns);
  //Self.ClearFilters;
  UpdateTitle;
end;

procedure TfFilter.DoFilterFocus;
var
  aControl: TControl;
  i: Integer;
begin
  if ToolBar.ComponentCount > 0 then
    begin
      if ToolBar.ControlCount>0 then
        aControl := ToolBar.Controls[0];
      for i := 0 to aControl.ComponentCount-1 do
        if (aControl.Components[i] is TEdit)
        or (aControl.Components[i] is TComboBox)
        or (aControl.Components[i] is TDateEdit)
        then if TWinControl(aControl.Components[i]).CanFocus then
          begin
            TWinControl(aControl.Components[i]).SetFocus;
            TCustomEdit(aControl.Components[i]).SelectAll;
            exit;
          end;
    end
  else if gList.CanFocus then
    gList.SetFocus
  else if bFilter.CanFocus then
    bFilter.SetFocus;
end;
procedure TfFilter.DoUpdateTimeLine;
var
  a: Integer;
  newVisible: Boolean;
  aFilter: TStringList;
  i: Integer;
begin
  newVisible := False;
  TimeLine.OnSetMarker:=nil;
  FTimeLineField := '';
  if FDataSet.DataSet.FieldDefs.IndexOf(SortField) > -1 then
    begin
      if (FDataSet.FieldByName(SortField).DataType = ftDate) or (FDataSet.FieldByName(SortField).DataType = ftDateTime) then
        FTimeLineField := SortField;
    end
  else if FDataSet.DataSet.FieldDefs.IndexOf('TIMESTAMPD') > -1 then
    begin
      FTimeLineField := 'TIMESTAMPD';
    end;
  if FTimeLineField = '' then
    begin
      pRight.Visible := False;
      exit;
    end;
  for a := 0 to gList.Columns.Count-1 do
    if gList.Columns[a].FieldName = FTimeLineField then
      begin
        newVisible:=True;
        break;
      end;
  if NewVisible then
    begin
      if FDataSet.FieldByName(FTimeLineField).AsDateTime = 0 then
        newVisible := False
      else
        begin
          TimeLine.StartDate:=FDataSet.FieldByName(FTimeLineField).AsDateTime+15;
          TimeLine.MarkerDate:=FDataSet.FieldByName(FTimeLineField).AsDateTime;
          TimeLine.OnSetMarker:=@TimeLineSetMarker;
        end;
    end;
  if NewVisible <> pRight.Visible then
    begin
      pRight.Visible := NewVisible;
      fRowEditor.GetGridSizes('FILTER'+FFilterType,List,gList,FDefaultRows,not FEditable,cbFilter.Text);
      aFilter := TStringList.Create;
      for i := 1 to gHeader.Columns.Count do
        aFilter.Add(gHeader.Cells[i,1]);
      gHeader.Columns.Assign(gList.Columns);
      for a := 0 to gList.Columns.Count-1 do
        begin
          gHeader.Columns[a].ButtonStyle:=cbsAuto;
          gHeader.Columns[a].PickList.Clear;
          gHeader.Columns[a].ReadOnly:=False;
        end;
      if Assigned(gList.DataSource.DataSet) and (gList.DataSource.DataSet.Active) then
        begin
          for i := 1 to aFilter.Count do
            gHeader.Cells[i,1] := aFilter[i-1];
        end;
      FAutoFilter := BuildAutoFilter(gList,gHeader);
    end;
  DoUpdateDSCount;
end;
procedure TfFilter.DoUpdateDSCount;
var
  aFullCount: Integer;
begin
  aFullCount := DataSet.FullCount;
  if aFullCount > DataSet.Count then
    pBottom.Caption:=Format(strFullRecordCount,[aFullCount,List.DataSet.RecordCount])
  else
    pBottom.Caption:=Format(strRecordCount,[List.DataSet.RecordCount]);
end;

procedure TfFilter.Asyncrefresh;
begin
  Application.QueueAsyncCall(@DoAsyncRefresh,0);
end;

constructor TfFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortDirection := sdDescending;
  FDestroyDataSet := True;
  pTop.Height:=40;
  gHeader.DefaultRowHeight:=22;
  gHeader.EditorBorderStyle:=bsNone;
  FGlobalFilter := True;
  FFirstMove := True;
  TimeLine := TTimeLine.Create(Self);
  TimeLine.Parent := pRight;
  TimeLine.Align := alClient;
  TimeLine.Orientation:=toHorizontal;
  TimeLine.Hint:=pRight.Hint;
  FEditable := False;
  gList.ScrollSyncControl := gHeader;
  tbToolbar.Images := fVisualControls.Images;
  gList.UseExtPicklist:=true;
  gHeader.CachedEditing:=False;
  FSortable:=True;
  SetRights;
end;
destructor TfFilter.Destroy;
var aClose: TCloseAction;
begin
  try
    if Assigned(DataSet) and Assigned(DataSet.DataSet) then DataSet.DataSet.AfterScroll:=@DatasetAfterScroll;
  except
  end;
  if Assigned(FOnClose) then
    FOnClose(Self,aClose);
  if Assigned(FOnScrolled) then
    FOnScrolled := nil;
  if Assigned(PRight) then
    PRight.Visible:=False;
  inherited Destroy;
end;
procedure TfFilter.SetFilter(const AValue: string);
var
  aFilter: TStringList;
  i: Integer;
  aChanged: Boolean;
begin
  aChanged := FFilter <> AValue;
  FFilter := AValue;
  eFilterEdit.Lines.Text:=FFilter;
  ParseForms(FFilter);
  fRowEditor.GetGridSizes('FILTER'+FFilterType,List,gList,FDefaultRows,not FEditable,cbFilter.Text);
  aFilter := TStringList.Create;
  for i := 1 to gHeader.Columns.Count do
    aFilter.Add(gHeader.Cells[i,1]);
  gHeader.Columns.Assign(gList.Columns);
  if not aChanged then
    begin
      if Assigned(gList.DataSource.DataSet) and (gList.DataSource.DataSet.Active) then
        begin
          for i := 1 to aFilter.Count-1 do
            gHeader.Cells[i,1] := aFilter[i-1];
        end;
      FAutoFilter := BuildAutoFilter(gList,gHeader);
    end
  else
    begin
      Self.ClearFilters;
      FAutoFilter := '';
    end;
  aFilter.Free;
end;
procedure TfFilter.SetFilterIn(const AValue: string);
begin
  with FDataSet.DataSet as IBaseDBFilter do
    FilterTables := AValue;
end;
procedure TfFilter.SetDataSet(const AValue: TBaseDBDataSet);
  procedure SetSorting;
  begin
    with FDataSet.DataSet as IBaseDbFilter do
      begin
        if FSortField <> '' then
          begin
            SortFields := FSortField;
            SortDirection := FSortDirection;
          end
        else
          begin
            FSortField := SortFields;
            FSortDirection := SortDirection;
          end;
      end;
  end;
var
  aFilter: String;
begin
  inherited SetDataSet(AValue);
  if not Assigned(fDataSet) then exit;
  try
    if Assigned(FDataSet) and Assigned(FDataSet.DataSet) then
      FDataSet.DataSet.AfterScroll:=nil;
  except
  end;
  with Application as IBaseDBInterface do
    aFilter := DBConfig.ReadString('DEFAULTFILTER'+FFilterType,'');
  List.DataSet := FDataset.DataSet;
  if aFilter = '' then
    begin
      with FDataSet.DataSet as IBaseDbFilter do
        begin
          if DefaultFilter <> '' then
            begin
              Filter := DefaultFilter;
              if cbMaxresults.Checked then
                Limit := seMaxResults.Value
              else Limit := 0;
              SetSorting;
              GlobalFilter := FGlobalFilter;
            end
          else
            begin
              FDataSet.Open;
              if Limit > 0 then
                begin
                  cbMaxresults.Enabled:=False;
                  cbMaxresults.Checked:=True;
                  seMaxResults.Value:=Limit;
                  cbMaxresults.Enabled:=True;
                end
              else cbMaxResults.Checked := False;
              SetSorting;
              FGlobalFilter := GlobalFilter;
            end;
        end;
    end
  else
    begin
      cbFilter.Text := aFilter;
      cbFilterSelect(nil);
    end;
  DataSet.DataSet.AfterScroll:=@DatasetAfterScroll;
  pRight.Visible:=False;
  fRowEditor.GetGridSizes('FILTER'+FFilterType,List,gList,FDefaultRows,not FEditable,cbFilter.Text);

  DefaultFilter := '';
  gHeader.Columns.Assign(gList.Columns);
//  Self.ClearFilters;
//  gHeaderHeaderSized(nil,True,1);
  UpdateTitle;
  DoUpdateTimeLine;
  if cbFilter.ItemIndex < 1 then
    begin
      FDefaultSortDirection:=SortDirection;
      FDefaultSorting:=SortField;
    end;
  if Assigned(FOnFilterChanged) then
    FOnFilterChanged(Self);
end;
procedure TfFilter.SetBaseFilter(const AValue: string);
begin
  if FbaseFilter=AValue then
    begin
      DataSet.DataSet.Refresh;
      exit;
    end;
  FbaseFilter:=AValue;
//  acFilter.Execute;
end;
procedure TfFilter.SetDefaultRows(const AValue: string);
begin
  if fDefaultRows=AValue then exit;
  fDefaultRows:=AValue;
  fRowEditor.GetGridSizes('FILTER'+FFilterType,gList.DataSource,gList,FDefaultRows,not FEditable,cbFilter.Text);
  gHeader.Columns.Assign(gList.Columns);
  Self.ClearFilters;
  UpdateTitle;
end;
procedure TfFilter.SetDistinct(const AValue: Boolean);
begin
  if FDistinct=AValue then exit;
  FDistinct:=AValue;
end;
procedure TfFilter.SetEditable(const AValue: Boolean);
begin
  if FEditable=AValue then exit;
  FEditable:=AValue;
  if FEditable then
    begin
      gList.Options := gList.Options-[dgRowSelect];
      gList.Options := gList.Options+[dgEditing];
    end
  else
    begin
      gList.Options := gList.Options+[dgRowSelect];
      gList.Options := gList.Options-[dgEditing];
    end;
  if AValue then gList.CachedEditing:=False;
end;
procedure TfFilter.ParseForms(Filter: string);
var
  aControl : TControl;
  tmp: String;
  data: String;
  aname: String;
  aPanel: TPanel;
  aLabel: TLabel;
  i: LongInt;
  aBevel: TBevel;
  aControladded: Boolean = False;
  function IsChange : Boolean;
  var
    tmp: String;
    data: String;
    aname: String;
    aCount : Integer = 0;
  begin
    Result := False;
    tmp := Filter;
    while pos('@',tmp) > 0 do
      begin
        tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
        data := copy(tmp,0,pos('@',tmp)-1);
        if data = '' then break;
        tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
        aname := copy(data,0,pos(':',data)-1);
        if aname = '' then
          aname := data;
        if (ToolBar.FindChildControl('TBC'+MD5Print(MD5String(aname))) = nil) then
          begin
            Result := True;
            exit;
          end;
        inc(aCount);
      end;
    Result := aCount <> ToolBar.ControlCount;
  end;

begin
  if not IsChange then exit;
  bFilter.Visible:=False;
  bFilter.Parent := Self;
  while ToolBar.ControlCount > 0 do
    begin
      aControl := ToolBar.Controls[0];
      ToolBar.RemoveControl(aControl);
      aControl.Free;
    end;
  tmp := Filter;
  tmp := StringReplace(tmp,'@PERMISSIONJOIN@','&PERMISSIONJOIN&',[rfReplaceAll]);
  tmp := StringReplace(tmp,'@PERMISSIONWHERE@','&PERMISSIONWHERE&',[rfReplaceAll]);
  tmp := StringReplace(tmp,'@DEFAULTORDER@','&DEFAULTORDER&',[rfReplaceAll]);
  tmp := StringReplace(tmp,'@AUTOFILTER@','&AUTOFILTER&',[rfReplaceAll]);
  while pos('@',tmp) > 0 do
    begin
      tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
      data := copy(tmp,0,pos('@',tmp)-1);
      if data = '' then break;
      tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
      aname := copy(data,0,pos(':',data)-1);
      if aname = '' then
        aname := data
      else data := copy(data,pos(':',data)+1,length(data));
      if not Assigned(ToolBar.FindChildControl('TBC'+MD5Print(MD5String(aname)))) then
        begin
          aPanel := TPanel.Create(ToolBar);
          aPanel.Name:='TBC'+MD5Print(MD5String(aname));
          aPanel.Left:=1000;
          aPanel.Parent := ToolBar;
          aPanel.Caption:='';
          aPanel.BevelOuter:=bvNone;
          aLabel := TLabel.Create(aPanel);
          aLabel.Align:=alLeft;
          aLabel.BorderSpacing.Top:=8;
          aLabel.Caption:=aName;
          aLabel.Parent:=aPanel;
          aBevel := TBevel.Create(aPanel);
          aBevel.Align:=alLeft;
          aBevel.Shape := bsLeftLine;
          abevel.Parent := aPanel;
          aBevel.Width:=5;
          aBevel.Left:=0;
          apanel.Width:=aLabel.Left+aLabel.Width+150;
          if data = 'DATE' then
            begin
              aControl := TDateEdit.Create(aPanel);
              aControl.Parent:= aPanel;
              aControl.Align:=alClient;
              i := aControl.Width;
              aControl.Align:=alLeft;
              aControl.Width:=i-TDateEdit(aControl).Button.Width-14;
              aControl.Name := 'TBE'+MD5Print(MD5String(aname));
              aBevel.BorderSpacing.Left:=30;
              aLabel.Left:=0;
            end
          else if copy(data,0,8) = 'DROPDOWN' then
            begin
              aControl := TComboBox.Create(aPanel);
              aControl.Parent:= aPanel;
              aControl.Align:=alClient;
              aControl.Name := 'TBE'+MD5Print(MD5String(aname));
              TComboBox(aControl).Text:='';
            end
          else
            begin
              aControl := TEdit.Create(aPanel);
              aControl.Parent:= aPanel;
              aControl.Align:=alClient;
              aControl.Name := 'TBE'+MD5Print(MD5String(aname));
              TEdit(aControl).Text := '';
            end;
          aControl.BorderSpacing.Around:=6;
          aControl.Parent:= aPanel;
          TWinControl(aControl).OnKeyPress:=@TWinControlKeyPress;
        end;
      aControladded := True;
    end;
  if aControlAdded then
    begin
      bFilter.Parent := ToolBar;
      bFilter.Align:=alRight;
      bFilter.Visible:=True;
    end;
end;

procedure TfFilter.ClearFilter;
begin
  if cbFilter.Text<>strNoSelectFilter then
    begin
      cbFilter.Text:=strNoSelectFilter;
      cbFilterSelect(nil);
    end;
end;

procedure TfFilter.SetActive;
var
  aFilter: TStringList;
  i: Integer;
begin
  if Visible then
    begin
      fRowEditor.GetGridSizes('FILTER'+FFilterType,List,gList,FDefaultRows,not FEditable,cbFilter.Text);
      aFilter := TStringList.Create;
      for i := 1 to gHeader.Columns.Count do
        aFilter.Add(gHeader.Cells[i,1]);
      gHeader.Columns.Assign(gList.Columns);
      for i := 1 to gHeader.Columns.Count do
        begin
          if aFilter.Count >= i then
            gHeader.Cells[i,1] := aFilter[i-1]
          else
            gHeader.Cells[i,1] := '';
        end;
      FAutoFilter := BuildAutoFilter(gList,gHeader);
      aFilter.Free;
      UpdateTitle;
    end;
  try
    if gList.CanFocus and gList.IsControlVisible then
      gList.SetFocus;
  except
  end;
end;
procedure TfFilter.DoBeforeClose;
begin
  PRight.Visible:=False;
end;
type
  THackDBGrid = class(TDBGrid);
  THackGrid=Class( TCustomGrid)
  published
    property Row;
    property RowCount;
    property TopRow;
    property DefaultRowHeight;
  End;
function TfFilter.ShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo): Boolean;
var
  aLongDesc: String;
  gc: TGridCoord;
  Rec: TBookmark;
  aLink : string;
  aDataSet: TDataSet;
  aSRow: Integer;
begin
  Result := False;
  if (HintInfo.HintControl = gList) then
    begin
      HintInfo.HintWindowClass:=TSearchHintWindow;
      gc:= gList.MouseCoord(HintInfo.CursorPos.x,HintInfo.CursorPos.y);
      if gc.y = HintY then exit;
      HintY := gc.y;
      gList.BeginUpdate;
      gList.DataSource.Enabled:=False;
      aDataSet := gList.DataSource.DataSet;
      Rec := aDataSet.GetBookmark;
      aSRow := gc.Y - gList.MouseCoord(0,THackDBGrid(gList).SelectedFieldRect.Top+1).Y;// - THackDBGrid(gList).DataLink.ActiveRecord;
      if aSRow = 0 then
        begin
          with Application as IBaseDbInterface do
            begin
              aLink := Data.BuildLink(aDataSet);
              HintInfo.HintStr := Data.GetLinkDesc(aLink);
              aLongDesc := Data.GetLinkLongDesc(aLink);
            end;
          if aLongDesc <> '' then
            HintInfo.HintStr := HintInfo.HintStr +lineending+ aLongDesc;
          {$IFDEF MAINAPP}
          Data.GetLinkIcon(aLink);
          {$ENDIF}
          Result := True;
        end;
      gList.DataSource.Enabled:=True;
      gList.EndUpdate;
      HintInfo.HideTimeout:=15000;
    end;
end;
function TfFilter.AddToolbarAction(aAction: TAction) : TToolButton;
var
  Toolbutton: TToolButton;
begin
  pToolbar.Width:=pToolbar.Width+24;
  pFilterOpt.Left:=pFilterOpt.Left+24;
  Toolbutton :=TToolButton.Create(tbToolBar);
  Toolbutton.Parent  := tbToolBar;
  ToolButton.Action := aAction;
  Result := Toolbutton;
end;
procedure TfFilter.AddToolbarToggle(aAction: TAction);
var
  Toolbutton: TToolButton;
begin
  pToolbar.Width:=pToolbar.Width+24;
  pFilterOpt.Left:=pFilterOpt.Left+24;
  Toolbutton :=TToolButton.Create(tbToolBar);
  Toolbutton.Parent  := tbToolBar;
  Toolbutton.AllowAllUp:=True;
  ToolButton.Action:=aAction;
end;
procedure TfFilter.AddContextAction(aAction: TAction);
var
  Item : TMenuItem;
begin
  Item := TMenuItem.Create(pmPopup);
  Item.Action := aAction;
  pmPopup.Items.Add(Item);
end;
function TfFilter.AddFilter(FieldName, Value: string): Boolean;
var
  a: Integer;
  bList: TfFilter;
begin
  Result := false;
  for a := 0 to gList.Columns.Count-1 do
    if gList.Columns[a].FieldName = Fieldname then
      begin
        gHeader.Cells[a+1,1] := Value;
        Result := True;
        break;
      end;
  FAutoFilter := BuildAutofilter(gList,gHeader);
  acFilter.Execute;
end;
procedure TfFilter.ClearFilters;
var
  a: Integer;
begin
  gList.SelectedRows.Clear;
  for a := 0 to gList.Columns.Count-1 do
    begin
      gHeader.Columns[a].ButtonStyle:=cbsAuto;
      gHeader.Columns[a].PickList.Clear;
      gHeader.Cells[a+1,1] := '';
    end;
  FAutoFilter := BuildAutofilter(gList,gHeader);
  acFilter.Execute;
end;
procedure TfFilter.DoRefresh;
var
  aRec : LargeInt;
  aPrevRec : LargeInt;
begin
  if not DataSet.Active then exit;
  aRec := DataSet.GetBookmark;
  DataSet.DataSet.Prior;
  aPrevrec := DataSet.GetBookmark;
  DataSet.DataSet.next;
  if DataSet.DataSet.Active and (not DataSet.CanEdit) then
    DataSet.DataSet.Refresh;
  if DataSet.GetBookmark = aRec then exit;
  if not aRec = Null and DataSet.GotoBookmark(aRec) then
    DataSet.GotoBookmark(aPrevrec);
  DoUpdateDSCount;
end;
procedure TfFilter.SetRights;
begin
  if not Assigned(Data) then exit;
  acFilterRights.Enabled:=Data.Users.Rights.Right('EDITFILTER') >= RIGHT_PERMIT;
  acDeleteFilter.Enabled:=Data.Users.Rights.Right('EDITFILTER') >= RIGHT_DELETE;
  miAdmin.Visible:=Data.Users.Rights.Right('OPTIONS')>=RIGHT_DELETE;
  acDelete.Visible:=Data.Users.Rights.Right('OPTIONS')>=RIGHT_DELETE;
end;
function TfFilter.GetLink(onlyOne : Boolean = True): string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(FDataSet) then
    begin
      Result := '';
      if onlyOne or (gList.SelectedRows.Count=0) then
        begin
          Result := Result+Data.BuildLink(DataSet.DataSet)+';';
        end
      else
        begin
          for i := 0 to gList.SelectedRows.Count-1 do
            begin
              DataSet.DataSet.GotoBookmark(gList.SelectedRows[i]);
              Result := Result+Data.BuildLink(DataSet.DataSet)+';';
              if onlyOne then break;
            end;
          gList.SelectedRows.Clear;
        end;
    end;
end;
procedure TfFilter.ShowFrame;
var
  aRec: LargeInt;
begin
  inherited ShowFrame;
  if not Assigned(DataSet) then exit;
  DoRefresh;
  SetActive;
end;

procedure TfFilter.Open;
begin
  DoOpen;
end;

initialization

end.

