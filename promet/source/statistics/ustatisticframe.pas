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
unit ustatisticframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, TAGraph, LR_DBSet,
  LR_Class, LR_View, Forms, Controls, ComCtrls, Buttons, ActnList, Menus,
  ExtCtrls, DbCtrls, StdCtrls, uExtControls, DBZVDateTimePicker, db,
  uPrometFrames, uPrometFramesInplace, uBaseDBClasses, Dialogs, Spin, EditBtn,
  DBGrids, variants,uStatistic,SynCompletion, SynHighlighterPas,md5,LCLType,
  TASeries, TACustomSeries,fpsqlparser,Clipbrd,uBaseVisualApplication,
  uBaseDatasetInterfaces,QBuilder;
type

  { TfStatisticFrame }

  TfStatisticFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acClose: TAction;
    acDelete: TAction;
    acSave: TAction;
    acExport: TAction;
    acImport: TAction;
    acPrint: TAction;
    acRights: TAction;
    acRestart: TAction;
    acGotoParent: TAction;
    acExecute: TAction;
    acFormatSQL: TAction;
    acCopyToNewTable: TAction;
    acIsScript: TAction;
    acQueryBuilder: TAction;
    ActionList1: TActionList;
    bEditFilter: TSpeedButton;
    bEditFilter1: TSpeedButton;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bExecute: TSpeedButton;
    bExecute1: TBitBtn;
    bExecute2: TSpeedButton;
    BtZoomIn: TBitBtn;
    BtZoomOut: TBitBtn;
    cbStatus: TComboBox;
    Datasource: TDatasource;
    DBMemo1: TDBMemo;
    eName: TDBEdit;
    Detail: TDatasource;
    frPreview: TfrPreview;
    frReport: TfrReport;
    gDetail: TDBGrid;
    gList: TDBGrid;
    gSubDetail: TDBGrid;
    GutterImages: TImageList;
    HigPascal: TSynPasSyn;
    HigSQL: TSynSQLSyn;
    IdleSQLTimer: TIdleTimer;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbErrors: TListBox;
    lStatus: TLabel;
    miCopy: TMenuItem;
    MenuItem6: TMenuItem;
    Label3: TLabel;
    Label4: TLabel;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    N1: TMenuItem;
    N1001: TMenuItem;
    N101: TMenuItem;
    N1501: TMenuItem;
    N2: TMenuItem;
    N2001: TMenuItem;
    N251: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N501: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N751: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel9: TPanel;
    pcPages: TExtMenuPageControl;
    pDetails: TPanel;
    pSubDetails: TPanel;
    pTools: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PDetail: TfrDBDataSet;
    pNav1: TPanel;
    miDelete: TMenuItem;
    pmAction: TPopupMenu;
    pNav2: TPanel;
    pNav3: TPanel;
    pmGrid: TPopupMenu;
    ProcMenu: TPopupMenu;
    PStatisticresults: TfrDBDataSet;
    PSubDetail: TfrDBDataSet;
    pTop: TPanel;
    SaveDialog: TSaveDialog;
    sbMenue: TSpeedButton;
    smQuerry: TSynMemo;
    smQuerry1: TSynMemo;
    smQuerry2: TSynMemo;
    spDetails: TSplitter;
    Splitter2: TSplitter;
    spSubDetails: TSplitter;
    StatisticDS: TDatasource;
    StatisticResults: TDatasource;
    SubDetail: TDatasource;
    ExecuteTimer: TTimer;
    ToolBar: TToolBar;
    ToolButton3: TSpeedButton;
    ToolButton4: TSpeedButton;
    ToolButton5: TSpeedButton;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tsDescription: TTabSheet;
    tsDetail: TTabSheet;
    tsMaster: TTabSheet;
    tsReport: TTabSheet;
    tsResults: TTabSheet;
    tsSubDetail: TTabSheet;
    ZoomBtn: TBitBtn;
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acFormatSQLExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acQueryBuilderExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure aStmtException(e: Exception;aCol,aRow : Integer);
    procedure bEditFilterClick(Sender: TObject);
    procedure BtZoomInClick(Sender: TObject);
    procedure BtZoomOutClick(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure DataSetFieldsFieldsGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure DataSetRemove(Sender: TObject);
    procedure DatasourceDataChange(Sender: TObject; Field: TField);
    procedure ExecuteTimerTimer(Sender: TObject);
    procedure frReportGetValue(const ParName: String; var ParValue: Variant);
    procedure FSynCompletionExecute(Sender: TObject);
    procedure FSynCompletionSearchPosition(var aPosition: integer);
    procedure FSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
      );
    procedure gListTitleClick(Column: TColumn);
    procedure IdleSQLTimerTimer(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure N2001Click(Sender: TObject);
    procedure pmGridPopup(Sender: TObject);
    procedure ProjectsStateChange(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
    procedure smQuerryChange(Sender: TObject);
    procedure TComboBoxChange(Sender: TObject);
    procedure TDateEditChange(Sender: TObject);
    procedure TEditChange(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ZoomBtnClick(Sender: TObject);
  private
    { private declarations }
    FEditable : Boolean;
    csData: TChartSeries;
    FSynCompletion : TSynCompletion;
    FVariables : TStringList;
    FVariableNames : TStringList;
    FTables : TStringList;
    FTreeNode : TTreeNode;
    FBuilder: TOQBuilderDialog;
    FVisualQueryEngine :TOQBEngine;
    procedure ParseForms(Filter: string);
    function CheckSQL(DoFormat : Boolean = False) : Boolean;
    function BuildSQL(aIn : string) : string;
  protected
    procedure AddHistory(Sender: TObject);
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
    procedure DoOpen;override;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    function CanHandleLink(aLink : string): Boolean; override;
    procedure ListFrameAdded(aFrame: TObject); override;

    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure ShowFrame; override;
  end;
procedure AddToMainTree(aAction : TAction;Node : TTreeNode);
var
  MainNode : TTreeNode;

implementation
uses uData,uProjects,uHistoryFrame,uLinkFrame,uImageFrame,uDocuments,
  uIntfStrConsts,uMainTreeFrame,uBaseDBInterface,
  uFilterFrame,uBaseSearch,Utils,uBaseERPDBClasses,uSelectReport,
  uNRights,uSearch,LCLProc,utask,fpsqltree,fpsqlscanner,SynEditMarks,LCLIntf,
  fpspreadsheet, fpsallformats,uFormAnimate,SynBeautifier,Printers,fpsTypes,
  uError,qbepromet;
{$R *.lfm}
resourcestring
    strQueryTime                  = 'Abfragezeit: %s Anzahl Datensätze: %d';
    strNewDir                     = 'Neues Verzeichnis';
    strEnteranName                = 'geben Sie einen Namen an';
    strNewStatistic               = 'Neue Statistik';
    strName                       = 'Name';
    strUnnamed                    = 'Unbenannte Statistik';
    strStatisticChanged           = 'Die Statistik wurde geändert, sollen die Änderungen gespeichert werden ?';
    strNotEnougthRights           = 'Sie besitzen nicht genügend Rechte um diese Anwendung zu nutzen';
    strGenerating                 = 'Abfrage wird ausgeführt...';
    strErrorBuildingReport        = 'Es ist ein Fehler beim erstellen des Reports aufgetreten.';
function AddEntry(ID,Typ : string;aTyp : TEntryTyp;aList : TStatistic) : TTreeNode;
var
  Node1: TTreeNode;
  i: Integer;
  aRec: LargeInt;
begin
  aRec := aList.GetBookmark;
  if Assigned(MainNode) then
    if aRec > 0 then
      for i := 0 to MainNode.Count-1 do
        if TTreeEntry(MainNode.Items[i].Data).Rec = aRec then exit;
  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,'',TTreeEntry.Create);
  TTreeEntry(Node1.Data).Rec := aList.GetBookmark;
  with aList.DataSet as IBaseManageDB do
    TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(aList.Id.AsString);
  TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(aList.ClassType);
  TTreeEntry(Node1.Data).Text[0] := aList.Text.AsString;
  TTreeEntry(Node1.Data).Typ := aTyp;
  result := Node1;
end;
procedure AddToMainTree(aAction: TAction; Node: TTreeNode);
var
  aDataSet: TStatistic;
  Node1: TTreeNode;
  aList: TStatistic;
begin
  if (Data.Users.Rights.Right('STATISTICS') > RIGHT_NONE) then
    begin
      MainNode := Node;
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
      Data.SetFilter(Data.Tree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('S')+'))',0,'','ASC',False,True,True);
      Data.Tree.DataSet.First;
      while not Data.Tree.dataSet.EOF do
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
          TTreeEntry(Node1.Data).DataSource := Data.Tree;
          TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
          TTreeEntry(Node1.Data).Typ := etDir;
          fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
          Data.Tree.DataSet.Next;
        end;
      aList := TStatistic.Create(nil);
      aList.CreateTable;
      try
        Data.SetFilter(aList,Data.ProcessTerm(Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue('')),0,'','ASC',False,True,True);
        aList.DataSet.First;
        while not aList.DataSet.EOF do
          begin
            AddEntry('','S',etStatistic,aList);
            aList.DataSet.Next;
          end;
      finally
        aList.Destroy;
      end;
    end;
end;
procedure TfStatisticFrame.ProjectsStateChange(Sender: TObject);
var
  aEnabled: Boolean;
begin
  aEnabled := DataSet.CanEdit or DataSet.Changed;
  acSave.Enabled := aEnabled;
  acCancel.Enabled:= aEnabled;
end;
procedure TfStatisticFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfStatisticFrame.smQuerryChange(Sender: TObject);
var
  FStatementList: TSQLElementList;
begin
  if (DataSet.DataSet.State <> dsEdit) and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  if Sender = smQuerry then
    DataSource.DataSet.FieldByName('QUERRY').AsString := smQuerry.Lines.Text;
  if Sender = smQuerry1 then
    DataSource.DataSet.FieldByName('DETAIL').AsString := smQuerry1.Lines.Text;
  if Sender = smQuerry2 then
    DataSource.DataSet.FieldByName('SUBDETAIL').AsString := smQuerry2.Lines.Text;
  IdleSQLTimer.Enabled:=True;
  DataSet.Change;
end;

procedure TfStatisticFrame.TComboBoxChange(Sender: TObject);
begin
  FVariables.Values[TControl(Sender).Name] := TComboBox(Sender).Text;
end;

procedure TfStatisticFrame.TDateEditChange(Sender: TObject);
begin
  FVariables.Values[TControl(Sender).Name] := TDateEdit(Sender).Text;
end;

procedure TfStatisticFrame.TEditChange(Sender: TObject);
begin
  FVariables.Values[TControl(Sender).Name] := TEdit(Sender).Text;
end;

procedure TfStatisticFrame.ToolButton5Click(Sender: TObject);
begin
  FDataSet.Edit;
  if ToolButton5.Down then
    FDataSet.FieldByName('ISSCRIPT').AsString:='Y'
  else FDataSet.FieldByName('ISSCRIPT').AsString:='N';
  if ToolButton5.Down then
    smQuerry.Highlighter:=HigPascal
  else smQuerry.Highlighter:=HigSQL;
end;

procedure TfStatisticFrame.ZoomBtnClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := Point(ZoomBtn.ClientOrigin.X, ZoomBtn.ClientOrigin.Y + ZoomBtn.Height + 2);
  N4.Visible := False;
  N5.Visible := False;
  N6.Visible := False;
  N7.Visible := False;
  ProcMenu.Popup(pt.x + 4, pt.y + 6);
end;
procedure TfStatisticFrame.ParseForms(Filter: string);
var
  aControl : TControl;
  tmp: String;
  data: String;
  aname: String;
  aPanel: TPanel;
  aLabel: TLabel;
  i: LongInt;
  aBevel: TBevel;
  FirstControl : TControl = nil;
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
  while ToolBar.ControlCount > 0 do
    begin
      aControl := ToolBar.Controls[0];
      ToolBar.RemoveControl(aControl);
      aControl.Free;
    end;
  tmp := Filter;
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
      if Toolbar.FindComponent('TBC'+MD5Print(MD5String(aname))) = nil then
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
              aControl.Align:=alNone;
              aControl.Left:=aLabel.Left+aLabel.Width;
              aControl.Width := 120;
              aControl.Name := 'TBE'+MD5Print(MD5String(aname));

              TDateEdit(aControl).Text:='';
              TDateEdit(aControl).OnChange:=@TDateEditChange;
              TDateEdit(aControl).Text := fVariables.Values['TBE'+MD5Print(MD5String(aname))];
            end
          else if copy(data,0,8) = 'DROPDOWN' then
            begin
              aControl := TComboBox.Create(aPanel);
              aControl.Align:=alClient;
              aControl.Name := 'TBE'+MD5Print(MD5String(aname));

              TComboBox(aControl).Text:='';
              TComboBox(aControl).OnChange:=@TComboBoxChange;
              TComboBox(aControl).Text := fVariables.Values['TBE'+MD5Print(MD5String(aname))];
              if not Assigned(FirstControl) then FirstControl:=aControl;
            end
          else if copy(data,0,8) = 'TEXT' then
            begin
              aControl := TEdit.Create(aPanel);
              aControl.Align:=alClient;
              aControl.Name := 'TBE'+MD5Print(MD5String(aname));

              TEdit(aControl).Text := fVariables.Values['TBE'+MD5Print(MD5String(aname))];
              TEdit(aControl).OnChange:=@TEditChange;
              if TEdit(aControl).Text = '' then
                TEdit(aControl).Text := '*';
              if not Assigned(FirstControl) then FirstControl:=aControl;
            end
          else
            begin
              aPanel.Free;
              aControl := nil;
            end;
          if Assigned(aControl) then
            begin
              aControl.BorderSpacing.Around:=6;
              aControl.Parent:= aPanel;
              aLabel.Left := 0;
              aBevel.Left:=1000;
              pcPages.ActivePage:=tsResults;
            end;
        end;
    end;
  if Assigned(FirstControl) then
    begin
      try
        TWinControl(FirstControl).SetFocus;
      except
      end;
    end;
end;
function TfStatisticFrame.CheckSQL(DoFormat: Boolean): Boolean;
var
  aStmt: TSQLStatemnt;
  m: TSynEditMark;
begin
  Result := True;
  aStmt := TSQLStatemnt.Create;
  aStmt.SQL:=BuildSQL(DataSet.FieldByName('QUERRY').AsString);
  aStmt.OnException:=@aStmtException;
  lbErrors.Clear;
  while smQuerry.Marks.Count > 0 do
    smQuerry.Marks.Delete(0);
  if not aStmt.Parse then
    lbErrors.Visible:=True
  else if DoFormat and (aStmt.FormatedSQL<>'') then
    smQuerry.Text:=aStmt.FormatedSQL;
  aStmt.Free;
end;

function MSecToTimeStr(mSec : Int64) : string;
var
  dt: TDateTime;
begin
  dt := mSec / MSecsPerDay;
  if dt > 1 then
    Result := Format('%d Tage, %s',[Trunc(dt),FormatDateTime('hh:mm:ss.z',frac(dt))])
  else
    Result := FormatDateTime('hh:mm:ss.zzz',dt);
end;
function TfStatisticFrame.BuildSQL(aIn: string): string;

function CheckWildgards(Str : string) : string;
begin
  Result := Str;
  Result := Stringreplace(Result,'*','%',[rfreplaceAll]);
  result := Stringreplace(Result,'?','_',[rfreplaceAll]);
end;
var
  sl: TStringList;
  aFilter,bFilter: String;
  i: Integer;
  tmp: String;
  adata: String;
  aname: String;
  aControl: TControl;
  cFilter: String;
begin
  Result := '';
  sl := TStringList.Create;
  aFilter := aIn;
  cFilter := aFilter;
  sl.Text:=Data.Preprocess(aFilter);
  //with Application as IBaseDBInterface do
    begin
      if Data.Users.Rights.Right('STATISTICS') <= RIGHT_DELETE then
        begin
          if pos('DELETE',Uppercase(afilter)) > 0 then exit;
          if pos('INSERT',Uppercase(afilter)) > 0 then exit;
          if pos('UPDATE',Uppercase(afilter)) > 0 then exit;
          if pos('DROP',Uppercase(afilter)) > 0 then exit;
        end;
      i := 0;
      while i < sl.Count do
        begin
          if copy(trim(sl[i]),0,2) = '--' then
            sl.Delete(i)
          else
            inc(i);
        end;
      aFilter := sl.Text;
      sl.Free;
      tmp := copy(aFilter,pos('WHERE',UpperCase(aFilter)),length(aFilter));
      aFilter := copy(aFilter,0,pos('WHERE',UpperCase(aFilter))-2);
      while pos('@',tmp) > 0 do
        begin
          tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
          adata := copy(tmp,0,pos('@',tmp)-1);
          if adata = '' then
            begin
              break;
            end;
          tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
          aname := copy(adata,0,pos(':',adata)-1);
          if aname = '' then
            aname := adata;
          if (ToolBar.FindChildControl('TBC'+MD5Print(MD5String(aname))) <> nil) then
            begin
              aControl := ToolBar.FindChildControl('TBC'+MD5Print(MD5String(aname)));
              aControl := TWinControl(aControl).FindChildControl('TBE'+MD5Print(MD5String(aname)));
              if aControl is TEdit then
                bFilter := CheckWildgards(TEdit(aControl).Text)
              else if aControl is TDateEdit then
                bFilter := Data.DateToFilter(TDateEdit(aControl).Date)
              else if aControl is TComboBox then
                bFilter := CheckWildgards(TComboBox(aControl).Text);
              cFilter := StringReplace(cFilter,'@'+adata+'@',bFilter,[]);
              FVariableNames.Values[aName]:='TBE'+MD5Print(MD5String(aname));
            end;
        end;
      cFilter := StringReplace(cFilter,'@USERID@',Data.Users.Id.AsString,[rfReplaceAll]);
    end;
  Result := ReplaceSQLFunctions(cFilter);
end;

procedure TfStatisticFrame.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='STATISTIC';
  TfHistoryFrame(Sender).DataSet := TStatistic(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfStatisticFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;

procedure TfStatisticFrame.acCancelExecute(Sender: TObject);
begin
  Abort;
end;

procedure TfStatisticFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      DataSet.DataSet.Delete;
      FDataSet.CascadicCancel;
//      Data.Commit(FConnection);
//      Data.StartTransaction(FConnection);
      acClose.Execute;
      Screen.Cursor := crDefault;
      try
        if Assigned(FTreeNode) then
          fMainTreeFrame.tvMain.Items.Delete(FTreeNode);
      except
      end;
    end;
end;

procedure TfStatisticFrame.acExecuteExecute(Sender: TObject);
var
  aTime: LongWord;
  i: Integer;
begin
  pDetails.Visible:=False;
  spDetails.Visible:=False;
  pSubDetails.Visible:=False;
  spSubDetails.Visible:=False;
  acExecute.Enabled:=False;
  aTime := GetTickCount;
  frPreview.Caption:=strGenerating;
  pcPages.ActivePage:=tsResults;
  Application.ProcessMessages;
  try
    try
      if Assigned(StatisticResults.DataSet) then
        StatisticResults.DataSet.Free;
      StatisticResults.DataSet := Data.GetNewDataSet(BuildSQL(DataSet.FieldByName('QUERRY').AsString),FConnection);
      StatisticResults.DataSet.Open;
      for i := 0 to StatisticResults.DataSet.Fields.Count-1 do
        begin
          if (StatisticResults.DataSet.Fields[i].DataType=ftString)
          or (StatisticResults.DataSet.Fields[i].DataType=ftMemo)
          then
            StatisticResults.DataSet.Fields[i].OnGetText:=@DataSetFieldsFieldsGetText
          else if (StatisticResults.DataSet.Fields[i].DataType=ftFloat) then
            TFloatField(StatisticResults.DataSet.Fields[i]).DisplayFormat:='########.##';
        end;
      if trim(smQuerry1.Lines.Text) <> '' then
        begin
          if Assigned(Detail.DataSet) then
            Detail.DataSet.Free;
          Detail.DataSet := Data.GetNewDataSet(BuildSQL(smQuerry1.Lines.Text),FConnection,StatisticResults.DataSet);
          Detail.DataSet.Open;
          pDetails.Visible:=True;
          spDetails.Visible:=True;
          if trim(smQuerry2.Lines.Text) <> '' then
            begin
              if Assigned(SubDetail.DataSet) then
                SubDetail.DataSet.Free;
              SubDetail.DataSet := Data.GetNewDataSet(BuildSQL(smQuerry2.Lines.Text),FConnection,Detail.DataSet);
              SubDetail.DataSet.Open;
              pSubDetails.Visible:=True;
              spSubDetails.Visible:=True;
            end;
        end;
      aTime := GetTickCount-aTime;
      lStatus.Caption:=Format(strQueryTime,[MSecToTimeStr(aTime),StatisticResults.DataSet.RecordCount]);
      lStatus.Visible:=True;
      fSelectReport.ReportType:= copy(DataSet.Id.AsString,length(DataSet.Id.AsString)-3,4);
      pcPages.ActivePage := tsResults;
      if (Data.Reports.Count > 0) and (DataSet.State<>dsInsert) then
        begin
          Printer.PrinterIndex:=-1;//Default Printer
          fSelectReport.Report := frReport;
          if fSelectReport.LoadReport then
            begin
              frReport.Preview := frPreview;
              tsReport.TabVisible:=True;
              frPreview.Clear;
              frPreview.Caption:='';
              pcPages.ActivePage := tsReport;
            end;
        end
      else
        pcPages.ActivePage:=tsResults;
      Application.ProcessMessages;
      if tsReport.TabVisible then
        begin
          try
            for i := 0 to FVariableNames.Count-1 do
              if (frReport.Variables.IndexOf(FVariableNames.Names[i])=-1) then
                frReport.Variables.Add(' '+FVariableNames.Names[i]);
            if frReport.PrepareReport then
              frReport.ShowPreparedReport;
            if frReport.EMFPages.Count > 1 then
              frPreview.TwoPages
            else frPreview.Zoom:=100;
          except
            begin
              Showmessage(strErrorBuildingReport);
              tsReport.Visible:=False;
            end;
          end;
        end;
      if (pcPages.ActivePage = tsResults) and Visible then
        begin
          gList.SetFocus;
        end;
      acExecute.Enabled:=True;
      bExecute1.Action := acExecute;
      exit;
    except
      on E : Exception do
        begin
          tsReport.Visible:=False;
          tsDescription.Visible:=True;
          pcPages.ActivePage:=tsDescription;
          acExecute.Enabled:=True;
          bExecute1.Action := acExecute;

          fError.ShowError(e.Message+lineending+BuildSQL(DataSet.FieldByName('QUERRY').AsString));
        end;
    end;
  finally
  end;
  acExecute.Enabled:=True;
  bExecute1.Action := acExecute;
end;

procedure TfStatisticFrame.acExportExecute(Sender: TObject);
var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  x: Integer;
  y: Integer;
  f: TextFile;
begin
  if SaveDialog.Execute then
    begin
      if SaveDialog.FilterIndex < 8 then
        begin
          // Create the spreadsheet
          MyWorkbook := TsWorkbook.Create;
          MyWorksheet := MyWorkbook.AddWorksheet('Prometheus Export');
          for x := 0 to StatisticResults.DataSet.FieldCount-1 do
            MyWorkSheet.WriteUTF8Text(0,x,StatisticResults.DataSet.FieldDefs[x].DisplayName);
          StatisticResults.DataSet.First;
          y := 1;
          StatisticResults.DataSet.DisableControls;
          while not StatisticResults.DataSet.EOF do
            begin
              for x := 0 to StatisticResults.DataSet.FieldCount-1 do
                case StatisticResults.DataSet.FieldDefs[x].DataType of
                ftSmallint, ftInteger, ftFloat, ftAutoInc:MyWorkSheet.WriteNumber(y,x,StatisticResults.DataSet.Fields[x].AsFloat);
                else
                  MyWorkSheet.WriteUTF8Text(y,x,StatisticResults.DataSet.Fields[x].AsString);
                end;
              StatisticResults.DataSet.Next;
              inc(y);
            end;
            StatisticResults.DataSet.EnableControls;
        end;
      // Save the spreadsheet to a file
      if ExtractFileExt(SaveDialog.FileName) = '' then
        begin
          case SaveDialog.FilterIndex of
          1,2,3,4,5:SaveDialog.FileName := SaveDialog.FileName+STR_EXCEL_EXTENSION;
          6:SaveDialog.FileName := SaveDialog.FileName+STR_OOXML_EXCEL_EXTENSION;
          7:SaveDialog.FileName := SaveDialog.FileName+STR_OPENDOCUMENT_CALC_EXTENSION;
          8:SaveDialog.FileName := SaveDialog.FileName+'.csv';
          end;
        end;
      if FileExists(SaveDialog.FileName) then
        DeleteFile(SaveDialog.FileName);
      if SaveDialog.FilterIndex < 8 then
        MyWorkbook.WriteToFile(SaveDialog.FileName,TsSpreadsheetFormat(SaveDialog.FilterIndex-1))
      else if SaveDialog.FilterIndex = 8 then
        begin
          StatisticResults.DataSet.DisableControls;
          AssignFile(f,UniToSys(SaveDialog.FileName));
          Rewrite(f);
          for x := 0 to StatisticResults.DataSet.FieldCount-1 do
            write(f,UniToSys(StatisticResults.DataSet.FieldDefs[x].DisplayName)+';');
          writeln(f);
          StatisticResults.DataSet.First;
          while not StatisticResults.DataSet.EOF do
            begin
              for x := 0 to StatisticResults.DataSet.FieldCount-1 do
                write(f,UniToSys(StatisticResults.DataSet.Fields[x].AsString)+';');
              writeln(f);
              StatisticResults.DataSet.Next;
            end;
          CloseFile(f);
          StatisticResults.DataSet.EnableControls;
        end;
      MyWorkbook.Free;
    end;
end;

procedure TfStatisticFrame.acFormatSQLExecute(Sender: TObject);
begin
  CheckSQL(True);
end;

procedure TfStatisticFrame.acPrintExecute(Sender: TObject);
var
  ID: String;
begin
  with Application as IBaseDBInterface do
    begin
      ID := DataSet.Id.AsString;
      if (not Assigned(StatisticResults.DataSet)) then
        acExecute.Execute;
      fSelectReport.Report := frReport;
      fSelectReport.ReportType:= copy(ID,length(ID)-3,4);
      fSelectReport.SetLanguage;
      fSelectReport.Execute;
    end;
end;

procedure TfStatisticFrame.acQueryBuilderExecute(Sender: TObject);
var
  aSL: TStringList;
  aStrm: TStream;
begin
  if not Assigned(FVisualQueryEngine) then
    begin
      FVisualQueryEngine := TOQBEnginePromet.Create(nil);
      FBuilder := TOQBuilderDialog.Create(nil);
      FBuilder.ShowButtons:=[bRunQuery];
      FVisualQueryEngine.DatabaseName:='Promet';
      FBuilder.OQBEngine := FVisualQueryEngine;
      FVisualQueryEngine.ShowSystemTables:=False;
      FVisualQueryEngine.NameQuote:=Data.QuoteField('')[1];
      FVisualQueryEngine.ValueQuote:=Data.QuoteValue('')[1];
    end;
  aSL := TStringList.Create;
  aStrm := Data.BlobFieldStream(DataSet.DataSet,'QUERRYBLD');
  aSL.LoadFromStream(aStrm);
  aStrm.Free;
  if FBuilder.Execute(aSL) then
    begin
      FDataSet.Edit;
      aSL.Clear;
      FBuilder.SaveToStringList(aSL);
      aStrm := TStringStream.Create('');
      aSL.SaveToStream(aStrm);
      aStrm.Position:=0;
      Data.StreamToBlobField(aStrm,DataSet.DataSet,'QUERRYBLD');
      aStrm.Free;
      if (FDataSet.FieldByName('QUERRY').AsString='') or (copy(FDataSet.FieldByName('QUERRY').AsString,0,40)='--Automatically Created by QuerryBuilder') then
        begin
          FDataSet.FieldByName('QUERRY').AsString := '--Automatically Created by QuerryBuilder'+#10#13+FBuilder.GenerateSQL;
          smQuerry.Text:=FDataSet.FieldByName('QUERRY').AsString;
        end;
    end;
  aSL.Free;
end;

procedure TfStatisticFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfStatisticFrame.acSaveExecute(Sender: TObject);
begin
  Save;
  if Assigned(FTreeNode) then
    begin
      TTreeEntry(FTreeNode.Data).Rec:=FDataSet.Id.AsVariant;
      TTreeEntry(FTreeNode.Data).DataSourceType:=TStatistic;
      with FDataSet.DataSet as IBaseManageDB do
        TTreeEntry(FTreeNode.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(FDataSet.Id.AsString);;
    end;
end;
procedure TfStatisticFrame.acSetTreeDirExecute(Sender: TObject);
begin
  if fMainTreeFrame.GetTreeEntry = -1 then exit;
  with DataSet.DataSet do
    begin
      Edit;
      FieldbyName('TREEENTRY').AsInteger:=fMainTreeFrame.GetTreeEntry;
      fMainTreeFrame.tvMain.Selected.Collapse(true);
    end;
end;

procedure TfStatisticFrame.aStmtException(e: Exception;aCol,aRow : Integer);
var
  m: TSynEditMark;
begin
  m := TSynEditMark.Create(smQuerry);
  m.Line := aRow;
  m.ImageList := GutterImages;
  m.ImageIndex := 0;
  m.Column:=aCol;
  m.Visible := true;
  smQuerry.Marks.Add(m);
  lbErrors.Items.Add('['+IntToStr(aRow)+':'+IntToStr(aCol)+'] '+e.Message);
end;

procedure TfStatisticFrame.bEditFilterClick(Sender: TObject);
var
  Animate: TAnimationController;
  nH: LongInt;
begin
  bEditFilter.Enabled:=False;
  Animate := TAnimationController.Create(Panel5);
  smQuerry.BeginUpdate;
  smQuerry1.BeginUpdate;
  smQuerry2.BeginUpdate;
  if bEditFilter.Down then
    begin
      pTop.Visible:=True;
      with Application as IBaseDbInterface do
        begin
          nH := DBConfig.ReadInteger('STATISTICEDITHEIGHT',244);
          if nH<100 then nH := 244;
          Animate.AnimateControlHeight(nH);
        end;
    end
  else
    begin
      with Application as IBaseDbInterface do
        DBConfig.WriteInteger('STATISTICEDITHEIGHT',Panel5.Height);
      Animate.AnimateControlHeight(45);
      if pos('WHERE',Uppercase(smQuerry.Lines.Text)) > 0 then
        ParseForms(copy(smQuerry.Lines.Text,pos('WHERE',Uppercase(smQuerry.Lines.Text)),length(smQuerry.Lines.Text)))
      else
        ParseForms(smQuerry.Lines.Text);
      pTop.Visible:=False;
    end;
  smQuerry2.EndUpdate;
  smQuerry1.EndUpdate;
  smQuerry.EndUpdate;
  bEditFilter.Enabled:=True;
  Splitter2.Visible:=bEditFilter.Down;
  pTools.Visible:=bEditFilter.Down;
end;

procedure TfStatisticFrame.BtZoomInClick(Sender: TObject);
begin
  frPreview.Zoom:=frPreview.Zoom*1.1;
end;

procedure TfStatisticFrame.BtZoomOutClick(Sender: TObject);
begin
  frPreview.Zoom:=frPreview.Zoom*0.9;
end;

procedure TfStatisticFrame.cbStatusSelect(Sender: TObject);
var
  tmp: String;
begin
  tmp := copy(cbStatus.text,pos('(',cbStatus.text)+1,length(cbStatus.text));
  tmp := copy(tmp,0,pos(')',tmp)-1);
  if not FDataSet.CanEdit then FDataSet.DataSet.Edit;
  FDataSet.FieldByName('STATUS').AsString:=tmp;
  acSave.Execute;
  DoOpen;
end;

procedure TfStatisticFrame.DataSetFieldsFieldsGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  aText := copy(Sender.AsString,0,100)
end;

procedure TfStatisticFrame.DataSetRemove(Sender: TObject);
begin
  if Assigned(FTreeNode) then FTreeNode.Free;
end;

procedure TfStatisticFrame.DatasourceDataChange(Sender: TObject; Field: TField);
begin
  ProjectsStateChange(Sender);
end;

procedure TfStatisticFrame.ExecuteTimerTimer(Sender: TObject);
begin
  ExecuteTimer.Enabled:=False;
  acExecute.Execute;
end;
procedure TfStatisticFrame.frReportGetValue(const ParName: String;
  var ParValue: Variant);
begin
  if FVariables.Values[FVariableNames.Values[ParName]]<>'' then
    ParValue:=FVariables.Values[FVariableNames.Values[ParName]];
end;

procedure TfStatisticFrame.FSynCompletionExecute(Sender: TObject);
function GetCurWord:string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with TSynCompletion(Sender).Editor do
    begin
      S:=Trim(Copy(LineText, 1, CaretX));
      I:=Length(S);
      while (i>0) and (S[i]<>'.') do Dec(I);
      if (I>0) then
      begin
        J:=i-1;
        //Get table name
        while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
        Result:=trim(Copy(S, j+1, i-j-1));
      end;
    end;
end;
var
  i: Integer;
  aStatement: String;
  s: String;
  aStrings: TStrings;
  ps : PChar;
begin
  with FSynCompletion.ItemList do
    begin
      Clear;
      s := GetCurWord;
      if s <> '' then
        begin
          aStrings := Data.GetColumns(s);
          ps := PChar(s);
          if aStrings.Count = 0 then
            aStrings := Data.GetColumns(AnsiExtractQuotedStr(ps,'"'));
          Assign(aStrings);
          if aStrings.Count = 0 then s := '';
          aStrings.Free;
        end;
      if FTables.Count > 0 then
        begin
          for i := 0 to FTables.Count-1 do
            if trim(FTables[i]) <> '' then
              begin
                aStrings := Data.GetColumns(FTables[i]);
                ps := PChar(FTables[i]);
                if aStrings.Count = 0 then
                  aStrings := Data.GetColumns(AnsiExtractQuotedStr(ps,'"'));
                FSynCompletion.ItemList.AddStrings(aStrings);
                aStrings.Free;
                s := 'found';
              end;
        end;
      if s = '' then
        begin
          for i := 0 to Data.Tables.Count-1 do
            Add(Data.Tables[i]);
          Add('select');
          Add('insert');
          Add('update');
          Add('delete');
          Add('from');
          Add('where');
          Add('into');
          Add('order by');
          Add('group by');
        end;
    end;
end;

procedure TfStatisticFrame.FSynCompletionSearchPosition(var aPosition: integer);
var
  i: Integer;
begin
  for i := 0 to FSynCompletion.ItemList.Count-1 do
    if Uppercase(copy(FSynCompletion.ItemList[i],0,length(FSynCompletion.CurrentString))) = Uppercase(FSynCompletion.CurrentString) then
      begin
        aPosition := i;
        FSynCompletion.TheForm.Position:=i-1;
        FSynCompletion.TheForm.Position:=i;
        exit;
      end;
end;

procedure TfStatisticFrame.FSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if (length(UTF8Key)=1) and (System.Pos(UTF8Key[1],FSynCompletion.EndOfTokenChr)>0) then
    begin
      FSynCompletion.TheForm.OnValidate(Sender,UTF8Key,[]);
      UTF8Key:='';
    end
end;

procedure TfStatisticFrame.gListTitleClick(Column: TColumn);
var
  i: Integer;
begin
  if not StatisticResults.DataSet.Active then exit;
  with StatisticResults.DataSet as IBaseDbFilter do
    begin
      if LocalSortFields = Column.FieldName then
        begin
          if SortDirection = sdAscending then
            begin
              SortDirection := sdDescending;
              SortLocal := True;
            end
          else if SortDirection = sdDescending then
            begin
              SortDirection := sdIgnored;
              SortLocal := True;
            end
          else
            begin
              LocalSortFields := Column.FieldName;
              SortLocal := True;
              SortDirection := sdAscending;
            end;
        end
      else
        begin
          LocalSortFields := Column.FieldName;
          SortLocal := True;
          SortDirection := sdAscending;
        end;
      for i := 0 to gList.Columns.Count-1 do
        begin
          if SortFields = TColumn(gList.Columns[i]).FieldName then
            begin
              if SortDirection = sdAscending then
                begin
                  gList.Columns[i].Title.ImageIndex := 0;
                end
              else if SortDirection = sdDescending then
                begin
                  gList.Columns[i].Title.ImageIndex := 1;
                end
              else
                begin
                  gList.Columns[i].Title.ImageIndex := -1;
                end;
            end
          else
            begin
              gList.Columns[i].Title.ImageIndex := -1;
            end;
        end;
    end;
end;

procedure TfStatisticFrame.IdleSQLTimerTimer(Sender: TObject);
begin
  IdleSQLTimer.Enabled:=False;
  CheckSQL;
end;

procedure TfStatisticFrame.MenuItem4Click(Sender: TObject);
begin

end;

procedure TfStatisticFrame.miCopyClick(Sender: TObject);
begin
  Clipboard.AsText:=gList.SelectedColumn.Field.AsString;
end;

procedure TfStatisticFrame.N2001Click(Sender: TObject);
begin
  if TComponent(Sender).Tag = 1 then
    frPreview.OnePage
  else if TComponent(Sender).Tag = 2 then
    frPreview.PageWidth
  else if TComponent(Sender).Tag = 3 then
    frPreview.TwoPages
  else frPreview.Zoom:=TComponent(Sender).Tag;
end;

procedure TfStatisticFrame.pmGridPopup(Sender: TObject);
begin
  miCopy.Enabled:=Assigned(gList.SelectedColumn);
end;

procedure TfStatisticFrame.DoOpen;
var
  aType: Char;
  aFound: Boolean;
  tmp: String;
begin
  tsReport.TabVisible:=False;
  SetRights;
  cbStatus.Items.Clear;
  cbStatus.Text := '';
  aType := 'T';
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
  Datasource.DataSet := DataSet.DataSet;
  DataSet.OnRemove:=@DataSetRemove;
  smQuerry.Lines.Text:=DataSet.FieldByName('QUERRY').AsString;
  smQuerry1.Lines.Text:=DataSet.FieldByName('DETAIL').AsString;
  smQuerry2.Lines.Text:=DataSet.FieldByName('SUBDETAIL').AsString;
  if pos('WHERE',Uppercase(smQuerry.Lines.Text)) > 0 then
    ParseForms(copy(smQuerry.Lines.Text,pos('WHERE',Uppercase(smQuerry.Lines.Text)),length(smQuerry.Lines.Text)))
  else
    ParseForms(smQuerry.Lines.Text);
  if Assigned(StatisticResults.DataSet) then
    begin
      StatisticResults.DataSet.Close;
      lStatus.Visible:=False;
    end;
  if Assigned(DataSet.FieldByName('ISSCRIPT')) then
    begin
      ToolButton5.Down:=DataSet.FieldByName('ISSCRIPT').AsString='Y';
      if ToolButton5.Down then
        smQuerry.Highlighter:=HigPascal
      else smQuerry.Highlighter:=HigSQL;
    end;
  pcPages.NewFrame(TfHistoryFrame,False,strHistory,@AddHistory);
  inherited DoOpen;
end;
function TfStatisticFrame.SetRights: Boolean;
begin
  FEditable := ((Data.Users.Rights.Right('STATISTICS') > RIGHT_READ));
  Result := FEditable;
  acDelete.Enabled:=FEditable and (Data.Users.Rights.Right('STATISTICS') > RIGHT_WRITE);
  acRights.Enabled:=Data.Users.Rights.Right('STATISTICS') >= RIGHT_PERMIT;
  bEditFilter.Visible:=FEditable;
  eName.Enabled := FEditable;
  DBMemo1.ReadOnly:=not FEditable;
end;

constructor TfStatisticFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTreeNode := nil;
  FSynCompletion := TSynCompletion.Create(Self);
  FSynCompletion.CaseSensitive := False;
  FSynCompletion.AddEditor(smQuerry);
  FSynCompletion.OnExecute:=@FSynCompletionExecute;
  FSynCompletion.OnUTF8KeyPress:=@FSynCompletionUTF8KeyPress;
  FSynCompletion.OnSearchPosition:=@FSynCompletionSearchPosition;
  FVariables := TStringList.Create;
  FVariableNames := TStringList.Create;
  FTables := TStringList.Create;
  pTop.Height := 0;
  Panel5.Height:=45;
end;

procedure TfStatisticFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  acSave.Enabled:=False;
  acCancel.Enabled:=False;
  if not Assigned(AValue) then exit;
end;
destructor TfStatisticFrame.Destroy;
begin
  Detail.DataSet.Free;
  Detail.DataSet:=nil;
  SubDetail.DataSet.Free;
  SubDetail.DataSet:=nil;
  if Assigned(DataSet) then
    begin
      DataSet.Destroy;
      DataSet := nil;
    end;
  FTables.Free;
  FVariables.Free;
  FVariableNames.Free;
  FDataSet:=nil;
  FSynCompletion.Free;
  inherited;
end;

function TfStatisticFrame.CanHandleLink(aLink: string): Boolean;
begin
  Result := ((copy(aLink,0,10) = 'STATISTICS'));
end;

procedure TfStatisticFrame.ListFrameAdded(aFrame: TObject);
begin
end;

function TfStatisticFrame.OpenFromLink(aLink: string) : Boolean;
var
  aParams: String = '';
  aName: String;
  aValue: String;
  DoExec: Boolean = False;
begin
  inherited;
  Result := False;
  if not CanHandleLink(aLink) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    begin
      aParams := copy(aLink,rpos('(',aLink),length(aLink));
      aLink := copy(aLink,0,rpos('(',aLink)-1);
    end;
  DataSet := TStatistic.CreateEx(Self,Data,FConnection);
  Data.SetFilter(FDataSet,Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1);
  if FDataSet.Count > 0 then
    begin
      TabCaption := TStatistic(FDataSet).Text.AsString;
      aParams := copy(aParams,2,length(aParams)-2);
      if trim(aParams)<>'' then
        begin
          FVariables.Clear;
          aParams := aParams+',';
          while pos(',',aParams)>0 do
            begin
              aName := copy(aParams,0,pos('=',aParams)-1);
              aValue := copy(aParams,pos('=',aParams)+1,length(aParams));
              aValue := copy(aValue,0,pos(',',aValue)-1);
              aParams := copy(aParams,pos(',',aParams)+1,length(aParams));
              if aName <> '' then
                FVariables.Values['TBE'+MD5Print(MD5String(aName))] := aValue;
            end;
          DoExec := True;
        end;
      DoOpen;
      Result := True;
      if DoExec then
        begin
          Application.ProcessMessages;
          ExecuteTimer.Enabled := True;
        end;
    end;
end;
procedure TfStatisticFrame.New;
begin
  inherited;
  SetRights;
  if not FEditable then exit;
  DataSet := TStatistic.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@ProjectsStateChange;
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DataSet.FieldByName('NAME').AsString:=strNewStatistic;
  TabCaption := strNewStatistic;
  DoOpen;
  acSave.Enabled := False;
  acCancel.Enabled:= False;
  FTreeNode := AddEntry('','S',etStatistic,TStatistic(DataSet));
  DataSet.Change;
  {bEditFilter.Down:=True;
  bEditFilterClick(nil);
  smQuerry.SetFocus;}
  ProjectsStateChange(Self);
  tsReport.TabVisible:=False;
end;
procedure TfStatisticFrame.SetLanguage;
begin
end;

procedure TfStatisticFrame.ShowFrame;
begin
  inherited ShowFrame;
  smQuerry.Beautifier := TSynBeautifier.Create(Application);
  smQuerry1.Beautifier := TSynBeautifier.Create(Application);
  smQuerry2.Beautifier := TSynBeautifier.Create(Application);
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfStatisticFrame);
end.

