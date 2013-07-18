{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
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
  DBGrids, variants,uStatistic,SynCompletion,md5,LCLType,
  TASeries, TACustomSeries,fpsqlparser,Clipbrd;
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
    ActionList1: TActionList;
    bEditFilter: TSpeedButton;
    Bevel3: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bExecute: TSpeedButton;
    bExecute1: TBitBtn;
    bExecute2: TSpeedButton;
    BtZoomIn: TBitBtn;
    BtZoomOut: TBitBtn;
    Datasource: TDatasource;
    Detail: TDatasource;
    frPreview: TfrPreview;
    frReport: TfrReport;
    gDetail: TDBGrid;
    gList: TDBGrid;
    gSubDetail: TDBGrid;
    GutterImages: TImageList;
    IdleSQLTimer: TIdleTimer;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
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
    Panel4: TPanel;
    Panel6: TPanel;
    pcTabs: TPageControl;
    PDetail: TfrDBDataSet;
    pDetails: TPanel;
    pNav1: TPanel;
    miDelete: TMenuItem;
    pmAction: TPopupMenu;
    pNav2: TPanel;
    pNav3: TPanel;
    pmGrid: TPopupMenu;
    ProcMenu: TPopupMenu;
    PStatisticresults: TfrDBDataSet;
    PSubDetail: TfrDBDataSet;
    pSubDetails: TPanel;
    pTop: TPanel;
    SaveDialog: TSaveDialog;
    sbMenue: TSpeedButton;
    smQuerry: TSynMemo;
    smQuerry1: TSynMemo;
    smQuerry2: TSynMemo;
    spDetails: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    spSubDetails: TSplitter;
    StatisticDS: TDatasource;
    StatisticResults: TDatasource;
    SubDetail: TDatasource;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar: TToolBar;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
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
    procedure acPrintExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure bEditFilterClick(Sender: TObject);
    procedure BtZoomInClick(Sender: TObject);
    procedure BtZoomOutClick(Sender: TObject);
    procedure DataSetRemove(Sender: TObject);
    procedure DatasourceStateChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
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
    procedure ZoomBtnClick(Sender: TObject);
  private
    { private declarations }
    FEditable : Boolean;
    csData: TChartSeries;
    FSynCompletion : TSynCompletion;
    FVariables : TStringList;
    FTables : TStringList;
    FTreeNode : TTreeNode;
    procedure ParseForms(Filter: string);
    procedure CheckSQL;
    function BuildSQL(aIn : string) : string;
  protected
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
    procedure DoOpen;override;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure ShowFrame; override;
  end;
  TOwnSQLParser = class(TSQLParser)
  end;
procedure AddToMainTree(aAction : TAction);
var
  MainNode : TTreeNode;

implementation
uses uData,uProjects,uHistoryFrame,uLinkFrame,uImageFrame,uDocuments,
  uIntfStrConsts,uMainTreeFrame,uBaseDBInterface,
  uFilterFrame,uBaseSearch,Utils,uBaseERPDBClasses,uSelectReport,
  uNRights,uSearch,LCLProc,utask,fpsqltree,fpsqlscanner,SynEditMarks,LCLIntf,
  fpspreadsheet, fpsallformats,uFormAnimate,SynBeautifier,Printers;
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
function AddEntry(ID,Typ : string;aTyp : TEntryTyp;aList : TStatistic) : TTreeNode;
var
  Node1: TTreeNode;
begin
  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,'',TTreeEntry.Create);
  TTreeEntry(Node1.Data).Rec := aList.GetBookmark;
  with aList.DataSet as IBaseManageDB do
    TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(aList.GetBookmark));
  TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(aList.ClassType);
  TTreeEntry(Node1.Data).Text[0] := aList.Text.AsString;
  TTreeEntry(Node1.Data).Typ := aTyp;
  result := Node1;
end;
procedure AddToMainTree(aAction : TAction);
var
  aDataSet: TStatistic;
  Node1: TTreeNode;
  aList: TStatistic;
begin
  if (Data.Users.Rights.Right('STATISTICS') > RIGHT_NONE) then
    begin
      Data.RegisterLinkHandler('STATISTICS',@fMainTreeFrame.OpenLink,@fMainTreeFrame.NewFromLink);
      MainNode := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
      MainNode.Height := 34;
      TTreeEntry(MainNode.Data).Typ := etStatistics;
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
      aList := TStatistic.Create(nil,Data);
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
            end;
        end;
    end;
end;

procedure TfStatisticFrame.CheckSQL;
var
  FStatementList: TSQLElementList;
  i: Integer;
  aStmt: TSQLElement = nil;
  FExcludeKeywords: TStringList;
  FToken: TSQLToken;
  FSQLStream : TStringStream;
  FSQLParser : TOwnSQLParser;
  FSQLScanner : TSQLScanner;
  m: TSynEditMark;
  FStmtCount : Integer = 0;
  ErrVisible : Boolean = False;

  procedure GetStmtTables;
  var
    aList: TSQLElementList;
    a: Integer;
  begin
    FTables.Clear;
    if aStmt is TSQLSelectStatement then
      begin
        aList := TSQLSelectStatement(aStmt).Tables;
        for a := 0 to aList.Count-1 do
          if aList[a] is TSQLSimpleTableReference then
            FTables.Add(TSQLSimpleTableReference(aList[a]).ObjectName.Name);
      end;
  end;
begin
  FSQLStream := TStringStream.Create(BuildSQL(DataSet.FieldByName('QUERRY').AsString));
  FExcludeKeywords := TStringList.Create;
  FExcludeKeywords.Add('SUBSTRING');
  FExcludeKeywords.Add('REPLACE');
  FExcludeKeywords.Add('CHARINDEX');
  FExcludeKeywords.Add('CHARINDEX');
  FExcludeKeywords.Add('CHARINDEX');
  FSQLScanner := TSQLScanner.Create(FSQLStream);
  FSQLScanner.ExcludeKeywords := FExcludeKeywords;
  FToken := FSQLScanner.FetchToken;
  lbErrors.Clear;
  if trim(FSQLStream.DataString) <> '' then inc(FStmtCount);
  try
    while (FToken <> tsqlEOF) do
      begin
        if FToken = tsqlSEMICOLON then inc(FStmtCount);
//        lbErrors.Items.Add('Token:'+IntToStr(FSQLScanner.CurRow)+':'+IntToStr(FSQLScanner.CurColumn)+' '+FSQLScanner.CurTokenString+' Info:'+TokenInfos[FSQLScanner.CurToken]);
        FToken := FSQLScanner.FetchToken;
      end;
  except
    on e : Exception do
      begin
        m := TSynEditMark.Create(smQuerry);
        m.Line := FSQLScanner.CurRow;
        m.ImageList := GutterImages;
        m.ImageIndex := 0;
        m.Column:=FSQLScanner.CurColumn;
        m.Visible := true;
        smQuerry.Marks.Add(m);
        lbErrors.Items.Add('['+IntToStr(FSQLScanner.CurRow)+':'+IntToStr(FSQLScanner.CurColumn)+'] '+e.Message);
        ErrVisible := True;
      end;
  end;
  FreeAndNil(FSQLScanner);
  FSQLStream.Position:=0;
  FSQLScanner := TSQLScanner.Create(FSQLStream);
  FSQLScanner.ExcludeKeywords := FExcludeKeywords;
  FSQLParser := TOwnSQLParser.Create(FSQLScanner);
  if not ErrVisible then
    begin
      while smQuerry.Marks.Count > 0 do
        smQuerry.Marks.Delete(0);
      try
        aStmt := FSQLParser.Parse;
        dec(FStmtCount);
        GetStmtTables;
        while (FStmtCount > 0) and (FSQLScanner.CurToken <> tsqlEOF) do
          begin
            aStmt := FSQLParser.Parse;
            dec(FStmtCount);
          end;

      except
        on e : ESQLParser do
          begin
            if e.Col > 0 then
              begin
                m := TSynEditMark.Create(smQuerry);
                m.Line := e.Line;
                m.ImageList := GutterImages;
                m.ImageIndex := 0;
                m.Column:=e.Col;
                m.Visible := true;
                smQuerry.Marks.Add(m);
                lbErrors.Items.Add('['+IntToStr(e.Line)+':'+IntToStr(e.Col)+'] '+e.Message+','+FSQLParser.CurSource);
                ErrVisible := True;
              end;
          end;
      end;
    end;
  FSQLParser.Free;
  FSQLScanner.Free;
  FExcludeKeywords.Free;
  FSQLStream.Free;
  if lbErrors.Visible <> ErrVisible then
    begin
      lbErrors.Visible:=ErrVisible;
      Splitter3.Visible:=ErrVisible;
    end;
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
sl.Text:=aFilter;
with Application as IBaseDBInterface do
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
          end;
      end;
    cFilter := StringReplace(cFilter,'@USERID@',Data.Users.Id.AsString,[rfReplaceAll]);
  end;
Result := cFilter;
end;

procedure TfStatisticFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;

procedure TfStatisticFrame.acCancelExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicCancel;
//      Data.Rollback(FConnection);
//      Data.StartTransaction(FConnection);
    end;
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
      if Assigned(FTreeNode) then
        fMainTreeFrame.tvMain.Items.Delete(FTreeNode);
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
  tsResults.TabVisible:=False;
  try
    try
      if Assigned(StatisticResults.DataSet) then
        StatisticResults.DataSet.Free;
      StatisticResults.DataSet := Data.GetNewDataSet(BuildSQL(DataSet.FieldByName('QUERRY').AsString));
      StatisticResults.DataSet.Open;
      if trim(smQuerry1.Lines.Text) <> '' then
        begin
          if Assigned(Detail.DataSet) then
            Detail.DataSet.Free;
          Detail.DataSet := Data.GetNewDataSet(BuildSQL(smQuerry1.Lines.Text),Data.MainConnection,StatisticResults.DataSet);
          Detail.DataSet.Open;
          pDetails.Visible:=True;
          spDetails.Visible:=True;
          if trim(smQuerry2.Lines.Text) <> '' then
            begin
              if Assigned(SubDetail.DataSet) then
                SubDetail.DataSet.Free;
              SubDetail.DataSet := Data.GetNewDataSet(BuildSQL(smQuerry2.Lines.Text),Data.MainConnection,Detail.DataSet);
              SubDetail.DataSet.Open;
              pSubDetails.Visible:=True;
              spSubDetails.Visible:=True;
            end;
        end;
      aTime := GetTickCount-aTime;
      lStatus.Caption:=Format(strQueryTime,[MSecToTimeStr(aTime),StatisticResults.DataSet.RecordCount]);
      lStatus.Visible:=True;
      fSelectReport.ReportType:= copy(DataSet.Id.AsString,length(DataSet.Id.AsString)-3,4);
      pcTabs.ActivePage := tsResults;
      if Data.Reports.Count > 0 then
        begin
          Printer.PrinterIndex:=-1;//Default Printer
          fSelectReport.Report := frReport;
          if fSelectReport.LoadReport then
            begin
              frReport.Preview := frPreview;
              tsReport.TabVisible:=True;
              frPreview.Clear;
              frPreview.Caption:='';
              pcTabs.ActivePage := tsReport;
            end;
        end;
      if tsReport.TabVisible then
        begin
          if frReport.PrepareReport then
            frReport.ShowPreparedReport;
          if frReport.EMFPages.Count > 1 then
            frPreview.TwoPages
          else frPreview.Zoom:=100;
        end;
      if pcTabs.ActivePage = tsResults then
        gList.SetFocus;
    except
      on E : Exception do
        begin
          acExecute.Enabled:=True;
          bExecute1.Action := acExecute;
          raise Exception.Create(e.Message+lineending+BuildSQL(DataSet.FieldByName('QUERRY').AsString));
        end;
    end;
  finally
    tsResults.TabVisible:=True;
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
          AssignFile(f,UTF8ToSys(SaveDialog.FileName));
          Rewrite(f);
          for x := 0 to StatisticResults.DataSet.FieldCount-1 do
            write(f,UTF8ToSys(StatisticResults.DataSet.FieldDefs[x].DisplayName)+';');
          writeln(f);
          StatisticResults.DataSet.First;
          while not StatisticResults.DataSet.EOF do
            begin
              for x := 0 to StatisticResults.DataSet.FieldCount-1 do
                write(f,UTF8ToSys(StatisticResults.DataSet.Fields[x].AsString)+';');
              writeln(f);
              StatisticResults.DataSet.Next;
            end;
          CloseFile(f);
          StatisticResults.DataSet.EnableControls;
        end;
      MyWorkbook.Free;
    end;
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
procedure TfStatisticFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfStatisticFrame.acSaveExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicPost;
//      Data.Commit(FConnection);
//      Data.StartTransaction(FConnection);
      if Assigned(FTreeNode) then
        begin
          TTreeEntry(FTreeNode.Data).Rec:=FDataSet.Id.AsVariant;
          TTreeEntry(FTreeNode.Data).DataSourceType:=TStatistic;
          with FDataSet.DataSet as IBaseManageDB do
            TTreeEntry(FTreeNode.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(FDataSet.GetBookmark));;
        end;
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

procedure TfStatisticFrame.bEditFilterClick(Sender: TObject);
var
  Animate: TAnimationController;
begin
  bEditFilter.Enabled:=False;
  Animate := TAnimationController.Create(pTop);
  smQuerry.BeginUpdate;
  smQuerry1.BeginUpdate;
  smQuerry2.BeginUpdate;
  if bEditFilter.Down then
    begin
      pTop.Visible:=True;
      Animate.AnimateControlHeight(244)
    end
  else
    begin
      Animate.AnimateControlHeight(0);
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
end;

procedure TfStatisticFrame.BtZoomInClick(Sender: TObject);
begin
  frPreview.Zoom:=frPreview.Zoom*1.1;
end;

procedure TfStatisticFrame.BtZoomOutClick(Sender: TObject);
begin
  frPreview.Zoom:=frPreview.Zoom*0.9;
end;

procedure TfStatisticFrame.DataSetRemove(Sender: TObject);
begin
  if Assigned(FTreeNode) then FTreeNode.Free;
end;

procedure TfStatisticFrame.DatasourceStateChange(Sender: TObject);
begin

end;

procedure TfStatisticFrame.FrameEnter(Sender: TObject);
begin
  //ActionList1.State:=asNormal;
end;
procedure TfStatisticFrame.FrameExit(Sender: TObject);
begin
  //ActionList1.State:=asSuspended;
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
begin
  tsReport.TabVisible:=False;
  SetRights;
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
  inherited DoOpen;
end;
function TfStatisticFrame.SetRights: Boolean;
begin
  FEditable := ((Data.Users.Rights.Right('STATISTICS') > RIGHT_READ));
  Result := FEditable;
  acDelete.Enabled:=FEditable and (Data.Users.Rights.Right('STATISTICS') > RIGHT_WRITE);
  acRights.Enabled:=Data.Users.Rights.Right('STATISTICS') >= RIGHT_PERMIT;
  bEditFilter.Enabled:=FEditable;
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
  FTables := TStringList.Create;
  pTop.Height := 0;
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
  if Assigned(FConnection) then
    begin
      CloseConnection(acSave.Enabled);
      DataSet.Destroy;
      DataSet := nil;
      FreeAndNil(FConnection);
    end;
  FTables.Free;
  FVariables.Free;
  FSynCompletion.Free;
  inherited;
end;
function TfStatisticFrame.OpenFromLink(aLink: string) : Boolean;
begin
  Result := False;
  if not ((copy(aLink,0,pos('@',aLink)-1) = 'STATISTICS')) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
//  Data.StartTransaction(FConnection);
  DataSet := TStatistic.Create(Self,Data,FConnection);
  Data.SetFilter(FDataSet,Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1);
  if FDataSet.Count > 0 then
    begin
      TabCaption := TProject(FDataSet).Text.AsString;
      DoOpen;
      Result := True;
    end;
end;
procedure TfStatisticFrame.New;
begin
  SetRights;
  if not FEditable then exit;
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
//  Data.StartTransaction(FConnection);
  DataSet := TStatistic.Create(Self,Data,FConnection);
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
  bEditFilter.Down:=True;
  pTop.Visible:=True;
  pTop.Height:=244;
  smQuerry.SetFocus;
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

end.

