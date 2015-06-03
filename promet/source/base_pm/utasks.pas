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
unit utasks;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, LR_DBSet, LR_Class,
   Forms, Controls, DBGrids, ValEdit, ExtCtrls, Buttons, ComCtrls,
  uPrometFramesInplaceDB, uExtControls, db, Grids, ActnList, Menus, StdCtrls,
  uBaseDBClasses, uBaseDbInterface, uGridView, uIntfStrConsts,
  Variants, uBaseSearch, Graphics, Spin, EditBtn, Dialogs,Clipbrd, ExtDlgs,uBaseDatasetInterfaces;
type
  TOnStartTime = procedure(Sender : TObject;aProject,aTask,aCategory : string) of object;

  { TfTaskFrame }

  TfTaskFrame = class(TPrometInplaceDBFrame)
    acExport: TAction;
    acImport: TAction;
    acDelPos: TAction;
    acAddPos: TAction;
    acMAkeSubTask: TAction;
    acRefresh: TAction;
    acLink: TAction;
    acStartTime: TAction;
    acStopTime: TAction;
    acPrint: TAction;
    acFilter: TAction;
    acSaveFilter: TAction;
    acFilterRights: TAction;
    acDeleteFilter: TAction;
    acGotoProject: TAction;
    acDefaultFilter: TAction;
    acMarkSeen: TAction;
    acOpen: TAction;
    acSetOwner: TAction;
    acSetUser: TAction;
    acAppendLinkToDependencies: TAction;
    acTerminate: TAction;
    acInformwithexternMail: TAction;
    acInformwithinternMail: TAction;
    acSave: TAction;
    acCancel: TAction;
    acMarkProblem: TAction;
    acMoveToProject: TAction;
    acAddTasksfromProject: TAction;
    acUnmakeSubTask: TAction;
    ActionList: TActionList;
    ActionList1: TActionList;
    bAddPos: TSpeedButton;
    bAddPos1: TSpeedButton;
    bDelegated2: TSpeedButton;
    bDeletePos10: TSpeedButton;
    bDeletePos11: TSpeedButton;
    bDeletePos12: TSpeedButton;
    bDeletePos8: TSpeedButton;
    bDeletePos9: TSpeedButton;
    bEnterTime2: TSpeedButton;
    Bevel11: TBevel;
    bFuture: TSpeedButton;
    bFuture1: TSpeedButton;
    bDeletePos: TSpeedButton;
    bDeletePos1: TSpeedButton;
    bDeletePos2: TSpeedButton;
    bDeletePos3: TSpeedButton;
    bDeletePos4: TSpeedButton;
    bDeletePos5: TSpeedButton;
    bDeletePos6: TSpeedButton;
    bDeletePos7: TSpeedButton;
    bDependencies1: TSpeedButton;
    bEditFilter: TSpeedButton;
    bEnterTime: TSpeedButton;
    bEnterTime1: TSpeedButton;
    Bevel10: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    bExecute: TSpeedButton;
    bOldTasks1: TSpeedButton;
    bDelegated1: TSpeedButton;
    bDependencies: TSpeedButton;
    bRefresh: TSpeedButton;
    bRefresh1: TSpeedButton;
    bOldTasks: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    bDelegated: TSpeedButton;
    CalendarDialog1: TCalendarDialog;
    cbFilter: TComboBox;
    cbMaxResults: TCheckBox;
    Datasource: TDatasource;
    ExtRotatedLabel5: TExtRotatedLabel;
    Label7: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miCopyLink: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel10: TPanel;
    Panel9: TPanel;
    pBottom: TPanel;
    pmGrid: TPopupMenu;
    PUsers: TfrDBDataSet;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    Users: TDatasource;
    eFilterEdit: TSynMemo;
    eFilterIn: TEdit;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    ExtRotatedLabel4: TLabel;
    History: TDatasource;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbResults: TListBox;
    lFilterEdit: TLabel;
    lFilterIn: TLabel;
    MandantDetails: TDatasource;
    MenuItem1: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    miOpen: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    bEditRows: TSpeedButton;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pFilterOpt: TPanel;
    pFilterOptions: TPanel;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pNav1: TPanel;
    pSearch: TPanel;
    Report: TfrReport;
    sbDelete: TSpeedButton;
    sbSave: TSpeedButton;
    sbSave1: TSpeedButton;
    bFilter: TSpeedButton;
    sbSavePublic: TSpeedButton;
    seMaxresults: TSpinEdit;
    SynSQLSyn2: TSynSQLSyn;
    pToolbar: TPanel;
    ToolBar: TToolBar;
    tbTop: TPanel;
    procedure acAddPosExecute(Sender: TObject);
    procedure acAddTasksfromProjectExecute(Sender: TObject);
    procedure acAppendLinkToDependenciesExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acDefaultFilterExecute(Sender: TObject);
    procedure acDeleteFilterExecute(Sender: TObject);
    procedure acDelPosExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acGotoProjectExecute(Sender: TObject);
    procedure acInformwithexternMailExecute(Sender: TObject);
    procedure acLinkExecute(Sender: TObject);
    procedure acMAkeSubTaskExecute(Sender: TObject);
    procedure acMarkProblemExecute(Sender: TObject);
    procedure acMarkSeenExecute(Sender: TObject);
    procedure acMoveToProjectExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acFilterRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveFilterExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure acSetOwnerExecute(Sender: TObject);
    procedure acSetUserExecute(Sender: TObject);
    procedure acStartTimeExecute(Sender: TObject);
    procedure acStopTimeExecute(Sender: TObject);
    procedure acTerminateExecute(Sender: TObject);
    procedure ActiveSearchEndSearch(Sender: TObject);
    procedure ActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string;aActive : Boolean; aLink: string;aPrio :Integer; aItem: TBaseDBList=nil);
    procedure acUnmakeSubTaskExecute(Sender: TObject);
    procedure bEditFilterClick(Sender: TObject);
    procedure cbFilterSelect(Sender: TObject);
    procedure ChangeVisibleRows(Sender: TObject);
    procedure DatasourceDataChange(Sender: TObject; Field: TField);
    procedure DatasourceStateChange(Sender: TObject);
    procedure eFilterEditChange(Sender: TObject);
    procedure FGridViewAddRow(Sender: TObject);
    procedure FGridViewAfterInsert(Sender: TObject);
    procedure FGridViewBeforeInsert(Sender: TObject);
    procedure FGridViewCellButtonClick(Sender: TObject; Cell: TPoint;
      Field: TColumn);
    procedure FGridViewCellChanged(Sender: TObject; NewCell, OldCell: TPoint);
    procedure FGridViewCheckBoxColumnToggle(Field: TColumn);
    procedure FGridViewDblClick(Sender: TObject);
    procedure FGridViewDelete(Sender: TObject);
    function FGridViewDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
    procedure FGridViewFilterCell(Sender: TObject; aCol: TColumn;
      aText: string; var NewText: string);
    procedure FGridViewGetCellText(Sender: TObject; aCol: TColumn;aRow : Integer;
      var NewText: string;aFont : TFont);
    function FGridViewSearchKey(Sender: TObject; X, Y: Integer; Field: TColumn;
      var Key: Word; Shift: TShiftState; SearchString: string) : Boolean;
    procedure FGridViewSetCellText(Sender: TObject; aCol: TColumn;
      aRow: Integer; var NewText: string);
    procedure FGridViewSetupPosition(Sender: TObject;Columns : TGridColumns);
    function fSearchAddTasksToProject(aLink: string): Boolean;
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchOpenItemMulti(aLink: string): Boolean;
    function fSearchOpenOwnerItem(aLink: string): Boolean;
    function fSearchOpenUserItem(aLink: string): Boolean;
    function fSearchOpenUserMailItem(aLink: string): Boolean;
    procedure lbResultsDblClick(Sender: TObject);
    procedure DoInsertInplaceSearch(Data : PtrInt);
    procedure pmGridPopup(Sender: TObject);
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
    procedure seMaxresultsChange(Sender: TObject);
  private
    FSearcheMail : string;
    FFilter: string;
    FFilterType : string;
    FBaseFilter: string;
    FBaseName: string;
    { private declarations }
    FGridView : TfGridView;
    FIgnoreUser: Boolean;
    FOnStartTime: TOnStartTime;
    FOwners : TStringList;
    FUserID: Variant;
    FUsers : TStringList;
    FProject : Variant;
    FProjectID : Variant;
    ActiveSearch : TSearch;
    FAutoFilter : string;
    aUsers : TUser;
    FUseTransactions: Boolean;
    function GetFilterIn: string;
    procedure ParseForms(Filter : string);
    procedure SetBaseFilter(AValue: string);
    procedure SetBaseName(AValue: string);
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetFilter(AValue: string);
    procedure SetFilterIn(AValue: string);
    procedure SetFilterType(AValue: string);
    procedure DoFilterFocus;
    procedure SetUserID(AValue: Variant);
  public
    { public declarations }
    FTaskNode : TTreeNode;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property  BaseName : string read FBaseName write SetBaseName;
    property UserID : Variant read FUserID write SetUserID;
    procedure SetRights(Editable : Boolean);override;
    procedure ShowFrame; override;
    procedure DoRefresh; override;
    property BaseFilter : string read FBaseFilter write SetBaseFilter;
    property FilterType : string read FFilterType write SetFilterType;
    property FilterIn : string read GetFilterIn write SetFilterIn;
    property Filter : string read FFilter write SetFilter;
    property GridView : TfGridView read FGridView;
    property IgnoreUser : Boolean read FIgnoreUser write FIgnoreUser;
    procedure SetLanguage; override;
    procedure Post;
    property OnStartTime : TOnStartTime read FOnStartTime write FOnStartTime;
    property UseTransactions : Boolean read FUseTransactions write FUseTransactions;
  end;
procedure AddToMainTree(aAction : TAction;var aNode : TTreeNode);
procedure RefreshTasks(FNode :TTreeNode);
function DayTimeToStr(nf : Real) : string;
function GetHoursPerDay : Real;
function StrToDayTime(aStr : string) : Real;
resourcestring
  strRecordCount                           = 'Anzahl: %d';
  strSearchFromTasks                       = 'Mit Öffnen wird das gewählte Projekt in die Aufgabe übernommen';
  strAssignedTasks                         = 'Aufgaben: %s';
  strMyTasks                               = 'von mir erstellte Aufgaben: %s';
  strUnterminatedDependencies              = 'Es gibt unterminierte Abhängigkeiten für diese Aufgabe:'+lineending+'%s';
  strFailed                                = 'Fehlgeschlagen';
  strChangeDependencies                    = 'Sollen Abhängigkeiten der gelöschten Aufgaben aufgelöst werden ? (längere Dauer)';
implementation
{$R *.lfm}
uses uRowEditor,uTask,ubasevisualapplicationtools,uData,uMainTreeFrame,
  uSearch,uProjects,uTaskEdit,uBaseApplication,LCLType,uBaseERPDBClasses,
  uSelectReport,uFormAnimate,md5,uNRights,uBaseVisualControls,
  uBaseVisualApplication,uError,uSendMail,uPerson,Utils,uprometipc;
procedure TfTaskFrame.SetDataSet(const AValue: TBaseDBDataSet);
var
  aFilter: String = '';
  aUser: TUser;
begin
  if AValue = FDataSet then exit;
  inherited SetDataSet(AValue);
  FDataSet.DataSet.Close;
  FGridView.DataSet := AValue;
  Datasource.DataSet := FDataSet.DataSet;
  aUsers := TUser.Create(nil);
  aUsers.Select(FUserID);
  aUsers.Open;
  if not FIgnoreUser then
    TTaskList(FDataSet).SelectByUser(aUsers.FieldByName('ACCOUNTNO').AsString);
  SetFilterType('B');
  if tbTop.Visible then
    begin
      with Application as IBaseDBInterface do
        aFilter := DBConfig.ReadString('DEFAULTFILTER'+FFilterType,'');
    end;
  if aFilter <> '' then
    begin
      try
        cbFilter.Text := aFilter;
        cbFilterSelect(nil);
      except
        begin
          cbFilter.Text := strNoSelectFilter;
          cbFilterSelect(nil);
        end;
      end;
    end
  else
    ChangeVisibleRows(bOldTasks);
  aUser := TUser.CreateEx(Self,Data);
  aUser.Select(fUserID);
  aUser.Open;
  if aUser.Count>0 then
    TTaskList(FDataSet).UserId:=aUser.FieldByName('ACCOUNTNO').AsString;
  aUser.Free;
  acMarkSeen.Visible:=not (AValue is TProjectTasks);
  if UseTransactions then
    Data.StartTransaction(FConnection);
end;
procedure TfTaskFrame.SetFilter(AValue: string);
var
  aFilter: TStringList;
  i: Integer;
  aChanged: Boolean;
begin
  aChanged := FFilter <> AValue;
  FFilter := AValue;
  eFilterEdit.Lines.Text:=FFilter;
  ParseForms(FFilter);
end;
procedure TfTaskFrame.SetFilterIn(AValue: string);
begin
  with FDataSet.DataSet as IBaseDBFilter do
    FilterTables := AValue;
end;
procedure TfTaskFrame.SetFilterType(AValue: string);
begin
  if FFilterType=AValue then Exit;
  FFilterType:=AValue;
  cbFilter.Clear;
  cbFilter.Items.Add(strNoSelectFilter);
  cbFilter.Text := strNoSelectFilter;
  with Application as IBaseDBInterface do
    with Data.Filters.DataSet do
      begin
        with Data.Filters.DataSet as IBaseDBFilter do
          Data.SetFilter(Data.Filters,Data.ProcessTerm(Data.QuoteField('TYPE')+'='+Data.QuoteValue(FFilterType))+' and ('+Data.ProcessTerm(Data.QuoteField('USER')+'='+Data.QuoteValue(aUsers.FieldByName('ACCOUNTNO').AsString))+' or '+Data.ProcessTerm(Data.Quotefield('USER')+'='+Data.QuoteValue(''))+')');
        First;
        while not EOF do
          begin
            cbFilter.Items.Add(FieldbyName('NAME').AsString);
            Next;
          end;
        Data.SetFilter(Data.Filters,'');
      end;
end;
procedure TfTaskFrame.DoFilterFocus;
begin
end;
procedure TfTaskFrame.SetUserID(AValue: Variant);
begin
  if FUserID=AValue then Exit;
  FUserID:=AValue;
end;
function DayTimeToStr(nf: Real): string;
begin
  if nf < 1/GetHoursPerDay then
    Result := IntToStr(round(nf*GetHoursPerDay*MinsPerHour))+'min'
  else if nf < 1 then
    Result := FormatFloat('0.0',nf*GetHoursPerDay)+'h'
  else Result := FormatFloat('0.0',nf);
end;
function GetHoursPerDay: Real;
begin
  Result := 8;
end;
function StrToDayTime(aStr: string): Real;
begin
  Result := 0;
  if copy(trim(aStr),length(trim(aStr)),1)='h' then
    begin
      if TryStrToFloat(trim(copy(trim(aStr),0,length(trim(aStr))-1)),Result) then
        Result := Result/GetHoursPerDay;
    end
  else if copy(trim(aStr),length(trim(aStr))-2,3)='min' then
    begin
      if TryStrToFloat(trim(copy(trim(aStr),0,length(trim(aStr))-3)),Result) then
        Result := (Result/GetHoursPerDay)/MinsPerHour;
    end
  else TryStrToFloat(aStr,Result);
end;
procedure TfTaskFrame.FGridViewSetupPosition(Sender: TObject;Columns : TGridColumns);
var
  i: Integer;
begin
  for i := 0 to FGridView.Columns.Count-1 do
    begin
      if (TColumn(FGridView.Columns[i]).FieldName = 'ACTIVE')
      or (TColumn(FGridView.Columns[i]).FieldName = 'COMPLETED')
      or (TColumn(FGridView.Columns[i]).FieldName = 'CHECKED')
      then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsCheckboxColumn;
          TColumn(FGridView.Columns[i]).ValueChecked:='Y';
          TColumn(FGridView.Columns[i]).ValueUnChecked:='N';
          TColumn(FGridView.Columns[i]).ReadOnly := False;
          FGridView.OnCheckBoxColumnToggle:=@FGridViewCheckBoxColumnToggle;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'PROJECT' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'USER' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'OWNER' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'STARTDATE' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'DUEDATE' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      ;
    end;
end;

function TfTaskFrame.fSearchAddTasksToProject(aLink: string): Boolean;
var
  aProject: TProject;
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  aProject := TProject.CreateEx(Self,Data);
  aProject.SelectFromLink(aLink);
  aProject.Open;
  Result := aProject.Count>0;
  if pSearch.Visible then
    FGridView.EndUpdate;
  pSearch.Visible:=False;
  if Result and (Assigned(TTaskList(DataSet).Parent) and (TTaskList(DataSet).Parent is TProject)) then
    begin
      TProject(TTaskList(DataSet).Parent).DuplicateFromOtherProcess(aProject);
      acRefresh.Execute;
    end;
  aProject.Free;
  Screen.Cursor:=crDefault;
end;

function TfTaskFrame.fSearchOpenItem(aLink: string): Boolean;
var
  aProject: TProject;
  aCount: Integer;
begin
  Result := False;
  aProject := TProject.CreateEx(Self,Data);
  aProject.SelectFromLink(aLink);
  aProject.Open;
  Result := aProject.Count>0;
  if pSearch.Visible then
    FGridView.EndUpdate;
  pSearch.Visible:=False;
  if Result and FGridView.GotoActiveRow then
    begin
      FGridView.BeginUpdate;
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      DataSet.FieldByName('PROJECTID').AsString := aProject.Id.AsString;
      DataSet.FieldByName('PROJECT').AsString := Data.GetLinkDesc(aLink);
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.EndUpdate;
      FGridView.SetEdited;
    end;
  aProject.Free;
end;

function TfTaskFrame.fSearchOpenItemMulti(aLink: string): Boolean;
var
  aProject: TProject;
  aRow: Integer;
begin
  Result := False;
  aProject := TProject.CreateEx(Self,Data);
  aProject.SelectFromLink(aLink);
  aProject.Open;
  Result := aProject.Count>0;
  if pSearch.Visible then
    FGridView.EndUpdate;
  pSearch.Visible:=False;
  if Result then
    for aRow := FGridView.gList.Selection.Bottom+1 downto FGridView.gList.Selection.Top+1 do
      begin
        if FGridView.GotoRowNumber(aRow-1) then
          begin
            if not FDataSet.CanEdit then
              FDataSet.DataSet.Edit;
            FDataSet.FieldByName('PROJECTID').AsString := aProject.Id.AsString;
            if not FDataSet.CanEdit then
              FDataSet.DataSet.Edit;
            FDataSet.FieldByName('PROJECT').AsString := Data.GetLinkDesc(aLink);
          end
        else break;
      end;
  FGridView.Refresh;
  aProject.Free;
end;

function TfTaskFrame.fSearchOpenOwnerItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
begin
  Result := False;
  aUser := TUser.CreateEx(Self,Data);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  Result := aUser.Count>0;
  if pSearch.Visible then
    FGridView.EndUpdate;
  pSearch.Visible:=False;
  if Result and FGridView.GotoActiveRow then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      DataSet.FieldByName('OWNER').AsString := aUser.FieldByName('ACCOUNTNO').AsString;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end
  else
    begin
      if DataSet.CanEdit then
        DataSet.DataSet.Post;
      DataSet.DataSet.Edit;
      DataSet.FieldByName('OWNER').Clear;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end;
  aUSer.Free;
end;
function TfTaskFrame.fSearchOpenUserItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
  aSel: TGridRect;
begin
  Result := False;
  aUser := TUser.CreateEx(Self,Data);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  Result := aUser.Count>0;
  if Result and FGridView.GotoActiveRow then
    begin
      if DataSet.CanEdit then
        DataSet.DataSet.Post;
      DataSet.DataSet.Edit;
      DataSet.FieldByName('USER').AsString := aUser.FieldByName('ACCOUNTNO').AsString;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end
  else
    begin
      if DataSet.CanEdit then
        DataSet.DataSet.Post;
      DataSet.DataSet.Edit;
      DataSet.FieldByName('USER').Clear;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end;
  if pSearch.Visible then
    FGridView.EndUpdate;
  pSearch.Visible:=False;
  aUSer.Free;
end;

function TfTaskFrame.fSearchOpenUserMailItem(aLink: string): Boolean;
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

procedure TfTaskFrame.lbResultsDblClick(Sender: TObject);
begin
  if lbResults.ItemIndex < 0 then exit;
  pSearch.Visible:=False;
  Application.ProcessMessages;
  if copy(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link,0,5) = 'USERS' then
    begin
      if pSearch.Caption='USER' then
        fSearchOpenUserItem(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link)
      else
        fSearchOpenOwnerItem(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
    end
  else if copy(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link,0,8) = 'PROJECTS' then
    begin
      fSearchOpenItem(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
    end;
  if pSearch.Visible then
    FGridView.EndUpdate;
  pSearch.Visible:=False;
  FGridView.SetFocus;
end;
procedure TfTaskFrame.DoInsertInplaceSearch(Data: PtrInt);
begin
  lbResultsDblClick(nil);
  Screen.Cursor:=crDefault;
end;

procedure TfTaskFrame.pmGridPopup(Sender: TObject);
begin
  acAppendLinkToDependencies.Enabled := Clipboard.HasFormat(LinkClipboardFormat);
end;

procedure TfTaskFrame.ReportGetValue(const ParName: String;
  var ParValue: Variant);
begin
  if Uppercase(ParName) = 'USER' then
    begin
      if trim(FUsers.Values[DataSet.FieldByName('USER').AsString]) = '' then
        FUsers.Values[DataSet.FieldByName('USER').AsString] := TTaskList(FDataSet).UserName;
      ParValue := FUsers.Values[DataSet.FieldByName('USER').AsString]
    end
  else if Uppercase(ParName) = 'OWNER' then
    begin
      if trim(FOwners.Values[DataSet.FieldByName('OWNER').AsString]) = '' then
        FOwners.Values[DataSet.FieldByName('OWNER').AsString] := TTaskList(FDataSet).OwnerName;
      ParValue := FOwners.Values[DataSet.FieldByName('OWNER').AsString]
    end;
end;
procedure TfTaskFrame.seMaxresultsChange(Sender: TObject);
begin
  if not cbMaxResults.Enabled then exit;
  acFilter.Execute;
end;
procedure TfTaskFrame.ParseForms(Filter: string);
begin

end;
function TfTaskFrame.GetFilterIn: string;
begin
  with FDataSet.DataSet as IBaseDBFilter do
    Result := FilterTables;
end;
procedure TfTaskFrame.acDelPosExecute(Sender: TObject);
begin
  if not FGridView.GotoActiveRow then exit;
  if ((DataSet.FieldByName('OWNER').AsString <> aUsers.FieldByName('ACCOUNTNO').AsString)
  and (not DataSet.FieldByName('OWNER').IsNull))
  and (not (DataSet is TProjectTasks)) then
    exit;
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      if MessageDlg(strChangeDependencies,mtInformation,[mbYes,mbNo],0) = mrNo then
        FGridView.DataSet.DataSet.DisableControls;
      FGridView.Delete;
      if FGridView.DataSet.DataSet.ControlsDisabled then
        FGridView.DataSet.DataSet.EnableControls;
      if DataSet is TProjectTasks then
        if not TProjectTasks(DataSet).Project.CanEdit then
          TProjectTasks(DataSet).Project.DataSet.Edit;
      acDelPos.Enabled := acAddPos.Enabled and ((FGridView.Count > 0) and (DataSet.State <> dsInsert));
    end;
end;
procedure TfTaskFrame.acFilterExecute(Sender: TObject);
var
  aFilter: String;
  tmp,adata,aname: String;
  aControl: TControl;
  Rec: LongInt = 0;
  aBaseFilter: String;
  aSQL: String;
begin
  aBaseFilter := '';
  if (not Assigned(DataSet)) or (not Assigned(DataSet.DataSet)) then exit;
  if DataSet.DataSet.Active then
    Rec := DataSet.GetBookmark;
  if (not bOldTasks.Down) and (not bOldTasks1.Down) and (pos('COMPLETED',FFilter) = 0) then
    aBaseFilter += '('+Data.QuoteField('COMPLETED')+'='+Data.QuoteValue('N')+')';
  if (not bDelegated.Down) and (not bDelegated1.Down) and (pos('OWNER',FFilter) = 0) and (pos('USER',FFilter) = 0) then
    begin
      if aBaseFilter <> '' then
        aBaseFilter += ' AND ';
      with DataSet.DataSet as IBaseDbFilter do
        aBaseFilter += '(('+Data.QuoteField('USER')+'='+Data.QuoteValue(aUsers.FieldByName('ACCOUNTNO').AsString)+'))';
    end;
  if (not bDependencies.Down) and (not bDependencies1.Down) and (pos('DEPDONE',FFilter) = 0) then
    begin
      if aBaseFilter <> '' then
        aBaseFilter += ' AND ';
      with DataSet.DataSet as IBaseDbFilter do
        aBaseFilter += '(('+Data.QuoteField('DEPDONE')+'='+Data.QuoteValue('Y')+') OR ((('+Data.QuoteField('STARTDATE')+'<='+Data.DateTimeToFilter(Now())+') or ('+Data.QuoteField('DUEDATE')+'<='+Data.DateTimeToFilter(Now())+'))))';
    end;
  if (not bFuture.Down) and (not bFuture1.Down) and (pos('STARTDATE',FFilter) = 0) then
    begin
      if aBaseFilter <> '' then
        aBaseFilter += ' AND ';
      with DataSet.DataSet as IBaseDbFilter do
        with Data.Filters.DataSet as IBaseDBFilter do
          aBaseFilter += '(('+Data.QuoteField('STARTDATE')+'<'+Data.DateTimeToFilter(Now()+14)+') or ('+Data.ProcessTerm(Data.QuoteField('STARTDATE')+'='+Data.QuoteValue(''))+'))';
    end;
  if (FBaseFilter <> '') and (aBaseFilter <> '') then
    aBaseFilter+=' AND ('+FBaseFilter+')'
  else if (FBaseFilter <> '') then
    aBaseFilter := FBaseFilter;
  aFilter := eFilterEdit.Lines.Text;
  tmp := aFilter;
  aFilter := '';
  while pos('%',tmp) > 0 do
    begin
      aFilter := aFilter+copy(tmp,0,pos('%',tmp)-1);
      tmp := copy(tmp,pos('%',tmp)+1,length(tmp));
      adata := copy(tmp,0,pos('%',tmp)-1);
      if adata = '' then break;
      tmp := copy(tmp,pos('%',tmp)+1,length(tmp));
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
  if lowercase(copy(trim(aFilter+tmp),0,6)) = 'select' then
    begin
      //TODO:Security Risk if eFilterEdit.Lines.Text changed !!!
      with DataSet.DataSet as IBaseDBFilter do
        begin
          if FAutoFilter <> '' then
            aSQL:=StringReplace(aFilter+tmp,'@AUTOFILTER@','AND ('+FAutoFilter+')',[])
          else
            aSQL:=StringReplace(aFilter+tmp,'@AUTOFILTER@','',[]);
          if FBaseFilter <> '' then
            aSQL:=StringReplace(aSQL,'@BASEFILTER@','AND ('+aBaseFilter+')',[])
          else
            aSQL:=StringReplace(aSQL,'@BASEFILTER@','',[]);
          FullSQL := aSQL;
          if cbMaxResults.Checked then
            Limit := seMaxResults.Value
          else Limit := 0;
          //if SortField <> '' then
          //  SortFields := SortField
          //else
          //SortFields := BaseSortFields;
          //SortDirection := FSortDirection;
          DataSet.Open;
        end;
      FGridView.Refresh(False);
    end
  else
    begin
      with DataSet.DataSet as IBaseDBFilter do
        aFilter := Data.ProcessTerm(aFilter+tmp);
      if pos('$',aFilter) > 0 then
        begin
          aFilter := StringReplace(aFilter,'$NOW()',Data.DateTimeToFilter(Now()),[rfReplaceAll,rfIgnoreCase]);
        end;
      if aBaseFilter <> '' then
        begin
          if aFilter = '' then
            aFilter := aBaseFilter
          else
            aFilter := '('+aBaseFilter+') and ('+aFilter+')';
        end;
      if FAutoFilter <> '' then
        begin
          if aFilter = '' then
            aFilter := FAutoFilter
          else
            aFilter := '('+aFilter+') and ('+FAutoFilter+')';
        end;
      with DataSet.DataSet as IBaseDBFilter do
        begin
          if cbMaxResults.Checked then
            Limit := seMaxResults.Value
          else Limit := 0;
    //      if SortField <> '' then
    //        SortFields := SortField
    //      else SortFields := BaseSortFields;
    //      SortDirection := FSortDirection;
    //      Distinct := FDistinct;
        end;
      FGridView.BaseFilter := aFilter;
    end;
  if Rec <> 0 then
    begin
      DataSet.GotoBookmark(Rec);
      DataSet.FreeBookmark(Rec);
    end;
  pBottom.Caption:=Format(strRecordCount,[DataSet.Count]);
  if acFilter.ActionComponent = bFilter then
    DoFilterFocus;
end;
procedure TfTaskFrame.acGotoProjectExecute(Sender: TObject);
var
  aProject: TProject;
begin
  if not FGridView.GotoActiveRow then exit;
  aProject := TProject.Create(nil);
  aProject.Select(DataSet.FieldByName('PROJECTID').AsVariant);
  aProject.Open;
  if aProject.Count > 0 then
    Data.GotoLink(Data.BuildLink(aProject.DataSet));
  aProject.Free;
end;

procedure TfTaskFrame.acInformwithexternMailExecute(Sender: TObject);
var
  i: Integer;
  aLink: String = '';
  aFile: String;
  sl: TStringList;
  aUser: TUser;
  aDesc: String = '';
begin
  fSearch.AllowSearchTypes(strUsers+','+strCustomers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenUserMailItem;
  FSearcheMail:='';
  if FGridView.GotoActiveRow then
    aLink := FGridView.DataSet.GetLink;
  if FGridView.DataSet.FieldByName('USER').AsString<>'' then
    begin
      aUser := TUser.Create(nil);
      aUser.SelectByAccountno(FGridView.DataSet.FieldByName('USER').AsString);
      aUser.Open;
      if aUser.Count>0 then;
        FSearcheMail := trim(aUser.FieldByName('EMAIL').AsString);
      aUser.Free;
    end;
  if FSearcheMail='' then
    fSearch.Execute(True,'LISTU',strSearchFromMailSelect);
  fSearch.SetLanguage;
  if (aLink<>'') then
    begin
      with BaseApplication as IBaseApplication do
        aFile := GetInternalTempDir+ValidateFileName(Data.GetLinkDesc(aLink))+'.plink';
      sl := TStringList.Create;
      sl.Add(aLink);
      sl.SaveToFile(aFile);
      sl.Free;
      if Data.GetLinkLongDesc(aLink)<>'' then
        aDesc := strDescription+':'+#9#9#9+LineEnding+Data.GetLinkLongDesc(aLink);
      if FGridView.DataSet.FieldByName('OWNER').AsString<>'' then
        begin
          aUser := TUser.Create(nil);
          aUser.SelectByAccountno(FGridView.DataSet.FieldByName('OWNER').AsString);
          aUser.Open;
          if aUser.Count>0 then;
            aDesc := strResponsable+':'+#9#9+aUser.FieldByName('NAME').AsString+LineEnding+aDesc;
          aUser.Free;
        end;
      if FGridView.DataSet.FieldByName('USER').AsString<>'' then
        begin
          aUser := TUser.Create(nil);
          aUser.SelectByAccountno(FGridView.DataSet.FieldByName('USER').AsString);
          aUser.Open;
          if aUser.Count>0 then;
            aDesc := strWorker+':'+#9#9#9+aUser.FieldByName('NAME').AsString+LineEnding+aDesc;
          aUser.Free;
        end;
      if FGridView.DataSet.FieldByName('PROJECT').AsString<>'' then
        aDesc := strProject+':'+#9#9#9+FGridView.DataSet.FieldByName('PROJECT').AsString+LineEnding+aDesc;
      if FGridView.DataSet.FieldByName('STARTDATE').AsString<>'' then
        aDesc := strStart+':'+#9#9#9+FGridView.DataSet.FieldByName('STARTDATE').AsString+LineEnding+aDesc;
      if FGridView.DataSet.FieldByName('DUEDATE').AsString<>'' then
        aDesc := strDue+':'+#9#9#9#9+FGridView.DataSet.FieldByName('DUEDATE').AsString+LineEnding+aDesc;
      aDesc := strTask+':'+#9#9#9+FGridView.DataSet.FieldByName('SUMMARY').AsString+LineEnding+aDesc;
      DoSendMail(strTask+':'+Data.GetLinkDesc(aLink),aDesc, aFile,'','','',FSearcheMail);
    end;
end;

procedure TfTaskFrame.acLinkExecute(Sender: TObject);
var
  aRow: LongInt;
  aLink: String;
  aTask: TTask;
  aDueDate : TDateTime = 0;
begin
  for aRow := FGridView.gList.Selection.Bottom downto FGridView.gList.Selection.Top+1 do
    begin
      if FGridView.GotoRowNumber(aRow-1) then
        begin
          aLink := Data.BuildLink(FGridView.DataSet.DataSet);
          if FGridView.GotoRowNumber(aRow) then
            begin
             aTask := TTask.Create(nil);
             aTask.Select(FGridView.DataSet.Id.AsVariant);
             aTask.Open;
             if aTask.Count > 0 then
               begin
                 aTask.Dependencies.Add(aLink);
               end;
             aTask.Free;
            end
          else break;
        end
      else break;
    end;
  acRefresh.Execute;
end;
procedure TfTaskFrame.acMAkeSubTaskExecute(Sender: TObject);
begin
  FGridView.SetChild;
end;

procedure TfTaskFrame.acMarkProblemExecute(Sender: TObject);
var
  aRow: Integer;
  aProject: TProject;
begin
  for aRow := FGridView.gList.Selection.Bottom+1 downto FGridView.gList.Selection.Top+1 do
    begin
      if FGridView.GotoRowNumber(aRow-1) then
        begin
          if not FDataSet.CanEdit then
            FDataSet.DataSet.Edit;
          if FDataSet.DataSet.FieldByName('NEEDSACTION').AsString<>'Y' then
            begin
              FDataSet.DataSet.FieldByName('NEEDSACTION').AsString:='Y'
            end
          else
            begin
              FDataSet.DataSet.FieldByName('NEEDSACTION').AsString:='N';
            end;
          if FDataSet.CanEdit then
            FDataSet.DataSet.Post;
          if FDataSet.DataSet.FieldByName('PROJECTID').AsString<>'' then
            begin
              aProject := TProject.CreateEx(Self,Data,Connection);
              aProject.Select(FDataSet.DataSet.FieldByName('PROJECTID').AsVariant);
              aProject.Open;
              if (aProject.Count>0) then
                begin
                  aProject.CheckNeedsAction;
                end;
              aProject.Free;
            end;
        end
      else break;
    end;
  FGridView.Refresh;
end;

procedure TfTaskFrame.acMarkSeenExecute(Sender: TObject);
var
  aRow: LongInt;
begin
  for aRow := FGridView.gList.Selection.Bottom+1 downto FGridView.gList.Selection.Top+1 do
    begin
      if FGridView.GotoRowNumber(aRow-1) then
        begin
         if (DataSet.FieldByName('USER').AsString = aUsers.FieldByName('ACCOUNTNO').AsString)
         or (DataSet.FieldByName('USER').IsNull) then
           begin
             if not FDataSet.CanEdit then
               FDataSet.DataSet.Edit;
             FDataSet.DataSet.FieldByName('SEEN').AsString:='Y';
             if FDataSet.CanEdit then
               FDataSet.DataSet.Post;
           end;
        end
      else break;
    end;
  FGridView.Refresh;
end;

procedure TfTaskFrame.acMoveToProjectExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.AllowSearchTypes(strProjects);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItemMulti;
  fSearch.Execute(True,'TASKSP',strSearchFromTasks);
  fSearch.SetLanguage;
end;

procedure TfTaskFrame.acOpenExecute(Sender: TObject);
var
  FTaskEdit: TfTaskEdit;
begin
  FGridView.GotoActiveRow;
  if DataSet is TProjectTasks then
    FTaskEdit := TfTaskEdit.Create(nil,TProjectTasks(DataSet).Project.Connection)
  else
    FTaskEdit := TfTaskEdit.Create(Self);
  if FTaskEdit.Execute(Data.BuildLink(FGridView.DataSet.DataSet)) then
    begin
      DataSet.Change;
      FGridView.Refresh;
    end
  else if acMarkSeen.Visible then
    acMarkSeen.Execute;
  if Assigned(FTaskEdit) then
    FTaskEdit.Free;
end;
procedure TfTaskFrame.acPrintExecute(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  fSelectReport.Report := Report;
  fSelectReport.SetLanguage;
  MandantDetails.DataSet := Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  if Supports(FDataSet, IBaseHistory, Hist) then
    History.DataSet := Hist.GetHistory.DataSet;
  PList.DataSet := DataSet.DataSet;
  Users.DataSet := Data.Users.DataSet;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      fSelectReport.ReportType := 'PRT';
    end;
  fSelectReport.Showmodal;
end;
procedure TfTaskFrame.acRefreshExecute(Sender: TObject);
begin
  FGridView.Refresh(True);
  RefreshTasks(FTaskNode);
  pBottom.Caption:=Format(strRecordCount,[DataSet.Count]);
end;
procedure TfTaskFrame.acFilterRightsExecute(Sender: TObject);
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

procedure TfTaskFrame.acSaveExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicPost;
      if UseTransactions then
        begin
          Data.CommitTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
      acSave.Enabled:=False;
      acCancel.Enabled:=False;
    end;
  acRefresh.Execute;
end;

procedure TfTaskFrame.acSaveFilterExecute(Sender: TObject);
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
      Data.Filters.Open;
      if Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive,loPartialKey]) then
        Data.Filters.DataSet.Edit
      else
        begin
          Data.Filters.DataSet.Append;
          cbFilter.Items.Add(cbFilter.text);
        end;
      with Data.Filters.DataSet do
        begin
          FieldByName('TYPE').AsString := FFilterType;
          FieldByName('NAME').AsString := cbFilter.Text;
          FieldByName('FILTER').AsString := eFilterEdit.Lines.Text;
          FieldByName('FILTERIN').AsString := eFilterIn.Text;
          FieldByName('USER').Clear;
          Post;
        end;
    end;
end;
procedure TfTaskFrame.acSearchExecute(Sender: TObject);
begin
end;

procedure TfTaskFrame.acSetOwnerExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.AllowSearchTypes(strUsers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenOwnerItem;
  fSearch.Execute(True,'TASKSU',strSearchFromTasks);
  fSearch.SetLanguage;
end;

procedure TfTaskFrame.acSetUserExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.AllowSearchTypes(strUsers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenUserItem;
  fSearch.Execute(True,'TASKSU',strSearchFromTasks);
  fSearch.SetLanguage;
end;

procedure TfTaskFrame.acStartTimeExecute(Sender: TObject);
var
  aProject: TProject;
  tmp: String;
begin
  if FGridView.GotoActiveRow then
    begin
      try
        aProject := TProject.Create(nil);
        aProject.Select(DataSet.FieldByName('PROJECTID').AsVariant);
        aProject.Open;
        {
        if FileExists(GetTempDir+'PMSTimeregistering') and (not Assigned(OnStartTime)) then
          begin
            tmp := 'Time.enter('+Data.BuildLink(aProject.DataSet)+';'+Data.BuildLink(DataSet.DataSet)+';)';
            SendIPCMessage(tmp,GetTempDir+'PMSTimeregistering');
            SendIPCMessage('Time.start',GetTempDir+'PMSTimeregistering');
          end;}
        if Assigned(OnStartTime) then
          OnStartTime(Self,Data.BuildLink(aProject.DataSet),Data.BuildLink(DataSet.DataSet),DataSet.FieldByName('CATEGORY').AsString);
        aProject.Free;
      except
      end;
      if FGridView.GotoActiveRow and DataSet.FieldByName('STARTEDAT').IsNull then
        begin
          if not DataSet.CanEdit then DataSet.DataSet.Edit;
          DataSet.FieldByName('STARTEDAT').AsDateTime:=Now();
          DataSet.DataSet.Post;
          acMarkSeen.Execute;
        end;
    end;
end;
procedure TfTaskFrame.acStopTimeExecute(Sender: TObject);
begin
  if not FGridView.GotoActiveRow then exit;
  if not DataSet.CanEdit then
    DataSet.DataSet.Edit;
  DataSet.FieldByName('COMPLETED').AsString:='Y';
  DataSet.DataSet.Post;
end;

procedure TfTaskFrame.acTerminateExecute(Sender: TObject);
var
  aDeps: TStrings;
  i: Integer;
begin
  if not FGridView.GotoActiveRow then exit;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  aDeps := TTaskList(DataSet).GetUnterminatedDependencies;
  if aDeps.Count>0 then
    begin
      for i := 0 to aDeps.Count-1 do
        aDeps[i] := Data.GetLinkDesc(aDeps[i]);
      Showmessage(Format(strUnterminatedDependencies,[aDeps.Text]));
    end
  else
    begin
      if not TTaskList(DataSet).Terminate(Now()) then
        Showmessage(strFailed);
    end;
  aDeps.Free;
  Screen.Cursor:=crDefault;
  Refresh;
end;

procedure TfTaskFrame.ActiveSearchEndSearch(Sender: TObject);
begin
  if not ActiveSearch.Active then
    begin
      if not ActiveSearch.NewFound then
        begin
          ActiveSearch.Start(ActiveSearch.SearchString,ActiveSearch.NextSearchLevel);
          exit;
        end;
      if (ActiveSearch.Count=0) and (lbResults.Items.Count=0) then
        begin
          if pSearch.Visible then
            FGridView.EndUpdate;
          pSearch.Visible:=False;
        end;
    end;
end;
procedure TfTaskFrame.ActiveSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPrio: Integer;
  aItem: TBaseDBList);
begin
  with pSearch do
    begin
      if not Visible then
        begin
          Visible := True;
          FGridView.beginUpdate;
        end;
    end;
  if aActive then
    if lbResults.Items.IndexOf(aName)=-1 then
      lbResults.Items.AddObject(aName,TLinkObject.Create(aLink));
end;
procedure TfTaskFrame.acUnmakeSubTaskExecute(Sender: TObject);
begin
  FGridView.UnsetChild;
end;
procedure TfTaskFrame.bEditFilterClick(Sender: TObject);
var
  Animate: TAnimationController;
begin
  Animate := TAnimationController.Create(pFilterOptions);
  bEditFilter.Enabled:=False;
  Application.ProcessMessages;
  if bEditFilter.Down then
    begin
      if Data.Users.Rights.Right('EDITFILTER') > RIGHT_READ then
        Animate.AnimateControlHeight(143)
      else
        Animate.AnimateControlHeight(37);
    end
  else
    Animate.AnimateControlHeight(0);
  bEditFilter.Enabled:=True;
  Animate.Free;
end;
procedure TfTaskFrame.cbFilterSelect(Sender: TObject);
var
  aControl: TControl;
  i: Integer;
begin
  with Application as IBaseDBInterface do
    begin
      eFilterIn.Text:='';
      if cbFilter.ItemIndex=0 then
        Filter := ''
      else
        begin
          if not Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive]) then
            with Data.Filters.DataSet,Data.Filters.DataSet as IBaseDBFilter do
              begin
                Filter := '';
                Open;
                if not Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive]) then
                  raise Exception.CreateFmt(strFilterNotFound,[cbFilter.Text]);
              end;
          FilterIn := Data.Filters.FieldByName('FILTERIN').AsString;
          eFilterIn.Text:=FilterIn;
          Filter := Data.Filters.FieldByName('FILTER').AsString;
//          SortField := Data.Filters.FieldByName('SORTFIELD').AsString;
//          if Data.Filters.FieldByName('SORTDIR').AsString = 'DESC' then
//            SortDirection := sdDescending
//          else if Data.Filters.FieldByName('SORTDIR').AsString = 'ASC' then
//            SortDirection := sdAscending
//          else
//            SortDirection := sdIgnored;
          FDataSet.Open;
        end;
      if Parent is TTabSheet then
        begin
          TTabSheet(Parent).Caption:=cbFilter.Text;
//          if cbFilter.Text = '' then
//            TTabSheet(Parent).Caption:=TfFilterTabs(TTabSheet(Parent).PageControl.Parent).TabNames;
        end;
      DoFilterFocus;
//      UpdateTitle;
//      if (bFilter.Focused and bFilter.Visible) or (not bFilter.Visible) then
      bOldTasks.Enabled:=cbFilter.ItemIndex=0;
      bOldTasks1.Enabled:=cbFilter.ItemIndex=0;
      bDependencies.Enabled:=cbFilter.ItemIndex=0;
      bDependencies1.Enabled:=cbFilter.ItemIndex=0;
      bDelegated.Enabled:=cbFilter.ItemIndex=0;
      bDelegated1.Enabled:=cbFilter.ItemIndex=0;
      bFuture.Enabled:=cbFilter.ItemIndex=0;
      bFuture1.Enabled:=cbFilter.ItemIndex=0;
      if not bOldTasks.Enabled then bOldTasks.Down:=False;
      if not bOldTasks1.Enabled then bOldTasks1.Down:=False;
      if not bDependencies.Enabled then bDependencies.Down:=False;
      if not bDependencies1.Enabled then bDependencies1.Down:=False;
      if not bDelegated.Enabled then bDelegated.Down:=False;
      if not bDelegated1.Enabled then bDelegated1.Down:=False;
      if not bFuture.Enabled then bFuture.Down:=False;
      if not bFuture1.Enabled then bFuture1.Down:=False;
      acFilter.Execute;
    end;
end;
procedure TfTaskFrame.ChangeVisibleRows(Sender: TObject);
begin
  acFilter.Execute;
end;
procedure TfTaskFrame.DatasourceDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if Field.FieldName = 'PROJECT' then
    begin
      if trim(Field.AsString) = '' then
        FDataSet.FieldByName('PROJECTID').Clear;
    end;
end;
procedure TfTaskFrame.DatasourceStateChange(Sender: TObject);
begin
  acDelPos.Enabled := acAddPos.Enabled and (FGridView.Count > 0);
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfTaskFrame.eFilterEditChange(Sender: TObject);
begin
  ParseForms(eFilterEdit.Lines.Text);
end;
procedure TfTaskFrame.FGridViewAddRow(Sender: TObject);
begin
  if not Assigned(FDataSet) then exit;
  if not FDataSet.DataSet.Active then exit;
  if TExtStringGrid(Sender).ColCount<1 then exit;
  try
    if not Assigned(TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row]) then
      TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row] := TRowObject.Create;
    if Assigned(FDataSet.FieldByName('SEEN')) then
      TRowObject(TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row]).Seen:=FDataSet.FieldByName('SEEN').AsString;
    if Assigned(FDataSet.FieldByName('DEPDONE')) then
      TRowObject(TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row]).Dependencies:=FDataSet.FieldByName('DEPDONE').AsString<>'Y';
    TRowObject(TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row]).ShouldStart:=0;
    if FDataSet.FieldByName('PLANTIME').AsString <> '' then
      TRowObject(TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row]).ShouldStart:=FDataSet.FieldByName('DUEDATE').AsDateTime-FDataSet.FieldByName('PLANTIME').AsFloat
    else if (FDataSet.FieldByName('DUEDATE').AsString<>'')  then
      TRowObject(TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row]).ShouldStart:=FDataSet.FieldByName('DUEDATE').AsDateTime-1;
  except
    TExtStringGrid(Sender).Objects[0,TExtStringGrid(Sender).Row] := nil;
  end;
end;
procedure TfTaskFrame.FGridViewAfterInsert(Sender: TObject);
begin
  if FProject <> Unassigned then
    begin
      FDataSet.FieldByName('PROJECTID').AsVariant := FProjectID;
      FDataSet.FieldByName('PROJECT').AsVariant := FProject;
    end;
end;
procedure TfTaskFrame.FGridViewBeforeInsert(Sender: TObject);
begin
  {
  if FDataSet.Count > 0 then
    begin
      FProject := FDataSet.FieldByName('PROJECT').AsVariant;
      FProjectID := FDataSet.FieldByName('PROJECTID').AsVariant;
    end
  else
    begin
      FProject := Null;
      FProjectID := Null;
    end;
  }
end;
procedure TfTaskFrame.FGridViewCellButtonClick(Sender: TObject; Cell: TPoint;
  Field: TColumn);
var
  i : Integer = 0;
begin
  if Field.FieldName = 'PROJECT' then
    begin
      fSearch.AllowSearchTypes(strProjects);
      fSearch.eContains.Clear;
      fSearch.sgResults.RowCount:=1;
      fSearch.OnOpenItem:=@fSearchOpenItem;
      fSearch.Execute(True,'TASKSP',strSearchFromTasks);
      fSearch.SetLanguage;
    end
  else if Field.FieldName = 'USER' then
    begin
      acSetUser.Execute;
    end
  else if Field.FieldName = 'OWNER' then
    begin
      acSetOwner.Execute;
    end
  else if Field.FieldName = 'DUEDATE' then
    begin
      if not Field.Field.IsNull then
        CalendarDialog1.Date:=Field.Field.AsDateTime;
      if CalendarDialog1.Execute then
        begin
          FGridView.BeginUpdate;
          if not DataSet.CanEdit then
            DataSet.DataSet.Edit;
          DataSet.FieldByName('DUEDATE').AsDateTime := CalendarDialog1.Date;
          FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
          FGridView.gList.EditorMode:=False;
          FGridView.EndUpdate;
          FGridView.SetEdited;
        end;
    end
  else if Field.FieldName = 'STARTDATE' then
    begin
      if not Field.Field.IsNull then
        CalendarDialog1.Date:=Field.Field.AsDateTime;
      if CalendarDialog1.Execute then
        begin
          FGridView.BeginUpdate;
          if not DataSet.CanEdit then
            DataSet.DataSet.Edit;
          DataSet.FieldByName('STARTDATE').AsDateTime := CalendarDialog1.Date;
          FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
          FGridView.gList.EditorMode:=False;
          FGridView.EndUpdate;
          FGridView.SetEdited;
        end;
    end
  ;
end;

procedure TfTaskFrame.FGridViewCellChanged(Sender: TObject; NewCell,
  OldCell: TPoint);
var
  aCell: TColumn;
  aCol: Integer;
  aColCount: Integer;
begin
  acSetOwner.Visible:=False;
  acSetUser.Visible:=False;
  aCol := FGridView.gList.Col-1;
  aColCount := FGridView.dgFake.Columns.Count;
  if (aCol>=aColCount) or (aCol<0) then exit;
  try
    aCell := FGridView.dgFake.Columns[aCol];
    if Assigned(aCell) then
      begin
        acSetOwner.Visible := aCell.FieldName = 'OWNER';
        acSetUser.Visible := aCell.FieldName = 'USER';
      end;
  except
  end;
end;

procedure TfTaskFrame.FGridViewCheckBoxColumnToggle(Field: TColumn);
begin
  Screen.Cursor:=crHourGlass;
  if (Field.FieldName = 'COMPLETED') and (DataSet.CanEdit) then
    DataSet.DataSet.Post;
  if (Field.FieldName = 'CHECKED') and (DataSet.CanEdit) then
    DataSet.DataSet.Post;
  Screen.Cursor:=crDefault;
end;
procedure TfTaskFrame.FGridViewDblClick(Sender: TObject);
begin
  acOpen.Execute;
end;

procedure TfTaskFrame.FGridViewDelete(Sender: TObject);
begin
  acDelPos.Execute;
end;

function TfTaskFrame.FGridViewDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
var
  oDate: TDateTime;
  i: Integer;
  aRect : TRect;
  aFont: TFont;
  aColor: TColor;
begin
  Result := False;
  TExtStringGrid(Sender).Canvas.Font.Style:=[];
  if Column.FieldName = 'SUMMARY' then
    begin
      if not Assigned(TExtStringGrid(Sender).Objects[Column.Index,DataCol]) then
        begin
          aFont := TFont.Create;
          TExtStringGrid(Sender).Objects[Column.Index,DataCol] := aFont;
          aRect := Rect;
          if Assigned(TExtStringGrid(Sender).Objects[0,DataCol]) and (TExtStringGrid(Sender).Objects[0,DataCol] is TRowObject) then
            begin
              if (TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).ShouldStart > 0) and (Now() > TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).ShouldStart) then
                aFont.Color := $0003C7A;
              if TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).NeedsAction='na' then
                begin
                  if FGridView.GotoRowNumber(DataCol) then
                    TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).NeedsAction:=TExtDBGrid(Column.Grid).DataSource.DataSet.FieldByName('NEEDSACTION').AsString;
                end;
            end;
          for i := 1 to TExtStringGrid(Sender).ColCount-1 do
            begin
              if TExtDBGrid(Column.Grid).Columns[i-1].FieldName = 'DUEDATE' then
                begin
                  if (trim(TExtStringGrid(Sender).Cells[i,DataCol]) <> '') and TryStrToDateTime(TExtStringGrid(Sender).Cells[i,DataCol],oDate) then
                    if oDate < Now() then
                      begin
                        if aFont.Color<>clGrayText then
                          aFont.Color := clred;
                      end;
                end;
              if TExtDBGrid(Column.Grid).Columns[i-1].FieldName = 'STARTDATE' then
                begin
                  if (trim(TExtStringGrid(Sender).Cells[i,DataCol]) <> '') and TryStrToDateTime(TExtStringGrid(Sender).Cells[i,DataCol],oDate) then
                    if oDate < Now() then
                      begin
                        aFont.Style := aFont.Style+[fsItalic];
                      end;
                end;
              if TExtDBGrid(Column.Grid).Columns[i-1].FieldName = 'COMPLETED' then
                begin
                  if TExtStringGrid(Sender).Cells[i,DataCol] = 'Y' then
                    begin
                      aFont.Color:=clGrayText;
                      aFont.Style:=aFont.Style+[fsStrikeOut];
                    end;
                end;
            end;
          if Assigned(TExtStringGrid(Sender).Objects[0,DataCol]) and (TExtStringGrid(Sender).Objects[0,DataCol] is TRowObject) then
            begin
              if TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).Seen <> 'Y' then
                aFont.Style:=aFont.Style+[fsBold];
              if TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).Dependencies then
                begin
                  aFont.Color:=clGrayText;
                end;
            end;
        end;
      if Assigned(TExtStringGrid(Sender).Objects[Column.Index,DataCol]) and (TExtStringGrid(Sender).Objects[Column.Index,DataCol] is TFont) then
        begin
          aFont := TFont(TExtStringGrid(Sender).Objects[Column.Index,DataCol]);
          TExtStringGrid(Sender).Canvas.Font.assign(aFont);
        end;
      if Assigned(TExtStringGrid(Sender).Objects[0,DataCol]) and (TExtStringGrid(Sender).Objects[0,DataCol] is TRowObject) then
        begin
          if TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).NeedsAction='Y' then
            begin
              fVisualControls.Images.Draw(TExtStringGrid(Sender).Canvas,Rect.Left-14,rect.Top,117);
            end;
        end;
    end
end;
procedure TfTaskFrame.FGridViewFilterCell(Sender: TObject; aCol: TColumn;
  aText: string; var NewText: string);
begin

end;
procedure TfTaskFrame.FGridViewGetCellText(Sender: TObject; aCol: TColumn;aRow : Integer;
  var NewText: string;aFont : TFont);
var
  aUser: String;
  nF: Extended;
begin
  if aCol.FieldName = 'OWNER' then
    begin
      aUser := StringReplace(NewText,'=','',[rfReplaceAll]);
      if aUser = '' then exit;
      if FOwners.Values[aUser] <> '' then
        NewText := FOwners.Values[aUser]
      else
        begin
          if FGridView.GotoRow(TRowObject(FGridView.gList.Objects[0,aRow]).Rec) then
            begin
              FOwners.Values[aUSer] := TTaskList(FDataSet).OwnerName;
              NewText:=FOwners.Values[aUser];
            end;
        end;
    end
  else  if aCol.FieldName = 'USER' then
    begin
      aUser := StringReplace(NewText,'=','',[rfReplaceAll]);
      if aUser = '' then exit;
      if trim(FUsers.Values[aUser]) <> '' then
        NewText := FUsers.Values[aUser]
      else
        begin
          if FGridView.GotoRow(TRowObject(FGridView.gList.Objects[0,aRow]).Rec) then
            begin
              FUsers.Values[aUser] := TTaskList(FDataSet).UserName;
              if trim(FUsers.Values[aUser]) <> '' then
                NewText:=FUsers.Values[aUser];
            end;
        end;
    end
  else  if (aCol.FieldName = 'PLANTIME') or (aCol.FieldName = 'BUFFERTIME') or (aCol.FieldName = 'TIME') then
    begin
      if TryStrToFloat(NewText,nF) then
        NewText := DayTimeToStr(nF);
    end;
end;
function TfTaskFrame.FGridViewSearchKey(Sender: TObject; X, Y: Integer;
  Field: TColumn; var Key: Word; Shift: TShiftState; SearchString: string) : Boolean;
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
  tmp: TCaption;
begin
  Result := False;
  if Assigned(Field) and ((Field.FieldName='PROJECT') or (Field.FieldName='USER') or (Field.FieldName='OWNER')) then
    begin
      if SearchString = '' then
        begin
          if pSearch.Visible then
            begin
              case Key of
              VK_PRIOR,
              VK_UP:
                begin
                  if lbResults.ItemIndex = -1 then
                    begin
                      lbResults.ItemIndex:=0;
                      if pSearch.Visible then
                        FGridView.EndUpdate;
                      pSearch.Visible := False;
                    end
                  else
                    begin
                      lbResults.ItemIndex:=lbResults.ItemIndex-1;
                      Key := 0;
                    end;
                end;
              VK_NEXT,
              VK_DOWN:
                begin
                  if lbResults.ItemIndex = -1 then
                    lbResults.ItemIndex:=0
                  else
                  if lbResults.ItemIndex < lbResults.Count-1 then
                    lbResults.ItemIndex:=lbResults.ItemIndex+1;
                  Key := 0;
                end;
              VK_RETURN:
                begin
                  Screen.Cursor:=crHourGlass;
                  Application.QueueAsyncCall(@DoInsertInplaceSearch,0);
                  Result := True;
                  Key := 0;
                end;
              VK_ESCAPE:
                begin
                  if pSearch.Visible then
                    FGridView.EndUpdate;
                  pSearch.Visible:=False;
                  Key := 0;
                end;
              end;
            end;
        end
      else
        begin
          if not pSearch.Visible then
            begin
              if pToolbar.Visible then
                pSearch.Left:=pToolbar.Width+X
              else pSearch.Left:=X;
              pSearch.Top:=Y;
              if tbTop.Visible then
                pSearch.Top:=pSearch.Top+FGridView.gHeader.Height;
            end;
          if Assigned(ActiveSearch) then
            ActiveSearch.Abort;
          SearchTypes := SearchTypes+[fsShortnames];
          SearchTypes := SearchTypes+[fsIdents];
          SearchTypes := SearchTypes+[fsDescription];
          SetLength(SearchLocations,length(SearchLocations)+1);
          if (Field.FieldName='PROJECT') then
            SearchLocations[length(SearchLocations)-1] := strProjects;
          if (Field.FieldName='USER') or (Field.FieldName='OWNER') then
            begin
              pSearch.Caption := Field.FieldName;
              SearchLocations[length(SearchLocations)-1] := strUsers;
            end;
          lbResults.Items.Clear;
          if Assigned(ActiveSearch) then
            ActiveSearch.Free;
          ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
          ActiveSearch.Sender := TComponent(Sender);
          ActiveSearch.OnItemFound:=@ActiveSearchItemFound;
          ActiveSearch.OnEndSearch:=@ActiveSearchEndSearch;
          ActiveSearch.Start(SearchString);
          Application.ProcessMessages;
        end;
    end;
end;
procedure TfTaskFrame.FGridViewSetCellText(Sender: TObject; aCol: TColumn;
  aRow: Integer; var NewText: string);
begin
  if (aCol.FieldName = 'PLANTIME') or (aCol.FieldName = 'BUFFERTIME') then
    begin
      if trim(NewText) <> '' then
        NewText:=FloatToStr(StrToDayTime(NewText));
    end;
end;
procedure TfTaskFrame.acAddPosExecute(Sender: TObject);
begin
  FGridView.InsertAfter(True);
end;

procedure TfTaskFrame.acAddTasksfromProjectExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.AllowSearchTypes(strProjects);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchAddTasksToProject;
  fSearch.Execute(True,'TASKSP',strSearchFromTasks);
  fSearch.SetLanguage;
end;

procedure TfTaskFrame.acAppendLinkToDependenciesExecute(Sender: TObject);
var
  Stream: TStringStream;
  aLinks: String;
  aLink: String;
begin
  if FGridView.GotoActiveRow then
    begin
      Stream := TStringStream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          Stream.Position:=0;
          aLinks := Stream.DataString;
        end
      else
        fError.ShowWarning(strCantgetClipboardContents);
      Stream.Free;
      TTaskList(FDataSet).Dependencies.Open;
      while pos(';',aLinks) > 0 do
        begin
          aLink := copy(aLinks,0,pos(';',aLinks)-1);
          TTaskList(FDataSet).Dependencies.Add(aLink);
          aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
        end;
    end;
end;

procedure TfTaskFrame.acCancelExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicCancel;
      if UseTransactions then
        begin
          Data.RollbackTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
  acRefresh.Execute;
end;

procedure TfTaskFrame.acDefaultFilterExecute(Sender: TObject);
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
procedure TfTaskFrame.acDeleteFilterExecute(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    begin
      if Data.Filters.DataSet.Locate('TYPE;NAME',VarArrayOf([FFilterType,cbFilter.Text]),[loCaseInsensitive,loPartialKey]) then
        if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
          begin
            Data.Filters.DataSet.Delete;
            cbFilter.Items.Delete(cbFilter.ItemIndex);
          end;
    end;
end;
procedure TfTaskFrame.SetBaseName(AValue: string);
begin
  FGridView.BaseName:=AValue;
end;
procedure TfTaskFrame.SetBaseFilter(AValue: string);
begin
  if FBaseFilter=AValue then Exit;
  FBaseFilter:=AValue;
end;
constructor TfTaskFrame.Create(AOwner: TComponent);
var
  aItem: TMenuItem;
begin
  inherited Create(AOwner);
  FGridView := TfGridView.Create(Self);
  FGridView.Parent := Self;
  FIgnoreUser:=False;
  FGridView.Align:=alClient;
  FGridView.DefaultRows:='GLOBALWIDTH:%;COMPLETED:30;SUMMARY:200;PROJECT:150;STARTDATE:60;PLANTIME:30;DUEDATE:60;USER:100;OWNER:100';
  BaseName := 'TASKS';
  FGridView.TreeField := 'PARENT';
  FGridView.IdentField := 'SUMMARY';
  FGridView.HasChildsField:='HASCHILDS';
  FGridView.ExpandedField:='EXPANDED';
  FGridView.WorkStatusField:='WORKSTATUS';
  FGridView.OnSetupPosition:=@FGridViewSetupPosition;
  FGridView.OnCellButtonClick:=@FGridViewCellButtonClick;
  FGridView.OnCellChanged:=@FGridViewCellChanged;
  FgridView.OnDblClick:=@FGridViewDblClick;
  FGridView.OnGetCellText:=@FGridViewGetCellText;
  FGridView.OnSetCellText:=@FGridViewSetCellText;
  FOwners := TStringList.Create;
  FUsers := TStringList.Create;
  FGridView.gList.Options:=FGridView.gList.Options+[goRangeSelect];
  FGridView.BeforeInsert:=@FGridViewBeforeInsert;
  FGridView.AfterInsert:=@FGridViewAfterInsert;
  FGridView.OnSearchKey:=@FGridViewSearchKey;
  FGridView.OnDrawColumnCell:=@FGridViewDrawColumnCell;
  FGridView.FilterRow:=True;
  FGridView.OnFilterCell:=@FGridViewFilterCell;
  FGridView.OnAddRow:=@FGridViewAddRow;
  FGridView.OnDelete:=@FGridViewDelete;
  FGridView.gList.PopupMenu := pmGrid;
  miCopyLink.Action := FGridView.acCopyLink;
  pFilterOptions.Height:=0;
  CalendarDialog1.Date:=Now();
end;
destructor TfTaskFrame.Destroy;
var
  i: Integer;
begin
  for i := 0 to lbResults.Items.Count-1 do
    lbResults.Items.Objects[i].Free;
  if Assigned(ActiveSearch) then ActiveSearch.Free;
  FreeAndNil(aUsers);
  FOwners.Free;
  FUsers.Free;
  FGridView.Destroy;
  inherited Destroy;
end;
procedure TfTaskFrame.SetRights(Editable: Boolean);
begin
  FGridView.SetRights(Editable);
  acFilterRights.Enabled:=Data.Users.Rights.Right('EDITFILTER') >= RIGHT_PERMIT;
  acDeleteFilter.Enabled:=Data.Users.Rights.Right('EDITFILTER') >= RIGHT_DELETE;
  ArrangeToolBar(pToolbar,ActionList1,'Tasks');
end;
procedure TfTaskFrame.ShowFrame;
begin
  FGridView.Refresh;
  if FGridView.CanFocus and Visible then
    FGridView.SetFocus;
end;

procedure TfTaskFrame.DoRefresh;
begin
  //FGridView.Refresh;
end;
procedure TfTaskFrame.SetLanguage;
begin

end;
procedure TfTaskFrame.Post;
begin
  FGridView.Post;
end;
procedure AddToMainTree(aAction: TAction;var aNode : TTreeNode);
var
  Node: TTreeNode;
  Node1: TTreeNode;
  aDataSet: TTaskList;
  aOwnUser: String;
  procedure CollectUsers(aParent : Variant);
  var
    aUsers: TUser;
    bParent : Variant;
  begin
    if aParent = Null then exit;
    aUsers := TUser.Create(nil);
    with aUsers.DataSet as IBaseDbFilter do
      begin
        SortFields := 'NAME';
        Limit:=0;
        SortDirection:=sdAscending;
      end;
    Data.SetFilter(aUsers,Data.QuoteField('PARENT')+'='+Data.QuoteValue(aParent));
    aUsers.DataSet.First;
    while not aUsers.EOF do
      begin
        if (aUsers.FieldByName('TYPE').AsString <> 'G') then
          begin
            if ((aUsers.FieldByName('LEAVED').AsString='') or (not (aUsers.FieldByName('LEAVED').AsDateTime < Now())))
            and (Data.Users.FieldByName('ACCOUNTNO').AsString <> aUsers.FieldByName('ACCOUNTNO').AsString)
            then
              begin
                Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
                TTreeEntry(Node1.Data).Typ := etTaskUser;
                TTreeEntry(Node1.Data).Text[0] := aUsers.FieldByName('NAME').AsString;
                TTreeEntry(Node1.Data).Rec := aUsers.GetBookmark;
                TTreeEntry(Node1.Data).DataSource := Data.Users;
              end;
          end
        else
          begin
            bParent := aUsers.FieldByName('SQL_ID').AsVariant;
            if bParent <> aParent then
              CollectUsers(bParent);
          end;
        aUsers.Next;
      end;
    aUsers.Free;
  end;

begin
  if (Data.Users.Rights.Right('TASKS') > RIGHT_NONE) then
    begin
      Node := aNode;
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := aAction;
      fMainTreeFrame.StartupTypes.Add(strTasks);
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Typ := etTaskPlan;
      if Data.Users.FieldByName('POSITION').AsString = 'LEADER' then
        begin
          CollectUsers(Data.Users.FieldByName('PARENT').AsVariant);
        end;
    end;
end;
procedure RefreshTasks(FNode: TTreeNode);
var
  aDataSet: TTaskList;
begin
  if not Assigned(FNode) then exit;
  if Assigned(TTreeEntry(FNode.Data).SubText) then
    TTreeEntry(FNode.Data).SubText.Free;
  TTreeEntry(FNode.Data).SubText := TStringlist.Create;
  aDataSet := TTaskList.Create(nil);
  aDataSet.CreateTable;
  aDataSet.SelectActiveByUser(Data.Users.FieldByName('ACCOUNTNO').AsString);
  aDataSet.Open;
  if (aDataSet.Count > 0) and (aDataSet.Count < 251) then
    TTreeEntry(FNode.Data).SubText.Add(Format(strAssignedTasks,[IntToStr(aDataSet.Count)]))
  else if (aDataSet.Count > 0) then
    TTreeEntry(FNode.Data).SubText.Add(Format(strAssignedTasks,['>250']));
  aDataSet.Open;
  aDataSet.Free;
end;
initialization
end.

