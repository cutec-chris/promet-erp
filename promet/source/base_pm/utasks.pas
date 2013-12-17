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
  LResources, Forms, Controls, DBGrids, ValEdit, ExtCtrls, Buttons, ComCtrls,
  uPrometFramesInplaceDB, uExtControls, db, Grids, ActnList, Menus, StdCtrls,
  simpleipc, uBaseDBClasses, uBaseDbInterface, uGridView, uIntfStrConsts,
  Variants, uBaseSearch, Graphics, Spin, EditBtn, Dialogs;
type
  TOnStartTime = procedure(Sender : TObject;aProject,aTask : string) of object;

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
    acUnmakeSubTask: TAction;
    ActionList: TActionList;
    ActionList1: TActionList;
    bAddPos: TSpeedButton;
    bAddPos1: TSpeedButton;
    bDelegated2: TSpeedButton;
    bDeletePos8: TSpeedButton;
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
    bFilter: TBitBtn;
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
    cbFilter: TComboBox;
    cbMaxResults: TCheckBox;
    Datasource: TDatasource;
    ExtRotatedLabel5: TExtRotatedLabel;
    Panel9: TPanel;
    pBottom: TPanel;
    PUsers: TfrDBDataSet;
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
    IPC: TSimpleIPCClient;
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
    sbSave2: TSpeedButton;
    sbSavePublic: TSpeedButton;
    seMaxresults: TSpinEdit;
    SynSQLSyn2: TSynSQLSyn;
    tbLeft: TPanel;
    ToolBar: TToolBar;
    tbTop: TPanel;
    procedure acAddPosExecute(Sender: TObject);
    procedure acDefaultFilterExecute(Sender: TObject);
    procedure acDeleteFilterExecute(Sender: TObject);
    procedure acDelPosExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acGotoProjectExecute(Sender: TObject);
    procedure acLinkExecute(Sender: TObject);
    procedure acMAkeSubTaskExecute(Sender: TObject);
    procedure acMarkSeenExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acFilterRightsExecute(Sender: TObject);
    procedure acSaveFilterExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure acStartTimeExecute(Sender: TObject);
    procedure acStopTimeExecute(Sender: TObject);
    procedure ActiveSearchEndSearch(Sender: TObject);
    procedure ActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string;aActive : Boolean; aLink: string; aItem: TBaseDBList=nil);
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
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchOpenOwnerItem(aLink: string): Boolean;
    function fSearchOpenUserItem(aLink: string): Boolean;
    procedure lbResultsDblClick(Sender: TObject);
    procedure DoInsertInplaceSearch(Data : PtrInt);
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
    procedure seMaxresultsChange(Sender: TObject);
  private
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
implementation
uses uRowEditor,uTask,ubasevisualapplicationtools,uData,uMainTreeFrame,
  uSearch,uProjects,uTaskEdit,uBaseApplication,LCLType,uBaseERPDBClasses,
  uSelectReport,uFormAnimate,md5,uNRights;
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
  aUsers := TUser.Create(nil,Data);
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
      cbFilter.Text := aFilter;
      cbFilterSelect(nil);
    end
  else
    ChangeVisibleRows(bOldTasks);
  aUser := TUser.Create(Self,Data);
  aUser.Select(fUserID);
  aUser.Open;
  if aUser.Count>0 then
    TTaskList(FDataSet).UserId:=aUser.FieldByName('ACCOUNTNO').AsString;
  aUser.Free;
  acMarkSeen.Visible:=not (AValue is TProjectTasks);
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
  if bFilter.Visible then
    bFilter.SetFocus;
end;
procedure TfTaskFrame.SetUserID(AValue: Variant);
begin
  if FUserID=AValue then Exit;
  FUserID:=AValue;
end;
function DayTimeToStr(nf: Real): string;
begin
  if nf < 1 then
    Result := FormatFloat('0.0',nf*GetHoursPerDay)+'h'
  else Result := FloatToStr(nf);
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
      if TryStrToFloat(copy(trim(aStr),0,length(trim(aStr))-1),Result) then
        Result := Result/GetHoursPerDay;
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
      ;
    end;
end;
function TfTaskFrame.fSearchOpenItem(aLink: string): Boolean;
var
  aProject: TProject;
  aCount: Integer;
begin
  Result := False;
  aProject := TProject.Create(Self,Data);
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
      DataSet.FieldByName('PROJECT').AsString := aProject.Text.AsString;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.EndUpdate;
      FGridView.SetEdited;
    end;
  aProject.Free;
end;
function TfTaskFrame.fSearchOpenOwnerItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
begin
  Result := False;
  aUser := TUser.Create(Self,Data);
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
  aUser := TUser.Create(Self,Data);
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
procedure TfTaskFrame.lbResultsDblClick(Sender: TObject);
begin
  if lbResults.ItemIndex < 0 then exit;
  pSearch.Visible:=False;
  Application.ProcessMessages;
  if copy(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link,0,5) = 'USERS' then
    begin
      fSearchOpenUserItem(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
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
  if (DataSet.FieldByName('OWNER').AsString <> aUsers.FieldByName('ACCOUNTNO').AsString)
  and (not DataSet.FieldByName('OWNER').IsNull) then
    exit;
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      FGridView.Delete;
      if DataSet is TProjectTasks then
        if not TProjectTasks(DataSet).Project.CanEdit then
          TProjectTasks(DataSet).Project.DataSet.Edit;
      RefreshTasks(FTaskNode);
      acDelPos.Enabled := acAddPos.Enabled and ((FGridView.Count > 0) and (DataSet.State <> dsInsert));
      FGridView.Refresh;
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
        aBaseFilter += '('+Data.QuoteField('DEPDONE')+'='+Data.QuoteValue('Y')+')';
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
  aProject := TProject.Create(nil,Data);
  aProject.Select(DataSet.FieldByName('PROJECTID').AsVariant);
  aProject.Open;
  if aProject.Count > 0 then
    Data.GotoLink(Data.BuildLink(aProject.DataSet));
  aProject.Free;
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
             aTask := TTask.Create(nil,Data);
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
end;
procedure TfTaskFrame.acMAkeSubTaskExecute(Sender: TObject);
begin
  FGridView.SetChild;
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
procedure TfTaskFrame.acOpenExecute(Sender: TObject);
var
  FTaskEdit: TfTaskEdit;
begin
  FGridView.GotoActiveRow;
  if DataSet is TProjectTasks then
    FTaskEdit := TfTaskEdit.Create(nil,TProjectTasks(DataSet).Project.Connection)
  else
    FTaskEdit := TfTaskEdit.Create(nil);
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
procedure TfTaskFrame.acStartTimeExecute(Sender: TObject);
var
  aProject: TProject;
  tmp: String;
begin
  if FGridView.GotoActiveRow then
    begin
      try
        aProject := TProject.Create(nil,Data);
        aProject.Select(DataSet.FieldByName('PROJECTID').AsVariant);
        aProject.Open;
        if IPC.ServerRunning then
          begin
            IPC.Connect;
            tmp := 'Time.enter('+Data.BuildLink(aProject.DataSet)+';'+Data.BuildLink(DataSet.DataSet)+';)';
            IPC.SendStringMessage(tmp);
            IPC.SendStringMessage('Time.start');
            IPC.Disconnect;
          end;
        if Assigned(OnStartTime) then
          OnStartTime(Self,Data.BuildLink(aProject.DataSet),Data.BuildLink(DataSet.DataSet));
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
procedure TfTaskFrame.ActiveSearchEndSearch(Sender: TObject);
begin
  if not ActiveSearch.Active then
    begin
      if ActiveSearch.Count=0 then
        begin
          if pSearch.Visible then
            FGridView.EndUpdate;
          pSearch.Visible:=False;
        end;
    end;
end;
procedure TfTaskFrame.ActiveSearchItemFound(aIdent: string; aName: string;
  aStatus: string;aActive : Boolean; aLink: string; aItem: TBaseDBList=nil);
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
      if (bFilter.Focused and bFilter.Visible) or (not bFilter.Visible) then
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
      fSearch.SetLanguage;
      while i < fSearch.cbSearchType.Count do
        begin
          if fSearch.cbSearchType.Items[i] <> strProjects then
            fSearch.cbSearchType.Items.Delete(i)
          else
            inc(i);
        end;
      fSearch.eContains.Clear;
      fSearch.sgResults.RowCount:=1;
      fSearch.OnOpenItem:=@fSearchOpenItem;
      fSearch.Execute(True,'TASKSP',strSearchFromTasks);
      fSearch.SetLanguage;
    end
  else if Field.FieldName = 'USER' then
    begin
      fSearch.SetLanguage;
      while i < fSearch.cbSearchType.Count do
        begin
          if fSearch.cbSearchType.Items[i] <> strUsers then
            fSearch.cbSearchType.Items.Delete(i)
          else
            inc(i);
        end;
      fSearch.eContains.Clear;
      fSearch.sgResults.RowCount:=1;
      fSearch.OnOpenItem:=@fSearchOpenUserItem;
      fSearch.Execute(True,'TASKSU',strSearchFromTasks);
      fSearch.SetLanguage;
    end
  else if Field.FieldName = 'OWNER' then
    begin
      fSearch.SetLanguage;
      while i < fSearch.cbSearchType.Count do
        begin
          if fSearch.cbSearchType.Items[i] <> strUsers then
            fSearch.cbSearchType.Items.Delete(i)
          else
            inc(i);
        end;
      fSearch.eContains.Clear;
      fSearch.sgResults.RowCount:=1;
      fSearch.OnOpenItem:=@fSearchOpenOwnerItem;
      fSearch.Execute(True,'TASKSU',strSearchFromTasks);
      fSearch.SetLanguage;
    end
  ;
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

function TfTaskFrame.FGridViewDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState
  ): Boolean;
var
  oDate: TDateTime;
  i: Integer;
  aRect : TRect;
  aFont: TFont;
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
            if (TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).ShouldStart > 0) and (Now() > TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).ShouldStart) then
              aFont.Color := $0003C7A;
          for i := 1 to TExtStringGrid(Sender).ColCount-1 do
            begin
              if TExtDBGrid(Column.Grid).Columns[i-1].FieldName = 'DUEDATE' then
                begin
                  if (trim(TExtStringGrid(Sender).Cells[i,DataCol]) <> '') and TryStrToDateTime(TExtStringGrid(Sender).Cells[i,DataCol],oDate) then
                    if oDate < Now() then
                      begin
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
            if TRowObject(TExtStringGrid(Sender).Objects[0,DataCol]).Seen <> 'Y' then
              aFont.Style:=aFont.Style+[fsBold];
        end;
      if Assigned(TExtStringGrid(Sender).Objects[Column.Index,DataCol]) and (TExtStringGrid(Sender).Objects[Column.Index,DataCol] is TFont) then
        begin
          aFont := TFont(TExtStringGrid(Sender).Objects[Column.Index,DataCol]);
          TExtStringGrid(Sender).Canvas.Font.assign(aFont);
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
  else  if (aCol.FieldName = 'PLANTIME') or (aCol.FieldName = 'BUFFERTIME')then
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
  if Assigned(Field) and ((Field.FieldName='PROJECT') or (Field.FieldName='USER')) then
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
              if tbLeft.Visible then
                pSearch.Left:=tbLeft.Width+X
              else pSearch.Left:=X;
              if tbTop.Visible then
                pSearch.Top:=tbTop.Height+Y
              else
                pSearch.Top:=Y;
              if FGridView.gHeader.Visible then
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
          if (Field.FieldName='USER') then
            SearchLocations[length(SearchLocations)-1] := strUsers;
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
  FGridView.DefaultRows:='GLOBALWIDTH:770;COMPLETED:30;SUMMARY:200;PROJECT:200;STARTDATE:60;DUEDATE:60;USER:100;OWNER:100';
  BaseName := 'TASKS';
  FGridView.TreeField := 'PARENT';
  FGridView.IdentField := 'SUMMARY';
  FGridView.HasChildsField:='HASCHILDS';
  FGridView.ExpandedField:='EXPANDED';
  FGridView.WorkStatusField:='WORKSTATUS';
  FGridView.OnSetupPosition:=@FGridViewSetupPosition;
  FGridView.OnCellButtonClick:=@FGridViewCellButtonClick;
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
  aItem := TMenuItem.Create(FGridView.pmPopup);
  aItem.Action:=acOpen;
  FGridView.pmPopup.Items.Insert(0,aItem);
  aItem := TMenuItem.Create(FGridView.pmPopup);
  aItem.Action:=acMarkSeen;
  FGridView.pmPopup.Items.Add(aItem);
  pFilterOptions.Height:=0;
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
end;
procedure TfTaskFrame.ShowFrame;
begin
  FGridView.Refresh;
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
    aUsers := TUser.Create(nil,Data);
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
  aDataSet := TTaskList.Create(nil,Data);
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
  {$I utasks.lrs}
end.

