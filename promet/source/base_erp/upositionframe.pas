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
unit uPositionFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, LCLType,
  LCLProc, LCLIntf, Graphics, db, uExtControls, DBGrids, ExtCtrls,
  Buttons, ActnList, ComCtrls, Variants, types, uBaseDbClasses,
  uIntfStrConsts,uBaseERPDBClasses, Menus, uPrometFramesInplace, uGridView,
  uBaseSearch;
type
  TUnprotectedFrame = class(TCustomFrame);
  THackCustomGrid = class(TCustomGrid);
  { TfPosition }

  TfPosition = class(TPrometInplaceFrame)
    acAddPos: TAction;
    acDelPos: TAction;
    acGotoArticle: TAction;
    acSearchArticle: TAction;
    acStartTimeregistering: TAction;
    acMakeSubPos: TAction;
    acRefresh: TAction;
    acRenumber: TAction;
    acAddfromArticle: TAction;
    acPermanentEditorMode: TAction;
    acSum: TAction;
    acDiscount: TAction;
    acViewTexts: TAction;
    acViewDetails: TAction;
    acUnMakeSebPos: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel4: TBevel;
    Position: TDatasource;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    lbResults: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OrderPos: TDataSource;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pmPosition: TPopupMenu;
    pSearch: TPanel;
    TabTimer: TIdleTimer;
    pcTabs: TExtMenuPageControl;
    pDetail: TPanel;
    pToolbar: TPanel;
    spDetails: TSplitter;
    tbAnsicht1: TToolBar;
    tbPosition: TToolBar;
    tbAnsicht: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tsDetails: TTabSheet;
    procedure acAddfromArticleExecute(Sender: TObject);
    procedure acAddPosExecute(Sender: TObject);
    procedure acDelPosExecute(Sender: TObject);
    procedure acGotoArticleExecute(Sender: TObject);
    procedure acMakeSubPosExecute(Sender: TObject);
    procedure acPermanentEditorModeExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acRenumberExecute(Sender: TObject);
    procedure acSearchArticleExecute(Sender: TObject);
    procedure acSumExecute(Sender: TObject);
    procedure ActiveSearchEndItemSearch(Sender: TObject);
    procedure ActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string;aActive : Boolean; aLink: string;aPrio : Integer; aItem: TBaseDBList=nil);
    procedure acUnMakeSebPosExecute(Sender: TObject);
    procedure acViewDetailsExecute(Sender: TObject);
    procedure acViewTextsExecute(Sender: TObject);
    procedure AddCalcTab(Sender: TObject);
    procedure PositionDataChange(Sender: TObject; Field: TField);
    procedure PositionStateChange(Sender: TObject);
    procedure DoAsyncInit(Data: PtrInt);
    procedure FDataSourceStateChange(Sender: TObject);
    procedure FGridViewAutoFilterChanged(Sender: TObject);
    procedure FGridViewCellButtonClick(Sender: TObject; Cell: TPoint;
      Field: TColumn);
    procedure FGridViewCellChanging(Sender: TObject);
    procedure FGridViewCellChanged(Sender: TObject; NewCell, OldCell: TPoint);
    function FGridViewSearchKey(Sender: TObject; X, Y: Integer;
      Field: TColumn; var Key: Word; Shift: TShiftState; SearchString: string) : Boolean;
    procedure FGridViewSetupPosition(Sender: TObject;Columns : TGridColumns);
    function fSearchOpenItem(aLink: string): Boolean;
    procedure lbResultsDblClick(Sender: TObject);
    procedure sgPositionsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sgPositionsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure spDetailsMoved(Sender: TObject);
    procedure TabTimerTimer(Sender: TObject);
  private
    { private declarations }
    FFound : Boolean;
    FPosTyp : Integer;
    FBaseName: string;
    FDataset: TBaseDBDataset;
    FFormName: string;
    FGridView : TfGridView;
    FCalculationDisabled : Integer;
    FRefID : Int64;
    FEditAble : Boolean;
    ActiveSearch : TSearch;
    FFirstShow : Boolean;
    procedure SetBaseName(AValue: string);
    procedure SetDataSet(const AValue: TBaseDBDataset);
    function  GetPosTyp : Integer;
    procedure SetFormName(const AValue: string);
    procedure RefreshTabs;
    procedure AddDocumentsTab(Sender: TObject);
  public
    { public declarations }
    InplaceFrames : array[0..8] of TPrometInplaceFrame;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    property  Dataset : TBaseDBDataset read FDataset write SetDataSet;
    property  BaseName : string read FBaseName write SetBaseName;
    property  FormName : string read FFormName write SetFormName;
    procedure DisableCalculation;
    procedure EnableCalculation;
    procedure SyncDataSource;
    procedure ResetEditor;
    procedure Post;
    procedure SetRights(Editable : Boolean);
    procedure AutoInsert;
    procedure SetFocus;override;
    procedure SetLanguage;
    property GridView : TfGridView read FGridView;
  end;
implementation
uses uRowEditor, uSearch, uBaseDbInterface, uOrder, uDocumentFrame, uDocuments,
  uData,uMasterdata,uBaseVisualApplication,uMainTreeFrame,ucalcframe;
{$R *.lfm}
procedure TfPosition.FDataSourceStateChange(Sender: TObject);
begin
  acDelPos.Enabled := acAddPos.Enabled and (FGridView.Count > 0);
end;

procedure TfPosition.FGridViewAutoFilterChanged(Sender: TObject);
begin
  FGridView.AutoFilter:=StringReplace(FGridView.AutoFilter,Data.QuoteField('TEXT'),Data.QuoteField('SHORTTEXT'),[rfReplaceAll]);
end;

procedure TfPosition.FGridViewCellButtonClick(Sender: TObject; Cell: TPoint;
  Field: TColumn);
var
  i: Integer = 0;
  aCount: Integer;
begin
  if Field.FieldName = 'IDENT' then
    begin
      acSearchArticle.Execute;
    end;
end;
procedure TfPosition.FGridViewCellChanging(Sender: TObject);
begin
  if acViewDetails.Checked then
    TabTimer.Enabled:=True;
end;
procedure TfPosition.FGridViewCellChanged(Sender: TObject; NewCell,
  OldCell: TPoint);
begin
  acDelPos.Enabled := acAddPos.Enabled and (FGridView.Count > 0);
end;
function TfPosition.FGridViewSearchKey(Sender: TObject; X, Y: Integer;
  Field: TColumn; var Key: Word; Shift: TShiftState; SearchString: string) : Boolean;
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
  tmp: TCaption;
begin
  Result := False;
  if Assigned(Field) and (((Field.FieldName='TEXT') and ((FDataSet.FieldByName('IDENT').AsString = '') or (FDataSet.FieldByName('TEXT').AsString = ''))) or (Field.FieldName='IDENT')) then
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
                  if (lbResults.ItemIndex = -1) and (lbResults.Count>0) then
                    lbResults.ItemIndex:=0
                  else
                  if lbResults.ItemIndex < lbResults.Count-1 then
                    lbResults.ItemIndex:=lbResults.ItemIndex+1;
                  Key := 0;
                end;
              VK_RETURN:
                begin
                  lbResultsDblClick(nil);
                  Result := FFound;
                  Key := 0;
                end;
              VK_ESCAPE:
                begin
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
              if pToolbar.Align=alLeft then
                pSearch.Left:=pToolbar.Width+X
              else
                pSearch.Left:=X;
              pSearch.Top:=Y;
            end;
          if Assigned(ActiveSearch) then
            ActiveSearch.Abort;
          SearchTypes := SearchTypes+[fsShortnames];
          SearchTypes := SearchTypes+[fsIdents];
          SearchTypes := SearchTypes+[fsMatchcode];
          SetLength(SearchLocations,length(SearchLocations)+1);
          SearchLocations[length(SearchLocations)-1] := strMasterdata;
          for i := 0 to lbResults.Items.Count-1 do
            lbResults.Items.Objects[i].Free;
          lbResults.Items.Clear;
          if not Assigned(ActiveSearch) then
            ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
          ActiveSearch.Sender := TComponent(Sender);
          ActiveSearch.OnItemFound:=@ActiveSearchItemFound;
          ActiveSearch.OnEndSearch:=@ActiveSearchEndItemSearch;
          ActiveSearch.Start(SearchString);
          Application.ProcessMessages;
    //      while ActiveSearch.Active do Application.ProcessMessages;
        end;
    end
  else
    pSearch.Visible:=False;
end;

procedure TfPosition.FGridViewSetupPosition(Sender: TObject;Columns : TGridColumns);
var
  i: Integer;
  a: Integer;
  aTextTyp: TTextTypes;
  aUnits: TUnits;
begin
  for i := 0 to FGridView.Columns.Count-1 do
    begin
      if TColumn(FGridView.Columns[i]).Fieldname = 'POSTYP' then
        begin
          FGridView.Columns[i].PickList.Clear;
          TBaseDBPosition(Dataset).PosTyp.Open;
          with TBaseDBPosition(Dataset).PosTyp.DataSet do
            begin
              First;
              while not Eof do
                begin
                  FGridView.Columns[i].PickList.Add(Format('%-3s %s',[FieldByName('NAME').AsString,FieldByName('TEXT').AsString]));
                  next;
                end;
            end;
        end
      else if TColumn(FGridView.Columns[i]).Fieldname = 'TEXTTYPE' then
        begin
          FGridView.Columns[i].PickList.Clear;
          aTextTyp := TTextTypes.Create(nil);
          aTextTyp.Open;
          with aTexttyp.DataSet do
            begin
              First;
              a := 0;
              while not Eof do
                begin
                  FGridView.Columns[i].PickList.Add(Format('%-2d%s',[a,FieldByName('NAME').AsString]));
                  inc(a);
                  next;
                end;
            end;
          aTextTyp.Free;
        end
      else if TColumn(FGridView.Columns[i]).Fieldname = 'STORAGE' then
        begin
          FGridView.Columns[i].PickList.Clear;
          Data.StorageType.Open;
          with Data.StorageType.DataSet do
            begin
              First;
              while not Eof do
                begin
                  FGridView.Columns[i].PickList.Add(Format('%-3s %s',[FieldByName('ID').AsString,FieldByName('NAME').AsString]));
                  next;
                end;
            end;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'QUANTITYU' then
        begin
          FGridView.Columns[i].PickList.Clear;
          aUnits := TUnits.Create(nil);
          aUnits.Open;
          with aUnits.DataSet do
            begin
              First;
              while not eof do
                begin
                  FGridView.Columns[i].PickList.Add(FieldByName('NAME').AsString);
                  next;
                end;
            end;
          aUnits.Free;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'IDENT' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'ACTIVE' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsCheckboxColumn;
          TColumn(FGridView.Columns[i]).ValueChecked:='Y';
          TColumn(FGridView.Columns[i]).ValueUnChecked:='N';
          TColumn(FGridView.Columns[i]).ReadOnly := False;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'VAT' then
        begin
          FGridView.Columns[i].PickList.Clear;
          TBaseDBPosition(Dataset).Vat.Open;
          with TBaseDBPosition(Dataset).Vat.DataSet do
            begin
              First;
              while not eof do
                begin
                  FGridView.Columns[i].PickList.Add(Format('%-3s %s',[FieldByName('ID').AsString,FieldByName('NAME').AsString]));
                  next;
                end;
            end;
        end
      ;
    end;
  with Application as IBaseDBInterface do
    acViewTexts.Checked:= DBConfig.ReadString('RDET'+BaseName,'Y') = 'Y';
  FGridView.UseDefaultRowHeight:=not acViewTexts.Checked;
  FGridView.CalculateRowHeights;
  with Application as IBaseDBInterface do
    begin
      acViewDetails.Checked:= DBConfig.ReadString('DVIS'+BaseName,'N') = 'Y';
    end;
  acViewDetailsExecute(nil);
  acDelPos.Enabled := acAddPos.Enabled and (FGridView.Count > 0);
end;
function TfPosition.fSearchOpenItem(aLink: string): Boolean;
var
  aCount: Integer;
  aMasterdata: TMasterdata;
  aCnt: Integer;
begin
  Result := False;
  aMasterdata := TMasterdata.CreateEx(Self,Data);
  aMasterdata.CreateTable;
  Data.SetFilter(aMasterdata,'('+Data.QuoteField('ID')+'='+Data.QuoteValue(fSearch.sgResults.Cells[1,fSearch.sgResults.Row])+')'+' AND '+Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y'));
  if Data.Locate(aMasterdata,'ID',fSearch.sgResults.Cells[1,fSearch.sgResults.Row],[loCaseInsensitive,loPartialKey]) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      aCount := DataSet.Count;
      aCnt := FGridView.DataSet.Count;
      DataSet.Assign(aMasterdata);
      if (GetPostyp <> -1) and Assigned(InplaceFrames[GetPosTyp]) then
        begin
          InplaceFrames[GetPosTyp].SetArticle(aMasterdata);
        end;
      if DataSet.CanEdit then DataSet.Post;
      if FGridView.DataSet.Count <> aCnt then
        FGridView.Refresh
      else
        FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      Result := True;
    end;
  aMasterdata.Destroy;
end;
procedure TfPosition.lbResultsDblClick(Sender: TObject);
var
  aMD: TMasterdata;
  aCount: Integer;
  Key: Word;
  Shift: TShiftState;
  aSelCol: objpas.Integer;
  aCnt: Integer;
begin
  FFound := False;
  if lbResults.ItemIndex < 0 then exit;
  pSearch.Visible:=False;
  aMD := TMasterdata.Create(nil);
  aMD.CreateTable;
  aMD.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
  aMD.Open;
  aCount := DataSet.Count;
  FGridView.gList.EditorMode:=False;
  if (aCount = 0) and (DataSet.State = dsInsert) then
    inc(aCount)
  else FFound:=True;
  aCnt := FGridView.DataSet.Count;
  DataSet.Assign(aMD);
  if (GetPostyp <> -1) and Assigned(InplaceFrames[GetPosTyp]) then
    begin
      InplaceFrames[GetPosTyp].SetArticle(aMD);
    end;
  if aCnt<>FGridView.DataSet.Count then
    FGridView.Refresh
  else
    FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
  FGridView.SetFocus;
  aMD.Free;
  pSearch.Visible:=False;
  Key := VK_TAB;
  Shift := [];
  THackCustomGrid(FGridView.gList).KeyDown(Key,Shift);
  FGridView.gList.EditorMode:=True;
end;
procedure TfPosition.acAddPosExecute(Sender: TObject);
begin
  FGridView.SetFocus;
  FGridView.InsertAfter;
end;

procedure TfPosition.acAddfromArticleExecute(Sender: TObject);
begin
  acAddPos.Execute;
  acSearchArticle.Execute;
end;

procedure TfPosition.acDelPosExecute(Sender: TObject);
begin
  FGridView.Delete;
  acDelPos.Enabled := acAddPos.Enabled and ((FGridView.Count > 0) and (DataSet.State <> dsInsert));
end;
procedure TfPosition.acGotoArticleExecute(Sender: TObject);
var
  aMasterdata: TMasterdataList;
begin
  aMasterdata := TMasterdata.CreateEx(Self,Data);
  aMasterdata.CreateTable;
  FGridView.GotoActiveRow;
  aMasterdata.Select(DataSet.FieldByName('IDENT').AsString,DataSet.FieldByName('VERSION').AsVariant,DataSet.FieldByName('LANGUAGE').AsVariant);
  aMasterdata.Open;
  if aMasterdata.Count = 0 then
    begin
      aMasterdata.Select(DataSet.FieldByName('IDENT').AsString);
      aMasterdata.Open;
    end;
  if aMasterdata.Count > 0 then
    begin
      Data.GotoLink(Data.BuildLink(aMasterdata.DataSet));
    end;
  aMasterdata.Free;
end;

procedure TfPosition.acMakeSubPosExecute(Sender: TObject);
begin
  FGridView.SetChild;
end;

procedure TfPosition.acPermanentEditorModeExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  if acPermanentEditormode.Checked then
    FGridView.gList.Options:=FGridView.gList.Options+[goAlwaysShowEditor]
  else
  FGridView.gList.Options:=FGridView.gList.Options-[goAlwaysShowEditor];
  with Application as IBaseDbInterface do
    begin
      if acPermanentEditorMode.Checked then
        DBConfig.WriteString('EPOSVIS','Y')
      else
        DBConfig.WriteString('EPOSVIS','');
    end;
end;

procedure TfPosition.acRefreshExecute(Sender: TObject);
begin
  FGridView.Refresh;
end;

procedure TfPosition.acRenumberExecute(Sender: TObject);
begin
  FGridView.RenumberRows;
  FGridView.Refresh(False);
end;

procedure TfPosition.acSearchArticleExecute(Sender: TObject);
var
  i: Integer;
  aIdx: Integer = -1;
begin
  FGridView.SelectCol('TEXT');
  fSearch.AllowSearchTypes(strMasterdata);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'POSITION',strSearchFromOrder);
  fSearch.SetLanguage;
end;

procedure TfPosition.acSumExecute(Sender: TObject);
var
  aPos: TPositionTyp;
begin
  TBaseDBPosition(DataSet).AppendSubTotal;
  FGridView.Refresh;
end;

procedure TfPosition.ActiveSearchEndItemSearch(Sender: TObject);
begin
  if not ActiveSearch.Active then
    begin
      if not ActiveSearch.NewFound then
        begin
          ActiveSearch.Start(ActiveSearch.SearchString,ActiveSearch.NextSearchLevel);
          exit;
        end;
      if (ActiveSearch.Count=0) and (lbResults.Items.Count=0) then
        pSearch.Visible:=False;
    end;
end;
procedure TfPosition.ActiveSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPrio: Integer;
  aItem: TBaseDBList);
begin
  with pSearch do
    begin
      if not Visible then
        Visible := True;
    end;
  if aActive then
    if lbResults.Items.IndexOf(Data.GetLinkDesc(aLink))=-1 then
      lbResults.Items.AddObject(Data.GetLinkDesc(aLink) ,TLinkObject.Create(aLink));
end;

procedure TfPosition.acUnMakeSebPosExecute(Sender: TObject);
begin
  FGridView.UnSetChild;
end;

procedure TfPosition.acViewDetailsExecute(Sender: TObject);
begin
  acViewDetails.Enabled:=False;
  if Assigned(Sender) then
    Application.ProcessMessages;
  pDetail.Visible:=acViewDetails.Checked;
  spDetails.Visible:=acViewDetails.Checked;
  if spDetails.Visible then
    begin
      spDetails.Top:=pDetail.Top-1;
      FGridView.First;
      with Application as IBaseDbInterface do
        begin
          DBConfig.WriteString('DVIS'+BaseName,'Y');
          pDetail.Height:=DBConfig.ReadInteger('DSIZ'+BaseName,pDetail.Height);
        end;
    end
  else
    with Application as IBaseDbInterface do
      DBConfig.WriteString('DVIS'+BaseName,'N');
  acViewDetails.Enabled:=True;
end;

procedure TfPosition.acViewTextsExecute(Sender: TObject);
begin
  acViewTexts.Enabled:=False;
  if Sender <> nil then
    Application.ProcessMessages;
  FGridView.UseDefaultRowHeight := not acViewTexts.Checked;
  with Application as IBaseDbInterface do
    begin
      if acViewTexts.Checked then
        DBConfig.WriteString('RDET'+BaseName,'Y')
      else
        DBConfig.WriteString('RDET'+BaseName,'N');
    end;
  acViewTexts.Enabled:=True;
end;

procedure TfPosition.AddCalcTab(Sender: TObject);
begin
  TfCalcPositionFrame(Sender).DataSet := FDataset;
  TfCalcPositionFrame(Sender).TabCaption:=strCalc;
end;

procedure TfPosition.PositionDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if Field.FieldName='POSTYP' then
    if (acViewDetails.Checked) and (not DataSet.DataSet.ControlsDisabled) then
      TabTimer.Enabled:=True;
end;

procedure TfPosition.PositionStateChange(Sender: TObject);
begin
  if (acViewDetails.Checked) and Assigned(FDataSet) and (not DataSet.DataSet.ControlsDisabled) and (FDataset.State=dsInsert) then
    TabTimer.Enabled:=True;
end;

procedure TfPosition.DoAsyncInit(Data: PtrInt);
begin
  FGridView.fGridViewEnter(FGridView);
  acPermanentEditorModeExecute(acPermanentEditorMode);
end;

procedure TfPosition.sgPositionsDragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
  nData : TTreeEntry;
  aMasterdata: TMasterdata;
begin
  if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
    begin
      nData := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
      aMasterdata := TMasterdata.CreateEx(Self,Data);
      aMasterdata.CreateTable;
      Data.SetFilter(aMasterdata,nData.Filter);
      Data.GotoBookmark(aMasterdata,nData.Rec);
      if  (FDataSet.State <> dsInsert)
      and (FDataSet.State <> dsEdit)
      then
        acAddPos.Execute;
      TBaseDBPosition(FDataSet).Assign(aMasterdata);
      if (GetPostyp <> -1) and Assigned(InplaceFrames[GetPosTyp]) then
        begin
          InplaceFrames[GetPosTyp].SetArticle(aMasterdata);
        end;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      aMasterdata.Free;
    end
  else
  if (Source = fSearch.sgResults) then
    begin
      aMasterdata := TMasterdata.CreateEx(Self,Data);
      aMasterdata.CreateTable;
      aMasterdata.SelectFromLink(fSearch.GetLink);
      aMasterdata.Open;
      if aMasterdata.Count > 0 then
        begin
          if  (FDataSet.State <> dsInsert)
          and (FDataSet.State <> dsEdit)
          then
            acAddPos.Execute;
          TBaseDbPosition(DataSet).Assign(aMasterdata);
          if (GetPostyp <> -1) and Assigned(InplaceFrames[GetPosTyp]) then
            begin
              InplaceFrames[GetPosTyp].SetArticle(aMasterdata);
            end;
          FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
        end;
      aMasterdata.Free;
    end;
end;
procedure TfPosition.sgPositionsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(uMainTreeFrame.fMainTreeFrame)
  and (Source = uMainTreeFrame.fMainTreeFrame.tvMain)
  and ((TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etArticle)) then
    Accept := True;
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      with fSearch.sgResults do
        if copy(fSearch.GetLink,0,10) = 'MASTERDATA' then
          Accept := True;
    end;
end;
procedure TfPosition.spDetailsMoved(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    DBConfig.WriteInteger('DSIZ'+BaseName,pDetail.Height);
end;
procedure TfPosition.TabTimerTimer(Sender: TObject);
var
  NewVisible: Boolean;
  PosTyp: LongInt;
begin
  if DataSet.DataSet.ControlsDisabled then exit;
  TabTimer.Enabled:=False;
  if acViewDetails.Checked then
    begin
      NewVisible:=False;
      if FGridView.GotoActiveRow then
        begin
          PosTyp := GetPosTyp;
          if (Postyp <> -1) and Assigned(InplaceFrames[PosTyp]) and (FPosTyp<>PosTyp) then
            begin
              FPosTyp:=PosTyp;
              if tsDetails.ControlCount > 0 then
                tsDetails.RemoveControl(tsDetails.Controls[0]);
              InplaceFrames[PosTyp].Parent := tsDetails;
              InplaceFrames[PosTyp].Align:=alClient;
              TUnprotectedFrame(InplaceFrames[GetPosTyp]).DoEnter;
              NewVisible:=True;
            end
          else if (Postyp <> -1) and Assigned(InplaceFrames[PosTyp]) then
            begin
              TUnprotectedFrame(InplaceFrames[GetPosTyp]).DoEnter;
            end
          else NewVisible:=False;
        end
      else NewVisible:=False;
      RefreshTabs;
    end;
end;
procedure TfPosition.SetDataSet(const AValue: TBaseDBDataset);
var
  SetLabels: Boolean;
begin
  FDataset:=AValue;
  SetLabels := FGridView.DataSet = nil;
  FGridView.DataSet := AValue;
  if Assigned(AValue) then
    begin
      Position.DataSet := FGridView.DataSet.DataSet;
      Orderpos.DataSet := FGridView.DataSet.DataSet;
      if SetLabels then
        DataSet.SetDisplayLabels(DataSet.DataSet);
    end
  else Position.DataSet := nil;
  with Application as IBaseDBInterface do
    acPermanentEditormode.Checked:= DBConfig.ReadString('EPOSVIS','N') = 'Y';
end;
procedure TfPosition.SetBaseName(AValue: string);
begin
  if FBaseName=AValue then Exit;
  FBaseName:=AValue;
  FGridView.BaseName:=AValue;
end;
function TfPosition.GetPosTyp: Integer;
begin
  Result := -1;
  if DataSet is TOrderPos then
    if TBaseDBPosition(DataSet).PosTyp.FieldByName('NAME').AsString = DataSet.FieldByName('POSTYP').AsString then
      Result := StrToIntDef(StringReplace(Trim(TBaseDBPosition(DataSet).PosTyp.FieldByName('TYPE').AsString),#13,'',[rfReplaceAll]),-1)
end;
procedure TfPosition.SetFormName(const AValue: string);
begin
  if FFormName=AValue then exit;
  FFormName:=AValue;
  pcTabs.ClearTabClasses;
  pcTabs.AddTabClass(TfDocumentFrame,strFiles,@AddDocumentsTab);
  pcTabs.AddTabClass(TfCalcPositionFrame,strCalc,@AddCalcTab);
  with Application as TBaseVisualApplication do
    AddTabClasses(FFormName,pcTabs);
end;
procedure TfPosition.RefreshTabs;
var
  aFrame: TTabSheet;
  aDocuments: TDocuments;
  aPosID: String;
begin
  if Data.Users.Rights.Right('OPTIONS') > RIGHT_READ then
    begin
      pcTabs.CanHaveCustomTabs(@TBaseVisualApplication(Application).OnAddCustomTab);
    end;
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      with Application as TBaseVisualApplication do
        AddTabs(pcTabs);
      //Documents
      aFrame := pcTabs.GetTab(TfDocumentFrame);
      if not Assigned(aFrame) then
        aDocuments := TDocuments.CreateEx(Self,Data)
      else aDocuments := TDocuments(TfDocumentFrame(aFrame.Controls[0]).DataSet);
      if DataSet.DataSet.FieldDefs.IndexOf('ORDERNO') <> -1 then
        aPosID := DataSet.FieldByName('ORDERNO').AsString+DataSet.FieldByName('POSNO').AsString
      else
        aPosID := DataSet.FieldByName('SQL_ID').AsString+DataSet.FieldByName('POSNO').AsString;
      aDocuments.Select(DataSet.Id.AsVariant,'P',aPosId,Null,Null);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        begin
          aDocuments.Free;
          if Assigned(aFrame) then
            begin
              pcTabs.WillRemoveTab(aFrame);
              aFrame.Free;
              pcTabs.TabIndex:=0;
            end;
        end
      else if not Assigned(aFrame) then
        begin
          pcTabs.AddTab(TfDocumentFrame.Create(Self),False);
          aFrame := pcTabs.GetTab(TfDocumentFrame);
          TfDocumentFrame(aFrame.Controls[0]).DataSet := aDocuments;
        end
      else TfDocumentFrame(aFrame.Controls[0]).DataSet := aDocuments;;
      //PositionCalc
      TBaseDBPosition(Dataset).PosCalc.Open;
      aFrame := pcTabs.GetTab(TfCalcPositionFrame);
      if (not Assigned(aFrame)) and (TBaseDBPosition(Dataset).PosCalc.Count>0) then
        begin
          pcTabs.AddTab(TfCalcPositionFrame.Create(Self),False);
          aFrame := pcTabs.GetTab(TfCalcPositionFrame);
          AddCalcTab(aFrame.Controls[0]);
        end
      else if Assigned(aFrame) and (TBaseDBPosition(Dataset).PosCalc.Count=0) then
        begin
          pcTabs.WillRemoveTab(aFrame);
          aFrame.Free;
          pcTabs.TabIndex:=0;
        end;
    end;
end;
procedure TfPosition.AddDocumentsTab(Sender: TObject);
var
  aPosID: String;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      TfDocumentFrame(Sender).DataSet := TDocuments.CreateEx(Self,Data);
    end;
  if not TfDocumentFrame(Sender).DataSet.DataSet.Active then
    begin
    //Data.SetFilter(TfDocumentFrame(Sender).DataSet,Data.QuoteField('SQL_ID')+'=NULL');
      TfDocumentFrame(Sender).DataSet.Select(0);
      TfDocumentFrame(Sender).DataSet.Open;
    end;
  if DataSet.DataSet.FieldDefs.IndexOf('ORDERNO') <> -1 then
    aPosID := DataSet.FieldByName('ORDERNO').AsString+DataSet.FieldByName('POSNO').AsString
  else
    aPosID := DataSet.FieldByName('SQL_ID').AsString+DataSet.FieldByName('POSNO').AsString;
  if TfDocumentFrame(Sender).DataSet.DataSet.FieldDefs.IndexOf('TYPE') <> -1 then
    TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'P',aPosID,Null,Null)
  else
    TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'P');
  TfDocumentFrame(Sender).SetRights(FEditable);
end;

procedure TfPosition.SetRights(Editable: Boolean);
var
  i: Integer;
begin
  FEditAble := Editable;
  acAddfromArticle.Enabled:=Editable;
  acDelPos.Enabled := Editable;
  acAddPos.Enabled := Editable;
  acMakeSubPos.Enabled:=Editable;
  acUnMakeSebPos.Enabled:=Editable;
  acSum.Enabled:=Editable;
  acRenumber.Enabled:=Editable;
  acGotoArticle.Enabled := (Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_NONE) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_NONE) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_NONE);
  FGridView.SetRights(Editable);
  for i := low(InplaceFrames) to High(InplaceFrames) do
    if Assigned(InplaceFrames[i]) then
      InplaceFrames[i].SetRights(Editable);
  ArrangeToolBar(pToolbar,ActionList1,'Position');
end;
procedure TfPosition.AutoInsert;
begin
  FGridView.AutoInsert;
end;
procedure TfPosition.SetFocus;
begin
  if CanFocus and Visible then
    inherited;
  FGridView.SetFocus;
  if FGridView.IsVisible and FFirstShow then
    begin
      Application.QueueAsyncCall(@DoAsyncInit,0);
      FFirstshow := False;
    end;
end;

procedure TfPosition.SetLanguage;
begin
  if Assigned(DataSet) then
    DataSet.SetDisplayLabels(DataSet.DataSet);
end;
constructor TfPosition.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  for i := 0 to high(InplaceFrames) do
    InplaceFrames[i] := nil;
  FPosTyp:=-1;
  FFirstShow:=True;
  pcTabs.AddTabClass(TfDocumentFrame,strFiles,@AddDocumentsTab);
  pcTabs.AddTabClass(TfCalcPositionFrame,strCalc,@AddCalcTab);
  FGridView := TfGridView.Create(Self);
  FGridView.OnCellChanging:=@FGridViewCellChanging;
  FGridView.OnCellChanged:=@FGridViewCellChanged;
  FGridView.OnCellButtonClick:=@FGridViewCellButtonClick;
  FGridView.OnSetupPosition:=@FGridViewSetupPosition;
  FGridView.OnDragOver:=@sgPositionsDragOver;
  FGridView.OnDragDrop:=@sgPositionsDragDrop;
  FGridView.DefaultRows:='GLOBALWIDTH:%;POSNO:32;TEXT:301;QUANTITY:40;QUANTITYU:62;SELLPRICE:68;VAT:49;GROSSPRICE:83;POSTYP:47;';
  FGridView.IdentField:='TEXT';
  FGridView.TextField:='TEXT';
  FGridView.NumberField:='POSNO';
  FGridView.SortField:='POSNO';
  FGridView.ShortTextField:='SHORTTEXT';
  FGridView.TreeField:='PARENT';
  FGridView.Parent := Self;
  FGridView.Align:=alClient;
  FGridView.FilterRow:=true;
  FGridView.WordWrap:=true;
  FGridView.OnSearchKey:=@FGridViewSearchKey;
  FGridView.OnAutoFilterChanged:=@FGridViewAutoFilterChanged;
  FGridView.UseDefaultRowHeight := False;
  FGridView.gList.PopupMenu:=pmPosition;
end;
destructor TfPosition.Destroy;
var
  i: Integer;
begin
  for i := 0 to lbResults.Items.Count-1 do
    lbResults.Items.Objects[i].Free;
  if Assigned(ActiveSearch) then ActiveSearch.Free;
  FGridView.Free;
  inherited Destroy;
end;
procedure TfPosition.DisableCalculation;
begin
  inc(FCalculationDisabled);
end;
procedure TfPosition.EnableCalculation;
begin
  dec(FCalculationDisabled);
end;
procedure TfPosition.SyncDataSource;
begin
  FGridView.SyncDataSource;
end;
procedure TfPosition.ResetEditor;
begin
  FgridView.ResetEditor;
end;

procedure TfPosition.Post;
begin
  FGridView.Post;
end;

end.

