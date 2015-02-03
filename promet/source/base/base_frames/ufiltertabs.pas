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

unit uFilterTabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, ComCtrls, DBGrids, uFilterFrame,
  db, Extctrls, Controls, Menus, uIntfStrConsts, uBaseDbClasses;

type

  { TfFilterTabs }

  TfFilterTabs = class(TFrame)
    ActiveDatasource: TDatasource;
    ImageList: TImageList;
    miClose: TMenuItem;
    pcViewTabs: TPageControl;
    pmClose: TPopupMenu;
    procedure miCloseClick(Sender: TObject);
    procedure pcViewTabsChange(Sender: TObject);
    procedure pcViewTabsPageChanged(Sender: TObject);
  private
    FAfterAddTab: TNotifyEvent;
    FBaseFilter: string;
    FDataset: TBaseDbDataset;
    fDefaultRows: string;
    FFilterType: string;
    FStdFilter: string;
    fTabNames: string;
    FInTabChange : Boolean;
    function GetDataSet: TBaseDbDataSet;
    function GetGrid: TDBGrid;
    procedure SetBaseFilter(const AValue: string);
    procedure SetDataSet(const AValue: TbaseDBDataSet);
    procedure SetDefaultRows(const AValue: string);
    procedure SetFilterType(const AValue: string);
    procedure SetGlobalFilter(const AValue: Boolean);
    procedure SetStdFilter(const AValue: string);
    procedure Settabnames(const AValue: string);
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    property ActiveList : TDBGrid read GetGrid;
    property ActiveDataSet : TBaseDbDataSet read GetDataSet;
    property Dataset : TBaseDBDataSet read FDataset write SetDataSet;
    property BaseFilter : string read FBaseFilter write SetBaseFilter;
    property StdFilter : string read FStdFilter write SetStdFilter;
    property GlobalFilter : Boolean write SetGlobalFilter;
    property FilterType : string read FFilterType write SetFilterType;
    property DefaultRows : string read fDefaultRows write SetDefaultRows;
    property AfterAddTab : TNotifyEvent read FAfterAddTab write FAfterAddTab;
    property TabNames : string read fTabNames write Settabnames;
    procedure DoRefresh;
    procedure Clear;
    procedure SetActive;
    function AddFilter(FieldName,Value : string) : Boolean;
    function ShowHint(var HintStr: string;var CanShow: Boolean; var HintInfo: THintInfo) : Boolean;
  end;

implementation
{$R *.lfm}

uses uData,uBaseVisualApplicationTools;

resourcestring
  strNewTab      = 'neues Tab';

{ TfFilterTabs }

procedure TfFilterTabs.pcViewTabsPageChanged(Sender: TObject);
var
  iPage1 : Integer;
  aPage: TTabSheet;
  aFilter: TfFilter;
  aTab: TfFilter;
begin
  if FInTabChange then exit;
  if not Assigned(pcViewTabs.ActivePage) then exit;
  FInTabChange := True;
  if pcViewTabs.ActivePage.ImageIndex = 0 then
    begin
      Screen.Cursor:=crHourglass;
      aPage := pcViewTabs.ActivePage;
      aPage.Caption:=FTabNames;
      aPage.ImageIndex:=-1;
      aTab := AddTab(aPage,FDataset);
      aTab.FilterType:=FFilterType;
      aTab.DefaultRows:=FdefaultRows;
      aTab.BaseFilter:=FBaseFilter;
      if Assigned(AfterAddTab) then
        FAfterAddTab(aTab);
      iPage1 := TCustomTabControl(pcViewTabs).Pages.Add(strNewTab);
      pcViewTabs.Pages[iPage1].ImageIndex:=0;
      SetActive;
      Screen.Cursor:=crDefault;
    end;
  ActiveDatasource.DataSet := ActiveList.DataSource.DataSet;
  FInTabChange := false;
end;

procedure TfFilterTabs.miCloseClick(Sender: TObject);
var
  aTab: TfFilter;
  aPage: TTabSheet;
begin
  if not Assigned(pcViewTabs.ActivePage) then exit;
  aTab := TfFilter(pcViewTabs.ActivePage.Controls[0]);
  aTab.DoBeforeClose;
  aTab.Parent := nil;
  aTab.Dataset.Free;
  aTab.Free;
  aPage := pcViewTabs.ActivePage;
  pcViewTabs.ActivePageIndex := pcViewTabs.ActivePageIndex-1;
  aPage.Free;
end;

procedure TfFilterTabs.pcViewTabsChange(Sender: TObject);
begin
  DoRefresh;
end;

function TfFilterTabs.GetGrid: TDBGrid;
begin
  if not Assigned(pcViewTabs.ActivePage) then exit;
  Result := TfFilter(pcViewTabs.ActivePage.Controls[0]).gList;
end;

function TfFilterTabs.GetDataSet: TBaseDbDataSet;
begin
  if not Assigned(pcViewTabs.ActivePage) then exit;
  Result := TfFilter(pcViewTabs.ActivePage.Controls[0]).DataSet;
end;

procedure TfFilterTabs.SetBaseFilter(const AValue: string);
var
  i: Integer;
begin
  FBaseFilter:=AValue;
  for i :=0 to pcViewTabs.PageCount-1 do
    if pcViewTabs.Pages[i].ImageIndex <> 0 then
      TfFilter(pcViewTabs.Pages[i].Controls[0]).BaseFilter := AValue;
end;

procedure TfFilterTabs.SetDataSet(const AValue: TBaseDbDataSet);
var
  aTab: TfFilter;
  iPage,iPage1 : Integer;
  aPage: TTabSheet;
  aFilter: TfFilter;
begin
  if FDataset=AValue then exit;
  FDataset:=AValue;
  if FDataSet = nil then
    begin
      Clear;
      exit;
    end;
  iPage := TCustomTabControl(pcViewTabs).Pages.Add(FTabNames);
  aPage := pcViewTabs.Pages[iPage];
  aTab := AddTab(aPage,AValue);
  aTab.FilterType:=FFilterType;
  aTab.DefaultRows:=FdefaultRows;
  aTab.BaseFilter:=FBaseFilter;
  aPage.Caption:=FTabNames;
  aPage.ImageIndex:=-1;
  if Assigned(FAfterAddTab) then
    FAfterAddTab(aTab);

  iPage1 := TCustomTabControl(pcViewTabs).Pages.Add(strNewTab);
  pcViewTabs.Pages[iPage1].ImageIndex:=0;
  pcViewTabs.ActivePage:=aPage;
  ActiveDatasource.DataSet := ActiveList.DataSource.DataSet;
end;

procedure TfFilterTabs.SetDefaultRows(const AValue: string);
begin
  if fDefaultRows=AValue then exit;
  fDefaultRows:=AValue;
end;

procedure TfFilterTabs.SetFilterType(const AValue: string);
begin
  if FFilterType=AValue then exit;
  FFilterType:=AValue;
end;

procedure TfFilterTabs.SetGlobalFilter(const AValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to pcViewTabs.PageCount-1 do
    if pcViewTabs.Pages[i].ImageIndex <> 0 then
      TfFilter(TTabSheet(pcViewTabs.Pages[i]).Controls[0]).GlobalFilter := AValue;
end;

procedure TfFilterTabs.SetStdFilter(const AValue: string);
begin
  if FStdFilter=AValue then exit;
  FStdFilter:=AValue;
end;

procedure TfFilterTabs.Settabnames(const AValue: string);
var
  i: Integer;
begin
  if fTabNames=AValue then exit;
  fTabNames:=AValue;
  for i := 0 to pcViewTabs.PageCount-1 do
    if pcViewTabs.Pages[i].ImageIndex <> 0 then
      if TfFilter(TTabSheet(pcViewTabs.Pages[i]).Controls[0]).cbFilter.Text = '' then
        pcViewTabs.Pages[i].Caption := AValue;
end;

constructor TfFilterTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInTabChange := False;
end;

procedure TfFilterTabs.DoRefresh;
var
  aList: TfFilter;
  aRec: LongInt;
  aRec1: LongInt;
begin
  if not Assigned(pcViewTabs.ActivePage) then exit;
  aList := TfFilter(pcViewTabs.ActivePage.Controls[0]);
  aList.Dataset.DataSet.DisableControls;
  aRec := aList.DataSet.GetBookmark;
  aList.Dataset.DataSet.Next;
  aRec1 := aList.Dataset.GetBookmark;
  aList.Dataset.DataSet.Refresh;
  if not aList.Dataset.GotoBookmark(aRec) then
    aList.Dataset.GotoBookmark(aRec1);
  aList.Dataset.FreeBookmark(aRec);
  aList.Dataset.FreeBookmark(aRec1);
  aList.Dataset.DataSet.EnableControls;
end;

procedure TfFilterTabs.Clear;
begin
  while pcViewTabs.PageCount > 0 do
    pcViewTabs.Pages[pcViewTabs.PageCount-1].Free;
end;

procedure TfFilterTabs.SetActive;
var
  bList: TfFilter;
begin
  if not Assigned(pcViewTabs.ActivePage) then exit;
//  if not fMain.Visible then exit;
  bList := TfFilter(pcViewTabs.ActivePage.Controls[0]);
  bList.SetActive;
end;

function TfFilterTabs.AddFilter(FieldName, Value: string) : Boolean;
var
  a: Integer;
  bList: TfFilter;
begin
  Result := false;
  bList := TfFilter(pcViewTabs.ActivePage.Controls[0]);
  if not Assigned(bList) then exit;
  with bList do
    begin
      for a := 0 to gList.Columns.Count-1 do
        if gList.Columns[a].FieldName = Fieldname then
          begin
            gHeader.Cells[a+1,1] := Value;
            Result := True;
            break;
          end;
      FAutoFilter := BuildAutofilter(gList,gHeader,nil);
      acFilter.Execute;
    end;
end;

function TfFilterTabs.ShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to pcViewTabs.PageCount-1 do
    if pcViewTabs.Pages[i].ImageIndex <> 0 then
      if TfFilter(TTabSheet(pcViewTabs.Pages[i]).Controls[0]).ShowHint(HintStr,CanShow,HintInfo) then
        begin
          Result := True;
          exit;
        end;
end;

initialization
end.

