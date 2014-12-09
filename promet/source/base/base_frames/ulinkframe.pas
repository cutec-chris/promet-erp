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
*******************************************************************************}
unit uLinkFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, ExtCtrls,
  uPrometFramesInplaceDB, uBaseDbClasses, uFilterFrame, Grids, DBGrids,
  Menus, ActnList,ClipBrd,uOrder,Dialogs, Buttons, uExtControls, db;
type

  { TfLinkFrame }

  TfLinkFrame = class(TPrometInplaceDBFrame)
    acFilter: TAction;
    acPasteLinks: TAction;
    acShowInOrder: TAction;
    acCopyToClipboard: TAction;
    acDelete: TAction;
    acAddLinks: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Datasource: TDatasource;
    dnContacts: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miOpen: TMenuItem;
    pToolbar: TPanel;
    Panel2: TPanel;
    pCont: TPanel;
    pmPopup: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure acAddLinksExecute(Sender: TObject);
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acPasteLinksExecute(Sender: TObject);
    procedure FContListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FContListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FContListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FContListViewDetails(Sender: TObject);
    function fSearchOpenItem(aLinks: string): Boolean;
    procedure pmPopupPopup(Sender: TObject);
  private
    fBaseName: string;
    { private declarations }
    FContList: TfFilter;
    FOrder: TOrder;
    FEditable : Boolean;
    procedure SetBaseName(const AValue: string);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    property BaseName : string read fBaseName write SetBaseName;
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    property Order : TOrder read FOrder write FOrder;
    procedure SetRights(Editable : Boolean);override;
    procedure ShowFrame; override;
  end;

implementation
uses uBaseVisualControls,Graphics,uBaseDbInterface,uData,uBaseVisualApplication,
  uError, uIntfStrConsts,uAccounting,Utils,uSearch,uMainTreeFrame,uMasterdata,uProjects,
  uPerson,umeeting;
{$R *.lfm}
resourcestring
  strDoAppendToAccount            = 'Der eingefuegte Verweis ist eine Kontobuchung,'+lineending+'moechten Sie diesen Vorgang als Beleg einfuegen ?';
  strEntryNotFound                = 'Eintrag konnte nicht gefundne werden !';
  strAddEntryToLinkedItem         = 'Soll der zu verlinkende Eintrag auch einen Link auf diesen Eintrag erhalten ?';
procedure TfLinkFrame.FContListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if Column.FieldName = 'ICON' then
        begin
          if not Column.Field.IsNull then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,Column.Field.AsInteger);
        end
      else
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
end;
procedure TfLinkFrame.acPasteLinksExecute(Sender: TObject);
var
  Stream: TStringStream;
  aLinks: String;
  aLinkDesc: String;
  aLink: String;
  aIcon : Integer;
  Accounts: TAccounts;
  tmp1: String;
  tmp2: String;
  addToLinked: Boolean = False;
  aHistory: TBaseHistory;
  bLink: String;
  aDS: TBaseDBList;
  aClass: TBaseDBDatasetClass;
begin
  Stream := TStringStream.Create('');
  if (pos('://',ClipBoard.AsText) > 0) then
    begin
      aLinks := ClipBoard.AsText;
      if copy(aLinks,length(aLinks)-1,1)<>';' then aLinks := aLinks+';';
      while pos(';',aLinks)>0 do
        begin
          aLink := copy(aLinks,0,pos(';',aLinks)-1);
          aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
          aLinkDesc := aLink;
          aIcon := Data.GetLinkIcon(aLink);
          with DataSet.DataSet do
            begin
              Insert;
              FieldByName('LINK').AsString := aLink;
              FieldByName('NAME').AsString := aLinkDesc;
              FieldByName('ICON').AsInteger := aIcon;
              FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              Post;
            end;
        end;
    end
  else if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
    begin
      Stream.Position:=0;
      aLinks := Stream.DataString;
    end
  else
    fError.ShowWarning(strCantgetClipboardContents);
  Stream.Free;
  addToLinked := MessageDlg(strAddEntryToLinkedItem,mtInformation,[mbYes,mbNo],0) = mrYes;
  while pos(';',aLinks) > 0 do
    begin
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
      aLinkDesc := Data.GetLinkDesc(aLink);
      aIcon := Data.GetLinkIcon(aLink);
      with DataSet.DataSet do
        begin
          Insert;
          FieldByName('LINK').AsString := aLink;
          FieldByName('NAME').AsString := aLinkDesc;
          FieldByName('ICON').AsInteger := aIcon;
          FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
          Post;
          if addToLinked then
            begin
              aDS := nil;
              bLink := Data.BuildLink(DataSet.Parent.DataSet);
              if Data.ListDataSetFromLink(aLink,aClass) then
                begin
                  aDS := TBaseDBList(aClass.Create(nil));
                end;
              if Assigned(aDS) then
                begin
                  tBaseDbList(aDS).SelectFromLink(aLink);
                  aDS.Open;
                  if aDS.Count>0 then
                    begin
                      Insert;
                      FieldByName('RREF_ID').AsVariant:=aDS.Id.AsVariant;
                      aLinkDesc := Data.GetLinkDesc(bLink);
                      aIcon := Data.GetLinkIcon(bLink);
                      FieldByName('LINK').AsString := bLink;
                      FieldByName('NAME').AsString := aLinkDesc;
                      FieldByName('ICON').AsInteger := aIcon;
                      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                      Post;
                    end;
                  aDS.Free;
                  DataSet.DataSet.Refresh;
                end;
            end;
        end;
      if (copy(aLink,0,16) = 'ACCOUNTEXCHANGE@') and Assigned(Order) then
        begin
          if MessageDlg(strAccounts,strDoAppendToAccount,mtConfirmation,[mbYes,mbNo],0) = mrYes then
            begin
              if rpos('{',aLink) > 0 then
                aLink := copy(aLink,0,rpos('{',aLink)-1)
              else if rpos('(',aLink) > 0 then
                aLink := copy(aLink,0,rpos('(',aLink)-1);
              aLink   := copy(aLink, pos('@', aLink) + 1, length(aLink));
              tmp1 := copy(aLink, 0, pos('&&', aLink) - 1);
              aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
              tmp2 := aLink;
              Accounts := TAccounts.CreateEx(Self,Data);
              Accounts.Open;
              if Accounts.DataSet.Locate('SQL_ID',tmp1,[]) then
                begin
                  Accounts.Exchange.Open;
                  if not Accounts.Exchange.DataSet.Locate('SQL_ID',tmp2,[]) then
                    begin
                      Data.SetFilter(Accounts.Exchange,'"SQL_ID"='+Data.QuoteValue(tmp2),1,'','ASC',False,False);
                      if not Accounts.Exchange.DataSet.Locate('SQL_ID',tmp2,[]) then
                        fError.ShowWarning(strEntryNotFound);
                    end;
                  if Accounts.Exchange.DataSet.Locate('SQL_ID',tmp2,[]) then
                      with Accounts.Exchange.Dataset do
                        begin
                          Edit;
                          FieldByName('VOUCHER').AsString := Data.BuildLink(Order.DataSet);
                          Post;
                        end;
                  Accounts.Free;
                end;
            end;
        end;
    end;
end;
procedure TfLinkFrame.acCopyToClipboardExecute(Sender: TObject);
var
  aLinks : string = '';
  Stream: TStringStream;
  i: Integer;
begin
  with FContList do
    begin
      if gList.SelectedRows.Count > 0 then
        begin
          for i := 0 to gList.SelectedRows.Count-1 do
            begin
              gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
              with Application as IBaseDbInterface do
                aLinks := aLinks+Datasource.DataSet.FieldByName('LINK').AsString+';';
            end;
          gList.SelectedRows.Clear;
        end
      else
        with Application as IBaseDbInterface do
          aLinks := aLinks+Datasource.DataSet.FieldByName('LINK').AsString+';';
      Stream := TStringStream.Create(aLinks);
      Clipboard.AddFormat(LinkClipboardFormat,Stream);
      Stream.Free;
    end;
end;

procedure TfLinkFrame.acAddLinksExecute(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(False,'LINKSADD','');
end;

procedure TfLinkFrame.acDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    with FContList do
      begin
        if gList.SelectedRows.Count > 0 then
          begin
            for i := 0 to gList.SelectedRows.Count-1 do
              begin
                gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
                gList.DataSource.DataSet.Delete;
              end;
            gList.SelectedRows.Clear;
          end
        else
          with Application as IBaseDbInterface do
            gList.DataSource.DataSet.Delete;
      end;
end;

procedure TfLinkFrame.FContListDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aRec: String;
  OldFilter: String;
  aLink: String;
  aLinkDesc: String;
  aIcon: Integer;
  DataT: TTreeEntry;
  addToLinked: Boolean;
  aLinks: String;
  aDS: TBaseDBList;
  bLink: String;
  aClass: TBaseDBDatasetClass;
begin
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      aLinks := fSearch.GetLink(True);
      addToLinked := MessageDlg(strAddEntryToLinkedItem,mtInformation,[mbYes,mbNo],0) = mrYes;
      while pos(';',aLinks) > 0 do
        begin
          aLink := copy(aLinks,0,pos(';',aLinks)-1);
          aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
          aLinkDesc := Data.GetLinkDesc(aLink);
          aIcon := Data.GetLinkIcon(aLink);
          with DataSet.DataSet do
            begin
              Insert;
              FieldByName('LINK').AsString := aLink;
              FieldByName('NAME').AsString := aLinkDesc;
              FieldByName('ICON').AsInteger := aIcon;
              FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              Post;
              if addToLinked then
                begin
                  aDS := nil;
                  bLink := Data.BuildLink(DataSet.Parent.DataSet);
                  if Data.ListDataSetFromLink(aLink,aClass) then
                    begin
                      aDS := TBaseDbList(aClass.Create(nil));
                    end;
                  if Assigned(aDS) then
                    begin
                      tBaseDbList(aDS).SelectFromLink(aLink);
                      aDS.Open;
                      if aDS.Count>0 then
                        begin
                          Insert;
                          FieldByName('RREF_ID').AsVariant:=aDS.Id.AsVariant;
                          aLinkDesc := Data.GetLinkDesc(bLink);
                          aIcon := Data.GetLinkIcon(bLink);
                          FieldByName('LINK').AsString := bLink;
                          FieldByName('NAME').AsString := aLinkDesc;
                          FieldByName('ICON').AsInteger := aIcon;
                          FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                          Post;
                        end;
                      aDS.Free;
                      DataSet.DataSet.Refresh;
                    end;
                end;
            end;
        end;
    end;
  if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
    begin
      DataT := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
      case DataT.Typ of
      etLink:
        begin
          aLink := DataT.Link;
          aLinkDesc := Data.GetLinkDesc(aLink);
          aIcon := Data.GetLinkIcon(aLink);
          with DataSet.DataSet do
            begin
              Insert;
              FieldByName('LINK').AsString := aLink;
              FieldByName('NAME').AsString := aLinkDesc;
              FieldByName('ICON').AsInteger := aIcon;
              FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              Post;
            end;
        end;
      end;
    end;
end;
procedure TfLinkFrame.FContListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DataT: TTreeEntry;
begin
  Accept := False;
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      with fSearch.sgResults do
        Accept := trim(fSearch.GetLink) <> '';
    end;
  if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
    begin
      if not Assigned(uMainTreeFrame.fMainTreeFrame.tvMain.Selected) then exit;
      DataT := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
      case DataT.Typ of
      etLink:
        begin
          Accept := True;
        end;
      end;
    end;
end;
procedure TfLinkFrame.FContListViewDetails(Sender: TObject);
begin
  Data.GotoLink(DataSet.FieldByName('LINK').AsString);
end;

function TfLinkFrame.fSearchOpenItem(aLinks: string): Boolean;
var
  addToLinked: Boolean;
  aLink: String;
  aIcon: Integer;
  aLinkDesc: String;
  aClass: TBaseDBDatasetClass;
  aDS: TBaseDBDataset;
  bLink: String;
begin
  aLinks := fSearch.GetLink(True);
  addToLinked := MessageDlg(strAddEntryToLinkedItem,mtInformation,[mbYes,mbNo],0) = mrYes;
  while pos(';',aLinks) > 0 do
    begin
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
      aLinkDesc := Data.GetLinkDesc(aLink);
      aIcon := Data.GetLinkIcon(aLink);
      with DataSet.DataSet do
        begin
          Insert;
          FieldByName('LINK').AsString := aLink;
          FieldByName('NAME').AsString := aLinkDesc;
          FieldByName('ICON').AsInteger := aIcon;
          FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
          Post;
          if addToLinked then
            begin
              aDS := nil;
              bLink := Data.BuildLink(DataSet.Parent.DataSet);
              if Data.ListDataSetFromLink(aLink,aClass) then
                begin
                  aDS := aClass.Create(nil);
                end;
              if Assigned(aDS) then
                begin
                  tBaseDbList(aDS).SelectFromLink(aLink);
                  aDS.Open;
                  if aDS.Count>0 then
                    begin
                      Insert;
                      FieldByName('RREF_ID').AsVariant:=aDS.Id.AsVariant;
                      aLinkDesc := Data.GetLinkDesc(bLink);
                      aIcon := Data.GetLinkIcon(bLink);
                      FieldByName('LINK').AsString := bLink;
                      FieldByName('NAME').AsString := aLinkDesc;
                      FieldByName('ICON').AsInteger := aIcon;
                      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                      Post;
                    end;
                  aDS.Free;
                  DataSet.DataSet.Refresh;
                end;
            end;
        end;
    end;
end;

procedure TfLinkFrame.pmPopupPopup(Sender: TObject);
var
  atmp: String;
begin
  atmp := ClipBoard.AsText;
  acPasteLinks.Enabled:=(Clipboard.HasFormat(LinkClipboardFormat) or (pos('://',atmp) > 0));
//  acShowInOrder.Visible:=Parent = fOrders.tsLinks;
end;
procedure TfLinkFrame.SetBaseName(const AValue: string);
var
  TopVisible: Boolean;
begin
  if fBaseName=AValue then exit;
  fBaseName:=AValue;
  TopVisible := FContList.pTop.Visible;
  FContList.FilterType:='PLINK'+AValue;
  FContList.pTop.Visible := TopVisible;
end;
constructor TfLinkFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContList := TfFilter.Create(Self);
  FOrder := nil;
  with FContList do
    begin
      FilterType:='PLINK';
      DefaultRows:='GLOBALWIDTH:%;ICON:30;NAME:200;REFERENCE:100;';
      Parent := pCont;
      Align := alClient;
      PTop.Visible := False;
      Editable := True;
      gList.CachedEditing:=True;
      Show;
    end;
  FContList.OnViewDetails:=@FContListViewDetails;
  FContList.OnDrawColumnCell:=@FContListDrawColumnCell;
  FContList.gList.PopupMenu := pmPopup;
  miOpen.Action := FContList.acOpen;
  FContList.gList.OnDragOver:=@FContListDragOver;
  FContList.gList.OnDragDrop:=@FContListDragDrop;
  FContList.gList.Options:=FContList.gList.Options+[dgMultiselect,dgPersistentMultiSelect];
end;
destructor TfLinkFrame.Destroy;
begin
  if Assigned(FContList) then
    begin
      FContList.DataSet := nil;
      FContList.Free;
    end;
  FDataSet := nil;
  inherited Destroy;
end;
procedure TfLinkFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  with FContList do
    begin
      with AValue.DataSet as IBaseDbFilter do
        FContList.DefaultFilter:=Filter;
    end;
  FContList.DataSet := AValue;
  DataSource.DataSet := AValue.DataSet;
end;
procedure TfLinkFrame.SetRights(Editable: Boolean);
begin
  FEditable := Editable;
  acPasteLinks.Enabled := Editable;
  dnContacts.Enabled:=Editable;
  ArrangeToolBar(pToolbar,ActionList1,'Links');
end;
procedure TfLinkFrame.ShowFrame;
begin
  inherited ShowFrame;
  FContList.SetActive;
end;
end.

