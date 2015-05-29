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
  Menus, ActnList,ClipBrd,uOrder,Dialogs, Buttons, uExtControls, db,uBaseDatasetInterfaces;
type

  { TfLinkFrame }

  TfLinkFrame = class(TPrometInplaceDBFrame)
    acFilter: TAction;
    acPasteLinks: TAction;
    acShowInOrder: TAction;
    acCopyToClipboard: TAction;
    acDelete: TAction;
    acAddLinks: TAction;
    acEditVersion: TAction;
    acAddFolder: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Datasource: TDatasource;
    dnContacts: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    miOpen: TMenuItem;
    pToolbar: TPanel;
    Panel2: TPanel;
    pCont: TPanel;
    pmPopup: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure acAddFolderExecute(Sender: TObject);
    procedure acAddLinksExecute(Sender: TObject);
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditVersionExecute(Sender: TObject);
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
  uPerson,umeeting,LCLIntf;
{$R *.lfm}
resourcestring
  strDoAppendToAccount            = 'Der eingefuegte Verweis ist eine Kontobuchung,'+lineending+'moechten Sie diesen Vorgang als Beleg einfuegen ?';
  strEntryNotFound                = 'Eintrag konnte nicht gefundne werden !';
  strAddEntryToLinkedItem         = 'Soll der zu verlinkende Eintrag auch einen Link auf diesen Eintrag erhalten ?';
  strDeleteBack                   = 'Es existieren Links von den verlinkten Objekten zurück auf dieses Objekt, sollen diese auch gelöscht werden ?';
  strLinkNotValid                 = 'Der Verweis zeigt auf eine ungültige Artikelversion, soll die Version angelegt werden ?';
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
          aIcon := Data.GetLinkIcon(aLink,True);
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
  if pos(';',aLinks)>0 then
    addToLinked := MessageDlg(strAddEntryToLinkedItem,mtInformation,[mbYes,mbNo],0) = mrYes;
  while pos(';',aLinks) > 0 do
    begin
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
      aLinkDesc := Data.GetLinkDesc(aLink);
      aIcon := Data.GetLinkIcon(aLink,True);
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
                      aIcon := Data.GetLinkIcon(bLink,True);
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
        begin
          with Application as IBaseDbInterface do
            aLinks := aLinks+Datasource.DataSet.FieldByName('LINK').AsString+';';
          ClipBoard.AsText:=Datasource.DataSet.FieldByName('LINK').AsString;
        end;
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

procedure TfLinkFrame.acAddFolderExecute(Sender: TObject);
var
  aLink: String;
  aLinkDesc: String;
  aIcon: Integer;
begin
  if SelectDirectoryDialog1.Execute then
    begin
      aLink := 'file://'+StringReplace(SelectDirectoryDialog1.FileName,'\','/',[rfReplaceAll]);
      aLinkDesc := ExtractFileName(SelectDirectoryDialog1.FileName);
      aIcon := 123;
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

procedure TfLinkFrame.acDeleteExecute(Sender: TObject);
var
  i: Integer;
  bLink: String;
  aLink: String;
  aClass: TBaseDBDatasetClass;
  aLinks: TLinks;
  aDS: TBaseDbList;
  DeleteBack : Boolean = False;
  procedure DeleteBackLink;
  begin
    with FContList do
      begin
        //Rückverweis löschen ?!
        bLink := Data.BuildLink(DataSet.Parent.DataSet);
        aLink := gList.DataSource.DataSet.FieldByName('LINK').AsString;
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
                aLinks := TLinks.Create(nil);
                aLinks.Filter(Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(aDS.Id.AsString));
                if aLinks.Locate('LINK',bLink,[]) then
                  begin
                    if DeleteBack or (MessageDlg(strDeleteBack,mtInformation,[mbYes,mbNo],0) = mrYes) then
                      begin
                        DeleteBack := True;
                        while aLinks.Locate('LINK',bLink,[]) do
                          aLinks.Delete;
                      end;
                  end;
                aLinks.Free;
              end;
          end;
      end;
  end;

begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    with FContList do
      begin
        if gList.SelectedRows.Count > 0 then
          begin
            for i := 0 to gList.SelectedRows.Count-1 do
              begin
                gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
                DeleteBackLink;
                gList.DataSource.DataSet.Delete;
              end;
            gList.SelectedRows.Clear;
          end
        else
          with Application as IBaseDbInterface do
            begin
              DeleteBackLink;
              gList.DataSource.DataSet.Delete;
            end;
      end;
end;

procedure TfLinkFrame.acEditVersionExecute(Sender: TObject);
var
  aNew: String;
  i: Integer;

  procedure ChangeLink;
  var
    aLink: String;
    cLink: String;
    aMasterdata: TMasterdata;
    ActOK : Boolean = False;
    bLink: String;
    LinkToMe: String;
    aDS: TBaseDbList;
    aClass: TBaseDBDatasetClass;
    aLinks: TLinks;
  begin
    LinkToMe := Data.BuildLink(DataSet.Parent.DataSet);
    aLink := Datasource.DataSet.FieldByName('LINK').AsString;
    if copy(aLink,0,11)='MASTERDATA@' then
      begin
        //Verweis auf uns im alten Artikel löschen
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
                aLinks := TLinks.Create(nil);
                aLinks.Filter(Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(aDS.Id.AsString));
                if aLinks.Locate('LINK',LinkToMe,[]) then
                  begin
                    while aLinks.Locate('LINK',LinkToMe,[]) do
                      aLinks.Delete;
                  end;
                aLinks.Free;
              end;
          end;
        //Verweis auf neuen Artikel erstellen / Artikelversion anlegen
        cLink := aLink;
        bLink := copy(cLink,0,pos('&&',cLink)-1);
        cLink := copy(cLink,pos('&&',cLink)+2,length(cLink));
        cLink := copy(cLink,pos('&&',cLink)+2,length(cLink));
        bLink := bLink+'&&'+aNew+'&&'+cLink;
        aMasterdata := TMasterdata.Create(nil);
        aMasterdata.SelectFromLink(bLink);
        aMasterdata.Open;
        if aMasterdata.Count=0 then
          begin
            if (MessageDlg(strLinks,strItem+' '+Data.GetLinkDesc(aLink)+' '+strLinkNotValid,mtConfirmation,[mbYes,mbNo],0) = mrYes) then
              begin
                aMasterdata.SelectFromLink(aLink);
                aMasterdata.Open;
                aMasterdata.Versionate(aNew,True);
                ActOK:=true;
              end;
          end
        else
          ActOK:=true;
        if ActOK then
          begin
            Datasource.DataSet.Edit;
            Datasource.DataSet.FieldByName('LINK').AsString:=bLink;
            Datasource.DataSet.FieldByName('NAME').AsString:=Data.GetLinkDesc(bLink);
            Datasource.DataSet.Post;
            //Verweis auf uns in neuer Artikelversion anlegen
            aMasterdata.SelectFromLink(bLink);
            aMasterdata.Open;
            if aMasterdata.Count>0 then
              begin
                aMasterdata.Links.Open;
                aMasterdata.Links.Add(LinkToMe);
              end;
          end;
        aMasterdata.Free;
      end;
  end;

begin
  if InputQuery(strVersion,strVersion,aNew) then
    begin
      with FContList do
        begin
          if gList.SelectedRows.Count > 0 then
            begin
              for i := 0 to gList.SelectedRows.Count-1 do
                begin
                  gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
                  with Application as IBaseDbInterface do
                    ChangeLink;
                end;
              gList.SelectedRows.Clear;
            end
          else
            with Application as IBaseDbInterface do
              ChangeLink;
        end;
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
          aIcon := Data.GetLinkIcon(aLink,True);
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
                          aIcon := Data.GetLinkIcon(bLink,True);
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
          aIcon := Data.GetLinkIcon(aLink,True);
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
var
  aLink: String;
begin
  if not Data.GotoLink(DataSet.FieldByName('LINK').AsString) then
    begin
      aLink := DataSet.FieldByName('LINK').AsString;
      if Uppercase(copy(aLink,0,pos('://',aLink)-1)) = 'HTTP' then
        begin
          OpenURL(aLink);
        end
      else if pos('://',aLink) > 0 then
        begin
          OpenDocument(aLink);
        end;
    end;
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
      aIcon := Data.GetLinkIcon(aLink,True);
      with DataSet.DataSet do
        begin
          if TLinks(DataSet).Add(aLink) then
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
                        aIcon := Data.GetLinkIcon(bLink,True);
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

