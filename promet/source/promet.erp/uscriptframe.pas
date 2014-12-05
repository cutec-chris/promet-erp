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
unit uscriptframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LR_DBSet, LR_Class, Forms, Controls, ExtCtrls,
  ActnList, ComCtrls, StdCtrls, DbCtrls, Buttons, Menus, db, uPrometFrames,
  uExtControls, uFilterFrame, uIntfStrConsts, Utils, Dialogs, variants,
  uScriptEditor,uBaseDbClasses;
type

  { TfScriptFrame }

  TfScriptFrame = class(TPrometMainFrame)
    acCancel: TAction;
    acSave: TAction;
    acSetTreeDir: TAction;
    acShowTreeDir: TAction;
    acClose: TAction;
    acCopy: TAction;
    acPaste: TAction;
    acDelete: TAction;
    acRights: TAction;
    acPrint: TAction;
    acPasteImage: TAction;
    acAddImage: TAction;
    acScreenshot: TAction;
    ActionList1: TActionList;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel6: TBevel;
    eArticleNumber: TDBEdit;
    History: TDatasource;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MandantDetails: TDatasource;
    Script: TDatasource;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miCopy: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miStartTimeregistering: TMenuItem;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    pcPages: TExtMenuPageControl;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pmAction: TPopupMenu;
    pmImage: TPopupMenu;
    Report: TfrReport;
    sbMenue: TSpeedButton;
    tsScript: TTabSheet;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure ScriptStateChange(Sender: TObject);
    procedure mShortTextExit(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
  private
    { private declarations }
    FEditor: TfScriptEditor;
    FEditable : Boolean;
    procedure AddDocuments(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure AddLinks(Sender: TObject);
  protected
    procedure DoOpen(RefreshVersions : Boolean = True);
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure SetDataSet(const AValue: TBaseDBDataset); override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure FrameAdded; override;
  end;
implementation
{$R *.lfm}
uses uMasterdata,uData,uArticlePositionFrame,uDocuments,uDocumentFrame,
  uHistoryFrame,uImageFrame,uLinkFrame,uBaseDbInterface,uListFrame,
  uArticleStorageFrame,uArticleRepairFrame,uArticleText,uCopyArticleData,
  uMainTreeFrame,uPrometFramesInplace,uarticlesupplierframe,
  uNRights,uBaseVisualApplication,uWikiFrame,uWiki,ufinance,
  uthumbnails,Clipbrd,uscreenshotmain,uBaseApplication,uprometscripts;
resourcestring
  strPrices                                  = 'Preise';
  strProperties                              = 'Eigenschaften';
  strStorage                                 = 'Lager';
  strSupplier                                = 'Lieferant';
  strRepair                                  = 'Reparatur';
  strTexts                                   = 'Texte';
  strChangeNumer                             = 'Nummer ändern';
  strNewArticle                              = 'neuer Artikel';
procedure TfScriptFrame.acSaveExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicPost;
      fScriptEditor.acSave.Execute;
      if UseTransactions then
        begin
          Data.CommitTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
end;
procedure TfScriptFrame.acSetTreeDirExecute(Sender: TObject);
begin
  if fMainTreeFrame.GetTreeEntry = -1 then exit;
  with DataSet.DataSet do
    begin
      Edit;
      FieldbyName('TREEENTRY').AsVariant:=fMainTreeFrame.GetTreeEntry;
      fMainTreeFrame.tvMain.Selected.Collapse(true);
    end;
end;
procedure TfScriptFrame.FrameEnter(Sender: TObject);
begin
  ActionList1.State:=asNormal;
end;
procedure TfScriptFrame.FrameExit(Sender: TObject);
begin
  ActionList1.State:=asSuspended;
end;
procedure TfScriptFrame.ScriptStateChange(Sender: TObject);
begin
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfScriptFrame.mShortTextExit(Sender: TObject);
begin
  if pcPages.CanFocus then
    pcPages.SetFocus;
end;
procedure TfScriptFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;
procedure TfScriptFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'S');
    end;
  TfDocumentFrame(Sender).BaseElement := FDataSet;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfScriptFrame.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='SC';
  TfHistoryFrame(Sender).DataSet := TBaseScript(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfScriptFrame.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='SC';
  TfLinkFrame(Sender).DataSet := TBaseScript(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfScriptFrame.acCancelExecute(Sender: TObject);
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
end;
procedure TfScriptFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;

procedure TfScriptFrame.acCopyExecute(Sender: TObject);
begin

end;

procedure TfScriptFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      Application.ProcessMessages;
      DataSet.Delete;
      FDataSet.CascadicCancel;
      if UseTransactions then
        begin
          Data.CommitTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;
procedure TfScriptFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfScriptFrame.DoOpen(RefreshVersions : Boolean = True);
var
  aDocuments: TDocuments;
  s: TStream;
  GraphExt: String;
  aDocFrame: TfDocumentFrame;
  Rec: LargeInt;
  aFilter: String;
  aType: Char;
  tmp: String;
  aFound: Boolean;
  aWiki: TWikiList;
  aWikiPage: TfWikiFrame;
  aWikiIdx: Integer;
  aID: String;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  pcPages.CloseAll;
  TBaseScript(DataSet).OpenItem;
  TabCaption := TBaseScript(FDataSet).Text.AsString;
  Script.DataSet := DataSet.DataSet;
  SetRights;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsLargeInt,'E',DataSet.Id.AsVariant);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          pcPages.AddTab(aDocFrame,False);
          aDocFrame.DataSet := aDocuments;
          aDocFrame.BaseElement := FDataSet;
        end;
    end;
  pcPages.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  TBaseScript(DataSet).History.Open;
  if TBaseScript(DataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TBaseScript(DataSet).Links.Open;
  if TBaseScript(DataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
  with Application as TBaseVisualApplication do
    AddTabClasses('SCR',pcPages);
  with Application as TBaseVisualApplication do
    AddTabs(pcPages);
  if DataSet.State<> dsInsert then
    begin
      aWiki := TWikiList.Create(nil,Data);
      if aWiki.FindWikiFolder('Promet-ERP-Help/forms/'+Self.ClassName+'/') then
        begin
          while not aWiki.EOF do
            begin
              aWikiPage := TfWikiFrame.Create(Self);
              aID := IntToStr(Int64(DataSet.Id.AsVariant));
              aWikiPage.Variables.Values['SQL_ID'] := aID;
              aWikiPage.Variables.Values['ID'] := TBaseDbList(DataSet).Number.AsString;
              aWikiPage.Variables.Values['TEXT'] := TBaseDbList(DataSet).Text.AsString;
              aWikiIdx := -1;
              if Assigned(TBaseDbList(DataSet).Status) then
                aWikiPage.Variables.Values['STATUS'] := TBaseDbList(DataSet).Status.AsString;
              if aWikiPage.OpenWikiPage('Promet-ERP-Help/forms/'+Self.ClassName+'/'+aWiki.Text.AsString) then
                begin
                  aWikiIdx := pcPages.AddTab(aWikiPage,False,aWiki.FieldByName('CAPTION').AsString);
                  aWikiPage.SetRights(FEditable);
                end
              else aWikiPage.Free;
              if aWiki.FieldByName('CAPTION').AsString = strOverview then
                begin
                  pcPages.Pages[aWikiIdx+1].PageIndex:=0;
                  pcPages.PageIndex:=0;
                end;
              aWikiPage.LeftBar:=True;
              aWiki.Next;
            end;
        end;
      aWiki.Free;
    end;
  if HasHelp then AddHelp(Self);
end;
function TfScriptFrame.SetRights: Boolean;
begin
  FEditable := (Data.Users.Rights.Right('MASTERDATA') > RIGHT_READ) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_READ) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_READ) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_READ);
  Result := FEditable;
  acDelete.Enabled:=FEditable and ((Data.Users.Rights.Right('MASTERDATA') > RIGHT_WRITE) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_WRITE) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_WRITE) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_WRITE));
  acPaste.Enabled:=FEditable;
  acRights.Enabled:=Data.Users.Rights.Right('MASTERDATA') >= RIGHT_PERMIT;
end;
constructor TfScriptFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor:=TfScriptEditor.Create(Self);
  FEditor.BorderStyle:=bsNone;
  FEditor.Parent:=tsScript;
  FEditor.Align:=alClient;
  FEditor.acSave.Visible:=False;
  fEditor.lName.Visible:=false;
  fEditor.eName.Visible:=false;
end;
destructor TfScriptFrame.Destroy;
begin
  FEditor.Free;
  if Assigned(FConnection) then
    begin
      CloseConnection(acSave.Enabled);
      DataSet.Destroy;
      DataSet := nil;
      FreeAndNil(FConnection);
    end;
  inherited Destroy;
end;
function TfScriptFrame.OpenFromLink(aLink: string) : Boolean;
begin
  inherited;
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  if UseTransactions then
    Data.StartTransaction(FConnection);
  DataSet := TBaseScript.Create(Self,Data,FConnection);
  DataSet.OnChange:=@ScriptStateChange;
  TBaseDbList(DataSet).SelectFromLink(aLink);
  Dataset.Open;
  DoOpen;
  Result := True;
end;

procedure TfScriptFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  FEditor.DataSet := TBaseScript(AValue);
end;

procedure TfScriptFrame.New;
begin
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  TabCaption := strNewArticle;
  if UseTransactions then
    Data.StartTransaction(FConnection);
  DataSet := TBaseScript.Create(Self,Data,FConnection);
  DataSet.OnChange:=@ScriptStateChange;
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
  acSave.Enabled := False;
  acCancel.Enabled:= False;
end;
procedure TfScriptFrame.SetLanguage;
begin
end;

procedure TfScriptFrame.FrameAdded;
begin
  inherited FrameAdded;
  FEditor.Show;
end;

end.
