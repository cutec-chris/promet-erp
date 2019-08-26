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
    acSetActiveObject: TAction;
    acImport: TAction;
    ActionList1: TActionList;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    cbActive: TDBCheckBox;
    cbStatus: TComboBox;
    cbVersion: TComboBox;
    DBMemo1: TDBMemo;
    eArticleNumber: TDBEdit;
    History: TDatasource;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MandantDetails: TDatasource;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel8: TPanel;
    Panel9: TPanel;
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
    pcPages: TExtMenuPageControl;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pmAction: TPopupMenu;
    pmImage: TPopupMenu;
    Report: TfrReport;
    sbMenue: TSpeedButton;
    tsCommon: TTabSheet;
    tsScript: TTabSheet;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetActiveObjectExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure cbVersionExit(Sender: TObject);
    procedure cbVersionSelect(Sender: TObject);
    procedure eArticleNumberChange(Sender: TObject);
    procedure FEditorOpenUnit(aUnitName: string; X, Y: Integer);
    function fSearchOpenItem(aLink: string): Boolean;
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
    procedure ShowFrame; override;
  end;
implementation
{$R *.lfm}
uses uMasterdata,uData,uArticlePositionFrame,uDocuments,uDocumentFrame,
  uHistoryFrame,uImageFrame,uLinkFrame,uBaseDbInterface,uListFrame,
  uArticleStorageFrame,uArticleRepairFrame,uArticleText,uCopyArticleData,
  uMainTreeFrame,uPrometFramesInplace,uarticlesupplierframe,
  uNRights,uBaseVisualApplication,uWikiFrame,uWiki,ufinance,
  uthumbnails,Clipbrd,uscreenshotmain,uBaseApplication,uprometscripts,
  uprometpascalscript,uBaseDatasetInterfaces,uSearch,uscriptimport;
resourcestring
  strPrices                                  = 'Preise';
  strProperties                              = 'Eigenschaften';
  strStorage                                 = 'Lager';
  strSupplier                                = 'Lieferant';
  strRepair                                  = 'Reparatur';
  strTexts                                   = 'Texte';
  strChangeNumer                             = 'Nummer ändern';
  strNewArticle                              = 'neuer Artikel';
  strShouldThisVersionated                   = 'Soll das Script versioniert werden ?';
  strNewScript                               = 'Neues Script';
procedure TfScriptFrame.acSaveExecute(Sender: TObject);
begin
  Save;
end;

procedure TfScriptFrame.acSetActiveObjectExecute(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'SCA','');
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

procedure TfScriptFrame.cbStatusSelect(Sender: TObject);
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

procedure TfScriptFrame.cbVersionExit(Sender: TObject);
var
  TargetVer: String;
  Version : string;
  nVersion: String;
begin
  Version := '';
  if cbVersion.Text <> '' then
    Version := cbVersion.Text;
  nVersion := Dataset.FieldByName('VERSION').AsString;
  if nVersion <> Version then
    begin //New Version
      if (MessageDlg(strVersion,strShouldThisVersionated,mtInformation,[mbYes,mbNo],0)<>mrYes) or (not TBaseScript(DataSet).Versionate(Version)) then
        cbVersion.Text := DataSet.FieldByName('VERSION').AsString
      else
        begin
          cbVersion.OnExit:=nil;
          cbVersion.Items.Add(TargetVer);
          cbVersion.Text:=TargetVer;
          DoOpen;
          FDataSet.Change;
          cbVersion.OnExit:=@cbVersionExit;
        end;
    end;
end;

procedure TfScriptFrame.cbVersionSelect(Sender: TObject);
var
  aId: String;
  aLanguage : Variant;
begin
  aId := TBaseScript(DataSet).Text.AsString;
  if UseTransactions then
    CloseConnection;
  Screen.Cursor:=crHourglass;
  application.ProcessMessages;
  if UseTransactions then
    Data.StartTransaction(FConnection);
  if not TBaseScript(DataSet).Locate('VERSION',cbVersion.Text,[]) then
    if cbVersion.Text='' then
      TBaseScript(DataSet).Locate('VERSION',Null,[]);
  DoOpen(False);
  Screen.Cursor:=crDefault;
end;

procedure TfScriptFrame.eArticleNumberChange(Sender: TObject);
begin
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;

procedure TfScriptFrame.FEditorOpenUnit(aUnitName: string; X, Y: Integer);
var
  aScript: TBaseScript;
begin
  aScript := TBaseScript.Create(nil);
  if pos('_',aUnitName)>0 then
    aScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(copy(aUnitName,0,pos('_',aUnitName)-1))+' AND UPPER('+data.QuoteField('VERSION')+')=UPPER('+Data.QuoteValue(copy(aUnitName,pos('_',aUnitName)+1,length(aUnitName)))+')')
  else
    aScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aUnitName)+' AND '+data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y'));
  if aScript.Count>0 then
    Data.GotoLink('SCRIPTS@'+aScript.Id.AsString);
  aScript.Free;
end;
function TfScriptFrame.fSearchOpenItem(aLink: string): Boolean;
var
  aDataSetClass: TBaseDBDatasetClass;
  aDataSet: TBaseDBDataset;
begin
  if TBaseDBModule(Data).DataSetFromLink(aLink,aDataSetClass) then
    begin
      aDataSet := aDataSetClass.CreateEx(nil,Data);
      TBaseDbList(aDataSet).SelectFromLink(aLink);
      aDataSet.Open;
      if aDataSet.Count>0 then
        TBaseScript(FDataSet).ActualObject := aDataSet
      else
        aDataSet.Free;
    end;
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
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'S',0);
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
  Abort;
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
      Abort;
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;

procedure TfScriptFrame.acImportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icImport,'S',FDataSet) then
    begin
      DataSet.DataSet.Refresh;
      DoOpen;
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
  try
    TBaseScript(DataSet).OpenItem;
  except
  end;
  TabCaption := TBaseScript(FDataSet).Text.AsString;
  Script.DataSet := DataSet.DataSet;
  SetRights;
  if Script.DataSet.State <> dsInsert then
    begin
      if Refreshversions then
        begin
          Rec := DataSet.GetBookmark;
          Script.DataSet.DisableControls;
          with DataSet.DataSet as IBaseDbFilter do
            begin
              aFilter := Filter;
              Filter := Data.QuoteField('NAME')+'='+Data.QuoteValue(DataSet.FieldByName('NAME').AsString);
            end;
          DataSet.Open;
          DataSet.DataSet.First;
          cbVersion.Items.Clear;
          while not DataSet.DataSet.EOF do
            begin
              cbVersion.Items.Add(DataSet.FieldByName('VERSION').AsString);
              DataSet.DataSet.Next;
            end;
          DataSet.GotoBookmark(Rec);
          cbVersion.Text:=DataSet.FieldByName('VERSION').AsString;
          Script.DataSet.EnableControls;
        end;
    end;

  aType := 'S';
  cbStatus.Items.Clear;
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

  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsLargeInt,'S');
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
      aWiki := TWikiList.Create(nil);
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
              if aWikiPage.OpenWikiPage('Promet-ERP-Help/forms/'+Self.ClassName+'/'+aWiki.FieldByName('NAME').AsString) and aWikiPage.HasContent then
                begin
                  aWikiIdx := pcPages.AddTab(aWikiPage,False,aWiki.FieldByName('CAPTION').AsString);
                  aWikiPage.SetRights(FEditable);
                  aWikiPage.LeftBar:=True;
                end
              else aWikiPage.Free;
              if (aWiki.FieldByName('CAPTION').AsString = strOverview) or (Uppercase(aWiki.FieldByName('NAME').AsString)='OVERVIEW')  then
                begin
                  pcPages.Pages[aWikiIdx+1].PageIndex:=0;
                  pcPages.PageIndex:=0;
                end;
              aWiki.Next;
            end;
        end;
      aWiki.Free;
    end;
  tsScript.PageIndex:=1;
  pcPages.ActivePage:=tsScript;
  if HasHelp then AddHelp(Self);
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
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
  FEditor.OnOpenUnit:=@FEditorOpenUnit;
end;
destructor TfScriptFrame.Destroy;
begin
  FEditor.Free;
  if Assigned(DataSet) then
    begin
      DataSet.Destroy;
      DataSet := nil;
    end;
  inherited Destroy;
end;
function TfScriptFrame.OpenFromLink(aLink: string) : Boolean;
begin
  inherited;
  Result := False;
  DataSet := TBaseScript.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@ScriptStateChange;
  TBaseDbList(DataSet).SelectFromLink(aLink);
  Dataset.Open;
  DataSet.Locate('ACTIVE','Y',[]);
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
  Inherited;
  TabCaption := strNewScript;
  DataSet := TBaseScript.CreateEx(Self,Data,FConnection);
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

procedure TfScriptFrame.ShowFrame;
begin
  inherited ShowFrame;
  if Assigned(FEditor) then
    FEditor.FLastCompileStamp:=0; //reset Compiling when we come from other Tab we dont know if user has changes somethiong to an dependency
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfScriptFrame);
end.
