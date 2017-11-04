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
unit uobjectframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LR_DBSet, LR_Class, Forms, Controls, ExtCtrls,
  ActnList, ComCtrls, StdCtrls, DbCtrls, Buttons, Menus, db, uPrometFrames,
  uExtControls, uFilterFrame, uIntfStrConsts, Utils, Dialogs, variants,
  uBaseDbClasses,uBaseDatasetInterfaces;
type

  { TfObjectFrame }

  TfObjectFrame = class(TPrometMainFrame)
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
    bAssignTree: TSpeedButton;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    bExecute: TSpeedButton;
    bShowTree: TSpeedButton;
    cbStatus: TComboBox;
    eArticleNumber: TDBEdit;
    eMatchCode: TDBEdit;
    gbTree: TGroupBox;
    History: TDatasource;
    iArticle: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lMatchCode: TLabel;
    lShortText: TLabel;
    MandantDetails: TDatasource;
    Element: TDatasource;
    Memo1: TDBMemo;
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
    mShortText: TDBMemo;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pComponents: TPanel;
    pcPages: TExtMenuPageControl;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pmAction: TPopupMenu;
    pNav1: TPanel;
    pmImage: TPopupMenu;
    pPreviewImage: TPanel;
    Report: TfrReport;
    sbAddImage: TSpeedButton;
    sbClipboardToImage: TSpeedButton;
    sbClipboardToImage1: TSpeedButton;
    sbMenue: TSpeedButton;
    tsCommon: TTabSheet;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acPasteImageExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure bChangeNumberClick(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure ElementStateChange(Sender: TObject);
    procedure mShortTextChange(Sender: TObject);
    procedure mShortTextExit(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
  private
    { private declarations }
    FEditable : Boolean;
    FMeasurement: TMeasurement;
    procedure AddMeasurement(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure AddImages(Sender: TObject);
    procedure AddLinks(Sender: TObject);
  protected
    procedure DoOpen(RefreshVersions : Boolean = True);
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
  end;
implementation
{$R *.lfm}
uses uMasterdata,uData,uArticlePositionFrame,uDocuments,uDocumentFrame,
  uHistoryFrame,uImageFrame,uLinkFrame,uBaseDbInterface,uListFrame,
  uArticleStorageFrame,uArticleRepairFrame,uArticleText,uCopyArticleData,
  uMainTreeFrame,uPrometFramesInplace,uarticlesupplierframe,
  uNRights,uSelectReport,uBaseVisualApplication,uWikiFrame,uWiki,ufinance,
  uthumbnails,Clipbrd,uscreenshotmain,uBaseApplication,umeasurements;
resourcestring
  strPrices                                  = 'Preise';
  strProperties                              = 'Eigenschaften';
  strStorage                                 = 'Lager';
  strSupplier                                = 'Lieferant';
  strRepair                                  = 'Reparatur';
  strTexts                                   = 'Texte';
  strChangeNumer                             = 'Nummer ändern';
  strNewArticle                              = 'neuer Artikel';
procedure TfObjectFrame.acSaveExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicPost;
      if UseTransactions then
        begin
          Data.CommitTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
end;
procedure TfObjectFrame.acScreenshotExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Application.ProcessMessages;
  Application.CreateForm(TfScreenshot,fScreenshot);
  with BaseApplication as IBaseApplication do
    fScreenshot.SaveTo:=AppendPathDelim(GetInternalTempDir)+'screenshot.jpg';
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  if DataSet.State=dsInsert then
    begin
      DataSet.Post;
      DataSet.Edit;
    end;
  pcPages.AddTab(TfImageFrame.Create(Self),False);
  aSheet := pcPages.GetTab(TfImageFrame);
  if Assigned(aSheet) then
    begin
      Application.ProcessMessages;
      with TfImageFrame(aSheet.Controls[0]) do
        begin
          if not DataSet.CanEdit then
            DataSet.Insert;
          with BaseApplication as IBaseApplication do
            iPreview.Picture.LoadFromFile(AppendPathDelim(GetInternalTempDir)+'screenshot.jpg');
          DataSet.Post;
        end;
      aThumbnails := TThumbnails.Create(nil);
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      while aThumbnails.Count>0 do
        aThumbnails.Delete;
      TMasterdata(DataSet).GenerateThumbnail;
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      if aThumbnails.Count>0 then
        begin
          aStream := TMemoryStream.Create;
          Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
          aStream.Position:=0;
          iArticle.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
          aStream.Free;
          acPasteImage.Visible:=False;
          acAddImage.Visible:=False;
          acScreenshot.Visible:=False;
        end
      else
        begin
          iArticle.Picture.Clear;
          acPasteImage.Visible:=True;
          acAddImage.Visible:=True;
          acScreenshot.Visible:=True;
        end;
      aThumbnails.Free;
    end;

  Application.MainForm.Show;
end;
procedure TfObjectFrame.acSetTreeDirExecute(Sender: TObject);
begin
  if fMainTreeFrame.GetTreeEntry = -1 then exit;
  with DataSet.DataSet do
    begin
      Edit;
      FieldbyName('TREEENTRY').AsVariant:=fMainTreeFrame.GetTreeEntry;
      fMainTreeFrame.tvMain.Selected.Collapse(true);
    end;
end;
procedure TfObjectFrame.bChangeNumberClick(Sender: TObject);
var
  str: String;
begin
  str := DataSet.FieldByName('NUMBER').AsString;
  if InputQuery(strChangeNumer,strnewNumber,str) and (str <> DataSet.FieldByName('NUMBER').AsString) then
    begin
      with DataSet.DataSet do
        begin
          Edit;
          FieldbyName('NUMBER').AsString:=str;
        end;
    end;
end;

procedure TfObjectFrame.cbStatusSelect(Sender: TObject);
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
procedure TfObjectFrame.ElementStateChange(Sender: TObject);
begin
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfObjectFrame.mShortTextChange(Sender: TObject);
var
  tmp: AnsiString;
begin
  if mShortText.Lines.Count > 0 then
    TabCaption := mShortText.Lines[0];
  tmp := StringReplace(UpperCase(StringReplace(ValidateFileName(mShorttext.Text),'_','',[rfReplaceAll])),' ','',[rfReplaceAll]);
  tmp := StringReplace(tmp,'-','',[rfReplaceAll]);
  if (copy(tmp,0,length(eMatchCode.Text)) = eMatchCode.Text)
  or ((length(tmp) < length(eMatchCode.text)) and (copy(eMatchCode.Text,0,length(tmp)) = tmp)) then
    if Assigned(eMatchCode.Field) then
      begin
        tmp := copy(tmp,0,eMatchCode.Field.Size);
        eMatchCode.Text := tmp;
      end;
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfObjectFrame.mShortTextExit(Sender: TObject);
begin
  if pcPages.CanFocus then
    pcPages.SetFocus;
end;
procedure TfObjectFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfObjectFrame.AddMeasurement(Sender: TObject);
begin
  TfMeasurementFrame(Sender).DataSet := FMeasurement;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfObjectFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'E',DataSet.FieldByName('NUMBER').AsString,DataSet.FieldByName('VERSION').AsVariant,DataSet.FieldByName('LANGUAGE').AsVariant,0);
    end;
  TfDocumentFrame(Sender).BaseElement := FDataSet;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfObjectFrame.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='OB';
  TfHistoryFrame(Sender).DataSet := TObjects(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfObjectFrame.AddImages(Sender: TObject);
begin
  TfImageFrame(Sender).DataSet := TObjects(FDataSet).Images;
  TObjects(FDataSet).Images.Open;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfObjectFrame.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='OB';
  TfLinkFrame(Sender).DataSet := TObjects(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfObjectFrame.acCancelExecute(Sender: TObject);
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
procedure TfObjectFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;

procedure TfObjectFrame.acCopyExecute(Sender: TObject);
begin

end;

procedure TfObjectFrame.acDeleteExecute(Sender: TObject);
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

procedure TfObjectFrame.acPasteImageExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  if Clipboard.HasPictureFormat then
    begin
      pcPages.AddTab(TfImageFrame.Create(Self),False);
      aSheet := pcPages.GetTab(TfImageFrame);
      if Assigned(aSheet) then
        begin
          Application.ProcessMessages;
          TfImageFrame(aSheet.Controls[0]).acPaste.Execute;
          TfImageFrame(aSheet.Controls[0]).DataSet.Post;
          aThumbnails := TThumbnails.Create(nil);
          aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
          aThumbnails.Open;
          while aThumbnails.Count>0 do
            aThumbnails.Delete;
          TObjects(DataSet).GenerateThumbnail;
          aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
          aThumbnails.Open;
          if aThumbnails.Count>0 then
            begin
              aStream := TMemoryStream.Create;
              Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
              aStream.Position:=0;
              iArticle.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
              aStream.Free;
              acPasteImage.Visible:=False;
              acAddImage.Visible:=False;
              acScreenshot.Visible:=False;
            end
          else
            begin
              iArticle.Picture.Clear;
              acPasteImage.Visible:=True;
              acAddImage.Visible:=True;
              acScreenshot.Visible:=True;
            end;
          aThumbnails.Free;
        end;
    end;
end;

procedure TfObjectFrame.acPrintExecute(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  fSelectReport.Report := Report;
  fSelectReport.SetLanguage;
  if Supports(FDataSet, IBaseHistory, Hist) then
    History.DataSet := Hist.GetHistory.DataSet;
  MandantDetails.DataSet:=Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  PList.DataSet := DataSet.DataSet;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      fSelectReport.ReportType := 'MAS';
    end;
  fSelectReport.Showmodal;
end;

procedure TfObjectFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfObjectFrame.DoOpen(RefreshVersions : Boolean = True);
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
  aMeasurement: TMeasurement;
begin
  pcPages.CloseAll;
  TObjects(DataSet).OpenItem;
  TabCaption := TObjects(FDataSet).Text.AsString;
  Element.DataSet := DataSet.DataSet;
  SetRights;
  aType := 'E';
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
  pcPages.AddTabClass(TfMeasurementFrame,strMeasurement,@AddMeasurement);
  FreeAndNil(FMeasurement);
  FMeasurement := TMeasurement.CreateEx(nil,Data,DataSet.Connection,DataSet.DataSet);
  FMeasurement.CreateTable;
  FMeasurement.Open;
  if FMeasurement.Count>0 then
    pcPages.AddTab(TfMeasurementFrame.Create(Self),False);
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
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
  TObjects(DataSet).History.Open;
  if TObjects(DataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  if not TObjects(DataSet).Images.DataSet.Active then
    TObjects(DataSet).Images.DataSet.Open;
  pcPages.AddTabClass(TfImageFrame,strImages,@AddImages);
  if (FDataSet.State = dsInsert) or (TObjects(DataSet).Images.Count > 0) then
    pcPages.AddTab(TfImageFrame.Create(Self),False);
  TObjects(DataSet).Images.DataSet.Close;
  aThumbnails := TThumbnails.Create(nil);
  aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
  aThumbnails.Open;
  if aThumbnails.Count>0 then
    begin
      aStream := TMemoryStream.Create;
      Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
      aStream.Position:=0;
      iArticle.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
      aStream.Free;
      acPasteImage.Visible:=False;
      acAddImage.Visible:=False;
      acScreenshot.Visible:=False;
    end
  else
    begin
      iArticle.Picture.Clear;
      acPasteImage.Visible:=True;
      acAddImage.Visible:=True;
      acScreenshot.Visible:=True;
    end;
  aThumbnails.Free;
  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TObjects(DataSet).Links.Open;
  if TObjects(DataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
  mShorttext.SetFocus;
  with Application as TBaseVisualApplication do
    AddTabClasses('OBJ',pcPages);
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
              if aWikiPage.OpenWikiPage('Promet-ERP-Help/forms/'+Self.ClassName+'/'+aWiki.FieldByName('NAME').AsString) then
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
  if HasHelp then AddHelp(Self);
end;
function TfObjectFrame.SetRights: Boolean;
begin
  FEditable := (Data.Users.Rights.Right('MASTERDATA') > RIGHT_READ) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_READ) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_READ) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_READ);
  Result := FEditable;
  acDelete.Enabled:=FEditable and ((Data.Users.Rights.Right('MASTERDATA') > RIGHT_WRITE) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_WRITE) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_WRITE) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_WRITE));
  acPaste.Enabled:=FEditable;
  acRights.Enabled:=Data.Users.Rights.Right('MASTERDATA') >= RIGHT_PERMIT;

  pComponents.Enabled := FEditable;
end;
constructor TfObjectFrame.Create(AOwner: TComponent);
var
  aType: Char;
begin
  inherited Create(AOwner);
  mShortText.WantTabs:=False;
end;
destructor TfObjectFrame.Destroy;
begin
  if Assigned(FConnection) then
    begin
      if Assigned(FMeasurement) then
        FreeAndNil(FMeasurement);
      CloseConnection(acSave.Enabled);
      DataSet.Destroy;
      DataSet := nil;
      FreeAndNil(FConnection);
    end;
  inherited Destroy;
end;
function TfObjectFrame.OpenFromLink(aLink: string) : Boolean;
begin
  inherited;
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  if UseTransactions then
    Data.StartTransaction(FConnection);
  DataSet := TObjects.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@ElementStateChange;
  TBaseDbList(DataSet).SelectFromLink(aLink);
  Dataset.Open;
  DoOpen;
  Result := True;
end;
procedure TfObjectFrame.New;
begin
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  TabCaption := strNewArticle;
  if UseTransactions then
    Data.StartTransaction(FConnection);
  DataSet := TObjects.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@ElementStateChange;
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
  acSave.Enabled := False;
  acCancel.Enabled:= False;
end;
procedure TfObjectFrame.SetLanguage;
begin
end;
end.
