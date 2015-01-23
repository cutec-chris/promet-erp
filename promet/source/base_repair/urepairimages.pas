unit urepairimages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, StdCtrls, DBGrids, ButtonPanel, ComCtrls, ExtCtrls, Buttons,
  uExtControls, uOrder, variants,ActnList,ExtDlgs;

type

  { TfRepairImages }

  TfRepairImages = class(TForm)
    acPasteImage: TAction;
    acAddImage: TAction;
    acScreenshot: TAction;
    ActionList1: TActionList;
    ButtonPanel1: TButtonPanel;
    cbStatus: TComboBox;
    RepairImage: TDatasource;
    DBNavigator1: TDBNavigator;
    eName: TDBEdit;
    gList: TDBGrid;
    eFilter: TEdit;
    gProblems: TExtDBGrid;
    iArticle: TImage;
    iPreview: TDBImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    lInternalNotes: TLabel;
    lNotesforCustomer: TLabel;
    lNotesforCustomer1: TLabel;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mErrordesc: TDBMemo;
    mInternalNotes: TDBMemo;
    mNotes: TDBMemo;
    mSolve: TDBMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    pCommon: TPanel;
    pcPages: TExtMenuPageControl;
    pmImage: TPopupMenu;
    pPreviewImage: TPanel;
    RepairDetail: TDataSource;
    sbAddImage: TSpeedButton;
    sbClipboardToImage: TSpeedButton;
    sbClipboardToImage1: TSpeedButton;
    tsAdditional: TTabSheet;
    tsCommon: TTabSheet;
    procedure AddLinks(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    procedure AddImages(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure DataSetDataSetAfterScroll(DataSet: TDataSet);
    procedure RepairImageStateChange(Sender: TObject);
    procedure eFilterEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acPasteImageExecute(Sender: TObject);
    procedure acAddImageExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FDataSet: TOrderRepairImages;
    procedure SetDataSet(AValue: TOrderRepairImages);
    procedure DoOpen;
  public
    { public declarations }
    procedure SetLanguage;
    function Execute : Boolean;
    property DataSet : TOrderRepairImages read FDataSet write SetDataSet;
    constructor Create(TheOwner: TComponent); override;
  end;

var
  fRepairImages: TfRepairImages;

implementation
uses uData,uHistoryFrame,uImageFrame,uIntfStrConsts,uDocuments,uDocumentFrame,uLinkFrame,
  uthumbnails,Clipbrd,uscreenshotmain,uBaseApplication;
{$R *.lfm}

{ TfRepairImages }

procedure TfRepairImages.FormCreate(Sender: TObject);
begin
  DataSet := TOrderRepairImages.Create(nil);
  DataSet.CreateTable;
  DataSet.Open;
end;
procedure TfRepairImages.acAddImageExecute(Sender: TObject);
var
  aThumbnails: TThumbnails;
  aStream: TFileStream;
  sThumb: TMemoryStream;
begin
  if OpenpictureDialog1.Execute then
    begin
      if DataSet.State=dsInsert then
        begin
          DataSet.Post;
          DataSet.Edit;
        end;
      aThumbnails := TThumbnails.Create(nil);
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      while aThumbnails.Count>0 do
        aThumbnails.Delete;
      with BaseApplication as IBaseApplication do
        aStream := TFileStream.Create(OpenPictureDialog1.FileName,fmOpenRead);
      sThumb := TMemoryStream.Create;
      if uthumbnails.GenerateThumbNail('.jpg',aStream,sThumb,'') then
        begin
          aThumbnails.Insert;
          aThumbnails.FieldByName('REF_ID_ID').AsVariant:=DataSet.Id.AsVariant;
          if sThumb.Size>0 then
            Data.StreamToBlobField(sThumb,aThumbnails.DataSet,'THUMBNAIL');
          aThumbnails.Post;
        end;
      aStream.Free;
      if aThumbnails.Count>0 then
        begin
          sThumb  := TMemoryStream.Create;
          Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',sThumb);
          sThumb.Position:=0;
          iArticle.Picture.LoadFromStreamWithFileExt(sThumb,'jpg');
          sThumb.Free;
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
procedure TfRepairImages.acPasteImageExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
  Bitmap : TBitmap;
  sThumb: TMemoryStream;
begin
  if Clipboard.HasPictureFormat then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.LoadFromClipboardFormat(Clipboard.FindPictureFormatID);
      aStream := TMemoryStream.Create;
      Bitmap.SaveToStream(aStream);
      aStream.Position:=0;
      aThumbnails := TThumbnails.Create(nil);
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      while aThumbnails.Count>0 do
        aThumbnails.Delete;
      sThumb := TMemoryStream.Create;
      if uthumbnails.GenerateThumbNail('.bmp',aStream,sThumb,'') then
        begin
          aThumbnails.Insert;
          aThumbnails.FieldByName('REF_ID_ID').AsVariant:=DataSet.Id.AsVariant;
          if sThumb.Size>0 then
            Data.StreamToBlobField(sThumb,aThumbnails.DataSet,'THUMBNAIL');
          aThumbnails.Post;
        end;
      aStream.Free;
      if aThumbnails.Count>0 then
        begin
          sThumb  := TMemoryStream.Create;
          Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',sThumb);
          sThumb.Position:=0;
          iArticle.Picture.LoadFromStreamWithFileExt(sThumb,'jpg');
          sThumb.Free;
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

procedure TfRepairImages.FormDestroy(Sender: TObject);
begin
  DataSet.Destroy;
end;

procedure TfRepairImages.acScreenshotExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TFileStream;
  sThumb: TMemoryStream;
begin
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Self.Hide;
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
  aThumbnails := TThumbnails.Create(nil);
  aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
  aThumbnails.Open;
  while aThumbnails.Count>0 do
    aThumbnails.Delete;
  with BaseApplication as IBaseApplication do
    aStream := TFileStream.Create(AppendPathDelim(GetInternalTempDir)+'screenshot.jpg',fmOpenRead);
  sThumb := TMemoryStream.Create;
  if uthumbnails.GenerateThumbNail('.jpg',aStream,sThumb,'') then
    begin
      aThumbnails.Insert;
      aThumbnails.FieldByName('REF_ID_ID').AsVariant:=DataSet.Id.AsVariant;
      if sThumb.Size>0 then
        Data.StreamToBlobField(sThumb,aThumbnails.DataSet,'THUMBNAIL');
      aThumbnails.Post;
    end;
  aStream.Free;
  if aThumbnails.Count>0 then
    begin
      sThumb  := TMemoryStream.Create;
      Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',sThumb);
      sThumb.Position:=0;
      iArticle.Picture.LoadFromStreamWithFileExt(sThumb,'jpg');
      sThumb.Free;
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
  Application.MainForm.Show;
  Self.Show;
end;
procedure TfRepairImages.RepairImageStateChange(Sender: TObject);
begin
  if DataSet.State=dsInsert then
    eName.SetFocus;
end;

procedure TfRepairImages.AddImages(Sender: TObject);
begin
  TfImageFrame(Sender).DataSet := TOrderRepairImages(FDataSet).Images;
  TOrderRepairImages(FDataSet).Images.Open;
end;

procedure TfRepairImages.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='REPI';
  TfLinkFrame(Sender).DataSet := TOrderRepairImages(FDataSet).Links;
end;

procedure TfRepairImages.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='REPI';
  TfHistoryFrame(Sender).DataSet := TOrderRepairImages(FDataSet).History;
  TfHistoryFrame(Sender).SetRights(True);
end;

procedure TfRepairImages.cbStatusSelect(Sender: TObject);
var
  tmp: String;
begin
  tmp := copy(cbStatus.text,pos('(',cbStatus.text)+1,length(cbStatus.text));
  tmp := copy(tmp,0,pos(')',tmp)-1);
  if not FDataSet.CanEdit then FDataSet.DataSet.Edit;
  FDataSet.FieldByName('STATUS').AsString:=tmp;
  FDataSet.Post;
  DoOpen;
end;

procedure TfRepairImages.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'R');
    end;
  TfDocumentFrame(Sender).BaseElement := FDataSet;
end;

procedure TfRepairImages.DataSetDataSetAfterScroll(DataSet: TDataSet);
begin
  Application.ProcessMessages;
  DoOpen;
end;

procedure TfRepairImages.eFilterEnter(Sender: TObject);
begin
  DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+fRepairImages.eFilter.Text+'*'))+') OR UPPER('+Data.ProcessTerm(Data.QuoteField('SYMTOMS')+')=UPPER('+Data.QuoteValue('*'+fRepairImages.eFilter.Text+'*'))+')');
end;

procedure TfRepairImages.SetDataSet(AValue: TOrderRepairImages);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  RepairImage.DataSet := AValue.DataSet;
  RepairDetail.DataSet := AValue.RepairDetail.DataSet;
end;

procedure TfRepairImages.DoOpen;
var
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  aType: Char;
  aFound: Boolean;
  aThumbnails: TThumbnails;
  tmp: String;
  aStream: TMemoryStream;
begin
  pcPages.CloseAll;
  pcPages.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  TOrderRepairImages(DataSet).History.Open;
  if TOrderRepairImages(DataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  if not TOrderRepairImages(DataSet).Images.DataSet.Active then
    TOrderRepairImages(DataSet).Images.DataSet.Open;
   pcPages.AddTabClass(TfImageFrame,strImages,@AddImages);
  if (TOrderRepairImages(DataSet).Images.Count > 0) then
    pcPages.AddTab(TfImageFrame.Create(Self),False);
  TOrderRepairImages(DataSet).Images.DataSet.Close;
  TOrderRepairImages(DataSet).RepairDetail.Open;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsLargeInt,'R');
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

  cbStatus.Items.Clear;
  cbStatus.Text := '';
  aType := 'R';
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
//  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
//  TOrderRepairImages(DataSet).Links.Open;
//  if TOrderRepairImages(DataSet).Links.Count > 0 then
//    pcPages.AddTab(TfLinkFrame.Create(Self),False);
end;

procedure TfRepairImages.SetLanguage;
begin
  if not Assigned(fRepairImages) then
    begin
      Application.CreateForm(TfRepairImages,fRepairImages);
      Self := fRepairImages;
    end;

end;

function TfRepairImages.Execute: Boolean;
begin
  if not Assigned(fRepairImages) then
    begin
      Application.CreateForm(TfRepairImages,fRepairImages);
      Self := fRepairImages;
    end;
  DataSet.DataSet.AfterScroll:=@DataSetDataSetAfterScroll;
  DoOpen;
  Result := fRepairImages.ShowModal = mrOK;
  if Result and DataSet.CanEdit then
    DataSet.Post;
end;

constructor TfRepairImages.Create(TheOwner: TComponent);
var
  i: Integer;
  aRepairProblems: TRepairProblems;
begin
  inherited Create(TheOwner);
  for i := 0 to gproblems.Columns.Count-1 do
    if TColumn(gProblems.Columns[i]).FieldName = 'ERROR' then
      begin
        aRepairProblems := TRepairProblems.Create(nil);
        aRepairProblems.CreateTable;
        aRepairProblems.Open;
        with aRepairProblems.DataSet do
          begin
            First;
            TColumn(gProblems.Columns[i]).PickList.Clear;
            while not EOF do
              begin
                TColumn(gProblems.Columns[i]).PickList.Add(FieldByName('PROBLEM').AsString);
                next;
              end;
          end;
        aRepairproblems.Free;
      end;
end;

end.

