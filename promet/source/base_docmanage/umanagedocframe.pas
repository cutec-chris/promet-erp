{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 26.03.2013
*******************************************************************************}
unit umanagedocframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  Buttons, ComCtrls, ActnList, thumbcontrol, uPrometFrames, uBaseDocPages,
  uBaseDBInterface, threadedimageLoader, uDocumentFrame, DBZVDateTimePicker,
  Dialogs, PairSplitter, Menus, uIntfStrConsts, variants, types, uTimeLine,
  uPreviewFrame;

type
  TfManageDocFrame = class(TPrometMainFrame)
    acDelete: TAction;
    acRefresh: TAction;
    acSetTag: TAction;
    acRebuildThumb: TAction;
    ActionList1: TActionList;
    bEditFilter: TSpeedButton;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    bExecute1: TSpeedButton;
    bRefresh2: TSpeedButton;
    bTag: TSpeedButton;
    bTag1: TSpeedButton;
    bZoomIn: TSpeedButton;
    bZoomOut: TSpeedButton;
    cbFilter: TComboBox;
    Datasource1: TDatasource;
    DBEdit1: TDBEdit;
    DBZVDateTimePicker1: TDBZVDateTimePicker;
    eSearch: TEdit;
    ExtRotatedLabel1: TLabel;
    ExtRotatedLabel2: TLabel;
    ExtRotatedLabel3: TLabel;
    ExtRotatedLabel4: TLabel;
    ExtRotatedLabel5: TLabel;
    IdleTimer1: TIdleTimer;
    iNoThumbnail: TImage;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pcPages: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pDocFrame: TPanel;
    pNav: TPanel;
    pNav1: TPanel;
    pmPopup: TPopupMenu;
    pNav2: TPanel;
    PopupMenu1: TPopupMenu;
    pRight: TPanel;
    pThumb: TPanel;
    pToolbar: TPanel;
    spPages: TSplitter;
    tbMenue1: TToolButton;
    tbToolBar: TToolBar;
    ThumbControl1: TThumbControl;
    tsDocument: TTabSheet;
    tsFiles: TTabSheet;
    procedure acDeleteExecute(Sender: TObject);
    procedure acRebuildThumbExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acSetTagExecute(Sender: TObject);
    procedure bExecute1Click(Sender: TObject);
    procedure bTag1Click(Sender: TObject);
    procedure bZoomInClick(Sender: TObject);
    procedure bZoomOutClick(Sender: TObject);
    procedure DoOnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure eSearchChange(Sender: TObject);
    procedure FDocFrameAftercheckInFiles(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure FTimeLineSetMarker(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure pmPopupPopup(Sender: TObject);
    procedure tbMenue1Click(Sender: TObject);
    procedure ThumbControl1DblClick(Sender: TObject);
    procedure ThumbControl1ImageLoaderManagerBeforeStartQueue(Sender: TObject);
    procedure ThumbControl1ImageLoaderManagerSetItemIndex(Sender: TObject);
    procedure ThumbControl1LoadFile(Sender: TObject; URL: string; out
      Stream: TStream);
    procedure ThumbControl1SelectItem(Sender: TObject; Item: TThreadedImage);
  private
    { private declarations }
    FLast : string;
    FFetchDS : TDataSet;
    FFetchSQL : string;
    FURL : string;
    aTag: String;
    aDate: String;
    FtempPath : string;
    FDocFrame: TfDocumentFrame;
    FTimeLine: TTimeLine;
    PreviewFrame: TfPreview;
    procedure FetchNext;
    procedure WaitForImage;
    procedure RebuidThumb;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure DoRefresh; override;
  end;

implementation
{$R *.lfm}
uses uData,udocuments,uWait,LCLIntf,Utils,uFormAnimate,uImportImages,
  ProcessUtils;
resourcestring
  strTag                   = 'Tag';
  strSetTag                = 'durch Klick setzen';
  strCantAccessFile        = 'Auf die Datei "%s" kann nicht zugegriffen werden,'+lineending+'kopieren Sie diese Datei ggf erst auf Ihren Computer oder mounten Sie die Quelle';
procedure TfManageDocFrame.ThumbControl1LoadFile(Sender: TObject; URL: string;
  out Stream: TStream);
begin
  FUrl := URL;
  if FLast=URL then
    fetchNext;
  if Assigned(TThreadedImage(Sender).Thread) then
    TThread(TThreadedImage(Sender).Thread).Synchronize(TThreadedImage(Sender).Thread,@WaitForImage)
  else WaitForImage;
  Stream := TfileStream.Create(FtempPath+URL,fmOpenRead);
end;
procedure TfManageDocFrame.ThumbControl1SelectItem(Sender: TObject;
  Item: TThreadedImage);
var
  i: Integer;
  tmp: String;
begin
  FDocFrame.Refresh(copy(Item.URL,0,pos('.',Item.URL)-1),'S');
  DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
  FTimeLine.StartDate:=DataSet.FieldByName('ORIGDATE').AsDateTime+60;
  FTimeLine.MarkerDate:=DataSet.FieldByName('ORIGDATE').AsDateTime;
  if (aTag <> '') and bTag.Down and (pos(aTag,DataSet.FieldByName('TAGS').AsString)=0) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      tmp := DataSet.FieldByName('TAGS').AsString;
      if (copy(tmp,length(tmp)-1,1) <> ',') and (trim(tmp) <> '') then
        tmp := tmp+',';
      tmp := tmp+aTag;
      DataSet.FieldByName('TAGS').AsString := tmp;
    end;
  if (aDate <> '') and bTag1.Down then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      DataSet.FieldByName('ORIGDATE').AsString := aDate;
    end;
end;
procedure TfManageDocFrame.ThumbControl1ImageLoaderManagerBeforeStartQueue(
  Sender: TObject);
var
  i: Integer;
begin
  if ThumbControl1.ImageLoaderManager.Queue.Count=0 then exit;
  FFetchSQL:='';
  for i := 0 to ThumbControl1.ImageLoaderManager.Queue.Count-1 do
    begin
      if not FileExists(FtempPath+TThreadedImage(ThumbControl1.ImageLoaderManager.Queue[i]).URL) then
        FFetchSQL:=FFetchSQL+' or '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(TThreadedImage(ThumbControl1.ImageLoaderManager.Queue[i]).URL,0,pos('.',TThreadedImage(ThumbControl1.ImageLoaderManager.Queue[i]).URL)-1));
    end;
  FFetchSQL:=copy(FFetchSQL,4,length(FFetchSQL));
  if FFetchSQL <> '' then
    begin
      with DataSet.DataSet as IBaseManageDB do
        FFetchSQL:='select '+Data.QuoteField('SQL_ID')+','+Data.QuoteField('THUMBNAIL')+' from '+Data.QuoteField(TableName)+' where '+FFetchSQL;
      FFetchDS := Data.GetNewDataSet(FFetchSQL);
      FFetchDS.Open;
      while not FFetchDS.EOF do
        begin
          Data.BlobFieldToFile(FFetchDS,'THUMBNAIL',FtempPath+FFetchDS.FieldByName('SQL_ID').AsString+'.jpg');
          if FileSize(FtempPath+FFetchDS.FieldByName('SQL_ID').AsString+'.jpg')=0 then
            begin
              iNoThumbnail.Picture.SaveToFile(FtempPath+FFetchDS.FieldByName('SQL_ID').AsString+'.jpg');
            end;
          FFetchDS.Next;
        end;
      FreeAndNil(FFetchDS);
    end;
end;

procedure TfManageDocFrame.ThumbControl1ImageLoaderManagerSetItemIndex(
  Sender: TObject);
begin
  FTimeLine.StartDate:=DataSet.FieldByName('ORIGDATE').AsDateTime+60;
  FTimeLine.MarkerDate:=DataSet.FieldByName('ORIGDATE').AsDateTime;
end;

procedure TfManageDocFrame.FrameEnter(Sender: TObject);
var
  aSheet: TTabSheet;
  aParent: TWinControl;
  aForm: TForm;
begin
  if Assigned(Parent) and (Parent is TTabSheet) then
    begin
      aSheet := Parent as TTabSheet;
      aParent := aSheet.PageControl.Parent;
      while Assigned(aParent) and (not (aParent is TForm)) do
        begin
          aParent := aParent.Parent;
        end;
      if Assigned(aParent) and (aParent is TForm) then
        aForm := aParent as TForm;
      if Assigned(aForm) then
        begin
          aForm.OnDropFiles:=@DoOnDropFiles;
          aForm.AllowDropFiles:=True;
        end;
    end;
end;

procedure TfManageDocFrame.FrameExit(Sender: TObject);
var
  aSheet: TTabSheet;
  aParent: TWinControl;
  aForm: TForm;
begin
  if Assigned(Parent) and (Parent is TTabSheet) then
    begin
      aSheet := Parent as TTabSheet;
      aParent := aSheet.PageControl.Parent;
      while Assigned(aParent) and (not (aParent is TForm)) do
        begin
          aParent := aParent.Parent;
        end;
      if Assigned(aParent) and (aParent is TForm) then
        aForm := aParent as TForm;
      if Assigned(aForm) then
        begin
          aForm.OnDropFiles:=nil;
          aForm.AllowDropFiles:=False;
        end;
    end;
end;

procedure TfManageDocFrame.FTimeLineSetMarker(Sender: TObject);
var
  OldIdx: Integer;
  i: Integer;
  arec: LargeInt;
begin
  if not ((not FDataSet.EOF) and ((FDataSet.FieldByName('ORIGDATE').IsNull) or (FDataSet.FieldByName('ORIGDATE').AsDateTime>FTimeline.MarkerDate))) then
    begin
      FDataSet.First;
      i := 0;
    end
  else i := ThumbControl1.ImageLoaderManager.ActiveIndex;
  while (not FDataSet.EOF) and ((FDataSet.FieldByName('ORIGDATE').IsNull) or (FDataSet.FieldByName('ORIGDATE').AsDateTime>FTimeline.MarkerDate)) do
    begin
      inc(i);
      arec:=FDataSet.GetBookmark;
      if i>ThumbControl1.ImageLoaderManager.CountItems then
        FetchNext;
      FDataSet.GotoBookmark(arec);
      FDataSet.Next;
    end;
  ThumbControl1.ImageLoaderManager.OnSetItemIndex:=nil;
  ThumbControl1.ImageLoaderManager.ActiveIndex:=i;
  ThumbControl1.ImageLoaderManager.OnSetItemIndex:=@ThumbControl1ImageLoaderManagerSetItemIndex;
  ThumbControl1.ScrollIntoView;
end;

procedure TfManageDocFrame.IdleTimer1Timer(Sender: TObject);
var
  aOldEntry: UTF8String;
  tmp: TCaption;
  aFilter: String;
  procedure AddFilter(aTmp : string);
  begin
    if atmp <> '' then
      begin
        if aFilter <> '' then
          aFilter := aFilter+' OR ';
        with DataSet.DataSet as IBaseDbFilter do
          aFilter := aFilter+Data.ProcessTerm(Data.QuoteField('TAGS')+'='+Data.QuoteValue('*'+atmp+'*'))+' OR '+
                     Data.ProcessTerm(Data.QuoteField('FULLTEXT')+'='+Data.QuoteValue('*'+atmp+'*'));
      end;
  end;

begin
  if IdleTimer1.Tag=1 then exit;
  IdleTimer1.Tag:=1;
  IdleTimer1.Enabled:=false;
  if Assigned(ThumbControl1.ImageLoaderManager.ActiveItem) then
    aOldEntry := copy(ThumbControl1.ImageLoaderManager.ActiveItem.URL,0,rpos('.',ThumbControl1.ImageLoaderManager.ActiveItem.URL)-1);
  TDocPages(DataSet).PrepareDataSet;
  with DataSet.DataSet as IBaseDbFilter do
    begin
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      Limit := 0;
      if trim(eSearch.Text)<>'' then
        begin
          tmp := eSearch.Text;
          aFilter := '';
          while pos(',',tmp) > 0 do
            begin
              AddFilter(copy(tmp,0,pos(',',tmp)-1));
              tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
            end;
          AddFilter(tmp);
          Filter :=  aFilter;
        end
      else
        Filter := '';
    end;
  DataSet.Open;
  FLast:='';
  ThumbControl1.MultiThreaded:=False;
  ThumbControl1.URLList:='';
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  Application.ProcessMessages;
  ThumbControl1.MultiThreaded:=True;
  IdleTimer1.Tag:=0;
end;

procedure TfManageDocFrame.MenuItem2Click(Sender: TObject);
begin

end;

procedure TfManageDocFrame.pmPopupPopup(Sender: TObject);
begin
  ThumbControl1.Click;
end;

procedure TfManageDocFrame.tbMenue1Click(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfManageDocFrame.ThumbControl1DblClick(Sender: TObject);
var
  i: Integer;
  aStream: TFileStream;
begin
  if not bExecute1.Down then
    begin
      bExecute1.Down:=True;
      bExecute1Click(nil);
    end;
  if Assigned(ThumbControl1.ImageLoaderManager.ActiveItem) then
    begin
      try
        aStream := TFileStream.Create(FtempPath+ThumbControl1.ImageLoaderManager.ActiveItem.URL,fmOpenRead);
        PreviewFrame.LoadFromStream(aStream,'JPG');
        aStream.Free;
      except
      end;
    end;
  Application.ProcessMessages;
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    begin
      if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
        if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
          begin
            PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
            break;
          end;
    end;
end;

procedure TfManageDocFrame.DoOnDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
  NewFileName: String = '';
begin
  if fPicImport.Execute then
    begin
      fWaitForm.ShowInfo(ExtractFileName(Filenames[0]));
      fWaitform.ProgressBar1.Max:=length(FileNames);
      fWaitform.ProgressBar1.Position:=0;
      fWaitform.ProgressBar1.Style:=pbstNormal;
      fWaitForm.Show;
      Application.ProcessMessages;
      for i := 0 to length(FileNames)-1 do
        begin
          if not FileExists(Filenames[i]) then
            begin
              NewFileName := AppendPathDelim(GetTempPath)+ExtractFileName(Filenames[i]);
              {$ifdef linux}
              ExecProcess('gvfs-copy "'+Filenames[i]+'" "'+NewFileName+'"');
              {$endif}
              if not FileExists(NewFileName) then
                Showmessage(Format(strCantAccessFile,[Filenames[i]]));
            end
          else NewFileName:=Filenames[i];
          if FileExists(NewFileName) then
            begin
              fWaitForm.ShowInfo(ExtractFileName(NewFileName));
              TDocPages(DataSet).AddFromFile(NewFileName);
              if not TDocPages(DataSet).CanEdit then TDocPages(DataSet).DataSet.Edit;
              TDocPages(DataSet).FieldByName('TAGS').AsString:=fPicImport.eTags.Text;
              TDocPages(DataSet).Post;
              if fPicImport.cbDelete.Checked then
                begin
                  DeleteFileUTF8(NewFileName);
                  if NewFileName<>Filenames[i] then
                    begin
                      {$ifdef linux}
                      ExecProcess('gvfs-rm "'+Filenames[i]+'"');
                      {$endif}
                    end;
                end;
            end;
          fWaitform.ProgressBar1.Position:=fWaitform.ProgressBar1.Position+1;
        end;
      fWaitform.ProgressBar1.Style:=pbstMarquee;
      fWaitform.Hide;
      acRefresh.Execute;
    end;
end;

procedure TfManageDocFrame.eSearchChange(Sender: TObject);
begin
  IdleTimer1.Enabled:=True;
end;

procedure TfManageDocFrame.FDocFrameAftercheckInFiles(Sender: TObject);
begin
  RebuidThumb;
end;

procedure TfManageDocFrame.acDeleteExecute(Sender: TObject);
var
  aItem: TThreadedImage;
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      while FDocFrame.DataSet.Count>0 do
        TDocuments(FDocFrame.DataSet).Delete;
      DataSet.Delete;
      aItem := ThumbControl1.ImageLoaderManager.ActiveItem;
      ThumbControl1.ImageLoaderManager.List.Delete(ThumbControl1.ImageLoaderManager.ActiveIndex);
//      aItem.Free;
      ThumbControl1.Arrange;
    end;
end;

procedure TfManageDocFrame.acRebuildThumbExecute(Sender: TObject);
begin
  RebuidThumb;
end;

procedure TfManageDocFrame.acRefreshExecute(Sender: TObject);
var
  OldIdx: Integer;
begin
  OldIdx:=ThumbControl1.ImageLoaderManager.ActiveIndex;
  DataSet.DataSet.Refresh;
  FLast:='';
  ThumbControl1.URLList:='';
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  while ThumbControl1.ImageLoaderManager.CountItems<OldIdx do
    FetchNext;
  ThumbControl1.ImageLoaderManager.ActiveIndex:=OldIdx;
  ThumbControl1.ScrollIntoView;
end;

procedure TfManageDocFrame.acSetTagExecute(Sender: TObject);
begin
  if bTag.Down then
    begin
      if not InputQuery(strTag,strSetTag,aTag) then
        aTag := ''
    end;
end;

procedure TfManageDocFrame.bExecute1Click(Sender: TObject);
begin
  if bExecute1.Down then
    begin
      Panel1.Align:=alLeft;
      Panel1.Width:=230;
      pcPages.Visible:=True;
      spPages.Visible:=True;
      pcPages.Align:=alClient;
    end
  else
    begin
      Panel1.Align:=alClient;
      pcPages.Visible:=False;
      spPages.Visible:=False;
    end;
end;

procedure TfManageDocFrame.bTag1Click(Sender: TObject);
begin
  if bTag1.Down then
    begin
      if not InputQuery(strDate,strSetTag,aDate) then
        aDate := ''
    end;
end;

procedure TfManageDocFrame.bZoomInClick(Sender: TObject);
begin
  ThumbControl1.ThumbHeight:=ThumbControl1.ThumbHeight+20;
  ThumbControl1.ThumbWidth:=ThumbControl1.ThumbWidth+20;
  acRefresh.Execute;
end;

procedure TfManageDocFrame.bZoomOutClick(Sender: TObject);
begin
  ThumbControl1.ThumbHeight:=ThumbControl1.ThumbHeight-20;
  ThumbControl1.ThumbWidth:=ThumbControl1.ThumbWidth-20;
  acRefresh.Execute;
end;

procedure TfManageDocFrame.FetchNext;
var
  i: Integer;
begin
  i := 0;
  if DataSet.DataSet.Locate('SQL_ID',copy(FLast,0,pos('.',FLast)-1),[]) then
    DataSet.Next
  else
    DataSet.First;
  while (not DataSet.EOF) and (i<500) do
    begin
      inc(i);
      FLast := DataSet.Id.AsString+'.jpg';
      ThumbControl1.ImageLoaderManager.AddImage(FLast).Name:=DataSet.FieldByName('NAME').AsString;
      DataSet.Next;
    end;
  ThumbControl1.Arrange;
end;

procedure TfManageDocFrame.WaitForImage;
var
  URL: String;
  aTime: DWORD;
begin
  URL := FURL;
  while (not FileExists(FtempPath+URL)) do
    Application.ProcessMessages;
end;

procedure TfManageDocFrame.RebuidThumb;
var
  aDocument: TDocument;
  aFullStream: TMemoryStream;
  i: Integer;
  aStream: TMemoryStream;
  aNumber: String;
begin
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    begin
      if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
        if (Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)='JPG')
        or (Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)='JPG') then
          begin
            aDocument := TDocument.Create(nil,Data);
            aDocument.SelectByID(FDocFrame.DataSet.Id.AsVariant);
            aDocument.Open;
            aNumber := aDocument.FieldByName('NUMBER').AsString;
            aDocument.SelectByNumber(aNumber);
            aDocument.Open;
            aFullStream := TMemoryStream.Create;
            aStream := TMemoryStream.Create;
            aDocument.CheckoutToStream(aFullStream);
            TDocPages(DataSet).GenerateThumbNail(ExtractFileExt(aDocument.FileName),aFullStream,aStream);
            if aStream.Size>0 then
              Data.StreamToBlobField(aStream,DataSet.DataSet,'THUMBNAIL');
            aDocument.Free;
            {
            aFullStream.Position:=0;
            aFullStream.SaveToFile(FtempPath+DataSet.FieldByName('SQL_ID').AsString+'.jpg');
            }
            DeleteFileUTF8(FtempPath+DataSet.FieldByName('SQL_ID').AsString+'.jpg');
            aFullStream.Free;
            aStream.Free;
          end;
    end;
  acRefresh.Execute;
end;

constructor TfManageDocFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DataSet := TDocPages.Create(nil,Data);
  FTempPath := GetTempDir+'promet_thumbs';
  ForceDirectoriesUTF8(FtempPath);
  FtempPath := AppendPathDelim(FtempPath);
  FDocFrame := TfDocumentFrame.Create(Self);
  FDocFrame.DataSet := TDocuments.Create(nil,Data);
  FDocFrame.Parent := pDocFrame;
  FDocFrame.Align:=alClient;
  FDocFrame.HasPreview:=False;
  FDocFrame.AftercheckInFiles:=@FDocFrameAftercheckInFiles;
  FTimeLine := TTimeLine.Create(Self);
  fTimeLine.Parent:=pRight;
  fTimeLine.Align:=alClient;
  fTimeLine.OnSetMarker:=@FTimeLineSetMarker;
  FTimeLine.Increment:=-16;
  FTimeLine.UseLongMonth:=False;
  ThumbControl1.ImageLoaderManager.OnSetItemIndex:=@ThumbControl1ImageLoaderManagerSetItemIndex;
  PreviewFrame := TfPreview.Create(Self);
  PreviewFrame.Parent := tsDocument;
  PreviewFrame.Align := alClient;
  PreviewFrame.Show;
end;

destructor TfManageDocFrame.Destroy;
begin
  FTimeLine.Free;
  DataSet.Free;
  FDocFrame.Free;
  PreviewFrame.Free;
  inherited Destroy;
end;

procedure TfManageDocFrame.Open;
begin
  ThumbControl1.ImageLoaderManager.BeforeStartQueue:=@ThumbControl1ImageLoaderManagerBeforeStartQueue;
  DataSet.CreateTable;
  TDocPages(DataSet).PrepareDataSet;
  with DataSet.DataSet as IBaseDbFilter do
    begin
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      Limit := 0;
    end;
  DataSet.Open;
  DataSet.First;
  FTimeLine.StartDate:=DataSet.FieldByName('ORIGDATE').AsDateTime;
  ThumbControl1.URLList:='';
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  bExecute1.Down:=False;
  bExecute1Click(nil);
end;

procedure TfManageDocFrame.DoRefresh;
begin
end;

end.

