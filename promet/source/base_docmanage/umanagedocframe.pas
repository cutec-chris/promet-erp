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

  { TfManageDocFrame }

  TfManageDocFrame = class(TPrometMainFrame)
    acDelete: TAction;
    acRefresh: TAction;
    acSetTag: TAction;
    acRebuildThumb: TAction;
    acEdit: TAction;
    acSaveAll: TAction;
    acSave: TAction;
    acImport: TAction;
    acRotate: TAction;
    ActionList1: TActionList;
    bEditFilter: TSpeedButton;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    bExecute1: TSpeedButton;
    bRefresh2: TSpeedButton;
    bRefresh3: TSpeedButton;
    bRefresh4: TSpeedButton;
    bTag: TSpeedButton;
    bTag1: TSpeedButton;
    bZoomIn: TSpeedButton;
    bZoomOut: TSpeedButton;
    bImport: TSpeedButton;
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
    ExtRotatedLabel6: TLabel;
    ExtRotatedLabel7: TLabel;
    IdleTimer1: TIdleTimer;
    iNoThumbnail: TImage;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
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
    pNav3: TPanel;
    pNav4: TPanel;
    pRight: TPanel;
    pThumb: TPanel;
    pToolbar: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    spPages: TSplitter;
    tbMenue1: TToolButton;
    tbToolBar: TToolBar;
    ThumbControl1: TThumbControl;
    tsDocument: TTabSheet;
    tsFiles: TTabSheet;
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acRebuildThumbExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acRotateExecute(Sender: TObject);
    procedure acSaveAllExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
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
    procedure pmPopupPopup(Sender: TObject);
    procedure tbMenue1Click(Sender: TObject);
    procedure ThumbControl1DblClick(Sender: TObject);
    procedure ThumbControl1ImageLoaderManagerBeforeStartQueue(Sender: TObject);
    procedure ThumbControl1ImageLoaderManagerSetItemIndex(Sender: TObject);
    procedure ThumbControl1LoadFile(Sender: TObject; URL: string; out
      Stream: TStream);
    procedure ThumbControl1Scrolled(Sender: TObject);
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
    SelectedItem: TThreadedImage;
    FFilter: String;
    procedure FetchNext;
    procedure WaitForImage;
    procedure RebuidThumb;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure DoRefresh; override;
    function GotoCurrentItem: Boolean;
    procedure OpenDir(aDir : Variant);
  end;

  { TImportCheckTherad }

  TImportCheckThread = class(TThread)
  private
    FDoc: TfManageDocFrame;
    procedure CheckImport;
  public
    procedure Execute;override;
    constructor Create(aDocFrame : TfManageDocFrame);
  end;

procedure AddToMainTree(Node : TTReeNode);
const
  MAX_IMAGES = 50;
implementation
{$R *.lfm}
uses uData,udocuments,uWait,LCLIntf,Utils,uFormAnimate,uImportImages,
  ProcessUtils,uMainTreeFrame,ucameraimport,FPimage,FPReadJPEG,FPCanvas,
  FPWriteJPEG,LCLProc;
resourcestring
  strTag                   = 'Tag';
  strSetTag                = 'durch Klick setzen';

procedure AddToMainTree(Node: TTReeNode);
var
  Node1: TTreeNode;
begin
  TTreeEntry(Node.Data).Typ := etDocuments;
  Data.SetFilter(Data.Tree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('D')+'))',0,'','ASC',False,True,True);
  Data.Tree.DataSet.First;
  while not Data.Tree.dataSet.EOF do
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
      TTreeEntry(Node1.Data).DataSource := Data.Tree;
      TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
      TTreeEntry(Node1.Data).Typ := etDocumentDir;
      fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
      Data.Tree.DataSet.Next;
    end;
end;

{ TImportCheckTherad }

procedure TImportCheckThread.CheckImport;
begin
  FDoc.acImport.Enabled := fCameraimport.ImportAvalibe;
end;

procedure TImportCheckThread.Execute;
begin
  Synchronize(@CheckImport);
end;

constructor TImportCheckThread.Create(aDocFrame: TfManageDocFrame);
begin
  FDoc := aDocFrame;
  FreeOnTerminate:=True;
  inherited Create(false);
end;

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

procedure TfManageDocFrame.ThumbControl1Scrolled(Sender: TObject);
begin
  ThumbControl1.Invalidate;
end;

procedure TfManageDocFrame.ThumbControl1SelectItem(Sender: TObject;
  Item: TThreadedImage);
var
  i: Integer;
  tmp: String;
begin
  SelectedItem := Item;
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
          aFilter := FFilter;
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
  SelectedItem:=nil;
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  Application.ProcessMessages;
  ThumbControl1.MultiThreaded:=True;
  IdleTimer1.Tag:=0;
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
  aFile: String;
  extn: String;
  aSecFile: String;
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
                  aFile := NewFileName;
                  extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aFile)));
                  if (extn = '.cr2')
                  or (extn = '.crw')
                  or (extn = '.dng')
                  or (extn = '.raw')
                  or (extn = '.erf')
                  or (extn = '.raf')
                  or (extn = '.3fr')
                  or (extn = '.fff')
                  or (extn = '.dcr')
                  or (extn = '.dcs')
                  or (extn = '.kdc')
                  or (extn = '.rwl')
                  or (extn = '.mef')
                  or (extn = '.mfw')
                  or (extn = '.iiq')
                  or (extn = '.mrw')
                  or (extn = '.mdc')
                  or (extn = '.nef')
                  or (extn = '.nrw')
                  or (extn = '.orf')
                  or (extn = '.rw2')
                  or (extn = '.pef')
                  or (extn = '.srw')
                  or (extn = '.x3f')
                  or (extn = '.cs1')
                  or (extn = '.cs4')
                  or (extn = '.cs16')
                  or (extn = '.srf')
                  or (extn = '.sr2')
                  or (extn = '.arw')
                  then
                    begin
                      if FileExistsUTF8(copy(aFile,0,rpos('.',aFile)-1)+'.jpg') then
                        aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.jpg'
                      else if FileExistsUTF8(copy(aFile,0,rpos('.',aFile)-1)+'.JPG') then
                        aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.JPG'
                      else if FileExistsUTF8(copy(aFile,0,rpos('.',aFile)-1)+'.Jpg') then
                        aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.Jpg'
                      else aSecFile:='';
                      if aSecFile <> '' then
                        begin
                          {$ifdef linux}
                          try
                            ExecProcess('gvfs-rm "'+aSecFile+'"');
                          except
                            DeleteFileUTF8(aSecFile);
                          end;
                          {$else}
                          DeleteFileUTF8(aSecFile);
                          {$endif}
                        end;
                    end;
                  if FileExistsUTF8(copy(NewFileName,0,length(NewFileName)-length(extn))+'.ufraw') then
                    DeleteFileUTF8(copy(NewFileName,0,length(NewFileName)-length(extn))+'.ufraw');
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
procedure TfManageDocFrame.acEditExecute(Sender: TObject);
var
  i: Integer;
  a: Integer;
begin
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = 'jpg ')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = 'jpeg ')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=i;
        FDocFrame.acViewFile.Execute;
        for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
          begin
            if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[a]) then
              if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
                begin
                  PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
                  break;
                end;
          end;
        break;
      end;
end;

procedure TfManageDocFrame.acImportExecute(Sender: TObject);
begin
  if fCameraimport.Execute(Self) then
    acRefresh.Execute;
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
  SelectedItem:=nil;
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  while ThumbControl1.ImageLoaderManager.CountItems<OldIdx do
    FetchNext;
  ThumbControl1.ImageLoaderManager.ActiveIndex:=OldIdx;
  ThumbControl1.ScrollIntoView;
end;

procedure TfManageDocFrame.acRotateExecute(Sender: TObject);
var
  i: Integer;
  Img: TFPMemoryImage;
  reader: TFPReaderJPEG;
  aDoc: TDocument;
  aFullStream: TMemoryStream;
  Img2: TFPMemoryImage;
  wr: TFPWriterJPEG;
  x: Integer;
  y: Integer;
  a: Integer;
begin
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = 'jpg ')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = 'jpeg ')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=i;
        if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
          begin
            Img := TFPMemoryImage.Create(0, 0);
            Img.UsePalette := false;
            reader := TFPReaderJPEG.Create;
            try
              aFullStream := TMemoryStream.Create;
              aDoc := TDocument.Create(nil,Data);
              aDoc.SelectByID(TDocuments(FDocFrame.DataSet).Id.AsVariant);
              aDoc.Open;
              aDoc.CheckoutToStream(aFullStream);
              aFullStream.Position:=0;
              Img.LoadFromStream(aFullStream, reader);
              reader.Free;
              if Assigned(Img) then
                begin
                  Img2 := TFPMemoryImage.create(0,0);
                  Img2.Width:=Img.Height+1;
                  Img2.Height:=Img.Width+1;
                  Img2.UsePalette := false;
                  for x := 0 to Img.Width-1 do
                    for y := 0 to Img.Height-1 do
                      begin
                        Img2.Colors[Img.Height-y,x] := Img.Colors[x,y];
                      end;
                  wr := TFPWriterJPEG.Create;
                  wr.ProgressiveEncoding:=True;
                  aFullStream.Size:=0;
                  aFullStream.Position:=0;
                  Img2.SaveToStream(aFullStream,wr);
                  wr.Free;
                  aDoc.CheckInFromStream(aFullStream);
                  aDoc.Free;
                  aFullStream.Free;
                  Img2.Free;
                end;
            finally
              Img.Free;
            end;

            for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
              begin
                if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[a]) then
                  if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
                    begin
                      PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
                      break;
                    end;
              end;
            acRebuildThumb.Execute;
          end;
        break;
      end;
end;

procedure TfManageDocFrame.acSaveAllExecute(Sender: TObject);
var
  ARect: TRect;
  Dum: TRect;
  i: Integer;
  a: Integer;
begin
  ARect.Left := 0;
  ARect.Top := 0;
  ARect.Bottom:=ThumbControl1.Height;
  Arect.Right:=ThumbControl1.Width;
  if not SelectDirectoryDialog1.Execute then exit;
  fWaitForm.ShowInfo(strSave);
  for i := 0 to ThumbControl1.ImageLoaderManager.List.Count - 1 do
    if IntersectRect(Dum, ARect, TThreadedImage(ThumbControl1.ImageLoaderManager.List[i]).Rect) then
      begin
        ThumbControl1.ImageLoaderManager.ActiveIndex:=i;
        ThumbControl1SelectItem(ThumbControl1,TThreadedImage(ThumbControl1.ImageLoaderManager.List[i]));
        for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
          if (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,4)) = 'jpg ')
          or (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,5)) = 'jpeg ')
          then
            begin
              FDocFrame.lvDocuments.ItemIndex:=a;
              FDocFrame.SaveFileToDir(SelectDirectoryDialog1.FileName);
            end;
      end;
  fWaitForm.Hide;
end;

procedure TfManageDocFrame.acSaveExecute(Sender: TObject);
var
  a: Integer;
begin
  ThumbControl1SelectItem(ThumbControl1,TThreadedImage(ThumbControl1.ImageLoaderManager.List[ThumbControl1.ImageLoaderManager.ActiveIndex]));
  for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,4)) = 'jpg ')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,5)) = 'jpeg ')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=a;
        FDocFrame.acSaveToFile.Execute;
      end;
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
  aItem: TThreadedImage;
begin
  i := 0;
  if DataSet.DataSet.Locate('SQL_ID',copy(FLast,0,pos('.',FLast)-1),[]) then
    DataSet.Next
  else
    DataSet.First;
  while (not DataSet.EOF) and (i<MAX_IMAGES) do
    begin
      inc(i);
      FLast := DataSet.Id.AsString+'.jpg';
      aItem := ThumbControl1.ImageLoaderManager.AddImage(FLast);
      aItem.Name:=DataSet.FieldByName('NAME').AsString;
      if not Assigned(SelectedItem) then
        SelectedItem := aItem;
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
        or (Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)='JPEG')
        then
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
  FFilter := '';
  SelectedItem:=nil;
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
  PreviewFrame.AddToolbarAction(acEdit);
  PreviewFrame.AddToolbarAction(acRotate);
end;
destructor TfManageDocFrame.Destroy;
begin
  FTimeLine.Free;
  FreeAndNil(FDataSet);
  FDocFrame.Free;
  PreviewFrame.Free;
  inherited Destroy;
end;
procedure TfManageDocFrame.Open;
var
  aRefThread: TImportCheckThread;
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
  SelectedItem:=nil;
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  bExecute1.Down:=False;
  bExecute1Click(nil);
  aRefThread := TImportCheckThread.Create(Self);
end;
procedure TfManageDocFrame.DoRefresh;
begin
end;

function TfManageDocFrame.GotoCurrentItem : Boolean;
begin
  Result := false;
  if SelectedItem=nil then exit;
  Result := DataSet.DataSet.Locate('SQL_ID',copy(SelectedItem.URL,0,pos('.',SelectedItem.URL)-1),[]);
end;

procedure TfManageDocFrame.OpenDir(aDir: Variant);
begin
  if aDir = Null then
    FFilter := ''
  else
    FFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(aDir);
  with DataSet.DataSet as IBaseDbFilter do
    begin
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      Limit := 0;
      Filter :=  FFilter;
    end;
  DataSet.Open;
  FLast:='';
  ThumbControl1.MultiThreaded:=False;
  ThumbControl1.URLList:='';
  SelectedItem:=nil;
  Datasource1.DataSet := DataSet.DataSet;
  FetchNext;
  Application.ProcessMessages;
  ThumbControl1.MultiThreaded:=True;
  IdleTimer1.Tag:=0;
  ThumbControl1.Invalidate;
end;

end.

