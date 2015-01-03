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
unit uDocumentFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, ComCtrls,
  ActnList, Menus, Dialogs, uPrometFramesInplaceDB, Graphics, StdCtrls,
  uBaseDBClasses, db, uIntfStrConsts,uGeneralStrConsts, Utils,UtilsVis, uExtControls, uWait, Variants,
  uPreviewFrame
  {$IFDEF WINDOWS}
  ,ActiveX,Windows,OleDropFiles
  {$ENDIF}
  ;
resourcestring
  strFiles                             = 'Dateien';
type

{ TfDocumentFrame }

TfDocumentFrame = class(TPrometInplaceDBFrame{$IFDEF WINDOWS},IDropSource{$ENDIF})
    acAcquire: TAction;
    acAddDocument: TAction;
    acAddFolder: TAction;
    acBigIcons: TAction;
    acCheckoutToRevision: TAction;
    acCopyAsLink: TAction;
    acCreateDirectory: TAction;
    acCreateFromTemplate: TAction;
    acDetails: TAction;
    acDocumentActions: TAction;
    acDocumentProperties: TAction;
    acEditFile: TAction;
    acExecute: TAction;
    acList: TAction;
    acPasteAsLink: TAction;
    acPrintFile: TAction;
    acRemoveDocument: TAction;
    acRename: TAction;
    acSaveToFile: TAction;
    acSmallIcons: TAction;
    acCheckinFromDir: TAction;
    acMoveLink: TAction;
    acCopyTextLink: TAction;
    acRights: TAction;
    acCheckinFromFile: TAction;
    acRefresh: TAction;
    ActionList1: TActionList;
    acViewFile: TAction;
    bCheckout: TSpeedButton;
    bCheckout1: TSpeedButton;
    bDocuAdd: TSpeedButton;
    bDocuAdd1: TSpeedButton;
    bDocuAdd2: TSpeedButton;
    bDocuAdd3: TSpeedButton;
    bDocuremove: TSpeedButton;
    bDocuremove1: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    bExecute: TSpeedButton;
    bExecute1: TSpeedButton;
    BigIcons: TImageList;
    bMenue1: TSpeedButton;
    bMenue2: TSpeedButton;
    bMenue3: TSpeedButton;
    bMenue4: TSpeedButton;
    bRefresh1: TSpeedButton;
    bRefresh2: TSpeedButton;
    DocumentDialog: TOpenDialog;
    DocumentSaveDialog: TSaveDialog;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    ExtRotatedLabel4: TExtRotatedLabel;
    FolderDialog: TSelectDirectoryDialog;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lvDocuments: TListView;
    MenuIcons: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miLink: TMenuItem;
    MenuItem8: TMenuItem;
    miAddFile: TMenuItem;
    miAddFolder: TMenuItem;
    miBigIcons: TMenuItem;
    miCheckoutToRevision: TMenuItem;
    miCopyLink: TMenuItem;
    miCreateDir: TMenuItem;
    miCreateFromTemplate: TMenuItem;
    miDeleteDocument: TMenuItem;
    miDetails: TMenuItem;
    miDocumentActions: TMenuItem;
    miDocumentProperties: TMenuItem;
    miEditFile: TMenuItem;
    miList: TMenuItem;
    miPasteLink: TMenuItem;
    miPrintFile: TMenuItem;
    miRename: TMenuItem;
    miSaveToFile: TMenuItem;
    miSmallIcons: TMenuItem;
    miViewFile: TMenuItem;
    Panel10: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pHeader: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pToolbar: TPanel;
    pmAddDocuments: TPopupMenu;
    pmDocumentAction: TPopupMenu;
    pmViewStyle: TPopupMenu;
    pPreviewT: TPanel;
    SmallIcons: TImageList;
    spPreview: TSplitter;
    procedure acAcquireExecute(Sender: TObject);
    procedure acAddDocumentExecute(Sender: TObject);
    procedure acAddFolderExecute(Sender: TObject);
    procedure acBigIconsExecute(Sender: TObject);
    procedure acCheckinFromDirExecute(Sender: TObject);
    procedure acCheckinFromFileExecute(Sender: TObject);
    procedure acCheckoutToRevisionExecute(Sender: TObject);
    procedure acCopyAsLinkExecute(Sender: TObject);
    procedure acCopyTextLinkExecute(Sender: TObject);
    procedure acCreateDirectoryExecute(Sender: TObject);
    procedure acCreateFromTemplateExecute(Sender: TObject);
    procedure acDetailsExecute(Sender: TObject);
    procedure acDocumentActionsExecute(Sender: TObject);
    procedure acDocumentPropertiesExecute(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure acListExecute(Sender: TObject);
    procedure acMoveLinkExecute(Sender: TObject);
    procedure acPasteAsLinkExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acRemoveDocumentExecute(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveToFileExecute(Sender: TObject);
    procedure acSmallIconsExecute(Sender: TObject);
    procedure acViewFileExecute(Sender: TObject);
    function aDocumentCheckCheckinFiles(aFiles: TStrings;Directory: string;var Desc : string): Boolean;
    procedure aDocumentCheckCheckOutFile(aFile: string);
    procedure bMenue1Click(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure lvDocumentsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvDocumentsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvDocumentsDblClick(Sender: TObject);
    procedure lvDocumentsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvDocumentsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lvDocumentsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure pmDocumentActionPopup(Sender: TObject);
    procedure spPreviewMoved(Sender: TObject);
  private
    FAfterCheckinFiles: TNotifyEvent;
    FAParent: Integer;
    FBaseElement: TBaseDBDataset;
    FHasPreview: Boolean;
    FID: string;
    FLanguage: Variant;
    FRefID: Variant;
    FTyp: string;
    FVersion: Variant;
    aDirectoryID : Integer;
    DirectoryIDs : array of integer;
    PreviewFrame: TfPreview;
    FEditable : Boolean;
    FDropForm : TForm;
    {$IFDEF WINDOWS}
    FDragStartPos: TPoint;
    DragDropFile : string;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult; stdcall;
    function GiveFeedback(dwEffect: DWORD): HResult; stdcall;
    {$ENDIF}
    procedure DoRefresh;
    procedure AddActualItem(aInsert: Boolean=False);
    function GotoSelected : Boolean;
    procedure DoEditDocument(Method : string = 'EDIT';ShowEditor : Boolean = False);
    procedure SetHasPreview(AValue: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure SetLanguage;override;
    function SaveFileToDir(aDir : string) : Boolean;
    procedure Refresh(RefID: Variant;Typ,ID : string;Version : Variant;Language : Variant;aParent : Integer = 0);overload;
    procedure Refresh(RefID : Variant;Typ : string;aParent : Integer = 0);overload;
    property RefID : Variant read FRefID;
    property Typ : string read FTyp;
    property ID : string read FID;
    property Version : Variant read FVersion;
    property Language : Variant read FLanguage;
    property DirectoryParent : Integer read FAParent;
    procedure DoOnDropFiles(Sender : TObject;const Filenames : array of string);
    procedure SetRights(Editable : Boolean);override;
    procedure ShowFrame; override;
    procedure SelectDocument(aLink : string);
    function GotoEntry(aItem : TListItem) : Boolean;
    property HasPreview : Boolean read FHasPreview write SetHasPreview;
    property AftercheckInFiles : TNotifyEvent read FAfterCheckinFiles write FAfterCheckInFiles;
    property BaseElement : TBaseDBDataset read FBaseElement write FBaseElement;
  end;
implementation
uses uDocuments, uData, uDocumentAddOptions, uBaseApplication, SecureUtils,
  uDocumentAcquire,PdfDoc,PdfImages,uOCR,uMessages,
  uPerson, uMimeTypeEdit, uDocumentProcess, uDocumentAction,uDocumentCheckin,
  uOrder,uBaseDBInterface,ClipBrd,uBaseVisualApplication,uSelectTemplate,
  uNRights,uDocProperties,ubaseconfig;
resourcestring
  strFileExtDesc                             = '%s Datei';
  strCheckingOutFile                         = 'Hole %s';
  strFailedCreatingDiff                      = 'konnte Differenzdatei von Datei %s nicht erstellen';
  strCantDeleteTempDirectory                 = 'Das temporäre Verzeichnis konnte nicht gelöscht werden !';
  strFailedExecuteProcess                    = 'Möglicherweise ist das auführen von "%s" fehlgeschlagen, Rückgabewert: %d';
  strNoValidCommand                          = 'Sie haben einen ungültigen befehl in den Dateiaktionen angegeben. Gültige Befehle müssen mit exec: oder mkdir: beginnen.';
  strNewImage                                = 'Neue Datei';
  strDocumentAdded                           = 'Datei hinzugefügt';
  strFailed                                  = 'fehlgeschlagen !';
{$R *.lfm}
procedure TfDocumentFrame.bMenue1Click(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;
procedure TfDocumentFrame.FrameEnter(Sender: TObject);
var
  aSheet: TTabSheet;
  aForm: TForm = nil;
  aParent: TWinControl;
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
procedure TfDocumentFrame.FrameExit(Sender: TObject);
var
  aSheet: TTabSheet;
  aParent: TWinControl;
  aForm: TForm = nil;
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
procedure TfDocumentFrame.lvDocumentsColumnClick(Sender: TObject;
  Column: TListColumn);
var
  ParentDirremoved: Boolean = False;
  aNew: TListItem;
begin
  if (lvDocuments.Items.Count > 0) and (lvDocuments.Items[0].ImageIndex = 5) then
    begin
      ParentDirremoved := True;
      lvDocuments.Items[0].Free;
    end;
  if lvDocuments.SortColumn=Column.Index then
    begin
      if lvDocuments.SortType = stNone then
        begin
          lvDocuments.SortType:=stData;
          lvDocuments.SortDirection:=ComCtrls.sdAscending;
        end
      else if lvDocuments.SortDirection=ComCtrls.sdAscending then
        lvDocuments.SortDirection:=ComCtrls.sdDescending
      else if lvDocuments.SortDirection=ComCtrls.sdDescending then
        lvDocuments.SortType:=stNone;
      lvDocuments.SortColumn:=Column.Index;
    end
  else
    begin
      lvDocuments.SortColumn:=Column.Index;
      lvDocuments.SortType:=stData;
      lvDocuments.SortDirection:=ComCtrls.sdAscending;
    end;
  if ParentDirremoved then
    begin
      aNew := lvDocuments.Items.Insert(0);
      aNew.ImageIndex:=5;
      aNew.Caption:='..';
    end;
end;
procedure TfDocumentFrame.lvDocumentsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  i:integer;
  d1, d2: TDateTime;
begin
  Compare := 0;
  if lvDocuments.Items.Count = 0 then exit;
  if lvDocuments.SortColumn = 0 then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else if lvDocuments.SortColumn=3 then
    begin
      d1 := StrToDate(Item1.SubItems[2]);
      d2 := StrToDate(Item2.SubItems[2]);
      if d1 < d2 then
        Compare := -1
      else if d1 > d2 then Compare := 1
      else
        Compare := 0;
    end
  else
    begin
      i := lvDocuments.SortColumn -1;
      if  (Item1.SubItems.Count <= i)
      and (Item2.SubItems.Count <= i) then
        Compare := CompareText(Item1.SubItems[i], Item2.SubItems[i]);
    end;
end;
procedure TfDocumentFrame.lvDocumentsDblClick(Sender: TObject);
begin
  if lvDocuments.Selected = nil then exit;
  if lvDocuments.Selected.ImageIndex = 5 then
    begin
      aDirectoryID := DirectoryIDs[length(DirectoryIDs)-1];
      Setlength(DirectoryIDs,length(DirectoryIDs)-1);
      Refresh(FRefID,FTyp,FID,FVersion,FLanguage,aDirectoryID);
    end
  else
    begin
      if not GotoSelected then exit;
      if FDataSet.FieldByName('ISDIR').AsString = 'Y' then
        begin
          Setlength(DirectoryIDs,length(DirectoryIDs)+1);
          DirectoryIDs[length(DirectoryIDs)-1] := aDirectoryID;
          aDirectoryID := FDataSet.FieldByName('NUMBER').AsInteger;
          Refresh(FRefID,FTyp,FID,FVersion,FLanguage,aDirectoryID);
        end
      else
        acViewFile.Execute;
    end;
end;
procedure TfDocumentFrame.lvDocumentsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF WINDOWS}
    if Button = mbLeft then
    begin
      FDragStartPos.x := X;
      FDragStartPos.y := Y;
    end;
  {$ENDIF}
end;
procedure TfDocumentFrame.lvDocumentsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  Threshold = 5;
{$IFDEF WINDOWS}
var
  Effect: DWORD;
  TempPath: String;
  f : TextFile;
  fl: TStringList;
  aDocument: TDocument;
  aID: Integer;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  with BaseApplication as IBaseConfig do
    if Assigned(Config) then
      TempPath := Config.ReadString('TEMPPATH','');
  if TempPath = '' then
    with BaseApplication as IBaseApplication do
      TempPath := GetInternalTempDir;
  if (ssLeft in Shift) and ((Abs(X - FDragStartPos.x) >= Threshold) or (Abs(Y - FDragStartPos.y) >= Threshold)) then
    begin
      Perform(WM_LBUTTONUP, 0, MakeLong(X, Y));
      Effect := DROPEFFECT_NONE;
      if GotoSelected then
        begin
          fl := TStringList.Create;
          if DataSet.FieldByName('ISDIR').AsString = 'Y' then
            begin
              DragDropFile := AppendPathDelim(TempPath)+TDocuments(DataSet).FileName;
              aDocument := TDocument.CreateEx(Self,Data);
              aID := DataSet.FieldByName('NUMBER').AsInteger;
              aDocument.SelectByNumber(aId);
              aDocument.Open;
              aDocument.DoCheckout(AppendPathDelim(TempPath));
              aDocument.Free;
              ForceDirectoriesUTF8(DragDropFile);
              fl.Add(UniToSys(TDocuments(DataSet).FileName));
              DoDragDrop(GetFileDataObject(TempPath,TDocuments(DataSet).FileName), Self, DROPEFFECT_MOVE, @Effect);
            end
          else
            begin
              DragDropFile := AppendPathDelim(TempPath)+UniToSys(TDocuments(DataSet).FileName);
              aID := DataSet.FieldByName('NUMBER').AsInteger;
              aDocument := TDocument.CreateEx(Self,Data);
              aDocument.SelectByNumber(aId);
              aDocument.Open;
              aDocument.DoCheckout(AppendPathDelim(TempPath));
              aDocument.Free;
              fl.Add(DragDropFile);
              DoDragDrop(GetFileDataObject(TempPath, TDocuments(DataSet).FileName), Self, DROPEFFECT_MOVE, @Effect);
            end;
          fl.Free;
        end;
    end;
  {$ENDIF}
end;
procedure TfDocumentFrame.lvDocumentsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  NewVisible: Boolean;
begin
  NewVisible := False;
  if Selected
  and pPreviewT.Visible
  and (not (FDataSet.FieldByName('ISDIR').AsString = 'Y'))
  and GotoSelected
  and (lvDocuments.SelCount=1)
  and (PreviewFrame.CanHandleType(Uppercase(FDataSet.FieldByName('EXTENSION').AsString)))
  then
    begin
      NewVisible := True;
      PreviewFrame.clear;
      PreviewFrame.LoadFromDocuments(FDataSet.Id.AsVariant);
    end;
  {
  pPreviewT.Visible:=NewVisible;
  spPreview.Visible:=NewVisible;
  with Application as IBaseDbInterface do
    pPreviewT.Width := DBConfig.ReadInteger('DOCPREVIEWWIDTH',400);
  if pPreviewT.Width < 20 then
    pPreviewT.Width := 400;
  spPreview.Left := 0;
  }
end;
procedure TfDocumentFrame.pmDocumentActionPopup(Sender: TObject);
var
  Stream: TStringStream;
begin
  acPasteAsLink.Enabled:=False;
  acMoveLink.Enabled:=False;
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        if copy(Stream.DataString,0,9) = 'DOCUMENTS' then
          begin
            acPasteAsLink.Enabled:=True;
            acMoveLink.Enabled:=True;
          end;
      Stream.Free;
    end;
end;
procedure TfDocumentFrame.spPreviewMoved(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    DBConfig.WriteInteger('DOCPREVIEWWIDTH',lvDocuments.Width);
end;
{$IFDEF WINDOWS}
function TfDocumentFrame.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: DWORD): HResult; stdcall;
var
  aDocument: TDocument;
  aID: Int64;
begin
  if fEscapePressed or (grfKeyState and MK_RBUTTON = MK_RBUTTON) or (not GotoSelected) then
    begin
      Result := DRAGDROP_S_CANCEL;
      if not DirectoryExistsUTF8(DragDropFile) then
        DeleteFileUTF8(DragDropFile)
      else
        RemoveDirUTF8(DragDropFile);
      exit;
    end
  else if grfKeyState and MK_LBUTTON = 0 then
    begin
      Result := DRAGDROP_S_DROP;
      if grfKeyState = 0 then
        begin
          //Dropped File to Desktop/Explorer
          aDocument := TDocument.CreateEx(Self,Data);
          aID := DataSet.FieldByName('NUMBER').AsInteger;
          aDocument.SelectByNumber(aId);
          aDocument.Open;
          aDocument.DoCheckout(copy(DragDropFile,0,rpos(DirectorySeparator,DragDropFile)-1));
          aDocument.Free;
        end;
    end
  else
    begin
      Result := S_OK;
    end;
end;
function TfDocumentFrame.GiveFeedback(dwEffect: DWORD): HResult; stdcall;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;
{$ENDIF}
procedure TfDocumentFrame.acAddDocumentExecute(Sender: TObject);
var
  i: Integer;
  aDocument: TDocument;
  DelOK: Boolean;
  label DelRetry;
begin
  if DocumentDialog.Execute then
    begin
      fDocumentAddOptions.SetLanguage;
      fDocumentAddOptions.cbAddToMessages.Checked := False;
      fDocumentAddOptions.cbAddToMessages.Enabled := False;
      fDocumentAddOptions.cbDeletefromFilesystem.Enabled := True;
      fDocumentAddOptions.eName.Text := '';
      fDocumentAddOptions.eName.Enabled := False;
      fDocumentAddOptions.ccCalendar.Date:=Now();
      if not fDocumentAddOptions.Execute then exit;
      Screen.Cursor := crHourglass;
      for i := 0 to DocumentDialog.Files.Count-1 do
        begin
          aDocument := TDocument.CreateEx(Self,Data);
          aDocument.Select(0);
          aDocument.Open;
          aDocument.Ref_ID:=FRefID;
          aDocument.BaseID:=FID;
          aDocument.BaseTyp:=FTyp;
          aDocument.BaseLanguage:=FLanguage;
          aDocument.BaseVersion:=FVersion;
          aDocument.ParentID:=aDirectoryID;
          aDocument.AddFromFile(DocumentDialog.Files[i],'',fDocumentAddOptions.ccCalendar.Date);
          if fDocumentAddOptions.cbDeletefromFilesystem.Checked then
            begin
              with BaseApplication as IBaseConfig do
                begin
                  DelRetry:
                    case Config.ReadInteger('DELETEMETHOD',0) of
                    0:DelOK := DeleteFileUTF8(DocumentDialog.Files[i]);
                    1:DelOK := DeleteSecure(DocumentDialog.Files[i]);
                    2:DelOK := DeleteSecure(DocumentDialog.Files[i],dmDoD522022);
                    3:DelOK := DeleteSecure(DocumentDialog.Files[i],dmOverride);
                    end;
                    if not DelOK then
                      if MessageDlg(strDelete,strCantDeleteTempDirectory,mtWarning,[mbRetry,mbAbort],0) = mrRetry then
                        goto DelRetry;
                end;
            end;
          DataSet.DataSet.Refresh;
          DataSet.GotoBookmark(aDocument.GetBookmark);
          aDocument.Free;
          AddActualItem(True);
          Application.ProcessMessages;
        end;
      Screen.Cursor := crDefault;
    end;
end;
procedure TfDocumentFrame.acAddFolderExecute(Sender: TObject);
var
  aID: Int64;
  aDocument: TDocument;
begin
  if FolderDialog.Execute then
    begin
      Screen.Cursor := crHourglass;
      fDocumentAddOptions.SetLanguage;
      fDocumentAddOptions.cbAddToMessages.Checked := False;
      fDocumentAddOptions.cbAddToMessages.Enabled := False;
      fDocumentAddOptions.cbDeletefromFilesystem.Enabled := True;
      fDocumentAddOptions.eName.Text := '';
      fDocumentAddOptions.eName.Enabled := False;
      fDocumentAddOptions.ccCalendar.Date:=Now();
      if not fDocumentAddOptions.Execute then exit;
      aDocument := TDocument.CreateEx(Self,Data);
      aDocument.Select(0);
      aDocument.Open;
      aDocument.Ref_ID:=FRefID;
      aDocument.BaseID:=FID;
      aDocument.BaseTyp:=FTyp;
      aDocument.BaseLanguage:=FLanguage;
      aDocument.BaseLanguage:=FVersion;
      aDocument.ParentID:=aDirectoryID;
      aDocument.AddFromDir(FolderDialog.FileName,'',fDocumentAddOptions.cbDeletefromFilesystem.Checked,fDocumentAddOptions.ccCalendar.Date);
      fDataSet.DataSet.Refresh;
      DataSet.GotoBookmark(aDocument.GetBookmark);
      AddActualItem;
      aDocument.Free;
      Screen.Cursor := crDefault;
    end;
end;
procedure TfDocumentFrame.acAcquireExecute(Sender: TObject);
var
  NewImage : TJpegImage;
  i: Integer;
  aTop: Integer;
  Stream: TMemoryStream;
  Extension: String;
  Report: TPdfDoc;
  Page: Integer;
  aTitle : string;
  aText : string;
  aDate : TDateTime;
  aDocument: TDocument;
  aMessage: TMessage;
  aPerson: TPerson;
begin
  pmAddDocuments.Close;
  Application.Processmessages;
  if fAcquire.Execute then
    begin
      if fAcquire.cbType.Text = 'JPEG' then
        begin
          //Draw all images (Pages) to one big image
          NewImage := TJpegImage.Create;
          NewImage.Width := 0;
          NewImage.Height := 0;
          NewImage.CompressionQuality:=65;
          for i := 0 to length(fAcquire.Images)-1 do
            begin
              if fAcquire.Images[i].Width > NewImage.Width then
                NewImage.Width := fAcquire.Images[i].Width;
              NewImage.Height := NewImage.Height+fAcquire.Images[i].Height;
            end;
          NewImage.Canvas.Brush.Color := clWhite;
          NewImage.Canvas.Pen.Color := clWhite;
          NewImage.Canvas.Rectangle(0,0,NewImage.Width,NewImage.Height);
          aTop := 0;
          for i := 0 to length(fAcquire.Images)-1 do
            begin
              NewImage.Canvas.Draw(0,aTop,fAcquire.Images[i].Bitmap);
              inc(aTop,fAcquire.Images[i].Height);
            end;
          //Save it to Doc
          Stream := TMemoryStream.Create;
          NewImage.SaveToStream(Stream);
          Stream.Position := 0;
          NewImage.Free;
          Extension := 'jpeg';
        end
      else if fAcquire.cbType.Text = 'PDF' then
        begin
          Report := TPdfDoc.Create;
          Report.NewDoc;
          NewImage := TJpegImage.Create;
          NewImage.CompressionQuality:=65;
          for i := 0 to length(fAcquire.Images)-1 do
            begin
              Report.AddPage;
              Report.Canvas.PageWidth:=fAcquire.Images[i].Width+1;
              Report.Canvas.PageHeight:=fAcquire.Images[i].Height+1;
//              Report.Canvas.PageHeight := trunc(Report.Canvas.PageHeight*0.8);
              NewImage.Width := fAcquire.Images[i].Width;
              NewImage.Height := fAcquire.Images[i].Height;
              NewImage.Canvas.Brush.Color := clWhite;
              NewImage.Canvas.Pen.Color := clWhite;
              NewImage.Canvas.Rectangle(0,0,NewImage.Width,NewImage.Height);
              NewImage.Canvas.Draw(0,0,fAcquire.Images[i].Bitmap);
              Report.AddXObject('Image'+IntToStr(i), CreatePdfImage(NewImage, 'Pdf-Jpeg'));
              Report.Canvas.DrawXObject(0, -1, fAcquire.Images[i].Width, fAcquire.Images[i].Height+1, 'Image'+IntToStr(i));
            end;
          NewImage.Free;
          Stream := TMemoryStream.Create;
          Report.SaveToStream(Stream);
          Stream.Position := 0;
          Report.Free;
          Extension := 'pdf';
        end;
      fDocumentAddOptions.SetLanguage;
      fDocumentAddOptions.cbAddToMessages.Enabled := False;
      fDocumentAddOptions.cbDeletefromFilesystem.Enabled := False;
      fDocumentAddOptions.eName.Text := strNewImage;
      fDocumentAddOptions.eName.Enabled := True;
      aPerson := TPerson.CreateEx(Self,Data,FConnection);
      Data.SetFilter(aPerson,'('+Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(FID)+') OR ('+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(FRefID))+')',1);
      if aPerson.Count > 0 then
        begin
          fDocumentAddOptions.cbAddToMessages.Checked := True;
          fDocumentAddOptions.cbAddToMessages.Enabled := True;
        end
      else
        begin
          fDocumentAddOptions.cbAddToMessages.Checked := false;
        end;
      aText := '';
      aDate := 0;
      aTitle := '';
      for Page := 0 to fAcquire.Texts.Count-1 do
        begin
          uOCR.FixText(TStringList(fAcquire.Texts[Page]));
          if aTitle = '' then
            aTitle := uOCR.GetTitle(TStringList(fAcquire.Texts[Page]));
          if aDate = 0 then
            aDate := uOCR.GetDate(TStringList(fAcquire.Texts[Page]));
          aText := aText+TStringList(fAcquire.Texts[Page]).Text;
        end;
      if aTitle <> '' then
        fDocumentAddOptions.eName.Text := aTitle;
      fDocumentAddOptions.ccCalendar.Date:=Now();
      if aDate <> 0 then
        fDocumentAddOptions.ccCalendar.Date := aDate;
      if not fDocumentAddOptions.Execute then exit;
      aDocument := TDocument.CreateEx(Self,Data);
      aDocument.Select(0);
      aDocument.Open;
      aDocument.Ref_ID:=FRefID;
      aDocument.BaseID:=FID;
      aDocument.BaseTyp:=FTyp;
      aDocument.BaseLanguage:=FLanguage;
      aDocument.BaseLanguage:=FVersion;
      aDocument.ParentID:=aDirectoryID;
      aDocument.AddFromStream(fDocumentAddOptions.eName.text,
                              Extension,
                              Stream,
                              aText,
                              fDocumentAddOptions.ccCalendar.Date);
      Stream.Free;
      for i := 0 to fAcquire.Texts.Count-1 do
        TStringList(fAcquire.Texts[i]).Free;
      fAcquire.Texts.Clear;
      if aPerson.Count > 0 then
        begin
          if fDocumentAddOptions.cbAddToMessages.Checked then
            begin
              aMessage := TMessage.CreateEx(Self,Data);
              aMessage.Select(0);
              aMessage.Open;
              with aMessage.DataSet do
                begin
                  Insert;
                  FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
                  FieldByName('TYPE').AsString := 'LETTE';
                  FieldByName('ID').AsString := Data.BuildLink(aDocument.DataSet);
                  FieldByName('MSG_ID').AsVariant := Data.GetUniID;
                  FieldByName('TREEENTRY').AsVariant := TREE_ID_MESSAGES;
                  FieldByName('READ').AsString := 'N';
                  FieldByName('SENDER').AsString := aPerson.Text.AsString;
                  FieldByName('SENDDATE').AsDateTime := fDocumentAddOptions.ccCalendar.Date;
                  FieldByName('SUBJECT').AsString := fDocumentAddOptions.eName.text;
                  Post;
                end;
              aMessage.Free;
            end;
          aPerson.History.Open;
          with aPerson.History.DataSet do
            begin
              Insert;
              FieldByName('LINK').AsString := Data.BuildLink(DataSet.DataSet);
              FieldByName('ACTIONICON').AsInteger := ACICON_DOCUMENTADDED;
              FieldByName('ACTION').AsString := strDocumentadded;
              FieldByName('REFERENCE').AsString := fDocumentAddOptions.eName.text;
              FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              Post;
            end;
        end;
      aPerson.Free;
      fDataSet.DataSet.Refresh;
      DataSet.GotoBookmark(aDocument.GetBookmark);
      AddActualItem(True);
      aDocument.Free;
    end;
end;
procedure TfDocumentFrame.acBigIconsExecute(Sender: TObject);
begin
  lvDocuments.ViewStyle:=vsIcon;
end;
procedure TfDocumentFrame.acCheckinFromDirExecute(Sender: TObject);
var
  aID: Int64;
  aDocument: TDocument;
  FileList: TStrings;
  Result: Boolean;
  aDir: String;
begin
  if FolderDialog.Execute then
    begin
      if GotoSelected then
        begin
          Screen.Cursor := crHourglass;
          aDocument := TDocument.CreateEx(Self,Data);
          aID := DataSet.FieldByName('NUMBER').AsInteger;
          aDocument.SelectByNumber(aID);
          aDocument.Open;
          aDocument.Ref_ID:=FRefID;
          aDocument.BaseID:=FID;
          aDocument.BaseTyp:=FTyp;
          aDocument.BaseLanguage:=FLanguage;
          aDocument.BaseLanguage:=FVersion;
          aDocument.ParentID:=aDirectoryID;
          //Collect Checkin Documents
          aDir := FolderDialog.FileName;
          if rpos(DirectorySeparator,aDir) > 0 then
            aDir := copy(aDir,0,rpos(DirectorySeparator,aDir)-1);
          aDocument.OnCheckCheckinFiles:=@aDocumentCheckCheckinFiles;
          FileList := aDocument.CollectCheckInFiles(aDir);
          if aDocument.CheckCheckinFiles(FileList,aDir) then
            begin
              //Checkin
              if FileList.Count > 0 then
                begin
                  Result := aDocument.CheckinFiles(FileList,aDir);
                  if not Result then Showmessage(strFailed);
                end;
            end;
          FileList.Free;
          fDataSet.DataSet.Refresh;
          aDocument.Free;
          Screen.Cursor := crDefault;
        end;
    end;
end;

procedure TfDocumentFrame.acCheckinFromFileExecute(Sender: TObject);
var
  FileList: TStringList;
  aDocument: TDocument;
  aID: Integer;
  Result: Boolean;
begin
  if DocumentDialog.Execute then
    begin
      if GotoSelected then
        begin
          Screen.Cursor := crHourglass;
          aDocument := TDocument.CreateEx(Self,Data);
          aID := DataSet.FieldByName('NUMBER').AsInteger;
          aDocument.SelectByNumber(aID);
          aDocument.Open;
          aDocument.Ref_ID:=FRefID;
          aDocument.BaseID:=FID;
          aDocument.BaseTyp:=FTyp;
          aDocument.BaseLanguage:=FLanguage;
          aDocument.BaseLanguage:=FVersion;
          aDocument.ParentID:=aDirectoryID;
          //Collect Checkin Documents
          FileList := TStringList.Create;
          FileList.Add(DocumentDialog.FileName+'=C');
          Result := aDocument.CheckinFiles(FileList,ExtractFileDir(DocumentDialog.FileName));
          if not Result then Showmessage(strFailed);
          FileList.Free;
          fDataSet.DataSet.Refresh;
          aDocument.Free;
          Screen.Cursor := crDefault;
        end;
    end;
end;
procedure TfDocumentFrame.acCheckoutToRevisionExecute(Sender: TObject);
var
  aDocument: TDocument;
  aID: Int64;
  asRev: String;
  aRev: Integer;
begin
  if not GotoSelected then exit;
  if InputQuery(strRevision,strRevision,asRev) then
    begin
      aRev := StrToInt(asRev);
      if DataSet.FieldByName('ISDIR').AsString = 'Y' then
        begin
          if FolderDialog.Execute then
            begin
              aDocument := TDocument.CreateEx(Self,Data);
              aID := DataSet.FieldByName('NUMBER').AsInteger;
              aDocument.SelectByNumber(aId);
              aDocument.Open;
              aDocument.DoCheckout(FolderDialog.FileName,arev);
              aDocument.Free;
            end;
        end
      else
        begin
          if DataSet.FieldByName('EXTENSION').AsString <> '' then
            DocumentSaveDialog.FileName := UniToSys(DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString)
          else
            DocumentSaveDialog.FileName := UniToSys(DataSet.FieldByName('NAME').AsString);
          if DocumentSaveDialog.Execute then
            begin
              aID := DataSet.FieldByName('NUMBER').AsInteger;
              aDocument := TDocument.CreateEx(Self,Data);
              aDocument.SelectByNumber(aId);
              aDocument.Open;
              aDocument.DoCheckout(ExtractFileDir(DocumentSaveDialog.Filename),arev);
              aDocument.Free;
            end;
        end;
    end;
end;
procedure TfDocumentFrame.acCopyAsLinkExecute(Sender: TObject);
var
  aLinks : string = '';
  Stream: TStringStream;
  i: Integer;
begin
  if not GotoSelected then exit;
  aLinks := Data.BuildLink(DataSet.DataSet)+';';
  Stream := TStringStream.Create(aLinks);
  Clipboard.AddFormat(LinkClipboardFormat,Stream);
  Stream.Free;
end;
procedure TfDocumentFrame.acCopyTextLinkExecute(Sender: TObject);
begin
  if GotoSelected then
    Clipboard.AsText:=Data.BuildLink(DataSet.DataSet);
end;
procedure TfDocumentFrame.acCreateDirectoryExecute(Sender: TObject);
var
  aDocument: TDocument;
  DocID: Int64;
begin
  DocID := Data.GetUniID(Connection);
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(0);
  aDocument.Open;
  aDocument.Ref_ID:=FRefID;
  aDocument.BaseID:=FID;
  aDocument.BaseTyp:=FTyp;
  aDocument.BaseLanguage:=FLanguage;
  aDocument.BaseVersion:=FVersion;
  aDocument.ParentID:=aDirectoryID;
  aDocument.CreateDirectory(InputBox(strName,strName,strNewDir));
  fDataSet.DataSet.Refresh;
  DataSet.GotoBookmark(aDocument.GetBookmark);
  AddActualItem;
  aDocument.Free;
end;
procedure TfDocumentFrame.acCreateFromTemplateExecute(Sender: TObject);
var
  Stream: TMemoryStream;
  i: Integer;
  a: Integer;
  tmp: string;
  aDocument: TDocument;
begin
  FSelectTemplate.DataSet := TDocumentTemplates.Create(nil);
  FSelectTemplate.DataSet.CreateTable;
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(0);
  if fSelectTemplate.Execute(FTyp,aDocument) then
    begin
      Stream := TMemoryStream.Create;
      Data.BlobFieldToStream(FSelectTemplate.DataSet.DataSet,'DOCUMENT',Stream);
      Stream.Position := 0;
{      if fSelectTemplate.cbReplacePlaceholders.Checked then
      if (copy(Uppercase(Data.Templates.FieldByName('EXTENSION').AsString),0,2) = 'OD') then
        begin
          Stream.SaveToFile(GetInternalTempDir+Data.Templates.FieldByName('NAME').AsString+'.'+Data.Templates.FieldByName('EXTENSION').AsString);
          Doc := TODFDocument.Create(GetInternalTempDir+Data.Templates.FieldByName('NAME').AsString+'.'+Data.Templates.FieldByName('EXTENSION').AsString);
          for i := 0 to Doc.Count-1 do
            begin
              tmp := copy(Doc.Values[i],2,length(Doc.Values[i])-2);
              for a := 0 to Data.ComponentCount-1 do
                begin
                  if (Data.Components[a] is TDataSource) and (Uppercase(TDataSource(Data.Components[a]).DataSet.Name) = Uppercase(copy(tmp,0,pos('.',tmp)-1))) then
                    if TDataSource(Data.Components[a]).DataSet.FieldDefs.IndexOf(copy(tmp,pos('.',tmp)+1,length(tmp))) >= 0 then
                      Doc.Values[i] := TDataSource(Data.Components[a]).FieldByName(copy(tmp,pos('.',tmp)+1,length(tmp))).AsString;
                end;
            end;
          Doc.Save;
          Doc.Free;
          Stream.LoadFromFile(GetInternalTempDir+Data.Templates.FieldByName('NAME').AsString+'.'+Data.Templates.FieldByName('EXTENSION').AsString);
          SysUtils.DeleteFile(GetInternalTempDir+Data.Templates.FieldByName('NAME').AsString+'.'+Data.Templates.FieldByName('EXTENSION').AsString);
        end;
}
      aDocument := TDocument.CreateEx(Self,Data);
      aDocument.Select(0);
      aDocument.Open;
      aDocument.Ref_ID:=FRefID;
      aDocument.BaseID:=FID;
      aDocument.BaseTyp:=FTyp;
      aDocument.BaseLanguage:=FLanguage;
      aDocument.BaseVersion:=FVersion;
      aDocument.ParentID:=aDirectoryID;
      aDocument.AddFromStream(FSelectTemplate.DataSet.FieldByName('NAME').AsString,
                            FSelectTemplate.DataSet.FieldByName('EXTENSION').AsString,
                            Stream,
                            '',
                            Now());
      Stream.Free;
      DataSet.DataSet.Refresh;
      DataSet.GotoBookmark(aDocument.GetBookmark);
      AddActualItem(True);
      DoEditDocument;
    end;
  aDocument.Free;
  FSelectTemplate.DataSet.Free;
end;
procedure TfDocumentFrame.acDetailsExecute(Sender: TObject);
begin
  lvDocuments.ViewStyle:=vsReport;
end;
procedure TfDocumentFrame.acDocumentActionsExecute(Sender: TObject);
var
  aDocument: TDocument;
begin
  fDocumentAction.SetLanguage;
  aDocument := TDocument.CreateEx(Self,Data);
  if GotoSelected then
    begin
      aDocument.SelectByNumber(FDataSet.FieldByName('NUMBER').AsVariant);
      aDocument.Open;
      fDocumentAction.Execute(aDocument);
    end;
  aDocument.Free;
end;

procedure TfDocumentFrame.acDocumentPropertiesExecute(Sender: TObject);
var
  aDocument: TDocument;
  aID: Integer;
begin
  if GotoSelected then
    begin
      aID := DataSet.FieldByName('NUMBER').AsInteger;
      aDocument := TDocument.CreateEx(Self,Data);
      aDocument.SelectByNumber(aId);
      aDocument.Open;
      fDocProperties.Execute(aDocument);
      aDocument.Free;
    end;
end;

procedure TfDocumentFrame.acExecuteExecute(Sender: TObject);
begin
  acViewFile.Execute;
end;
procedure TfDocumentFrame.acListExecute(Sender: TObject);
begin
  lvDocuments.ViewStyle:=vsList;
end;
procedure TfDocumentFrame.acMoveLinkExecute(Sender: TObject);
var
  Stream: TStringStream;
  aDocument: TDocument;
begin
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          aDocument := TDocument.CreateEx(Self,Data);
          aDocument.SelectByLink(Stream.DataString);
          aDocument.Open;
          aDocument.MoveTo(FRefID,FTyp,FID,FVersion,FLanguage,aDirectoryID);
          DataSet.DataSet.Refresh;
          DataSet.GotoBookmark(aDocument.GetBookmark);
          AddActualItem;
          aDocument.Free;
        end;
    end;
end;
procedure TfDocumentFrame.acPasteAsLinkExecute(Sender: TObject);
var
  Stream: TStringStream;
  aDocument: TDocument;
begin
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          aDocument := TDocument.CreateEx(Self,Data);
          aDocument.Select(0);
          aDocument.Open;
          aDocument.Ref_ID:=FRefID;
          aDocument.BaseID:=FID;
          aDocument.BaseTyp:=FTyp;
          aDocument.BaseLanguage:=FLanguage;
          aDocument.BaseVersion:=FVersion;
          aDocument.ParentID:=aDirectoryID;
          aDocument.AddFromLink(Stream.DataString);
          DataSet.DataSet.Refresh;
          DataSet.GotoBookmark(aDocument.GetBookmark);
          AddActualItem;
          aDocument.Free;
        end;
    end;
end;

procedure TfDocumentFrame.acRefreshExecute(Sender: TObject);
begin
  Refresh(FRefID,FTyp,FID,FVersion,FLanguage,aDirectoryID);
end;

procedure TfDocumentFrame.acRemoveDocumentExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      if not GotoSelected then exit;
      if DataSet.Count > 0 then
        DataSet.Delete;
      lvDocuments.Items.Delete(lvDocuments.Selected.Index);
    end;
end;
procedure TfDocumentFrame.acRenameExecute(Sender: TObject);
begin
  if not GotoSelected then exit;
  DataSet.DataSet.Edit;
  DataSet.FieldByName('NAME').AsString := InputBox(strRename,strNewName,DataSet.FieldByName('NAME').AsString);
  DataSet.DataSet.Post;
  lvDocuments.Selected.Caption:=DataSet.FieldByName('NAME').AsString;
end;
procedure TfDocumentFrame.acRightsExecute(Sender: TObject);
begin
  if not GotoSelected then exit;
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfDocumentFrame.acSaveToFileExecute(Sender: TObject);
var
  aDocument: TDocument;
  aID: Int64;
begin
  if not GotoSelected then exit;
  fWaitForm.ShowInfo(Format(strCheckingOutFile,['']));
  fWaitForm.Show;
  if DataSet.FieldByName('ISDIR').AsString = 'Y' then
    begin
      if FolderDialog.Execute then
        begin
          aDocument := TDocument.CreateEx(Self,Data);
          aID := DataSet.FieldByName('NUMBER').AsInteger;
          aDocument.SelectByNumber(aId);
          aDocument.Open;
          aDocument.OnCheckCheckOutFile:=@aDocumentCheckCheckOutFile;
          aDocument.DoCheckout(FolderDialog.FileName);
          aDocument.Free;
        end;
    end
  else
    begin
      if DataSet.FieldByName('EXTENSION').AsString <> '' then
        DocumentSaveDialog.FileName := UniToSys(DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString)
      else
        DocumentSaveDialog.FileName := UniToSys(DataSet.FieldByName('NAME').AsString);
      if DocumentSaveDialog.Execute then
        begin
          aID := DataSet.FieldByName('NUMBER').AsInteger;
          aDocument := TDocument.CreateEx(Self,Data);
          aDocument.SelectByNumber(aId);
          aDocument.Open;
          aDocument.OnCheckCheckOutFile:=@aDocumentCheckCheckOutFile;
          aDocument.DoCheckout(ExtractFileDir(DocumentSaveDialog.Filename));
          aDocument.Free;
        end;
    end;
  fWaitForm.hide;
end;
procedure TfDocumentFrame.acSmallIconsExecute(Sender: TObject);
begin
  lvDocuments.ViewStyle:=vsSmallIcon;
end;
procedure TfDocumentFrame.acViewFileExecute(Sender: TObject);
begin
  DoEditDocument('VIEW');
end;
function TfDocumentFrame.aDocumentCheckCheckinFiles(aFiles: TStrings;Directory: string;var Desc : string): Boolean;
var
  aHist : IBaseHistory;
begin
  Result := fDocumentCheckin.Execute(aFiles,Directory);
  Application.ProcessMessages;
  Screen.Cursor:=crHourGlass;
  if Result and Assigned(fDocumentCheckin) then
    begin
      Desc := fDocumentCheckin.mComment.Text;
      if Desc <> '' then
        if Assigned(FBaseElement) then
          if Supports(BaseElement,IBaseHistory,aHist) then
            aHist.History.AddItem(DataSet.DataSet,Desc,'','',nil,ACICON_DOCUMENTADDED);
    end;
  Screen.Cursor:=crDefault;
end;

procedure TfDocumentFrame.aDocumentCheckCheckOutFile(aFile: string);
begin
  fWaitForm.ShowInfo(Format(strCheckingOutFile,[aFile]));
end;

procedure TfDocumentFrame.DoRefresh;
var
  aNew: TListItem;
  i: Integer;
begin
  lvDocuments.BeginUpdate;
  lvDocuments.Clear;
  if not VarIsNull(FrefID) then
    begin
      while Smallicons.Count > 12 do
        SmallIcons.Delete(12);
      while Bigicons.Count > 12 do
        BigIcons.Delete(12);
      if FaParent <> 0 then
        begin
          aNew := lvDocuments.Items.Add;
          aNew.Caption:='..';
          aNew.ImageIndex := 5;
        end
      else
        begin
          Setlength(DirectoryIDs,0);
          aDirectoryID := 0;
        end;
      with DataSet.DataSet do
        begin
          First;
          while not EOF do
            begin
              AddActualItem;
              Next;
            end;
        end;
    end;
  lvDocuments.EndUpdate;
  for i := 0 to lvDocuments.Items.Count-1 do
    lvDocuments.Items[i].Selected:=False;
  lvDocuments.SortColumn:=lvDocuments.SortColumn;
end;
procedure TfDocumentFrame.AddActualItem(aInsert : Boolean = False);
var
  aNew: TListItem;
  Stream : TMemoryStream;
  TargetBitmap: TBitmap;
  TmpImg: TPicture;
begin
  if aInsert then
    begin
      if (lvDocuments.Items.Count>0) and (lvDocuments.Items[0].Caption='..') then
        aNew := lvDocuments.Items.Insert(1)
      else
        aNew := lvDocuments.Items.Insert(0);
    end
  else
    aNew := lvDocuments.Items.Add;
  aNew.Caption := DataSet.FieldByName('NAME').AsString;
  if DataSet.FieldByName('ISDIR').AsString <> 'Y' then
    begin
      aNew.SubItems.Add(Format(strFileExtDesc,[DataSet.FieldByName('EXTENSION').AsString]));
    end
  else
    begin
      aNew.SubItems.Add('');
    end;
  if TDocuments(DataSet).Size >-1 then
    aNew.SubItems.Add(Utils.SizeToText(TDocuments(DataSet).Size))
  else
    aNew.SubItems.Add('');
  aNew.SubItems.Add(DateTimeToStr(TDocuments(DataSet).LastModified));
  if DataSet.FieldByName('ISLINK').AsString = 'Y' then
    begin
      if DataSet.FieldByName('ISDIR').AsString = 'Y' then
        aNew.ImageIndex := 10
      else
        aNew.ImageIndex := 11;
    end
  else
    begin
      if DataSet.FieldByName('ISDIR').AsString = 'Y' then
        aNew.ImageIndex := 7
      else
        aNew.ImageIndex := 9;
    end;
  aNew.SubItems.Add(IntToStr(DataSet.GetBookmark));
end;
function TfDocumentFrame.GotoSelected: Boolean;
begin
  Result := False;
  if lvDocuments.Selected = nil then exit;
  if lvDocuments.Selected.SubItems.Count < 4 then exit;
  if DataSet.GotoBookmark(StrToInt64(lvDocuments.Selected.SubItems[3])) then
    Result := True;
end;
procedure TfDocumentFrame.DoEditDocument(Method: string; ShowEditor: Boolean);
var
  DoDelete : Boolean = False;
  Filename: String;
  UseStarter : Boolean;
  Rec : string;
  TempID : string;
  FExit: LongInt;
  aDocument: TDocument;
  bDocument: TDocument;
  aID: Integer;
begin
  if not GotoSelected then exit;
  Randomize;
  TempID := ValidateFileName(Data.BuildLink(DataSet.DataSet)+IntToStr(Random(99999)));
  TempID := StringReplace(TempID,'{','_',[rfReplaceAll]);
  TempID := StringReplace(TempID,'}','_',[rfReplaceAll]);
  aID := DataSet.FieldByName('NUMBER').AsInteger;
  bDocument := TDocument.CreateEx(Self,Data);
  bDocument.SelectByNumber(aId);
  bDocument.Open;
  Data.SetFilter(bDocument.DocumentActions,Data.QuoteField('NUMBER')+'='+Data.QuoteValue(DataSet.FieldByName('NUMBER').AsString));
  if bDocument.DocumentActions.Count > 0 then
    begin
      UseStarter := not (bDocument.DocumentActions.FieldByName('USESTARTER').AsString = 'N');
      UseStarter := UseStarter and ((FileExistsUTF8(ExtractFilePath(Application.Exename)+'pstarter'+ExtractFileExt(Application.Exename)))
                                 or (DirectoryExistsUTF8(FileUtil.CleanAndExpandDirectory(Application.Location+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator)+'pstarter.app')));
      if bDocument.DocumentActions.FieldByName('ACTION').AsString <> 'N' then
        begin
          if (bDocument.DocumentActions.FieldByName('CODIR').AsString = 'Y') and (DataSet.FieldByName('ISDIR').AsString = 'Y') then
            begin
              DoDelete := True;
              fWaitForm.SetLanguage;
              fWaitForm.lStep.Caption := '';
              fWaitForm.Show;
              Application.ProcessMessages;
              aDocument := TDocument.Create(nil);
              aDocument.SelectByNumber(DataSet.FieldByName('NUMBER').AsInteger);
              aDocument.Open;
              aDocument.DoCheckout(copy(aDocument.GetCheckOutPath('',TempID),0,rpos(DirectorySeparator,aDocument.GetCheckOutPath('',TempID))-1));
              fWaitForm.Hide;
            end
          else
            begin
              aDocument := TDocument.Create(nil);
              aDocument.SelectByNumber(DataSet.FieldByName('NUMBER').AsInteger);
              aDocument.Open;
              aDocument.DoCheckout(copy(aDocument.GetCheckOutPath('',TempID),0,rpos(DirectorySeparator,aDocument.GetCheckOutPath('',TempID))-1));
//              aDocument.DoCheckout(aDocument.GetCheckOutPath('',TempID));
            end;
          aDocument.AftercheckInFiles:=FAfterCheckinFiles;
          if bDocument.DocumentActions.FieldByName('ACTION').AsString = 'S' then
            begin //Special Action
              aDocument.OnCheckCheckinFiles:=@aDocumentCheckCheckinFiles;
              TDocExecuteThread.Create(aDocument,StringReplace(bDocument.DocumentActions.FieldByName('ACTIONCMD').AsString,'%NAME%',aDocument.GetCheckoutPath('',TempID),[rfReplaceAll]),DoDelete,UseStarter,TempID,bDocument.DocumentActions.Id.AsVariant,Null);
              Screen.Cursor := crDefault;
              exit;
            end
          else if bDocument.DocumentActions.FieldByName('ACTION').AsString = 'D' then
            begin //Standart Action
              //TODO:Checkout single file if CODIR=N
            end;
        end;
    end
  else if (DataSet.FieldByName('ISDIR').AsString = 'Y') then exit;

  //Find Standart Action and execute
  bDocument.MimeTypes.Open;
  bDocument.MimeTypes.DataSet.Filtered := False;
  bDocument.MimeTypes.DataSet.First;
  while not bDocument.MimeTypes.DataSet.EOF do
    begin
      if pos(Uppercase(DataSet.FieldByName('EXTENSION').AsString),UpperCase(bDocument.MimeTypes.FieldByName('EXTENSIONS').AsString)) > 0 then
        break;
      bDocument.MimeTypes.DataSet.Next;
    end;
  if (pos(Uppercase(DataSet.FieldByName('EXTENSION').AsString),UpperCase(bDocument.MimeTypes.FieldByName('EXTENSIONS').AsString)) = 0)
  or (bDocument.MimeTypes.FieldByName(Method).AsString = '') then
    begin
      //add extension
      fMimeTypeEdit.SetLanguage;
      fMimeTypeEdit.SetupDB;
      fMimeTypeEdit.eOpenWith.DataField := Method;
      if (pos(DataSet.FieldByName('EXTENSION').AsString,bDocument.MimeTypes.FieldByName('EXTENSIONS').AsString) = 0) then
        begin
          bDocument.MimeTypes.DataSet.Insert;
          bDocument.MimeTypes.FieldByName('MIME').AsString := GetMimeTypeForExtension(DataSet.FieldByName('EXTENSION').AsString);
        end;
      bDocument.MimeTypes.DataSet.Edit;
      if (Method = 'EDIT') or (Method = 'VIEW') then
        bDocument.MimeTypes.FieldByName(Method).AsString := GetProcessforExtension(piOpen,DataSet.FieldByName('EXTENSION').AsString)
      else if Method = 'PRINT' then
        bDocument.MimeTypes.FieldByName(Method).AsString := GetProcessforExtension(piPrint,DataSet.FieldByName('EXTENSION').AsString);
      if pos('%s',bDocument.MimeTypes.FieldByName(Method).AsString) = 0 then
        bDocument.MimeTypes.FieldByName(Method).AsString := bDocument.MimeTypes.FieldByName(Method).AsString+' "%s"';
      if not fMimeTypeEdit.Execute(bDocument) then
        begin
          if bDocument.MimeTypes.DataSet.State = dsInsert then
            bDocument.MimeTypes.DataSet.Cancel;
          exit;
        end;
      if bDocument.MimeTypes.FieldByName(Method).AsString = '' then
        begin
          if bDocument.MimeTypes.DataSet.State = dsInsert then
            bDocument.MimeTypes.DataSet.Cancel
          else
            bDocument.MimeTypes.DataSet.Delete;
          exit;
        end;
      if pos(UpperCase(DataSet.FieldByName('EXTENSION').AsString),UpperCase(bDocument.MimeTypes.FieldByName('EXTENSIONS').AsString)) = 0 then
        begin
          bDocument.MimeTypes.DataSet.Edit;
          bDocument.MimeTypes.FieldByName('EXTENSIONS').AsString := bDocument.MimeTypes.FieldByName('EXTENSIONS').AsString+DataSet.FieldByName('EXTENSION').AsString+',';
          bDocument.MimeTypes.DataSet.Post;
        end;
      if bDocument.MimeTypes.DataSet.State = dsEdit then
        bDocument.MimeTypes.DataSet.Post;
    end;
  fWaitForm.SetLanguage;
  fWaitForm.lStep.Caption := '';
  fWaitForm.Show;
  Application.ProcessMessages;
  aDocument := TDocument.Create(nil);
  aDocument.SelectByNumber(DataSet.FieldByName('NUMBER').AsInteger);
  aDocument.Open;
  Filename := aDocument.GetCheckoutPath('',TempID);
  aDocument.DoCheckout(copy(Filename,0,rpos(DirectorySeparator,Filename)-1));
  fWaitForm.Hide;
  DoDelete := True;
  UseStarter := FileExistsUTF8(ExtractFilePath(Application.Exename)+'pstarter'+ExtractFileExt(Application.Exename));
  aDocument.AftercheckInFiles:=FAfterCheckinFiles;
  aDocument.OnCheckCheckinFiles:=@aDocumentCheckCheckinFiles;
  TDocExecuteThread.Create(aDocument,'exec:'+StringReplace(bDocument.MimeTypes.FieldByName(Method).AsString,'%s',filename,[rfReplaceAll]),DoDelete,UseStarter and (bDocument.MimeTypes.FieldByName('USESTARTER').AsString<>'N'),TempID,Null,bDocument.MimeTypes.Id.AsVariant);
  bDocument.Free;
end;

procedure TfDocumentFrame.SetHasPreview(AValue: Boolean);
begin
  if FHasPreview=AValue then Exit;
  FHasPreview:=AValue;
  pPreviewT.Visible:=FHasPreview;
  spPreview.Visible:=FHasPreview;
end;

procedure TfDocumentFrame.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;
procedure TfDocumentFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  if AValue.DataSet.Active then
    begin
      DoRefresh;
      FRefID := DataSet.FieldByName('REF_ID_ID').AsVariant;
      FAParent := 0;
      FTyp := DataSet.FieldByName('TYPE').AsString;
      if DataSet.DataSet.FieldDefs.IndexOf('ID') <> -1 then
        begin
          FId := DataSet.FieldByName('ID').AsString;
          FVersion := DataSet.FieldByName('VERSION').AsVariant;
          FLanguage := DataSet.FieldByName('LANGUAGE').AsVariant;
        end;
    end;
end;
constructor TfDocumentFrame.Create(AOwner: TComponent);
var
  aOrderType: TOrderTyp;
  aItem: TMenuItem;
begin
  inherited Create(AOwner);
  Caption := strFiles;
  PreviewFrame := TfPreview.Create(Self);
  PreviewFrame.Parent := pPreviewT;
  PreviewFrame.Align := alClient;
  PreviewFrame.Show;
  FEditable := True;
  FHasPreview:=True;
end;
destructor TfDocumentFrame.Destroy;
begin
  if Assigned(Parent) then Parent := nil;
  FDataSet := nil;
  PreviewFrame.Destroy;
  inherited Destroy;
end;
function TfDocumentFrame.OpenFromLink(aLink: string) : Boolean;
begin
  Result := False;
end;
procedure TfDocumentFrame.SetLanguage;
begin
end;

function TfDocumentFrame.SaveFileToDir(aDir: string): Boolean;
var
  aID: Integer;
  aDocument: TDocument;
begin
  if not GotoSelected then exit;
  aID := DataSet.FieldByName('NUMBER').AsInteger;
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.SelectByNumber(aId);
  aDocument.Open;
  aDocument.OnCheckCheckOutFile:=@aDocumentCheckCheckOutFile;
  aDocument.DoCheckout(aDir);
  aDocument.Free;
end;

procedure TfDocumentFrame.Refresh(RefID: Variant;Typ, ID: string; Version: Variant; Language: Variant;
  aParent: Integer);
begin
  if not VarISNull(RefID) then
    begin
      TDocuments(DataSet).Select(RefID,Typ,ID,Version,Language,aParent);
      TDocuments(DataSet).Open;
    end;
  FRefID := RefID;
  FTyp := Typ;
  FID := ID;
  FVersion := Version;
  FLanguage := Language;
  FAParent := aParent;
  DoRefresh;
end;
procedure TfDocumentFrame.Refresh(RefID: Variant;Typ : string; aParent: Integer);
begin
  if not VarISNull(RefID) then
    begin
      TDocuments(DataSet).Select(RefID,Typ,aParent);
      TDocuments(DataSet).Open;
    end;
  FRefID := RefID;
  FTyp := Typ;
  FID := '';
  FVersion := Null;
  FLanguage := Null;
  FAParent := aParent;
  DoRefresh;
end;
procedure TfDocumentFrame.DoOnDropFiles(Sender : TObject;const Filenames: array of string);
var
  i: Integer;
  aID: Int64;
  aDocument: TDocument;
  DelOK: Boolean;
label DelRetry;
begin
  if not Visible then exit;
  if length(Filenames) = 0 then exit;
  if not FEditable then exit;
  fDocumentAddOptions.SetLanguage;
  fDocumentAddOptions.cbAddToMessages.Enabled := False;
  fDocumentAddOptions.cbDeletefromFilesystem.Enabled := True;
  fDocumentAddOptions.eName.Text := '';
  fDocumentAddOptions.eName.Enabled := False;
  fDocumentAddOptions.ccCalendar.Date:=Now();
  if not fDocumentAddOptions.Execute then exit;
  Screen.Cursor := crHourglass;
  for i := 0 to length(Filenames)-1 do
    begin
      if DirectoryExists(Filenames[i]) then
        begin
          aDocument := TDocument.CreateEx(Self,Data);
          aDocument.Select(0);
          aDocument.Open;
          aDocument.Ref_ID:=FRefID;
          aDocument.BaseID:=FID;
          aDocument.BaseTyp:=FTyp;
          aDocument.BaseLanguage:=FLanguage;
          aDocument.BaseLanguage:=FVersion;
          aDocument.ParentID:=aDirectoryID;
          aDocument.AddFromDir(FileNames[i],'',fDocumentAddOptions.cbDeletefromFilesystem.Checked,fDocumentAddOptions.ccCalendar.Date);
          fDataSet.DataSet.Refresh;
          DataSet.GotoBookmark(aDocument.GetBookmark);
          AddActualItem;
          aDocument.Free;
        end
      else
        begin
          aDocument := TDocument.CreateEx(Self,Data);
          aDocument.Select(0);
          aDocument.Open;
          aDocument.Ref_ID:=FRefID;
          aDocument.BaseID:=FID;
          aDocument.BaseTyp:=FTyp;
          aDocument.BaseLanguage:=FLanguage;
          aDocument.BaseVersion:=FVersion;
          aDocument.ParentID:=aDirectoryID;
          aDocument.AddFromFile(FileNames[i],'',fDocumentAddOptions.ccCalendar.Date);
          if fDocumentAddOptions.cbDeletefromFilesystem.Checked then
            begin
              with BaseApplication as IBaseConfig do
                begin
                  DelRetry:
                    case Config.ReadInteger('DELETEMETHOD',0) of
                    0:DelOK := DeleteFileUTF8(FileNames[i]);
                    1:DelOK := DeleteSecure(FileNames[i]);
                    2:DelOK := DeleteSecure(FileNames[i],dmDoD522022);
                    3:DelOK := DeleteSecure(FileNames[i],dmOverride);
                    end;
                    if not DelOK then
                      if MessageDlg(strDelete,strCantDeleteTempDirectory,mtWarning,[mbRetry,mbAbort],0) = mrRetry then
                        goto DelRetry;
                end;
            end;
          DataSet.DataSet.Refresh;
          DataSet.GotoBookmark(aDocument.GetBookmark);
          AddActualItem(True);

          AddActualItem;
        end;
    end;
  Refresh(FRefID,FTyp,FID,FVersion,FLanguage,aDirectoryID);
  Screen.Cursor := crDefault;
end;
procedure TfDocumentFrame.SetRights(Editable: Boolean);
begin
  FEditable := Editable;
  acAddDocument.Enabled:=Editable;
  acAddfolder.Enabled:=Editable;
  acRemoveDocument.Enabled:=Editable;
  acCreateDirectory.Enabled:=Editable;
  acAcquire.Enabled := Editable;
  acCreateFromTemplate.Enabled := Editable;
  acPasteAsLink.Enabled:=Editable;
  acCheckinFromDir.Enabled:=Editable;
  acRights.Enabled:=Data.Users.Rights.Right('DOCUMENTS') >= RIGHT_PERMIT;
  ArrangeToolBar(pToolbar,ActionList1,'Document');
end;
procedure TfDocumentFrame.ShowFrame;
var
  bParent: TWinControl;
begin
  inherited ShowFrame;
  with Application as IBaseDbInterface do
    pPreviewT.Width := (Width-lvDocuments.Left)-DBConfig.ReadInteger('DOCPREVIEWWIDTH',400);
  spPreview.Left := 0;
  if Visible then
    begin
      try
        if Assigned(FDropForm) then
          begin
            FDropForm.OnDropFiles := nil;
            FDropForm.AllowDropFiles:=False;
          end;
      except
      end;
      bParent := Parent;
      while Assigned(bParent) do
        begin
          bParent := bParent.Parent;
          if Assigned(bParent) and (bParent is TForm) then break;
        end;
      if Assigned(bParent) and (bParent is TForm) then
        begin
          FDropForm := TForm(bParent);
    //      if not Assigned(FDropForm.OnDropFiles) then
            FDropForm.OnDropFiles:=@DoOnDropFiles;
            FDropForm.AllowDropFiles:=True;
        end;
    end;
end;
procedure TfDocumentFrame.SelectDocument(aLink: string);
var
  i: Integer;
  aDoc: TDocuments;
begin
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  aDoc := TDocuments.CreateEx(Self,Data);
  aDoc.SelectByLink(aLink);
  aDoc.Open;
  if aDoc.Count > 0 then
    begin
      aLink := aDoc.Id.AsString;
      for i := 0 to lvDocuments.Items.Count-1 do
        begin
          if lvDocuments.Items[i].SubItems.Count >= 4 then
            if lvDocuments.Items[i].SubItems[3] = aLink then
              begin
                lvDocuments.Selected := lvDocuments.Items[i];
                break;
              end;
        end;
    end;
  aDoc.Free;
end;
function TfDocumentFrame.GotoEntry(aItem: TListItem): Boolean;
begin
  Result := False;
  if not Assigned(aItem) then exit;
  if aItem.SubItems.Count < 4 then exit;
  if DataSet.GotoBookmark(StrToInt64(aItem.SubItems[3])) then
    Result := True;
end;

end.

