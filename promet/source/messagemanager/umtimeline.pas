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
Created 07.10.2013
*******************************************************************************}
unit umtimeline;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, types,  Forms, Controls, Graphics, Dialogs,
  DBGrids, Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils,
  ZVDateTimePicker, uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage, ugridview,
  uExtControls, uBaseVisualControls, uBaseDbClasses, uFormAnimate, uBaseSearch,
  ImgList, uBaseDbInterface, uQuickHelpFrame, uHistoryFrame,uBaseDatasetInterfaces;
type

  { TfmTimeline }

  TfmTimeline = class(TForm)
    acFollow: TAction;
    acRefresh: TAction;
    acSend: TAction;
    acAnswer: TAction;
    acDetailView: TAction;
    acMarkAllasRead: TAction;
    acMarkasRead: TAction;
    acAddUser: TAction;
    acAddImage: TAction;
    acAddScreenshot: TAction;
    acRights: TAction;
    acDelete: TAction;
    acCopyToClipboard: TAction;
    acDeleteENviroment: TAction;
    acSetLink: TAction;
    acStartTimeRegistering: TAction;
    acAddTag: TAction;
    acViewThread: TAction;
    ActionList1: TActionList;
    bSend: TBitBtn;
    cbEnviroment: TComboBox;
    lbResults: TListBox;
    MainMenu1: TMainMenu;
    mEntry: TMemo;
    IdleTimer1: TIdleTimer;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    minewestDown: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miNewMandant: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    pEnviroment: TPanel;
    pTop: TPanel;
    pInput: TPanel;
    pMessages: TPanel;
    PopupMenu1: TPopupMenu;
    pSearch: TPanel;
    sbAddScreenshot1: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    sbAddFile: TSpeedButton;
    sbAddScreenshot: TSpeedButton;
    sbAddUser: TSpeedButton;
    SpeedButton1: TSpeedButton;
    tbRootEntrys: TSpeedButton;
    tbThread: TSpeedButton;
    tbUser: TSpeedButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    tbAdd: TSpeedButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    tsHistory: TTabSheet;
    procedure acAddScreenshotExecute(Sender: TObject);
    procedure acAddTagExecute(Sender: TObject);
    procedure acAddUserExecute(Sender: TObject);
    procedure acAnswerExecute(Sender: TObject);
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDetailViewExecute(Sender: TObject);
    procedure acFollowExecute(Sender: TObject);
    procedure acMarkAllasReadExecute(Sender: TObject);
    procedure acMarkasReadExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acSendExecute(Sender: TObject);
    procedure acSetLinkExecute(Sender: TObject);
    procedure acStartTimeRegisteringExecute(Sender: TObject);
    procedure ActiveSearchEndItemSearch(Sender: TObject);
    procedure ActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string; aActive: Boolean; aLink: string;aPrio : Integer; aItem: TBaseDBList=nil);
    procedure AsyncScrollTop(Data: PtrInt);
    function FContListDrawColumnCell(Sender: TObject; const aRect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function fSearchOpenUser(aLink: string): Boolean;
    procedure fTimelineGetCellText(Sender: TObject; aCol: TColumn;
      aRow: Integer; var NewText: string; aFont: TFont);
    procedure fTimelinegetRowHeight(Sender: TObject; aCol: TColumn;
      aRow: Integer; var aHeight: Integer;var aWidth : Integer);
    procedure fTimelinegListDblClick(Sender: TObject);
    procedure fTimelinegListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure lbResultsDblClick(Sender: TObject);
    procedure mEntryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mEntryKeyPress(Sender: TObject; var Key: char);
    procedure minewestDownClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    function SetLinkfromSearch(aLink: string): Boolean;
    procedure tbThreadClick(Sender: TObject);
    procedure tbUserClick(Sender: TObject);
    procedure tbAddClick(Sender: TObject);
    procedure tbRootEntrysClick(Sender: TObject);
  private
    { private declarations }
    FQuickHelpFrame: TfQuickHelpFrame;
    ActiveSearch : TSearch;
    SysCommands : TSystemCommands;
    FDrawnDate : TDateTime;
    FParentItem : Variant;
    FOldBaseFilterU: String;
    FOldRecU: LargeInt;
    FOldBaseFilterT: String;
    FOldRecT: LargeInt;
    FOldSortT: String;
    FOldSortDT: TSortDirection;
    FoldAutoFilterU : Boolean;
    FOldAutoFilterT : Boolean;
    FOldLimitT: Integer;
    FUserHist : TUser;
    function GetUsersFromString(var tmp : string) : TStringList;
    procedure MarkAsRead;
    procedure AddTag(aTag : string);
  public
    { public declarations }
    fTimeline : TfGridView;
    procedure Execute(aHist : TBaseHistory);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddHelp;
  end;

  { TImagingThread }

  TImagingThread = class(TThread)
  private
    FId : Variant;
    FObj: TMGridObject;
    FRow : Integer;
    procedure AddThumb;
    procedure DoRefresh;
  public
    constructor Create(aId : Variant;aObj : TMGridObject;aRow : Integer);
    procedure Execute; override;
  end;

var
  fmTimeline: TfmTimeline;
const
  DrawImageWidth = 150;
implementation
uses uBaseApplication, uData, uOrder,uMessages,uBaseERPDBClasses,
  uMain,LCLType,utask,uProcessManager,uprometipc,ProcessUtils,ufollow,udetailview,
  LCLIntf,wikitohtml,uDocuments,uthumbnails,uscreenshotmain,uWiki,uSearch,
  LCLProc,uProjects,ubaseconfig,usimpleprocess,LMessages;
resourcestring
  strTo                                  = 'an ';
  strTag                                 = 'Tag';
{$R *.lfm}
{ TImagingThread }

function GetThumbnailBitmap(aDocument: TDocuments;aWidth : Integer=310;aHeight : Integer=428): TBitmap;
var
  aJpg: TJPEGImage;
  aFilename: String;
begin
  Result := TBitmap.Create;
  try
    try
      aFilename := GetThumbNailPath(aDocument,aWidth,aHeight);
      if aFilename='' then exit;
      aJpg := TJPEGImage.Create;
      try
        aJpg.LoadFromFile(aFilename);
      except
        begin
          aJpg.Free;
          exit;
        end;
      end;
      Result.Width:=aJpg.Width;
      Result.Height:=aJpg.Height;
      Result.Canvas.Draw(0,0,aJpg);
    except
      FreeAndNil(Result);
    end;
  finally
    aJpg.Free;
  end;
end;

procedure TImagingThread.AddThumb;
var
  aDocument: TDocument;
  mTime: DWORD;
begin
  mTime:=GetTickCount;
  if not assigned(Data) then exit;
  aDocument := TDocument.Create(nil);
  aDocument.Select(FId,'H',0);
  aDocument.ActualLimit:=1;
  aDocument.Open;
  mTime := GetTickCount;
  TMGridObject(FObj).HasAttachment := aDocument.Count>0;
  if aDocument.Count>0 then
    begin
      TMGridObject(FObj).Image := GetThumbnailBitmap(aDocument);
    end;
  aDocument.Free;
end;

procedure TImagingThread.DoRefresh;
begin
  TRowObject(fmTimeline.fTimeline.gList.Objects[0,FRow]).RefreshHeight:=True;
  fmTimeline.fTimeline.gList.Invalidate;
end;

constructor TImagingThread.Create(aId: Variant; aObj: TMGridObject;
  aRow: Integer);
begin
  FreeOnTerminate:=True;
  Fid := aId;
  FObj := aObj;
  FRow := aRow;
  inherited Create(False);
end;

procedure TImagingThread.Execute;
begin
  Synchronize(@AddThumb);
  Synchronize(@DoRefresh);
end;
procedure TfmTimeline.Execute(aHist: TBaseHistory);
var
  aBoundsRect: TRect;
  aTime: DWORD;
begin
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('Execute:Enter');
  aTime:=GetTickCount;
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfmTimeline,fmTimeline);
      Self := fmTimeline;
      fTimeline.Parent := pMessages;
      fTimeline.Align := alClient;
      fTimeline.DefaultRows:='GLOBALWIDTH:%;TIMESTAMPD:100;ACTIONICON:30;ACTION:250;REFERENCE:50;';
      fTimeline.BaseName:='PTLINE';
      fTimeline.SortDirection:=sdDescending;
      fTimeline.SortField:='TIMESTAMPD';
      fTimeline.TextField:='ACTION';
      fTimeline.IdentField:='ACTION';
      fTimeLine.TreeField:='PARENT';
      fTimeline.FilterRow:=True;
      fTimeline.ReadOnly:=True;
      fTimeline.OnDrawColumnCell:=@FContListDrawColumnCell;
      fTimeline.OnGetCellText:=@fTimelineGetCellText;
      fTimeline.gList.OnKeyDown:=@fTimelinegListKeyDown;
      fTimeline.gList.OnDblClick:=@fTimelinegListDblClick;
      fTimeline.gList.Options:=fTimeline.gList.Options-[goVertLine];
      fTimeline.gHeader.Options:=fTimeline.gHeader.Options-[goVertLine];
      fTimeline.gList.PopupMenu := PopupMenu1;
      fTimeline.OnGetCellText:=@fTimelineGetCellText;
      fTimeline.OngetRowHeight:=@fTimelinegetRowHeight;
      fTimeline.WordWrap:=True;
      fTimeline.DataSet := aHist;
      with Application as IBaseConfig,Application as IBaseApplication do
        begin
          RestoreConfig;
          Config.ReadRect('TIMELINERECT',aBoundsRect,BoundsRect);
        end;
      fTimeline.Show;
      Show;
      BoundsRect := aBoundsRect;
      if Assigned(Data) then
        acDelete.Visible:=Data.Users.Rights.Right('HISTORY')>=RIGHT_DELETE;
      //minewestDownClick(nil);
      Application.QueueAsyncCall(@AsyncScrollTop,0);
    end;
  if fmTimeline.WindowState=wsMinimized then
    fmTimeline.WindowState:=wsNormal;
  Show;
  AddHelp;
  fTimeline.acFilter.Execute;
  IdleTimer1.Enabled:=True;
  fTimeline.SetActive;
  Application.QueueAsyncCall(@AsyncScrollTop,0);
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('Execute:Exit - '+IntToStr(GetTickCount-aTime));
end;
constructor TfmTimeline.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fTimeline := TfGridView.Create(Self);
  fTimeline.ShowHint:=True;
end;
destructor TfmTimeline.Destroy;
begin
  try
    inherited Destroy;
  except
  end;
end;
procedure TfmTimeline.AddHelp;
var
  aWiki: TWikiList;
begin
  if Assigned(FQuickHelpFrame) then exit;
  if not Assigned(Data) then exit;
  aWiki := TWikiList.Create(nil);
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
  if aWiki.FindWikiPage('Promet-ERP-Help/workflows/tftimeline') then
    begin
      FQuickHelpFrame := TfQuickHelpFrame.Create(nil);
      if not FQuickHelpFrame.OpenWikiPage(aWiki) then
        FreeAndNil(FQuickHelpFrame)
      else
        begin
          FQuickHelpFrame.Parent:=Self;
          FQuickHelpFrame.Align:=alTop;
          FQuickHelpFrame.BorderSpacing.Around:=8;
        end;
    end;
  aWiki.Free;
end;

function TfmTimeline.FContListDrawColumnCell(Sender: TObject; const aRect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
var
  aColor: TColor;
  aText: String;
  aHeight: Integer;
  aRRect: TRect;
  aMiddle: Integer;
  aObj: TObject;
  bRect: TRect;
  r: TRect;
  aEn: Boolean;
  cRect: TRect;
  aFactor: Extended;
  aWidth: Integer;
  mTime: DWORD;
begin
  mTime := GetTickCount;
  with (Sender as TCustomGrid), Canvas do
    begin
      Result := True;
      Canvas.Font.Style := [];
      Canvas.FillRect(aRect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      aColor := Column.Color;
      if (not (gdFixed in State)) and (TStringGrid(Sender).AlternateColor<>AColor) then
        begin
          if (TStringGrid(Sender).AltColorStartNormal and Odd(DataCol-TStringGrid(Sender).FixedRows)) {(1)} or
             (not TStringGrid(Sender).AltColorStartNormal and Odd(DataCol)) {(2)} then
            AColor := TStringGrid(Sender).AlternateColor;
        end;
      if (gdSelected in State) then
        begin
          aColor := TStringGrid(Sender).SelectedColor;
          TStringGrid(Sender).Canvas.Font.Color:=clHighlightText;
        end;
      if (Column.FieldName = 'ACTIONICON') then
        begin
          Canvas.Brush.Color:=aColor;
          Canvas.FillRect(aRect);
          Canvas.Pen.Color:=clGray;
          aMiddle :=((aRect.Right-aRect.Left) div 2);
          Canvas.MoveTo(aRect.Left+aMiddle,aRect.Top);
          Canvas.LineTo(aRect.Left+aMiddle,aRect.Bottom);
          aHeight := 14;
          aRRect := Rect(aRect.left+(aMiddle-(aHeight div 2)),
                         aRect.Top+((aRect.Bottom-aRect.Top) div 2)-(aHeight div 2),
                         aRect.left+(aMiddle+(aHeight div 2)),
                         aRect.Top+((aRect.Bottom-aRect.Top) div 2)+(aHeight div 2));
          Canvas.Ellipse(aRRect);
          if not (TExtStringGrid(Sender).Cells[Column.Index+1,DataCol] = '') then
            begin
              aObj := fTimeline.gList.Objects[Column.Index+1,DataCol];
              fVisualControls.HistoryImages.StretchDraw(Canvas,StrToIntDef(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol],-1),aRRect);// Draw(Canvas,Rect.Left,Rect.Top,);
            end;
          if (gdSelected in State) and TStringGrid(Sender).Focused then
            TStringGrid(Sender).Canvas.DrawFocusRect(arect);
        end
      else if (Column.FieldName = 'REFOBJECT') or (Column.FieldName = 'OBJECT') then
        begin
          with Application as IBaseDbInterface do
            aText := Data.GetLinkDesc(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol]);
          with TStringGrid(Sender).Canvas do
            begin
              TStringGrid(Sender).Canvas.Brush.Color:=aColor;
              FillRect(aRect);
            end;
          TStringGrid(Sender).Canvas.Brush.Style:=bsClear;
          aTextStyle.Alignment:=Column.Alignment;
          TextRect(aRect,aRect.Left+3,aRect.Top,aText,aTextStyle);
          Result := True;
        end
      else if (Column.FieldName='ACTION') then
        begin
          aObj := fTimeline.gList.Objects[Column.Index+1,DataCol];
          if Assigned(aObj) then
            begin
              result := True;
              if TMGridObject(aObj).Bold then
                begin
                  Canvas.Brush.Color := clHighlight;
                  bRect := aRRect;
                  bRect.Right := bRect.Left+4;
                  Canvas.Rectangle(bRect);
                end;
              if TMGridObject(aObj).Text='' then
                TMGridObject(aObj).Text := copy(StripWikiText(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol]),0,1000);
              atext := TMGridObject(aObj).Text;
              TStringGrid(Sender).Canvas.Brush.Color:=aColor;
              TStringGrid(Sender).Canvas.FillRect(aRect);
              TStringGrid(Sender).Canvas.Brush.Style:=bsClear;
              bRect := aRect;
              if TMGridObject(aObj).IsThreaded then
                fVisualControls.Images.Draw(TStringGrid(Sender).Canvas,aRect.Left-16,aRect.Top+16,112);
              if TMGridObject(aObj).HasAttachment then
                begin
                  fVisualControls.Images.Draw(TStringGrid(Sender).Canvas,aRect.Left-16,aRect.Top,70);
                  bRect.Right:=bRect.Right-DrawImageWidth;
                  try
                    if Assigned(TMGridObject(aObj).Image) and (TMGridObject(aObj).Image.Height>0) and (TMGridObject(aObj).Image.Width>0) then
                      begin
                        cRect := Rect(bRect.Right,bRect.Top,aRect.Right,0);
                        if TMGridObject(aObj).Image.Height>TMGridObject(aObj).Image.Width then
                          aFactor := TMGridObject(aObj).Image.Height/TMGridObject(aObj).Image.Width
                        else aFactor := TMGridObject(aObj).Image.Width/TMGridObject(aObj).Image.Height;
                        aWidth := DrawImageWidth;
                        if TMGridObject(aObj).Image.Width < DrawImageWidth then
                          aWidth := TMGridObject(aObj).Image.Width;
                        if TMGridObject(aObj).Image.Width>TMGridObject(aObj).Image.Height then
                          cRect.Bottom:=round(cRect.Top+(aWidth/aFactor))
                        else cRect.Bottom:=round(cRect.Top+(aWidth * aFactor));
                        TStringGrid(Sender).Canvas.StretchDraw(cRect,TMGridObject(aObj).Image);
                      end;
                  except
                    TMGridObject(aObj).Image := nil;
                  end;
                end;
              if TMGridObject(aObj).Caption <> '' then
                brect.Top := bRect.Top+TStringGrid(Sender).Canvas.TextExtent('A').cy;
              if TMGridObject(aObj).Bold then
                TStringGrid(Sender).Canvas.Font.Style := [fsBold];
              if fTimeline.WordWrap then
                TStringGrid(Sender).Canvas.TextRect(bRect,aRect.Left+3,bRect.Top,aText,aTextStyleW)
              else
                TStringGrid(Sender).Canvas.TextRect(bRect,aRect.Left+3,bRect.Top,aText,aTextStyle);
              bRect := aRect;
              TStringGrid(Sender).Canvas.Font.Color:=clGray;
              brect.Bottom := aRect.Top+Canvas.TextExtent('A').cy;
              TStringGrid(Sender).Canvas.Font.Style := [];
              TStringGrid(Sender).Canvas.TextOut(arect.Left+3,aRect.Top,TMGridObject(aObj).Caption);
              if (gdSelected in State) and TStringGrid(Sender).Focused then
                TStringGrid(Sender).Canvas.DrawFocusRect(arect);
            end
          else result := False;
        end
      else
        begin
          Result := False;
        end;
    end;
  mTime := GetTickCount-mTime;
  if mTime>0 then
    with Application as IBaseApplication do
      if mTime>100 then
        Debug('DrawColumnCellEnd:'+IntToStr(mTime)+'ms');
end;
procedure TfmTimeline.acRefreshExecute(Sender: TObject);
begin
  FTimeLine.Refresh(True);
end;
procedure TfmTimeline.acFollowExecute(Sender: TObject);
begin
  if fFollow.Execute then
    begin
      tbUser.Down:=False;
      tbThread.Down:=False;
      tbRootEntrys.Down:=False;
      fMain.RefreshFilter2;
      Data.SetFilter(fTimeline.DataSet,fMain.Filter+' '+fMain.Filter2,200);
      acRefresh.Execute;
    end;
end;
procedure TfmTimeline.acMarkAllasReadExecute(Sender: TObject);
begin
  fTimeline.DataSet.First;
  while not fTimeline.DataSet.EOF do
    begin
      MarkAsRead;
      fTimeline.DataSet.Next;
    end;
end;
procedure TfmTimeline.acMarkasReadExecute(Sender: TObject);
begin
  if fTimeline.GotoActiveRow then
    MarkAsRead;
end;
procedure TfmTimeline.acAnswerExecute(Sender: TObject);
var
  tmp: String;
begin
  Application.ProcessMessages;
  if fTimeline.GotoActiveRow then
    begin
      if ((fTimeline.DataSet.FieldByName('LINK').AsString <> '')
      and (fTimeline.DataSet.FieldByName('ACTIONICON').AsString <> '8'))
      and (Sender = nil)
      then
        begin
          if not ProcessExists('prometerp'+ExtractFileExt(Application.ExeName),'') then
            begin
              ExecProcess(AppendPathDelim(ExpandFileName(AppendPathdelim(Application.Location) + '..'))+'prometerp'+ExtractFileExt(Application.ExeName),'',False);
            end;
          SendIPCMessage('OpenLink('+fTimeline.DataSet.FieldByName('LINK').AsString+')');
        end
      else
        begin
          tmp := fTimeline.DataSet.FieldByName('REFERENCE').AsString;
          if trim(tmp)='' then
            tmp := fTimeline.DataSet.FieldByName('CHANGEDBY').AsString;
          tmp := StringReplace(trim(tmp),' ','_',[rfReplaceAll]);
          mEntry.Lines.Text:='@'+tmp+' ';
          mEntry.SelStart:=length(mEntry.Lines.Text);
          if tbAdd.Visible then
            begin
              tbAdd.Down:=True;
              tbAddClick(tbAdd);
            end;
          try
            mEntry.SetFocus;
          except
          end;
        end;
      FParentItem := fTimeline.DataSet.Id.AsVariant;
      MarkAsRead;
    end;
end;

procedure TfmTimeline.acCopyToClipboardExecute(Sender: TObject);
begin
  fTimeline.acCopyToClipboard.Execute;
end;

procedure TfmTimeline.acDeleteExecute(Sender: TObject);
begin
  if fTimeline.GotoActiveRow then
    begin
      fTimeline.Delete;
    end;
end;
procedure TfmTimeline.acAddScreenshotExecute(Sender: TObject);
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aDocPage: TTabSheet;
  aName : string = 'screenshot.jpg';
  aPageIndex: Integer;
  aUsers: TStringList;
  tmp: TCaption;
  i: Integer;
  Found: Boolean = False;
  aId : Variant;
begin
  mEntry.SelText := '[[Bild:'+aName+']]';
  mEntry.SelStart:=mEntry.SelStart+length(mEntry.SelText);
  tmp := mEntry.Lines.Text;
  aUsers := GetUsersFromString(tmp);
  FUserHist := TUser.Create(nil);
  for i := 0 to aUsers.Count-1 do
    begin
      Data.SetFilter(FUserHist,Data.QuoteField('IDCODE')+'='+Data.QuoteValue(aUsers[i]));
      if FUserHist.Count=0 then
        begin
          Data.SetFilter(FUserHist,'',0);
          if not FUserHist.DataSet.Locate('IDCODE',aUsers[i],[loCaseInsensitive]) then
            FUserHist.DataSet.Close;
        end;
      if FUserHist.Count>0 then
        begin
          Found := True;
          FUserHist.History.AddParentedItem(FUserHist.DataSet,tmp,FParentItem,'',Data.Users.IDCode.AsString,nil,ACICON_USEREDITED,'',True,True);
        end;
    end;
  if not Found then
    begin
      FUserHist.Select(Data.Users.Id.AsVariant);
      FUserHist.Open;
      FUserHist.History.AddParentedItem(Data.Users.DataSet,tmp,FParentItem,'',Data.Users.IDCode.AsString,nil,ACICON_USEREDITED,'',True,True);
    end;
  if FUserHist.History.CanEdit then
    FUserHist.History.Post;
  aUsers.Free;
  aId := FUserHist.History.Id.AsVariant;
  Application.ProcessMessages;
  Self.Hide;
  Application.ProcessMessages;
  sleep(1000); //wait for hide animations ...
  Application.ProcessMessages;
  //aName := InputBox(strScreenshotName, strEnterAnName, aName);
  Application.ProcessMessages;
  acSend.Enabled:=False;
  Application.CreateForm(TfScreenshot,fScreenshot);
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    fScreenshot.SaveTo:=AppendPathDelim(GetInternalTempDir)+aName;
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(aId,'H',0);
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    aDocument.AddFromFile(AppendPathDelim(GetInternalTempDir)+aName);
  aDocument.Free;
  Self.Show;
  acSend.Enabled:=True;
end;

procedure TfmTimeline.acAddTagExecute(Sender: TObject);
var
  aValue: String;
  aTag: String;
begin
  if fTimeline.GotoActiveRow then
    if InputQuery(strTag,strName,aValue) then
      begin
        aValue := aValue+',';
        while pos(',',aValue)>0 do
          begin
            aTag := copy(aValue,0,pos(',',aValue)-1);
            aValue := copy(aValue,pos(',',aValue)+1,length(aValue));
            if copy(trim(aTag),0,1)<>'#' then
              aTag := '#'+trim(aTag);
            if aTag <> '#' then
              AddTag(aTag);
          end;
      end;
end;

procedure TfmTimeline.acAddUserExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.SetLanguage;
  fSearch.AllowSearchTypes(strUsers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenUser;
  fSearch.Execute(True,'MESSA','');
  fSearch.SetLanguage;
end;
procedure TfmTimeline.acDetailViewExecute(Sender: TObject);
begin
  if fTimeline.GotoActiveRow then
    fDetailView.Execute(TBaseHistory(fTimeline.DataSet));
end;
procedure TfmTimeline.acSendExecute(Sender: TObject);
var
  tmp: String;
  aUser: TUser;
  aUsers : TStringList;
  i: Integer;
  Found: Boolean;
  AddTask: Boolean = False;
  aTask: TTask;
  aTag: String;
  aSource: String;
  aObject: String;
  bHistory: TBaseHistory;
begin
  tmp := trim(mEntry.Lines.Text);
  if lowercase(copy(tmp,0,4)) = 'task' then
    begin
      tmp := trim(copy(tmp,5,length(tmp)));
      AddTask := True;
    end
  else if lowercase(copy(tmp,0,7)) = 'aufgabe' then
    begin
      tmp := trim(copy(tmp,8,length(tmp)));
      AddTask := True;
    end;
  if copy(tmp,0,1) = '@' then
    begin
      Found := False;
      tmp := copy(tmp,2,length(tmp));
      aUsers := GetUsersFromString(tmp);
      aUser := TUser.Create(nil);
      for i := 0 to aUsers.Count-1 do
        begin
          Data.SetFilter(aUser,Data.QuoteField('IDCODE')+'='+Data.QuoteValue(aUsers[i]));
          if aUser.Count=0 then
            begin
              Data.SetFilter(aUser,'',0);
              if not aUser.DataSet.Locate('IDCODE',aUsers[i],[loCaseInsensitive]) then
                aUser.DataSet.Close;
            end;
          if aUser.Count>0 then
            begin
              Found := True;
              if not AddTask then
                begin
                  if Assigned(FUserHist) then
                    begin
                      FUserHist.History.Edit;
                      FUserHist.History.FieldByName('ACTION').AsString:=tmp;
                      FUserHist.History.Post;
                      FreeAndNil(FUserHist);
                    end
                  else
                    aUser.History.AddParentedItem(aUser.DataSet,tmp,FParentItem,'',Data.Users.IDCode.AsString,nil,ACICON_USEREDITED,'',True,True)
                end
              else
                begin
                  aTask := TTask.Create(nil);
                  aTask.Insert;
                  aTask.FieldByName('SUMMARY').AsString:=tmp;
                  aTask.FieldByName('USER').AsString:=aUser.FieldByName('ACCOUNTNO').AsString;
                  aTask.DataSet.Post;
                  Data.Users.History.AddItem(aTask.DataSet,aTask.FieldByName('SUMMARY').AsString,Data.BuildLink(aTask.DataSet),'',nil,ACICON_TASKADDED,'',False);;
                  aTask.Free;
                end;
            end;
        end;
      aUser.Free;
      if (not Found) and (FParentItem<>Null) then
        begin
          bHistory := TBaseHistory.Create(nil);
          bHistory.Select(FParentItem);
          bHistory.Open;
          if bHistory.Count>0 then
            begin
              aSource := bHistory.FieldByName('SOURCE').AsString;
              aObject := bHistory.FieldByName('OBJECT').AsString;
              Data.Users.History.AddParentedItemPlain(aObject,tmp,FParentItem,'',Data.Users.IDCode.AsString,'',ACICON_USEREDITED,'',True,False,True);
              Data.Users.History.FieldByName('SOURCE').AsString:=aSource;
              Data.Users.History.Post;
              Found:=True;
            end;
          bHistory.Free;
        end;
    end
  else if (not AddTask) then
    begin
      if Assigned(FUserHist) then
        begin
          FUserHist.History.Edit;
          FUserHist.History.FieldByName('ACTION').AsString:=tmp;
          FUserHist.History.Post;
          FreeAndNil(FUserHist);
        end
      else
        Data.Users.History.AddParentedItem(Data.Users.DataSet,mEntry.Lines.Text,FParentItem,'','',nil,ACICON_USEREDITED,'',True,True);
      Found := True;
    end
  else
    begin
      aTask := TTask.Create(nil);
      aTask.Insert;
      aTask.FieldByName('SUMMARY').AsString:=tmp;
      aTag := '';
      if pos('#',tmp)>0 then
        begin
          aTag := copy(tmp,pos('#',tmp)+1,length(tmp));
          aTag := copy(aTag,0,pos(' ',aTag)-1);
          aTask.FieldByName('CATEGORY').AsString:=aTag;
        end;
      aTask.FieldByName('USER').AsString:=Data.Users.FieldByName('ACCOUNTNO').AsString;
      aTask.DataSet.Post;
      Data.Users.History.AddParentedItem(aTask.DataSet,aTask.FieldByName('SUMMARY').AsString,FParentItem,Data.BuildLink(aTask.DataSet),'',nil,ACICON_TASKADDED,'',False);;
      aTask.Free;
    end;
  fTimeline.Refresh;
  if Found then
    begin
      mEntry.Lines.Clear;
      if tbAdd.Visible then
        begin
          tbAdd.Down:=False;
          tbAddClick(tbAdd);
        end;
      fTimeline.SetActive;
    end;
  FParentItem := Null;
end;

procedure TfmTimeline.acSetLinkExecute(Sender: TObject);
begin
  if fTimeline.GotoActiveRow then
    begin
      fSearch.SetLanguage;
      fSearch.OnOpenItem:=@SetLinkfromSearch;
      fSearch.Execute(True,'MESSLINK',strSearchfromHistoryMode);
    end;
end;

procedure TfmTimeline.acStartTimeRegisteringExecute(Sender: TObject);
var
  tmp: String;
  aTask: TTask;
  aProject: TProject;
  aLink: String;
begin
  if fTimeline.GotoActiveRow then
    if FileExists(UniToSys(GetTempDir+'PMSTimeregistering')) then
      begin
        aTask := TTask.Create(nil);
        aTask.SelectFromLink(fTimeline.DataSet.FieldByName('LINK').AsString);
        aTask.Open;
        aProject := TProject.Create(nil);
        aProject.Select(aTask.FieldByName('PROJECTID').AsString);
        aProject.Open;
        if aProject.Count>0 then
          aLink := Data.BuildLink(aProject.DataSet);
        aProject.Free;
        aTask.Free;
        tmp := 'Time.enter('+aLink+';'+fTimeline.DataSet.FieldByName('LINK').AsString+';)';
        SendIPCMessage(tmp,GetTempDir+'PMSTimeregistering');
        SendIPCMessage('Time.start',GetTempDir+'PMSTimeregistering');
      end;
end;

procedure TfmTimeline.ActiveSearchEndItemSearch(Sender: TObject);
begin
  if not ActiveSearch.Active then
    begin
      if ActiveSearch.Count=0 then
        pSearch.Visible:=False;
    end;
end;
procedure TfmTimeline.ActiveSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPrio: Integer;
  aItem: TBaseDBList);
begin
  with pSearch do
    begin
      if not Visible then
        Visible := True;
    end;
  if aActive then
    lbResults.Items.AddObject(aName,TLinkObject.Create(aLink));
end;

procedure TfmTimeline.AsyncScrollTop(Data: PtrInt);
var
  aMsg : TLMVScroll;
  aTime: DWORD;
begin
  aTime := GetTickCount;
  if fTimeline.InvertedDrawing then
    begin
      fTimeline.gList.TopRow:=fTimeline.gList.RowCount-fTimeline.gList.VisibleRowCount;
      aMsg.Msg:=LM_VSCROLL;
      aMsg.ScrollCode:=SB_BOTTOM;
      fTimeline.gList.Dispatch(aMsg);
    end
  else
    fTimeline.gList.TopRow:=0;
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('AsyncScrollTop:'+IntToStr(GetTickCount-aTime));
end;

procedure TfmTimeline.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IdleTimer1.Enabled:=False;
  with Application as IBaseConfig do
    Config.WriteRect('TIMELINERECT',BoundsRect);
  CloseAction:=caHide;
end;
procedure TfmTimeline.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      if (pInput.Visible) and (pInput.Height>5) then exit;
      pSearch.Visible:=False;
      Close;
    end;
end;
function TfmTimeline.fSearchOpenUser(aLink: string): Boolean;
var
  tmp: String;
  aUser: TUser;
begin
  tmp := copy(mEntry.Text,0,mEntry.SelStart);
  if pos(' ',tmp)>0 then
    begin
      mEntry.Text:=copy(mEntry.Text,pos(' ',mEntry.Text)+1,length(mEntry.Text));
      tmp := copy(tmp,0,pos(' ',tmp)-1);
    end
  else mEntry.Text:='';
  if copy(tmp,0,1)<>'@' then
    begin
      mEntry.Text:=tmp+' '+mEntry.Text;
      tmp := '@';
    end;
  aUser := TUser.Create(nil);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  if aUser.Count>0 then
    tmp := tmp+aUser.IDCode.AsString;
  auser.Free;
  mEntry.Text:=tmp+' '+mEntry.Text;
  mEntry.SelStart:=length(mEntry.Text);
end;
procedure TfmTimeline.fTimelineGetCellText(Sender: TObject; aCol: TColumn;
  aRow: Integer; var NewText: string; aFont: TFont);
var
  aTime: TDateTime;
  fhasObject: Boolean;
  i: Integer;
  aObj: TObject;
  arec: LargeInt;
  aDocument: TDocument;
  mTime: types.DWORD;
begin
  if aCol.FieldName='LINK' then
    NewText := Data.GetLinkDesc(NewText)
  else if aCol.FieldName='OBJECT' then
    NewText := Data.GetLinkDesc(NewText)
  else if (aCol.FieldName='ACTION') then
    begin
      fHasObject := False;
      for i := 0 to fTimeline.dgFake.Columns.Count-1 do
        if fTimeline.dgFake.Columns[i].FieldName='OBJECT' then
          FHasObject := True;
      NewText := StripWikiText(NewText);
      if pos('data:im',NewText)>0 then NewText := copy(NewText,0,pos('data:im',NewText)-1);
      if (not fHasObject) and (aRow>=fTimeLine.gList.FixedRows) then
        begin
          aObj := fTimeline.gList.Objects[aCol.Index+1,aRow];
          if not Assigned(aObj) then
            begin
              fTimeline.gList.Objects[aCol.Index+1,aRow] := TMGridObject.Create;
              arec := fTimeline.DataSet.GetBookmark;
              aObj := fTimeline.gList.Objects[aCol.Index+1,aRow];
              if fTimeline.GotoRowNumber(aRow) then
                begin
                  if fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString <> Data.BuildLink(Data.Users.dataSet) then
                    begin
                      if copy(fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString,0,6) = 'USERS@' then
                        TMGridObject(aObj).Caption := strTo+Data.GetLinkDesc(fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString)
                      else
                        TMGridObject(aObj).Caption := Data.GetLinkDesc(fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString);
                      fTimeline.gList.RowHeights[aRow] := fTimeline.gList.RowHeights[aRow]+12;
                    end;
                  TMGridObject(aObj).Bold:=(fTimeline.dgFake.DataSource.DataSet.FieldByName('READ').AsString<>'Y');
                  if (fTimeline.dgFake.DataSource.DataSet.RecordCount>0) and (pos('nsfw',lowercase(NewText))=0) then
                    begin
                      TMGridObject(aObj).IsThreaded:=not fTimeline.dgFake.DataSource.DataSet.FieldByName('PARENT').IsNull;
                      if (pos('[[image',lowercase(NewText))>0)
                      or (pos('[[bild',lowercase(NewText))>0)
                      or (pos('[[file',lowercase(NewText))>0)
                      or (pos('[[datei',lowercase(NewText))>0)
                      then
                        TImagingThread.Create(fTimeline.dgFake.DataSource.DataSet.FieldByName('SQL_ID').AsVariant,TMGridObject(aObj),aRow);
                    end;
                end;
              fTimeline.DataSet.GotoBookmark(aRec);
            end;
          NewText := TMGridObject(aObj).Caption+lineending+NewText;
          if length(NewText)>1000 then
            NewText:=copy(NewText,0,1000)+LineEnding+'...';
          if (pos('nsfw',lowercase(NewText))>0) then
            NewText := 'NSFW';
        end;
    end
  else if aCol.FieldName='TIMESTAMPD' then
    begin
      aObj := fTimeline.gList.Objects[aCol.Index+1,aRow];
      if Assigned(aObj) then
        begin
          if TMGridObject(aObj).Bold then
            TStringGrid(Sender).Canvas.Font.Style:=[fsBold]
          else
            TStringGrid(Sender).Canvas.Font.Style:=[];
        end;
      if TryStrToDateTime(NewText,aTime) then
        begin
          if (trunc(aTime) = FDrawnDate) or (trunc(aTime) = trunc(Now())) then
            NewText:=TimeToStr(frac(aTime))
          else
            FDrawnDate:=trunc(aTime);
          aCol.Alignment:=taRightJustify;
        end;
    end;
end;
procedure TfmTimeline.fTimelinegetRowHeight(Sender: TObject; aCol: TColumn;
  aRow: Integer; var aHeight: Integer; var aWidth: Integer);
var
  aObj: TObject;
  aFactor: Extended;
begin
  aObj := fTimeline.gList.Objects[aCol.Index+1,aRow];
  if Assigned(aObj) then
    begin
      if TMGridObject(aObj).HasAttachment
      and Assigned(TMGridObject(aObj).Image)
      and (TMGridObject(aObj).Image.Width>0) then
        begin
          if TMGridObject(aObj).Image.Height>TMGridObject(aObj).Image.Width then
            aFactor := TMGridObject(aObj).Image.Height/TMGridObject(aObj).Image.Width
          else aFactor := TMGridObject(aObj).Image.Width/TMGridObject(aObj).Image.Height;
          aWidth := aWidth-DrawImageWidth;
          aHeight:=round((DrawImageWidth/aFactor));
        end;
    end;
end;
procedure TfmTimeline.fTimelinegListDblClick(Sender: TObject);
begin
  acAnswerExecute(nil);
end;
procedure TfmTimeline.fTimelinegListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  if Key = VK_INSERT then
    begin
      tbAdd.Down:=True;
      tbAddClick(tbAdd);
    end
  else fTimeline.gListKeyDown(Sender,Key,Shift);
  if Key = VK_G then
    begin
      if fTimeline.GotoActiveRow then
        begin
          MarkAsRead;
          fTimeline.gList.Row:=fTimeline.gList.Row+1;
        end;
    end;
end;
procedure TfmTimeline.IdleTimer1Timer(Sender: TObject);
begin
  //debugln('Refresh Timeline idle');
  if mEntry.Focused then exit;
  FTimeLine.Refresh(True);
end;
procedure TfmTimeline.lbResultsDblClick(Sender: TObject);
var
  aUser: TUser;
  aText: TCaption;
begin
  if lbResults.ItemIndex = -1 then exit;
  aUser := TUser.Create(nil);
  aUser.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
  aUser.Open;
  aText := mEntry.Text;
  if pos(',',atext)>0 then
    aText := copy(aText,0,rpos(',',aText))
  else if pos('@',atext)>0 then
    aText := copy(aText,0,pos('@',aText));
  atext := atext+aUser.IDCode.AsString;
  mEntry.Text:=aText;
  mEntry.SelStart:=length(atext);
  pSearch.Visible:=False;
  aUser.Free;
end;
procedure TfmTimeline.mEntryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if pSearch.Visible then
    begin
      case Key of
      VK_PRIOR,
      VK_UP:
        begin
          if lbResults.ItemIndex = -1 then
            begin
              lbResults.ItemIndex:=0;
              pSearch.Visible := False;
            end
          else
            begin
              lbResults.ItemIndex:=lbResults.ItemIndex-1;
              Key := 0;
            end;
        end;
      VK_NEXT,
      VK_DOWN:
        begin
          if lbResults.ItemIndex = -1 then
            lbResults.ItemIndex:=0
          else
          if lbResults.ItemIndex < lbResults.Count-1 then
            lbResults.ItemIndex:=lbResults.ItemIndex+1;
          Key := 0;
        end;
      VK_RETURN:
        begin
          lbResultsDblClick(nil);
          Key := 0;
        end;
      VK_ESCAPE,VK_SPACE:
        begin
          pSearch.Visible:=False;
          if Key = VK_ESCAPE then
            Key := 0;
        end;
      end;
    end
  else
    begin
      if (ssCtrl in Shift)
      and (Key = VK_RETURN) then
        begin
          mEntry.Text:=mEntry.Text+LineEnding;
        end
      else if Key = VK_ESCAPE then
        begin
          if tbAdd.Visible then
            begin
              tbAdd.Down:=false;
              tbAddClick(tbAdd);
            end;
          mEntry.Text:='';
          Key := 0;
          if fTimeline.Visible then
            fTimeline.SetActive;
        end;
    end;
end;
procedure TfmTimeline.mEntryKeyPress(Sender: TObject; var Key: char);
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
  aText: TCaption;
  tmp: String;
begin
  tmp := copy(mEntry.Text,0,mEntry.SelStart)+Key;
  if (
    (copy(tmp,1,1) = '@') and (pos(' ',tmp)=0)
  ) and (pSearch.Visible=False)
  then
    begin
      pSearch.Visible:=True;
    end
  else if pSearch.Visible then
    begin
      aText := mEntry.text;
      if pos(',',atext)>0 then
        aText := copy(aText,rpos(',',aText)+1,length(aText))
      else if pos('@',atext)>0 then
        aText := copy(aText,pos('@',aText)+1,length(atext));
      if trim(atext)='' then exit;
      if Assigned(ActiveSearch) then
        ActiveSearch.Abort;
      SearchTypes := SearchTypes+[fsShortnames];
      SearchTypes := SearchTypes+[fsIdents];
      SetLength(SearchLocations,length(SearchLocations)+1);
      SearchLocations[length(SearchLocations)-1] := strUsers;
      for i := 0 to lbResults.Items.Count-1 do
        lbResults.Items.Objects[i].Free;
      lbResults.Items.Clear;
      if not Assigned(ActiveSearch) then
        ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
      ActiveSearch.Sender := TComponent(Sender);
      ActiveSearch.OnItemFound:=@ActiveSearchItemFound;
      ActiveSearch.OnEndSearch:=@ActiveSearchEndItemSearch;
      ActiveSearch.Start(aText+Key);
      Application.ProcessMessages;
    end;
end;

procedure TfmTimeline.minewestDownClick(Sender: TObject);
begin
  if minewestDown.Checked then
    pInput.Align:=alBottom
  else pInput.Align:=alTop;
  tbAdd.Visible:=not minewestDown.Checked;
  tbAdd.Down := False;
  pInput.Visible:=minewestDown.Checked;
  pInput.Height:=110;
  fTimeline.InvertedDrawing := minewestDown.Checked;
  //fTimeline.Refresh;
  if Assigned(Data) then
    begin
      fTimeline.DataSet.First;
      fTimeline.GotoDataSetRow;
    end;
end;

procedure TfmTimeline.PopupMenu1Popup(Sender: TObject);
begin
  acStartTimeRegistering.Visible := False;
  if fTimeline.GotoActiveRow then
    acStartTimeRegistering.Visible := (Data.GetLinkIcon(fTimeline.DataSet.FieldByName('LINK').AsString) = IMAGE_TASK) and FileExists(UniToSys(GetTempDir+'PMSTimeregistering'));
end;

function TfmTimeline.SetLinkfromSearch(aLink: string): Boolean;
begin
  if fTimeline.GotoActiveRow then
    begin
      fTimeline.DataSet.Edit;
      fTimeline.DataSet.FieldByName('LINK').AsString:=aLink;
      fTimeline.DataSet.Post;
    end;
end;

procedure TfmTimeline.tbThreadClick(Sender: TObject);
var
  FRoot : Variant;
begin
  Screen.Cursor:=crHourGlass;
  tbRootEntrys.Down:=False;
  fTimeline.ApplyAutoFilter:=True;
  if tbThread.Down then
    begin
      fTimeline.GotoActiveRow;
      FOldBaseFilterT := fTimeline.BaseFilter;
      FOldRecT := fTimeline.DataSet.GetBookmark;
      FOldSortT := fTimeline.SortField;
      FOldSortDT := fTimeline.SortDirection;
      FOldAutoFilterT := fTimeline.ApplyAutoFilter;
      FOldLimitT := fTimeline.DataSet.ActualLimit;
      FRoot := fTimeline.DataSet.FieldByName('ROOT').AsVariant;
      if FRoot = Null then
        FRoot := fTimeline.DataSet.FieldByName('PARENT').AsVariant;
      if FRoot = Null then
        FRoot := fTimeline.DataSet.FieldByName('SQL_ID').AsVariant;
      fTimeline.ApplyAutoFilter:=False;
      fTimeline.SortField:='TIMESTAMPD';
      fTimeline.SortDirection:=sdAscending;
      fTimeline.DataSet.ActualLimit:=0;
      fTimeline.DataSet.ActualFilter:='';
      fTimeline.BaseFilter:='('+Data.QuoteField('ROOT')+'='+Data.QuoteValue(FRoot)+') OR ('+Data.QuoteField('PARENT')+'='+Data.QuoteValue(FRoot)+') OR ('+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(FRoot)+')';
    end
  else
    begin
      fTimeline.DataSet.ActualFilter:=trim(fMain.Filter+' '+fMain.Filter2);
      fTimeline.ApplyAutoFilter:=FOldAutoFilterT;
      fTimeline.SortField := FOldSortT;
      fTimeline.SortDirection := FOldSortDT;
      fTimeline.DataSet.ActualLimit:=FOldLimitT;
      if FOldBaseFilterT<>'' then
        begin
          fTimeline.BaseFilter:=FOldBaseFilterT;
          FOldBaseFilterT:='';
        end
      else
        fTimeline.BaseFilter:='';
      if FOldRecT<>0 then
        begin
          fTimeline.DataSet.GotoBookmark(FOldRecT);
          fTimeline.GotoDataSetRow;
          FOldRecT:=0;
        end;
      fTimeline.GotoDataSetRow;
    end;
  acMarkAllasRead.Visible:=tbThread.Down or tbUser.Down;
  Screen.Cursor:=crDefault;
end;
procedure TfmTimeline.tbUserClick(Sender: TObject);
var
  FUser: String;
begin
  Screen.Cursor:=crHourGlass;
  if tbRootEntrys.Down then
    tbRootEntrys.Down:=False;
  if tbUser.Down then
    begin
      fTimeline.GotoActiveRow;
      FOldBaseFilterU := fTimeline.BaseFilter;
      FOldRecU := fTimeline.DataSet.GetBookmark;
      FUser := fTimeline.DataSet.FieldByName('REFERENCE').AsString;
      FOldAutoFilterU := fTimeline.ApplyAutoFilter;
      fTimeline.ApplyAutoFilter:=False;
      fTimeline.BaseFilter:='('+Data.QuoteField('REFERENCE')+'='+Data.QuoteValue(FUser)+')';
    end
  else
    begin
      fTimeline.DataSet.ActualLimit:=100;
      fTimeline.ApplyAutoFilter:=FoldAutoFilterU;
      if FOldBaseFilterU<>'' then
        begin
          fTimeline.BaseFilter:=FOldBaseFilterU;
          FOldBaseFilterU:='';
        end
      else
        fTimeline.BaseFilter:='';
      if FOldRecU<>0 then
        begin
          fTimeline.DataSet.GotoBookmark(FOldRecU);
          fTimeline.GotoDataSetRow;
          FOldRecU:=0;
        end;
      fTimeline.GotoDataSetRow;
    end;
  acMarkAllasRead.Visible:=tbThread.Down or tbUser.Down;
  Screen.Cursor:=crDefault;
end;
procedure TfmTimeline.tbAddClick(Sender: TObject);
var
  aController: TAnimationController;
begin
  FParentItem:=Null;
  FreeAndNil(FUserHist);
  aController := TAnimationController.Create(pInput);
  if tbAdd.Down then
    begin
      pInput.Height := 0;
      pInput.Visible:=True;
      aController.AnimateControlHeight(110);
      mEntry.SetFocus;
    end
  else
    begin
      aController.AnimateControlHeight(0);
      pSearch.Visible:=False;
      if Assigned(FUserHist) then
        begin
          FUserHist.History.Delete;
          FreeAndNil(FUserHist);
        end;
      mEntry.Lines.Clear;
    end;
  aController.Free;
end;
procedure TfmTimeline.tbRootEntrysClick(Sender: TObject);
var
  FRoot : Variant;
begin
  Screen.Cursor:=crHourGlass;
  fTimeline.ApplyAutoFilter:=True;
  tbThread.Down:=False;
  tbUser.Down:=False;
  if tbRootEntrys.Down then
    begin
      fTimeline.GotoActiveRow;
      FRoot := fTimeline.DataSet.FieldByName('ROOT').AsVariant;
      if FRoot = Null then
        FRoot := fTimeline.DataSet.FieldByName('PARENT').AsVariant;
      fTimeline.DataSet.ActualLimit:=100;
      fTimeline.BaseFilter:=Data.ProcessTerm(Data.QuoteField('PARENT')+'='+Data.QuoteValue(''));
    end
  else
    fTimeline.BaseFilter:='';
  Screen.Cursor:=crDefault;
end;
function TfmTimeline.GetUsersFromString(var tmp: string): TStringList;
begin
  Result := TStringList.Create;
  while (pos(',',tmp) > 0) and (pos(',',tmp) < pos(' ',tmp)) do
    begin
      Result.Add(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
    end;
  if pos(' ',tmp)>1 then
    result.Add(copy(tmp,0,pos(' ',tmp)-1));
  tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
end;
procedure TfmTimeline.MarkAsRead;
var
  i: Integer;
begin
  if (not ((fTimeline.DataSet.FieldByName('CHANGEDBY').AsString=Data.Users.FieldByName('IDCODE').AsString) and (fTimeline.DataSet.FieldByName('ACTIONICON').AsInteger=8))) then
    begin
      with fTimeline.DataSet.DataSet as IBaseManageDB do
        UpdateStdFields:=False;
      if not fTimeline.DataSet.CanEdit then
        fTimeline.DataSet.DataSet.Edit;
      fTimeline.DataSet.FieldByName('READ').AsString:='Y';
      fTimeline.DataSet.post;
      with fTimeline.DataSet.DataSet as IBaseManageDB do
        UpdateStdFields:=True;
      for i := 0 to fTimeline.dgFake.Columns.Count-1 do
        if fTimeline.dgFake.Columns[i].FieldName='ACTION' then
          begin
            if Assigned(fTimeline.gList.Objects[i+1,fTimeline.gList.Row]) then
              begin
                TMGridObject(fTimeline.gList.Objects[i+1,fTimeline.gList.Row]).Bold:=False;
                if Assigned(fTimeline.gList.Objects[i+1,0]) then
                  TRowObject(fTimeline.gList.Objects[i+1,0]).RefreshHeight:=True;
              end;
          end;
      fTimeline.gList.Invalidate;
    end;
end;

procedure TfmTimeline.AddTag(aTag: string);
begin
  with fTimeline.DataSet.DataSet as IBaseManageDB do
    UpdateStdFields:=False;
  if not fTimeline.DataSet.CanEdit then
    fTimeline.DataSet.DataSet.Edit;
  fTimeline.DataSet.FieldByName('ACTION').AsString:=fTimeline.DataSet.FieldByName('ACTION').AsString+' '+aTag;
  fTimeline.DataSet.post;
  with fTimeline.DataSet.DataSet as IBaseManageDB do
    UpdateStdFields:=True;
  fTimeline.gList.Invalidate;
end;

initialization
  AddSearchAbleDataSet(TUser);

end.
