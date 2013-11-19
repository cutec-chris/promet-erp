unit umtimeline;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, ZVDateTimePicker,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage,ugridview,uHistoryFrame,
  uExtControls,uBaseVisualControls,uBaseDbClasses,uFormAnimate,uBaseSearch;
type
  TMGridObject = class(TObject)
  public
    Caption : string;
  end;

  TfmTimeline = class(TForm)
    acFollow: TAction;
    acRefresh: TAction;
    acSend: TAction;
    ActionList1: TActionList;
    bSend: TBitBtn;
    lbResults: TListBox;
    mEntry: TMemo;
    IdleTimer1: TIdleTimer;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miNewMandant: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    pInput: TPanel;
    Panel2: TPanel;
    pSearch: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TSpeedButton;
    ToolButton3: TToolButton;
    tsHistory: TTabSheet;
    procedure acFollowExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acSendExecute(Sender: TObject);
    procedure ActiveSearchEndItemSearch(Sender: TObject);
    procedure ActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string; aActive: Boolean; aLink: string; aItem: TBaseDBList=nil);
    procedure bSendClick(Sender: TObject);
    function FContListDrawColumnCell(Sender: TObject; const aRect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure fTimelineGetCellText(Sender: TObject; aCol: TColumn;
      aRow: Integer; var NewText: string; aFont: TFont);
    procedure fTimelinegListDblClick(Sender: TObject);
    procedure fTimelinegListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure lbResultsDblClick(Sender: TObject);
    procedure mEntryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mEntryKeyPress(Sender: TObject; var Key: char);
    procedure pSearchClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
    ActiveSearch : TSearch;
    SysCommands : TSystemCommands;
    fTimeline : TfGridView;
    FDrawnDate : TDateTime;
    aHistoryFrame: TfHistoryFrame;
    FParentItem : Variant;
  public
    { public declarations }
    procedure Execute;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowFrame;
  end;
var
  fmTimeline: TfmTimeline;
implementation
uses uBaseApplication, uData, uBaseDbInterface, uOrder,uMessages,uBaseERPDBClasses,
  uMain,LCLType,utask,uProcessManager,uprometipc,ProcessUtils,ufollow;
resourcestring
  strTo                                  = 'an ';
procedure TfmTimeline.Execute;
var
  aBoundsRect: TRect;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfmTimeline,fmTimeline);
      Self := fmTimeline;
      fTimeline.Parent := Panel2;
      fTimeline.Align := alClient;
      fTimeline.DefaultRows:='GLOBALWIDTH:430;TIMESTAMPD:100;ACTIONICON:30;ACTION:250;REFERENCE:50;';
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
      fTimeline.DataSet := TBaseHistory.Create(Self,Data);
      fTimeline.gList.OnKeyDown:=@fTimelinegListKeyDown;
      fTimeline.gList.OnDblClick:=@fTimelinegListDblClick;
      fTimeline.gList.Options:=fTimeline.gList.Options-[goVertLine];
      fTimeline.gHeader.Options:=fTimeline.gHeader.Options-[goVertLine];
      Data.SetFilter(fTimeline.DataSet,fMain.Filter+' '+fMain.Filter2,300);
      with Application as IBaseApplication do
        Config.ReadRect('TIMELINERECT',aBoundsRect,BoundsRect);
      fTimeline.Show;
      Show;
      BoundsRect := aBoundsRect;
    end;
  if fmTimeline.WindowState=wsMinimized then
    fmTimeline.WindowState:=wsNormal;
  Show;
  fTimeline.acFilter.Execute;
  //IdleTimer1.Enabled:=True;
  fTimeline.SetActive;
  fTimeline.gList.TopRow:=0;
end;

constructor TfmTimeline.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fTimeline := TfGridView.Create(Self);
  aHistoryFrame := TfHistoryFrame.Create(Self);
  fTimeline.ShowHint:=True;
end;

destructor TfmTimeline.Destroy;
begin
  inherited Destroy;
end;

procedure TfmTimeline.ShowFrame;
begin
  FTimeLine.Refresh(True);
end;

function TfmTimeline.FContListDrawColumnCell(Sender: TObject; const aRect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
var
  aColor: TColor;
  aText: String;
  aTextStyle : TTextStyle = (Alignment:taLeftJustify;
                             Layout : tlTop;
                             SingleLine : False;
                             Clipping  : True;
                             ExpandTabs:False;
                             ShowPrefix:False;
                             Wordbreak:false;
                             Opaque:True;
                             SystemFont:False;
                             RightToLeft:False);
  aHeight: Integer;
  aRRect: TRect;
  aMiddle: Integer;
  aObj: TObject;
  bRect: TRect;
begin
  with (Sender as TCustomGrid), Canvas do
    begin
      Result := True;
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
            aHistoryFrame.HistoryImages.StretchDraw(Canvas,StrToIntDef(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol],-1),aRRect);// Draw(Canvas,Rect.Left,Rect.Top,);
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
              atext := TExtStringGrid(Sender).Cells[Column.Index+1,DataCol];
              TStringGrid(Sender).Canvas.Brush.Color:=aColor;
              TStringGrid(Sender).Canvas.FillRect(aRect);
              TStringGrid(Sender).Canvas.Brush.Style:=bsClear;
              bRect := aRect;
              brect.Top := bRect.Top+TStringGrid(Sender).Canvas.TextExtent('A').cy;
              TStringGrid(Sender).Canvas.TextRect(bRect,aRect.Left+3,bRect.Top,aText,aTextStyle);
              TStringGrid(Sender).Canvas.Font.Color:=clGray;
              bRect := aRect;
              brect.Bottom := aRect.Top+Canvas.TextExtent('A').cy;
              TStringGrid(Sender).canvas.TextOut(arect.Left+3,aRect.Top,TMGridObject(aObj).Caption);
              if (gdSelected in State) and TStringGrid(Sender).Focused then
                TStringGrid(Sender).Canvas.DrawFocusRect(arect);
            end
          else result := False;
        end
      else
        Result := False;
    end;
end;

procedure TfmTimeline.acRefreshExecute(Sender: TObject);
begin
  FTimeLine.Refresh(True);
end;

procedure TfmTimeline.acFollowExecute(Sender: TObject);
begin
  if fFollow.Execute then
    begin
      fMain.RefreshFilter2;
      Data.SetFilter(fTimeline.DataSet,fMain.Filter+' '+fMain.Filter2,300);
      acRefresh.Execute;
    end;
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
begin
  tmp := trim(mEntry.Lines.Text);
  if lowercase(copy(tmp,0,4)) = 'task' then
    begin
      tmp := trim(copy(tmp,5,length(tmp)));
      AddTask := True;
    end;
  if copy(tmp,0,1) = '@' then
    begin
      Found := False;
      tmp := copy(tmp,2,length(tmp));
      aUsers := TStringList.Create;
      while (pos(',',tmp) > 0) and (pos(',',tmp) < pos(' ',tmp)) do
        begin
          aUsers.Add(copy(tmp,0,pos(',',tmp)-1));
          tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
        end;
      if pos(' ',tmp)>1 then
        aUsers.Add(copy(tmp,0,pos(' ',tmp)-1));
      tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
      aUser := TUser.Create(nil,Data);
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
                aUser.History.AddParentedItem(aUser.DataSet,tmp,FParentItem,'',Data.Users.FieldByName('IDCODE').AsString,nil,ACICON_USEREDITED,'',True,True)
              else
                begin
                  aTask := TTask.Create(nil,Data);
                  aTask.Insert;
                  aTask.FieldByName('SUMMARY').AsString:=tmp;
                  aTask.FieldByName('USER').AsString:=aUser.FieldByName('ACCOUNTNO').AsString;
                  aTask.DataSet.Post;
                  aTask.Free;
                end;
            end;
        end;
      aUser.Free;
    end
  else
    begin
      Data.Users.History.AddItem(Data.Users.DataSet,mEntry.Lines.Text,'','',nil,ACICON_USEREDITED,'',True,True);
      Found := True;
    end;
  fTimeline.Refresh;
  if Found then
    begin
      mEntry.Lines.Clear;
      ToolButton2.Down:=False;
      ToolButton2Click(ToolButton2);
      fTimeline.SetActive;
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
  aStatus: string; aActive: Boolean; aLink: string; aItem: TBaseDBList=nil);
begin
  with pSearch do
    begin
      if not Visible then
        Visible := True;
    end;
  if aActive then
    lbResults.Items.AddObject(aName,TLinkObject.Create(aLink));
end;

procedure TfmTimeline.bSendClick(Sender: TObject);
begin

end;
procedure TfmTimeline.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IdleTimer1.Enabled:=False;
  with Application as IBaseApplication do
    Config.WriteRect('TIMELINERECT',BoundsRect);
  CloseAction:=caHide;
end;

procedure TfmTimeline.FormDestroy(Sender: TObject);
begin
  aHistoryFrame.Free;
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

procedure TfmTimeline.FormShow(Sender: TObject);
begin
  ShowFrame;
end;

procedure TfmTimeline.fTimelineGetCellText(Sender: TObject; aCol: TColumn;
  aRow: Integer; var NewText: string; aFont: TFont);
var
  aTime: TDateTime;
  fhasObject: Boolean;
  i: Integer;
  aObj: TObject;
  arec: LargeInt;
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
                  if copy(fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString,0,6) = 'USERS@' then
                    TMGridObject(aObj).Caption := strTo+Data.GetLinkDesc(fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString)
                  else
                    TMGridObject(aObj).Caption := Data.GetLinkDesc(fTimeline.dgFake.DataSource.DataSet.FieldByName('OBJECT').AsString);
                end;
              fTimeline.DataSet.GotoBookmark(aRec);
              fTimeline.gList.RowHeights[aRow] := fTimeline.gList.RowHeights[aRow]+12;
            end;
          NewText := TMGridObject(aObj).Caption+lineending+NewText;
        end;
    end
  else if aCol.FieldName='TIMESTAMPD' then
    begin
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

procedure TfmTimeline.fTimelinegListDblClick(Sender: TObject);
var
  tmp: String;
begin
  Application.ProcessMessages;
  if fTimeline.GotoActiveRow then
    begin
      if (fTimeline.DataSet.FieldByName('LINK').AsString <> '')
      and (fTimeline.DataSet.FieldByName('ACTIONICON').AsString <> '8')
      then
        begin
          if not ProcessExists('prometerp'+ExtractFileExt(Application.ExeName)) then
            begin
              ExecProcess(AppendPathDelim(ExpandFileName(AppendPathdelim(Application.Location) + '..'))+'prometerp'+ExtractFileExt(Application.ExeName),'',False);
            end;
          SendIPCMessage('OpenLink('+fTimeline.DataSet.FieldByName('LINK').AsString+')');
        end
      else if fTimeline.DataSet.FieldByName('REFERENCE').AsString <> '' then
        begin
          tmp := fTimeline.DataSet.FieldByName('REFERENCE').AsString;
          mEntry.Lines.Text:='@'+tmp+' ';
          mEntry.SelStart:=length(mEntry.Lines.Text);
          ToolButton2.Down:=True;
          ToolButton2Click(ToolButton2);
        end;
      FParentItem := fTimeline.DataSet.Id.AsVariant;
    end;
end;

procedure TfmTimeline.fTimelinegListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_INSERT then
    begin
      ToolButton2.Down:=True;
      ToolButton2Click(ToolButton2);
    end
  else fTimeline.gListKeyDown(Sender,Key,Shift);
end;

procedure TfmTimeline.IdleTimer1Timer(Sender: TObject);
begin
  FTimeLine.Refresh(True);
end;

procedure TfmTimeline.lbResultsDblClick(Sender: TObject);
var
  aUser: TUser;
  aText: TCaption;
begin
  if lbResults.ItemIndex = -1 then exit;
  aUser := TUser.Create(nil,data);
  aUser.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
  aUser.Open;
  aText := mEntry.Text;
  if pos(',',atext)>0 then
    aText := copy(aText,0,rpos(',',aText))
  else if pos('@',atext)>0 then
    aText := copy(aText,0,pos('@',aText));
  atext := atext+aUser.FieldByName('IDCODE').AsString;
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
      VK_ESCAPE:
        begin
          pSearch.Visible:=False;
          Key := 0;
        end;
      end;
    end
  else
    begin
      if Key = VK_ESCAPE then
        begin
          ToolButton2.Down:=false;
          ToolButton2Click(ToolButton2);
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
begin
  if (((key = '@')
  or ((Key=',') and (pos(' ',mEntry.Text)=0))
  or ((Key = #8) and (copy(mEntry.Text,0,1)='@'))
  ) and (pSearch.Visible=False))
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

procedure TfmTimeline.pSearchClick(Sender: TObject);
begin

end;

procedure TfmTimeline.ToolButton2Click(Sender: TObject);
var
  aController: TAnimationController;
begin
  FParentItem:=Null;
  aController := TAnimationController.Create(pInput);
  if ToolButton2.Down then
    begin
      pInput.Height := 0;
      pInput.Visible:=True;
      aController.AnimateControlHeight(57);
      mEntry.SetFocus;
    end
  else
    begin
      aController.AnimateControlHeight(0);
      pSearch.Visible:=False;
    end;
  aController.Free;
end;

initialization
  {$I umtimeline.lrs}
  AddSearchAbleDataSet(TUser);

end.
