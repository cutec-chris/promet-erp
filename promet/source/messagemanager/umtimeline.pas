unit umtimeline;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, ZVDateTimePicker,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage,ugridview,uHistoryFrame,
  uExtControls,uBaseVisualControls,uBaseDbClasses,uFormAnimate;
type

  { TfmTimeline }

  TfmTimeline = class(TForm)
    acFollow: TAction;
    acRefresh: TAction;
    acSend: TAction;
    ActionList1: TActionList;
    bSend: TBitBtn;
    ToolButton2: TSpeedButton;
    IdleTimer1: TIdleTimer;
    mEntry: TMemo;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miNewMandant: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure acRefreshExecute(Sender: TObject);
    procedure acSendExecute(Sender: TObject);
    procedure bSendClick(Sender: TObject);
    function FContListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure fTimelineGetCellText(Sender: TObject; aCol: TColumn;
      aRow: Integer; var NewText: string; aFont: TFont);
    procedure fTimelinegListDblClick(Sender: TObject);
    procedure fTimelinegListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure mEntryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
    SysCommands : TSystemCommands;
    fTimeline : TfGridView;
    aHistoryFrame: TfHistoryFrame;
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
  uMain,LCLType,utask,uProcessManager,uprometipc,ProcessUtils;
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
      fTimeline.DefaultRows:='GLOBALWIDTH:530;ACTIONICON:30;ACTION:200;REFERENCE:100;OBJECT:100;TIMESTAMPD:100;';
      fTimeline.BaseName:='PTLINE';
      fTimeline.SortDirection:=sdDescending;
      fTimeline.SortField:='TIMESTAMPD';
      fTimeline.TextField:='ACTION';
      fTimeline.ReadOnly:=True;
      fTimeline.FilterRow:=True;
      fTimeline.OnDrawColumnCell:=@FContListDrawColumnCell;
      fTimeline.OnGetCellText:=@fTimelineGetCellText;
      fTimeline.DataSet := TBaseHistory.Create(Self,Data);
      fTimeline.gList.OnKeyDown:=@fTimelinegListKeyDown;
      fTimeline.gList.OnDblClick:=@fTimelinegListDblClick;
      Data.SetFilter(fTimeline.DataSet,fMain.Filter,500);
      with Application as IBaseApplication do
        Config.ReadRect('TIMELINERECT',aBoundsRect,BoundsRect);
      fTimeline.Show;
      Show;
      ShowFrame;
      fTimeline.SetActive;
      BoundsRect := aBoundsRect;
      fTimeline.gList.TopRow:=0;
    end;
  fTimeline.Show;
  Show;
  fTimeline.acFilter.Execute;
  FTimeLine.Refresh(True);
  IdleTimer1.Enabled:=True;
  fTimeline.SetActive;
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

function TfmTimeline.FContListDrawColumnCell(Sender: TObject; const Rect: TRect;
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
begin
  with (Sender as TCustomGrid), Canvas do
    begin
      Result := True;
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if (Column.FieldName = 'ACTIONICON') then
        begin
          if not (TExtStringGrid(Sender).Cells[Column.Index+1,DataCol] = '') then
            aHistoryFrame.HistoryImages.Draw(Canvas,Rect.Left,Rect.Top,StrToIntDef(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol],-1));
        end
      else if (Column.FieldName = 'REFOBJECT') or (Column.FieldName = 'OBJECT') then
        begin
          with Application as IBaseDbInterface do
            aText := Data.GetLinkDesc(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol]);
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
          with TStringGrid(Sender).Canvas do
            begin
              TStringGrid(Sender).Canvas.Brush.Color:=aColor;
              FillRect(Rect);
            end;
          TStringGrid(Sender).Canvas.Brush.Style:=bsClear;
          TextRect(Rect,Rect.Left+3,Rect.Top,aText,aTextStyle);
          Result := True;
        end
      else
        Result := False;
    end;
end;

procedure TfmTimeline.acRefreshExecute(Sender: TObject);
begin
  FTimeLine.Refresh(True);
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
                aUser.History.AddItem(aUser.DataSet,tmp,'',Data.Users.FieldByName('IDCODE').AsString,nil,ACICON_USEREDITED,'',True,True)
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
      if (Panel1.Visible) and (Panel1.Height>5) then exit;
      Close;
    end;
end;
procedure TfmTimeline.fTimelineGetCellText(Sender: TObject; aCol: TColumn;
  aRow: Integer; var NewText: string; aFont: TFont);
begin
  if aCol.FieldName='LINK' then
    NewText := Data.GetLinkDesc(NewText)
  else if aCol.FieldName='OBJECT' then
    NewText := Data.GetLinkDesc(NewText);
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

procedure TfmTimeline.mEntryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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

procedure TfmTimeline.ToolButton2Click(Sender: TObject);
var
  aController: TAnimationController;
begin
  aController := TAnimationController.Create(Panel1);
  if ToolButton2.Down then
    begin
      Panel1.Height := 0;
      Panel1.Visible:=True;
      aController.AnimateControlHeight(57);
      mEntry.SetFocus;
    end
  else
    begin
      aController.AnimateControlHeight(0);
    end;
  aController.Free;
end;

initialization
  {$I umtimeline.lrs}

end.
