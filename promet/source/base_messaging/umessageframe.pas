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
unit uMessageFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, PairSplitter, ComCtrls,
  ActnList, uPrometFrames, uBaseVisualControls, uFilterFrame, uBaseDbClasses,
  DBGrids, Grids, Graphics, Buttons, ExtCtrls, uViewMessage, Variants, db,
  ClipBrd, Menus, uExtControls,uBaseDatasetInterfaces;
type
  TfMessageFrame = class(TPrometMainFrame)
    acNew: TAction;
    acDelete: TAction;
    acArchive: TAction;
    acMarkasRead: TAction;
    acCreateContact: TAction;
    acAnswer: TAction;
    acPrint: TAction;
    acForward: TAction;
    acCopyMailAddress: TAction;
    ActionList1: TActionList;
    bAnswer: TSpeedButton;
    bCreateContact: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    bForward: TSpeedButton;
    bPrint: TSpeedButton;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RefreshTimer: TIdleTimer;
    ScrollTimer: TIdleTimer;
    PairSplitter1: TPairSplitter;
    pMessageView: TPanel;
    psItems: TPairSplitterSide;
    psItem: TPairSplitterSide;
    procedure acAnswerExecute(Sender: TObject);
    procedure acArchiveExecute(Sender: TObject);
    procedure acCopyMailAddressExecute(Sender: TObject);
    procedure acCreateContactExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acMarkasReadExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure FListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FListScrolled(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure ScrollTimerTimer(Sender: TObject);
  private
    { private declarations }
    FList: TfFilter;
    fViewMessages : TfViewMessage;
    VisibleID: String;
    FBoardDir: Boolean;
    FDir : Variant;
  public
    { public declarations }
    FMessageNode : TTreeNode;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure OpenDir(Directory : Variant);
    procedure DoRefresh;override;
  end;
procedure RefreshMessages(FMessageNode :TTreeNode);
implementation
{$R *.lfm}
uses uMessages,uData,uBaseDbInterface,uPerson,uIntfStrConsts,uMessageEdit,
  uMainTreeFrame,Utils,uWikiMessage,uBaseVisualApplication;
resourcestring
  strCustomerNotFound                         = 'Kontakt nicht gefunden !';
  strNewMessageCView                          = '%s Neue Nachrichten';
  strNewUnknownMessageCView                   = '%s Neue unbekannte Nachrichten';
  strNewSendMessageCView                      = '%s ungesendete Nachrichten';

procedure TfMessageFrame.FListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if Column.FieldName = 'SUBJECT' then
        begin
          if Datasource.DataSet.FieldByName('READ').AsString = 'Y' then
            Canvas.Font.Style := []
          else
            Canvas.Font.Style := [fsBold];
          DefaultDrawColumnCell(Rect, DataCol, Column, State);
        end
      else if Column.FieldName = 'TYPE' then
        begin
          if Datasource.DataSet.FieldByName('TYPE').AsString = 'EMAIL' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,IMAGE_MESSAGE)
          else if DataSource.DataSet.FieldByName('TYPE').AsString = 'LETTE' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,IMAGE_SCRIPT)
          else if DataSource.DataSet.FieldByName('TYPE').AsString = 'FEED' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,IMAGE_FEED);
        end
      else
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
      end;
end;
procedure TfMessageFrame.acDeleteExecute(Sender: TObject);
var
  Rec: LongInt;
  i: Integer;
begin
  Screen.Cursor:=crHourGlass;
  acDelete.Enabled := False;
  Application.ProcessMessages;
  if FList.gList.SelectedRows.Count > 0 then
    begin
      for i := 0 to FList.gList.SelectedRows.Count-1 do
        begin
          TMessageList(FList.DataSet).DataSet.GotoBookmark(Pointer(FList.gList.SelectedRows.Items[i]));
          TMessageList(FList.DataSet).Delete;
        end;
      FList.gList.SelectedRows.Clear;
    end
  else if FList.gList.DataSource.DataSet.RecordCount > 0 then
    TMessageList(FList.DataSet).Delete;
  FList.List.DataSet.Next;
  Rec := FList.DataSet.GetBookmark;
  FList.DataSet.DataSet.Refresh;
  RefreshTimer.Enabled:=True;
  FList.DataSet.GotoBookmark(Rec);
  acDelete.Enabled := True;
  Screen.Cursor:=crDefault;
end;
procedure TfMessageFrame.acForwardExecute(Sender: TObject);
var
  fMessageEdit: TfMessageEdit;
  Rec: LongInt;
begin
  Rec := FList.DataSet.GetBookmark;
  fMessageEdit := TfMessageEdit.Create(nil);
  fMessageEdit.ForwardMail(fViewMessages.Message);
  FList.DataSet.GotoBookmark(Rec);
end;
procedure TfMessageFrame.acMarkasReadExecute(Sender: TObject);
var
  Rec: LongInt;
begin
  TMessageList(FList.DataSet).MarkasRead;
  FList.List.DataSet.Next;
  Rec := FList.DataSet.GetBookmark;
  FList.DataSet.DataSet.Refresh;
  RefreshTimer.Enabled:=True;
  FList.DataSet.GotoBookmark(Rec);
end;
procedure TfMessageFrame.acNewExecute(Sender: TObject);
var
  fMessageEdit: TfMessageEdit;
  fWikiEdit: TfWikiMessage;
begin
  if FBoardDir then
    begin
      fWikiEdit := TfWikiMessage.Create(Self);
      fWikiEdit.New(FDir);
    end
  else
    begin
      fMessageEdit := TfMessageEdit.Create(nil);
      fMessageEdit.SendMailTo('');
    end;
end;
procedure TfMessageFrame.acArchiveExecute(Sender: TObject);
var
  Rec: LongInt;
begin
  TMessageList(FList.DataSet).Archive;
  FList.List.DataSet.Next;
  Rec := FList.DataSet.GetBookmark;
  FList.DataSet.DataSet.Refresh;
  RefreshTimer.Enabled:=True;
  FList.DataSet.GotoBookmark(Rec);
end;
procedure TfMessageFrame.acCopyMailAddressExecute(Sender: TObject);
begin
  Clipboard.AsText := FList.DataSet.FieldByName('SENDER').AsString;
end;
procedure TfMessageFrame.acAnswerExecute(Sender: TObject);
var
  fMessageEdit: TfMessageEdit;
  Rec: LongInt;
begin
  Rec := FList.DataSet.GetBookmark;
  fMessageEdit := TfMessageEdit.Create(nil);
  fMessageEdit.AnswerMail(fViewMessages.Message);
  FList.DataSet.GotoBookmark(Rec);
end;
procedure TfMessageFrame.acCreateContactExecute(Sender: TObject);
var
  aCustomer: TPersonList;
  aCont: TPersonContactData;
  tmp: String;
  bCustomer: TPerson;
  aClass : TBaseDBDatasetClass;
  function SeparateLeft(const Value, Delimiter: string): string;
  var
    x: Integer;
  begin
    x := Pos(Delimiter, Value);
    if x < 1 then
      Result := Value
    else
      Result := Copy(Value, 1, x - 1);
  end;
  function SeparateRight(const Value, Delimiter: string): string;
  var
    x: Integer;
  begin
    x := Pos(Delimiter, Value);
    if x > 0 then
      x := x + Length(Delimiter) - 1;
    Result := Copy(Value, x + 1, Length(Value) - x);
  end;
  function GetEmailAddr(const Value: string): string;
  var
    s: string;
  begin
    s := SeparateRight(Value, '<');
    s := SeparateLeft(s, '>');
    Result := Trim(s);
  end;
begin
  aCustomer := TPersonList.Create(nil);
  if FList.DataSet.FieldByName('TYPE').AsString = 'LETTE' then
    begin
      Data.SetFilter(aCustomer,Data.QuoteField('NAME')+'='+Data.QuoteValue(FList.DataSet.FieldByName('SENDER').AsString));
      if aCustomer.Count > 0 then
        Data.GotoLink(Data.BuildLink(aCustomer.DataSet))
      else raise Exception.Create(strCustomerNotFound);
    end
  else
    begin
      aCont := TPersonContactData.CreateEx(Self,Data);
      tmp := getemailaddr(FList.DataSet.FieldByName('SENDER').AsString);
      Data.SetFilter(aCont,'UPPER("DATA") like UPPER('''+tmp+''')',0,'','ASC',False,True);
      if aCont.DataSet.Locate('DATA',VarArrayOf([tmp]),[loCaseInsensitive]) then
        begin
          Data.SetFilter(aCustomer,Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(aCont.FieldByName('ACCOUNTNO').AsString));
          if aCustomer.DataSet.Locate('ACCOUNTNO',aCont.FieldByName('ACCOUNTNO').AsString,[]) then
            begin
              Data.GotoLink(Data.BuildLink(aCustomer.DataSet));
            end
          else
            begin
              aCont.DataSet.Delete;
            end;
        end
      else
        begin
          bCustomer := TPerson.Create(nil);
          bCustomer.Append;
          tmp := FList.DataSet.FieldByName('SENDER').AsString;
          if pos('<',tmp) > 0 then
            begin
              bCustomer.FieldByName('NAME').AsString := StringReplace(copy(tmp,0,pos('<',tmp)-1),'"','',[rfReplaceAll]);
              tmp := StringReplace(UpperCase(StringReplace(ValidateFileName(bCustomer.FieldByName('NAME').AsString),'_','',[rfReplaceAll])),' ','',[rfReplaceAll]);
              tmp := StringReplace(tmp,'-','',[rfReplaceAll]);
              bCustomer.FieldByName('MATCHCODE').AsString := tmp;
            end;
          with bCustomer.ContactData.DataSet do
            begin
              Open;
              Append;
              FieldByName('TYPE').AsString := 'MAIL';
              tmp := getemailaddr(FList.DataSet.FieldByName('SENDER').AsString);
              FieldByName('DATA').AsString := getemailaddr(tmp);
              Post;
            end;
          with bCustomer.History.DataSet do
            begin
              Open;
              Insert;
              FieldByName('LINK').AsString := Data.BuildLink(FList.DataSet.DataSet);
              FieldByName('ACTION').AsString := Format(strActionMessageReceived,[FList.DataSet.FieldByName('SUBJECT').AsString]);
              FieldByName('ACTIONICON').AsInteger := ACICON_MAILNEW;
              FieldByName('REFERENCE').AsString := FList.DataSet.FieldByName('SUBJECT').AsString;
              FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              Post;
            end;
          FList.DataSet.DataSet.Edit;
          FList.DataSet.FieldByName('TREEENTRY').AsVariant := TREE_ID_MESSAGES;
          FList.DataSet.DataSet.Post;
        end;
      aCont.Free;
    end;
  aCustomer.Free;
end;
procedure TfMessageFrame.FListScrolled(Sender: TObject);
begin
  ScrollTimer.Enabled:=True;
end;
procedure TfMessageFrame.RefreshTimerTimer(Sender: TObject);
begin
  RefreshMessages(FMessageNode);
  RefreshTimer.Enabled := False;
end;
procedure TfMessageFrame.ScrollTimerTimer(Sender: TObject);
begin
  ScrollTimer.Enabled:=False;
  if FList.List.DataSet.ControlsDisabled then exit;
  if not Visible then exit;
  if VisibleID <> FList.List.DataSet.FieldByName('ID').AsString then
    begin
      VisibleID := FList.List.DataSet.FieldByName('ID').AsString;
      fViewMessages.ShowMessage(FList.List.DataSet,False);
    end;
end;
constructor TfMessageFrame.Create(AOwner: TComponent);
var
  aItem: TMenuItem;
begin
  inherited Create(AOwner);
  FList := TfFilter.Create(Self);
  with FList do
    begin
      FilterType:='I';
      DefaultRows:='GLOBALWIDTH:%;TYPE:70;SUBJECT:400;SENDER:200;TIMESTAMPD:100;';
      Parent := psItems;
      Align := alClient;
      aItem := TMenuItem.Create(FList.pmPopup);
      aItem.Action := acMarkasRead;
      FList.pmPopup.Items.Add(aItem);
      aItem := TMenuItem.Create(FList.pmPopup);
      aItem.Action := acCopyMailAddress;
      FList.pmPopup.Items.Add(aItem);
      Show;
    end;
  FList.OnDrawColumnCell:=@FListDrawColumnCell;
  FList.OnScrolled:=@FListScrolled;
  FList.Dataset := TMessageList.Create(nil);
  fViewMessages := TfViewMessage.Create(Self);
  fViewMessages.Parent := pMessageView;
  fViewMessages.Align:=alClient;
  FList.AddToolbarAction(acNew);
  FList.AddToolbarAction(acArchive);
  FList.AddToolbarAction(acMarkasRead);
  FList.AddToolbarAction(acDelete);
  {$IFDEF LCLCARBON}
  {$ENDIF}
end;
destructor TfMessageFrame.Destroy;
begin
  if Assigned(FConnection) then
    begin
      CloseConnection;
      FConnection.Free;
    end;
  fViewMessages.Free;
  FList.Free;
  inherited Destroy;
end;
procedure TfMessageFrame.OpenDir(Directory: Variant);
begin
  FList.ClearFilters;
  Data.Tree.Open;
  if Data.Tree.DataSet.Locate(Data.Tree.Id.FieldName,Directory,[]) then
    Caption := Data.Tree.FieldByName('NAME').AsString;
  if (Data.Tree.FieldByName('TYPE').AsString <> 'B')
  and (Data.Tree.Id.AsInteger <> TREE_ID_LOG_MESSAGES)
  then
    FList.BaseFilter:=Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(Directory))+' and '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString)
   else
    FList.BaseFilter:=Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(Directory));
  FBoardDir := Data.Tree.FieldByName('TYPE').AsString = 'B';
  FDir := Directory;
  FList.SortField:='SENDDATE';
  FList.SortDirection:=sdDescending;
  FList.acFilter.Execute;
  FList.SetActive;
  acAnswer.Enabled:=FList.DataSet.Count>0;
  acForward.Enabled:=FList.DataSet.Count>0;
  acCreateContact.Enabled:=FList.DataSet.Count>0;
  acDelete.Enabled:=FList.DataSet.Count>0;
  acArchive.Enabled:=FList.DataSet.Count>0;
  acMarkasRead.Enabled:=FList.DataSet.Count>0;
  DoOpen;
end;
procedure TfMessageFrame.Dorefresh;
begin
  FList.DoRefresh;
  acAnswer.Enabled:=FList.DataSet.Count>0;
  acForward.Enabled:=FList.DataSet.Count>0;
  acCreateContact.Enabled:=FList.DataSet.Count>0;
  acDelete.Enabled:=FList.DataSet.Count>0;
  acArchive.Enabled:=FList.DataSet.Count>0;
  acMarkasRead.Enabled:=FList.DataSet.Count>0;
end;
procedure RefreshMessages(FMessageNode: TTreeNode);
var
  MsgIndex: TMessageList;
  aHeight : Integer = 0;
  i: Integer;
begin
  if not Assigned(FMessageNode) then exit;
  MsgIndex := TMessageList.Create(nil);
  if Assigned(TTreeEntry(FMessageNode.Data).SubText) then
    TTreeEntry(FMessageNode.Data).SubText.Free;
  TTreeEntry(FMessageNode.Data).SubText := TStringlist.Create;
  try
    Data.SetFilter(MsgIndex,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(IntToStr(TREE_ID_MESSAGES))+' AND '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('READ')+'='+Data.QuoteValue('N')+' and '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),51);
    if (MsgIndex.Count > 0) and (MsgIndex.Count < 51) then
      TTreeEntry(FMessageNode.Data).SubText.Add(Format(strNewMessageCView,[IntToStr(MsgIndex.Count)]))
    else if (MsgIndex.Count > 0) then
      TTreeEntry(FMessageNode.Data).SubText.Add(Format(strNewMessageCView,['>50']));
  except
  end;
  try
    Data.SetFilter(MsgIndex,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(IntToStr(TREE_ID_UNKNOWN_MESSAGES))+' AND '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('READ')+'='+Data.QuoteValue('N')+' and '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),51);
    if (MsgIndex.Count > 0) and (MsgIndex.Count < 51) then
      TTreeEntry(FMessageNode.Data).SubText.Add(Format(strNewUnknownMessageCView,[IntToStr(MsgIndex.Count)]))
    else if (MsgIndex.Count > 0) then
      TTreeEntry(FMessageNode.Data).SubText.Add(Format(strNewUnknownMessageCView,['>50']));
  except
  end;
  try
    Data.SetFilter(MsgIndex,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(IntToStr(TREE_ID_SEND_MESSAGES))+' AND '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('READ')+'='+Data.QuoteValue('N')+' and '+Data.QuoteField('MESSAGEIDX')+'.'+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),51);
    if MsgIndex.Count > 0 then
      TTreeEntry(FMessageNode.Data).SubText.Add(Format(strNewSendMessageCView,[IntToStr(MsgIndex.Count)]));
  except
  end;
  for i := 0 to TTreeEntry(FMessageNode.Data).SubText.Count-1 do
    aHeight := aHeight+FMessageNode.TreeView.Canvas.TextHeight(TTreeEntry(FMessageNode.Data).SubText[i]);
  if aHeight > 32 then FmessageNode.Height := aHeight
  else FMessageNode.Height := 32;
  fMainTreeFrame.tvMain.Invalidate;
  MsgIndex.free;
end;
initialization
  TBaseVisualApplication(Application).RegisterForm(TfMessageFrame);
end.

