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

info@cu-tec.de
*******************************************************************************}
unit uMessageEdit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Dialogs, Buttons, StdCtrls,
  ExtCtrls, ComCtrls, Utils, db, uIntfStrConsts, md5, mimepart, mimemess,
  Variants, uExtControls, uViewMessage, FileUtil, LCLType,
  ActnList, urichframe, RichMemo, LCLProc, ClipBrd, Menus, DbCtrls, EditBtn,
  htmltortf, uMessages, uDocumentFrame, uBaseSearch, uBaseDbClasses;
type
  THackComboBox = class(TComboBox);

  { TfMessageEdit }

  TfMessageEdit = class(TForm)
    acClose: TAction;
    acShowHeader: TAction;
    acSaveAs: TAction;
    ActionList1: TActionList;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel6: TBevel;
    bSend: TSpeedButton;
    cbAccount: TComboBox;
    cbTo: TComboBox;
    eCC: TComboBox;
    cbType: TComboBox;
    eSubject: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbResults: TListBox;
    lMessage: TLabel;
    lCC: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    pSearch: TPanel;
    pTos: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pcTabs: TExtMenuPageControl;
    pMain: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    sbStatus: TStatusBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ToolBar1: TPanel;
    tsContent: TTabSheet;
    procedure acAddAsOrderExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acShowHeaderExecute(Sender: TObject);
    procedure bSendClick(Sender: TObject);
    procedure cbToEnter(Sender: TObject);
    procedure cbToExit(Sender: TObject);
    procedure cbToKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbToKeyPress(Sender: TObject; var Key: char);
    procedure dbMandantmsgWalkPart(const Sender: TMimePart);
    procedure eSubjectChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbResultsDblClick(Sender: TObject);
    procedure MessageEditChange(Sender: TObject);
    procedure SenderTComboBoxActiveSearchItemFound(aIdent: string;
      aName: string; aStatus: string;aActive : Boolean; aLink: string;aPrio : Integer; aItem: TBaseDBList=nil);
  private
    FConnection: TComponent;
    FDataSet: TMessage;
    FDocumentFrame: TfDocumentFrame;
    aRec : string;
    fViewMessages : TfViewMessage;
    fRichEdit: TfRichFrame;
    IsModified : Boolean;
    ActiveSearch: TSearch;
    procedure AddSignature;
    procedure MarkReadOnly;
    procedure MarkReadWrite;
    procedure AddDocuments(Sender: TObject);
    procedure EnterCB(Data: PtrInt);
    procedure AddHistory(Sender: TObject);
  public
    { public declarations }
    Successfulsended : Boolean;
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    property DataSet : TMessage read FDataSet;
    property Connection : TComponent read FConnection;
    procedure Showmail;
    function  AnswerMail(AnswerMessage : TMessage) : Boolean;
    function  ForwardMail(AnswerMessage : TMessage) : Boolean;
    procedure SendMailTo(receiver : string);
    procedure SendMailToWithDoc(Receiver: string;Subject : string;Msg : string;Document : string;DeleteFile : Boolean);
    procedure OpenFromLink(aLink : string);
  end;
implementation
{$R *.lfm}
uses uLogWait,uData,uBaseDbInterface,uBaseVisualApplication,
  uDocuments, uminiconvencoding, uOrder,uPrometFrames,uPerson,lMessages,uEditText,
  uMimeMessages,uHistoryFrame,uprometframesinplace,uMessageRoute;
resourcestring
  strLoggingIn                  = 'Logging In...';
  strLoggingOut                 = 'Logging out...';
  strSendingMessage             = 'Sending Message...';
  strEncodingMessage            = 'Encoding Message...';
  strAddingParts                = 'Adding Parts...';
  strAddingUsers                = 'Adding Users...';
  strFailedToSendMail           = 'Mailversand fehlgeschlagen !';
  strFailedToAddSenderAddress   = 'Konnte Absenderadresse nicht hinzufügen (möglicherweise ist diese ungültig) !';
  strFailedToAddReceiverAdress  = 'Konnte Empfängeradresse nicht hinzufügen (möglicherweise ist diese ungültig) !';
  strActionMessageSend          = 'Nachricht gesendet';
  strMailSubjectClear           = 'Das Betreff Feld in Ihrer e-Mail sollte nicht leer sein, das wird von vielen Spam Filtern als Spam gewertet.';
  strRealLeaveMail              = 'Möchten Sie die Nachricht wirklich verwerfen ?';
  strAddingListEntry            = 'Verarbeite Eintrag "%s"';
  strAddingMail                 = 'erstelle Mail an "%s"';
procedure TfMessageEdit.AddSignature;
var
  tmpRich: TRichMemo;
  tmpRich2: TRichMemo;
  OldClipbrd: String;
  ss: TStringStream;

  function MsgPos : Integer;
  begin
    Result := UTF8Pos('%MESSAGE%',Stringreplace(tmpRich.Lines.Text,#10,'',[rfReplaceAll]));
  end;
begin
  {$IFDEF LCLGTK2}
  exit;
  {$ENDIF}
  {
  if Data.DataModule.Locate(Data.Options.DataSet,'OPTION','SIGNATURE',[loCaseInsensitive,loPartialKey]) then
    begin
      tmpRich := TRichMemo.Create(Self);
      tmpRich.Parent := Self;
      tmpRich.HandleNeeded;
      ss := TStringStream.Create(Data.Options.FieldByName('VALUE').AsString);
      tmpRich.LoadRichText(ss);
      ss.Free;
      OldClipbrd := Clipboard.AsText;
      tmpRich2 := TRichMemo.Create(Self);
      tmpRich2.Parent := Self;
      tmpRich2.HandleNeeded;
      tmpRich.SelStart:=0;
      tmpRich.SelLength:=MsgPos-1;
      tmpRich.CopyToClipboard;
      tmpRich2.PasteFromClipboard;
      fRichEdit.rmText.SelStart:=0;
      fRichEdit.rmText.SelLength:=UTF8Length(fRichEdit.rmText.Lines.Text);
      if fRichEdit.rmText.SelLength > 0 then
        begin
          fRichEdit.rmText.CopyToClipboard;
          tmpRich2.PasteFromClipboard;
        end;
      tmpRich.SelStart:=MsgPos+9;
      tmpRich.SelLength:=UTF8Length(tmpRich.Lines.Text);
      tmpRich.CopyToClipboard;
      tmpRich2.PasteFromClipboard;
      ss := TStringStream.Create('');
      tmpRich2.SaveRichText(ss);
      fRichEdit.AsRichText:=ss.DataString;
      ss.Free;
      tmpRich.Free;
      tmpRich2.Free;
      ClipBoard.AsText:=OldClipbrd;
    end;
  }
end;
constructor TfMessageEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := Data.GetNewConnection;
  FDataSet := TMessage.CreateEx(Self,Data,FConnection);
end;
destructor TfMessageEdit.Destroy;
begin
  FDataset.Free;
  inherited;
end;
procedure TfMessageEdit.bSendClick(Sender: TObject);
var
  tmp: String;
  tmp1: String;
  i: Integer;
  atmp: String;
  tmp2: String;
  aList: TStringList;
  Rec: String;
  actRec: String;
  aMimePart: TMimePart;
  MP: TMimePart;
  DontHide: Boolean;
  aStream: TStringStream;
  aLists: TLists = nil;
  aPerson: TPerson;
  bDataSet : TMessage;
  aDocuments: TDocuments;
  bDocument: TDocument;
  tmpID: String;

  procedure SendMessageCopyTo(aReceiver : string);
  var a : Integer;
    aGUID : TGuid;
  begin
    bDataSet := TMessage.CreateEx(Self,Data);
    Randomize;
    bDataSet.Insert;
    tmpID := '';
    CreateGUID(aGUID);
    tmpID := StringReplace(StringReplace(StringReplace(GUIDToString(aGUID),'-','',[rfReplaceAll]),'{','',[rfReplaceAll]),'}','',[rfReplaceAll]);
    fLogWaitForm.ShowInfo(Format(strAddingMail,[tmpID]));
    fLogWaitForm.ShowInfo(Format(strAddingMail,[aReceiver]));
    bDataSet.FieldByName('ID').AsString := tmpID;
    bDataSet.FieldByName('SQL_ID').AsVariant := Data.GetUniID;
    bDataSet.FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
    bDataSet.FieldByName('MSG_ID').AsVariant :=  bDataSet.FieldByName('SQL_ID').AsVariant;
    bDataSet.FieldByName('TYPE').AsString := 'EMAIL';
    bDataSet.FieldByName('READ').AsString := 'N';
    bDataSet.FieldByName('SENDER').AsString := cbAccount.Text;
    bDataSet.FieldByName('SENDDATE').AsDateTime := Now();
    bDataSet.FieldByName('SUBJECT').AsString := eSubject.Text;
    bDataSet.FieldByName('TREEENTRY').AsVariant := TREE_ID_SEND_MESSAGES;
    bDataSet.DataSet.Post;
    bDataSet.DataSet.Edit;
    bDataSet.Content.Select(tmpID);
    bDataSet.Content.Open;
    bDataSet.Content.DataSet.Append;
    bDataSet.Content.FieldByName('ID').AsString := tmpID;
    bDataSet.Content.FieldByName('RECEIVERS').AsString := aReceiver;
    bDataSet.Content.FieldByName('CC').AsString := eCC.Text;
    bDataSet.Content.FieldByName('TIMESTAMPD').AsDateTime := Now();
    if cbType.Text = 'HTML' then
      begin
        bDataSet.Content.FieldByName('DATATYP').AsString := 'HTML';
        aStream := TStringStream.Create(RTFToHTML(frichEdit.rmText,''));
        Data.StreamToBlobField(aStream,bDataSet.Content.DataSet,'DATA');
        aStream.Free;
      end
    else
      begin
        bDataSet.Content.FieldByName('DATATYP').AsString := 'PLAIN';
        aStream := TStringStream.Create(frichEdit.rmText.Text);
        Data.StreamToBlobField(aStream,bDataSet.Content.DataSet,'DATA');
        aStream.Free;
      end;
    bDataSet.Content.DataSet.Post;
    bDataSet.CascadicPost;
    aDocuments := TDocuments.CreateEx(Self,Data);
    aDocuments.Select(FDataSet.Content.Id.AsVariant,'N',tmpID,Null,Null);
    aDocuments.Open;
    if aDocuments.Count > 0 then
      begin
        with aDocuments.DataSet do
          begin
            while not EOF do
              begin
                bDocument := TDocument.CreateEx(Self,Data);
                bDocument.Select(bDataSet.Content.Id.AsVariant,'N',tmpID,Null,Null);
                bDocument.AddFromLink(Data.BuildLink(aDocuments.DataSet));
                bDocument.Free;
                Next;
              end;
          end;
      end;
    aDocuments.Free;
  end;

begin
  if lowercase(copy(cbTo.text,length(cbTo.Text)-5,6)) = '@lists' then
    begin
      aLists := TLists.CreateEx(Self,Data);
      aLists.Open;
      if not aLists.DataSet.Locate('NAME',copy(cbTo.Text,0,length(cbTo.Text)-6),[loCaseInsensitive]) then
        FreeAndNil(aLists)
      else aLists.Entrys.Open;
    end;
  with DataSet.DataSet do
    begin //Messagenot there
      FieldByName('READ').AsString := 'N';
      FieldByName('SENDER').AsString := cbAccount.Text;
      FieldByName('SENDDATE').AsDateTime := Now();
      FieldbyName('SUBJECT').AsString := eSubject.Text;
      FieldByName('TREEENTRY').AsVariant := TREE_ID_SEND_MESSAGES;
      if not FDataSet.Content.DataSet.Active then
        begin
          FDataSet.Content.Select(FDataSet.FieldByName('ID').AsString);
          FDataSet.Content.Open;
        end;
      with FDataSet.Content.DataSet do
        begin
          Edit;
          FieldByName('ID').AsString := FDataSet.FieldByName('ID').AsString;
          if not Assigned(aLists) then
            FieldbyName('RECEIVERS').AsString := cbTo.text
          else
            begin
              tmp := aLists.Entrys.FieldByName('LINK').AsString;
              while (copy(tmp,0,pos('@',tmp)-1) <> 'CUSTOMERS') and (aLists.Entrys.FieldByName('ACTIVE').AsString='Y') and (not aLists.Entrys.DataSet.EOF) do
                begin
                  aLists.Entrys.DataSet.Next;
                  tmp := aLists.Entrys.FieldByName('LINK').AsString;
                end;
              if (copy(tmp,0,pos('@',tmp)-1) = 'CUSTOMERS') then
                begin
                  aPerson := TPerson.CreateEx(Self,Data);
                  aPerson.SelectFromLink(tmp);
                  aPerson.Open;
                  aPerson.ContactData.Open;
                  if aPerson.ContactData.DataSet.Locate('TYPE;ACTIVE',VarArrayOf(['MAIL','Y']),[])
                  or aPerson.ContactData.DataSet.Locate('TYPE',VarArrayOf(['MAIL']),[])
                  then
                    begin
                      FieldbyName('RECEIVERS').AsString := aPerson.ContactData.FieldByName('DATA').AsString;
                      with aLists.Entrys.DataSet do
                        begin
                          Edit;
                          FieldByName('ACTIVE').AsString:='Y';
                          Post;
                        end;
                    end;
                  aPerson.Free;
                end;
              aLists.Entrys.DataSet.Next;
            end;
          FieldbyName('CC').AsString := eCC.Text;
          FieldByName('TIMESTAMPD').AsDateTime := Now();
          if cbType.Text = 'HTML' then
            begin
              FieldbyName('DATATYP').AsString := 'HTML';
              aStream := TStringStream.Create(RTFToHTML(frichEdit.rmText,''));
              Data.StreamToBlobField(aStream,FDataSet.Content.DataSet,'DATA');
              aStream.Free;
            end
          else
            begin
              FieldbyName('DATATYP').AsString := 'PLAIN';
              aStream := TStringStream.Create(frichEdit.rmText.Text);
              Data.StreamToBlobField(aStream,FDataSet.Content.DataSet,'DATA');
              aStream.Free;
            end;
          Successfulsended := True;
          sbStatus.SimpleText:='';
          Post;
        end;
      DataSet.CascadicPost;
      if Assigned(aLists) then
        begin
          fLogWaitForm.SetLanguage;
          fLogWaitForm.lbLog.Clear;
          fLogWaitForm.Show;
          while not aLists.Entrys.DataSet.EOF do
            begin
              if aLists.Entrys.FieldByName('ACTIVE').AsString<>'Y' then
                begin
                  tmp := aLists.Entrys.FieldByName('LINK').AsString;
                  fLogWaitForm.ShowInfo(Format(strAddingListEntry,[Data.GetLinkDesc(tmp)]));
                  while (copy(tmp,0,pos('@',tmp)-1) <> 'CUSTOMERS') and (aLists.Entrys.FieldByName('ACTIVE').AsString='Y') and (not aLists.Entrys.DataSet.EOF) do
                    begin
                      aLists.Entrys.DataSet.Next;
                      tmp := aLists.Entrys.FieldByName('LINK').AsString;
                    end;
                  if (copy(tmp,0,pos('@',tmp)-1) = 'CUSTOMERS') then
                    begin
                      aPerson := TPerson.CreateEx(Self,Data);
                      aPerson.SelectFromLink(tmp);
                      aPerson.Open;
                      aPerson.ContactData.Open;
                      if aPerson.ContactData.DataSet.Locate('TYPE;ACTIVE',VarArrayOf(['MAIL','Y']),[])
                      or aPerson.ContactData.DataSet.Locate('TYPE',VarArrayOf(['MAIL']),[])
                      then
                        begin
                          SendMessageCopyTo(aPerson.ContactData.FieldByName('DATA').AsString);
                          with aLists.Entrys.DataSet do
                            begin
                              Edit;
                              FieldByName('ACTIVE').AsString:='Y';
                              Post;
                            end;
                          bDataSet.Free;
                        end;
                      aPerson.Free;
                    end;
                end;
              aLists.Entrys.DataSet.Next;
            end;
          fLogWaitForm.bAbort.Kind:=bkClose;
        end;
      with Application as IBaseDbInterface do
        Data.CommitTransaction(FConnection);
      FreeAndNil(aLists);
      {$ifndef heaptrc}
      TBaseVisualApplication(Application).MessageHandler.SendCommand('smtpsender','Send('+Data.Users.FieldByName('NAME').AsString+')');
      {$endif}
      IsModified := False;
      Self.Close;
    end;
end;
procedure TfMessageEdit.cbToEnter(Sender: TObject);
var
  aShift: TShiftState;
  aKey: Word = VK_RIGHT;
begin
  if (copy(TComboBox(Sender).Text,length(TComboBox(Sender).Text),1) <> '')
  and (trim(TComboBox(Sender).Text) <> '') then
    begin
      TComboBox(Sender).Text := TComboBox(Sender).Text+',';
      Application.QueueAsyncCall(@EnterCB,PtrInt(Sender));
    end;
end;
procedure TfMessageEdit.cbToExit(Sender: TObject);
begin
  pSearch.Visible := False;
  if (copy(TComboBox(Sender).Text,length(TComboBox(Sender).Text),1) = ',') then
    begin
      TComboBox(Sender).Text := copy(TComboBox(Sender).Text,0,length(TComboBox(Sender).Text)-1);
    end;
end;
procedure TfMessageEdit.cbToKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_PRIOR,
  VK_UP:
    begin
      if lbResults.ItemIndex = -1 then
        lbResults.ItemIndex:=0;
      lbResults.ItemIndex:=lbResults.ItemIndex-1;
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
  end;
end;
procedure TfMessageEdit.cbToKeyPress(Sender: TObject; var Key: char);
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
  tmp: TCaption;
begin
  if Assigned(ActiveSearch) then
    ActiveSearch.Abort;
  SearchTypes := SearchTypes+[fsShortnames];
  SearchTypes := SearchTypes+[fsIdents];
  SetLength(SearchLocations,length(SearchLocations)+1);
  SearchLocations[length(SearchLocations)-1] := strCustomers;
  SetLength(SearchLocations,length(SearchLocations)+1);
  SearchLocations[length(SearchLocations)-1] := strLists;
  with Sender as TComboBox do
    begin
      lbResults.Items.Clear;
      if not Assigned(ActiveSearch) then
        ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
      ActiveSearch.Sender := TComponent(Sender);
      ActiveSearch.OnItemFound:=@SenderTComboBoxActiveSearchItemFound;
      tmp := TComboBox(Sender).Text;
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
      ActiveSearch.Start(tmp);
      while ActiveSearch.Active do Application.ProcessMessages;
    end;
end;
procedure TfMessageEdit.acCloseExecute(Sender: TObject);
begin
  Close;
end;
procedure TfMessageEdit.acSaveAsExecute(Sender: TObject);
var
  msg: TMimeMess;
  aFN: String;
begin
  if SaveDialog1.Execute then
    begin
      msg := TMimeMessage(DataSet).EncodeMessage;
      aFN := SaveDialog1.FileName;
      if Pos('.',aFN)=0 then
        aFN := aFN+'.eml';
      msg.Lines.SaveToFile(aFN);
    end;
end;
procedure TfMessageEdit.acShowHeaderExecute(Sender: TObject);
begin
  fEditText.SetLanguage;
  fEditText.mText.Lines.Text:=FDataSet.Content.FieldByName('HEADER').AsString;
  fEditText.ShowModal;
end;
procedure TfMessageEdit.acAddAsOrderExecute(Sender: TObject);
var
  aOrderType: TOrderTyp;
  aFrame: TPrometMainFrame;
  aLinkFrame: TPrometMainFrame = nil;
  aLinkIndex: Integer;
  i: Integer;
begin
  aOrderType := TOrderTyp.CreateEx(Self,Data);
  Data.SetFilter(aOrderType,Data.QuoteField('STATUSNAME')+'='+Data.QuoteValue(copy(TMenuItem(Sender).Caption,length(strNewOrder)+1,length(TMenuItem(Sender).Caption))));
  if (aOrderType.Count > 0) and Assigned(FDocumentFrame) then
    begin
      Application.ProcessMessages;
      {
      aFrame := TfOrderFrame.Create(Self);
      fMain.pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      aFrame.New(aOrderType.FieldByName('STATUS').AsString);
//      TOrder(aFrame.DataSet).Address.DataSet.Insert;
//      TOrder(aFrame.DataSet).Address.Assign(DataSet);
      aFrame.ShowPreview(FDocumentFrame.DataSet.Id.Asvariant);
      if FDocumentFrame.acCopyAsLink.Execute then
        begin
          aFrame.pcHeader.AddTab(TfLinkFrame.Create(aFrame),False);
          for i := 0 to aFrame.pcHeader.PageCount-2 do
            if aFrame.pcHeader.Pages[i].ControlCount = 1 then
              if aFrame.pcHeader.Pages[i].Controls[0] is TfLinkFrame then
                aLinkFrame := TfLinkFrame(aFrame.pcHeader.Pages[i].Controls[0]);
          if Assigned(aLinkFrame) then
            aLinkFrame.acPasteLinks.Execute;
        end;
      }
    end;
  aOrderType.Destroy;
end;
procedure TfMessageEdit.dbMandantmsgWalkPart(const Sender: TMimePart);
var
  s : string;
  ss: TStringStream;
begin
  Sender.DecodePart;
  if Sender.PrimaryCode = MP_TEXT then
    begin
      if (UpperCase(Sender.Secondary) = 'PLAIN') or (UpperCase(Sender.Secondary) = 'HTML') then
        begin
//          s := ReadStrFromStream(Sender.DecodedLines,Sender.DecodedLines.Size);
          if (AnsiToUTF8(s) <> '') and (s <> AnsiToUTF8(s)) then
            s := AnsiToUTF8(s);
          ss := TStringStream.Create('');
          {
          Data.DataModule.BlobFieldToStream(Data.Messages.DataSet,'DATA',ss);
          if (UpperCase(Sender.Secondary) = 'PLAIN') then
            s := ss.DataString+lineending+s;
          ss.Free;
          ss := TStringStream.Create(s);
          if Data.Messages.FieldByName('DATATYP').AsString = '' then
            Data.Messages.FieldByName('DATATYP').AsString := Uppercase(Sender.Secondary);
          if Data.Messages.FieldByName('DATATYP').AsString = Uppercase(Sender.Secondary) then
            Data.DataModule.StreamToBlobField(ss,Data.Messages.DataSet,'DATA')
          else if Sender.Secondary = 'HTML' then
            begin //Use HTML if PLAIN+HTML are avalible
              Data.Messages.FieldByName('DATATYP').AsString := Uppercase(Sender.Secondary);
              Data.DataModule.StreamToBlobField(ss,Data.Messages.DataSet,'DATA');
            end;
          }
          ss.Free;
       end;
    end;
end;
procedure TfMessageEdit.eSubjectChange(Sender: TObject);
begin
  if eSubject.Text = '' then
    begin
      lMessage.Caption:=strMailSubjectClear;
      lMessage.Visible:=True;
    end
  else lMessage.Visible:=False;
end;
procedure TfMessageEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ((fViewMessages.mContent.Visible and fViewMessages.mContent.Modified and (not fViewMessages.mContent.ReadOnly))
  or (fRichEdit.Visible and fRichEdit.rmText.Modified))
  and IsModified then
    if MessageDlg(strMessages,strRealLeaveMail,mtWarning,[mbNo,mbYes],0) = mrNo then
      begin
        CloseAction := caNone;
        exit;
      end;
  if DataSet.CanEdit then
    begin
      DataSet.CascadicCancel;
      with Application as IBaseDbInterface do
        Data.RollbackTransaction(FConnection);
    end;
  CloseAction := caFree;
end;
procedure TfMessageEdit.FormCreate(Sender: TObject);
begin
  fViewMessages := TfViewMessage.Create(Self);
  fViewMessages.Parent := tsContent;
  fViewMessages.Align:=alClient;
  fRichEdit := TfRichFrame.Create(Self);
  fRichEdit.Parent := tsContent;
  fRichEdit.Align:=alClient;
end;
procedure TfMessageEdit.FormDestroy(Sender: TObject);
begin
  fViewMessages.Free;
  fRichEdit.Free;
end;
procedure TfMessageEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfMessageEdit.lbResultsDblClick(Sender: TObject);
var
  tmp: string;
  aPerson: TPerson;
  aList: TLists;
begin
  tmp := TComboBox(ActiveSearch.Sender).Text;
  tmp := copy(tmp,0,pos(',',tmp));
  if lbResults.ItemIndex < 0 then exit;
  pSearch.Visible:=False;
  aPerson := TPerson.Create(nil);
  aPerson.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
  aPerson.Open;
  if aPerson.Count > 0 then
    begin
      Data.SetFilter(aPerson.ContactData,Data.QuoteField('TYPE')+'='+Data.QuoteValue('MAIL'));
      if aPerson.ContactData.Count > 0 then
        begin
          aPerson.ContactData.DataSet.Locate('ACTIVE','Y',[]);
          if aPerson.ContactData.FieldByName('ACTIVE').AsString <> 'N' then
            tmp := tmp+aPerson.ContactData.FieldByName('DATA').AsString;
        end;
    end;
  aPerson.Free;
  if tmp = '' then
    tmp := TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link;
  if copy(TComboBox(ActiveSearch.Sender).Text,length(TComboBox(ActiveSearch.Sender).Text),1) <> ',' then
    TComboBox(ActiveSearch.Sender).Text := tmp+',';
  if copy(tmp,0,pos('@',tmp)-1) = 'LISTS' then
    begin
      aList := TLists.CreateEx(Self,Data);
      aList.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
      aList.Open;
      TComboBox(ActiveSearch.Sender).Text := aList.FieldByName('NAME').AsString+'@lists';
      aList.Free;
    end;
  Application.QueueAsyncCall(@EnterCB,PtrInt(ActiveSearch.Sender));
end;
procedure TfMessageEdit.MessageEditChange(Sender: TObject);
begin
  IsModified := True;
end;
procedure TfMessageEdit.SenderTComboBoxActiveSearchItemFound(aIdent: string;
  aName: string; aStatus: string; aActive: Boolean; aLink: string;
  aPrio: Integer; aItem: TBaseDBList);
begin
  with pSearch do
    begin
      if not Visible then
        begin
          Visible := True;
          Left := TCombobox(ActiveSearch.Sender).Parent.Left+TCombobox(ActiveSearch.Sender).Left+3;
          Top := pMain.Top+pTos.Top+TCombobox(ActiveSearch.Sender).Top+TCombobox(ActiveSearch.Sender).Height;
        end;
    end;
  if aActive then
    begin
      if copy(aLink,0,pos('@',aLink)-1) = 'LISTS' then
        lbResults.Items.AddObject(aName+' ('+strLists+')',TLinkObject.Create(aLink))
      else
        lbResults.Items.AddObject(aName,TLinkObject.Create(aLink));
    end;
end;
procedure TfMessageEdit.Showmail;
var
  tmpStrLst : TStringList;
  NewHTML: TSimpleIpHtml;
  i: Integer;
  ss: TStringStream;
  fs : TFileStream;
  ID: String;
  sl: TStringList;
  tmp: String;
  aDocuments: TDocuments;
begin
  Data.StartTransaction(FConnection);
  pcTabs.ActivePage:=tsContent;
  aRec := '';
  tmpStrLst := TStringList.Create;
  FDataSet.Content.Open;
  tmpStrLst.Text:=DataSet.Content.FieldByName('RECEIVERS').AsString;
  cbTo.Clear;
  cbTo.Text := '';
  for i := 0 to tmpStrLst.Count-1 do
    if trim(tmpStrLst[i]) <> '' then
      if i=0 then
        cbTo.Text := cbTo.Text+tmpStrLst[i]
      else
        cbTo.Text := cbTo.Text+','+tmpStrLst[i];
  tmpStrLst.Text:=FDataSet.Content.FieldByName('CC').AsString;
  eCC.Text := '';
  for i := 0 to tmpStrLst.Count-1 do
    if i=0 then
      eCC.Text := eCC.Text+tmpStrLst[i]
    else
      eCC.Text := eCC.Text+','+tmpStrLst[i];
  tmpStrLst.Free;
  eSubject.Text := FDataSet.FieldByName('SUBJECT').AsString;
  pTos.Visible:=(eCC.Text <> '') or (cbTo.Text <> '');
  cbAccount.Text:=FDataSet.FieldByName('SENDER').AsString;
  fViewMessages.Visible:=True;
  fRichEdit.Visible:=False;
  fViewMessages.ShowMessage(DataSet.DataSet,True);
  MarkReadOnly;
  IsModified := False;
  pcTabs.AddTabClass(TfDocumentFrame,strAttatchments,@AddDocuments);
  aDocuments := TDocuments.CreateEx(Self,Data);
  if FDataSet.Content.Count > 0 then
    begin
      aDocuments.Select(FDataSet.Content.Id.AsVariant,'N',DataSet.FieldByName('ID').AsString,Null,Null);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          pcTabs.AddTab(TfDocumentFrame.Create(Self),False);
          TfDocumentFrame(pcTabs.GetTab(TfDocumentFrame).Controls[0]).DataSet := aDocuments;
        end;
    end
  else aDocuments.Free;
  pcTabs.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  DataSet.History.Open;
  if DataSet.History.Count > 0 then
    pcTabs.AddTab(TfHistoryFrame.Create(Self),False);
  if FDataSet.Content.FieldByName('HEADER').AsString<>'' then
    pcTabs.AddTab(TfMessageRoute.Create(Self),False);
  Show;
end;
function TfMessageEdit.AnswerMail(AnswerMessage : TMessage) : Boolean;
var
  tmpStrLst : TStringList;
  NewHTML: TSimpleIpHtml;
  i: Integer;
  ss: TStringStream;
  sl: TStringList;
  Rec : string;
  tmp: String;
begin
  Visible := True;
  pcTabs.ActivePage:=tsContent;
  Data.StartTransaction(FConnection);
  FDataSet.Insert;
  randomize;
//  frDocuments.Refresh(tmpMID,'N',tmpID,Null,Null);
  fViewMessages.Visible:=False;
  fRichEdit.Visible:=false;
  SuccessfulSended := False;
  tmpStrLst := TStringList.Create;
  pTos.Visible:=True;
  cbTo.Clear;
  if not (trim(AnswerMessage.FieldByName('REPLYTO').AsString) = '') then
    cbTo.Items.Add(AnswerMessage.FieldByName('REPLYTO').AsString);
  if not (trim(Answermessage.FieldByName('SENDER').AsString) = '') then
    cbTo.Items.Add(AnswerMessage.FieldByName('SENDER').AsString);
  cbTo.ItemIndex:=0;
  tmpStrLst.Text:=AnswerMessage.Content.FieldByName('CC').AsString;
  eCC.Text := '';
  for i := 0 to tmpStrLst.Count-1 do
    if i=0 then
      eCC.Text := eCC.Text+tmpStrLst[i]
    else
      eCC.Text := ';'+eCC.Text+tmpStrLst[i];
  tmpStrLst.Free;
  eSubject.Text := 'Re:'+AnswerMessage.FieldByName('SUBJECT').AsString;
  fRichEdit.Visible:=True;
  fRichEdit.Clear;
  sl := TStringList.Create;
  sl.Text := Answermessage.Content.AsString;
  for i := 0 to sl.Count-1 do
    if sl[i] <> '' then
      sl[i] := '>'+sl[i];
  i := 0;
  while i < sl.Count do
    if sl[i] = '' then
      sl.Delete(i)
    else
      inc(i);
  fRichEdit.AsText:=sl.Text;
  sl.Free;
  FDataSet.FieldByName('PARENT').AsVariant:=AnswerMessage.Number.AsVariant;
  AddSignature;
  MarkReadWrite;
  pcTabs.AddTabClass(TfDocumentFrame,strAttatchments,@AddDocuments);
  //TODO:correct
  sl := TStringList.Create;
  sl.Delimiter:=';';
  sl.DelimitedText:=StringReplace(AnswerMessage.Content.FieldByName('RECEIVERS').AsString,',',';',[rfReplaceAll]);
  for i := 0 to sl.Count-1 do
    if cbAccount.Items.IndexOf(sl[i]) <> -1 then
      begin
        cbAccount.Text:=sl[i];
        break;
      end;
  sl.Free;
  with Answermessage.DataSet do
    begin
      Edit;
      FieldByName('READ').AsString:='Y';
      Post;
    end;
//  if pcTabs.ActivePage = tsAttatchments then
//    frDocuments.Refresh(tmpMID,'N',Data.MessageIDX.FieldByName('ID').AsString,Null,Null);
  IsModified := False;
end;
function TfMessageEdit.ForwardMail(AnswerMessage: TMessage): Boolean;
begin
  Result := AnswerMail(AnswerMessage);
  eSubject.Text := 'Fwd:'+AnswerMessage.FieldByName('SUBJECT').AsString;
  cbTo.Clear;

end;
procedure TfMessageEdit.SendMailTo(receiver: string);
var
  i: Integer;
begin
  pcTabs.ActivePage:=tsContent;
  aRec := '';
  randomize;
  Data.StartTransaction(FConnection);
  FDataSet.Insert;
  SuccessfulSended := False;
  pTos.Visible:=True;
  cbTo.Clear;
  cbTo.Text := receiver;
  fViewMessages.Visible:=False;
  fRichEdit.Visible:=True;
  fRichEdit.Clear;
  eCC.Text:='';
  eSubject.Text := '';
  AddSignature;
  MarkReadWrite;
  pcTabs.AddTabClass(TfDocumentFrame,strAttatchments,@AddDocuments);
  IsModified := False;
  Show;
  if receiver <> '' then
    eSubject.SetFocus
  else cbTo.SetFocus;
end;
procedure TfMessageEdit.SendMailToWithDoc(Receiver: string; Subject: string;
  Msg: string; Document: string; DeleteFile: Boolean);
var
  i: Integer;
  aDocument : TDocument;
  aDocuments : TDocuments;
begin
  pcTabs.ActivePage:=tsContent;
  aRec := '';
  randomize;
  Data.StartTransaction(FConnection);
  FDataSet.Append;
  with FDataSet.DataSet do
    begin
      FieldByName('SQL_ID').AsVariant := Data.GetUniID;
      FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
      FieldByName('MSG_ID').AsVariant :=  FieldByName('SQL_ID').AsVariant;
      FieldByName('TYPE').AsString := 'EMAIL';
      FieldByName('READ').AsString := 'N';
      FieldByName('SENDER').AsString := cbAccount.Text;
      FieldByName('SENDDATE').AsDateTime := Now();
      FieldbyName('SUBJECT').AsString := eSubject.Text;
      FieldByName('TREEENTRY').AsVariant := TREE_ID_SEND_MESSAGES;
    end;
  FDataSet.DataSet.Post;
  FDataSet.DataSet.Edit;
  FDataSet.Content.Select(FDataSet.FieldByName('ID').AsString);
  FDataSet.Content.Open;
  FDataSet.Content.DataSet.Edit;
  FDataSet.Content.DataSet.Post;
  SuccessfulSended := False;
  cbTo.Clear;
  cbTo.Text := receiver;
  fViewMessages.Visible:=False;
  fRichEdit.Visible:=True;
  fRichEdit.Clear;
  eCC.Text:='';
  eSubject.Text := '';

  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(0);
  aDocument.Open;
  aDocument.Ref_ID:=FDataSet.Content.Id.AsVariant;
  aDocument.BaseID:=FDataSet.FieldByName('ID').AsString;
  aDocument.BaseTyp:='N';
  aDocument.BaseLanguage:=Null;
  aDocument.Baseversion:=Null;
  aDocument.ParentID:=0;
  aDocument.AddFromFile(Document,
                       '',
                       Now());
  aDocument.Free;

  pcTabs.AddTabClass(TfDocumentFrame,strAttatchments,@AddDocuments);
  aDocuments := TDocuments.CreateEx(Self,Data);
  aDocuments.Select(FDataSet.Content.Id.AsVariant,'N',DataSet.FieldByName('ID').AsString,Null,Null);
  aDocuments.Open;
  pcTabs.AddTab(TfDocumentFrame.Create(Self),False);
  TfDocumentFrame(pcTabs.GetTab(TfDocumentFrame).Controls[0]).DataSet := aDocuments;
  eSubject.Text := Subject;
  if Msg <> '' then
    fRichEdit.AsText := Msg;

  AddSignature;
  MarkReadWrite;
  IsModified := False;
  Show;

  {
  tmpMID := Data.DataModule.GetUniID;
  AddFileToDocuments(0,
                     tmpMID,
                     'N',
                     FDataSet.FieldByName('ID').AsString,
                     '',
                     Null,
                     copy(ExtractFileName(Document),0,rpos('.',ExtractFileName(Document))-1),
                            UniToSys(Document),
                            DeleteFile,
                            Now());
  frDocuments.Refresh(tmpMID,'N',FDataSet.FieldByName('ID').AsString,Null,Null);
  SuccessfulSended := False;
  cbTo.Clear;
  cbTo.Text := receiver;
  fViewMessages.Visible:=False;
  fRichEdit.Visible:=True;
  fRichEdit.Clear;
  eCC.Text:='';
  eSubject.Text := Subject;
  AddSignature;
  if Msg <> '' then
    fRichEdit.AsText := Msg;
  MarkReadWrite;
  IsModified := False;
  }
end;
procedure TfMessageEdit.OpenFromLink(aLink: string);
var
  aID: String;
begin
  aID := copy(aLink,pos('@',aLink)+1,length(aLink));
  if rpos('{',aID) > 0 then
    aID := copy(aID,0,rpos('{',aID)-1)
  else if rpos('(',aId) > 0 then
    aID := copy(aID,0,rpos('(',aID)-1);
  Data.SetFilter(DataSet,Data.QuoteField('ID')+'='+Data.QuoteValue(aID));
  if DataSet.Count > 0 then
    begin
      DataSet.Select(DataSet.Id.AsVariant);
      DataSet.Open;
      ShowMail;
    end
  else Destroy;
end;
procedure TfMessageEdit.MarkReadOnly;
begin
  cbTo.Enabled := False;
  eCC.Enabled := False;
  cbAccount.Enabled := False;
  eSubject.Enabled:=False;
  fRichEdit.ReadOnly:=True;
  bSend.Enabled := False;
  sbStatus.SimpleText:='';
  lMessage.Visible:=False;
end;
procedure TfMessageEdit.MarkReadWrite;
var
  tmp: String;
  tmp1: String;
begin
  cbTo.Enabled := True;
  eCC.Enabled := True;
  eSubject.Enabled:=True;
  fRichEdit.ReadOnly:=False;
  bSend.Enabled := True;
  cbAccount.Enabled:=True;
  cbAccount.Clear;
  fRichEdit.rmText.OnChange:=@MessageEditChange;
  with Application as IBaseDbInterface do
    tmp := DBConfig.ReadString('MAILACCOUNTS','');
  while pos('|',tmp) > 0 do
    begin  //Servertype;Server;Username;Password;Active
      if (copy(tmp,0,pos(';',tmp)-1) = 'SMTP')
      or (copy(tmp,0,pos(';',tmp)-1) = 'IMAP')
      then
        begin
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
          tmp1 := copy(tmp,0,pos(';',tmp)-1);
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
          tmp1 := copy(tmp,0,pos(';',tmp)-1);
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
          if copy(tmp,0,pos(';',tmp)-1) = 'YES' then
            cbAccount.Items.Add(tmp1);
        end;
      tmp := copy(tmp,pos('|',tmp)+1,length(tmp));
    end;
  cbAccount.ItemIndex:=0;
  sbStatus.SimpleText:='';
  eSubjectChange(nil);
  with DataSet.DataSet do
    begin
      FieldByName('SQL_ID').AsVariant := Data.GetUniID;
      FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
      FieldByName('MSG_ID').AsVariant := FieldByName('SQL_ID').AsVariant;
      FieldByName('TYPE').AsString := 'EMAIL';
      FieldByName('READ').AsString := 'Y';
      FieldByName('SENDER').AsString := cbAccount.Text;
      FieldByName('SENDDATE').AsDateTime := Now();
      FieldbyName('SUBJECT').AsString := eSubject.Text;
      FieldByName('TREEENTRY').AsVariant := TREE_ID_SEND_MESSAGES;
    end;
end;
procedure TfMessageEdit.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
  aItem: TMenuItem;
  aOrderType: TOrderTyp;
  bItem: TMenuItem;
begin
  if DataSet.State = dsInsert then
    begin
      DataSet.DataSet.Post;
      DataSet.DataSet.Edit;
    end;
  if not DataSet.Content.DataSet.Active then
    begin
      DataSet.Content.Select(FDataSet.FieldByName('ID').AsString);
      DataSet.Content.Open;
      if DataSet.Content.Count = 0 then
        begin
          DataSet.Content.DataSet.Edit;
          DataSet.Content.DataSet.Post;
        end;
    end;
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
//      aDocuments.Select(DataSet.Id.AsVariant,'N',DataSet.FieldByName('ID').AsString,Null,Null);
//      aDocuments.Open;
      TfDocumentFrame(Sender).Refresh(DataSet.Content.Id.AsVariant,'N',DataSet.FieldByName('ID').AsString,Null,Null);
    end;
  FDocumentFrame := TfDocumentFrame(Sender);
  aItem := TMenuItem.Create(TfDocumentFrame(Sender).pmDocumentAction);
  aItem.Caption:=strNewVoucher;
  TfDocumentFrame(Sender).pmDocumentAction.Items.Add(aItem);
  aOrderType := TOrderTyp.CreateEx(Self,Data);
  aOrderType.Open;
  Data.SetFilter(aOrderType,'('+Data.QuoteField('SI_ORDER')+' = ''Y'')');
  aOrderType.DataSet.First;
  while not aOrderType.DataSet.EOF do
    begin
      bItem := TMenuItem.Create(aItem);
      bItem.Caption:=strNewOrder+aOrderType.FieldByName('STATUSNAME').AsString;
      bItem.OnClick:=@acAddAsOrderExecute;
      aItem.Add(bItem);
      aOrderType.DataSet.Next;
    end;
  aOrderType.Free;
end;
procedure TfMessageEdit.EnterCB(Data: PtrInt);
begin
  TCombobox(Data).SelLength:=0;
  TCombobox(Data).SelStart:=length(TCombobox(Data).Text);
end;

procedure TfMessageEdit.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='MESSAGE';
  TfHistoryFrame(Sender).DataSet := DataSet.History;
  TPrometInplaceFrame(Sender).SetRights(True);
end;

initialization
end.

