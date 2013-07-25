{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uTaskEdit;
interface
uses
  LMessages, LCLProc, LCLType, LCLIntf, LResources, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ZVDateTimePicker,
  DBZVDateTimePicker, utask, uExtControls, Buttons, EditBtn, ButtonPanel, Spin,
  DbCtrls, db, uIntfStrConsts,DBGrids,Grids, ActnList,uHistoryFrame;
type

  { TfTaskEdit }

  TfTaskEdit = class(TForm)
    acPasteLink: TAction;
    acSave: TAction;
    acAbort: TAction;
    ActionList1: TActionList;
    Bevel10: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    cbChecked: TDBCheckBox;
    cbChecked1: TDBCheckBox;
    cbClass: TExtDBCombobox;
    cbCompleted: TDBCheckBox;
    cbCompleted1: TDBCheckBox;
    cbPriority: TDBComboBox;
    cbState: TExtDBCombobox;
    cbCategory: TDBComboBox;
    EarlystDate: TDBZVDateTimePicker;
    EndTimeLbl2: TLabel;
    eTime: TEdit;
    EndDate1: TDBZVDateTimePicker;
    EndTimeLbl1: TLabel;
    eBuffer: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Panel11: TPanel;
    pHist: TPanel;
    Splitter1: TSplitter;
    Task: TDatasource;
    eSummary: TDBEdit;
    EndDate: TDBZVDateTimePicker;
    eProject: TDBEdit;
    EndTimeLbl: TLabel;
    pcPages: TExtMenuPageControl;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mNotes: TDBMemo;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    lMessage: TLabel;
    FileDialog: TOpenDialog;
    Panel8: TPanel;
    Panel9: TPanel;
    pbStatus: TProgressBar;
    pgEvent: TPageControl;
    StartDate: TDBZVDateTimePicker;
    StartTimeLbl: TLabel;
    tabEvent: TTabSheet;
    ToolButton1: TBitBtn;
    ToolButton2: TBitBtn;
    tsNotes: TTabSheet;
    ToolBar1: TPanel;
    ToolBar2: TPanel;
    procedure acAbortExecute(Sender: TObject);
    procedure acPasteLinkExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure cbStateSelect(Sender: TObject);
    procedure eBufferExit(Sender: TObject);
    procedure eTimeExit(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TfListFrameFListgListDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure TfListFrameFListViewDetails(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private { Private declarations }
    FDataSet : TTask;
    aConnection: TComponent;
    FEditable : Boolean;
    FHistoryFrame: TfHistoryFrame;
    OwnConnection: Boolean;
    procedure DoOpen;
    procedure AddDocuments(Sender: TObject);
    procedure AddLinks(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure AddList(Sender: TObject);
  public { Public declarations }
    constructor Create(TheOwner: TComponent;TheConnection : TComponent = nil);
    destructor Destroy; override;
    function Execute(aLink : string) : Boolean;
    procedure SetLanguage;
  end;
implementation
uses uData,uDocumentFrame,uDocuments,uLinkFrame,uprometframesinplace,
  uListFrame,uBaseVisualControls,ClipBrd,uBaseVisualApplication,
  uError,utasks;
resourcestring
  strDependencies               = 'Abhängigkeiten';
  strClassTask                  = 'T Aufgabe';
  strClassMilestone             = 'M Meilenstein';
procedure TfTaskEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfTaskEdit.FormShow(Sender: TObject);
begin
  FHistoryFrame.DataSet := TTask(FDataSet).History;
  FHistoryFrame.SetRights(FEditable);
end;
procedure TfTaskEdit.acPasteLinkExecute(Sender: TObject);
var
  Stream: TStringStream;
  aLinks: String;
  aLink: String;
begin
  Stream := TStringStream.Create('');
  if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
    begin
      Stream.Position:=0;
      aLinks := Stream.DataString;
    end
  else
    fError.ShowWarning(strCantgetClipboardContents);
  Stream.Free;
  while pos(';',aLinks) > 0 do
    begin
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
      FDataSet.Dependencies.Add(aLink);
      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
    end;
end;
procedure TfTaskEdit.acAbortExecute(Sender: TObject);
begin
  if Assigned(aConnection) then
    begin
      FDataSet.CascadicCancel;
    end;
  Close;
end;
procedure TfTaskEdit.acSaveExecute(Sender: TObject);
begin
  if Assigned(aConnection) then
    begin
      FDataSet.CascadicPost;
    end;
  Close;
end;

procedure TfTaskEdit.cbStateSelect(Sender: TObject);
  procedure RecurseStatus(aId : Variant);
  var
    aTasks: TTaskList;
  begin
    if (aId = Null) or (aId = 0) then exit;
    aTasks := TTaskList.Create(Self,Data,aConnection);
    aTasks.SelectByParent(aId);
    aTasks.Open;
    with aTasks.DataSet do
      begin
        First;
        while not EOF do
          begin
            if aTasks.FieldByName('WORKSTATUS').AsString <> cbState.Text then
              begin
//                RecurseStatus(aTasks.Id.AsVariant);
                aTasks.DataSet.Edit;
                aTasks.FieldByName('WORKSTATUS').AsString := FDataSet.FieldByName('WORKSTATUS').AsString;
                aTasks.DataSet.Post;
              end;
            Next;
          end;
      end;
    aTasks.Free;
  end;
begin
  RecurseStatus(FDataSet.Id.AsVariant);
end;

procedure TfTaskEdit.eBufferExit(Sender: TObject);
begin
  if not FDataSet.CanEdit then
    FDataSet.DataSet.Edit;
  if eBuffer.Text<>'' then
    FDataSet.FieldByName('BUFFERTIME').AsString:=FloatToStr(StrToDayTime(eBuffer.Text))
  else FDataSet.FieldByName('BUFFERTIME').Clear;
end;

procedure TfTaskEdit.eTimeExit(Sender: TObject);
begin
  if not FDataSet.CanEdit then
    FDataSet.DataSet.Edit;
  if eTime.Text<>'' then
    FDataSet.FieldByName('PLANTIME').AsString:=FloatToStr(StrToDayTime(eTime.Text))
  else FDataSet.FieldByName('PLANTIME').Clear;
end;

procedure TfTaskEdit.TfListFrameFListgListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  inherited;
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if Column.FieldName = 'ICON' then
        begin
          if not Column.Field.IsNull then
            fVisualControls.Images.Draw(TDBGrid(Sender).Canvas,Rect.Left+1,Rect.Top+1,Column.Field.AsInteger)
        end
      else
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
      end;
end;
procedure TfTaskEdit.TfListFrameFListViewDetails(Sender: TObject);
begin
  Data.GotoLink(TTask(FDataSet).Dependencies.FieldByName('LINK').AsString);
end;
procedure TfTaskEdit.ToolButton1Click(Sender: TObject);
begin
  Close;
end;
procedure TfTaskEdit.DoOpen;
var
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  nF: Extended;
  aType: Char;
begin
  cbCategory.Items.Clear;
  aType := 'T';
  Data.Categories.CreateTable;
  Data.SetFilter(Data.Categories,Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType));
  Data.Categories.First;
  while not Data.Categories.EOF do
    begin
      if Data.Categories.FieldByName('ACTIVE').AsString<>'N' then
        cbCategory.Items.Add(Data.Categories.FieldByName('NAME').AsString);
      Data.Categories.DataSet.Next;
    end;

  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(FDataSet.Id.AsInteger,'T',0);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          pcPages.AddTab(aDocFrame,False);
          aDocFrame.DataSet := aDocuments;
        end;
    end;
  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TTask(FDataSet).Links.Open;
  if TTask(FDataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
  {
  pcPages.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  TTask(FDataSet).History.Open;
  if TTask(FDataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  }
  pcPages.AddTabClass(TfListFrame,strDependencies,@AddList);
  TTask(FDataSet).Dependencies.Open;
  if TTask(FDataSet).Dependencies.Count > 0 then
    pcPages.AddTab(TfListFrame.Create(nil),False,strDependencies);
  TTask(FDataSet).History.Open;
  FHistoryFrame.BaseName:='TASK';
  if TryStrToFloat(FDataSet.FieldByName('PLANTIME').AsString,nF) then
    eTime.Text := DayTimeToStr(nF)
  else
    eTime.Text := FDataSet.FieldByName('PLANTIME').AsString;
  if TryStrToFloat(FDataSet.FieldByName('BUFFERTIME').AsString,nF) then
    eBuffer.Text := DayTimeToStr(nF)
  else
    eBuffer.Text := FDataSet.FieldByName('BUFFERTIME').AsString;
end;
procedure TfTaskEdit.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(FDataSet.Id.AsInteger,'T',0);
    end;
  TfDocumentFrame(Sender).BaseElement:=FDataSet;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfTaskEdit.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='T';
  TfLinkFrame(Sender).DataSet := TTask(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfTaskEdit.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='TASK';
  TfHistoryFrame(Sender).DataSet := TTask(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfTaskEdit.AddList(Sender: TObject);
begin
  if TfListFrame(Sender).TabCaption = strDependencies then
    begin
      with TfListFrame(Sender) do
        begin
          FList.FilterType:='TSKDEP';
          FList.DefaultRows:='GLOBALWIDTH:200;ICON:25;NAME:175;';
          FList.DestroyDataSet:=False;
          TTask(FDataSet).Dependencies.Open;
          FList.DataSet := TTask(FDataSet).Dependencies;
          FList.pTop.Visible:=False;
          FList.gList.OnDrawColumnCell:=@TfListFrameFListgListDrawColumnCell;
          fList.gList.ReadOnly:=True;
          FList.AddContextAction(acPasteLink);
          DataSource.DataSet := TTask(FDataSet).Dependencies.DataSet;
          FList.Editable:=False;
          FList.OnViewDetails:=@TfListFrameFListViewDetails;
        end;
    end
end;
constructor TfTaskEdit.Create(TheOwner: TComponent;TheConnection : TComponent = nil);
begin
  inherited Create(TheOwner);
  FEditable:=True;
  OwnConnection := (TheConnection = nil);
  if TheConnection = nil then
    begin
      aConnection := Data.GetNewConnection
    end
  else aConnection := TheConnection;
  FDataSet := TTask.Create(nil,Data,aConnection);
  FDataSet.CreateTable;
  FHistoryFrame := TfHistoryFrame.Create(Self);
  FHistoryFrame.Parent := pHist;
  FHistoryFrame.Align:=alClient;
end;
destructor TfTaskEdit.Destroy;
begin
  FreeAndNil(FDataSet);
  if OwnConnection then FreeAndNil(aConnection);
  inherited Destroy;
end;
function TfTaskEdit.Execute(aLink: string): Boolean;
var
  ActControl: TWinControl;
  nF: Extended;
begin
  ActControl := Screen.ActiveControl;
  SetLanguage;
  if not OwnConnection then
    Data.StartTransaction(aConnection);
  FDataSet.SelectFromLink(aLink);
  FDataSet.CreateTable;
  FDataSet.Open;
  if FDataSet.Count > 0 then
    begin
      Task.DataSet := FDataSet.DataSet;
      DoOpen;
      Show;
      if FDataSet.FieldByName('SUMMARY').IsNull then
        eSummary.SetFocus
      else mNotes.SetFocus;
      while Visible do
        Application.ProcessMessages;
      Result := ModalResult = mrOk;
      if not Assigned(FDataSet) then exit;
      if (ModalResult = mrNone) and FDataSet.Changed then
        if (MessageDlg(strItem+' '+FDataSet.Text.AsString,strItemnotSaved,mtInformation,[mbYes,mbNo],0) = mrYes) then
          Result := True;
      if Assigned(FDataSet) then
        begin
          if Result then
            begin
              FDataSet.CascadicPost;
              if not OwnConnection then
                Data.CommitTransaction(aConnection)
            end
          else
            begin
              FDataSet.CascadicCancel;
              if not OwnConnection then
                Data.RollbackTransaction(aConnection);
            end;
        end;
    end;
end;

procedure TfTaskEdit.SetLanguage;
begin
  cbClass.Clear;
  cbClass.Items.Add(strClassTask);
  cbClass.Items.Add(strClassMilestone);
  Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue('P'));
  cbState.Clear;
  with Data.States.DataSet do
    begin
      First;
      while not eof do
        begin
          cbState.Items.Add(Format('%-4s%s',[FieldByName('STATUS').AsString,FieldByName('STATUSNAME').AsString]));
          next;
        end;
    end;
end;

initialization
{$R *.lfm}
end.
 
