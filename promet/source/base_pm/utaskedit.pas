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
unit uTaskEdit;
interface
uses
  LMessages, LCLProc, LCLType, LCLIntf,  SysUtils, Classes, math, Graphics,
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
    bDelegated2: TSpeedButton;
    Bevel10: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    Button1: TButton;
    cbCategory: TDBComboBox;
    cbChecked: TDBCheckBox;
    cbChecked1: TDBCheckBox;
    cbClass: TExtDBCombobox;
    cbCompleted: TDBCheckBox;
    cbCompleted1: TDBCheckBox;
    cbPriority: TDBComboBox;
    cbState: TExtDBCombobox;
    DBText1: TDBText;
    EarlystDate: TDBZVDateTimePicker;
    EarlystDate1: TDBZVDateTimePicker;
    eBuffer: TEdit;
    EndTimeLbl3: TLabel;
    EndTimeLbl4: TLabel;
    EndTimeLbl5: TLabel;
    eTime: TEdit;
    eUser: TEditButton;
    eOwner: TEditButton;
    EndDate: TDBZVDateTimePicker;
    EndDate1: TDBZVDateTimePicker;
    EndTimeLbl: TLabel;
    EndTimeLbl1: TLabel;
    EndTimeLbl2: TLabel;
    eOrder: TDBEdit;
    eProject: TEditButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mNotes: TDBMemo;
    Panel1: TPanel;
    Panel11: TPanel;
    pHist: TPanel;
    Splitter1: TSplitter;
    StartDate: TDBZVDateTimePicker;
    StartTimeLbl: TLabel;
    tsAdditional: TTabSheet;
    Task: TDatasource;
    eSummary: TDBEdit;
    pcPages: TExtMenuPageControl;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    lMessage: TLabel;
    FileDialog: TOpenDialog;
    Panel8: TPanel;
    pgEvent: TPageControl;
    tabEvent: TTabSheet;
    ToolButton1: TBitBtn;
    ToolButton2: TSpeedButton;
    tsNotes: TTabSheet;
    ToolBar1: TPanel;
    procedure acAbortExecute(Sender: TObject);
    procedure acPasteLinkExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure bDelegated2Click(Sender: TObject);
    procedure bSetUserClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbStateSelect(Sender: TObject);
    procedure eBufferExit(Sender: TObject);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eTimeExit(Sender: TObject);
    procedure eUserButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchOpenOwnerItem(aLink: string): Boolean;
    function fSearchOpenUserItem(aLink: string): Boolean;
    procedure TfHistoryFrameAddUserMessage(Sender: TObject);
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
  uError,utasks,uSearch,uBaseDbClasses,uhistoryadditem,uBaseDBInterface,
  uProjects;
resourcestring
  strDependencies               = 'Abhängigkeiten';
  strClassTask                  = 'T Aufgabe';
  strClassMilestone             = 'M Meilenstein';
  strClassAP                    = 'P Arbeitspaket';
  strEditTask                   = 'Aufgabe bearbeiten';
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

function TfTaskEdit.fSearchOpenItem(aLink: string): Boolean;
var
  aParent: TProject;
begin
  aParent := TProject.Create(nil);
  aParent.SelectFromLink(aLink);
  aParent.Open;
  if aParent.Count>0 then
    begin
      FDataSet.Edit;
      FDataSet.FieldByName('PROJECT').AsString:=Data.GetLinkDesc(aLink);
      eProject.Text:=Data.GetLinkDesc(aLink);
      FDataSet.FieldByName('PROJECTID').AsVariant:=aParent.Id.AsVariant;
    end;
  aParent.free;
end;

function TfTaskEdit.fSearchOpenOwnerItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
begin
  Result := False;
  aUser := TUser.CreateEx(Self,Data);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  Result := aUser.Count>0;
  if Result then
    begin
      if not FDataSet.CanEdit then
        FDataSet.DataSet.Edit;
      FDataSet.FieldByName('OWNER').AsString := aUser.FieldByName('ACCOUNTNO').AsString;
      eOwner.Text:=aUser.Text.AsString;
    end
  else
    begin
      if not FDataSet.CanEdit then
        FDataSet.DataSet.Edit;
      FDataSet.FieldByName('OWNER').Clear;
      eOwner.Text:='';
    end;
  aUSer.Free;
end;

function TfTaskEdit.fSearchOpenUserItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
begin
  Result := False;
  aUser := TUser.CreateEx(Self,Data);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  Result := aUser.Count>0;
  if Result then
    begin
      if not FDataSet.CanEdit then
        FDataSet.DataSet.Edit;
      FDataSet.FieldByName('USER').AsString := aUser.FieldByName('ACCOUNTNO').AsString;
      eUser.Text:=aUser.Text.AsString;
    end
  else
    begin
      if not FDataSet.CanEdit then
        FDataSet.DataSet.Edit;
      FDataSet.FieldByName('USER').Clear;
      eUser.Text:='';
    end;
  aUSer.Free;
end;

procedure TfTaskEdit.TfHistoryFrameAddUserMessage(Sender: TObject);
var
  aOwner: String;
  aUser: TUser;
begin
  aUser := TUser.Create(nil);
  aUser.SelectByAccountno(FDataSet.FieldByName('OWNER').AsString);
  aUser.Open;
  if aUser.Count>0 then
    begin
      aUser.History.AddItem(FDataSet.DataSet,TfHistoryAddItem(Sender).eAction.Text,Data.BuildLink(FDataSet.DataSet),TfHistoryAddItem(Sender).eReference.Text,FDataSet.DataSet,10,'',True,True);
    end;
  aUser.Free;
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

procedure TfTaskEdit.bDelegated2Click(Sender: TObject);
var
  aProject: TProject;
begin
  aProject := TProject.Create(nil);
  aProject.Select(FDataSet.FieldByName('PROJECTID').AsVariant);
  aProject.Open;
  if aProject.Count > 0 then
    Data.GotoLink(Data.BuildLink(aProject.DataSet));
  aProject.Free;
end;

procedure TfTaskEdit.bSetUserClick(Sender: TObject);
begin

end;

procedure TfTaskEdit.Button1Click(Sender: TObject);
var
  aColTime: float;
begin
  with FDataSet do
    begin
      aColTime := GetTimesForTask;
      if aColTime>0 then
        begin
          Edit;
          FieldByName('TIME').AsFloat:=aColTime;
        end;
    end;
end;

procedure TfTaskEdit.cbStateSelect(Sender: TObject);
  procedure RecurseStatus(aId : Variant);
  var
    aTasks: TTaskList;
  begin
    if (aId = Null) or (aId = 0) then exit;
    aTasks := TTaskList.CreateEx(Self,Data,aConnection);
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

procedure TfTaskEdit.eProjectButtonClick(Sender: TObject);
var
  i : Integer = 0;
begin
  fSearch.AllowSearchTypes(strProjects);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'TASKSP',strSearchFromProjects);
  fSearch.SetLanguage;
end;

procedure TfTaskEdit.eTimeExit(Sender: TObject);
begin
  if not FDataSet.CanEdit then
    FDataSet.DataSet.Edit;
  if eTime.Text<>'' then
    FDataSet.FieldByName('PLANTIME').AsString:=FloatToStr(StrToDayTime(eTime.Text))
  else FDataSet.FieldByName('PLANTIME').Clear;
end;

procedure TfTaskEdit.eUserButtonClick(Sender: TObject);
var
  i :Integer = 0;
begin
  fSearch.AllowSearchTypes(strUsers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  if Sender = eUser then
    fSearch.OnOpenItem:=@fSearchOpenUserItem
  else
    fSearch.OnOpenItem:=@fSearchOpenOwnerItem;
  fSearch.Execute(True,'TASKSU',strSearchFromTasks);
  fSearch.SetLanguage;
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
  OldPageIdx: Integer;
  aUser: TUser;
begin
  cbCategory.Items.Clear;
  aType := 'T';
  Data.Categories.CreateTable;
  Data.Categories.Open;
  Data.Categories.DataSet.Filter:=Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType);
  Data.Categories.DataSet.Filtered:=True;
  Data.Categories.First;
  while not Data.Categories.EOF do
    begin
      if Data.Categories.FieldByName('ACTIVE').AsString<>'N' then
        cbCategory.Items.Add(Data.Categories.FieldByName('NAME').AsString);
      Data.Categories.DataSet.Next;
    end;
  OldPageIdx := pcPages.ActivePageIndex;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
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
    pcPages.AddTab(TfListFrame.Create(Self),False,strDependencies);
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
  if OldPageIdx<pcPages.PageCount-1 then
    pcPages.PageIndex:=OldPageIdx;
  aUser := TUser.Create(nil);
  aUser.SelectByAccountno(FDataSet.FieldByName('USER').AsString);
  aUser.Open;
  eUser.Text:=aUser.FieldByName('NAME').AsString;

  aUser.SelectByAccountno(FDataSet.FieldByName('OWNER').AsString);
  aUser.Open;
  eOwner.Text:=aUser.FieldByName('NAME').AsString;
  aUser.Free;
end;
procedure TfTaskEdit.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
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
  TfHistoryFrame(Sender).OnAddUserMessage:=@TfHistoryFrameAddUserMessage;
end;
procedure TfTaskEdit.AddList(Sender: TObject);
begin
  if TfListFrame(Sender).TabCaption = strDependencies then
    begin
      with TfListFrame(Sender) do
        begin
          FList.FilterType:='TSKDEP';
          FList.DefaultRows:='GLOBALWIDTH:%;ICON:25;NAME:175;';
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
  FDataSet := TTask.CreateEx(nil,Data,aConnection);
  FDataSet.CreateTable;
  FHistoryFrame := TfHistoryFrame.Create(Self);
  FHistoryFrame.Parent := pHist;
  FHistoryFrame.Align:=alClient;
  FHistoryFrame.OnAddUserMessage:=@TfHistoryFrameAddUserMessage;
end;
destructor TfTaskEdit.Destroy;
begin
  try
    FreeAndNil(FDataSet);
    if OwnConnection then FreeAndNil(aConnection);
  except
  end;
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
      Caption:=strEditTask+' - '+FDataSet.FieldByName('SUMMARY').AsString;
      eProject.Text:=FDataSet.FieldByName('PROJECT').AsString;
      while Visible do
        begin
          Application.ProcessMessages;
          sleep(100);
        end;
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
  cbClass.Items.Add(strClassAP);
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
 
