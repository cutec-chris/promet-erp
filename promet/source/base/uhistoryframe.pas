{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uHistoryFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, ExtCtrls,
  uPrometFramesInplaceDB, uExtControls, uBaseDbClasses, uFilterFrame, Grids, DBGrids,
  Buttons, ActnList, ugridview,Clipbrd;
type

  { TfHistoryFrame }

  TfHistoryFrame = class(TPrometInplaceDBFrame)
    acAdd: TAction;
    acDelete: TAction;
    acAddLinked: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    bRefresh1: TSpeedButton;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    HistoryImages: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pButtons: TPanel;
    pCont: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure aButtonClick(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acAddLinkedExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure bRefresh1Click(Sender: TObject);
    function FContListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState) : Boolean;
    procedure FContListViewDetails(Sender: TObject);
  private
    fBaseName: string;
    { private declarations }
//    FContList: TfFilter;
    FGridView : TfGridView;
    procedure SetBaseName(const AValue: string);
    procedure RestoreButtons;
  protected
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    property BaseName : string read fBaseName write SetBaseName;
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetRights(Editable : Boolean);override;
    procedure ShowFrame; override;
  end;
implementation
uses uBaseVisualControls,Graphics,uData,uBaseDbInterface,uHistoryAddItem,
  uBaseERPDBClasses,uBaseVisualApplication;
{$R *.lfm}
resourcestring
  strHistory0                              = 'bearbeitet';
  strHistory1                              = 'neue Nachricht empfangen';
  strHistory2                              = 'Nachricht verschickt';
  strHistory3                              = 'Anruf';
  strHistory4                              = 'neuer Vorgang';
  strHistory5                              = '';
  strHistory6                              = 'Vorgang gewandelt';
  strHistory7                              = 'neues Dokument';
  strHistory8                              = 'Vermerk von Benutzer';
  strHistory9                              = 'Aufgabe erledigt';
  strHistory10                             = 'Aufgabe hinzugefügt';
  strHistory11                             = 'Termin geändert';
  strHistory12                             = 'umbenannt';
function TfHistoryFrame.FContListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState) : Boolean;
begin
  if not Assigned(Self) then exit;
  with (Sender as TCustomGrid), Canvas do
    begin
      Result := True;
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if (Column.FieldName = 'ACTIONICON') and Assigned(Column.Field) then
        begin
          if not Column.Field.IsNull then
            HistoryImages.Draw(Canvas,Rect.Left,Rect.Top,StrToIntDef(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol],-1));
        end
      else
        Result := False;
    end;
end;
procedure TfHistoryFrame.acAddExecute(Sender: TObject);
begin
  if fHistoryAddItem.Execute then
    begin
      TBaseHistory(DataSet).AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,'',fHistoryAddItem.eReference.Text,nil,ACICON_USEREDITED,'',True,True);
      FGridView.Refresh;
    end;
end;
procedure TfHistoryFrame.aButtonClick(Sender: TObject);
var
  aFilter: String;
  i: Integer;
  s: String;
begin
  with DataSet.DataSet as IBaseDbFilter do
    aFilter := Data.ProcessTerm(Data.QuoteField('ACTIONICON')+'='+Data.QuoteValue(''));
  for i := 0 to pButtons.ComponentCount-1 do
    if TSpeedButton(pButtons.Components[i]).Down then
    aFilter += ' OR '+Data.QuoteField('ACTIONICON')+'='+Data.QuoteValue(IntToStr(TSpeedButton(pButtons.Components[i]).Tag));
  FGridView.BaseFilter:='('+aFilter+')';
  s := '';
  for i := 0 to pButtons.ComponentCount-1 do
    if TSpeedButton(pButtons.Components[i]).Down then
      s := s+IntToStr(TSpeedButton(pButtons.Components[i]).Tag)+',';
  with Application as IBaseDbInterface do
    DBConfig.WriteString('HISTB:'+fBaseName,s);
end;
procedure TfHistoryFrame.acAddLinkedExecute(Sender: TObject);
var
  Stream: TStringStream;
  aLink: String = '';
begin
  if fHistoryAddItem.Execute then
    begin
      Stream := TStringStream.Create('');
      if (pos('://',ClipBoard.AsText) > 0) then
        begin
          aLink := ClipBoard.AsText;
        end
      else if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          Stream.Position:=0;
          aLink := Stream.DataString;
        end;
      if pos(';',aLink) > 0 then
        aLink := copy(aLink,0,pos(';',aLink)-1);
      Stream.Free;
      TBaseHistory(DataSet).AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,aLink,fHistoryAddItem.eReference.Text,nil,ACICON_USEREDITED,'',True,True);
      FGridView.Refresh;
    end;
end;
procedure TfHistoryFrame.acDeleteExecute(Sender: TObject);
begin
  FGridView.Delete;
end;

procedure TfHistoryFrame.bRefresh1Click(Sender: TObject);
begin
  FGridView.Refresh(True);
end;

procedure TfHistoryFrame.FContListViewDetails(Sender: TObject);
begin
  if FGridView.GotoActiveRow then
    begin
      if DataSet.FieldByName('LINK').AsString='' then
        begin
          DataSet.Prior;
          if FDataSet.DataSet.BOF and (FDataSet.FieldByName('CHANGEDBY').AsString = Data.Users.DataSet.FieldByName('IDCODE').AsString) then
            begin
              if fHistoryAddItem.Execute(FDataSet) then
                begin
                  if not FDataSet.CanEdit then
                    FDataSet.DataSet.Edit;
                  FDataSet.DataSet.FieldByName('ACTION').AsString:=fHistoryAddItem.eAction.Text;
                  if FDataSet.CanEdit then
                    FDataSet.DataSet.Post;
                end;
            end;
        end
      else
        Data.GotoLink(DataSet.FieldByName('LINK').AsString);
    end;
end;
procedure TfHistoryFrame.SetBaseName(const AValue: string);
var
  TopVisible: Boolean;
begin
  if fBaseName=AValue then exit;
  fBaseName:=AValue;
//  TopVisible := FContList.pTop.Visible;
  FGridView.BaseName:='PHIST'+AValue;
//  FContList.pTop.Visible := TopVisible;
  RestoreButtons;
end;

type
  TDefaultButtons = 1..30;
procedure TfHistoryFrame.RestoreButtons;
var
  i: Integer;
  aButton: TSpeedButton;
  DefaultButtons : set of TDefaultButtons = [1,2,3,4,5,6,7,8,9,11];
  s: String;
begin
  with Application as IBaseDbInterface do
    s := DBConfig.ReadString('HISTB:'+fBaseName,'');
  while pButtons.ComponentCount>0 do
    pButtons.Components[0].Free;
  for i := 0 to HistoryImages.Count-1 do
    begin
      aButton := TSpeedButton.Create(pButtons);
      aButton.Glyph := nil;
      HistoryImages.GetBitmap(i,AButton.Glyph);
      aButton.Tag:=i;
      aButton.GroupIndex:=100+i;
      aButton.AllowAllUp:=True;
      if (i in DefaultButtons) and (s='') then
        aButton.Down:=True;
      if s <> '' then
        aButton.Down := pos(IntToStr(i)+',',s)>0;
      aButton.OnClick:=@aButtonClick;
      aButton.Parent:=pButtons;
      aButton.Align:=alTop;
      case i of
      0:aButton.Hint:=strHistory0;
      1:aButton.Hint:=strHistory1;
      2:aButton.Hint:=strHistory2;
      3:aButton.Hint:=strHistory3;
      4:aButton.Hint:=strHistory4;
      5:aButton.Hint:=strHistory5;
      6:aButton.Hint:=strHistory6;
      7:aButton.Hint:=strHistory7;
      8:aButton.Hint:=strHistory8;
      9:aButton.Hint:=strHistory9;
      10:aButton.Hint:=strHistory10;
      11:aButton.Hint:=strHistory11;
      12:aButton.Hint:=strHistory12;
      end;
    end;
end;

constructor TfHistoryFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGridView := TfGridView.Create(Self);
  with FGridView do
    begin
      BaseName:='PHIST';
      DefaultRows:='GLOBALWIDTH:530;ACTIONICON:30;ACTION:200;REFERENCE:100;COMMISSION:100;TIMESTAMPD:100;';
      Parent := pCont;
      Align := alClient;
      SortDirection:=sdDescending;
      SortField:='TIMESTAMPD';
      TextField:='ACTION';
      ReadOnly:=True;
      Show;
    end;
  FGridView.OnDrawColumnCell:=@FContListDrawColumnCell;
  FGridView.OnDblClick:=@FContListViewDetails;
  Panel3.Visible:=Data.Users.Rights.Right('HISTORY') > RIGHT_WRITE;
  RestoreButtons;
end;
destructor TfHistoryFrame.Destroy;
begin
  if Assigned(FGridView) then
    begin
      FGridView.DataSet := nil;
      FGridView.Free;
      FGridView := nil;
    end;
  DataSet := nil;
  inherited Destroy;
end;
procedure TfHistoryFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  if not Assigned(FGridView) then exit;
  FGridView.DataSet := AValue;
  aButtonClick(nil);
//  FGridView.acFilter.Execute;
end;
procedure TfHistoryFrame.SetRights(Editable : Boolean);
begin
end;
procedure TfHistoryFrame.ShowFrame;
begin
  inherited ShowFrame;
  FGridView.Refresh(True);
  FGridView.SetActive;
end;

end.

