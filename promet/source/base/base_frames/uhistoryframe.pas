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
*******************************************************************************}
unit uHistoryFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, ExtCtrls,
  uPrometFramesInplaceDB, uExtControls, db, uBaseDbClasses, uFilterFrame, Grids,
  DBGrids, Buttons, ActnList, ugridview, Clipbrd, Graphics;
type

  { TfHistoryFrame }

  TfHistoryFrame = class(TPrometInplaceDBFrame)
    acAdd: TAction;
    acDelete: TAction;
    acAddLinked: TAction;
    acIgnore: TAction;
    acRefresh: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    bRefresh1: TSpeedButton;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    pToolbar: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pButtons: TPanel;
    pCont: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    procedure aButtonClick(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acAddLinkedExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acIgnoreExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    function FContListDrawColumnCell(Sender: TObject; const aRect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState) : Boolean;
    procedure FContListViewDetails(Sender: TObject);
    procedure FTimeLineGetCellText(Sender: TObject; aCol: TColumn;
      aRow: Integer; var NewText: string; aFont: TFont);
  private
    fBaseName: string;
    { private declarations }
//    FContList: TfFilter;
    FTimeLine : TfGridView;
    FOnAddUserMessage: TNotifyEvent;
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
    property OnAddUserMessage : TNotifyEvent read FOnAddUserMessage write FOnAddUserMessage;
  end;
  { TMGridObject }

  TMGridObject = class(TObject)
  public
    Caption : string;
    Bold : Boolean;
    Text : string;
    TextColor : tColor;
    FontSize : Integer;
    HasAttachment : Boolean;
    IsThreaded : Boolean;
    Image : TBitmap;
    constructor Create;
    destructor Destroy; override;
  end;
const
  DrawImageWidth = 100;
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
  aTextStyleW : TTextStyle = (Alignment:taLeftJustify;
                             Layout : tlTop;
                             SingleLine : False;
                             Clipping  : True;
                             ExpandTabs:False;
                             ShowPrefix:False;
                             Wordbreak:True;
                             Opaque:True;
                             SystemFont:False;
                             RightToLeft:False);
implementation
uses uBaseVisualControls,uData,uBaseDbInterface,uHistoryAddItem,
  uBaseERPDBClasses,uBaseVisualApplication,wikitohtml,uColors;
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
  strHistory13                             = 'aus Office geändert';
  strHistory14                             = 'extern geändert';
const HistryPrio : array[0..14] of byte = (1,4,3,5,2,1,4,5,7,3,2,4,1,2,5);
constructor TMGridObject.Create;
begin
  Bold := False;
  TextColor:=clWindowText;
  Image := nil;
  FontSize:=0;
end;
destructor TMGridObject.Destroy;
begin
  try
    if Assigned(Image) then
      Image.Free;
  except
  end;
  inherited Destroy;
end;
function TfHistoryFrame.FContListDrawColumnCell(Sender: TObject;
  const aRect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState
  ): Boolean;
var
  aColor: TColor;
  aMiddle: Integer;
  aHeight: Integer;
  aRRect: TRect;
  aObj: TObject;
  bRect: TRect;
  atext: String;
  cRect: TRect;
  aFactor: Extended;
  aWidth: Integer;
  tmpSize: Integer;
begin
  if not Assigned(Self) then exit;
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
      if (Column.FieldName = 'ACTIONICON') and Assigned(Column.Field) then
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
                end;
              if TMGridObject(aObj).Caption <> '' then
                brect.Top := bRect.Top+TStringGrid(Sender).Canvas.TextExtent('A').cy;
              if TMGridObject(aObj).Bold then
                TStringGrid(Sender).Canvas.Font.Style := [fsBold];
              {$IFDEF WINDOWS}
              tmpSize := GetFontData(TStringGrid(Sender).Canvas.Font.Handle).Height;
              TStringGrid(Sender).Canvas.Font.Height :=tmpSize-TMGridObject(aObj).FontSize;
              {$ENDIF}
              if not (gdSelected in State) then
                begin
                  TStringGrid(Sender).Canvas.Font.Color:=TMGridObject(aObj).TextColor;
                end;
              if fTimeline.WordWrap then
                TStringGrid(Sender).Canvas.TextRect(bRect,aRect.Left+3,bRect.Top,aText,aTextStyleW)
              else
                TStringGrid(Sender).Canvas.TextRect(bRect,aRect.Left+3,bRect.Top,aText,aTextStyle);
              {$IFDEF WINDOWS}
              TStringGrid(Sender).Canvas.Font.Height := tmpSize;
              {$ENDIF}
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
end;
procedure TfHistoryFrame.acAddExecute(Sender: TObject);
var
  i: Integer;
  aClass: TBaseDBDatasetClass;
  aObj: TBaseDBDataset;
  aHist : IBaseHistory;
begin
  if fHistoryAddItem.Execute then
    begin
      if Assigned(TBaseHistory(DataSet).Parent) then
        TBaseHistory(DataSet).AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,'',fHistoryAddItem.eReference.Text,TBaseHistory(DataSet).Parent.DataSet,ACICON_USEREDITED,'',True,True)
      else
        TBaseHistory(DataSet).AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,'',fHistoryAddItem.eReference.Text,nil,ACICON_USEREDITED,'',True,True);
      for i := 0 to fHistoryAddItem.lbAdditional.Count-1 do
        if Data.ListDataSetFromLink(fHistoryAddItem.lbAdditional.Items[i],aClass) then
          begin
            aObj := aClass.CreateEx(nil, Data);
            if aObj is TBaseDbList then
              begin
                TBaseDBList(aObj).SelectFromLink(fHistoryAddItem.lbAdditional.Items[i]);
                aObj.Open;
                if aObj.Count>0 then
                  begin
                    if Supports(aObj,IBaseHistory,aHist) then
                      begin
                        aHist.History.AddItem(Data.Users.DataSet,fHistoryAddItem.eAction.Text,'',fHistoryAddItem.eReference.Text,nil,ACICON_USEREDITED,'',True,True);
                        aHist := nil;
                      end;
                  end;
              end;
            aObj.Destroy;
          end;
      FTimeLine.Refresh(True);
      if Assigned(FOnAddUserMessage) then
        FOnAddUserMessage(fHistoryAddItem);
      fHistoryAddItem.lbAdditional.Clear;
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
  FTimeLine.BaseFilter:='('+aFilter+')';
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
      if Assigned(FOnAddUserMessage) then
        FOnAddUserMessage(fHistoryAddItem);
      FTimeLine.Refresh;
    end;
end;
procedure TfHistoryFrame.acDeleteExecute(Sender: TObject);
begin
  FTimeLine.Delete;
end;

procedure TfHistoryFrame.acIgnoreExecute(Sender: TObject);
begin
  if FTimeLine.GotoActiveRow then
    begin
      with DataSet.DataSet as IBaseManageDB do
        UpdateStdFields := False;
      DataSet.Edit;
      if DataSet.FieldByName('IGNORE').AsString <> 'Y' then
        DataSet.FieldByName('IGNORE').AsString:='Y'
      else
        DataSet.FieldByName('IGNORE').AsString:='N';
      DataSet.Post;
      with DataSet.DataSet as IBaseManageDB do
        UpdateStdFields := True;
    end;
end;

procedure TfHistoryFrame.acRefreshExecute(Sender: TObject);
begin
  FTimeLine.Refresh(True);
end;

procedure TfHistoryFrame.FContListViewDetails(Sender: TObject);
begin
  if FTimeLine.GotoActiveRow then
    begin
      if DataSet.FieldByName('LINK').AsString='' then
        begin
          DataSet.Prior;
          if FDataSet.DataSet.BOF and (FDataSet.FieldByName('CHANGEDBY').AsString = Data.Users.IDCode.AsString) then
            begin
              if fHistoryAddItem.Execute(FDataSet) then
                begin
                  if not FDataSet.CanEdit then
                    FDataSet.DataSet.Edit;
                  FDataSet.DataSet.FieldByName('ACTION').AsString:=fHistoryAddItem.eAction.Text;
                  FDataSet.DataSet.FieldByName('REFERENCE').AsString:=fHistoryAddItem.eReference.Text;
                  if FDataSet.CanEdit then
                    FDataSet.DataSet.Post;
                end;
              FTimeLine.Refresh(True);
            end;
        end
      else
        Data.GotoLink(DataSet.FieldByName('LINK').AsString);
    end;
end;

procedure TfHistoryFrame.FTimeLineGetCellText(Sender: TObject; aCol: TColumn;
  aRow: Integer; var NewText: string; aFont: TFont);
var
  fHasObject: Boolean;
  i: Integer;
  aObj: TObject;
  arec: LargeInt;
  aTime: TDateTime;
  aI: Integer;
  aLight: Extended;
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
                  aI := FTimeLine.DataSet.FieldByName('ACTIONICON').AsInteger;
                  aLight:=0;
                  if length(HistryPrio)>aI then
                    begin
                      aLight := 0.8-((1*HistryPrio[aI]/10)*0.8);
                      if HistryPrio[aI]<4 then
                        TMGridObject(aObj).FontSize := -1;
                      if HistryPrio[aI]>6 then
                        TMGridObject(aObj).FontSize := 1;
                    end;
                  TMGridObject(aObj).TextColor:=Ligthen(TMGridObject(aObj).TextColor,aLight);
                end;
              fTimeline.DataSet.GotoBookmark(aRec);
            end;
          NewText := TMGridObject(aObj).Caption+lineending+NewText;
          if length(NewText)>1000 then
            NewText:=copy(NewText,0,1000)+LineEnding+'...';
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
          aCol.Alignment:=taRightJustify;
        end;
    end;
end;

procedure TfHistoryFrame.SetBaseName(const AValue: string);
var
  TopVisible: Boolean;
begin
  if fBaseName=AValue then exit;
  fBaseName:=AValue;
//  TopVisible := FContList.pTop.Visible;
  FTimeLine.BaseName:='PHIST'+AValue;
//  FContList.pTop.Visible := TopVisible;
  RestoreButtons;
end;

type
  TDefaultButtons = 1..30;
procedure TfHistoryFrame.RestoreButtons;
var
  i: Integer;
  aButton: TSpeedButton;
  DefaultButtons : set of TDefaultButtons = [1,2,3,4,5,6,7,8,9,11,14];
  s: String;
begin
  with Application as IBaseDbInterface do
    s := DBConfig.ReadString('HISTB:'+fBaseName,'');
  while pButtons.ComponentCount>0 do
    pButtons.Components[0].Free;
  for i := 0 to fVisualControls.HistoryImages.Count-1 do
    begin
      aButton := TSpeedButton.Create(pButtons);
      aButton.Glyph := nil;
      aButton.Flat:=True;
      fVisualControls.HistoryImages.GetBitmap(i,AButton.Glyph);
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
      13:aButton.Hint:=strHistory13;
      14:aButton.Hint:=strHistory14;
      end;
    end;
end;

constructor TfHistoryFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeLine := TfGridView.Create(Self);
  with FTimeLine do
    begin
      BaseName:='PHIST';
      DefaultRows:='GLOBALWIDTH:%;ACTIONICON:30;ACTION:200;REFERENCE:100;COMMISSION:100;TIMESTAMPD:100;CHANGEDBY:40;';
      Parent := pCont;
      Align := alClient;
      SortDirection:=sdDescending;
      SortField:='TIMESTAMPD';
      TextField:='ACTION';
      ReadOnly:=True;
      FTimeLine.FilterRow:=True;
      WordWrap:=True;
      Show;
    end;
  FTimeLine.OnDrawColumnCell:=@FContListDrawColumnCell;
  FTimeLine.OnDblClick:=@FContListViewDetails;
  FTimeLine.OnGetCellText:=@FTimeLineGetCellText;
  FTimeLine.WordWrap:=true;
  Panel3.Visible:=Data.Users.Rights.Right('HISTORY') > RIGHT_WRITE;
  RestoreButtons;
end;
destructor TfHistoryFrame.Destroy;
begin
  if Assigned(FTimeLine) then
    begin
      FTimeLine.DataSet := nil;
      FTimeLine.Free;
      FTimeLine := nil;
    end;
  DataSet := nil;
  inherited Destroy;
end;
procedure TfHistoryFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  if not Assigned(FTimeLine) then exit;
  FTimeLine.DataSet := AValue;
  aButtonClick(nil);
end;
procedure TfHistoryFrame.SetRights(Editable : Boolean);
begin
  ArrangeToolBar(pToolbar,ActionList1,'History');
end;
procedure TfHistoryFrame.ShowFrame;
begin
  inherited ShowFrame;
  FTimeLine.Refresh(True);
  FTimeLine.SetActive;
end;

end.

