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
Created 04.04.2012
*******************************************************************************}
unit uorderaddressframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ActnList,
  db, uOrder,LCLType, ExtCtrls, DBGrids, variants,Graphics,Clipbrd;
type
  TSearchKey = procedure(Sender : TObject;X,Y : Integer;var Key : Word;Shift : TShiftState;SearchString : string) of object;

  { TfOrderAddress }

  TfOrderAddress = class(TFrame)
    acInsertAddress: TAction;
    acDeleteAddress: TAction;
    acGotoAddress: TAction;
    acSearchAddress: TAction;
    ActionList1: TActionList;
    bAddressDelete1: TSpeedButton;
    bAdressNew1: TSpeedButton;
    bAdressNew2: TSpeedButton;
    bAdressNew3: TSpeedButton;
    cbType: TComboBox;
    Datasource: TDatasource;
    mAddress: TMemo;
    Panel1: TPanel;
    pToolbar: TPanel;
    SearchTimer: TTimer;
    SpeedButton1: TSpeedButton;
    procedure acDeleteAddressExecute(Sender: TObject);
    procedure acGotoAddressExecute(Sender: TObject);
    procedure acInsertAddressExecute(Sender: TObject);
    procedure acSearchAddressExecute(Sender: TObject);
    procedure cbTypeSelect(Sender: TObject);
    procedure DatasourceStateChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    procedure mAddressChange(Sender: TObject);
    procedure mAddressDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure mAddressDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure mAddressExit(Sender: TObject);
    procedure mAddressKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure mAddressKeyPress(Sender: TObject; var Key: char);
    procedure SearchTimerTimer(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FDataSet: TOrderAddress;
    FSearchKey: TSearchKey;
    OldState : TDataSetState;
    RealAddr : Boolean;
    procedure SetDataSet(AValue: TOrderAddress);
    { private declarations }
  public
    { public declarations }
    Rec : variant;
    constructor Create(TheOwner: TComponent); override;
    property DataSet : TOrderAddress read FDataSet write SetDataSet;
    procedure SetLanguage;
    procedure SetRights(Editable : Boolean);
    property OnSearchKey : TSearchKey read FSearchKey write FSearchKey;
    function GotoAddress: Boolean;
    function EnterEdit : Boolean;
    procedure SetFocus;override;
  end;
implementation
{$R *.lfm}
uses uIntfStrConsts,uPersonFrame,uMainTreeFrame,uData,uPerson,uOrderFrame,
  uSearch,uBaseDBInterface,uAddressFrame,uBaseVisualApplication,uOrderAdressDetail;
procedure TfOrderAddress.cbTypeSelect(Sender: TObject);
begin
  if (DataSet.State = dsInsert) or DataSet.GotoBookmark(Rec) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
    end
  else if (DataSet.State <> dsInsert) then
    DataSet.Append;
  DataSet.FieldByName('TYPE').AsString:=copy(cbType.Text,0,3);
end;
procedure TfOrderAddress.acInsertAddressExecute(Sender: TObject);
var
  aFrame: TfPersonFrame;
begin
  aFrame := TfPersonFrame.Create(Self);
  fMainTreeFrame.pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
  aFrame.eName.SetFocus;
  aFrame.CustomerOf := FDataSet.Order.FieldByName('ORDERNO').AsString;// Data.BuildLink(DataSet.DataSet);
  if mAddress.Font.Color=clDefault then
    begin
      Clipboard.AsText:=mAddress.Text;
      if aFrame.pcPages.PageIndex=1 then
        TfAddressFrame(aFrame.pcPages.Pages[1].Controls[0]).bPasteFromClipboard.Click;
    end;
end;

procedure TfOrderAddress.acSearchAddressExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.AllowSearchTypes(strCustomerCont+','+strCustomers+','+strAdresses);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'CONTACT',strSearchFromOrder);
  fSearch.SetLanguage;
end;

procedure TfOrderAddress.acGotoAddressExecute(Sender: TObject);
var
  aPerson: TPerson;
begin
  if TOrderAddress(DataSet).FieldByName('ACCOUNTNO').IsNull then exit;
  aPerson := TPerson.CreateEx(Self,Data);
  aPerson.SelectByAccountNo(DataSet.FieldByName('ACCOUNTNO').AsString);
  aPerson.Open;
  if aPerson.Count > 0 then
    begin
      Data.GotoLink(Data.BuildLink(aPerson.DataSet));
    end;
  aPerson.Free;
end;

procedure TfOrderAddress.acDeleteAddressExecute(Sender: TObject);
var
  aOF: TfOrderFrame;
  DoR: Boolean = False;
begin
  if (DataSet.DataSet.RecordCount = 0) and (DataSet.dataSet.State <> dsInsert) then
    begin
      aOF := TfOrderFrame(Parent.Owner);
      Parent.RemoveControl(Self);
      aOF.RefreshAddress;
      Self.Free;
      exit;
    end;
  if (Rec=0) then
    begin
      DataSet.DataSet.Cancel;
      DoR := True;
    end
  else if (not (Rec=0)) and (FDataSet.GotoBookmark(Rec)) then
    begin
      DataSet.Delete;
      DoR := True;
    end;
  if DoR then
    begin
      aOF := TfOrderFrame(Parent.Owner);
      Parent.RemoveControl(Self);
      aOF.RefreshAddress;
      Self.Free;
    end;
end;
procedure TfOrderAddress.DatasourceStateChange(Sender: TObject);
begin
  if (OldState = dsInsert) and (Rec=0) then
    Rec := DataSet.Id.AsVariant;
  OldState := Datasource.State;
end;

procedure TfOrderAddress.FrameEnter(Sender: TObject);
begin
  if mAddress.CanFocus then
    mAddress.SetFocus;
end;

function TfOrderAddress.fSearchOpenItem(aLink: string): Boolean;
var
  aPerson: TPerson;
begin
  aPerson := TPerson.CreateEx(Self,Data);
  aPerson.SelectFromLink(fSearch.GetLink);
  aPerson.Open;
  if aPerson.Count > 0 then
    begin
      aPerson.Address.Open;
      DataSet.Assign(aPerson);
    end;
  aPerson.Free;
  mAddress.Text:=DataSet.ToString;
  mAddress.Font.Color:=clDefault;
  TfOrderFrame(Parent.Owner).GotoPosition;
end;

procedure TfOrderAddress.mAddressChange(Sender: TObject);
var
  i: Integer;
begin
  if (DataSet.State = dsInsert) or DataSet.GotoBookmark(Rec) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
    end
  else if (DataSet.State <> dsInsert) then
    begin
      DataSet.Append;
      for i := 0 to cbType.Items.Count-1 do
        if copy(cbType.Items[i],0,3) = DataSet.FieldByName('TYPE').AsString then
          begin
            cbType.ItemIndex:=i;
            break;
          end;
    end;
  DataSet.FromString(mAddress.Text);
  if DataSet.FieldByName('ACCOUNTNO').IsNull then
    DataSet.FieldByName('ACCOUNTNO').AsString:='';
end;
procedure TfOrderAddress.mAddressDragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
  nData : TTreeEntry;
  aPerson: TPerson;
begin
  if (DataSet.State = dsInsert) or DataSet.GotoBookmark(Rec) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
    end;
  if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
    begin
      nData := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
      aPerson := TPerson.CreateEx(Self,Data);
      Data.SetFilter(aPerson,nData.Filter);
      Data.GotoBookmark(aPerson,nData.Rec);
      aPerson.Address.Open;
      DataSet.Assign(aPerson);
      aPerson.Free;
    end
  else
  if (Source = fSearch.sgResults) then
    begin
      aPerson := TPerson.CreateEx(Self,Data);
      aPerson.SelectFromLink(fSearch.GetLink);
      aPerson.Open;
      if aPerson.Count > 0 then
        begin
          aPerson.Address.Open;
          DataSet.Assign(aPerson);
        end;
      aPerson.Free;
    end
  else if Source is TDragEntry then
    begin
      aPerson := TPerson.CreateEx(Self,Data);
      aPerson.SelectFromLink(TDragEntry(Source).Links);
      aPerson.Open;
      if aPerson.Count > 0 then
        begin
          aPerson.Address.Open;
          DataSet.Assign(aPerson);
        end;
      aPerson.Free;
    end;
  mAddress.Text:=DataSet.ToString;
  mAddress.Font.Color:=clDefault;
  TfOrderFrame(Parent.Owner).GotoPosition;
end;
procedure TfOrderAddress.mAddressDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if not cbType.Enabled then exit;
  if Assigned(uMainTreeFrame.fMainTreeFrame)
  and (Source = uMainTreeFrame.fMainTreeFrame.tvMain)
  and ((TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etCustomer)
  or (TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etSupplier)) then
    Accept := True;
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      with fSearch.sgResults do
        if Data.GetLinkIcon(fSearch.GetLink)= IMAGE_PERSON then
          Accept := True;
    end;
  if Source is TDragEntry then
    begin
      Accept := (pos('CUSTOMERS' ,TDragEntry(Source).Links)>0)
             or (pos('ADRESSES' ,TDragEntry(Source).Links)>0);
      exit;
    end;
end;
procedure TfOrderAddress.mAddressExit(Sender: TObject);
var
  akey : Word = VK_ESCAPE;
begin
  if mAddress.Lines.Count < 2 then
    if Assigned(FSearchKey) then
      FSearchKey(Self,mAddress.Left,mAddress.Top+Canvas.TextExtent('xYg').cy,aKey,[],'');
end;
procedure TfOrderAddress.mAddressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if RealAddr = False then
    begin
      RealAddr:=True;
      mAddress.Font.Color:=clDefault;
      mAddress.Clear;
    end;
  if mAddress.Lines.Count < 2 then
    if Assigned(FSearchKey) then
      FSearchKey(Self,mAddress.Left,mAddress.Top+Canvas.TextExtent('xYg').cy,Key,Shift,'');
end;
procedure TfOrderAddress.mAddressKeyPress(Sender: TObject; var Key: char);
begin
  SearchTimer.Enabled:=True;
end;
procedure TfOrderAddress.SearchTimerTimer(Sender: TObject);
var
  aKey : Word = 0;
begin
  SearchTimer.Enabled:=False;
  if mAddress.Lines.Count < 2 then
    if Assigned(FSearchKey) then
      FSearchKey(Self,Left+mAddress.Left,mAddress.Top+Canvas.TextExtent('xYg').cy,aKey,[],mAddress.Lines[0]);
end;

procedure TfOrderAddress.SpeedButton1Click(Sender: TObject);
begin
  fEditadress.setLanguage;
  fEditAdress.cDatasource.DataSet := Datasource.DataSet;
  if GotoAddress and fEditAdress.Execute then
    mAddress.Text:=DataSet.ToString;
end;

procedure TfOrderAddress.SetDataSet(AValue: TOrderAddress);
var
  i: Integer;
begin
  if FDataSet <>AValue then
    begin
      FDataSet := AValue;
      DataSource.DataSet := AValue.DataSet;
    end;
  if ((FDataSet.State = dsInsert) or FDataSet.GotoBookmark(Rec)) then
    begin
      if DataSet.ToString<>'' then
        begin
          mAddress.Text:=DataSet.ToString;
          RealAddr := True;
          mAddress.Font.Color:=clDefault;
        end;
      for i := 0 to cbType.Items.Count-1 do
        if copy(cbType.Items[i],0,3) = FDataSet.FieldByName('TYPE').AsString then
          begin
            cbType.ItemIndex:=i;
            break;
          end;
    end;
end;

constructor TfOrderAddress.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  RealAddr:=False;
end;

procedure TfOrderAddress.SetLanguage;
begin
  cbType.Clear;
  cbType.Items.Add(strIADAddress);
  cbType.Items.Add(strDADAddress);
end;
procedure TfOrderAddress.SetRights(Editable: Boolean);
begin
  mAddress.ReadOnly:=not Editable;
  cbType.Enabled := Editable;
  bAdressNew1.Enabled:=Editable;
  bAddressDelete1.Enabled:=Editable;
  with Application as IBaseDbInterface do
    if DBConfig.ReadBoolean('TBLEFT',True) then
      pToolbar.Align:=alLeft
    else pToolbar.Align:=alRight;
end;
function TfOrderAddress.GotoAddress : Boolean;
begin
  Result := (DataSet.State = dsInsert) or DataSet.GotoBookmark(Rec);
end;
function TfOrderAddress.EnterEdit: Boolean;
begin
  if (DataSet.State = dsInsert) or DataSet.GotoBookmark(Rec) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
    end
  else if (DataSet.State <> dsInsert) then
    DataSet.Append;
end;

procedure TfOrderAddress.SetFocus;
begin
  inherited SetFocus;
  mAddress.SetFocus;
end;

end.

