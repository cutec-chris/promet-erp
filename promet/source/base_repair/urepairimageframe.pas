unit urepairimageframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, DBGrids,
  uPositionFrame, Grids, DbCtrls, ExtDlgs, Menus, EditBtn, Buttons,
  uExtControls, db,uprometframesinplace,uOrder,uMasterdata,Graphics;

type
 { TfRepairImageFrame }

  TfRepairImageFrame = class(TPrometInplaceFrame)
    cbImage: TComboBox;
    cbOperation: TDBComboBox;
    cbVersion1: TDBComboBox;
    cbWarrenty: TDBCheckBox;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    dnRepairPos: TDBNavigator;
    eSerial1: TDBEdit;
    gProblems: TExtDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    lNotesforCustomer1: TLabel;
    RepairDetail: TDataSource;
    lInfo: TLabel;
    lErrordescription: TLabel;
    lInternalNotes: TLabel;
    lNotesforCustomer: TLabel;
    lOperation: TLabel;
    lSerial1: TLabel;
    lVersion1: TLabel;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mErrordesc: TDBMemo;
    mInternalNotes: TDBMemo;
    mNotes: TDBMemo;
    Panel3: TPanel;
    Position: TDatasource;
    Repair: TDatasource;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure cbImageKeyPress(Sender: TObject; var Key: char);
    procedure cbImageSelect(Sender: TObject);
    procedure ComboSearch(Data: PtrInt);
    procedure eSerial1Exit(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TfPositionDatasetTOrderPosRepairDetailsDataSetAfterScroll(
      DataSet: TDataSet);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer2StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FOldRowHeight : Integer;
    gProblemsColumn: TRECT;
    FImages : TOrderRepairImages;
    Repairtime: Integer;
    PosrepairTime : Integer;
    procedure SetRepairImage;
  public
    { public declarations }
    procedure SetRights(Editable: Boolean); override;
    procedure SetLanguage;
    destructor Destroy; override;
    procedure SetArticle(aMasterdata: TMasterdata); override;
  end;

implementation
uses uData,uRowEditor,urepairimages,uIntfStrConsts,uBaseERPDBClasses;
{$R *.lfm}

{ TfRepairImageFrame }

procedure TfRepairImageFrame.Timer1StartTimer(Sender: TObject);
begin
  //Repairtime := 2;
  Timer1.Interval := 60000;
  //Timer1.Enabled := True;
  lInfo.Visible := True;
  Timer2.Enabled := False;
  lInfo.Font.Color := clGreen;
  lInfo.Color := clInfoBk;
  lInfo.Caption := 'Reparaturzeit: '+Format(' %d m',[Repairtime]);
end;

procedure TfRepairImageFrame.Timer2StartTimer(Sender: TObject);
begin
  Timer2.Interval := 1000;
end;

procedure TfRepairImageFrame.Timer1Timer(Sender: TObject);
begin
  lInfo.Caption := 'Reparaturzeit: '+Format(' %d m',[Repairtime]);
  Dec(Repairtime);
  if (Repairtime < 0) then
    begin
      Timer1.Enabled := False;
      lInfo.Font.Color :=  clRed;
      lInfo.Caption := 'Reparaturzeit überschritten';
      Timer2.Enabled := True;
    end;
end;

procedure TfRepairImageFrame.Timer2Timer(Sender: TObject);
begin
  if lInfo.Font.Color = clRed then
    begin
      lInfo.Font.Color := clInfoBk;
    end
  else lInfo.Font.Color := clRed;
end;

procedure TfRepairImageFrame.SetRepairImage;
begin
  with TOrderPos(TfPosition(Owner).Dataset).Repair do
    begin
      Open;
      if FieldByName('ERRIMAGE').AsVariant=fRepairImages.DataSet.Id.AsVariant then exit;
      Edit;
      FieldByName('ERRIMAGE').AsVariant:=fRepairImages.DataSet.Id.AsVariant;
      FieldByName('IMAGENAME').AsString:=fRepairImages.DataSet.FieldByName('NAME').AsString;
      if FieldByName('NOTES').IsNull then
        FieldByName('NOTES').AsString:=fRepairImages.DataSet.FieldByName('NOTES').AsString;
      if FieldByName('INTNOTES').IsNull then
        FieldByName('INTNOTES').AsString:=fRepairImages.DataSet.FieldByName('INTNOTES').AsString;
      try
        fRepairImages.DataSet.Edit;
        fRepairImages.DataSet.FieldByName('COUNTER').AsInteger:=fRepairImages.DataSet.FieldByName('COUNTER').AsInteger+1;
        fRepairImages.DataSet.Post;
      except
      end;
      Details.Open;
      while Details.Count>0 do Details.Delete;
      fRepairImages.DataSet.RepairDetail.First;
      while not fRepairImages.DataSet.RepairDetail.EOF do
        begin
          Details.Insert;
          Details.FieldByName('ASSEMBLY').AsString := fRepairImages.DataSet.RepairDetail.FieldByName('ASSEMBLY').AsString;
          Details.FieldByName('PART').AsString := fRepairImages.DataSet.RepairDetail.FieldByName('PART').AsString;
          Details.FieldByName('ERROR').AsString := fRepairImages.DataSet.RepairDetail.FieldByName('ERROR').AsString;
          Details.Post;
          fRepairImages.DataSet.RepairDetail.Next;
        end;
      Post;
    end;
end;

procedure TfRepairImageFrame.SpeedButton1Click(Sender: TObject);
begin
  if fRepairImages.Execute then
    begin
      cbImage.Text := fRepairImages.DataSet.FieldByName('NAME').AsString;
      SetRepairImage;
    end;
end;

procedure TfRepairImageFrame.TfPositionDatasetTOrderPosRepairDetailsDataSetAfterScroll
  (DataSet: TDataSet);
begin
  if not Assigned(Owner) then exit;
  if not Assigned(TfPosition(Owner).Dataset) then exit;
  if not TfPosition(Owner).Dataset.Active then exit;
  if TfPosition(Owner).Dataset is TOrderPos then
    with TfPosition(Owner).DataSet as TOrderPos do
      begin
        if Repair.Count>0 then
          begin
            FImages.Select(Repair.FieldByName('ERRIMAGE').AsVariant);
            FImages.Open;
            if FImages.Count>0 then
              cbImage.Text:=FImages.FieldByName('NAME').AsString
            else cbImage.Text:='';
          end;
      end;
end;

procedure TfRepairImageFrame.cbImageKeyPress(Sender: TObject; var Key: char);
begin
  if (key in ['a'..'z'])
  or (key in ['A'..'Z'])
  or (key in ['0'..'9']) then
    begin
      Application.QueueAsyncCall(@ComboSearch,0);
    end;
end;

procedure TfRepairImageFrame.cbImageSelect(Sender: TObject);
begin
  if fRepairImages.DataSet.Locate('NAME',cbImage.text,[]) then
    begin
      SetRepairImage;
    end;
end;

procedure TfRepairImageFrame.ComboSearch(Data: PtrInt);
var
  aText: String;
  i: Integer;
  fAdded: Boolean;
begin
  aText := trim(cbImage.Text);
  if aText<>'' then
    begin
      fAdded := False;
      fRepairImages.eFilter.Text:=aText;
      if fRepairImages.DataSet.Count>0 then
        begin
          cbImage.Items.BeginUpdate;
          i := 0;
          while i < cbImage.Items.Count do
            begin
              if fRepairImages.DataSet.Locate('NAME',cbImage.Items[i],[]) then
                inc(i)
              else cbImage.Items.Delete(i);
            end;
          while not fRepairImages.DataSet.EOF do
            begin
              if cbImage.Items.IndexOf(fRepairImages.DataSet.FieldByName('NAME').AsString)=-1 then
                cbImage.Items.Add(fRepairImages.DataSet.FieldByName('NAME').AsString);
              fRepairImages.DataSet.Next;
              fAdded := True;
            end;
          cbImage.Items.EndUpdate;
        end;
      if fAdded and (not cbImage.DroppedDown) then
        begin
          cbImage.DroppedDown:=True;
          cbImage.Text:=aText;
          cbImage.SelStart:=length(cbImage.Text);
        end;
    end;
end;

procedure TfRepairImageFrame.eSerial1Exit(Sender: TObject);
var
  aStorageJournal: TStorageJournal;
begin
  lInfo.Visible:=False;
  if trim(eSerial1.Text)<>'' then
    begin
      aStorageJournal := TStorageJournal.Create(nil);
      aStorageJournal.Filter(Data.QuoteField('ID')+'='+Data.QuoteValue(Position.DataSet.FieldByName('IDENT').AsString)+' AND '+Data.QuoteField('SERIAL')+'='+Data.QuoteValue(trim(eSerial1.Text))+' AND NOT '+Data.ProcessTerm(Data.QuoteField('NOTE')+'='+Data.QuoteValue('')));
      //Wenn Notiz zu Artikel mit dieser Serienummer im Lagerjournal vorhanden, zeigen wir sie an
      if aStorageJournal.Count>0 then
        begin
          lInfo.Caption:=aStorageJournal.FieldByName('NOTE').AsString;
          lInfo.Visible:=True;
        end;
      aStorageJournal.Free;
    end;
end;

procedure TfRepairImageFrame.FrameEnter(Sender: TObject);
begin
  lInfo.Visible := False;
  if TfPosition(Owner).Dataset is TOrderPos then
    with TfPosition(Owner).DataSet as TOrderPos do
      begin
        cbImage.Clear;
        //if Self.Repair.DataSet = Repair.DataSet then exit;
        fRepairImages.SetLanguage;
        Repair.Open;
        FImages := TOrderRepairImages.Create(nil);
        Position.DataSet := DataSet;
        RepairDetail.DataSet := Repair.Details.DataSet;
        Repair.Details.DataSet.AfterScroll:=@TfPositionDatasetTOrderPosRepairDetailsDataSetAfterScroll;
        Repair.Details.Open;
        Self.Repair.DataSet := Repair.DataSet;
        if Repair.Count>0 then
          begin
            FImages.Select(Repair.FieldByName('ERRIMAGE').AsVariant);
            FImages.Open;
            if FImages.Count>0 then
              cbImage.Text:=FImages.FieldByName('NAME').AsString
            else cbImage.Text:='';
          end;
      end;
end;

procedure TfRepairImageFrame.SetRights(Editable: Boolean);
begin
  SetLanguage;
  Panel3.Enabled:=Editable;
  DBGrid1.ReadOnly:=not Editable;
end;

procedure TfRepairImageFrame.SetLanguage;
begin
  cbOperation.Clear;
  cbOperation.Items.Add(strRepaired);
  cbOperation.Items.Add(strDiscarded);
  cbOperation.Items.Add(strWaitingforCustomer);
  cbOperation.Items.Add(strAssemblyexchanged);
  cbOperation.Items.Add(strIsNew);
end;

procedure TfRepairImageFrame.SetArticle(aMasterdata: TMasterdata);
begin
  if not aMasterdata.FieldByName('REPAIRTIME').IsNull then
    begin
      Position.Edit;
      Position.DataSet.FieldByName('REPAIRTIME').AsVariant:=aMasterdata.FieldByName('REPAIRTIME').AsVariant;
      Repairtime := aMasterdata.FieldByName('REPAIRTIME').value;
      Timer1.Enabled := True;
      lInfo.Visible := True;
      Timer2.Enabled := False;
      lInfo.Font.Color := clGreen;
      lInfo.Color := clInfoBk;
      lInfo.Caption := 'Reparaturzeit: '+Format(' %d m',[Repairtime]);
    end
  else lInfo.Visible := False;
end;

destructor TfRepairImageFrame.Destroy;
begin
  if Assigned(fRepairImages) then
    FreeAndNil(fRepairImages);
  inherited Destroy;
end;

{ TfRepairImageFrame }


end.

