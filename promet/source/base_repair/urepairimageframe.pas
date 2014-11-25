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
    eSerial1: TDBEdit;
    Label1: TLabel;
    Timer: TLabel;
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
    procedure cbImageChange(Sender: TObject);
    procedure cbImageKeyPress(Sender: TObject; var Key: char);
    procedure cbImageSelect(Sender: TObject);
    procedure ComboSearch(Data: PtrInt);
    procedure FrameEnter(Sender: TObject);
    procedure PositionDataChange(Sender: TObject; Field: TField);
    procedure PositionDataSetBeforePost(DataSet: TDataSet);
    procedure SpeedButton1Click(Sender: TObject);
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
  public
    { public declarations }
    procedure SetRights(Editable: Boolean); override;
    procedure SetLanguage;
    destructor Destroy; override;
    procedure SetArticle(aMasterdata: TMasterdata); override;
  end;

implementation
uses uData,uRowEditor,urepairimages,uIntfStrConsts;
{$R *.lfm}

{ TfRepairImageFrame }

procedure TfRepairImageFrame.Timer1StartTimer(Sender: TObject);
begin
  //Repairtime := 2;
  Timer1.Interval := 60000;
  //Timer1.Enabled := True;
  Timer.Visible := True;
  Timer2.Enabled := False;
  Timer.Font.Color := clGreen;
  Timer.Color := clInfoBk;
  Timer.Caption := 'Reparaturzeit: '+Format(' %d m',[Repairtime]);
end;

procedure TfRepairImageFrame.Timer2StartTimer(Sender: TObject);
begin
  Timer2.Interval := 1000;
end;

procedure TfRepairImageFrame.Timer1Timer(Sender: TObject);
begin
  Timer.Caption := 'Reparaturzeit: '+Format(' %d m',[Repairtime]);
  Dec(Repairtime);
  if (Repairtime < 0) then
    begin
      Timer1.Enabled := False;
      Timer.Font.Color :=  clRed;
      Timer.Caption := 'Reparaturzeit überschritten';
      Timer2.Enabled := True;
    end;
end;

procedure TfRepairImageFrame.Timer2Timer(Sender: TObject);
begin
  if Timer.Font.Color = clRed then
    begin
      Timer.Font.Color := clInfoBk;
    end
  else Timer.Font.Color := clRed;
end;

procedure TfRepairImageFrame.SpeedButton1Click(Sender: TObject);
begin
  if fRepairImages.Execute then
    begin
      cbImage.Text := fRepairImages.DataSet.FieldByName('NAME').AsString;
      with TOrderPos(TfPosition(Owner).Dataset).Repair do
        begin
          Open;
          Edit;
          FieldByName('ERRIMAGE').AsVariant:=fRepairImages.DataSet.Id.AsVariant;
          Post;
        end;
    end;
end;

procedure TfRepairImageFrame.cbImageChange(Sender: TObject);
begin

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
      with TOrderPos(TfPosition(Owner).Dataset).Repair do
        begin
          Open;
          Edit;
          FieldByName('ERRIMAGE').AsVariant:=fRepairImages.DataSet.Id.AsVariant;
          Post;
        end;
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

procedure TfRepairImageFrame.FrameEnter(Sender: TObject);
begin
  Timer.Visible := False;
  if TfPosition(Owner).Dataset is TOrderPos then
    with TfPosition(Owner).DataSet as TOrderPos do
      begin
        cbImage.Clear;
        if Self.Repair.DataSet = Repair.DataSet then exit;
        fRepairImages.SetLanguage;
        Repair.Open;
        FImages := TOrderRepairImages.Create(nil,Data);
        Position.DataSet := DataSet;
        Self.Repair.DataSet := Repair.DataSet;
        if Repair.Count>0 then
          begin
            FImages.Select(Repair.FieldByName('ERRIMAGE').AsVariant);
            FImages.Open;
            if FImages.Count>0 then
              cbImage.Text:=FImages.FieldByName('NAME').AsString;
          end;
      end;
end;

procedure TfRepairImageFrame.PositionDataChange(Sender: TObject; Field: TField);
var
  Quantity : double;
  QuantityOld : double;
begin
  //Quantity := DataSet.FieldByName('QUANTITY');
  if Quantity<>0 then
    begin
      Quantity:=1;
    end;
end;

procedure TfRepairImageFrame.PositionDataSetBeforePost(DataSet: TDataSet);
var
  Quantity : double;
begin
  Quantity := DataSet.FieldByName('QUANTITY').OldValue-DataSet.FieldByName('QUANTITY').Value;
  if Quantity<>0 then
    begin
      Quantity:=1;
    end;
end;

procedure TfRepairImageFrame.SetRights(Editable: Boolean);
begin
  SetLanguage;
  Enabled:=Editable;
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
      Repairtime := aMasterdata.FieldByName('REPAIRTIME').value;
      Timer1.Enabled := True;
      Timer.Visible := True;
      Timer2.Enabled := False;
      Timer.Font.Color := clGreen;
      Timer.Color := clInfoBk;
      Timer.Caption := 'Reparaturzeit: '+Format(' %d m',[Repairtime]);
    end
  else Timer.Visible := False;
  Position.DataSet.BeforePost:=@PositionDataSetBeforePost;
end;

destructor TfRepairImageFrame.Destroy;
begin
  if Assigned(fRepairImages) then
    FreeAndNil(fRepairImages);
  inherited Destroy;
end;

{ TfRepairImageFrame }


end.

