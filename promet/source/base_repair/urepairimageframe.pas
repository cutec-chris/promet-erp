unit urepairimageframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, DBGrids,
  uPositionFrame, Grids, DbCtrls, ExtDlgs, Menus, EditBtn, Buttons,
  uExtControls, db,uprometframesinplace,uOrder;

type
 { TfRepairImageFrame }

  TfRepairImageFrame = class(TPrometInplaceFrame)
    cbOperation: TDBComboBox;
    cbVersion1: TDBComboBox;
    cbWarrenty: TDBCheckBox;
    cbImage: TComboBox;
    Position: TDatasource;
    Repair: TDatasource;
    eSerial1: TDBEdit;
    Label1: TLabel;
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
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    procedure cbImageChange(Sender: TObject);
    procedure cbImageKeyPress(Sender: TObject; var Key: char);
    procedure cbImageSelect(Sender: TObject);
    procedure ComboSearch(Data: PtrInt);
    procedure FrameEnter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FOldRowHeight : Integer;
    gProblemsColumn: TRECT;
    FImages : TOrderRepairImages;
  public
    { public declarations }
    procedure SetRights(Editable: Boolean); override;
    procedure SetLanguage;
    destructor Destroy; override;
  end;

implementation
uses uData,uRowEditor,urepairimages,uIntfStrConsts;
{$R *.lfm}

{ TfRepairImageFrame }

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

destructor TfRepairImageFrame.Destroy;
begin
  if Assigned(fRepairImages) then
    FreeAndNil(fRepairImages);
  inherited Destroy;
end;

{ TfRepairImageFrame }


end.

