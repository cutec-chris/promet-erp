unit uRepairPositionFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  DBGrids, uPositionFrame, Grids, DbCtrls, ExtDlgs, Menus, uExtControls;

type
 { TfRepairPositionFrame }

  TfRepairPositionFrame = class(TFrame)
    cbOperation: TDBComboBox;
    cbVersion1: TDBComboBox;
    cbWarrenty: TDBCheckBox;
    dnRepairPos: TDBNavigator;
    eSerial1: TDBEdit;
    gProblems: TExtDBGrid;
    lErrordescription: TLabel;
    lFoundProblems: TLabel;
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
    procedure cbVersion1DropDown(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure gProblemsColExit(Sender: TObject);
    procedure gProblemsColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer
      );
    procedure gProblemsColumnSized(Sender: TObject);
    procedure gProblemsDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure gProblemsSelectEditor(Sender: TObject; Column: TColumn;
      var Editor: TWinControl);
  private
    { private declarations }
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FOldRowHeight : Integer;
    gProblemsColumn: TRECT;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetBounds(var Msg: TGridMessage); message GM_SETBOUNDS;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
  public
    { public declarations }
  end;

implementation
uses uData,uRowEditor;
{$R *.lfm}

{ TfRepairPositionFrame }

procedure TfRepairPositionFrame.FrameEnter(Sender: TObject);
begin
  FOldRowHeight := TStringGrid(fGrid).RowHeights[FRow];
  if Self.Constraints.MinHeight > TStringGrid(fGrid).RowHeights[FRow] then
    TStringGrid(fGrid).RowHeights[FRow] := Self.Constraints.MinHeight;
end;

procedure TfRepairPositionFrame.cbVersion1DropDown(Sender: TObject);
begin
//  fMasterdata.HideDetails(false);
//  fMasterdata.OpenMasterdata;
//  if fMain.ControlPanels.IndexOf(fMasterdata) <> -1 then
//    cbVersion.Items.Assign(fMasterdata.cbVersion.Items);
//  cbVersion1.Items.Assign(cbVersion.Items);
end;

procedure TfRepairPositionFrame.FrameExit(Sender: TObject);
begin
  TStringGrid(fGrid).RowHeights[FRow] := FOldRowHeight;
end;

procedure TfRepairPositionFrame.gProblemsColExit(Sender: TObject);
var
  i: Integer;
  PartColumn: TColumn = nil;
begin
  if gProblems.SelectedField.FieldName <> 'ASSEMBLY' then exit;
  for i := 0 to gProblems.Columns.Count-1 do
    if gProblems.Columns[i].FieldName = 'PART' then
      begin
        PartColumn := gProblems.Columns[i];
        break;
      end;
  if not Assigned(PartColumn) then exit;
  PartColumn.PickList.Clear;
  {
  if not Data.RepairAssembly.DataSet.Active then exit;
  if Data.Locate(Data.RepairAssembly,'ASSEMBLY',Data.OrderRepairDetail.FieldByName('ASSEMBLY').AsString,[]) then
    begin
      Data.RepairParts.DataSet.First;
      while not Data.RepairParts.DataSet.EOF do
        begin
          PartColumn.PickList.Add(Data.RepairParts.FieldByName('PART').AsString);
          Data.RepairParts.DataSet.Next;
        end;
    end;
  }
end;

procedure TfRepairPositionFrame.gProblemsColumnMoved(Sender: TObject;
  FromIndex, ToIndex: Integer);
begin
  fRowEditor.SetGridSizes('REPAIR',gProblems.DataSource,gProblems);
end;

procedure TfRepairPositionFrame.gProblemsColumnSized(Sender: TObject);
begin
  fRowEditor.SetGridSizes('REPAIR',gProblems.DataSource,gProblems);
end;

procedure TfRepairPositionFrame.gProblemsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
    gProblemsColumn := Rect;
end;

procedure TfRepairPositionFrame.gProblemsSelectEditor(Sender: TObject;
  Column: TColumn; var Editor: TWinControl);
var
  aFilter: String;
begin
  {
  if not pREDetails.IsVisible then exit;
  if not Data.OrderPos.DataSet.Active then exit;
  if Column.FieldName = 'ERROR' then exit;
  if Column.FieldName = 'ASSEMBLY' then
    begin
      Column.PickList.Clear;
      aFilter := Data.GetFilter(Data.Masterdata);
      if Stringreplace(aFilter,' ','',[rfReplaceAll]) <> Data.QuoteField('ID')+'='Data.QuoteValue(Data.OrderPos.FieldByName('IDENT').AsString) then
        begin
          Data.SetFilter(Data.Masterdata,Data.QuoteField('ID')+'='+Data.QuoteValue(Data.OrderPos.FieldByName('IDENT').AsString));
          if Data.Locate(Data.Masterdata,'ID;VERSION',VararrayOf([Data.OrderPos.FieldByName('IDENT').AsVariant,Data.OrderPos.FieldByName('VERSION').AsVariant]),[]) then
            fMasterdata.OpenMasterdata
          else
            exit;
        end
      else
        begin
          if Data.Masterdata.FieldByName('VERSION').AsVariant = Data.OrderPos.FieldByName('VERSION').AsVariant then
            begin
              if not Data.RepairAssembly.DataSet.Active then
                fMasterdata.OpenMasterdata;
            end
          else
            begin
              if Data.Locate(Data.Masterdata,'ID;VERSION',VararrayOf([Data.OrderPos.FieldByName('IDENT').AsVariant,Data.OrderPos.FieldByName('VERSION').AsVariant]),[]) then
                fMasterdata.OpenMasterdata
              else if Data.Locate(Data.Masterdata,'ID',VararrayOf([Data.OrderPos.FieldByName('IDENT').AsVariant]),[]) then
                fMasterdata.OpenMasterdata
              else
                exit;
            end;
        end;
      with Data.RepairAssembly.DataSet do
        begin
          First;
          while not eof do
            begin
              Column.PickList.Add(FieldbyName('ASSEMBLY').AsString);
              Next;
            end;
        end;
    end;
    }
end;

procedure TfRepairPositionFrame.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=0;
end;

procedure TfRepairPositionFrame.msg_SetBounds(var Msg: TGridMessage);
var
  aWidth : Integer = 0;
  i: Integer;
begin
  with Msg.CellRect do
    begin
      if Top < 0 then
        begin
          TStringGrid(FGrid).EditorMode := False;
          exit;
        end;
      for i := 1 to TStringGrid(FGrid).Columns.Count-1 do
        aWidth := aWidth+TStringGrid(FGrid).Columns[i].Width;
      SetBounds(TStringGrid(FGrid).Columns[0].Width, Top, aWidth-1, (Bottom-Top)-1);
    end;
end;

procedure TfRepairPositionFrame.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

end.
