{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uRowEditor;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  db,dbgrids,uIntfStrConsts,Grids,Variants,LCLType, PairSplitter, ButtonPanel,
  uData, types;
type

  { TfRowEditor }

  TfRowEditor = class(TForm)
    bpButtons: TButtonPanel;
    lbSource: TListBox;
    lbDestination: TListBox;
    bRemoveRow: TBitBtn;
    bAddRow: TBitBtn;
    lTableRows: TLabel;
    lViewRows: TLabel;
    pLeft: TPanel;
    pRight: TPanel;
    Splitter1: TSplitter;
    procedure bAddRowClick(Sender: TObject);
    procedure bRemoveRowClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbDestinationDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbDestinationDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbDestinationStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure lbSourceDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbSourceStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    { private declarations }
    DestDragIndex: Integer;
    DestDropIndex: Integer;
    SourceDragIndex: Integer;
    SourceDropIndex: Integer;
  public
    { public declarations }
    function Execute(aConfigName : string;DataSource : TDataSource;Grid : TDBGrid; Filter : string = '') : Boolean;
    procedure SetGridSizes(aConfigName : string;DataSource : TDataSource;Grid : TDBGrid; Filter : string = '');
    function GetGridSizes(aConfigName : string;DataSource: TDataSource; Grid: TDBGrid;Defaults : string = '';MakereadOnly : Boolean = False; Filter : string = '') : Boolean;
  end;

var
  fRowEditor: TfRowEditor;

implementation
uses uBaseDBInterface;
procedure TfRowEditor.bAddRowClick(Sender: TObject);
begin
  if lbSource.ItemIndex > -1 then
    begin
      lbDestination.Items.Add(lbSource.Items[lbSource.ItemIndex]);
      lbSource.Items.Delete(lbSource.ItemIndex);
    end;
end;
procedure TfRowEditor.bRemoveRowClick(Sender: TObject);
begin
  if lbDestination.ItemIndex > -1 then
    begin
      lbSource.Items.Add(lbDestination.Items[lbDestination.ItemIndex]);
      lbDestination.Items.Delete(lbDestination.ItemIndex);
    end;
end;
procedure TfRowEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfRowEditor.lbDestinationDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  dropindex: Integer;
begin
  if Source = lbSource then
    bAddRowClick(Self)
  else
    begin
      dropindex:=TListBox(Sender).itemAtPos(point(x,y),false); {this is where to drop it}
      {The "move" call below  will shift items up or down as necessary from
      dropindex to the slot evacuated by the dragged object to make room for the
      dragged index at the dropindex location}
      if (dropindex>=0) and (DestDragIndex>=0) then
      begin
        TListBox(Sender).items.move(DestDragIndex, dropindex); {move the dragged item}
        TListBox(Sender).itemindex:=dropindex;  {keep it as the selected item}
      end;
    end;
end;

procedure TfRowEditor.lbDestinationDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbSource) or (Source = lbDestination);
end;
procedure TfRowEditor.lbDestinationStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DestDragIndex:=lbDestination.itemindex;
end;
procedure TfRowEditor.lbSourceDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dropindex: Integer;
begin
  if Source = lbDestination then
    bRemoveRowClick(Self)
  else
    begin
      dropindex:=TListBox(Sender).itemAtPos(point(x,y),false); {this is where to drop it}
      {The "move" call below  will shift items up or down as necessary from
      dropindex to the slot evacuated by the dragged object to make room for the
      dragged index at the dropindex location}
      if dropindex>=0 then
      begin
        TListBox(Sender).items.move(SourceDragIndex, dropindex); {move the dragged item}
        TListBox(Sender).itemindex:=dropindex;  {keep it as the selected item}
      end;
    end;
end;

procedure TfRowEditor.lbSourceStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  SourceDragIndex:=lbSource.ItemIndex;
end;

function BuildFieldDesc(Field : TField) : string;
begin
  if Assigned(Field) then
    Result := Field.DisplayLabel+' ('+Field.FieldName+')';
end;
function ExtractFieldName(Desc : string) : string;
begin
  Result := copy(Desc,pos('(',Desc)+1,length(Desc));
  Result := copy(Result,0,length(Result)-1);
end;
function TfRowEditor.Execute(aConfigName : string;DataSource: TDataSource; Grid: TDBGrid; Filter : string = ''): Boolean;
var
  i: Integer;
  s: String;
  b: Integer;
  a: Integer;
  tmp: String;
  ActControl: TWinControl;
  OK: Boolean;
begin
  ActControl := Screen.ActiveControl;
  if not Assigned(fRowEditor) then
    begin
      Application.CreateForm(TfRowEditor,fRowEditor);
      Self := fRowEditor;
    end;
  Result := False;
  lbSource.Items.Clear;
  lbDestination.Items.Clear;
  if not Assigned(DataSource) then exit;
  if not Assigned(Grid) then exit;
  for i := 0 to DataSource.DataSet.FieldCount-1 do
//    if DataSource.DataSet.Fields[i].FieldName <> 'SQL_ID' then
      lbSource.Items.Add(BuildFieldDesc(DataSource.DataSet.Fields[i]));
  for i := 0 to Grid.Columns.Count-1 do
    begin
      lbDestination.Items.Add(BuildFieldDesc(TColumn(Grid.Columns[i]).Field));
      if lbSource.Items.IndexOf(BuildFieldDesc(TColumn(Grid.Columns[i]).Field)) > -1 then
        lbSource.Items.Delete(lbSource.Items.IndexOf(BuildFieldDesc(TColumn(Grid.Columns[i]).Field)));
    end;
  OK := False;
  OK := Showmodal = mrOK;
  Result := OK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
  tmp := aConfigName;
  if Filter <> '' then
    tmp := tmp+','+Filter;
  if Result then
    with Application as IBaseDbInterface do
      begin
        s := 'GLOBALWIDTH:'+IntToStr(Grid.Width)+';';
        for i := 0 to lbDestination.Items.Count-1 do
          begin
            s := s+ExtractFieldName(lbDestination.Items[i])+':';
            b := -1;
            for a := 0 to Grid.Columns.Count-1 do
              if TColumn(Grid.Columns[a]).FieldName = ExtractFieldName(lbDestination.Items[i]) then
                b := TColumn(Grid.Columns[a]).Width;
            if b = -1 then b := 70;
            s := s+IntToStr(b)+';';
          end;
        DBConfig.WriteString('GRID:'+Uppercase(tmp),s);
        GetGridSizes(aConfigName,DataSource,Grid,'',False,Filter);
      end;
end;
procedure TfRowEditor.SetGridSizes(aConfigName : string;DataSource: TDataSource; Grid: TDBGrid; Filter : string = '');
var
  s: String;
  i: Integer;
  tmp: String;
begin
  if not Assigned(DataSource) then exit;
  if not Assigned(Grid) then exit;
  tmp := aConfigName;
  if Filter <> '' then
    tmp := tmp+','+Filter;
  s := 'GLOBALWIDTH:'+IntToStr(Grid.Width)+';';
  for i := 0 to Grid.Columns.Count-1 do
    s := s+TColumn(Grid.Columns[i]).FieldName+':'+IntToStr(TColumn(Grid.Columns[i]).Width)+';';
  if s <> '' then
    with Application as IBaseDbInterface do
      DBConfig.WriteString('GRID:'+Uppercase(tmp),s);
end;
function TfRowEditor.GetGridSizes(aConfigName : string;DataSource: TDataSource; Grid: TDBGrid;Defaults : string = '';MakereadOnly : Boolean = False; Filter : string = '') : Boolean;
var
  cl: TGridColumn;
  s: String;
  OldDS: TDataSource;
  tmp: String;
  tmpCols: TGridColumns;
  a: Integer;
  IsReadOnly: Boolean;
  GlobalWidth: LongInt = 0;
  Factor : real = 1;
  aConfig: TDataSet;
  i: Integer;
begin
  Result := False;
  if not Assigned(DataSource) then exit;
  if not Assigned(DataSource.DataSet) then exit;
  if not Assigned(Grid) then exit;
  tmp := aConfigName;
  if Filter <> '' then
    tmp := tmp+','+Filter;
  IsReadOnly := True;
  with Application as IBaseDbInterface do
    begin
    s := DBConfig.ReadString('GRID:'+Uppercase(tmp),Defaults);
    if ((s = '') and (Filter <> '')) and Data.IsSQLDB then
      begin
        aConfig := Data.GetNewDataSet('select '+Data.QuoteField('VALUE')+' from '+Data.QuoteField('OPTIONS')+' where '+Data.QuoteField('OPTION')+'='+Data.QuoteValue('GRID:'+Uppercase(tmp)));
        aConfig.Open;
        s := aConfig.FieldByName('VALUE').AsString;
        aConfig.Free;
      end;
    end;
  if s <> '' then
    begin
      tmpCols := TGridColumns.Create(Grid,TColumn);
      OldDS := Grid.DataSource;
      Grid.DataSource := nil;
      tmpCols.Assign(Grid.Columns);
      Grid.Columns.Clear;
      if copy(s,0,12) = 'GLOBALWIDTH:' then
        begin
          s := copy(s,pos(':',s)+1,length(s));
          GlobalWidth := StrToIntDef(copy(s,0,pos(';',s)-1),0);
          s := copy(s,pos(';',s)+1,length(s));
          if GlobalWidth <> 0 then
            Factor := Grid.Width/GlobalWidth;
        end;
      while pos(';',s) > 0 do
        begin
          cl := Grid.Columns.Add;
          TColumn(cl).ReadOnly := MakeReadOnly;
          TColumn(cl).FieldName := copy(s,0,pos(':',s)-1);
          s := copy(s,pos(':',s)+1,length(s));
          TColumn(cl).Width := StrToIntDef(copy(s,0,pos(';',s)-1),64);
          if TColumn(cl).Width = 0 then
            TColumn(cl).Width := 64;
          TColumn(cl).Width := round(TColumn(cl).Width * Factor);
          if TColumn(cl).Width > Grid.Width then
            TColumn(cl).Width := Grid.Width-30;
          s := copy(s,pos(';',s)+1,length(s));
          for a := 0 to tmpCols.Count-1 do
            if TColumn(tmpCols[a]).FieldName = TColumn(cl).FieldName then
              begin
                TColumn(cl).PickList.Assign(TColumn(tmpCols[a]).PickList);
                TColumn(cl).ButtonStyle := TColumn(tmpCols[a]).ButtonStyle;
                break;
              end;
          if not TColumn(cl).ReadOnly then
            IsReadOnly := false;
          if DataSource.DataSet.FieldDefs.IndexOf(TColumn(cl).FieldName) <> -1 then
            if (DataSource.DataSet.FieldDefs[DataSource.DataSet.FieldDefs.IndexOf(TColumn(cl).FieldName)].Size = 1)
            and ((TColumn(cl).FieldName = 'CHECKED')
              or (TColumn(cl).FieldName = 'ACTIVE')
              or (TColumn(cl).FieldName = 'ISPAUSE')
              )
            then
              begin
                TColumn(cl).ButtonStyle:=cbsCheckboxColumn;
                TColumn(cl).ValueChecked:='Y';
                TColumn(cl).ValueUnChecked:='N';
                TColumn(cl).ReadOnly := False;
                IsReadOnly := false;
              end;
        end;
      Grid.DataSource := OldDS;
      tmpCols.Free;
      Result := True;
      Grid.ReadOnly := IsReadOnly;
    end;
end;
initialization
  {$I uroweditor.lrs}
end.
