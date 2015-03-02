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
Created 01.06.2006
*******************************************************************************}
unit uRowEditor;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  db,dbgrids,uIntfStrConsts,Grids,Variants,LCLType, PairSplitter, ButtonPanel,
  uData, types;
type

  { TfRowEditor }

  TfRowEditor = class(TForm)
    bpButtons: TButtonPanel;
    cbProcent: TCheckBox;
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
{$R *.lfm}
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
    lbSource.Items.Add(BuildFieldDesc(DataSource.DataSet.Fields[i]));
  for i := 0 to Grid.Columns.Count-1 do
    begin
      lbDestination.Items.Add(BuildFieldDesc(TColumn(Grid.Columns[i]).Field));
      if lbSource.Items.IndexOf(BuildFieldDesc(TColumn(Grid.Columns[i]).Field)) > -1 then
        lbSource.Items.Delete(lbSource.Items.IndexOf(BuildFieldDesc(TColumn(Grid.Columns[i]).Field)));
    end;
  tmp := aConfigName;
  if Filter <> '' then
    tmp := tmp+','+Filter;
  with Application as IBaseDbInterface do
    s := DBConfig.ReadString('GRID:'+Uppercase(tmp),'GLOBALWIDTH:%;');
  cbProcent.Checked:=copy(s,0,14)='GLOBALWIDTH:%;';
  OK := False;
  OK := Showmodal = mrOK;
  Result := OK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
  if Result then
    with Application as IBaseDbInterface do
      begin
        if cbProcent.Checked then
          s := 'GLOBALWIDTH:%;'
        else
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
const
  SBWidth = 31;
  MIN_WIDTH = 15;
procedure TfRowEditor.SetGridSizes(aConfigName : string;DataSource: TDataSource; Grid: TDBGrid; Filter : string = '');
var
  s: String;
  i: Integer;
  tmp: String;
  FullWidth : Integer = 0;
  IsReadOnly: Boolean = True;
  a: Integer;
begin
  Grid.AutoFillColumns:=False;
  if not Assigned(DataSource) then exit;
  if not Assigned(Grid) then exit;
  tmp := aConfigName;
  if Filter <> '' then
    tmp := tmp+','+Filter;
  with Application as IBaseDbInterface do
    s := DBConfig.ReadString('GRID:'+Uppercase(tmp),'GLOBALWIDTH:%;');
  if copy(s,0,14)='GLOBALWIDTH:%;' then
    s := 'GLOBALWIDTH:%;'
  else
    s := 'GLOBALWIDTH:'+IntToStr(Grid.Width)+';';

  for i := 0 to Grid.Columns.Count-1 do
    begin
      for a := 0 to Grid.Columns.Count-1 do
        if Grid.Columns[a].Index = i then
          begin
            s := s+TColumn(Grid.Columns[a]).FieldName+':'+IntToStr(TColumn(Grid.Columns[a]).Width)+';';
            FullWidth := FullWidth+TColumn(Grid.Columns[i]).Width;
            if not TColumn(Grid.Columns[a]).ReadOnly then
              IsReadOnly := false;
          end;
    end;
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
  ib : IBaseDbInterface;
  FullWidth: Integer;
  s1: String;
  GridWidth: Integer;
  Percentage: Boolean;
  sl: String;
begin
  Result := False;
  if not Assigned(DataSource) then exit;
  if not Assigned(DataSource.DataSet) then exit;
  if not Assigned(Grid) then exit;
  if not Supports(Application,IBaseDBInterface,ib) then exit;
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
      Grid.AutoFillColumns:=False;
      tmpCols.Assign(Grid.Columns);
      Grid.Columns.Clear;
      GridWidth := Grid.Width;
      sl := s;
      if copy(s,0,12) = 'GLOBALWIDTH:' then
        begin
          s := copy(s,pos(':',s)+1,length(s));
          Percentage := copy(s,0,pos(';',s)-1)='%';
          GlobalWidth := StrToIntDef(copy(s,0,pos(';',s)-1),0);
          s := copy(s,pos(';',s)+1,length(s));
          if GlobalWidth <> 0 then
            Factor := GridWidth/GlobalWidth;
        end;
      s1 := s;
      //see if Width of all columns > Grid.Width
      FullWidth := 0;
      while pos(';',s) > 0 do
        begin
          s := copy(s,pos(':',s)+1,length(s));
          FullWidth := FullWidth+StrToIntDef(copy(s,0,pos(';',s)-1),64);
          s := copy(s,pos(';',s)+1,length(s));
        end;
      if Percentage and (FullWidth>0) then
        Factor := Factor+(((Grid.Width-SBWidth)/FullWidth)-1);
      if Factor < 0.2 then Factor := 1;
      s := s1;
      //Collect Columns
      while pos(';',s) > 0 do
        begin
          cl := Grid.Columns.Add;
          TColumn(cl).SizePriority:=0;
          TColumn(cl).ReadOnly := MakeReadOnly;
          TColumn(cl).FieldName := copy(s,0,pos(':',s)-1);
          s := copy(s,pos(':',s)+1,length(s));
          TColumn(cl).Width := StrToIntDef(copy(s,0,pos(';',s)-1),64);
          if TColumn(cl).Width = 0 then
            TColumn(cl).Width := 64;
          TColumn(cl).Width := round(TColumn(cl).Width * Factor);
          if TColumn(cl).Width<MIN_WIDTH then
            TColumn(cl).Width := MIN_WIDTH;
          if TColumn(cl).Width > Grid.Width then
            TColumn(cl).Width := Grid.Width-SBWidth;
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
      cl.Width:=cl.Width-2;
      cl.SizePriority := 1;
      Grid.DataSource := OldDS;
      tmpCols.Free;
      Result := True;
      Grid.ReadOnly := IsReadOnly;
      {
      Grid.AutoFillColumns:=copy(sl,0,12) = 'GLOBALWIDTH:';
      if Grid.AutoFillColumns then
        Grid.ScrollBars:=ssVertical
      else
        Grid.ScrollBars:=ssAutoHorizontal;
      Grid.AutoSizeColumns;
      }
    end;
end;
initialization
end.

