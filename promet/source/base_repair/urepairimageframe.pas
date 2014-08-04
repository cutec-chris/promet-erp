unit urepairimageframe;

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
    eSerial1: TDBEdit;
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
  private
    { private declarations }
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
    FOldRowHeight : Integer;
    gProblemsColumn: TRECT;
  public
    { public declarations }
  end;

implementation
uses uData,uRowEditor;
{$R *.lfm}

{ TfRepairPositionFrame }


end.

