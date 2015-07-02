unit uspreetsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, ComCtrls, StdCtrls,
  ColorBox, ExtCtrls, ValEdit, Dialogs, ActnList, Menus, fpspreadsheetgrid;

type
  TFrame1 = class(TFrame)
    AcAddColumn: TAction;
    AcAddRow: TAction;
    AcBorderAll: TAction;
    AcBorderBottom: TAction;
    AcBorderBottomDbl: TAction;
    AcBorderBottomMedium: TAction;
    AcBorderHCenter: TAction;
    AcBorderInner: TAction;
    AcBorderLeft: TAction;
    AcBorderNone: TAction;
    AcBorderOuter: TAction;
    AcBorderOuterMedium: TAction;
    AcBorderRight: TAction;
    AcBorderTop: TAction;
    AcBorderTopBottom: TAction;
    AcBorderTopBottomThick: TAction;
    AcBorderVCenter: TAction;
    AcCommentAdd: TAction;
    AcCommentDelete: TAction;
    AcCommentEdit: TAction;
    AcCopyFormat: TAction;
    AcCSVParams: TAction;
    AcCurrencySymbols: TAction;
    AcDecDecimals: TAction;
    AcDeleteColumn: TAction;
    AcDeleteRow: TAction;
    AcEdit: TAction;
    AcFont: TAction;
    AcFontBold: TAction;
    AcFontItalic: TAction;
    AcFontStrikeout: TAction;
    AcFontUnderline: TAction;
    AcFormatSettings: TAction;
    AcHorCenterAlign: TAction;
    AcHorDefaultAlign: TAction;
    AcIncDecimals: TAction;
    AcLeftAlign: TAction;
    AcMergeCells: TAction;
    AcNew: TAction;
    AcNFCurrency: TAction;
    AcNFCurrencyRed: TAction;
    AcNFCusstomMS: TAction;
    AcNFCustomMSZ: TAction;
    AcNFDayMonth: TAction;
    AcNFExp: TAction;
    AcNFFixed: TAction;
    AcNFFixedTh: TAction;
    AcNFGeneral: TAction;
    AcNFLongDate: TAction;
    AcNFLongTime: TAction;
    AcNFLongTimeAM: TAction;
    AcNFMonthDay: TAction;
    AcNFPercentage: TAction;
    AcNFShortDate: TAction;
    AcNFShortDateTime: TAction;
    AcNFShortTime: TAction;
    AcNFShortTimeAM: TAction;
    AcNFTimeInterval: TAction;
    AcOpen: TAction;
    AcQuit: TAction;
    AcRightAlign: TAction;
    AcSaveAs: TAction;
    AcShowGridlines: TAction;
    AcShowHeaders: TAction;
    AcSort: TAction;
    AcSortColAsc: TAction;
    AcTextHoriz: TAction;
    AcTextStacked: TAction;
    AcTextVertCCW: TAction;
    AcTextVertCW: TAction;
    ActionList: TActionList;
    AcVAlignBottom: TAction;
    AcVAlignCenter: TAction;
    AcVAlignDefault: TAction;
    AcVAlignTop: TAction;
    AcViewInspector: TAction;
    AcWordwrap: TAction;
    AddressPanel: TPanel;
    BordersPopupMenu: TPopupMenu;
    CbBackgroundColor: TColorBox;
    EdCellAddress: TEdit;
    FontComboBox: TComboBox;
    FontSizeComboBox: TComboBox;
    FormatToolBar: TToolBar;
    FormulaMemo: TMemo;
    FormulaToolBar: TToolBar;
    FormulaToolbarSplitter: TSplitter;
    ImageList: TImageList;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MnuCurrency: TMenuItem;
    MnuCurrencyRed: TMenuItem;
    MnuFmtDateTimeDM: TMenuItem;
    MnuFmtDateTimeMSZ: TMenuItem;
    MnuFmtDateTimeMY: TMenuItem;
    MnuLongDate: TMenuItem;
    MnuLongTime: TMenuItem;
    MnuLongTimeAM: TMenuItem;
    MnuNFExp: TMenuItem;
    MnuNFFixed: TMenuItem;
    MnuNFFixedTh: TMenuItem;
    MnuNFGeneral: TMenuItem;
    MnuNFPercentage: TMenuItem;
    MnuShortDate: TMenuItem;
    MnuShortDateTime: TMenuItem;
    MnuShortTime: TMenuItem;
    MnuShortTimeAM: TMenuItem;
    MnuTimeInterval: TMenuItem;
    NumFormatPopupMenu: TPopupMenu;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    TabControl: TTabControl;
    TbBorders: TToolButton;
    TbNumFormats: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton28: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    WorksheetGrid: TsWorksheetGrid;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

initialization
  {$I unit1.lrs}

end.

