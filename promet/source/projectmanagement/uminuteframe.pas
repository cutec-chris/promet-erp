unit uminuteframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, StdCtrls,
  Buttons, ComCtrls, Spin, EditBtn, uExtControls, DBZVDateTimePicker;

type
  TfMinuteFrame = class(TFrame)
    Bevel3: TBevel;
    eName: TDBMemo;
    Label3: TLabel;
    Label4: TLabel;
    lname: TLabel;
    mInfo: TDBMemo;
    Panel4: TPanel;
    Panel6: TPanel;
    pComponents: TPanel;
    pcPages: TExtMenuPageControl;
    sbMenue: TSpeedButton;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tsInfo: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

