unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, StdCtrls, ExtCtrls, JvDesignSurface, JvDesignUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    Active1: TMenuItem;
    ButtonButton: TToolButton;
    csDesigning1: TMenuItem;
    DelphiSelector1: TMenuItem;
    File1: TMenuItem;
    Grid1: TMenuItem;
    ImageButton: TToolButton;
    ImageList1: TImageList;
    JvDesignPanel: TJvDesignPanel;
    LabelButton: TToolButton;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    PanelButton: TToolButton;
    Rules1: TMenuItem;
    Save1: TMenuItem;
    SaveDialog: TSaveDialog;
    SelectButton: TToolButton;
    ToolBar1: TToolBar;
    VSSelector1: TMenuItem;
    WindowProcHook1: TMenuItem;
    procedure Active1Click(Sender: TObject);
    procedure csDesigning1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Rules1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure JvDesignPanelGetAddClass(Sender: TObject; var ioClass: String);
    procedure JvDesignPanelPaint(Sender: TObject);
    procedure PaletteButtonClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    DesignClass: string;
    StickyClass: Boolean;

  end; 

var
  MainForm: TMainForm;

implementation

uses
  JvDesignImp;
{$R *.lfm}

{ TMainForm }

procedure TMainForm.New1Click(Sender: TObject);
begin
    JvDesignPanel.Clear;
end;

procedure TMainForm.Grid1Click(Sender: TObject);
begin

end;

procedure TMainForm.csDesigning1Click(Sender: TObject);
begin
    JvDesignPanel.Active := false;
  if WindowProcHook1.Checked then
    JvDesignPanel.Surface.MessengerClass := TJvDesignWinControlHookMessenger
  else
    JvDesignPanel.Surface.MessengerClass := TJvDesignDesignerMessenger;
  JvDesignPanel.Active := true;
  JvDesignPanel.Invalidate;

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
     OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  JvDesignPanel.Surface.Active := true;

end;

procedure TMainForm.Active1Click(Sender: TObject);
begin
    JvDesignPanel.Active := Active1.Checked;
  JvDesignPanel.Invalidate;

end;

procedure TMainForm.Open1Click(Sender: TObject);
begin
    if OpenDialog.Execute then
    JvDesignPanel.LoadFromFile(OpenDialog.Filename);

end;

procedure TMainForm.Rules1Click(Sender: TObject);
begin
    if Rules1.Checked then
  begin
    JvDesignPanel.Color := clWhite;
    JvDesignPanel.DrawRules := true;
    JvDesignPanel.OnPaint := nil;
  end else
  begin
    JvDesignPanel.Color := clBtnFace;
    JvDesignPanel.DrawRules := false;
    JvDesignPanel.OnPaint := @JvDesignPanelPaint;
  end;
  JvDesignPanel.Invalidate;

end;

procedure TMainForm.Save1Click(Sender: TObject);
begin
    if SaveDialog.Execute then
    JvDesignPanel.SaveToFile(SaveDialog.Filename);

end;

procedure TMainForm.JvDesignPanelGetAddClass(Sender: TObject;
  var ioClass: String);
begin
  ioClass := DesignClass;
  if not StickyClass then
  begin
      DesignClass := '';
    SelectButton.Down  := true;
  end;

end;

procedure TMainForm.JvDesignPanelPaint(Sender: TObject);
begin
  with JvDesignPanel do
     DesignPaintGrid(Canvas, ClientRect, Color);

end;

procedure TMainForm.PaletteButtonClick(Sender: TObject);
const
  cClasses: array[0..4] of string = ( '', 'TButton', 'TLabel', 'TPanel',
    'TImage' );

begin
// StickyClass := (GetKeyState(VK_SHIFT) < 0);
    StickyClass := False;
   DesignClass := cClasses[TControl(Sender).Tag];

end;

initialization
  RegisterClass(TButton);
  RegisterClass(TLabel);
  RegisterClass(TPanel);
  RegisterClass(TImage);
end.

