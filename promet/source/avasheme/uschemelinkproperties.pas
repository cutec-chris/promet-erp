unit uschemelinkproperties;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, usimplegraph, ExtCtrls, StdCtrls, CheckLst, ComCtrls;

type
  TfLinkProperties = class(TForm)
    Label1: TLabel;
    LinkLabel: TEdit;
    Style: TGroupBox;
    StyleSolid: TRadioButton;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    StyleDash: TRadioButton;
    StyleDot: TRadioButton;
    Colors: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    LinkLineColor: TPanel;
    LinkStyleColor: TPanel;
    btnChangeFont: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    btnApply: TButton;
    Label4: TLabel;
    AllOptions: TCheckListBox;
    LabelPlacement: TGroupBox;
    Edit4: TEdit;
    LabelPosition: TUpDown;
    Size: TGroupBox;
    Edit1: TEdit;
    PenWidth: TUpDown;
    LineBegin: TGroupBox;
    Label5: TLabel;
    LineBeginStyle: TComboBox;
    Label6: TLabel;
    Edit2: TEdit;
    LineBeginSize: TUpDown;
    LineEnd: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    LineEndStyle: TComboBox;
    Edit3: TEdit;
    LineEndSize: TUpDown;
    LineColor: TShape;
    StyleColor: TShape;
    Label9: TLabel;
    Label10: TLabel;
    Edit5: TEdit;
    LabelSpacing: TUpDown;
    procedure LinkLineColorClick(Sender: TObject);
    procedure LinkStyleColorClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    S: TEvsSimpleGraph;
    L: TEvsGraphObjectList;
    procedure SetObjectOptions(Value: TEvsGraphObjectOptions);
    function GetObjectOptions: TEvsGraphObjectOptions;
    procedure SetLinkOptions(Value: TEvsGraphLinkOptions);
    function GetLinkOptions: TEvsGraphLinkOptions;
    procedure ApplyChanges;
  public
    class function Execute(Links: TEvsGraphObjectList): Boolean;
  end;

implementation

{$R *.lfm}

{ TfLinkProperties }

class function TfLinkProperties.Execute(Links: TEvsGraphObjectList): Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      L := Links;
      with TEvsGraphLink(Links[0]) do
      begin
        S := Owner;
        LinkLabel.Text := Text;
        LabelPosition.Position := TextPosition;
        LabelSpacing.Position := TextSpacing;
        PenWidth.Position := Pen.Width;
        case Pen.Style of
          psSolid: StyleSolid.Checked := True;
          psDash: StyleDash.Checked := True;
          psDot: StyleDot.Checked := True;
        end;
        LineColor.Brush.Color := Pen.Color;
        StyleColor.Brush.Color := Brush.Color;
        LineBeginStyle.ItemIndex := Ord(BeginStyle);
        LineBeginSize.Position := BeginSize;
        LineEndStyle.ItemIndex := Ord(EndStyle);
        LineEndSize.Position := EndSize;
        FontDialog.Font := Font;
        SetObjectOptions(Options);
        SetLinkOptions(LinkOptions);
      end;
      if ShowModal = mrOK then
      begin
        ApplyChanges;
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TfLinkProperties.ApplyChanges;
var
  I: Integer;
begin
  S.BeginUpdate;
  try
    for I := 0 to L.Count - 1 do
      with TEvsGraphLink(L[I]) do
      begin
        BeginUpdate;
        try
          Text := LinkLabel.Text;
          TextPosition := LabelPosition.Position;
          TextSpacing := LabelSpacing.Position;
          Pen.Width := PenWidth.Position;
          if StyleSolid.Checked then
            Pen.Style := psSolid
          else if StyleDash.Checked then
            Pen.Style := psDash
          else if StyleDot.Checked then
            Pen.Style := psDot;
          Pen.Color := LineColor.Brush.Color;
          Brush.Color := StyleColor.Brush.Color;
          BeginStyle := TEvsLinkBeginEndStyle(LineBeginStyle.ItemIndex);
          BeginSize := LineBeginSize.Position;
          EndStyle := TEvsLinkBeginEndStyle(LineEndStyle.ItemIndex);
          EndSize := LineEndSize.Position;
          Font := FontDialog.Font;
          Options := GetObjectOptions;
          LinkOptions := GetLinkOptions;
        finally
          EndUpdate;
        end;
      end;
  finally
    S.EndUpdate;
  end;
end;

procedure TfLinkProperties.LinkLineColorClick(Sender: TObject);
begin
  ColorDialog.Color := LineColor.Brush.Color;
  if ColorDialog.Execute then
    LineColor.Brush.Color := ColorDialog.Color;
end;

procedure TfLinkProperties.LinkStyleColorClick(Sender: TObject);
begin
  ColorDialog.Color := StyleColor.Brush.Color;
  if ColorDialog.Execute then
    StyleColor.Brush.Color := ColorDialog.Color;
end;

procedure TfLinkProperties.btnChangeFontClick(Sender: TObject);
begin
  FontDialog.Execute;
end;

procedure TfLinkProperties.btnApplyClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TfLinkProperties.FormCreate(Sender: TObject);
begin
  SetBounds(Screen.Width - Width - 30, 50, Width, Height);
end;

function TfLinkProperties.GetObjectOptions: TEvsGraphObjectOptions;
var
  Option: TEvsGraphObjectOption;
begin
  Result := [];
  for Option := Low(TEvsGraphObjectOption) to High(TEvsGraphObjectOption) do
    if AllOptions.Checked[Ord(Option)] then
      Include(Result, Option);
end;

procedure TfLinkProperties.SetObjectOptions(Value: TEvsGraphObjectOptions);
var
  Option: TEvsGraphObjectOption;
begin
  for Option := Low(TEvsGraphObjectOption) to High(TEvsGraphObjectOption) do
    AllOptions.Checked[Ord(Option)] := Option in Value;
end;

function TfLinkProperties.GetLinkOptions: TEvsGraphLinkOptions;
var
  Option: TEvsGraphLinkOption;
begin
  Result := [];
  for Option := Low(TEvsGraphLinkOption) to High(TEvsGraphLinkOption) do
    if AllOptions.Checked[Ord(Option) + Ord(High(TEvsGraphObjectOption)) + 1] then
      Include(Result, Option);
end;

procedure TfLinkProperties.SetLinkOptions(Value: TEvsGraphLinkOptions);
var
  Option: TEvsGraphLinkOption;
begin
  for Option := Low(TEvsGraphLinkOption) to High(TEvsGraphLinkOption) do
    AllOptions.Checked[Ord(Option) + Ord(High(TEvsGraphObjectOption)) + 1] := Option in Value;
end;

end.
