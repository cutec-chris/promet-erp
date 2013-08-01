{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uRichFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ActnList, Spin, StdCtrls, RichMemo, FontComboBox;

type

  { TfRichFrame }

  TfRichFrame = class(TFrame)
    acItalic: TAction;
    acBold: TAction;
    acStrikeOut: TAction;
    acUnderline: TAction;
    ActionList: TActionList;
    bStrikeout: TSpeedButton;
    cbFontColor: TColorButton;
    rmText: TRichMemo;
    bItalic: TSpeedButton;
    bBold: TSpeedButton;
    bUnderline: TSpeedButton;
    seFontSize: TSpinEdit;
    procedure acBoldExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure acStrikeOutExecute(Sender: TObject);
    procedure acUnderlineExecute(Sender: TObject);
    procedure cbFontChange(Sender: TObject);
    procedure cbFontColorColorChanged(Sender: TObject);
    procedure rmTextClick(Sender: TObject);
    procedure rmTextExit(Sender: TObject);
    procedure rmTextKeyPress(Sender: TObject; var Key: char);
    procedure seFontSizeChange(Sender: TObject);
  private
    { private declarations }
    actAttributes: TFontParams;
    OldSelStart : Integer;
    cbFont : TFontComboBox;
    FRichText : string;
    function GetPlainText: string;
    function GetReadOnly: Boolean;
    function GetRichText: string;
    procedure PositionChanged;
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetPlainText(const AValue: string);
    procedure SetRichText(const AValue: string);
  protected
  public
    { public declarations }
    constructor Create(AOwner : TComponent);override;
    property AsText : string read GetPlainText write SetPlainText;
    property AsRichText : string read GetRichText write SetRichText;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    procedure LoadFromStream(Stream : TStream);
    procedure SaveToStream(Stream : TStream);
    procedure Clear;
  end;

var
  fRichFrame: TfRichFrame;

implementation

{$R *.lfm}

{ TfRichFrame }

procedure TfRichFrame.rmTextClick(Sender: TObject);
begin
  if rmText.SelStart <> OldSelStart then
    PositionChanged;
end;

procedure TfRichFrame.rmTextExit(Sender: TObject);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  rmText.SaveRichText(ss);
  ss.Position:=0;
  FRichText := ss.DataString;
  ss.Free;
  {$IFDEF WINDOWS}
  FRichText := copy(FRichText,0,length(FRichText)-2);
  {$ENDIF}
end;

procedure TfRichFrame.acBoldExecute(Sender: TObject);
begin
  acBold.Checked:=not acBold.Checked;
  if acBold.Checked then
    actAttributes.Style:=actAttributes.Style+[fsBold]
  else
    actAttributes.Style:=actAttributes.Style-[fsBold];
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
end;

procedure TfRichFrame.acItalicExecute(Sender: TObject);
begin
  acItalic.Checked:=not acItalic.Checked;
  if acItalic.Checked then
    actAttributes.Style:=actAttributes.Style+[fsItalic]
  else
    actAttributes.Style:=actAttributes.Style-[fsItalic];
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
end;

procedure TfRichFrame.acStrikeOutExecute(Sender: TObject);
begin
  acStrikeout.Checked:=not acStrikeout.Checked;
  if acStrikeout.Checked then
    actAttributes.Style:=actAttributes.Style+[fsStrikeout]
  else
    actAttributes.Style:=actAttributes.Style-[fsStrikeout];
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
end;

procedure TfRichFrame.acUnderlineExecute(Sender: TObject);
begin
  acUnderline.Checked:=not acUnderline.Checked;
  if acUnderline.Checked then
    actAttributes.Style:=actAttributes.Style+[fsUnderline]
  else
    actAttributes.Style:=actAttributes.Style-[fsUnderline];
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
end;

procedure TfRichFrame.cbFontChange(Sender: TObject);
begin
  actAttributes.Name := cbFont.Text;
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
  rmText.SetFocus;
end;

procedure TfRichFrame.cbFontColorColorChanged(Sender: TObject);
begin
  actAttributes.Color := cbFontColor.ButtonColor;
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
end;

procedure TfRichFrame.rmTextKeyPress(Sender: TObject; var Key: char);
begin
  if rmText.SelStart <> OldSelStart then
    PositionChanged;
  rmText.Invalidate;
end;

procedure TfRichFrame.seFontSizeChange(Sender: TObject);
begin
  actAttributes.Size := seFontSize.Value;
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
  rmText.SetFocus;
end;

function TfRichFrame.GetPlainText: string;
var
  tmp: String;
begin
  tmp := rmText.Lines.Text;
  Result := tmp;
end;

procedure TfRichFrame.PositionChanged;
begin
  rmText.GetTextAttributes(rmText.SelStart, actAttributes);
  if actAttributes.Color <> cbFontColor.ButtonColor then
    cbFontColor.ButtonColor := actAttributes.Color;
  acBold.Checked := fsBold in actAttributes.Style;
  bItalic.Down := fsItalic in actAttributes.Style;
  bUnderline.Down := fsUnderline in actAttributes.Style;
  bStrikeout.Down := fsStrikeOut in actAttributes.Style;
  OldSelStart := rmText.SelStart;
  seFontSize.Value := actAttributes.Size;
  cbFont.Text:=ActAttributes.Name;
end;

procedure TfRichFrame.SetReadOnly(const AValue: Boolean);
begin
  rmText.ReadOnly:=AValue;
  acItalic.Enabled:=not AValue;
  acUnderline.Enabled:=not AValue;
  acBold.Enabled:=not AValue;
  acStrikeOut.Enabled:=not AValue;
end;

function TfRichFrame.GetReadOnly: Boolean;
begin
  Result := rmText.ReadOnly;
end;

function TfRichFrame.GetRichText: string;
begin
  Result := FRichText;
end;

procedure TfRichFrame.SetPlainText(const AValue: string);
begin
  rmText.Lines.Text := AValue;
  rmText.SelStart:=0;
  rmText.SelLength:=0;
  PositionChanged;
  Application.ProcessMessages;
end;

procedure TfRichFrame.SetRichText(const AValue: string);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(AValue);
  rmText.Clear;
  rmText.LoadRichText(ss);
  ss.Free;
  rmTextExit(rmText);
end;

constructor TfRichFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRichText := '';
  cbFont := TFontComboBox.Create(Self);
  cbFont.Parent := Self;
  cbFont.Left := 168;
  cbFont.Width := 160;
  cbFont.Top := 5;
  cbFont.UseItemFont:=True;
  cbFont.OnChange:=@cbFontChange;
end;

procedure TfRichFrame.LoadFromStream(Stream: TStream);
begin
  rmText.LoadRichText(Stream);
end;

procedure TfRichFrame.SaveToStream(Stream: TStream);
begin
  rmText.SaveRichText(Stream);
end;

procedure TfRichFrame.Clear;
begin
  rmText.Clear;
end;

end.
