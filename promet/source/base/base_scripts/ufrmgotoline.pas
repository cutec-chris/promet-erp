unit uFrmGotoLine;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEditTypes;

type
  TfrmGotoLine = class(TForm)
    edtCharNumber: TEdit;
    edtLineNumber: TEdit;
    Button1: TButton;
    btnGoto: TButton;
    lblLineNumber: TLabel;
    lblCharNumber: TLabel;
    procedure FormShow(Sender: TObject);
  private
    function GetCaret: TPoint;
    procedure SetCaret(const Value: TPoint);
    procedure SetChar(const Value: Integer);
    procedure SetLine(const Value: Integer);
    function GetChar: Integer;
    function GetLine: Integer;
    { Private declarations }
  public
    { Public declarations }
    property Char : Integer read GetChar write SetChar;
    property Line : Integer read GetLine write setLine;
    property CaretXY:TPoint read GetCaret write SetCaret;
  end;

var
  frmGotoLine: TfrmGotoLine;

implementation

{$R *.lfm}

{ TfrmGotoLine }

function TfrmGotoLine.GetCaret: TPoint;
begin
  Result.x := StrToInt(edtCharNumber.Text);
  Result.y := StrToInt(edtLineNumber.Text);
end;

function TfrmGotoLine.GetChar: Integer;
begin
  Result := StrToInt(edtCharNumber.Text)
end;

function TfrmGotoLine.GetLine: Integer;
begin
  Result := StrToInt(edtLineNumber.Text)
end;

procedure TfrmGotoLine.SetCaret(const Value: TPoint);
begin
  edtCharNumber.Text := IntToStr(Value.x);
  edtLineNumber.Text := IntToStr(Value.y);
end;

procedure TfrmGotoLine.SetChar(const Value: Integer);
begin
  edtCharNumber.Text := IntToStr(Value);
end;

procedure TfrmGotoLine.SetLine(const Value: Integer);
begin
  edtLineNumber.Text := IntToStr(Value);
end;

procedure TfrmGotoLine.FormShow(Sender: TObject);
begin
  edtLineNumber.SetFocus;
end;

end.
