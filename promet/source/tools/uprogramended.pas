unit uProgramEnded;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel;

type

  { TfProgramEnded }

  TfProgramEnded = class(TForm)
    bpButtons: TButtonPanel;
    lMessage: TLabel;
    cbDontShowthisDialogAgain: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
    property Filename : string read FFilename write SetFilename;
  end; 

var
  fProgramEnded: TfProgramEnded;

implementation
{$R *.lfm}

{ TfProgramEnded }

resourcestring
  strProgramEndedText           = 'Das Programm mit dem die Datei : "%s" gestartet wurde, scheint beendet worden zu sein. Falls nicht klicken Sie OK erst wenn Sie es wirklich beendet haben.';


procedure TfProgramEnded.FormCreate(Sender: TObject);
begin
  bpButtons.OKButton.OnClick:=@OKButtonClick;
  SetLanguage;
end;

procedure TfProgramEnded.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfProgramEnded.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  lMessage.Caption := Format(strProgramEndedText,[Filename]);
end;

procedure TfProgramEnded.SetLanguage;
begin
end;

initialization

end.
