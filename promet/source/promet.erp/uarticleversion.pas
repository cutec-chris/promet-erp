unit uArticleVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type
  TfVersionate = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbActivate: TCheckBox;
    eVersion: TEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
  end;

var
  fVersionate: TfVersionate;

implementation

{$R *.lfm}

function TfVersionate.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfVersionate,fVersionate);
      Self := fVersionate;
    end;
  fVersionate.eVersion.Text:='';
  Result := ShowModal=mrOK;
end;

end.

