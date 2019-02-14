unit unewnumberset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel;

type

  { TfNewNumberset }

  TfNewNumberset = class(TForm)
    ButtonPanel1: TButtonPanel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
  end;

var
  fNewNumberset: TfNewNumberset;

implementation

{$R *.lfm}

{ TfNewNumberset }

function TfNewNumberset.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfNewNumberset,fNewNumberset);
      Self := fNewNumberset;
    end;
  Result := Showmodal = mrOK;
end;

end.

