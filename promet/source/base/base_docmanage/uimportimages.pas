unit uImportImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type
  TfPicImport = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbDelete: TCheckBox;
    eTags: TEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
  end;

var
  fPicImport: TfPicImport;

implementation

{$R *.lfm}

function TfPicImport.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfPicImport,fPicImport);
      Self := fPicImport;
    end;
  eTags.SelectAll;
  Result := ShowModal=mrOK;
end;

end.

