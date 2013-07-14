unit uAddNote;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, lresources, forms, controls, graphics, dialogs, Buttons,
  StdCtrls,uGeneralStrConsts;

type

  { TfAddNote }

  TfAddNote = class(tform)
    bAbort: tbutton;
    bSubmit: tbutton;
    mNote: tmemo;
    procedure babortclick(sender: tobject);
    procedure bsubmitclick(sender: tobject);
  private
    { private declarations }
  public
    { public declarations }
    Add : Boolean;
    procedure SetLanguage;
  end;

var
  fAddNote: TfAddNote;

implementation

{ TfAddNote }

procedure tfaddnote.bsubmitclick(sender: tobject);
begin
  Add := True;
  Close;
end;

procedure tfaddnote.babortclick(sender: tobject);
begin
  Add := False;
  Close;
end;

procedure tfaddnote.setlanguage;
begin
  Caption := strAddNote;
  bSubmit.Caption := strSubmit;
  bAbort.Caption := strAbort;
end;

initialization
  {$I uaddnote.lrs}

end.

