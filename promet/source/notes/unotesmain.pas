unit unotesmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  StdCtrls
  {$IFDEF WINDOWS}
  ,Windows
  {$ENDIF}
  ;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  Brush.Style := bsClear;
  BorderStyle := bsNone;
  {$ENDIF}
end;

procedure TForm1.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  {$IFDEF WINDOWS}
  Params.WndParent:=GetDesktopWindow;
  Params.Style := Params.Style or WS_CHILD;
  Params.ExStyle := Params.ExStyle OR WS_EX_TRANSPARENT;
  {$ENDIF}
end;

end.

