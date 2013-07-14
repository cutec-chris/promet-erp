unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,FPimage,FPReadJPEG, types,
  uThumbViewer;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  scale : real = 1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  aThumbViewer: TThumbViewer;
begin
  aThumbViewer := TThumbViewer.Create(Self);
  aThumbViewer.Align:=alClient;
  aThumbViewer.Parent :=Self;
  aThumbViewer.Count:=100;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  {
  if ssCtrl in Shift then
    begin
      if (WheelDelta < 0) and (scale < 0.08) then exit;
      if (WheelDelta > 0) and (scale > 5) then exit;

      scale := scale+(WheelDelta/1000);
      Invalidate;
    end;
  }
end;

end.

