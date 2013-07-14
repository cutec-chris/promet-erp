{soner.pas meine erweiterungen}

unit soner;

interface

uses
  Classes, Graphics;

 //originalen sahen schlecht oder ganz schwarz aus deshalb zeichne ich es selbst
 //todo soner noch nicht fertig, es wird nur rechteck gezeichnet.
function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; Style: Byte; IsRounded, IsDown,
  IsFocused: Boolean): TRect;
  
implementation

function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; Style: Byte; IsRounded, IsDown,
  IsFocused: Boolean): TRect;
begin
  Canvas.Rectangle(Client);
end;
  
end.

