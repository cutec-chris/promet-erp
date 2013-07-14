unit uCall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfCall }

  TfCall = class(TForm)
    bAnswer: TBitBtn;
    lNotes: TLabel;
    lfrom: TLabel;
    lIncommingCall: TLabel;
    mNotes: TMemo;
    bNew: TSpeedButton;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fCall: TfCall;

implementation

initialization
  {$I ucall.lrs}

end.

