unit utimeregistration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, uPrometFrames;

type

  { TfTimeRegistration }

  TfTimeRegistration = class(TPrometMainFrame)
    cbUser: TComboBox;
    deFrom: TDateEdit;
    deTo: TDateEdit;
    lTo: TLabel;
    lFrom: TLabel;
    lUser: TLabel;
    pToolbar: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  fTimeRegistration: TfTimeRegistration;

implementation

{$R *.lfm}

{ TfTimeRegistration }

constructor TfTimeRegistration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TfTimeRegistration.Destroy;
begin
  inherited Destroy;
end;

end.

