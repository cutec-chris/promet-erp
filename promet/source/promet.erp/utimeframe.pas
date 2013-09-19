unit uTimeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,uPrometFrames,
  uEnterTime;

type

  { TfTimeFrame }

  TfTimeFrame = class(TPrometMainFrame)
  private
    { private declarations }
  public
    { public declarations }
    FTimeReg : TfEnterTime;
    destructor Destroy; override;
    procedure DoOpen;override;
    procedure ShowFrame; override;
  end;

implementation

{$R *.lfm}

{ TfTimeFrame }

destructor TfTimeFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TfTimeFrame.DoOpen;
begin
  inherited DoOpen;
end;

procedure TfTimeFrame.ShowFrame;
begin
  inherited ShowFrame;
  FTimeReg.SetActive;
end;

end.

