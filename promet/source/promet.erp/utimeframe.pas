unit uTimeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,uPrometFrames;

type

  { TfTimeFrame }

  TfTimeFrame = class(TPrometMainFrame)
  private
    { private declarations }
  public
    { public declarations }
    destructor Destroy; override;
    procedure DoOpen;override;
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

end.

