unit umaintasks;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, uPrometFrames,uTasks;
type
  { TfMainTaskFrame }
  TfMainTaskFrame = class(TPrometMainFrame)
  private
    { private declarations }
    FTasks : TfTaskFrame;
  public
    { public declarations }
    property Tasks : TfTaskFrame read FTasks;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRefresh; override;
  end;

implementation
{$R *.lfm}
{ TfMainTaskFrame }
constructor TfMainTaskFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTasks := TfTaskFrame.Create(Self);
  FTasks.Parent := Self;
  fTasks.Align:=alClient;
  fTasks.Show;
end;
destructor TfMainTaskFrame.Destroy;
begin
  FTasks.Destroy;
  inherited Destroy;
end;
procedure TfMainTaskFrame.DoRefresh;
begin
  FTasks.DoRefresh;
end;

end.

