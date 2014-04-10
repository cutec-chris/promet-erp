unit umaintasks;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, uPrometFrames,uTasks,uBaseDbClasses;
type
  { TfMainTaskFrame }
  TfMainTaskFrame = class(TPrometMainFrame)
  private
    { private declarations }
    FTasks : TfTaskFrame;
  public
    { public declarations }
    procedure SetDataSet(const AValue: TBaseDBDataset); override;
    property Tasks : TfTaskFrame read FTasks;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRefresh; override;
    procedure ShowFrame; override;
  end;

implementation
{$R *.lfm}

procedure TfMainTaskFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  DoOpen;
end;

{ TfMainTaskFrame }
constructor TfMainTaskFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTasks := TfTaskFrame.Create(Self);
  FTasks.GridView.TreeField := 'LPARENT';
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

procedure TfMainTaskFrame.ShowFrame;
begin
  FTasks.ShowFrame;
end;

end.

