unit uprojectdispoframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Spin,
  ActnList, Buttons, uPrometFrames, uPrometFramesInplace, uBaseDBClasses;

type
  TfProjectDispoFrame = class(TPrometMainFrame)
    acRefresh: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    seProjects: TSpinEdit;
    procedure acRefreshExecute(Sender: TObject);
  private
    { private declarations }
    StartDay : TDate;
    Projects : TList;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;



implementation
{$R *.lfm}
uses uProjects,uData;
procedure TfProjectDispoFrame.acRefreshExecute(Sender: TObject);
var
  aProjects: TProjectList;
  aProject: TProject;
begin
  Projects.Clear;
  aProjects := TProjectList.Create(nil,Data);
  aProjects.Open;
  with aProjects.DataSet do
    begin
      while not Eof  do
        begin
          aProject := TProject.Create(nil,Data);
          aProject.Select(aProjects.Id.AsVariant);
          aProject.Open;
//          aProject.Tasks.Open;
          Projects.Add(aProject);
          Next;
        end;
    end;
  aProjects.Free;
end;

constructor TfProjectDispoFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartDay:=Now();
  Projects := TList.Create;
end;

destructor TfProjectDispoFrame.Destroy;
begin
  Projects.Free;
  inherited Destroy;
end;

end.
