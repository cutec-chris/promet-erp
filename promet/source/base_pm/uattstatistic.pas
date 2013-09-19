unit uAttStatistic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls,gsGanttCalendar;

type
  TfAttStatistic = class(TForm)
    ButtonPanel1: TButtonPanel;
    lbItems: TListBox;
  private
    { private declarations }
  public
    { public declarations }
    procedure Execute(aInterval : TInterval);
  end;

var
  fAttStatistic: TfAttStatistic;

implementation

{$R *.lfm}

procedure TfAttStatistic.Execute(aInterval: TInterval);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfAttStatistic,fAttStatistic);
      Self := fAttStatistic;
    end;
  lbItems.Clear;
end;

end.

