unit upaygroups;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ButtonPanel;

type
  TfPaygroups = class(TForm)
    ButtonPanel1: TButtonPanel;
    Paygroups: TDataSource;
    DBGrid1: TDBGrid;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fPaygroups: TfPaygroups;

implementation

{$R *.lfm}

end.

