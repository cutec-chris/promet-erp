unit uTaskPlanOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, ButtonPanel, db,uBaseDbClasses;

type

  { TfUserPlanOptions }

  TfUserPlanOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    Datasource1: TDatasource;
    eUsage: TDBEdit;
    eWorkTime: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aUser : TUser) : Boolean;
  end;

var
  fUserPlanOptions: TfUserPlanOptions;

implementation

{$R *.lfm}

{ TfUserPlanOptions }

function TfUserPlanOptions.Execute(aUser: TUser): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfUserPlanOptions,fUserPlanOptions);
      Self := fUserPlanOptions;
    end;
  Datasource1.DataSet := aUser.DataSet;
  if Showmodal = mrOK then
    begin
      if aUser.CanEdit then
        aUser.Post
    end
  else if aUser.CanEdit then aUser.DataSet.Cancel;
end;

end.

