{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}

unit uwait;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, lresources, forms, controls, graphics, dialogs, StdCtrls,
  uIntfStrConsts, ExtCtrls, LMessages, Fileutil, ComCtrls;

type

  { TfWaitForm }

  TfWaitForm = class(tform)
    Image1: TImage;
    lStep: tlabel;
    lPleaseWait: tlabel;
    ProgressBar1: TProgressBar;
  private
    { private declarations }
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
  public
    { public declarations }
    procedure ShowInfo(Info : string);
    procedure SetLanguage;
  end; 

var
  fWaitForm: TfWaitForm;

implementation

{ TfWaitForm }

procedure TfWaitForm.WMCloseQuery(var message: TLMessage);
begin //Workaround for #0012552
  Close;
end;

procedure TfWaitForm.ShowInfo(Info: string);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfWaitForm,fWaitForm);
      Self := fWaitForm;
    end;
  BringToFront;
  lStep.Caption := info;
  Application.Processmessages;
end;

procedure TfWaitForm.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfWaitForm,fWaitForm);
      Self := fWaitForm;
    end;
end;

initialization
  {$I uwait.lrs}

end.
