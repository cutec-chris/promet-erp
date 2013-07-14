{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}

unit uLogWait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, uIntfStrConsts;

type

  { TfLogWaitForm }

  TfLogWaitForm = class(TForm)
    bAbort: TBitBtn;
    lbLog: TListBox;
    procedure FormShow(Sender: TObject);
    procedure ShowInfo(Info: string);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
  end;

var
  fLogWaitForm: TfLogWaitForm;

implementation

{ TfLogWaitForm }

procedure TfLogWaitForm.ShowInfo(Info: string);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfLogWaitForm,fLogWaitform);
      Self := fLogWaitForm;
    end;
  if trim(info) = '' then exit;
  lbLog.Items.Add(StringReplace(StringReplace(Info,#10,'',[rfreplaceAll]),#13,'',[rfreplaceAll]));
  lbLog.ItemIndex := lbLog.Items.Count-1;
  lbLog.MakeCurrentVisible;
  Application.Processmessages;
end;

procedure TfLogWaitForm.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfLogWaitForm,fLogWaitform);
      Self := fLogWaitForm;
    end;
end;

procedure TfLogWaitForm.FormShow(Sender: TObject);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfLogWaitForm,fLogWaitform);
      Self := fLogWaitForm;
    end;
  lbLog.Items.Clear;
end;

initialization
  {$I ulogwait.lrs}

end.

