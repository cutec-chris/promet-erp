{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

unit uSysMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons;

type

  { TfSysMonitor }

  TfSysMonitor = class(TForm)
    mLog: TMemo;
    sbClean: TSpeedButton;
    tbTop: TToolBar;
    tbEnabled: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure sbCleanClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DoLog(s : string);
  end; 

var
  fSysMonitor: TfSysMonitor;

implementation

{ TfSysMonitor }

procedure TfSysMonitor.sbCleanClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfSysMonitor.FormCreate(Sender: TObject);
begin

end;

procedure TfSysMonitor.DoLog(s: string);
begin
  if Visible and tbEnabled.Down then
    begin
      mLog.Lines.Add(s);
      mLog.SelStart:=length(mLog.Lines.Text);
    end;
end;

initialization
  {$I usysmonitor.lrs}

end.
