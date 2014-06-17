{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

program cashpoint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uerror, umain, upassword, uaddserial, udata, DBFLaz,
  lazreport, zcomponent, general;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfError, fError);
  fMain.DoCreate;
  Application.Run;
end.

