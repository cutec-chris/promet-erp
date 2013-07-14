{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uprometframesinplace;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, uPrometFrames, uExtControls;
type
  TPrometInplaceFrame = class(TExtControlFrame)
  public
    procedure SetRights(Editable : Boolean);virtual;abstract;
  published
    property OnEnter;
    property OnExit;
  end;
implementation
{$R *.lfm}
end.
