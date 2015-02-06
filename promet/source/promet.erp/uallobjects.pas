unit uAllObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseVisualApplication, ActnList;

implementation

procedure RegisterObjectTypes;
var
  aListAction: TAction;
  aNewAction: TAction;
begin
  aListAction := TAction.Create(nil);
  aNewAction := TAction.Create(nil);
  TVisualApplicationObjectType.Create(aNewAction,aListAction);
end;

initialization
  RegisterObjectTypes;
end.

