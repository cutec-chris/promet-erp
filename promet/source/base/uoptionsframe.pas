{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TOptionsFrame }

  TOptionsFrame = class(TFrame)
  private
    FInTransaction: Boolean;
  public
    procedure StartTransaction;virtual;
    procedure CommitTransaction;virtual;
    procedure RollbackTransaction;virtual;
    property InTransaction : Boolean read FInTransaction;
    procedure SetLanguage;virtual;
  end;

implementation

{ TOptionsFrame }

procedure TOptionsFrame.StartTransaction;
begin
  FInTransaction := True;
end;

procedure TOptionsFrame.CommitTransaction;
begin
  FInTransaction := False;
end;

procedure TOptionsFrame.RollbackTransaction;
begin
  FInTransaction := False;
end;

procedure TOptionsFrame.SetLanguage;
begin
end;

end.
