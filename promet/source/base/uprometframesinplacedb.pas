{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uPrometFramesInplaceDB;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, uPrometFrames, uPrometFramesInplace,
  uBaseDBClasses;
type
  TPrometInplaceDBFrame = class(TPrometInplaceFrame)
  private
  protected
    FConnection: TComponent;
    FDataSet: TBaseDBDataSet;
    procedure SetDataSet(const AValue: TBaseDBDataSet);virtual;
  public
    property Connection : TComponent read FConnection;
    property DataSet : TBaseDBDataSet read FDataSet write SetDataSet;
    procedure SetLanguage;virtual;abstract;
    function OpenFromLink(aLink : string) : Boolean;virtual;abstract;
    procedure New;virtual;
    procedure DoRefresh;virtual;
  end;
implementation
{$R *.lfm}
procedure TPrometInplaceDBFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
end;
procedure TPrometInplaceDBFrame.New;
begin
end;
procedure TPrometInplaceDBFrame.DoRefresh;
begin
end;

end.
