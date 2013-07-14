{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uTextFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, db,uPrometFramesInplaceDB;
type
  TfTextFrame = class(TPrometInplaceDBFrame)
    Datasource: TDatasource;
    mText: TDBMemo;
  private
    { private declarations }
  public
    { public declarations }
    procedure SetRights(Editable : Boolean);override;
  end; 

implementation

{$R *.lfm}

procedure TfTextFrame.SetRights(Editable: Boolean);
begin
  mText.ReadOnly:=not Editable;
end;

end.

