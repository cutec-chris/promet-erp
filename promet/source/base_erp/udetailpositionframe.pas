{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uDetailPositionFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls, db,
  uPrometFramesInplace;

type
  TfDetailPositionFrame = class(TPrometInplaceFrame)
    cbPQuantityU: TDBComboBox;
    cbTexttyp: TDBComboBox;
    cbVersion: TDBComboBox;
    Position: TDatasource;
    eDQuantity: TDBEdit;
    ePDiscont: TDBEdit;
    ePPosprice: TDBEdit;
    ePQuantity: TDBEdit;
    ePWholeprice: TDBEdit;
    eSerial: TDBEdit;
    eShortText: TDBMemo;
    eTenderPos: TDBEdit;
    lDQuantity: TLabel;
    lLongtext: TLabel;
    lPDiscont: TLabel;
    lPQuantity: TLabel;
    lPSellprice: TLabel;
    lPWholeprice: TLabel;
    lQuantityUnit: TLabel;
    lSerial: TLabel;
    lShortText: TLabel;
    lTenderPos: TLabel;
    lTexttype: TLabel;
    lVersion: TLabel;
    mLongText: TDBMemo;
    procedure FrameEnter(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetRights(Editable : Boolean);override;
  end; 

implementation
{$R *.lfm}
uses uPositionFrame,uBaseERPDBClasses;
procedure TfDetailPositionFrame.FrameEnter(Sender: TObject);
begin
  with TfPosition(Owner).DataSet as TBaseDbPosition do
    begin
      Position.DataSet := DataSet;
    end;
end;

procedure TfDetailPositionFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

