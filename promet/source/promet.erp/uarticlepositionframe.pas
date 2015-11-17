{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}
unit uArticlePositionFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uExtControls,uPositionFrame,uBaseDBClasses,uPrometFramesInplace;
type

  { TfArticlePositionFrame }

  TfArticlePositionFrame = class(TPrometInplaceFrame)
    cbPLType: TExtDBCombobox;
    PosDS: TDatasource;
    lType: TLabel;
    Panel1: TPanel;
  private
    { private declarations }
    FPosFrame: TfPosition;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetDataSet(const AValue: TBaseDBDataSet);
    procedure SetRights(Editable : Boolean);override;
    procedure SetFocus; override;
    procedure ShowFrame; override;
    procedure DoRefresh(ForceRefresh: Boolean=False); override;
  end;
implementation
uses uDetailPositionFrame,utextpositionframe,uMasterdata;
{$R *.lfm}
resourcestring
  strPieceListTypeList          = 'L Im lager direkt buchen';
  strPieceListTypeOrder         = 'O In Auftrag auflösen';
  strPieceListTypeProduction    = 'P In produktion auflösen';
constructor TfArticlePositionFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosFrame := TfPosition.Create(Self);
  FPosFrame.Parent := Self;
  FPosFrame.Align:=alClient;
  FPosFrame.Show;
  FPosFrame.InplaceFrames[0] := TfDetailPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[3] := TfTextPositionFrame.Create(FPosFrame);
  FPosFrame.BaseName:='MASTERDATA';
  FPosFrame.FormName:='ARP';
  cbPLType.Items.Add(strPieceListTypeList);
  cbPLType.Items.Add(strPieceListTypeOrder);
  cbPLType.Items.Add(strPieceListTypeProduction);
end;
destructor TfArticlePositionFrame.Destroy;
begin
  FPosFrame.Free;
  inherited Destroy;
end;
procedure TfArticlePositionFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  FPosFrame.Dataset := AValue;
  PosDS.DataSet := TMDpos(AValue).Masterdata.DataSet;
end;
procedure TfArticlePositionFrame.SetRights(Editable: Boolean);
begin
  FPosFrame.SetRights(Editable);
end;

procedure TfArticlePositionFrame.SetFocus;
begin
  if CanFocus and Visible then
    inherited;
  FPosFrame.SetFocus;
end;

procedure TfArticlePositionFrame.ShowFrame;
begin
  inherited ShowFrame;
  SetFocus;
end;

procedure TfArticlePositionFrame.DoRefresh(ForceRefresh: Boolean);
begin
  FPosFrame.DoRefresh(ForceRefresh);
end;

end.

