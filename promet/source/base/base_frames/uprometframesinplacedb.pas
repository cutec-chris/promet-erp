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
Created 01.06.2006
*******************************************************************************}
unit uPrometFramesInplaceDB;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons,
  uPrometFrames, uPrometFramesInplace, uBaseDBClasses, uExtControls;
type

  { TPrometInplaceDBFrame }

  TPrometInplaceDBFrame = class(TPrometInplaceFrame)
  protected
    FConnection: TComponent;
    FDataSet: TBaseDBDataSet;
    procedure SetDataSet(const AValue: TBaseDBDataSet);virtual;
    procedure SetConnection(AValue: TComponent);virtual;
  public
    property Connection : TComponent read FConnection write SetConnection;
    property DataSet : TBaseDBDataSet read FDataSet write SetDataSet;
    procedure SetLanguage;virtual;abstract;
    function OpenFromLink(aLink : string) : Boolean;virtual;abstract;
    procedure New;virtual;
    procedure DoRefresh;override;
    procedure FrameAdded; override;
  end;
implementation
{$R *.lfm}

procedure TPrometInplaceDBFrame.SetConnection(AValue: TComponent);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
end;

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

procedure TPrometInplaceDBFrame.FrameAdded;
begin
  inherited FrameAdded;
end;

end.

