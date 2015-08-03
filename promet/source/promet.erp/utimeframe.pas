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
Created 01.08.2013
*******************************************************************************}
unit uTimeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,uPrometFrames,
  uEnterTime;

type

  { TfTimeFrame }

  TfTimeFrame = class(TPrometMainFrame)
  private
    { private declarations }
  public
    { public declarations }
    FTimeReg : TfEnterTime;
    destructor Destroy; override;
    procedure DoOpen;override;
    procedure ShowFrame; override;
  end;

implementation

uses uBaseVisualApplication,uIntfStrConsts,uBaseDBInterface;
{$R *.lfm}

{ TfTimeFrame }

destructor TfTimeFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TfTimeFrame.DoOpen;
begin
  inherited DoOpen;
end;

procedure TfTimeFrame.ShowFrame;
begin
  inherited ShowFrame;
  FTimeReg.SetActive;
end;
initialization
//  TBaseVisualApplication(Application).RegisterForm(TfTimeFrame);
end.

