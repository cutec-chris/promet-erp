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
info@cu-tec.de
*******************************************************************************}

unit uSysMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons;

type

  { TfSysMonitor }

  TfSysMonitor = class(TForm)
    mLog: TMemo;
    sbClean: TSpeedButton;
    tbTop: TToolBar;
    tbEnabled: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure sbCleanClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DoLog(s : string);
  end; 

var
  fSysMonitor: TfSysMonitor;

implementation

{ TfSysMonitor }

procedure TfSysMonitor.sbCleanClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfSysMonitor.FormCreate(Sender: TObject);
begin

end;

procedure TfSysMonitor.DoLog(s: string);
begin
  if Visible and tbEnabled.Down then
    begin
      mLog.Lines.Add(s);
      mLog.SelStart:=length(mLog.Lines.Text);
    end;
end;

initialization
  {$I usysmonitor.lrs}

end.
