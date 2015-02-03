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
unit uDocumentOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, EditBtn, StdCtrls,
  uOptionsFrame;

type
  TfDocumentOptions = class(TOptionsFrame)
    eTempDirectory: TDirectoryEdit;
    ltempDirectory: TLabel;
    rbDeleteDoD522022: TRadioButton;
    rbDeleteNormal: TRadioButton;
    rbDeleteSecure: TRadioButton;
    rbOverride: TRadioButton;
  private
    { private declarations }
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
{$R *.lfm}
uses uBaseApplication,ubaseconfig;
procedure TfDocumentOptions.StartTransaction;
begin
  inherited StartTransaction;
  with BaseApplication as IBaseConfig do
    begin
      eTempDirectory.text := Config.ReadString('TEMPPATH','');
      case Config.ReadInteger('DELETEMETHOD',0) of
      0:rbDeleteNormal.Checked := True;
      1:rbDeleteSecure.Checked := True;
      2:rbDeleteDoD522022.Checked := True;
      3:rbOverride.Checked := True;
      end;
    end;
end;

procedure TfDocumentOptions.CommitTransaction;
begin
  with BaseApplication as IBaseConfig do
    begin
      Config.WriteString('TEMPPATH',eTempDirectory.text);
      if rbDeleteNormal.Checked then
        Config.WriteInteger('DELETEMETHOD',0)
      else if rbDeleteSecure.Checked then
        Config.WriteInteger('DELETEMETHOD',1)
      else if rbDeleteDoD522022.Checked then
        Config.WriteInteger('DELETEMETHOD',2)
      else if rbOverride.Checked then
        Config.WriteInteger('DELETEMETHOD',3);
    end;
  inherited CommitTransaction;
end;

procedure TfDocumentOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
end;

initialization

end.

