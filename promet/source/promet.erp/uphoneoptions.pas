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
unit uPhoneOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, CheckLst,
  uOptionsFrame,ubaseconfig;

type
  TfPhoneOptions = class(TOptionsFrame)
    clbAllowedPhoneLines: TCheckListBox;
    lAllowedPhoneLines: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
uses uBaseApplication, uPhones;
procedure TfPhoneOptions.StartTransaction;
var
  i: Integer;
  tmp: String;
begin
  inherited StartTransaction;
  clbAllowedPhoneLines.Clear;
  for i := 0 to uPhones.Phones.Count-1 do
    clbAllowedPhoneLines.Checked[clbAllowedPhoneLines.Items.Add(uPhones.Phones.Phones[i].Name)] := True;
  with Application as IBaseConfig do
    tmp := Config.ReadString('PHONELINES','');
  while pos(';',tmp) > 0 do
    begin
      if clbAllowedPhoneLines.Items.IndexOf(copy(tmp,0,pos(';',tmp)-1)) > -1 then
        clbAllowedPhoneLines.Checked[clbAllowedPhoneLines.Items.IndexOf(copy(tmp,0,pos(';',tmp)-1))] := false;
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    end;
end;

procedure TfPhoneOptions.CommitTransaction;
var
  i: Integer;
  tmp: String;
begin
  tmp := '';
  for i := 0 to clbAllowedPhoneLines.Count-1 do
    if not clbAllowedPhoneLines.Checked[i] then
      tmp := tmp+clbAllowedPhoneLines.Items[i]+';';
  with Application as IBaseConfig do
    Config.WriteString('PHONELINES',tmp);
  inherited CommitTransaction;
end;

procedure TfPhoneOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
end;

initialization
  {$I uphoneoptions.lrs}

end.

