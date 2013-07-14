unit uPhoneOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, CheckLst,
  uOptionsFrame;

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
  with Application as IBaseApplication do
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
  with Application as IBaseApplication do
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

