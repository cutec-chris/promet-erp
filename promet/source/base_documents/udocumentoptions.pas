unit uDocumentOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, EditBtn, StdCtrls,
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
uses uBaseApplication;
procedure TfDocumentOptions.StartTransaction;
begin
  inherited StartTransaction;
  with BaseApplication as IBaseApplication do
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
  with BaseApplication as IBaseApplication do
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
  {$I udocumentoptions.lrs}

end.

