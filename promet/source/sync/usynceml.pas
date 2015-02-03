unit usynceml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uBaseApplication, uBaseDbInterface, Mimemess;

type
  TfMailImport = class(TForm)
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fMailImport: TfMailImport;

implementation
uses uData,uMimeMessages;
{$R *.lfm}
procedure TfMailImport.FormCreate(Sender: TObject);
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('SyncMailEML');
      RestoreConfig;
      with Application as IBaseDbInterface do
        LoadMandants;
      if not Login then
        begin
          Application.Terminate;
          exit;
        end;
    end;
end;

procedure TfMailImport.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  sl: TStringList;
  aMsg: TMimeMess;
  i: Integer;
  aMessage: TMimeMessage;
begin
  Screen.Cursor := crHourglass;
  for i := 0 to length(FileNames)-1 do
    begin
      aMsg := TMimeMess.Create;
      aMsg.Lines.LoadFromFile(FileNames[i]);
      aMsg.DecodeMessage;
      aMessage := TMimeMessage.CreateEx(Self,Data);
      aMessage.Select(0);
      aMessage.Open;
      aMessage.DataSet.Insert;
      aMessage.FieldByName('USER').AsString := '*';
      aMessage.FieldByName('TYPE').AsString := 'EMAIL';
      aMessage.FieldByName('READ').AsString := 'N';
      aMessage.DecodeMessage(aMsg);
      aMsg.Free;
      aMessage.DataSet.Post;
    end;
  Screen.Cursor := crDefault;
end;

procedure TfMailImport.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  with Application as IBaseApplication do
    DoExit;
end;

end.

