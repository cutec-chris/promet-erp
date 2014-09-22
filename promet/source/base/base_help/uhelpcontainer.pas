unit uHelpContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  StdCtrls, uWikiFrame;

type

  { TfHelpContainer }

  TfHelpContainer = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    WikiFrame : TfWikiFrame;
    procedure SetLanguage;
  end; 

var
  fHelpContainer: TfHelpContainer;

implementation
{$R *.lfm}
uses uData,uBaseDBInterface;
procedure TfHelpContainer.FormCreate(Sender: TObject);
begin
  WikiFrame := TfWikiFrame.Create(Self);
  WikiFrame.Parent := Self;
  WikiFrame.Align:=alClient;
  WikiFrame.SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
end;

procedure TfHelpContainer.FormDestroy(Sender: TObject);
begin
  WikiFrame.Destroy;
end;

procedure TfHelpContainer.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfHelpContainer,fHelpContainer);
      Self := fHelpContainer;
    end;
end;

initialization

end.

