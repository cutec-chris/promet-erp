unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids,uSessionDBClasses;

type
  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    ActionList1: TActionList;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Variables: TDatasource;
    History: TDatasource;
    Session: TDatasource;
    gUsers: TDBGrid;
    IdleTimer1: TIdleTimer;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miNewMandant: TMenuItem;
    miOptions: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    Properties: TXMLPropStorage;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Sessions: TSessions;
    procedure DoCreate;
  end;

var
  fMain: TfMain;

implementation
{$R *.lfm}
uses uBaseApplication, uData, uBaseDbInterface;

procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('WebStat');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;

procedure TfMain.acLoginExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  IdleTimer1.Enabled:=True;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  Sessions := TSessions.Create(Self,Data);
  Sessions.Open;
  Sessions.History.Open;
  Sessions.Variables.Open;
  Session.DataSet := Sessions.DataSet;
  Variables.DataSet := Sessions.Variables.DataSet;
  History.DataSet := Sessions.History.DataSet;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
const
  memowidth = 200; // 10 Zeichen
var
  s: string;
begin
  with (Sender as TDBGrid) do begin
    if Column.Field.IsBlob then begin
      Canvas.FillRect(Rect);
      s := copy(Column.Field.AsString, 1, memowidth);
      Canvas.TextOut(Rect.Left+2, Rect.Top+2, s);
    end else begin
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  end;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Sessions.Free;
  with Application as IBaseApplication do
    begin
      SaveConfig;
      DoExit;
    end;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
end;

initialization

end.