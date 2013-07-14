unit umain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage;
type
  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    ActionList1: TActionList;
    ActiveUsers: TDatasource;
    cbCommand: TComboBox;
    Commands: TDatasource;
    DBGrid1: TDBGrid;
    gUsers: TDBGrid;
    IdleTimer1: TIdleTimer;
    Label1: TLabel;
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
    Panel1: TPanel;
    Properties: TXMLPropStorage;
    Splitter1: TSplitter;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure cbCommandSelect(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure UserScroll(DataSet: TDataSet);
  private
    { private declarations }
    SysCommands : TSystemCommands;
  public
    { public declarations }
    procedure DoCreate;
  end;
var
  fMain: TfMain;
implementation
uses uBaseApplication, uData, uBaseDbInterface, uOrder;
procedure TfMain.IdleTimer1Timer(Sender: TObject);
begin
  with Data.ActiveUsers.DataSet do
    begin
      DisableControls;
      Refresh;
      EnableCOntrols;
    end;
end;
procedure TfMain.UserScroll(DataSet: TDataSet);
begin
  if Assigned(SysCommands) then
    Data.SetFilter(SysCommands,Data.QuoteField('PROCESS_ID')+'='+Data.QuoteValue(Data.ActiveUsers.Id.AsString),5);
end;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('ClientManagement');
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
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
  Data.SetFilter(Data.ActiveUsers,'',1000,'TIMESTAMPD','DESC');
  SysCommands := TSystemCommands.Create(Self,Data);
  Data.ActiveUsers.DataSet.AfterScroll:=@UserScroll;
  Commands.DataSet := SysCommands.DataSet;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  Data.ActiveUsers.DataSet.AfterScroll:=nil;
  FreeAndnil(SysCommands);
  with Application as IBaseApplication do
    Logout;
end;
procedure TfMain.cbCommandSelect(Sender: TObject);
begin
  if cbCommand.text = '' then exit;
  with SysCommands.DataSet do
    begin
      Append;
      FieldByName('PROCESS_ID').AsVariant := Data.ActiveUsers.Id.AsVariant;
      FieldByName('COMMAND').AsString := cbCommand.Text;
      cbCommand.Text := '';
      Post;
    end;
end;

procedure TfMain.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
const
  memowidth = 30; // 10 Zeichen
var
  s: string;
begin
  if not Assigned(Column) then exit;
  if not Assigned(Column.Field) then exit;
  with (Sender as TDBGrid) do
    begin
      Canvas.FillRect(Rect);
      s := copy(Column.Field.AsString, 1, memowidth);
      Canvas.TextOut(Rect.Left+2, Rect.Top+2, s);
    end;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with Application as IBaseApplication do
    begin
      Data.ActiveUsers.DataSet.AfterScroll:=nil;
      FreeAndnil(SysCommands);
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
  if Assigned(Data) then
    ActiveUsers.DataSet := Data.ActiveUsers.DataSet;
end;

initialization
  {$I umain.lrs}

end.