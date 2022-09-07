unit uarchivestoremain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, ZVDateTimePicker,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, Interfaces, uSystemMessage;
type

  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    ActiveUsers: TDataSource;
    Commands: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
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
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ZVDateTimePicker1: TZVDateTimePicker;
    ZVDateTimePicker2: TZVDateTimePicker;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
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
{$R *.lfm}
uses uBaseApplication, uData, uBaseDbInterface, uOrder,uMessages;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Archivestore');
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
procedure TfMain.Button1Click(Sender: TObject);
var
  aStore: TArchivedMessage;
begin
  if SelectDirectoryDialog1.Execute then
    begin
      aStore := TArchivedMessage.CreateEx(Self,Data);
      Data.SetFilter(aStore,Data.QuoteField('TIMESTAMPD')+'>'+Data.DateTimeToFilter(ZVDateTimePicker1.Date)+' AND '+Data.QuoteField('TIMESTAMPD')+'<'+Data.DateTimeToFilter(ZVDateTimePicker2.Date));
      with aStore.DataSet do
        begin
          First;
          while not EOF do
            begin
              Data.BlobFieldToFile(aStore.DataSet,'DATA',AppendPathDelim(SelectDirectoryDialog1.FileName)+Utils.ValidateFileName(FieldByName('ID').asString)+'.eml');
              Next;
            end;
        end;
      aStore.Free;
    end;
end;
procedure TfMain.Button2Click(Sender: TObject);
begin
  Close;
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
  ZVDateTimePicker1.Date:=Now()-1;
  ZVDateTimePicker2.Date:=Now();
end;

initialization

end.
