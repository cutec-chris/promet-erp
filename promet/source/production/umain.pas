unit umain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils,
  uIntfStrConsts, db, memds, FileUtil, IpHtml, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage, uOrder;
type
  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acNextStep: TAction;
    acPriorStep: TAction;
    acExecuteStep: TAction;
    acPrepare: TAction;
    acLoadOrder: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TSpeedButton;
    BitBtn4: TSpeedButton;
    eOrder: TEdit;
    IpHtmlPanel1: TIpHtmlPanel;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    procedure acLoadOrderExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure eOrderKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FOrder : TOrder;
    procedure DoOpen;
  public
    { public declarations }
    procedure DoCreate;
  end;
var
  fMain: TfMain;
resourcestring
  strNoOrderFound                       = 'Es wurde kein Auftrag oder Artikel gefunden der zum Suchkriterium passt !';
implementation
{$R *.lfm}
uses uBaseApplication, uData, uBaseDbInterface,uMasterdata;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Production');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
var
  a: TOrder;
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  FOrder := TOrder.Create(nil);
end;

procedure TfMain.acLoadOrderExecute(Sender: TObject);
var
  aMasterdata: TMasterdata;
begin
  //Try to select by Orderno
  FOrder.SelectFromNumber('');
  if IsNumeric(eOrder.Text) then
    FOrder.SelectFromNumber(eOrder.Text);
  FOrder.Open;
  if FOrder.Count=0 then
    begin
      //Try to select by Commission
      FOrder.SelectFromNumber(eOrder.Text);
    end;
  FOrder.Open;
  if FOrder.Count=0 then
    begin
      //Find Article and Create Order
      aMasterdata := TMasterdata.Create(nil);
      aMasterdata.SelectFromNumber(eOrder.Text);
      aMasterdata.Open;
      if aMasterdata.Count>0 then
        begin
          FOrder.Insert;
          FOrder.Positions.Insert;
          FOrder.Positions.Assign(aMasterdata);
          FOrder.Positions.Post;
          FOrder.Post;
        end;
      aMasterdata.Free;
    end;
  if FOrder.Count=0 then
    begin
      Showmessage(strNoOrderFound);
      eOrder.SelectAll;
      eOrder.SetFocus;
    end
  else DoOpen;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.eOrderKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    begin
      acLoadOrder.Execute;
    end;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FOrder.Free;
  with Application as IBaseApplication do
    begin
      Data.ActiveUsers.DataSet.AfterScroll:=nil;
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
    begin
    end;
end;

procedure TfMain.DoOpen;
begin

end;

initialization
end.
