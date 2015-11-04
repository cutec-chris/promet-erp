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
    acSearchOrder: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TSpeedButton;
    BitBtn4: TSpeedButton;
    Button1: TButton;
    cbVersion: TComboBox;
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
    tvStep: TTreeView;
    procedure acLoadOrderExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acSearchOrderExecute(Sender: TObject);
    procedure eOrderKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function SetOrderfromSearch(aLink: string): Boolean;
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
uses uBaseApplication, uData, uBaseDbInterface,uMasterdata,uSearch,variants;
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
  if (FOrder.Count=0) and (IsNumeric(eOrder.Text)) then
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
      if cbVersion.Enabled and (cbVersion.Text<>'') then
        aMasterdata.Locate('VERSION',cbVersion.Text,[]);
      if aMasterdata.Count>0 then
        begin
          FOrder.OrderType.Open;
          if FOrder.OrderType.Locate('SI_PROD;TYPE',VarArrayOf(['Y',7]),[]) then
            begin
              FOrder.Insert;
              FOrder.Positions.Insert;
              //FOrder.Status.AsString:=FOrder.OrderType.FieldByName('STATUS').AsString;
              FOrder.Positions.Assign(aMasterdata);
              FOrder.Positions.Post;
              FOrder.Post;
            end;
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

procedure TfMain.acSearchOrderExecute(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@SetOrderfromSearch;
  fSearch.AllowSearchTypes(strOrders+','+strMasterdata);
  fSearch.Execute(True,'PRODSE','');
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

function TfMain.SetOrderfromSearch(aLink: string): Boolean;
var
  aMasterdata: TMasterdata;
begin
  aMasterdata := TMasterdata.Create(nil);
  aMasterdata.SelectFromLink(aLink);
  aMasterdata.Open;
  eOrder.Text:=aMasterdata.Number.AsString;
  cbVersion.Text:=aMasterdata.Version.AsString;
  aMasterdata.Select(aMasterdata.Number.AsString);
  aMasterdata.Open;
  cbVersion.Enabled:=aMasterdata.Count>1;
  cbVersion.Items.Clear;
  aMasterdata.First;
  while not aMasterdata.EOF do
    begin
      cbVersion.Items.Add(aMasterdata.Version.AsString);
      aMasterdata.Next;
    end;
  aMasterdata.Free;
end;

procedure TfMain.DoOpen;
begin
  eOrder.Enabled:=FOrder.Count>0;
  cbVersion.Enabled:=cbVersion.Enabled and (FOrder.Count>0);
  tvStep.Enabled:=FOrder.Count>0;
  tvStep.Items.Clear;
  if FOrder.Count>0 then
    begin
      FOrder.Positions.First;
      while not FOrder.Positions.EOF do
        begin

          FOrder.Positions.Next;
        end;
    end;
end;

initialization
end.
