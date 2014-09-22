unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, dbf, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, uIntfStrConsts,
  db, uBaseDatamodule,FileUtil, Translations,md5,
  ComCtrls, ExtCtrls;

type

  { TfMain }

  TfMain = class(TForm)
    acAddDirectory: TAction;
    acBack: TAction;
    acBugtracker: TAction;
    acDeleteDirectory: TAction;
    acDeleteMandant: TAction;
    acDeleteWholeMessageDir: TAction;
    acForward: TAction;
    acHelpIndex: TAction;
    acInfo: TAction;
    acPrint: TAction;
    acClear: TAction;
    acViewList: TAction;
    acLogin: TAction;
    acLogout: TAction;
    acNew: TAction;
    acNewCustomer: TAction;
    acNewMandant: TAction;
    acNewOrder: TAction;
    acNewTask: TAction;
    acNewTermin: TAction;
    acPersonalOptions: TAction;
    acProperties: TAction;
    acQuit: TAction;
    acRegister: TAction;
    acShowHide: TAction;
    acTimeRegistering: TAction;
    acViewDetails: TAction;
    ActionList: TActionList;
    bBonStorno: TBitBtn;
    bClear: TBitBtn;
    bNew: TBitBtn;
    bPosStorno: TBitBtn;
    bPrintBon: TBitBtn;
    bQuantity: TBitBtn;
    bViewList: TBitBtn;
    bViewDetails: TBitBtn;
    cbPaymentType: TComboBox;
    dbMandant: TDbf;
    dgBon: TDBGrid;
    gList: TDBGrid;
    lStatus: TLabel;
    lSum: TLabel;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miHelp: TMenuItem;
    miHelpindex: TMenuItem;
    miInfo: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miNewMandant: TMenuItem;
    miOptions: TMenuItem;
    miPersonalOptions: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    pcButtons: TPageControl;
    pList: TPanel;
    pDetails: TPanel;
    Properties: TXMLPropStorage;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acViewDetailsExecute(Sender: TObject);
    procedure acViewListExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pDetailsClick(Sender: TObject);
  private
    { private declarations }
    DataDB : TBaseDataModule;
    function DoLogin(Mandant, User: string; HideStatus: Boolean=False) : Boolean;
  public
    { public declarations }
    procedure DoCreate;
    procedure FillPasswordUsers;
    procedure SetLanguage;
    procedure SetRights;
    procedure SetupDB;
  end; 

var
  fMain: TfMain;

implementation
{$R *.lfm}
uses uAppConsts,uPassword,uData,uError,uMashineID;

resourcestring
  strLoggedIn                       = 'Angemeldet';
  strNoBonFormular                  = 'kein Bon Formular vorhanden !';
  strPOSStatusNotLoggedIn           = 'kein Kassierer angemeldet';

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
  if not DirectoryExistsUTF8(GetConfigDir(vAppname)) then
    ForceDirectoriesUTF8(GetConfigDir(vAppname));
  if not DirectoryExistsUTF8(GetGlobalConfigDir(vAppname)) then
    ForceDirectoriesUTF8(GetGlobalConfigDir(vAppname));
  Properties.FileName := GetConfigDir(vAppname)+'pos_config.xml';
  Properties.Restore;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  DataDB.CloseDB;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  if not Assigned(DataDB) then
    acLogin.Execute;
end;

procedure TfMain.pDetailsClick(Sender: TObject);
begin
end;

function TfMain.DoLogin(Mandant, User: string;HideStatus : Boolean = False) : Boolean;
begin
  Result := False;
  if not Assigned(DataDb) then
    begin
      if not dbMandant.Locate('NAME',Mandant,[]) then exit;
      DataDB := CreateDataModule(dbMandant.FieldbyName('DBTYP').AsString);
      if Assigned(DataDB) then
        begin
          Data.DataModule := DataDB;
          DataDB.Mandants := dbMandant;
          DataDB.SetProperties(dbMandant.FieldbyName('DBPROP').AsString);
          if not DataDB.CreateDB then
            begin
              Data.DataModule := nil;
              DataDB.Free;
              exit;
            end;
          Data.DataModule := nil;
        end;
    end;
  DataDb.DisableAllControls;
  DataDB.OpenDB;
  Data.DataModule := DataDB;
  if (not DataDB.CheckDB) or (not Data.Locate(Data.Users,'NAME',User,[])) then
    begin
      DataDB.CloseDB;
      Screen.Cursor := crDefault;
      Data.DataModule := nil;
      fError.ShowWarning(strErrorDatabaseStructurenotActual);
      Application.Terminate;
      exit;
    end;
  SetRights;
  Data.DataModule.OpenChilds(Data.Users.DataSet);
  SetupDB;
  DataDb.EnableAllControls;
  acLogout.Enabled:=True;
  acLogin.Enabled:=False;
  DataDB.AppendUserToActiveList;
  lStatus.Caption:=Format(strLoggedIn,[Data.Users.FieldByName('NAME').AsString]);
  Result := True;
end;

procedure TfMain.acLoginExecute(Sender: TObject);
begin
  fPassword.cbMandant.Text:='';
  fPassword.cbMandant.Enabled := True;
  fPassword.cbUser.Enabled := False;
  fPassword.ePasswort.Enabled := False;
  fPassword.ePasswort.Text := '';
  if fMain.Properties.StoredValue['AUTOMATICLOGIN']=IntToStr(CreateUserID) then
    begin
      DoLogin(Properties.StoredValue['LOGINMANDANT'],Properties.StoredValue['LOGINUSER'],True);
      exit;
    end;
  if fPassword.Execute then
    begin
      if not DataDB.Locate(DataDB.Users,'NAME',fPassword.cbUser.Text,[]) then exit;
      if DataDB.Users.FieldByName('PASSWORD').AsString = '' then
        begin
          DataDB.Users.Edit;
          DataDB.Users.FieldByName('PASSWORD').AsString := md5print(MD5String(fPassword.ePasswort.text));
          DataDB.Users.Post;
        end
      else
        if not (DataDB.Users.FieldByName('PASSWORD').AsString = md5print(MD5String(fPassword.ePasswort.text))) then
          begin
            ShowWarning(strWrongPassword);
            Application.Terminate;
            exit;
          end;
      fPassword.ePasswort.Text:='';
      DoLogin(fPassword.cbMandant.Text,fPassword.cbUser.Text);
    end
  else Application.Terminate;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  fMain.Properties.StoredValue['AUTOMATICLOGIN']:='NO';
  Close;
end;

procedure TfMain.acNewExecute(Sender: TObject);
begin
  if not Data.Locate(Data.OrderType,'TYPE','6 ',[loPartialKey]) then
    begin
      fError.ShowWarning(strNoBonFormular);
      exit;
    end;
  Data.Orders.DataSet.Insert;
  Data.DataModule.OpenChilds(Data.Orders.DataSet);
end;

procedure TfMain.acPrintExecute(Sender: TObject);
begin

  Data.DataModule.CloseChilds(Data.Orders.DataSet);
end;

procedure TfMain.acViewDetailsExecute(Sender: TObject);
begin
  pDetails.Visible := True;
  pList.Visible := false;
end;

procedure TfMain.acViewListExecute(Sender: TObject);
begin
  pDetails.Visible := false;
  pList.Visible := True;
end;

procedure TfMain.DoCreate;
begin
  with dbMandant,FieldDefs do
    begin
      Close;
      Clear;
      Add('MANDANT',ftAutoInc,0,True);
      Add('NAME',ftString,30,True);
      Add('DBTYP',ftString,3,True);
      Add('DBPROP',ftMemo,0,False);
      FilePath := GetConfigDir(vAppname);
      if not FileExistsUTF8(FilePath+DirectorySeparator+Tablename) then
        CreateTable;
      Open;
      First;
      while not EOF do  //List mandants and validate Tables
        begin
          fPassword.cbMandant.Items.Add(FieldbyName('NAME').AsString);
          Next;
        end;
    end;
  SetLanguage;
  SetRights;
end;

procedure TfMain.FillPasswordUsers;
begin
  try
  if Assigned(DataDB) then
    DataDB.Free;
  except
  end;
  DataDB := nil;
  fPassword.cbUser.Clear;
  if not dbMandant.Locate('NAME',fpassword.cbMandant.text,[]) then exit;
  DataDB := CreateDataModule(dbMandant.FieldbyName('DBTYP').AsString);
  if Assigned(DataDB) then
    begin
      Data.DataModule := DataDB;
      DataDB.Mandants := dbMandant;
      DataDB.SetProperties(dbMandant.FieldbyName('DBPROP').AsString);
      if not DataDB.CreateDB then
        begin
          Data.DataModule := nil;
          DataDB.Free;
          exit;
        end;
      if not DataDB.Users.Active then
        DataDB.Users.Open;
      while not DataDB.Users.EOF do
        begin
          if (DataDB.Users.FieldByName('TYPE').IsNull) and (DataDB.Users.FieldByName('LEAVED').IsNull or (not (DataDB.Users.FieldByName('LEAVED').AsDateTime < Now()))) then
            fPassword.cbUser.Items.Add(DataDB.Users.FieldByName('NAME').AsString);
          DataDb.Users.Next;
        end;
      fPassword.cbUser.Enabled:=True;
      if fPassword.cbUser.Items.Count=1 then
        begin
          fPassword.cbUser.ItemIndex:=0;
          fPassword.cbUserSelect(nil);
          if fPassword.Visible then
            fPassword.ePasswort.SetFocus;
        end;
      Data.DataModule := nil;
    end;
end;

procedure TfMain.SetLanguage;
begin
  if Properties.StoredValue['LANGUAGE'] = '' then
    Properties.StoredValue['LANGUAGE'] := 'english';
  LoadLanguage(Properties.StoredValue['LANGUAGE']);
  if FileExistsUTF8(ProgramDirectory+'languages'+Directoryseparator+'lazreport.'+copy(Properties.StoredValue['LANGUAGE'],0,2)+'.po') then
    TranslateUnitResourceStrings('lr_const',ProgramDirectory+'languages'+Directoryseparator+'lazreport.'+copy(Properties.StoredValue['LANGUAGE'],0,2)+'.po');
  lSum.Caption := '';
  lStatus.Caption := strPOSStatusNotLoggedIn;

  fError.SetLanguage;
end;

procedure TfMain.SetRights;
begin
  if Assigned(DataDB) and (DataDB.OrderPos.Active) then
    begin
      bPrintBon.Enabled := True;
      bQuantity.Enabled := True;
      bPosStorno.Enabled := True;
      bBonStorno.Enabled := True;
      bClear.Enabled := True;
      cbPaymentType.Enabled := True;
      dgBon.Enabled := True;
      acNew.Enabled := True;
    end
  else
    begin
      bPrintBon.Enabled := False;
      bQuantity.Enabled := False;
      bPosStorno.Enabled := False;
      bBonStorno.Enabled := False;
      bClear.Enabled := False;
      cbPaymentType.Enabled := False;
      dgBon.Enabled := False;
      acNew.Enabled := False;
    end;
end;

procedure TfMain.SetupDB;
begin
  dgBon.DataSource := Data.OrderPos;
end;

initialization

end.
