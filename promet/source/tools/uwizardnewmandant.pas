{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
info@cu-tec.de
*******************************************************************************}
unit uWizardNewMandant;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, uIntfStrConsts, EditBtn,
  {VirtualStringTree, }FileUtil,db, ZConnection, DbCtrls, uData, uEncrypt,
  ClipBrd, Spin,ProcessUtils,UTF8Process,process,lclproc,uBaseDatasetInterfaces;
type
  TDirNode = record
    Name : string;
  end;

  { TfWizardNewMandant }

  TfWizardNewMandant = class(TForm)
    bAbort0: TButton;
    bAbort1: TButton;
    bAbort6: TButton;
    bAbort5: TButton;
    BitBtn1: TBitBtn;
    bNext0: TButton;
    bNext1: TButton;
    bNext4: TButton;
    bPrev0: TButton;
    bPrev1: TButton;
    bPrev5: TButton;
    Button1: TButton;
    bvImage: TPanel;
    bvleft: TBevel;
    bvleft1: TBevel;
    bvRight0: TBevel;
    bvRight1: TBevel;
    bvRight5: TBevel;
    cbExistingdatabase: TRadioButton;
    cbLanguage: TComboBox;
    cbNewdatabase: TRadioButton;
    cbSQLType: TComboBox;
    cbSyncHelp: TCheckBox;
    DirectoryEdit1: TFileNameEdit;
    eMandantname: TComboBox;
    eSQLdatabase1: TFileNameEdit;
    eDBServer: TEdit;
    eSQLPassword: TEdit;
    eSQLDatabase2: TEdit;
    eSQLPassword1: TEdit;
    eSQLUser: TEdit;
    eSQLServer: TEdit;
    eSQLdatabase: TFileNameEdit;
    eSQLUser1: TEdit;
    iDatabaseCreated: TImage;
    iDatabaseFilled: TImage;
    iDatabaseUpdated: TImage;
    imDialog: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lDefaultValue: TLabel;
    lDescription0: TLabel;
    lDescription1: TLabel;
    lDescription10: TLabel;
    lDescription11: TLabel;
    lDescription12: TLabel;
    lDescription13: TLabel;
    lDescription5: TLabel;
    lDescription6: TLabel;
    lDescription7: TLabel;
    lDescription8: TLabel;
    lDescription9: TLabel;
    lLanguage: TLabel;
    lMandantname: TLabel;
    lPassword1: TLabel;
    lProfile: TLabel;
    lUsername: TLabel;
    lPassword: TLabel;
    lDatabase: TLabel;
    lDBType: TLabel;
    lDBServer: TLabel;
    lUpdatingDatabase: TLabel;
    lFillingDefaultValues: TLabel;
    lCreatingDatabase: TLabel;
    lUsername1: TLabel;
    md: TDatasource;
    pButtons0: TPanel;
    pButtons5: TPanel;
    pCont0: TPanel;
    pCont4: TPanel;
    pDetails: TPanel;
    pButtons1: TPanel;
    pCont1: TPanel;
    pResult: TPanel;
    pLeft: TPanel;
    pCont2: TPanel;
    lDescription2: TLabel;
    pButtons2: TPanel;
    bvRight2: TBevel;
    bNext2: TButton;
    bPrev2: TButton;
    bAbort2: TButton;
    pCont3: TPanel;
    lDescription3: TLabel;
    pButtons3: TPanel;
    bvRight3: TBevel;
    bNext3: TButton;
    bPrev3: TButton;
    bAbort3: TButton;
    pCont5: TPanel;
    lDescription4: TLabel;
    lDatabaseDirectory1: TLabel;
    pButtons4: TPanel;
    bvRight4: TBevel;
    bNext5: TButton;
    bPrev4: TButton;
    bAbort4: TButton;
    DirectoryEdit2: TDirectoryEdit;
    bCopyConnectionString: TSpeedButton;
    rbFB: TRadioButton;
    rbPersonal: TRadioButton;
    RadioButton2: TRadioButton;
    rbSqlite: TRadioButton;
    rbFBE: TRadioButton;
    rbDelete: TRadioButton;
    rbFromFile: TRadioButton;
    rbPQ: TRadioButton;
    rbMS: TRadioButton;
    seSyncID: TSpinEdit;
    SilentTimer: TTimer;
    tvProfile: TTreeView;
    procedure bAbort0Click(Sender: TObject);
    procedure bCopyConnectionStringClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure bNext0Click(Sender: TObject);
    procedure bPrev0Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbLanguageSelect(Sender: TObject);
    procedure cbSQLTypeSelect(Sender: TObject);
    procedure DirectoryEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure eMandantnameSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbPQChange(Sender: TObject);
    procedure rbSqliteChange(Sender: TObject);
    procedure SilentTimerTimer(Sender: TObject);
  private
    { private declarations }
    Steps : array of Integer;
    procedure CollectProfiles(Node : TTreeNode);
  public
    { public declarations }
    procedure SetLanguage;
    function DoSave : Boolean;
    function  DoExecStep(Step : Integer) : Integer;
    procedure InitWizard;
  end;
var
  fWizardNewMandant: TfWizardNewMandant;

implementation
{$R *.lfm}
uses uError,uImpCSV, uBaseApplication, uBaseDbInterface, uBaseDbClasses,
  uBaseERPDBClasses, uOrder, uSync, uOptions, uMandantOptions, uuseroptions,
  uProcessOptions,uSyncOptions,uDocuments,uWiki,Utils,uProjects,uMasterdata,
  uPerson,utask,usimpleprocess;
resourcestring
  strdBase                      = 'DBase Datenbank';
  strSQLDatabase                = 'SQL basierte Datenbank';
  strSQLiteDatabase             = 'SQLite basierte Datenank';
  strfailedCreatingDatabase     = 'Das erstellen der Datenbank ist fehlgeschlagen';
  strCheckAndCorrectDbFailed    = 'Das prüfen der Datenbank für Mandant %s ist fehlgeschlagen';
  strErrorImportingCSVFile      = 'Vorgabedaten konnten nicht importiert werden';
  strEnterAnMandantName         = 'geben Sie einen Mandantennamen an';
  strSelectProfile              = 'Wählen Sie ein Profil';
  strSyncIDChanged              = 'SyncID wurde geändert!';
  strSuccess                    = 'erfolgreich durchgeführt!';
  strImporting                  = 'Importiere ';
{ TfWizardNewMandant }

procedure TfWizardNewMandant.bAbort0Click(Sender: TObject);
var
  aProcess: TProcessUTF8;
begin
  if pDetails.Visible then
    begin
      if Application.HasOption('execute') then
      if FileExists(UniToSys(Application.GetOptionValue('execute'))) then
        begin
          aProcess := TProcessUTF8.Create(Self);
          aProcess.CommandLine:=Application.GetOptionValue('execute');
          if Application.HasOption('c','config-path') then
            aProcess.CommandLine:=aProcess.CommandLine+' "--config-path='+Application.GetOptionValue('c','config-path')+'"';
          aProcess.Options := [poNoConsole];
          try
            aProcess.Execute;
            aProcess.Free;
          except
          end;
        end
    end;
  Close;
end;

procedure TfWizardNewMandant.bCopyConnectionStringClick(Sender: TObject);
begin
  Clipboard.AsText :='SQL:'+cbSQLType.text+';'+eSQLServer.text+';'+UniToSys(eSQLDatabase.text)+';'+eSQLUser.text+';x'+Encrypt(eSQLPassword.Text,99998);
end;
procedure TfWizardNewMandant.BitBtn1Click(Sender: TObject);
begin
  ExecuteProcess(AppendPathDelim(Application.Location)+'helpviewer'+ExtractFileExt(Application.ExeName),'');
end;
procedure TfWizardNewMandant.bNext0Click(Sender: TObject);
var
  i: LongInt;
begin
  i := Steps[length(Steps)-1];
  i := DoExecStep(i);
  Setlength(Steps,length(Steps)+1);
  Steps[length(Steps)-1] := i;
  TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-2]))).Visible := false;
  if (FindComponent('pCont'+IntToStr(i)) <> nil) and (TButton(Sender).Caption <> strFinish) then
    begin
      TPanel(FindComponent('pCont'+IntToStr(i))).Visible := True
    end
  else
    begin
      if DoSave then
        pDetails.Visible:=True;
    end;
end;
procedure TfWizardNewMandant.bPrev0Click(Sender: TObject);
var
  tmp: String;
begin
  if length(Steps)<2 then exit;
  tmp := 'pCont'+IntToStr(Steps[length(Steps)-1]);
  if FindComponent(tmp)<>nil then
    TPanel(FindComponent(tmp)).Visible := false;
  if FindComponent('pCont'+IntToStr(Steps[length(Steps)-2])) <> nil then
    TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-2]))).Visible := True;
  Setlength(Steps,length(Steps)-1);
end;
procedure TfWizardNewMandant.Button1Click(Sender: TObject);
begin
  fOptions.tvMain.Selected := fOptions.tvMain.Items.FindNodeWithText(strMandant);
  fOptions.Showmodal;
end;
procedure TfWizardNewMandant.cbLanguageSelect(Sender: TObject);
begin
  with Application as IBaseApplication do
    Language := cbLanguage.Text;
end;

procedure TfWizardNewMandant.cbSQLTypeSelect(Sender: TObject);
begin
  if (eSQLServer.Text = '') or (eSQLServer.Text = 'localhost') then
    eSQLServer.Text := 'localhost';
  if pos('promet-erp',eSQLdatabase.Text)=0 then exit;
  if Application.HasOption('database') then
    eSQLDatabase.Text:=Application.GetOptionValue('database')
  else
    eSQLDatabase.Text:=GetUserDir+'promet-erp.db';
  if cbSQLType.Text <> 'sqlite-3' then
    begin
      eSQLDatabase.Text:='promet-erp';
    end;
  if pos('firebird',cbSQLType.Text)>0 then
    begin
      eSQLDatabase.Text:='promet-erp.fdb';
    end;
  if pos('firebirdd',cbSQLType.Text)>0 then
    begin
      eSQLServer.Text:='';
    end;
end;

procedure TfWizardNewMandant.DirectoryEdit1AcceptFileName(Sender: TObject;
  var Value: String);
begin
  if eMandantname.Text='' then
    begin
      eMandantname.Text:=copy(ExtractFileName(Value),0,rpos('.',ExtractFileName(Value))-1);
    end;
  rbFromFile.Checked:=True;
end;

procedure TfWizardNewMandant.eMandantnameSelect(Sender: TObject);
var
  tmp: String;
  mSettings: TStringList;
begin
  rbDelete.Enabled := True;
  cbExistingDatabase.Checked:=True;
  with Application as IBaseDBInterface do
    begin
      if FileExists(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension) then
        begin
          mSettings := TStringList.Create;
          mSettings.LoadFromFile(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension);
          if mSettings[0] = 'SQL' then
            begin
              tmp := mSettings[1];
              cbSQLType.Text := copy(tmp,0,pos(';',tmp)-1);
              tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
              eSQLServer.Text := copy(tmp,0,pos(';',tmp)-1);
              tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
              eSQLDatabase.Text := copy(tmp,0,pos(';',tmp)-1);
              tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
              eSQLUser.Text := copy(tmp,0,pos(';',tmp)-1);
              tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
              eSQLPassword.Text := Decrypt(copy(tmp,2,length(tmp)),99998);
            end;
          mSettings.Free;
        end;
    end;
end;

procedure TfWizardNewMandant.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  fOptions.Free;
  with Application as IBaseDBInterface do
    DBLogout;
  CanClose:=True;
end;

procedure TfWizardNewMandant.FormCreate(Sender: TObject);
begin
  SetLanguage;
  InitWizard;
end;

procedure TfWizardNewMandant.FormDestroy(Sender: TObject);
begin
end;

procedure TfWizardNewMandant.FormShow(Sender: TObject);
begin
  if Application.HasOption('firebird') then
    begin
      cbSQLType.Text:='firebirdd-2.1';
      cbSQLTypeSelect(nil);
    end;
  if Application.HasOption('silent') then
    begin
      SilentTimer.Enabled := True;
    end;
end;

{
sqlite-3
postgresql-8
postgresql-7
mssql
mysql
mysql-4.1
mysql-5
firebird-2.5
firebird-2.1
firebird-2.0
firebird-1.5
firebird-1.0
firebirdd-2.5
firebirdd-2.1
firebirdd-2.0
firebirdd-1.5
interbase-6
}
procedure TfWizardNewMandant.rbPQChange(Sender: TObject);
begin
  if rbPQ.Checked then
    cbSQLType.Text:='postgresql-8'
  else if rbMS.Checked then
    cbSQLType.Text:='mssql'
  else if rbFB.Checked then
    cbSQLType.Text:='firebird-2.1'
  ;
  cbSQLTypeSelect(cbSQLType);
end;
procedure TfWizardNewMandant.rbSqliteChange(Sender: TObject);
begin
  if rbFBE.Checked then
    begin
      if cbSQLType.Text='firebirdd-2.1' then exit;
      cbSQLType.Text:='firebirdd-2.1'
    end
  else if rbSqlite.Checked then
    begin
      if cbSQLType.Text='sqlite-3' then exit;
      cbSQLType.Text:='sqlite-3';
    end;
  cbSQLTypeSelect(cbSQLType);
  eSQLdatabase1.Text:=eSQLdatabase.Text;
end;

procedure TfWizardNewMandant.SilentTimerTimer(Sender: TObject);
begin
  SilentTimer.Enabled:=False;
  Application.ProcessMessages;
  eMandantname.Text:='Standard';
  bNext0Click(bNext0);
end;

procedure TfWizardNewMandant.CollectProfiles(Node: TTreeNode);
var
  FindRec: TSearchRec;
  NewNode: TTreeNode;
begin
  IF FindFirst(UniToSys(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'importdata'+DirectorySeparator+Node.GetTextPath)+'*'), faDirectory, FindRec) = 0 THEN
    REPEAT
      IF ((FindRec.Name <> 'standard') AND (FindRec.Name <> '.') AND (FindRec.Name <> '..')) AND (FindRec.Attr and faDirectory = faDirectory) THEN
        begin
          NewNode := tvProfile.Items.AddChild(Node,FindRec.Name);
          try
            CollectProfiles(NewNode);
            NewNode.Expand(False);
          except
            tvProfile.Items.Delete(NewNode);
          end;
        end;
    UNTIL FindNext(FindRec) <> 0;
  FindClose(FindRec);
end;
procedure TfWizardNewMandant.SetLanguage;
var
  i: Integer;
  NextButton: TButton;
begin
  i := 0;
  while FindComponent('bPrev'+IntToStr(i)) <> nil do
    begin
      TButton(FindComponent('bPrev'+IntToStr(i))).Caption := strPrev;
      if i = 0 then
        TButton(FindComponent('bPrev'+IntToStr(i))).Enabled := False;
      TButton(FindComponent('bNext'+IntToStr(i))).Caption := strNext;
      NextButton := TButton(FindComponent('bNext'+IntToStr(i)));
      TButton(FindComponent('bAbort'+IntToStr(i))).Caption := strAbort;
      inc(i);
    end;
  NextButton.Caption := strFinish;
  bAbort5.Caption:=strAbort;
  //Application depend Language strings
end;
function TfWizardNewMandant.DoSave : Boolean;
var
  AlreadyExists: Boolean;
  Res: Boolean;
  aOrder: TOrder;
  aSyncDB: TSyncDB;
  mSettings: TStringList;
  aSettings: TCaption;
  aType: String;
  aProcess: TProcessUTF8;
  sres: String;
  aTexttyp: TTextTypes;
  tmp: String;
  procedure DoCreateTable(aTableC : TClass);
  var
    aTableName: string;
    aTable : TBaseDBDataset;
  begin
    try
      aTable := TBaseDbDataSetClass(aTableC).Create(nil);
      with aTable.DataSet as IBaseManageDB do
        aTableName := TableName;
      aTable.CreateTable;
      aTable.Open;
//      aTable.Free;
    except
    end;
  end;
  procedure DoImportTable(aTable : TBaseDbDataSet);
  var
    aTableName: string;
  begin
    lDefaultValue.Caption:=strImporting+' '+(aTable.DataSet as IBaseManageDB).TableName;
    Application.ProcessMessages;
    try
      with aTable.DataSet as IBaseManageDB do
        aTableName := TableName;
      if FileExists(UniToSys(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'importdata'+DirectorySeparator+tvProfile.Selected.GetTextPath)+lowercase(aTableName)+'.csv')) then
        begin
          aTable.CreateTable;
          aTable.Open;
          if aTable.DataSet.IsEmpty then
            CSVImport(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'importdata'+DirectorySeparator+tvProfile.Selected.GetTextPath)+lowercase(aTableName)+'.csv',';',aTable.DataSet);
        end
      else if FileExists(UniToSys(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'importdata'+DirectorySeparator+'standard')+lowercase(aTableName)+'.csv')) then
        begin
          aTable.CreateTable;
          aTable.Open;
          if aTable.DataSet.IsEmpty then
            CSVImport(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'importdata'+DirectorySeparator+'standard')+lowercase(aTableName)+'.csv',';',aTable.DataSet);
        end;
    except
    end;
  end;
  procedure DoImport(aTable : TClass);
  var
    aImportTable: TBaseDBDataset;
  begin
    aImportTable := TBaseDbDataSetClass(aTable).Create(nil);
    DoImportTable(aImportTable);
    aImportTable.Destroy;
  end;
begin
  Application.ProcessMessages;
  Screen.Cursor := crHourglass;
  try
  //Wizard finished, use the made settings
  with Application as IBaseDBInterface do
    begin
      mSettings := TStringList.Create;
      aType := 'SQL';
      mSettings.Add('SQL');
      aSettings := cbSQLType.text+';'+eSQLServer.text+';'+UniToSys(eSQLDatabase.text)+';'+eSQLUser.text+';x'+Encrypt(eSQLPassword.Text,99998);
      mSettings.Add(aSettings);
      mSettings.SaveToFile(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension);
      mSettings.Free;
    end;
  pCont0.Visible := False;
  pCont5.Visible := False;
  pResult.Visible := True;
  Application.ProcessMessages;
  with Application as IBaseDBInterface do
    begin
      Res := DBLogin(eMandantname.Text,'');
      if not Res then
        begin
          DBTyp:=aType;
          try
            Data.CreateDBFromProperties(aSettings);
            Res := DBLogin(eMandantname.Text,'');
          except
            on e : exception do
              begin
                LastError := e.Message;
                Res := False;
              end;
          end;
        end
      else if Application.HasOption('silent') then //Db existiert schon und silent installation
        begin
          Application.Terminate;
          exit;
        end;
      if Res then
        uData.Data := Data;
    end;
  iDatabaseCreated.Visible := True;
  Application.ProcessMessages;
  if Res then
    begin
      Button1.Enabled:=cbNewDatabase.Checked;
      if cbNewDatabase.Checked then
        begin
          if seSyncID.Value > 0 then
            begin
              Data.SyncOffset:=seSyncID.Value;
            end;
          Data.Users.Open;
          while not Data.Users.DataSet.EOF do
            Data.Users.DataSet.Delete;

          if cbSyncHelp.Checked then
            begin
              aSyncDB := TSyncDB.CreateEx(Self,Data);
              aSyncDB.CreateTable;
              aSyncDB.Insert;
              aSyncDB.FieldByName('NAME').AsString:='Help';
              aSyncDB.FieldByName('PROPERTIES').AsString:='SQL:sqlite-3;localhost;help.db;;x';
              aSyncDB.FieldByName('ACTIVE').AsString:='Y';
              aSyncDB.DataSet.Post;
              aSyncDB.Tables.Insert;
              aSyncDB.Tables.FieldByName('NAME').AsString:='WIKI';
              aSyncDB.Tables.FieldByName('ACTIVE').AsString:='Y';
              aSyncDB.Tables.DataSet.Post;
              aSyncDB.Tables.Insert;
              aSyncDB.Tables.FieldByName('NAME').AsString:='DOCUMENTS';
              aSyncDB.Tables.FieldByName('ACTIVE').AsString:='Y';
              aSyncDB.Tables.FieldByName('FILTERIN').AsString:=Data.QuoteField('TYPE')+'='+Data.QuoteValue('W');
              aSyncDB.Tables.DataSet.Post;
              aSyncDB.Tables.Insert;
              aSyncDB.Tables.FieldByName('NAME').AsString:='TREE';
              aSyncDB.Tables.FieldByName('ACTIVE').AsString:='Y';
              aSyncDB.Tables.FieldByName('FILTERIN').AsString:=Data.QuoteField('TYPE')+'='+Data.QuoteValue('W');
              aSyncDB.Tables.DataSet.Post;
              aSyncDB.Tables.Insert;
              aSyncDB.Tables.FieldByName('NAME').AsString:='SCRIPTS';
              aSyncDB.Tables.FieldByName('ACTIVE').AsString:='Y';
              aSyncDB.Tables.FieldByName('FILTERIN').AsString:=Data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue('CmdLn.*'))+' OR '+Data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue('Import*'))+' OR '+Data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue('Export*'));
              aSyncDB.Tables.DataSet.Post;
              aSyncDB.Free;
            end;
          DoImportTable(Data.Numbers);
          DoImportTable(Data.Users);
          DoImportTable(Data.Users.Rights);
          DoImportTable(Data.Tree);
          DoImportTable(Data.Reports);
          DoImportTable(Data.Filters);
          aTexttyp := TTextTypes.Create(nil);
          DoImportTable(aTextTyp);
          aTexttyp.Free;
          DoImportTable(Data.MandantDetails);
          Data.ProcessClient.CreateTable;
          Data.ProcessClient.Open;
          with Data.ProcessClient.DataSet do
            begin
              Insert;
              FieldByName('NAME').AsString:=GetSystemName;
              FieldByName('STATUS').AsString:='N';
              Post;
            end;
          Data.ProcessClient.Processes.Open;
          with Data.ProcessClient.Processes.DataSet do
            begin
              Insert;
              FieldByName('NAME').AsString:='sync_db';
              FieldByName('INTERVAL').AsInteger:=1000;
              Post;
            end;
          with Data.ProcessClient.DataSet do
            begin
              Insert;
              FieldByName('NAME').AsString:='*';
              FieldByName('STATUS').AsString:='N';
              Post;
            end;
          Data.ProcessClient.Processes.Open;
          with Data.ProcessClient.Processes.DataSet do
            begin
              Insert;
              FieldByName('NAME').AsString:='pop3receiver';
              FieldByName('INTERVAL').AsInteger:=10;
              Insert;
              FieldByName('NAME').AsString:='smtpsender';
              FieldByName('INTERVAL').AsInteger:=3;
              Insert;
              FieldByName('NAME').AsString:='feedreceiver';
              FieldByName('INTERVAL').AsInteger:=20;
              Post;
              Insert;
              FieldByName('NAME').AsString:='twitterreceiver';
              FieldByName('INTERVAL').AsInteger:=10;
              Post;
            end;

          DoImport(TSyncDB);
          DoImport(TSyncTable);
          DoImportTable(Data.Forms);
          DoImportTable(Data.Userfielddefs);
          DoImportTable(Data.StorageTypes);
          DoImportTable(Data.Currency);
          DoImportTable(Data.Countries);
          DoImportTable(Data.PaymentTargets);
          DoImport(TOrderTyp);
          DoImport(TPositionTyp);
          DoImport(TPriceTypes);
          DoImport(TStorageTyp);
          DoImport(TCountries);
          DoImport(TLanguages);
          DoImport(TVat);
          DoImport(TStates);
          DoImport(TUnits);
          DoImport(TDispatchTypes);
          DoImport(TCategory);
          DoImport(TFinancialAccounts);

          DoImport(TProject);
          DoImport(TTaskList);
          DoImport(TDependencies);
          DoImport(TPerson);
          DoImport(TMasterdata);
          DoImport(TOrder);
          try
            Data.Tree.CreateTable;
            Data.SetFilter(Data.Tree,'');
            Data.Tree.ImportStandartEntrys;
          except
          end;

          iDatabaseFilled.Visible := True;
          fOptions.RegisterOptionsFrame(TfMandantOptions.Create(fOptions),strMandant,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfProcessOptions.Create(fOptions),strProcesses,strPersonalOptions);
        //  fOptions.RegisterOptionsFrame(TfSyncOptions.Create(fOptions),strSync,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfUserOptions.Create(fOptions),strUsers,strGeneralOptions);
          Application.Processmessages;
          if cbSyncHelp.Checked then
            begin
              DoCreateTable(TDocument);
              DoCreateTable(TWikiList);
              ChDir(AppendPathDelim(Application.Location));
              if Application.HasOption('c','config-path') then
                begin
                  if ExecProcess('sync_db'+ExtractFileExt(Application.ExeName)+' "--config-path='+Application.GetOptionValue('c','config-path')+'" "--mandant='+eMandantname.Text+'"',AppendPathDelim(Application.Location)) then
                    iDatabaseUpdated.Visible:=True;
                  Clipboard.AsText:='sync_db'+ExtractFileExt(Application.ExeName)+' "--config-path='+Application.GetOptionValue('c','config-path')+'" "--mandant='+eMandantname.Text+'"';
                end
              else
                begin
                  if ExecProcess('sync_db'+ExtractFileExt(Application.ExeName)+' "--mandant='+eMandantname.Text+'"',AppendPathDelim(Application.Location)) then
                    iDatabaseUpdated.Visible:=True;
                end;
            end;
        end
      else if cbExistingDatabase.Checked then
        begin
          if seSyncID.Value > 0 then
            begin
              Data.SyncOffset:=seSyncID.Value;
              Showmessage(strSyncIDChanged);
            end;
        end;
    end;
  except
    on e : Exception do
      begin
        Res := False;
        with BaseApplication as IBaseDbInterface do
          LastError := e.Message;
      end;
  end;
  Screen.Cursor := crDefault;
  if not Res then
    begin
      with BaseApplication as IBaseDbInterface do
        Showmessage(LastError);
      Application.Processmessages;
      bPrev0Click(nil);
    end;
  Result := Res;
end;
function TfWizardNewMandant.DoExecStep(Step: Integer): Integer;
begin
  //Do whatever to do in these Step and return the next step
  Result := 0;
  case Step of
  0:
    begin
      if (eMandantName.Text = '') then
        begin
          ShowMessage(strEnterAnMandantName);
          result := 0;
          exit;
        end;
      if cbNewDatabase.Checked then
        Result := 1
      else
        begin
          Result := 2;
          with Application as IBaseDBInterface do
            if FileExists(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension) then
              Result := 5;
        end;
      if rbDelete.Checked then
        begin
          with Application as IBaseDBInterface do
            begin
              if FileExists(UniToSys(AppendPathDelim(MandantPath)+'mandant.dbf')) then
                DeleteFile(UniToSys(AppendPathDelim(MandantPath)+'mandant.dbf'));
              if FileExists(UniToSys(AppendPathDelim(MandantPath)+'mandant.dbt')) then
                DeleteFile(UniToSys(AppendPathDelim(MandantPath)+'mandant.dbt'));
              if FileExists(UniToSys(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension)) then
                if DeleteFile(UniToSys(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension)) then
                  ShowMessage(strSuccess);
            end;
          Application.Terminate;
          exit;
        end;
      if rbFromFile.Checked then
        begin
          with Application as IBaseDBInterface do
            begin
              if not FileExists(UniToSys(DirectoryEdit1.FileName)) then
                begin
                  result := 0;
                  exit;
                end;
              if CopyFile(UniToSys(DirectoryEdit1.FileName),UniToSys(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension)) then
                ShowMessage(strSuccess)
              else
                begin
                  result := 0;
                  exit;
                end;
            end;
          Application.Terminate;
          exit;
        end;
    end;
  1:
    begin
      if (tvProfile.Selected = nil) then
        begin
          ShowMessage(strSelectProfile);
          result := 1;
          exit;
        end;
      Result := 2;
      with Application as IBaseDBInterface do
        if FileExists(AppendPathDelim(MandantPath)+eMandantname.Text+MandantExtension) then
          Result := 5;
      if Application.HasOption('silent') then
        Result := 6;
    end;
  2:
    begin
      if rbPersonal.Checked then
        Result := 3
      else
        Result := 4;
      eSQLdatabase1.Text:=eSQLdatabase.Text;
    end;
  3:
    begin
      rbSqliteChange(rbSqlite);
      eSQLdatabase.Text:=eSQLdatabase1.Text;
      Result := 5;
    end;
  4:
    begin
      eSQLdatabase.Text:=eSQLdatabase2.Text;
      eSQLServer.Text:=eDBServer.Text;
      eSQLUser.Text:=eSQLUser1.Text;
      eSQLPassword.Text:=eSQLPassword1.Text;
      rbPQChange(rbPQ);
      Result := 5;
    end;
  end;
end;
procedure TfWizardNewMandant.InitWizard;
var
  i : Integer;
  AInfo: TSearchRec;
  ade: ComCtrls.TTreeNode;
begin
  Setlength(Steps,1);
  Steps[0] := 0;
  i := 0;
  while FindComponent('pCont'+IntToStr(i)) <> nil do
    begin
      TPanel(FindComponent('pCont'+IntToStr(i))).Visible := False;
      inc(i);
    end;
  pCont0.Visible := True;
  pResult.Visible := false;
  tvProfile.Items.Clear;
  CollectProfiles(nil);
  ade := tvProfile.Items.FindNodeWithText('Warenwirtschaft (alles)');
  if Assigned(ade) and Assigned(aDe.Items[0]) then
    tvProfile.Selected := aDe.FindNode('Deutschland');
  eSQLDatabase.Text:=GetUserDir+'promet-erp.db';
  cbSQLTypeSelect(nil);
  with Application as IBaseDBInterface do
    LoadMandants;
  //Wizard finished, use the made settings
  with Application as IBaseDBInterface do
    begin
      If FindFirst (AppendPathDelim(MandantPath)+'*'+MandantExtension,faAnyFile,AInfo)=0 then
        Repeat
          With aInfo do
            begin
              If (Attr and faDirectory) <> faDirectory then
                eMandantname.Items.Add(copy(Name,0,length(Name)-length(MandantExtension)));
            end;
        Until FindNext(ainfo)<>0;
      FindClose(aInfo);
    end;
end;
initialization
end.

