unit tCreateDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, FileUtil, uTestGlobals,
  uData, uBaseDataModule, Forms;

type

  { CreateDatabase }

  CreateDatabase= class(TTestCase)
  published
    procedure DeleteData;
    procedure CreateData;
    procedure AppendMandant;
    procedure AddMandant;
    procedure CreateDataModule;
    procedure SetDBProperties;
    procedure CreateDB;
    procedure CheckDB;
    procedure AlterDB;
    procedure AssignDataModule;
  end;

implementation

procedure CreateDatabase.DeleteData;
begin
  if DirectoryExists('testconfig') then
    DeleteDirectory('testconfig',False);
end;

procedure CreateDatabase.CreateData;
begin
  BaseApplication.CreateForm(TData,Data);
end;

procedure CreateDatabase.AppendMandant;
begin
  if not Data.AppendMandant('testconfig') then
    Fail('could not append Mandant');
end;

procedure CreateDatabase.AddMandant;
begin
  with Data.Mandants.DataSet do
    begin
      Insert;
      FieldByName('NAME').AsString:='Testmandant';
      FieldByName('DBTYP').AsString:='SQL';
      FieldByName('DBPROP').AsString:='sqlite-3;;'+AppendPathDelim(Application.Location)+'testconfig'+DirectorySeparator+'test.db;';
      Post;
    end;
end;

procedure CreateDatabase.CreateDataModule;
begin
  DataDB := uBaseDataModule.CreateDataModule(Data.Mandants.FieldByName('DBTYP').AsString);
  if not Assigned(DataDB) then
    Fail('Failed Creating Data Module');
end;

procedure CreateDatabase.AssignDataModule;
begin
  Data.DataModule := DataDB;
  DataDB.Mandants := Data.Mandants.DataSet;
end;

procedure CreateDatabase.SetDBProperties;
begin
  DataDB.SetProperties(Data.Mandants.FieldByName('DBPROP').AsString);
end;

procedure CreateDatabase.CreateDB;
begin
  if not DataDB.CreateDB then
    Fail('Failed Creating DB');
end;

procedure CreateDatabase.CheckDB;
begin
  if not DataDB.CheckDB then
    Fail('new created DB have to be checked ?');
end;

procedure CreateDatabase.AlterDB;
begin
  if not DataDB.AlterDB then
    Fail('failed Altering DB');
end;

initialization

  RegisterTest(CreateDatabase); 
end.
