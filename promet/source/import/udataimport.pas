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
Created 11.06.2012
*******************************************************************************}
unit uDataImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ExtCtrls, Buttons, db;

type
  TImporterCapability = (icImport,icExport);
  TImporterCapabilities = set of TImporterCapability;

  { TfDataImport }

  TfDataImport = class(TForm)
    bChangeFormat: TBitBtn;
    bSourceOptions: TBitBtn;
    bpButtons: TButtonPanel;
    cbFormat: TComboBox;
    lDesteny: TLabel;
    Label2: TLabel;
    lInfo: TLabel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure bChangeFormatClick(Sender: TObject);
    procedure bSourceOptionsClick(Sender: TObject);
    procedure cbDataSourceChange(Sender: TObject);
    procedure cbFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FAppend: Boolean;
    FConfigDir: string;
    FFilter: string;
    FTraget: TDataSource;
    FTyp : TImporterCapability;
    procedure SetAppend(const AValue: Boolean);
    procedure SetConfigDir(AValue: string);
    procedure SetFilter(const AValue: string);
    procedure SetTarget(const AValue: TDataSource);
    procedure CheckAll;
    { private declarations }
  public
    { public declarations }
    property Target : TDataSource read FTraget write SetTarget;
    property BaseDir : string read FFilter write SetFilter;
    property ConfigDir : string read FConfigDir write SetConfigDir;
    property AppendonExport : Boolean read FAppend write SetAppend;
    function Execute(Typ : TImporterCapability;DefaultFormat : string = '') : Boolean;
  end;

  { TImporter }

  TImporter = class(TObject)
  private
  protected
    FDataSet: TDataSet;
    function GetCapablities: TImporterCapabilities;virtual;abstract;
  public
    function Execute(Typ : TImporterCapability;ShowDialog : Boolean = False) : Boolean;virtual;abstract;
    property DataSource : TDataSet read FDataSet;
    function GetConfig : string;virtual;abstract;
    property Capabilities : TImporterCapabilities read GetCapablities;
    procedure SetConfig(Typ : TImporterCapability;aConfig : string);virtual;abstract;
  end;
  TImporterClass = class of TImporter;

  TImporterTyp = class
  public
    ImporterClass : TImporterClass;
    Name : string;
  end;

var
  fDataImport: TfDataImport;
  ImportSources : TList = nil;

procedure RegisterImportSource(aClass : TImporterClass;aName : string);

resourcestring
  strPleaseenteranFormatName            = 'Bitte geben Sie einen Namen für das Format an !';
  strImporterclassnotFound              = 'Datenquellenklasse wurde nicht gefunden !';
  strPleasSelectanImportfilebeforeEdit  = 'Bitte wählen Sie eine Datenquellenklasse !';
  strSelectAnFormat                     = 'Bitte wählen Sie ein Datenformat, oder erstellen Sie ein neues. Geben Sie dazu einen Namen für das neue Format ein, und klicken Sie den Konfigurieren Knopf.';
  strSelectAnDataSource                 = 'Bitte wählen Sie eine Datenquelle';
  strCreateAnFormat                     = 'Bitte erstellen Sei ein Datenformat. Geben Sie dazu einen Namen für das neue Format ein, und klicken Sie den Konfigurieren Knopf.';
  strConfigureDataSource                = 'Setzen Sie die Quelle / Einstellungen der Datenquelle und klicken Sie OK';
  strDataImport                         = 'Datenimport';
  strDataExport                         = 'Datenexport';
  strDataSource                         = 'Datenquelle';
  strDataDestination                    = 'Datenausgabe';
  strRealDelete                         = 'Möchten Sie das Profil wirklich löschen ?';

implementation

uses uDataImportConfig;

procedure RegisterImportSource(aClass: TImporterClass; aName: string);
var
  aTyp: TImporterTyp;
begin
  if not Assigned(ImportSources) then
    ImportSources := TList.Create;
  aTyp := TImporterTyp.Create;
  aTyp.ImporterClass:=aClass;
  aTyp.Name:=aName;
  ImportSources.Add(aTyp);
end;

procedure UnregisterImportSources;
var
  i: Integer;
begin
  for i := 0 to ImportSources.Count-1 do
    TImporterTyp(ImportSources[i]).Free;
end;

{$R *.lfm}

{ TfDataImport }

procedure TfDataImport.SetTarget(const AValue: TDataSource);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDataImport,fDataImport);
      Self := fDataImport;
    end;
  if FTraget=AValue then exit;
  FTraget:=AValue;
end;

procedure TfDataImport.CheckAll;
var
  ShowInfo: Boolean;
begin
  bpButtons.OKButton.Enabled:=False;
  ShowInfo := false;
  if (cbFormat.ItemIndex = -1) then
    begin
      if cbFormat.Items.Count = 0 then
        begin
          lInfo.Caption:=strCreateAnFormat;
          bChangeFormat.SetFocus;
        end
      else
        begin
          lInfo.Caption:=strSelectAnFormat;
          cbFormat.SetFocus;
        end;
      ShowInfo := True;
    end
  else if not ShowInfo then
    begin
      lInfo.Caption:=strConfigureDataSource;
      ShowInfo := True;
      bSourceOptions.SetFocus;
      bpButtons.OKButton.Enabled:=True;
    end;
  lInfo.Visible := ShowInfo;
end;

function TfDataImport.Execute(Typ : TImporterCapability;DefaultFormat: string): Boolean;
var
  i: Integer;
begin
  if Typ = icImport then
    begin
      Caption := strDataImport;
      lDesteny.Caption:=strDataSource;
    end
  else
    begin
      Caption := strDataExport;
      lDesteny.Caption:=strDataDestination;
    end;
  cbFormat.Items.Assign(fDataInput.Configs);
  FTyp := Typ;
  if cbFormat.Items.IndexOf(cbFormat.Text) = -1 then
    cbFormat.ItemIndex := -1;
  if cbFormat.Items.Count = 1 then
    cbFormat.ItemIndex := 0;
  Result := Showmodal = mrOK;
  if Result then
    begin
      Screen.Cursor:=crHourGlass;
      fDataInput.DestDataSet := Target.DataSet;
      fDataInput.ActiveFormat := cbFormat.Text;
      if (Typ = icImport) and fDataInput.DoImport then
        Result := True;
      if (Typ = icExport) and fDataInput.DoExport then
        Result := True;
      Screen.Cursor:=crDefault;
    end;
end;

procedure TfDataImport.bChangeFormatClick(Sender: TObject);
var
  i: Integer;
  aImporter: TImporter;
begin
  if cbFormat.Text = '' then
    begin
      Showmessage(strPleaseenteranFormatName);
      exit;
    end;
  fDataInput.DestDataSet := Target.DataSet;
  if fDataInput.Execute(FTyp,cbFormat.Text) then
    cbFormat.Items.Assign(fDataInput.Configs);
  CheckAll;
end;

procedure TfDataImport.bSourceOptionsClick(Sender: TObject);
begin
  fDataInput.ConfigDataSource(FTyp,cbFormat.Text,True);
  CheckAll;
end;

procedure TfDataImport.cbDataSourceChange(Sender: TObject);
begin
  CheckAll;
end;

procedure TfDataImport.cbFormatChange(Sender: TObject);
begin
  CheckAll;
end;

procedure TfDataImport.FormCreate(Sender: TObject);
begin
  FAppend := True;
end;

procedure TfDataImport.FormShow(Sender: TObject);
begin
  CheckAll;
end;

procedure TfDataImport.SpeedButton1Click(Sender: TObject);
begin
  if cbFormat.ItemIndex = -1 then exit;
  if MessageDlg(strRealDelete,mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      if DeleteFile(AppendPathDelim(AppendPathDelim(Application.Location+'importconfig')+FFilter)+cbFormat.Text+'.cfg') then
        cbFormat.Items.Delete(cbFormat.ItemIndex);
    end;
end;

procedure TfDataImport.SetFilter(const AValue: string);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDataImport,fDataImport);
      Self := fDataImport;
    end;
  if FFilter=AValue then exit;
  FFilter:=AValue;
  fDataInput.BaseDir:=AValue;
end;

procedure TfDataImport.SetAppend(const AValue: Boolean);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDataImport,fDataImport);
      Self := fDataImport;
    end;
  if FAppend=AValue then exit;
  FAppend:=AValue;
  fDataInput.AppendonExport:=AValue;
end;

procedure TfDataImport.SetConfigDir(AValue: string);
begin
  if not Assigned(fDataInput) then
    begin
      Application.CreateForm(TfDataInput,fDataInput);
    end;
  fDataInput.ConfigDir:=AValue;
end;

finalization
  UnregisterImportSources;
  ImportSources.Destroy;

end.

