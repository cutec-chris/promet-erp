unit uDataImportDBF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls, ComCtrls, uDataImport, SdfData, dbf,
  Utils;

type

  { TFixedLengthImport }

  TDbfImport = class(TImporter)
  protected
    function GetCapablities: TImporterCapabilities;override;
  public
    function Execute(Typ : TImporterCapability;ShowDialog : Boolean = False) : Boolean;override;
    function GetConfig : string;override;
    procedure SetConfig(Typ : TImporterCapability;aConfig : string);override;
  end;

  { TfDbfOptions }

  TfDbfOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    Dbf1: TDbf;
    feFile: TFileNameEdit;
    lFile: TLabel;
    procedure feFileAcceptFileName(Sender: TObject; var Value: String);
  private
    { private declarations }
    FTyp : TImporterCapability;
  public
    { public declarations }
  end; 

var
  fDbfOptions: TfDbfOptions;

resourcestring
  strDBFName                = 'DBase Datei';

implementation

{$R *.lfm}

{ TDbfImport }

function TDbfImport.GetCapablities: TImporterCapabilities;
begin
  Result:=[icImport];
end;

function TDbfImport.Execute(Typ : TImporterCapability;ShowDialog: Boolean): Boolean;
var
  tmp: String;
begin
  if not Assigned(fDBFOptions) then
    Application.CreateForm(TfDBFOptions,fDBFOptions);
  fDbfOptions.FTyp := Typ;
  if Typ <> icImport then exit;
  if Typ = icImport then
    fDbfOptions.feFile.DialogKind:=dkOpen
  else
    fDbfOptions.feFile.DialogKind:=dkSave;
  FDataSet := fDbfOptions.Dbf1;
  if ShowDialog or (not FDataSet.Active) then
    begin
      Result := fDbfOptions.ShowModal = mrOK;
      FDataSet := fDbfOptions.Dbf1;
      if Result then
        begin
          tmp := fDbfOptions.feFile.FileName;
          try
            fDbfOptions.feFileAcceptFileName(nil,tmp);
          except
            fDbfOptions.Dbf1.Active := False;
          end;
        end;
    end
  else
    Result := True;
end;

function TDbfImport.GetConfig: string;
begin
  with fDbfOptions do
    begin
      Result := feFile.FileName+';';
    end;
end;

procedure TDbfImport.SetConfig(Typ : TImporterCapability;aConfig: string);
var
  tmp: String;
begin
  if not Assigned(fDBFOptions) then
    Application.CreateForm(TfDBFOptions,fDBFOptions);
  tmp := aConfig;
  with fDbfOptions do
    begin
      FTyp := Typ;
      feFile.FileName:=copy(tmp,0,pos(';',tmp)-1);
      tmp := feFile.FileName;
      try
        feFileAcceptFileName(nil,tmp);
      except
        Dbf1.Active := False;
      end;
    end;
end;

{ TfDbfOptions }

procedure TfDbfOptions.feFileAcceptFileName(Sender: TObject; var Value: String
  );
var
  sl: TStringList;
  i: Integer;
begin
  Dbf1.Active:=False;
  Dbf1.FilePathFull:=UniToSys(ExtractFilePath(Value));
  Dbf1.TableName:=UniToSys(ExtractFileName(Value));
  try
    Dbf1.Active:=True;
  except
  end;
end;

initialization
  RegisterImportSource(TDbfImport,strDBFName);

end.
