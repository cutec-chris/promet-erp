unit uDataimportCSV;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditBtn, ButtonPanel, ComCtrls, uDataImport,Utils;

type

  { TCSVImport }

  TCSVImport = class(TImporter)
  protected
    function GetCapablities: TImporterCapabilities;override;
  public
    function Execute(Typ : TImporterCapability;ShowDialog : Boolean = False) : Boolean;override;
    function GetConfig : string;override;
    procedure SetConfig(Typ : TImporterCapability;aConfig : string);override;
  end;

  { TfCSVOptions }

  TfCSVOptions = class(TForm)
    Bevel1: TBevel;
    ButtonPanel1: TButtonPanel;
    cbFirstLineasShema: TCheckBox;
    eComment: TLabeledEdit;
    eDelemiter: TLabeledEdit;
    feFile: TFileNameEdit;
    Label1: TLabel;
    lOptions: TLabel;
    lFile: TLabel;
    mRows: TMemo;
    pOptions: TPanel;
    SDFDataSet: TSdfDataSet;
    TreeView1: TTreeView;
    procedure cbFirstLineasShemaChange(Sender: TObject);
    procedure eDelemiterChange(Sender: TObject);
    procedure feFileAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Collapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { private declarations }
    FTyp : TImporterCapability;
  public
    { public declarations }
  end; 

var
  fCSVOptions: TfCSVOptions;

implementation

resourcestring
  strCSVName                = 'Textdatei, mit Trennzeichen getrennt';

{$R *.lfm}

{ TCSVImport }

function TCSVImport.GetCapablities: TImporterCapabilities;
begin
  Result:=[icImport,icExport];
end;

function TCSVImport.Execute(Typ : TImporterCapability;ShowDialog : Boolean = False): Boolean;
begin
  if not Assigned(fCSVOptions) then
    Application.CreateForm(TfCSVOptions,fCSVOptions);
  if Typ = icImport then
    fCSVOptions.feFile.DialogKind:=dkOpen
  else
    fCSVOptions.feFile.DialogKind:=dkSave;
  fCSVOptions.FTyp := Typ;
  FDataSet := fCSVOptions.SDFDataSet;
  if ShowDialog or ((Typ=icImport) and (not FDataSet.Active)) then
    Result := fCSVOptions.ShowModal = mrOK
  else
    Result := True;
end;

function TCSVImport.GetConfig: string;
begin
  with fCSVOptions do
    begin
      Result := feFile.FileName+';"'+eDelemiter.Text+'";"'+eComment.Text+'";';
      if cbFirstLineasShema.Checked then
        Result := Result+'Y;'
      else
        Result := Result+'N;';
      mRows.Lines.Delimiter:=';';
      Result := Result+mRows.Lines.DelimitedText;
    end;
end;

procedure TCSVImport.SetConfig(Typ : TImporterCapability;aConfig: string);
var
  tmp: String;
begin
  if not Assigned(fCSVOptions) then
    Application.CreateForm(TfCSVOptions,fCSVOptions);
  tmp := aConfig;
  with fCSVOptions do
    begin
      FTyp := Typ;
      feFile.FileName:=copy(tmp,0,pos(';"',tmp)-1);
      tmp := copy(tmp,pos(';"',tmp)+2,length(tmp));
      eDelemiter.Text:=copy(tmp,0,pos('";"',tmp)-1);
      tmp := copy(tmp,pos('";"',tmp)+3,length(tmp));
      eComment.Text:=copy(tmp,0,pos('";',tmp)-1);
      tmp := copy(tmp,pos('";',tmp)+2,length(tmp));
      cbFirstLineasShema.Checked := copy(tmp,1,1) = 'Y';
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      mRows.Lines.Delimiter:=';';
      mRows.Lines.DelimitedText:=tmp;
      tmp := feFile.FileName;
      try
        feFileAcceptFileName(nil,tmp);
        cbFirstLineasShemaChange(nil);
      except
        SDFDataSet.Active := False;
      end;
    end;
end;

{ TfCSVOptions }

procedure TfCSVOptions.feFileAcceptFileName(Sender: TObject;
  var Value: String);
var
  sl: TStringList;
  i: Integer;
begin
  SDFDataSet.Active:=False;
  if FTyp = icImport then
    begin
      sl := TStringList.Create;
      sl.LoadFromFile(UniToSys(Value));
      i := 0;
      //Delete Comments
      while i < sl.Count do
        begin
          if (length(eComment.Text) > 0) and (copy(sl[i],0,length(eComment.Text)) = eComment.Text) then
            sl.Delete(i)
          else
            inc(i);
        end;
      sl.SaveToFile(GetTempDir+'import.tmp.csv');
      sl.Free;
      SDFDataSet.ReadOnly:=True;
      SDFDataSet.FileMustExist:=True;
      SDFDataSet.FileName:=GetTempDir+'import.tmp.csv';
    end
  else if FTyp = icExport then
    begin
      SDFDataSet.ReadOnly:=False;
      SDFDataSet.FileMustExist:=False;
      SDFDataSet.FileName:=UniToSys(Value);
    end;
  try
//    if FTyp = icImport then
    SDFDataSet.Active:=True;
  except
  end;
  if cbFirstLineasShema.Checked then
    mRows.Lines.Assign(SDFDataSet.Schema);
end;

procedure TfCSVOptions.FormCreate(Sender: TObject);
begin
  TreeView1.Items[0].Collapse(False);
  Height := 118;
end;

procedure TfCSVOptions.TreeView1Collapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  if not Visible then exit;
  Height := 118;
end;

procedure TfCSVOptions.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if not Visible then exit;
  Height := 300;
end;

procedure TfCSVOptions.cbFirstLineasShemaChange(Sender: TObject);
var
  tmp: String;
begin
  SDFDataSet.Active:=False;
  SDFDataSet.FirstLineAsSchema:=cbFirstLineasShema.Checked;
  if not cbFirstLineasShema.Checked then
    SDFDataSet.Schema.Assign(mRows.Lines);
  try
    tmp := feFile.FileName;
    feFileAcceptFileName(nil,tmp);
  except
  end;
  if cbFirstLineasShema.Checked then
    mRows.Lines.Assign(SDFDataSet.Schema);
//  mRows.ReadOnly:=cbFirstLineasShema.Checked;
end;

procedure TfCSVOptions.eDelemiterChange(Sender: TObject);
var
  c : Char;
  tmp: String;
begin
  SDFDataSet.Active:=False;
  if length(eDelemiter.Text) > 0 then
    c := copy(eDelemiter.Text,0,1)[1];
  SDFDataSet.Delimiter:=c;
  try
    tmp := feFile.FileName;
    feFileAcceptFileName(nil,tmp);
  except
  end;
  if cbFirstLineasShema.Checked then
    mRows.Lines.Assign(SDFDataSet.Schema);
end;

initialization
  RegisterImportSource(TCSVImport,strCSVName);

end.
