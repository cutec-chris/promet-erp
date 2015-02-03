unit uDataImportFixedLength;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls, ComCtrls, uDataImport, SdfData,Utils;

type

  { TFixedLengthImport }

  TFixedLengthImport = class(TImporter)
  protected
    function GetCapablities: TImporterCapabilities;override;
  public
    function Execute(Typ : TImporterCapability;ShowDialog : Boolean = False) : Boolean;override;
    function GetConfig : string;override;
    procedure SetConfig(Typ : TImporterCapability;aConfig : string);override;
  end;

  { TfFixedOptions }

  TfFixedOptions = class(TForm)
    Bevel1: TBevel;
    ButtonPanel1: TButtonPanel;
    eComment: TLabeledEdit;
    feFile: TFileNameEdit;
    FLDataSet: TFixedFormatDataSet;
    Label1: TLabel;
    lFile: TLabel;
    lOptions: TLabel;
    mRows: TMemo;
    pptions: TPanel;
    TreeView1: TTreeView;
    procedure eCommentChange(Sender: TObject);
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
  fFixedOptions: TfFixedOptions;

resourcestring
  strFLName                = 'Textdatei, feste Spaltenlänge';

implementation

{$R *.lfm}

{ TFixedLengthImport }

function TFixedLengthImport.GetCapablities: TImporterCapabilities;
begin
  Result:=[icImport,icExport];
end;

function TFixedLengthImport.Execute(Typ : TImporterCapability;ShowDialog: Boolean): Boolean;
var
  tmp: String;
begin
  if not Assigned(fFixedOptions) then
    Application.CreateForm(TfFixedOptions,fFixedOptions);
  if Typ = icImport then
    fFixedOptions.feFile.DialogKind:=dkOpen
  else
    fFixedOptions.feFile.DialogKind:=dkSave;
  FDataSet := fFixedOptions.FLDataSet;
  fFixedOptions.FTyp := Typ;
  if ShowDialog or ((Typ=icImport) and (not FDataSet.Active)) then
    begin
      Result := fFixedOptions.ShowModal = mrOK;
      FDataSet := fFixedOptions.FLDataSet;
      if Result then
        begin
          tmp := fFixedOptions.feFile.FileName;
          try
            fFixedOptions.feFileAcceptFileName(nil,tmp);
          except
            fFixedOptions.FLDataSet.Active := False;
          end;
        end;
    end
  else
    Result := True;
end;

function TFixedLengthImport.GetConfig: string;
begin
  with fFixedOptions do
    begin
      Result := feFile.FileName+';"'+eComment.Text+'";';
      mRows.Lines.Delimiter:=';';
      Result := Result+mRows.Lines.DelimitedText;
    end;
end;

procedure TFixedLengthImport.SetConfig(Typ : TImporterCapability;aConfig: string);
var
  tmp: String;
begin
  if not Assigned(fFixedOptions) then
    Application.CreateForm(TfFixedOptions,fFixedOptions);
  tmp := aConfig;
  with fFixedOptions do
    begin
      FTyp := Typ;
      feFile.FileName:=copy(tmp,0,pos(';"',tmp)-1);
      tmp := copy(tmp,pos(';"',tmp)+2,length(tmp));
      eComment.Text:=copy(tmp,0,pos('";',tmp)-1);
      tmp := copy(tmp,pos('";',tmp)+2,length(tmp));
      mRows.Lines.Delimiter:=';';
      mRows.Lines.DelimitedText:=tmp;
      tmp := feFile.FileName;
      try
        feFileAcceptFileName(nil,tmp);
      except
        FLDataSet.Active := False;
      end;
    end;
end;

{ TfFixedOptions }

procedure TfFixedOptions.feFileAcceptFileName(Sender: TObject; var Value: String
  );
var
  sl: TStringList;
  i: Integer;
begin
  FlDataSet.Active:=False;
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
      FLDataSet.ReadOnly:=True;
      FLDataSet.FileMustExist:=True;
      FLDataSet.FileName:=GetTempDir+'import.tmp.csv';
    end
  else if FTyp = icExport then
    begin
      FLDataSet.ReadOnly:=False;
      FLDataSet.FileMustExist:=False;
      FLDataSet.FileName:=UniToSys(Value);
    end;
  FLDataSet.Schema.Assign(mRows.Lines);
  try
//    if FTyp = icImport then
    FLDataSet.Active:=True;
  except
  end;
end;

procedure TfFixedOptions.FormCreate(Sender: TObject);
begin
  TreeView1.Items[0].Collapse(False);
  Height := 118;
end;

procedure TfFixedOptions.TreeView1Collapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  if not Visible then exit;
  Height := 118;
end;

procedure TfFixedOptions.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if not Visible then exit;
  Height := 300;
end;

procedure TfFixedOptions.eCommentChange(Sender: TObject);
var
  tmp: String;
begin
  tmp := feFile.FileName;
  try
    feFileAcceptFileName(nil,tmp);
  except
    FLDataSet.Active := False;
  end;
end;

initialization
  RegisterImportSource(TFixedLengthImport,strFLName);

end.
