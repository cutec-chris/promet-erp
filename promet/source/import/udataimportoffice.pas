unit uDataImportOffice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls, ComCtrls, uDataImport, memds, db,
  fpspreadsheet, fpsallformats,Utils;

type

  { TFixedLengthImport }

  { TOfficeImport }

  TOfficeImport = class(TImporter)
    procedure fOfficeOptionsBeforeClose(DataSet: TDataSet);
  protected
    function GetCapablities: TImporterCapabilities;override;
  public
    function Execute(Typ : TImporterCapability;ShowDialog : Boolean = False) : Boolean;override;
    function GetConfig : string;override;
    procedure SetConfig(Typ : TImporterCapability;aConfig : string);override;
  end;

  { TfOfficeOptions }

  TfOfficeOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    feFile: TFileNameEdit;
    lFile: TLabel;
    MemDataset1: TMemDataset;
    procedure feFileAcceptFileName(Sender: TObject; var Value: String);
  private
    { private declarations }
    FTyp : TImporterCapability;
  public
    { public declarations }
  end; 

var
  fOfficeOptions: TfOfficeOptions;

resourcestring
  strOfficeName                = 'Office Datei';

implementation

{$R *.lfm}

{ TOfficeImport }

procedure TOfficeImport.fOfficeOptionsBeforeClose(DataSet: TDataSet);
var
  MyWorkBook: TsWorkbook;
  Sheet: TsWorksheet;
  x: Integer;
  y : Integer = 0;
begin exit;
  //Write File
  MyWorkBook := TsWorkBook.Create;
  try
    DataSet.DisableControls;
    DataSet.First;
    Sheet := MyWorkBook.AddWorksheet('Export');
    while not DataSet.EOF do
      begin
        for x := 0 to DataSet.Fields.Count-1 do
          begin
            if DataSet.Fields[x] is TNumericField then
              Sheet.WriteNumber(y,x,DataSet.Fields[x].AsFloat)
            else
              Sheet.WriteUTF8Text(y,x,DataSet.Fields[x].AsWideString);
          end;
        inc(y);
        DataSet.Next;
      end;
    DataSet.EnableControls;
  except
  end;
  MyWorkBook.WriteToFile(UniToSys(fOfficeOptions.feFile.FileName),TsSpreadsheetFormat(fOfficeOptions.feFile.FilterIndex-1));
  MyWorkBook.Free;
end;

function TOfficeImport.GetCapablities: TImporterCapabilities;
begin
  Result:=[icImport,icExport];
end;

function TOfficeImport.Execute(Typ : TImporterCapability;ShowDialog: Boolean): Boolean;
var
  tmp: String;
begin
  if not Assigned(fOfficeOptions) then
    Application.CreateForm(TfOfficeOptions,fOfficeOptions);
  fOfficeOptions.FTyp := Typ;
  if Typ = icImport then
    fOfficeOptions.feFile.DialogKind:=dkOpen
  else
    fOfficeOptions.feFile.DialogKind:=dkSave;
  FDataSet := fOfficeOptions.MemDataSet1;
  if ShowDialog then
    begin
      Result := fOfficeOptions.ShowModal = mrOK;
      FDataSet := fOfficeOptions.MemDataSet1;
      if Result then
        begin
          tmp := fOfficeOptions.feFile.FileName;
          try
            fOfficeOptions.feFileAcceptFileName(nil,tmp);
          except
            fOfficeOptions.MemDataSet1.Active := False;
          end;
          fOfficeOptions.MemDataSet1.BeforeClose:=nil;
          if fOfficeOptions.FTyp = icExport then
            fOfficeOptions.MemDataSet1.BeforeClose:=@fOfficeOptionsBeforeClose;
        end;
    end
  else
    Result := True;
end;

function TOfficeImport.GetConfig: string;
begin
  with fOfficeOptions do
    begin
      Result := feFile.FileName+';';
    end;
end;

procedure TOfficeImport.SetConfig(Typ : TImporterCapability;aConfig: string);
var
  tmp: String;
begin
  if not Assigned(fOfficeOptions) then
    Application.CreateForm(TfOfficeOptions,fOfficeOptions);
  tmp := aConfig;
  with fOfficeOptions do
    begin
      FTyp := Typ;
      feFile.FileName:=copy(tmp,0,pos(';',tmp)-1);
      tmp := feFile.FileName;
      try
        feFileAcceptFileName(nil,tmp);
      except
        MemDataSet1.Active := False;
      end;
    end;
end;

{ TfOfficeOptions }

procedure TfOfficeOptions.feFileAcceptFileName(Sender: TObject; var Value: String
  );
var
  MyWorkbook: TsWorkbook;
  Sheet: TsWorksheet;
  x: Integer;
  y: Integer;
begin
  MemDataSet1.Active:=False;
  if FTyp = icImport then
    begin
      MyWorkBook := TsWorkBook.Create;
      try
        MyWorkbook.ReadFromFile(UniToSys(Value),TsSpreadsheetFormat(feFile.FilterIndex-1));
        if MyWorkBook.GetWorksheetCount > 0 then
          begin
            Sheet := MyWorkBook.GetFirstWorksheet;
            for x := 0 to Sheet.GetLastColNumber do
              MemDataSet1.FieldDefs.Add(chr(ord('A')+x),ftString,10000);
          end;
      except
        MyWorkBook.Free;
        exit;
      end;
      try
        MemDataSet1.Active:=True;
        MemDataSet1.DisableControls;
        for y := 0 to Sheet.GetLastRowNumber-1 do
          with MemDataSet1 do
            begin
              Append;
              for x := 0 to Sheet.GetLastColNumber do
                Fields[x].AsString:=UniToSys(Sheet.ReadAsUTF8Text(y,x));
              Post;
            end;
        MemDataSet1.EnableControls;
      except
      end;
      MyWorkBook.Free;
    end
  else if FTyp = icExport then
    begin
      for x := 0 to 20 do
        MemDataSet1.FieldDefs.Add(chr(ord('A')+x),ftString,10000);
      MemDataSet1.Active:=True;
    end;
end;

initialization
  RegisterImportSource(TOfficeImport,strOfficeName);

end.
