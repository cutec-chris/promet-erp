unit uDataImportConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, ExtCtrls, db, Utils, uDataImport, Variants, uminiconvencoding;

type

  { TfDataInput }

  TfDataInput = class(TForm)
    bAdd: TBitBtn;
    bDelete: TBitBtn;
    bInsertCommand: TBitBtn;
    bSourceOptions: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbCommand: TComboBox;
    cbDataSource: TComboBox;
    eFixValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbDestColumns: TListBox;
    lbSourceColumns: TListBox;
    lbResult: TListBox;
    leDecimal: TLabeledEdit;
    rbFirstrecord: TRadioButton;
    rbLastrecord: TRadioButton;
    procedure bAddClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bInsertCommandClick(Sender: TObject);
    procedure bSourceOptionsClick(Sender: TObject);
    procedure cbDataSourceSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FActiveFormat: string;
    FAppend: Boolean;
    FConfigDir: string;
    FConfigs: TStringList;
    FDDS: TDataSet;
    FFilter: string;
    FSDS: TDataSet;
    FTyp : TImporterCapability;
    aImporter : TImporter;
    actFormat : string;
    procedure SetDestDS(const AValue: TDataSet);
    procedure SetFilter(const AValue: string);
    procedure SetSourceDS(const AValue: TDataSet);
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property SourceDataSet : TDataSet read FSDS write SetSourceDS;
    property DestDataSet : TDataSet read FDDS write SetDestDS;
    function Execute(Typ : TImporterCapability;ConfigName : string) : Boolean;
    property ActiveFormat : string read FActiveFormat write FActiveFormat;
    function DoImport(ConfigName : string = '') : Boolean;
    function DoExport(ConfigName : string = '') : Boolean;
    function GetConfig(ConfigName : string) : string;
    procedure SetConfig(Configname,ConfigValue : string);
    property Configs : TStringList read FConfigs;
    function ConfigDataSource(Typ : TImporterCapability;aFormat : string;ShowDialog : Boolean = False;SetDataSource : Boolean = True) : Boolean;
    property BaseDir : string read FFilter write SetFilter;
    property ConfigDir : string read FConfigDir write FConfigDir;
    property AppendonExport : Boolean read FAppend write FAppend;
  end;

var
  fDataInput: TfDataInput;

procedure SplitCurrent(Dataset:Tdataset);

implementation

{$R *.lfm}

{ TfDataInput }

procedure TfDataInput.SetSourceDS(const AValue: TDataSet);
var
  i: Integer;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDataInput,fDataInput);
      Self := fDataInput;
    end;
  FSDS:=AValue;
  lbSourceColumns.Items.Clear;
  if Assigned(FSDS) then
    for i := 0 to FSDS.FieldDefs.Count-1 do
      lbSourceColumns.Items.Add(FSDS.FieldDefs[i].Name);
end;
constructor TfDataInput.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigDir:=Application.Location;
end;
function TfDataInput.Execute(Typ : TImporterCapability;ConfigName : string): Boolean;
var
  i: Integer;
begin
  actFormat := Configname;
  FTyp := Typ;
  cbDataSource.Clear;
  if Assigned(ImportSources) then
    begin
      for i := 0 to ImportSources.Count-1 do
        cbDataSource.Items.Add(TImporterTyp(ImportSources[i]).Name);
    end;
  if cbDataSource.Items.Count = 1 then
    cbDataSource.ItemIndex := 0;
  ConfigDataSource(Typ,actFormat,False);
  Result := false;
  if not Assigned(FDDS) then exit;
  if Assigned(aImporter) and aImporter.Execute(Typ) then
    SourcedataSet := aImporter.DataSource;
  Result := Showmodal = mrOK;
  if Result then
    begin
      if Assigned(ImportSources) then
        begin
          for i := 0 to ImportSources.Count-1 do
            if TImporterTyp(ImportSources[i]).Name = cbDataSource.Text then
              begin
                if Assigned(aImporter) then
                  begin
                    if aImporter.ClassType <> TImporterClass(TImporterTyp(ImportSources[i]).ImporterClass) then
                      FreeAndNil(aImporter);
                  end;
                if not Assigned(aImporter) then
                  aImporter := TImporterClass(TImporterTyp(ImportSources[i]).ImporterClass).Create;
              end;
        end;
      lbResult.Items.Insert(0,aImporter.GetConfig);
      lbResult.Items.Insert(0,'DS:'+cbDataSource.Text);
      if rbLastRecord.Checked then
        lbResult.Items.Insert(0,'LASTRECORDFIRST');
      if leDecimal.Text <> '<system>' then
        lbResult.Items.Insert(0,'DT:'+leDecimal.Text);
      lbResult.Items.SaveToFile(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+Configname+'.cfg');
      SetFilter(FFilter);
    end;
end;

procedure SplitCurrent(Dataset:Tdataset);
var
  aField : Variant;
  i      : Integer;
begin
  // Create a variant Array
  aField := VarArrayCreate([0,DataSet.Fieldcount-1],VarVariant);
  // read values into the array
  for i := 0 to (DataSet.Fieldcount-1) do
    aField[i] := DataSet.fields[i].Value ;
  DataSet.Append ;
  // Put array values into new the record
  for i := 0 to (DataSet.Fieldcount-1) do
    if DataSet.Fields[i].FieldName <> 'SQL_ID' then
      DataSet.fields[i].Value := aField[i] ;
end;

function TfDataInput.DoImport(ConfigName: string): Boolean;
var
  Step: Integer;
  tmp: String;
  aField: TField;
  i: Integer;
  tmpd: String;
begin
  Result := false;
  if not Assigned(FDDS) then exit;
  if ConfigName = '' then
    actFormat := ActiveFormat
  else
    actFormat := Configname;
  ConfigDataSource(icImport,actFormat,False);
  Result := false;
  if not Assigned(aImporter) then exit;
  if not (icImport in aImporter.Capabilities) then exit;
  if aImporter.Execute(icImport) then
    begin
      SourcedataSet := aImporter.DataSource;
      with SourceDataSet do
        begin
          if (not rbLastRecord.Checked) then
            First
          else
            Last;
          while (rbLastRecord.Checked and (not BOF)) or ((not rbLastRecord.Checked) and (not EOF)) do
            begin
              Step := 0;
              while Step < lbResult.Count do
                begin
                  if lbResult.Items[Step] = 'APPEND' then
                    DestDataSet.Append
                  else if lbResult.Items[Step] = 'INSERT' then
                    DestDataSet.Insert
                  else if lbResult.Items[Step] = 'POST' then
                    DestDataSet.Post
                  else if lbResult.Items[Step] = 'SPLIT' then
                    SplitCurrent(DestDataSet)
                  else if pos('=',lbResult.Items[Step]) > 0 then
                    begin
                      try
                      aField := DestDataSet.FieldByName(copy(lbResult.Items[Step],0,pos('=',lbResult.Items[Step])-1));
                      if Assigned(aField) then
                        begin
                          tmp := copy(lbResult.Items[Step],pos('=',lbResult.Items[Step])+1,length(lbResult.Items[Step]));
                          if copy(tmp,0,1) = '"' then
                            begin
                              tmp := copy(tmp,2,length(tmp));
                              tmp := copy(tmp,0,rpos('"',tmp)-1);
                              aField.AsString:=ConvertEncoding(tmp,GuessEncoding(tmp),EncodingUTF8);
                            end
                          else if Assigned(SourceDataSet.FindField(tmp)) then
                            begin
                              if aField.DataType = ftFloat then
                                begin
                                  if pos('.',SourceDataSet.FindField(tmp).AsString) > 0 then
                                    aField.AsString := StringReplace(SourceDataSet.FindField(tmp).AsString,'.',DecimalSeparator,[])
                                  else
                                    aField.AsVariant := SourceDataSet.FindField(tmp).AsVariant;
                                end
                              else if (aField.DataType = ftString) or (aField.DataType = ftMemo) then
                                begin
                                  tmpd := SourceDataSet.FindField(tmp).AsString;
                                  tmpd := ConvertEncoding(tmpd,GuessEncoding(tmpd),EncodingUTF8);
                                  aField.AsString := tmpd;
                                end
                              else
                                aField.AsVariant := SourceDataSet.FindField(tmp).AsVariant;
                            end;
                        end;
                      except
                      end;
                    end;
                  inc(Step);
                end;
              if (not rbLastRecord.Checked) then
                Next
              else
                Prior;
            end;
        end;
      aImporter.DataSource.Active:=False;
    end;
end;

function TfDataInput.DoExport(ConfigName: string): Boolean;
var
  Step: Integer;
  aField: TField;
  tmp: String;
  OldSeparator: Char;
begin
  Result := false;
  if not Assigned(FDDS) then exit;
  if ConfigName = '' then
    actFormat := ActiveFormat
  else
    actFormat := Configname;
  ConfigDataSource(icExport,actFormat,False);
  Result := false;
  if not Assigned(aImporter) then exit;
  if not (icExport in aImporter.Capabilities) then exit;
  if aImporter.Execute(icExport) then
    begin
      SourcedataSet := aImporter.DataSource;
      if not SourcedataSet.Active then
        SourceDataSet.Active := True;
      if not FAppend then
        begin
          while (not SourceDataSet.EOF) do
            SourceDataSet.Delete;
        end;
      with DestDataSet do
        begin
          if (not rbLastRecord.Checked) then
            DestDataSet.First
          else
            DestDataSet.Last;
          while (rbLastRecord.Checked and (not DestDataSet.BOF)) or ((not rbLastRecord.Checked) and (not DestDataSet.EOF)) do
            begin
//              if Assigned(OnProgess) then
//                OnProgress(Self);
              Step := 0;
              while Step < lbResult.Count do
                begin
                  if lbResult.Items[Step] = 'APPEND' then
                    SourceDataSet.Append
                  else if lbResult.Items[Step] = 'INSERT' then
                    SourceDataSet.Insert
                  else if lbResult.Items[Step] = 'POST' then
                    SourceDataSet.Post
                  else if lbResult.Items[Step] = 'SPLIT' then
                    Next
                  else if pos('=',lbResult.Items[Step]) > 0 then
                    begin
                      try
                      tmp := lbResult.Items[Step];
                      aField := SourceDataSet.FieldByName(copy(tmp,pos('=',tmp)+1,length(tmp)));
                      if Assigned(aField) then
                        begin
                          tmp := copy(tmp,0,pos('=',tmp)-1);
                          if copy(tmp,0,1) = '"' then
                            begin
                              tmp := copy(tmp,2,length(tmp));
                              tmp := copy(tmp,0,rpos('"',tmp)-1);
                              aField.AsString:=tmp;
                            end
                          else if Assigned(DestDataSet.FindField(tmp)) then
                            begin
                              if DestDataSet.FindField(tmp).DataType = ftFloat then
                                begin
                                  if leDecimal.Text <> '<system>' then
                                    begin
                                      OldSeparator := DecimalSeparator;
                                      if length(leDecimal.Text) >= 1 then
                                        DecimalSeparator  := leDecimal.Text[1];
                                      aField.AsString := FormatFloat('0.0',DestDataSet.FindField(tmp).AsFloat);
                                      DecimalSeparator := OldSeparator;
                                    end
                                  else
                                    aField.AsFloat := DestDataSet.FindField(tmp).AsFloat;
                                end
                              else if not DestDataSet.FindField(tmp).IsNull then
                                aField.AsString := DestDataSet.FindField(tmp).AsString;
                            end;
                        end;
                      except
                      end;
                    end;
                  inc(Step);
                end;
              if (not rbLastRecord.Checked) then
                DestDataSet.Next
              else
                DestDataSet.Prior;
            end;
        end;
      aImporter.DataSource.Active:=False;
    end;
end;

function TfDataInput.GetConfig(ConfigName: string): string;
begin
  Result := '';
  if FileExists(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+Configname+'.cfg') then
    begin
      lbResult.Items.LoadFromFile(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+Configname+'.cfg');
      if lbResult.Items.Count > 0 then
        begin
          Result := lbResult.Items[0];
          lbResult.Items.Delete(0);
        end;
    end;
end;

procedure TfDataInput.SetConfig(Configname, ConfigValue: string);
begin
  if FileExists(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+Configname+'.cfg') then
    begin
      lbResult.Items.LoadFromFile(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+Configname+'.cfg');
      if lbResult.Items.Count > 0 then
        begin
          lbResult.Items[0] := ConfigValue;
          lbResult.Items.SaveToFile(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+Configname+'.cfg');
        end;
    end;
end;

function TfDataInput.ConfigDataSource(Typ : TImporterCapability;aFormat : string;ShowDialog: Boolean;SetDataSource : Boolean = True): Boolean;
var
  i: Integer;
begin
  lbResult.Items.Text:='APPEND'+lineending;
  rbFirstRecord.Checked:=True;
  if FileExists(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+aFormat+'.cfg') then
    begin
      lbResult.Items.LoadFromFile(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+aFormat+'.cfg');
      if lbResult.Items.Count > 0 then
        begin
          if copy(lbResult.Items[0],0,3) = 'DT:' then
            begin
              leDecimal.Text:=copy(lbResult.Items[0],4,length(lbResult.Items[0]));
              lbResult.Items.Delete(0);
            end
          else leDecimal.Text:='<system>';
          if lbResult.Items[0] = 'LASTRECORDFIRST' then
            begin
              rbLastRecord.Checked:=True;
              lbResult.Items.Delete(0);
            end;
          if copy(lbResult.Items[0],0,3) = 'DS:' then
            begin
              if SetDataSource then
                cbDataSource.Text:=copy(lbResult.Items[0],4,length(lbResult.Items[0]));
              lbResult.Items.Delete(0);
            end;
          if Assigned(ImportSources) then
            begin
              for i := 0 to ImportSources.Count-1 do
                if TImporterTyp(ImportSources[i]).Name = cbDataSource.Text then
                  begin
                    if Assigned(aImporter) then
                      begin
                        if aImporter.ClassType <> TImporterClass(TImporterTyp(ImportSources[i]).ImporterClass) then
                          FreeAndNil(aImporter);
                      end;
                    if not Assigned(aImporter) then
                      aImporter := TImporterClass(TImporterTyp(ImportSources[i]).ImporterClass).Create;
                  end;
            end;
          if Assigned(aImporter) then
            aImporter.SetConfig(Typ,lbResult.Items[0]);
          lbResult.Items.Delete(0);
        end;
    end
  else
    begin
      if Assigned(ImportSources) then
        begin
          for i := 0 to ImportSources.Count-1 do
            if TImporterTyp(ImportSources[i]).Name = cbDataSource.Text then
              begin
                if Assigned(aImporter) then
                  begin
                    if aImporter.ClassType <> TImporterClass(TImporterTyp(ImportSources[i]).ImporterClass) then
                      FreeAndNil(aImporter);
                  end;
                if not Assigned(aImporter) then
                  aImporter := TImporterClass(TImporterTyp(ImportSources[i]).ImporterClass).Create;
              end;
        end;
    end;
  if Assigned(aImporter) then
    if aImporter.Execute(Typ,ShowDialog) then
      begin
        lbResult.Items.Insert(0,aImporter.GetConfig);
        lbResult.Items.Insert(0,'DS:'+cbDataSource.Text);
        if rbLastRecord.Checked then
          lbResult.Items.Insert(0,'LASTRECORDFIRST');
        if leDecimal.Text <> '<system>' then
          lbResult.Items.Insert(0,'DT:'+leDecimal.Text);
        lbResult.Items.SaveToFile(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+aFormat+'.cfg');
        if lbResult.Items[0] = 'LASTRECORDFIRST' then
          lbResult.Items.Delete(0);
        if copy(lbResult.Items[0],0,3) = 'DS:' then
          lbResult.Items.Delete(0);
        lbResult.Items.Delete(0);
        SourceDataSet := aImporter.DataSource;
      end;
end;

procedure TfDataInput.bDeleteClick(Sender: TObject);
begin
  if lbResult.ItemIndex > -1 then
    lbResult.Items.Delete(lbResult.ItemIndex);
end;

procedure TfDataInput.bInsertCommandClick(Sender: TObject);
begin
  if cbCommand.Text <> '' then
    lbResult.Items.Add(cbCommand.Text);
end;

procedure TfDataInput.bSourceOptionsClick(Sender: TObject);
begin
  ConfigDataSource(FTyp,actFormat,True);
end;

procedure TfDataInput.cbDataSourceSelect(Sender: TObject);
begin
  ConfigDataSource(FTyp,actFormat,False,False);
end;

procedure TfDataInput.FormCreate(Sender: TObject);
begin
  FConfigs := TStringList.Create;
  FAppend := True;
  SetFilter('');
end;

procedure TfDataInput.FormDestroy(Sender: TObject);
begin
  FConfigs.Free;
end;

procedure TfDataInput.bAddClick(Sender: TObject);
begin
  if lbDestColumns.ItemIndex = -1 then exit;
  if lbSourceColumns.ItemIndex = -1 then
    lbResult.Items.Add(lbDestColumns.Items[lbDestColumns.ItemIndex]+'="'+eFixValue.Text+'"')
  else
    lbResult.Items.Add(lbDestColumns.Items[lbDestColumns.ItemIndex]+'='+lbSourceColumns.Items[lbSourceColumns.ItemIndex]);
  lbSourceColumns.ItemIndex := -1;
  lbDestColumns.ItemIndex := -1;
end;

procedure TfDataInput.SetDestDS(const AValue: TDataSet);
var
  i: Integer;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDataInput,fDataInput);
      Self := fDataInput;
    end;
  if FDDS=AValue then exit;
  FDDS:=AValue;
  lbDestColumns.Items.Clear;
  for i := 0 to FDDS.FieldDefs.Count-1 do
    lbDestColumns.Items.Add(FDDS.FieldDefs[i].Name);
end;

procedure TfDataInput.SetFilter(const AValue: string);
var
  Info: TSearchRec;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDataInput,fDataInput);
      Self := fDataInput;
    end;
  if not DirectoryExists(AppendPathDelim(FConfigDir+'importconfig')+FFilter) then
    ForceDirectories(AppendPathDelim(FConfigDir+'importconfig')+FFilter);
  FConfigs.Clear;
  FFilter:=AValue;
  if FindFirst(AppendPathDelim(AppendPathDelim(FConfigDir+'importconfig')+FFilter)+'*.cfg',faAnyFile,Info)=0 then
    repeat
      With Info do
        FConfigs.Add(copy(Name,0,rpos('.',Name)-1));
    until FindNext(info)<>0;
  FindClose(Info);
end;

end.

