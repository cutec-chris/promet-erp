unit ureporting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_RRect, LR_ChBox, LR_Shape, LR_DBSet,
  LR_DSet, LR_Desgn, lr_e_pdf, fpjson;

type

  { TPrintData }

  TPrintData = class(TDataModule)
    frCheckBoxObject1: TfrCheckBoxObject;
    frDesigner1: TfrDesigner;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    frTNPDFExport1: TfrTNPDFExport;
    frUserDataset1: TfrUserDataset;
    Report: TfrReport;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
  private
    { private declarations }
    DataSets : TList;
    RecordIndex : array of Integer;
  public
    procedure aDataSetCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure aDataSetFirst(Sender: TObject);
    procedure aDataSetNext(Sender: TObject);
    { public declarations }
    procedure GenerateDataSets(Json: TJSONData; aName: string = '');
  end;

var
  PrintData: TPrintData;

implementation

{$R *.lfm}

{ TPrintData }

procedure TPrintData.ReportGetValue(const ParName: String; var ParValue: Variant
  );
begin
  ParValue:=ParName;
end;

procedure TPrintData.aDataSetCheckEOF(Sender: TObject; var Eof: Boolean);
var
  aDS: TJSONObject;
begin
  aDS := TJSONObject(DataSets[TfrUserDataset(Sender).Tag]);
  Eof:=RecordIndex[TfrUserDataset(Sender).Tag]>=aDS.Count-1;
end;

procedure TPrintData.aDataSetFirst(Sender: TObject);
begin
  RecordIndex[TfrUserDataset(Sender).Tag] := 0;
end;

procedure TPrintData.aDataSetNext(Sender: TObject);
begin
  RecordIndex[TfrUserDataset(Sender).Tag] := RecordIndex[TfrUserDataset(Sender).Tag]+1;
end;

procedure TPrintData.DataModuleCreate(Sender: TObject);
begin
  DataSets := TList.Create;
end;

procedure TPrintData.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(DataSets);
end;

procedure TPrintData.GenerateDataSets(Json: TJSONData;aName : string = '');
var
  i: Integer;
  aDataSet: TfrUserDataset;
begin
  if (aName <> '') and (aName <> 'Fields') then
    begin
      aDataSet:= TfrUserDataset.Create(Self);
      aDataSet.Name:=aName;
      aDataSet.Tag := DataSets.Add(Json);
      aDataSet.OnCheckEOF:=@aDataSetCheckEOF;
      aDataSet.OnNext:=@aDataSetNext;
      aDataSet.OnFirst:=@aDataSetFirst;
      SetLength(RecordIndex,DataSets.Count+1);
      RecordIndex[DataSets.Count-1]:=0;
    end;
  for i := 0 to Json.Count-1 do
    begin
      if Json is TJSONObject then
        GenerateDataSets(Json.Items[i],TJSONObject(Json).Names[i])
      else
        GenerateDataSets(Json.Items[i]);
    end;
end;

end.

