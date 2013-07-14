 program traceimport;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this }, DBFLaz,Dbf,db,Utils, uAppconsts,
  FileUtil,Forms,uBaseDataModule,uData, synapse,
  Dialogs,Controls,synautil,httpsend,
  dom,xmlread,md5,
  uIntfStrConsts,uDocumentManagement,Variants;

type

  { TTraceImport }

  TTraceImport = class(TCustomApplication)
  private
    mailaccounts : string;
  protected
    procedure DoRun; override;
    procedure WriteMessage(s : string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TTraceImport }

procedure TTraceImport.DoRun;
var
  dbMandant: TDbf;
  DataDB : TBaseDataModule;
  http: THTTPSend;
  aNode : TDOMNode;
  Doc : TXMLDocument;
  tmp: String;
  MID: String;
  BMID : Int64;
  aSendDate: Double;
  aFeedName: String;
  aTreeEntry: Integer;
  ss: TStringStream;
  MyFormatSettings : TFormatSettings;
  OldformatSettings: TFormatSettings;
  aDateNode: TDOMNode;
  aIDNode: TDOMNode;
  aTitleNode: TDOMNode;
  aMessageNode: TDOMNode;
  FeedType: String;
  aLinkValue: String;
  aProp: String;
begin
  Data := TData.Create(Self);
  Data.AppendMandant;
  with Data.Mandants.DataSet do
    begin
      First;
      if not Locate('NAME',Application.GetOptionValue('m','mandant'),[]) then
        begin
          Close;
          writeln('Mandant not found !');
          Halt;
          exit;
        end;
      DataDB := CreateDataModule(Data.Mandants.DataSet.FieldByName('DBTYP').AsString);
      if Assigned(DataDB) then
        begin
          writeln('Datamodule assigned, opening...');
          DataDB.SetProperties(Data.Mandants.DataSet.FieldbyName('DBPROP').AsString);
          Data.DataModule := DataDB;
          try
            if not DataDB.CreateDB then raise Exception.Create('Cant Create DS');
            DataDB.Users.Open;
          except
            FreeAndNil(DataDB);
            halt;
            exit;
          end;
          Data.ActivateDataModule;
          with Data.DataModule do
            begin
              Options.Open;
              DeletedItems.Open;
              Orders.Open;
              OrderType.Open;
              Currency.Open;
              Paymenttargets.Open;
              OrderPosTyp.Open;
              OrderPos.Open;
            end;
          writeln('Datamodule open...');
          while not Terminated do
            begin
              //do whatever you want to do

            end;
        end;
    end;
  // stop program loop
  Terminate;
end;

procedure TTraceImport.WriteMessage(s: string);
begin
  writeln(s);
end;

constructor TTraceImport.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTraceImport.Destroy;
begin
  Data.DeactivateDataModule;
  Data.Free;
  inherited Destroy;
end;

var
  Application: TTraceImport;

begin
  Application:=TTraceImport.Create(nil);
  Application.Run;
  Application.Free;
end.

