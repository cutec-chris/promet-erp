unit uPrometORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SQLDB, MSSQLConn, SQLite3Conn,ubasestreamer;

type
  { TSQLStreamer }

  TSQLStreamer = class(TBaseStreamer)
  private
    FObject : TPersistent;
  public
    //we try to use this class with the same transaction/connection the whole time
    //so its added during Constructor when using it in another thread, it should be used with another Transaction
    constructor Create(Context : TThreadID;Obj : TPersistent);
    //generates SQL to Fill all Published properties and Gerneric TFPGList Types
    //(generates recursive joined Query for default TFPGList Type (or if only one is avalible) and separate Querys for all other)
    //only when this query fails the table structure for all sub-tables is checked so without changes of the table structure we dont have overhead
    procedure Load(Cascadic : Boolean);override;
    //Generates recursive an update Statement per record if SQL_ID is filled or n insert stetement if not
    procedure Save(Cascadic : Boolean);override;
  end;

  { TLockedQuery }

  TLockedQuery = class
  private
    cs : TRTLCriticalSection;
  public
    Query : TSQLQuery;
    constructor Create(aQuery : TSQLQuery);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  TContext = record
    Transaction : TSQLTransaction;
    Id : TThreadID;
  end;

  TSQLDBDataModule = class
  public
    Querys : array of TLockedQuery;
    Contexts : array of TContext;
  end;

var
  //Globally used Querylist to hold all Update/Insert Querys globally prepared avalible
  Data : TSQLDBDataModule;

implementation

{ TLockedQuery }

constructor TLockedQuery.Create(aQuery: TSQLQuery);
begin
  InitCriticalSection(cs);
  Query := aQuery;
end;

destructor TLockedQuery.Destroy;
begin
  DoneCriticalSection(cs);
  inherited Destroy;
end;

procedure TLockedQuery.Lock;
begin
  EnterCriticalSection(cs);
end;

procedure TLockedQuery.Unlock;
begin
  LeaveCriticalSection(cs);
end;

{ TSQLStreamer }

constructor TSQLStreamer.Create(Context: TThreadID; Obj: TPersistent);
begin
  FObject := Obj;
end;

procedure TSQLStreamer.Load(Cascadic: Boolean);
begin

end;

procedure TSQLStreamer.Save(Cascadic: Boolean);
begin

end;

initialization
  Data := TSQLDBDataModule.Create;
finalization
  FreeAndNil(Data);
end.

