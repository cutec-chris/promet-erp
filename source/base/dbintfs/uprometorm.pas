unit uPrometORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SQLDB, MSSQLConn, SQLite3Conn;

type

  { TSQLStreamer }

  TSQLStreamer = class(TComponent)
  private
    FTransaction: TComponent;
  public
    //we try to use this class with the same transaction/connection the whole time
    //so its added during Constructor when using it in another thread, it should be used with another Transaction
    constructor Create(Transaction : TComponent);
    //generates SQL to Fill all Published properties and Gerneric TFPGList Types
    //(generates recursive joined Query for default TFPGList Type (or if only one is avalible) and separate Querys for all other)
    procedure Load;
    //Generates recursive an update Statement per record if SQL_ID is filled or n insert stetement if not
    procedure Save;
  end;

implementation

{ TSQLStreamer }

constructor TSQLStreamer.Create(Transaction: TComponent);
begin
  FTransaction := Transaction;
end;

procedure TSQLStreamer.Load;
begin

end;

procedure TSQLStreamer.Save;
begin

end;

end.

