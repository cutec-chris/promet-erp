unit ubasestreamer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBaseStreamer }

  TBaseStreamer = class(TComponent)
  private
    FObject : TPersistent;
  public
    procedure Load(Cascatic : Boolean = True);virtual;abstract;
    procedure Save(Cascatic : Boolean = True);virtual;abstract;
    property Obj : TPersistent read FObject write FObject;
    function Select(aFilter : string) : Integer;overload;virtual;abstract;
    function Select(aId : Int64) : Boolean;overload;virtual;
  end;

implementation

{ TBaseStreamer }

function TBaseStreamer.Select(aId: Int64): Boolean;
begin
  Result := Select('SQL_ID='+IntToStr(aId)) = 1;
end;

end.

