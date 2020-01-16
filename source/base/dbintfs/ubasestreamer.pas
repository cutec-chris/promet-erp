unit ubasestreamer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBaseStreamer = class(TComponent)
  public
    procedure Load(Cascatic : Boolean = True);virtual;abstract;
    procedure Save(Cascatic : Boolean = True);virtual;abstract;
  end;

implementation

end.

