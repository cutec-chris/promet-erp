unit unntpdaemonmapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type
  TNNTPDaemonMapper = class(TDaemonMapper)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  NNTPDaemonMapper: TNNTPDaemonMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TNNTPDaemonMapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;
end.

