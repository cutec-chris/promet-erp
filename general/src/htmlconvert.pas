unit htmlconvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FastHTMLParser, FileUtil, LConvEncoding;

type

  { DummyObj }

  TDummyObj = class(TObject)
  private
    Text : string;
    procedure TextFound(aText: string);
  end;

function
HTMLtoTXT(html: string) : string;

implementation

function HTMLtoTXT(html: string) : string;
var
  Parser: THTMLParser;
  Obj: TDummyObj;
begin
  Obj := TDummyObj.Create;
  Parser := THTMLParser.Create(html);
  Parser.OnFoundText:=@Obj.TextFound;
  Parser.Exec;
  Result := ConvertEncoding(Obj.Text,GuessEncoding(Obj.Text),'UTF-8');
  Parser.Free;
  Obj.Free;
end;


{ DummyObj }

procedure TDummyObj.TextFound(aText: string);
begin
  if copy(aText,0,6) <> 'html {' then
    Text := Text+lineending+aText;
end;

end.

