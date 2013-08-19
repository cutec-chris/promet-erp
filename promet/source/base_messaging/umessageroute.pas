unit uMessageRoute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, DBGrids, memds, db,
  uprometframesinplace,synautil;

type

  { TfMessageRoute }

  TfMessageRoute = class(TPrometInplaceFrame)
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    MemDataset1: TMemDataset;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation
uses uMessageEdit;
{$R *.lfm}
resourcestring
  strMessageroute                           = 'Nachrichtenroute';
{ TfMessageRoute }

constructor TfMessageRoute.Create(AOwner: TComponent);
var
  sl: TStringList;
  atmp: String;
  i: Integer;
begin
  inherited Create(AOwner);
  TabCaption:=strMessageRoute;
  sl := TStringList.Create;
  sl.Text := TfMessageEdit(AOwner).DataSet.Content.FieldByName('HEADER').AsString;
  for i := 0 to sl.Count-1 do
    begin
      atmp := sl[i];
      if copy(atmp,0,9) = 'Received:' then
        begin
          MemDataset1.Append;
          atmp := trim(copy(atmp,10,length(atmp)));
          if copy(atmp,0,4)='from' then
            atmp := trim(copy(atmp,5,length(atmp)));
          MemDataset1.FieldByName('From').AsString:=copy(atmp,0,pos('by ',atmp)-1);
          atmp := trim(copy(atmp,pos('by ',atmp)+2,length(atmp)));
          MemDataset1.FieldByName('By').AsString:=copy(atmp,0,pos('for ',atmp)-1);
          if pos('with ',lowercase(MemDataset1.FieldByName('By').AsString))>0 then
            begin
              MemDataset1.FieldByName('Connection').AsString:=copy(MemDataset1.FieldByName('By').AsString,pos('with ',lowercase(MemDataset1.FieldByName('By').AsString))+5,length(MemDataset1.FieldByName('By').AsString));
              MemDataset1.FieldByName('By').AsString:=copy(MemDataset1.FieldByName('By').AsString,0,pos('with ',lowercase(MemDataset1.FieldByName('By').AsString))-1);
            end;
          atmp := trim(copy(atmp,pos('for ',atmp)+3,length(atmp)));
          MemDataset1.FieldByName('For').AsString:=copy(atmp,0,pos(';',atmp)-1);
          if pos('with ',lowercase(MemDataset1.FieldByName('For').AsString))>0 then
            begin
              MemDataset1.FieldByName('Connection').AsString:=copy(MemDataset1.FieldByName('For').AsString,pos('with ',lowercase(MemDataset1.FieldByName('For').AsString))+5,length(MemDataset1.FieldByName('For').AsString));
              MemDataset1.FieldByName('For').AsString:=copy(MemDataset1.FieldByName('For').AsString,0,pos('with ',lowercase(MemDataset1.FieldByName('For').AsString))-1);
            end;
          atmp := trim(copy(atmp,pos(';',atmp)+1,length(atmp)));
          MemDataset1.FieldByName('Time').AsDateTime := DecodeRfcDateTime(atmp);
          MemDataset1.Post;
        end;
    end;
  sl.Free;
end;

end.

