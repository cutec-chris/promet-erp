{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
unit ubasedatasetinterfaces2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpjsonrtti, Contnrs;

type
  { TAbstractDBDataset2 }

  TAbstractDBDataset2 = class(TPersistent)
  private
    FChanged: Boolean;
    FConnection: TComponent;
    FId: Int64;
    FTableName: string;
    FTimestampd: TDateTime;
    function GetActive: Boolean;
    function GetCount: Integer;
    function GetFullCount: Integer;
    procedure SetActive(AValue: Boolean);
  protected
  public
    procedure Open;virtual;
    procedure Close;virtual;
    procedure Change;virtual;
    procedure UnChange;virtual;
    property Count : Integer read GetCount;
    property FullCount : Integer read GetFullCount;
    property Connection : TComponent read FConnection;
    class function GetRealTableName : string;virtual;
    property TableName : string read FTableName write FTableName;
    procedure CascadicPost;virtual;
    procedure CascadicCancel;virtual;
    function Delete : Boolean;virtual;
    procedure Insert;virtual;
    procedure Append;virtual;
    procedure First;virtual;
    procedure Last;virtual;
    procedure Next;virtual;
    procedure Prior;virtual;
    procedure Post;virtual;
    procedure Edit;virtual;
    procedure Cancel;virtual;
    class procedure DefineFields(aDataSet: TDataSet); virtual;
    function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean; virtual;
    function EOF : Boolean;virtual;
    function FieldByName(const aFieldName : string) : db.TField;virtual;
    property Changed : Boolean read FChanged;
    property Active : Boolean read GetActive write SetActive;
  published
    property SQL_ID : Int64 read FId write FId;
    property TIMESTAMPD : TDateTime read FTimestampd write FTimestampd;
  end;

  TAbstractMasterDetail = class(TObjectList)
  public
    class function GetObjectTyp : TClass;virtual;abstract;
  end;

implementation

{ TAbstractDBDataset2 }

function TAbstractDBDataset2.GetActive: Boolean;
begin
  Result := False;
end;

function TAbstractDBDataset2.GetCount: Integer;
begin
  Result := 0;
end;

function TAbstractDBDataset2.GetFullCount: Integer;
begin
  Result := 0;
end;

procedure TAbstractDBDataset2.SetActive(AValue: Boolean);
begin
end;

procedure TAbstractDBDataset2.Open;
begin
end;

procedure TAbstractDBDataset2.Close;
begin
end;

procedure TAbstractDBDataset2.Change;
begin
  FChanged:=True;
end;

procedure TAbstractDBDataset2.UnChange;
begin
  FChanged:=False;
end;

class function TAbstractDBDataset2.GetRealTableName: string;
begin
  Result := Self.ClassName;
  if copy(Result,0,1)='T' then
    Result := copy(Result,2,length(Result));
end;

procedure TAbstractDBDataset2.CascadicPost;
begin

end;

procedure TAbstractDBDataset2.CascadicCancel;
begin

end;

function TAbstractDBDataset2.Delete: Boolean;
begin

end;

procedure TAbstractDBDataset2.Insert;
begin

end;

procedure TAbstractDBDataset2.Append;
begin

end;

procedure TAbstractDBDataset2.First;
begin

end;

procedure TAbstractDBDataset2.Last;
begin

end;

procedure TAbstractDBDataset2.Next;
begin

end;

procedure TAbstractDBDataset2.Prior;
begin

end;

procedure TAbstractDBDataset2.Post;
begin

end;

procedure TAbstractDBDataset2.Edit;
begin

end;

procedure TAbstractDBDataset2.Cancel;
begin

end;

class procedure TAbstractDBDataset2.DefineFields(aDataSet: TDataSet);
begin
end;

function TAbstractDBDataset2.Locate(const keyfields: string;
  const keyvalues: Variant; options: TLocateOptions): boolean;
begin

end;

function TAbstractDBDataset2.EOF: Boolean;
begin

end;

function TAbstractDBDataset2.FieldByName(const aFieldName: string): db.TField;
begin

end;

end.

