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

  { TFieldEmulation
      Get Access to Object Property with RTTI
  }

  TFieldEmulation = class(TField)
  protected
    constructor Create(AOwner: TPersistent;Propname : string);
  end;

  { TAbstractDBDataset2 }

  TAbstractDBDataset2 = class(TInterfacedPersistent)
  private
    FChanged: Boolean;
    FConnection: TComponent;
    FId: Int64;
    FParent: TPersistent;
    FTableName: string;
    FTimestampd: TDateTime;
    function GetActive: Boolean;
    function GetCount: Integer;
    function GetFullCount: Integer;
    procedure SetActive(AValue: Boolean);
  protected
  public
    constructor Create(aParent : TPersistent);virtual;
    property Parent : TPersistent read FParent;
    class function MapField(aField : string) : string;virtual;
    procedure FillDefaults;virtual;
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
    procedure Post;virtual;
    procedure Edit;virtual;
    procedure Cancel;virtual;
    function FieldByName(const aFieldName : string) : TFieldEmulation;virtual;
    property Changed : Boolean read FChanged;
    property Active : Boolean read GetActive write SetActive;
  published
    property SQL_ID : Int64 read FId write FId;
    property TIMESTAMPD : TDateTime read FTimestampd write FTimestampd;
  end;
  TAbstractDBDataset2Class = class of TAbstractDBDataset2;
  { TAbstractMasterDetail }

  TAbstractMasterDetail = class(TObjectList)
  private
    FFilter: string;
    FParent: TPersistent;
    FIndex : Integer;
    function GetDataset: TAbstractDBDataset2;
    procedure SetFilter(AValue: string);
  public
    constructor Create(aParent : TPersistent);
    property Parent : TPersistent read FParent;
    class function GetObjectTyp : TClass;virtual;abstract;
    property Dataset : TAbstractDBDataset2 read GetDataset;
    property Filter : string read FFilter write SetFilter;
    function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean; virtual;
    function FieldByName(const aFieldName : string) : TFieldEmulation;virtual;
    function Delete : Boolean;virtual;
    procedure Insert;virtual;
    procedure Append;virtual;
    procedure First;virtual;
    procedure Last;virtual;
    procedure Next;virtual;
    procedure Prior;virtual;
    function EOF : Boolean;virtual;
  end;

implementation

uses uData;

{ TFieldEmulation }

constructor TFieldEmulation.Create(AOwner: TPersistent; Propname: string);
begin
  inherited Create(nil);
end;

{ TAbstractMasterDetail }
function TAbstractMasterDetail.GetDataset: TAbstractDBDataset2;
begin
  Result := TAbstractDBDataset2(Items[FIndex]);
end;

procedure TAbstractMasterDetail.SetFilter(AValue: string);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

constructor TAbstractMasterDetail.Create(aParent: TPersistent);
begin
  FParent := aParent;
  Inherited Create;
end;
function TAbstractMasterDetail.Locate(const keyfields: string;
  const keyvalues: Variant; options: TLocateOptions): boolean;
begin
  Result := False;
end;

function TAbstractMasterDetail.FieldByName(const aFieldName: string
  ): TFieldEmulation;
begin
  Result := Dataset.FieldByName(aFieldName);
end;

function TAbstractMasterDetail.Delete: Boolean;
begin
  Result := False;
end;
procedure TAbstractMasterDetail.Insert;
begin
end;
procedure TAbstractMasterDetail.Append;
begin
end;
procedure TAbstractMasterDetail.First;
begin
  FIndex := 0;
end;
procedure TAbstractMasterDetail.Last;
begin
  FIndex := Count-1;
end;
procedure TAbstractMasterDetail.Next;
begin
  if FIndex < Count-1 then
    inc(FIndex);
end;
procedure TAbstractMasterDetail.Prior;
begin
  if FIndex > 0 then
    dec(FIndex);
end;

function TAbstractMasterDetail.EOF: Boolean;
begin
  Result := FIndex = Count-1;
end;

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

constructor TAbstractDBDataset2.Create(aParent: TPersistent);
begin
  inherited Create;
  FParent := aParent;
end;

class function TAbstractDBDataset2.MapField(aField: string): string;
begin
  Result := aField;
end;

procedure TAbstractDBDataset2.FillDefaults;
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
procedure TAbstractDBDataset2.Post;
begin
end;
procedure TAbstractDBDataset2.Edit;
begin
end;
procedure TAbstractDBDataset2.Cancel;
begin
end;
function TAbstractDBDataset2.FieldByName(const aFieldName: string
  ): TFieldEmulation;
begin
  Result := TFieldEmulation.Create(Self,aFieldName);
end;

end.

