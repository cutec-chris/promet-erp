{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  17.02.2001                                       *}
{*        Version         :  1.1                                              *}
{*        EMail           :  tapi@delphiclub.de                               *}
{******************************************************************************}
{*                                                                            *}
{*    This File is free software; You can redistribute it and/or modify it    *}
{*    under the term of GNU Library General Public License as published by    *}
{*    the Free Software Foundation. This File is distribute in the hope       *}
{*    it will be useful "as is", but WITHOUT ANY WARRANTY OF ANY KIND;        *}
{*    See the GNU Library Public Licence for more details.                    *}
{*                                                                            *}
{******************************************************************************}
{*                                                                            *}
{*    Diese Datei ist Freie-Software. Sie können sie weitervertreiben         *}
{*    und/oder verändern im Sinne der Bestimmungen der "GNU Library GPL"      *}
{*    der Free Software Foundation. Diese Datei wird,"wie sie ist",           *}
{*    zur Verfügung gestellt, ohne irgendeine GEWÄHRLEISTUNG                  *}
{*                                                                            *}
{******************************************************************************}
{*                          www.delphiclub.de                                 *}
{******************************************************************************}

unit TAPIProv;

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}
interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,tapi;

{$INCLUDE TAPI.INC}


type
  PTAPIProviderEntry =^TTAPIProviderEntry;
  TTAPIProviderEntry =Class(TPersistent)
  private
    FFileName:String;
    FPermanentID:DWord;
  protected
  public
    property FileName:String read FFileName ;
    property PermanentID:Dword read FPermanentID;
    constructor Create;virtual;
    destructor Destroy;override;
  end;

  TTAPIProviderList =class(TPersistent)
  private
    FProviderIndex:Integer;
    FProviderList:PLineProviderList;
    FAPIHighVersion:LongInt;
    function GetNumProviders: DWord;
    function GetAPIHighVersion: ShortString;
    procedure SetAPIHighVersion(const Value: ShortString);
    function GetValue(Index: Integer): String;
    procedure SetValue(Index: Integer; const Value: String);
    function GetProviderIndex: Integer;
    procedure SetProviderIndex(const Value: Integer);
  protected
    procedure Update;
  public
    function GetProviderFileName(Index:Integer):String;
    function GetProviderPermanentID(Index:Integer):DWord;
    property NumProviders:DWord read GetNumProviders;
    property APIVersion:ShortString read GetAPIHighVersion write SetAPIHighVersion ;
    property FileName[Index:Integer]:String read GetValue write SetValue ;default;
    constructor Create;virtual;
    destructor Destroy;override;
  published
     property ProviderIndex:Integer read GetProviderIndex write SetProviderIndex stored TRUE default 1;

  end;

  TProviderEvent=procedure(Sender:TObject;var ProviderFileName:String;ProviderID:DWord)of Object;

  TTAPIProvider = class(TComponent)
  private
    FAfterAdd:TProviderEvent;
    FProviderList:TTAPIProviderList;
    FAfterRemove: TProviderEvent;
    FBeforeAdd: TProviderEvent;
    FBeforeRemove: TProviderEvent;
    FOnChange: TProviderEvent;
    function GetNumProviders: Dword;
    procedure SetProviderFileName(const Value: String);
    procedure SetProviderID;
    function GetFileName: TTAPIProviderList;
    procedure SetFileName(const Value: TTAPIProviderList);
    procedure SetPermanentProviderID(const Value:DWord);
    function GetPermanentProviderID: DWord;
    function GetProviderFileName: String;
  protected
  public
    procedure Add(FileName:String);
    procedure Config;
    procedure Remove(FileName:String);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    property PermanentProviderID:DWord read GetPermanentProviderID write SetPermanentProviderID default 0;
    property FileName:String read GetProviderFileName write SetProviderFileName;
    property NumProviders:Dword read GetNumProviders ;
  published
    property AfterAdd:TProviderEvent read FAfterAdd write FAfterAdd;
    property BeforeAdd:TProviderEvent read FBeforeAdd write FBeforeAdd;
    property AfterRemove:TProviderEvent read FAfterRemove write FAfterRemove;
    property BeforeRemove:TProviderEvent read FBeforeRemove write FBeforeRemove;
    property OnChange:TProviderEvent read FOnChange write FOnChange;
    property ProviderName:TTAPIProviderList read GetFileName write SetFileName ;
   end;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr,TAPICurVer;

{$IFDEF TAPI30}
procedure Register;
begin
  RegisterComponents('TAPI30', [TTAPIProvider]);
end;
{$ELSE}
{$IFDEF TAPI22}
procedure Register;
begin
  RegisterComponents('TAPI22', [TTAPIProvider]);
end;
{$ELSE}
{$IFDEF TAPI21}
procedure Register;
begin
  RegisterComponents('TAPI21', [TTAPIProvider]);
end;
{$ELSE}
{$IFDEF TAPI20}
procedure Register;
begin
  RegisterComponents('TAPI20', [TTAPIProvider]);
end;
{$ELSE}
procedure Register;
begin
  RegisterComponents('TAPI', [TTAPIProvider]);
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ TTAPIProvider }

procedure TTAPIProvider.Add(FileName:String);
var R:Longint;
    ppid:DWord;
begin
  ppid:=PermanentProviderID;
  if Assigned(FBeforeAdd) then FBeforeAdd(self,FileName,ppid);
  R:=LineAddProvider(PChar(FileName),TForm(Owner).Handle,ppid);
  if (DWord(R)>DWORD($80000000))  then
  begin
    RaiseTAPILineError(R);
  end;
  FProviderList.Update;
  if Assigned(FAfterAdd) then FAfterAdd(self,FileName,ppid);
end;



procedure TTAPIProvider.Config;
var R:Longint;
    ppid:DWord;
begin
  ppid:=PermanentProviderId;
  R:=LineConfigProvider(TForm(Owner).Handle,ppid);
  if (DWord(R)>DWORD($80000000)) then
  begin
    RaiseTAPILineError(R);
  end;

end;

constructor TTAPIProvider.Create;
begin
  inherited Create(AOwner);
  if not Assigned(FProviderList) then FProviderList:=TTAPIProviderList.Create;
end;

destructor TTAPIProvider.Destroy;
begin
  FProviderList.Free;
  inherited Destroy;
end;




function TTAPIProvider.GetFileName: TTAPIProviderList;
begin
  if not Assigned(FProviderList) then
  begin
    FProviderList:=TTAPIProviderList.Create;
    FProviderList.Update;
  end;
  Result:=FProviderList;
end;

function TTAPIProvider.GetNumProviders: Dword;
begin
  with FProviderList do
  begin
    Update;
    result:=NumProviders;
  end;
end;

procedure TTAPIProvider.SetPermanentProviderID(const Value: DWord);
var i:integer;
begin
  IF PermanentProviderID <> Value then
  begin
    If Assigned(FProviderList)=False Then FProviderList:=TTAPIProviderList.Create;
    for i:=1 to FProviderList.NumProviders do
    begin
      if FProviderList.GetProviderPermanentID(i)=Value then
      begin
        FProviderList.ProviderIndex:=i;
        SetProviderID;
        Exit;
      end;
    end;
  end;
end;

procedure TTAPIProvider.Remove(FileName:String);
var R:Longint;
    ppid:DWord;
begin
  ppid:=PermanentProviderID;
  if Assigned(FBeforeRemove) then FBeforeRemove(self,FileName,ppid);
  SetProviderID;
  R:=lineRemoveProvider(ppid,TForm(Owner).Handle);
  if (DWord(R)>DWORD($80000000))  then
  begin
    RaiseTAPILineError(R);
  end;
  FProviderList.Update;
  if Assigned(FAfterRemove) then FAfterRemove(self,FileName,ppid);
end;

procedure TTAPIProvider.SetFileName(const Value: TTAPIProviderList);
begin
  if not Assigned(FProviderList) then FProviderList:=TTAPIProviderList.Create;
  FProviderList:=Value;
end;

procedure TTAPIProvider.SetProviderFileName(const Value: String);
var i:Integer;
begin
  if AnsiCompareText(FileName, Value) <> 0 then
  begin
    For i:=1 to FProviderList.NumProviders do
    begin
      IF AnsiCompareText(Value,FProviderList.GetProviderFileName(I)) = 0 Then
      begin
        FProviderList.ProviderIndex:=i;
        SetProviderID;
        exit;
      end;
    end;
  end;
end;

procedure TTAPIProvider.SetProviderID;
var i:Integer;
    ppid:DWord;
    fn:String;
begin
  If Assigned(FProviderList)=False Then FProviderList:=TTAPIProviderList.Create;
  PermanentProviderID:=0;
  For i:=1 to FProviderList.NumProviders do
  begin
    IF AnsiCompareText(FileName,FProviderList.GetProviderFileName(I)) = 0 Then
    begin
      PermanentProviderID:=FProviderList.GetProviderPermanentID(I);
      FProviderList.ProviderIndex:=i;
      exit;
    end;
  end;
  ppid:=PermanentProviderID;
  fn:=FileName;
  if Assigned(FOnChange) then FOnChange(self,fn,ppid);
end;

function TTAPIProvider.GetPermanentProviderID: DWord;
begin
  result:=FProviderList.GetProviderPermanentID(FProviderList.ProviderIndex);
end;

function TTAPIProvider.GetProviderFileName: String;
begin
  result:=FProviderList.GetProviderFileName(FProviderList.ProviderIndex);
end;

{ TTAPIProviderList }

constructor TTAPIProviderList.Create;
begin
  GetMem(FProviderList,(SizeOf(TLINEPROVIDERLIST)+1000));
  FProviderList^.dwTotalSize:=SizeOf(TLINEPROVIDERLIST)+1000;
  ProviderIndex:=1;
  Update;
end;

destructor TTAPIProviderList.Destroy;
begin
  FreeMem(FProviderList);
  inherited Destroy;
end;

function TTAPIProviderList.GetNumProviders: DWord;
begin
  result:=FProviderList^.dwNumProviders;
end;

function TTAPIProviderList.GetAPIHighVersion: ShortString;
begin
  result:='$'+IntToHex(FAPIHighVersion,8);
end;

procedure TTAPIProviderList.SetAPIHighVersion(const Value: ShortString);
  var dummy:LongInt;
begin
  dummy:=StrToIntDef(Value,$00010000);
  FAPIHighVersion:=dummy;
end;

procedure TTAPIProviderList.Update;
var R:LongInt;
    I:Integer;
    E:TTAPIProviderEntry;
    LPEntry:PLINEPROVIDERENTRY;
    Dummy:Array of byte;
    E_tag:DWORD;
begin
  E_tag:=0;
  FAPIHighVersion:=TAPI_CURRENT_VERSION;
  R:=lineGetProviderList(FAPIHighVersion,FProviderList);
  If R=0 then
  begin
    for I:=1 to NumProviders do
    begin
      E:=TTAPIProviderEntry.Create;
      LPEntry:=PLINEPROVIDERENTRY(PChar(FProviderList)+DWORD(FProviderList.dwProviderListOffset+E_tag));
      E.FPermanentID:=LPEntry^.dwPermanentProviderID;
      SetLength(Dummy,LPEntry^.dwProviderFilenameSize+1);
      StrCopy(PChar(Dummy),PChar(FProviderList)+LPEntry^.dwProviderFileNameOffset);
      E.FFileName:=PChar(Dummy);
      E_tag:=E_tag+SizeOf(TLINEPROVIDERENTRY) ;
      E.Free;
    end;
  end
  else
  begin
    RaiseTAPILineError(R)
  end;
end;

function TTAPIProviderList.GetProviderFileName(Index: Integer): String;
var FileNameOffset:DWord;
    Dummy:Array[0..255]of Char;
    Dummy1:Array[0..255]of Char;
begin
  FillChar(Dummy,254,#0);
  FillChar(Dummy1,254,#0);
  StrCopy(Dummy,PChar(FProviderList)+DWORD(FProviderList.dwProviderListOffset+DWORD((Index-1)*SizeOf(TLINEPROVIDERENTRY))+8));
  StrCopy(Dummy1,PChar(FProviderList)+DWORD(FProviderList.dwProviderListOffset+DWORD((Index-1)*SizeOf(TLINEPROVIDERENTRY))+9));
  FileNameOffset:=Byte(Dummy[0])+(256*(Byte(Dummy1[0])));
  StrCopy(Dummy,PChar(FProviderList)+DWORD(FilenameOffset));
  Result:=Dummy;
end;

function TTAPIProviderList.GetProviderPermanentID(Index: Integer): DWord;
Var Dummy:Array[0..255]of char;
begin
  StrCopy(Dummy,PChar(FProviderList)+DWORD(FProviderList.dwProviderListOffset+DWORD((Index-1)*SizeOf(TLINEPROVIDERENTRY))));
  Result:=Byte(Dummy[0])+(256*(Byte(Dummy[1])));
end;

function TTAPIProviderList.GetValue(Index: Integer): String;
begin
  result:=GetProviderFileName(Index);
end;

procedure TTAPIProviderList.SetValue(Index: Integer; const Value: String);
begin
  FProviderIndex:=Index;
end;

function TTAPIProviderList.GetProviderIndex: Integer;
begin
  result:=FProviderIndex;
end;

procedure TTAPIProviderList.SetProviderIndex(const Value: Integer);
begin
  if FProviderIndex <> Value then
  begin
    FProviderIndex:=Value;
  end;
end;

{TTAPIProviderEntry }

constructor TTAPIProviderEntry.Create;
begin
  inherited Create;
end;

destructor TTAPIProviderEntry.Destroy;
begin
  inherited Destroy;
end;

{$ENDIF}
{$ENDIF}
end.