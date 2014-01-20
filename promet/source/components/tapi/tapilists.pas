{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  17.02.2001                                       *}
{*        Version         :  1.0                                              *}
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
unit TAPILists;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses Windows,Classes,TAPI;

{$INCLUDE TAPI.INC}

type PLineDeviceCapsItem=^TLineDeviceCapsItem;
     TLineDeviceCapsItem=class(TObject)
       ID:LongWord;
       APIVer:LongWord;
       ExtVer:LongWord;
       Caps:PLineDevCaps;
       //ACaps:Array of TAddressCapsItem;
     end;
type PPhoneCapsItem=^TPhoneCapsItem;
     TPhoneCapsItem=class(TObject)
       ID:LongWord;
       APIVer:LongWord;
       ExtVer:LongWord;
       Caps:PPhoneCaps;
     end;

type TTAPIAddressCapsList=class(TPersistent)
     private
       FList:TList;
     protected
     public
       constructor Create;
       destructor Destroy;override;
     end;

type
  TTAPIDeviceCapsList=class
  private
    FList:TList;
    FAddressCapsList:TTAPIAddressCapsList;
    function GetItems(Index: Integer): TLineDeviceCapsItem;
    procedure SetItems(Index: Integer; const Value: TLineDeviceCapsItem);
  protected
  public
    constructor Create;
    destructor Destroy;override;
    function Add(AItem:TLineDeviceCapsItem):Integer;
    property Items[Index: Integer]: TLineDeviceCapsItem read GetItems write SetItems;
  end;

type
 TTAPIPhoneCapsList=class
  private
    FList:TList;
    function GetItems(Index: Integer): TPhoneCapsItem;
    procedure SetItems(Index: Integer; const Value: TPhoneCapsItem);
  protected
  public
    constructor Create;
    destructor Destroy;override;
    function Add(AItem:TPhoneCapsItem):Integer;
    property Items[Index: Integer]: TPhoneCapsItem read GetItems write SetItems;
  end;

 {$ENDIF}
 {$ENDIF}

 implementation

 {$IFDEF WINDOWS}
 {$IFDEF CPU32}

{ TTAPIDeviceCapsList }

function TTAPIDeviceCapsList.Add(AItem: TLineDeviceCapsItem): Integer;
begin
  Result:=FList.Add(AItem);
end;

constructor TTAPIDeviceCapsList.Create;
begin
  inherited;
  FList:=TList.Create;
  FList.Capacity:=50;
  FAddressCapsList:=TTAPIAddressCapsList.Create;
end;

destructor TTAPIDeviceCapsList.Destroy;
begin
  FAddressCapsList.Free;
  FList.Free;
  inherited;
end;

function TTAPIDeviceCapsList.GetItems(Index: Integer): TLineDeviceCapsItem;
begin
  result:=TLineDeviceCapsItem(FList.Items[Index]);
end;

procedure TTAPIDeviceCapsList.SetItems(Index: Integer;
  const Value: TLineDeviceCapsItem);
begin
  FList.Items[Index]:=Value;
end;

{ TTAPIAddressCapsList }

constructor TTAPIAddressCapsList.Create;
begin
  inherited;
  FList:=TList.Create;
  FList.Capacity:=50;
end;

destructor TTAPIAddressCapsList.Destroy;
begin
  FList.Free;
  inherited;
end;

{ TTAPIPhoneCapsList }

function TTAPIPhoneCapsList.Add(AItem: TPhoneCapsItem): Integer;
begin
  Result:=FList.Add(AItem);
end;

constructor TTAPIPhoneCapsList.Create;
begin
  inherited;
  FList:=TList.Create;
  FList.Capacity:=50;
end;

destructor TTAPIPhoneCapsList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTAPIPhoneCapsList.GetItems(Index: Integer): TPhoneCapsItem;
begin
  result:=TPhoneCapsItem(FList.Items[Index]);
end;

procedure TTAPIPhoneCapsList.SetItems(Index: Integer;
  const Value: TPhoneCapsItem);
begin
  FList.Items[Index]:=@Value;
end;

{$ENDIF}
{$ENDIF}
end.