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
unit TAPITrans;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses Windows,controls,classes,SysUtils,TAPI,TAPIErr;

{$INCLUDE TAPI.INC}

type
  TCardOptions=set of (coHidden,coPredefined);
  TLineLocationOption = (lloPulseDial);
  TLineLocationOptions = set of TLineLocationOption;

  PLocationEntry =^TLocationEntry;
  TLocationEntry =class
  private
    FPermanentLocationID:DWORD;
    FLocationName:string;
    FCountryCode:DWORD;
    FCityCode:String;
    FPreferredCardID:DWORD;
    FLocalAccessCode:String;
    FLongDistanceAccessCode:String;
    FTollPrefixList:TList;
    FCountryID:DWord;
    FOptions:TLineLocationOptions;
    FCancelCallWaiting:String;
  public
    constructor Create(var AEntry:PLINELOCATIONENTRY;ACaps:PLINETRANSLATECAPS );virtual;
    destructor Destroy;override;
    property PermanentLocationID:DWORD read FPermanentLocationID write FPermanentLocationID;
    property LocationName:string read FLocationName write FLocationName;
    property CountryCode:DWORD read FCountryCode write FCountryCode;
    property CityCode:String read FCityCode write FCityCode;
    property PreferredCardID:DWORD read FPreferredCardID write FPreferredCardID;
    property LocalAccessCode:String read FLocalAccessCode write FLocalAccessCode;
    property LongDistanceAccessCode:String read FLongDistanceAccessCode write FLongDistanceAccessCode;
    property TollPrefixList:TList read FTollPrefixList write FTollPrefixList;
    property CountryID:DWord read FCountryID write FCountryID;
    property Options:TLineLocationOptions read FOptions write FOptions;
    property CancelCallWaiting:String read FCancelCallWaiting write FCancelCallWaiting;
  end;

  PCardEntry = ^TCardEntry;
  TCardEntry =class
  private
    FPermanentCardID:DWord;
    FCardName:String;
    FCardNumberDigits:Dword;
    FSameAreaRule:String;
    FLongDistanceRule:String;
    FInternationalRule:String;
    FOptions:TCardOptions;
  public
    constructor Create(var AEntry:PLINECARDENTRY;ACaps:PLINETRANSLATECAPS );virtual;
    destructor Destroy;override;
    property PermanentCardID:DWord read FPermanentCardID write FPermanentCardID;
    property CardName:String read FCardName write FCardName;
    property CardNumberDigits:Dword read FCardNumberDigits write FCardNumberDigits;
    property SameAreaRule:String read FSameAreaRule write FSameAreaRule;
    property LongDistanceRule:String read FLongDistanceRule write FLongDistanceRule;
    property InternationalRule:String read FInternationalRule write FInternationalRule;
    property Options:TCardOptions read FOptions write FOptions;
  end;

  TLocationList=class(TPersistent)
  private
    FList:TList;
    function GetItems(Index: Integer): PLocationEntry;
  public
    constructor Create(ATranslateCaps:PLineTranslateCaps); virtual;
    destructor Destroy;override;
    property Items[Index: Integer]: PLocationEntry read GetItems;
    function Add(Item: PLocationEntry): Integer;
  end;

  TCardList=class(TPersistent)
  private
    FList:TList;
    function GetItems(Index: Integer): PCardEntry;
  public
    constructor Create(ATranslateCaps:PLineTranslateCaps); virtual;
    destructor Destroy;override;
    property Items[Index: Integer]: PCardEntry read GetItems;
    function Add(Items:PCardEntry):Integer;
  end;

  TTranslateCaps=class(TPersistent)
  private
    FCaps:PLINETRANSLATECAPS;
    FCurrentLocationID: DWord;
    FCurrentPreferredCardID: DWord;
    FNumCards: DWORD;
    FNumLocations: DWord;
    FLocationList:TLocationList;
    FCardList:TCardList;
  public
    constructor Create(ATranslateCaps:PLineTranslateCaps); virtual;
    destructor Destroy;override;
    property LocationList:TLocationList read FLocationList;
    property CardList:TCardList read FCardlist;
  published
    property NumLocations:DWord read FNumLocations;
    property CurrentLocationID:DWord read FCurrentLocationID;
    property NumCards:DWORD read FNumCards;
    property CurrentPreferredCardID:DWord read FCurrentPreferredCardID;
  end;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}


{ TTranslateCaps }

constructor TTranslateCaps.Create;
begin
  FCaps:=ATranslateCaps;
  FNumLocations:=FCaps.dwNumLocations;
  FCurrentLocationID:=FCaps.dwCurrentLocationID;
  FNumCards:=FCaps.dwNumCards;
  FCurrentPreferredCardID:=FCaps.dwCurrentPreferredCardID;
  FLocationList:=TLocationList.Create(ATranslateCaps);
  FCardList:=TCardList.Create(ATranslateCaps);
end;

destructor TTranslateCaps.Destroy;
begin
  inherited;
end;

{ TLocationEntry }

constructor TLocationEntry.Create;
var Dummy: array of Byte;
begin
  FPermanentLocationID:=AEntry.dwPermanentLocationID;
  SetLength(Dummy,AEntry^.dwLocationNameSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwLocationNameOffset);
  FLocationName:=PChar(Dummy);
  FCountryCode:=AEntry^.dwCountryCode;
  SetLength(Dummy,AEntry^.dwCityCodeSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwCityCodeOffset);
  FCityCode:=PChar(Dummy);
  FPreferredCardID:=AEntry^.dwPreferredCardID;
  SetLength(Dummy,AEntry^.dwLocalAccessCodeSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwLocalAccessCodeOffset);
  FLocalAccessCode:=PChar(Dummy);
  SetLength(Dummy,AEntry^.dwLongDistanceAccessCodeSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwLongDistanceAccessCodeOffset);
  FLongDistanceAccessCode:=PChar(Dummy);
  FTollPrefixList:=TList.Create;
  FCountryID:=AEntry^.dwCountryID;
  IF AEntry^.dwOptions=LINELOCATIONOPTION_PULSEDIAL then FOptions:=[lloPulseDial] else FOptions:=[];
  SetLength(Dummy,AEntry^.dwCancelCallWaitingSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwCancelCallWaitingOffset);
  FCancelCallWaiting:=PChar(Dummy);
end;

destructor TLocationEntry.Destroy;
begin
  FTollPrefixList.Destroy;
  inherited;
end;

{ TLocationList }

function TLocationList.Add(Item: PLocationEntry): Integer;
begin
  result:=FList.Add(Item);
end;

constructor TLocationList.Create(ATranslateCaps: PLineTranslateCaps);
var Dummy:Array of Byte;
    LocationEntry:PLINELOCATIONENTRY;
    Entry:TLocationEntry;
    i:Integer;
    NumL:DWord;
begin
  FList:=TList.Create;
  NumL:=0;
  for i:=0 to ATranslateCaps^.dwNumLocations-1 do
  begin
    SetLength(Dummy,ATranslateCaps^.dwLocationListSize+1);
    //StrCopy(PChar(Dummy),PCHAR(ATranslateCaps)+(ATranslateCaps^.dwLocationListOffset+NumL));
    //LocationEntry:=PLINELOCATIONENTRY(Dummy);
    LocationEntry:=PLINELOCATIONENTRY(PChar(ATranslateCaps)+(ATranslateCaps^.dwLocationListOffset+NumL));
    NumL:=NumL+SizeOF(TLINELOCATIONENTRY);
    Entry:=TLocationEntry.Create(LocationEntry,ATranslateCaps);
    Add(@Entry);
  end;
end;

destructor TLocationList.Destroy;
begin
  FList.Destroy;
  inherited;
end;

function TLocationList.GetItems(Index: Integer): PLocationEntry;
begin
  result:=FList.Items[Index];
end;

{ TCardList }

function TCardList.Add(Items: PCardEntry): Integer;
begin
  result:=FList.Add(Items);
end;

constructor TCardList.Create(ATranslateCaps: PLineTranslateCaps);
var Dummy:Array of Byte;
    CardEntry:PLINECARDENTRY;
    Entry:TCardEntry;
    i:Integer;
    NumL:DWord;
begin
  FList:=TList.Create;
  NumL:=0;
  for i:=0 to ATranslateCaps^.dwNumCards-1 do
  begin
    SetLength(Dummy,ATranslateCaps^.dwCardListSize+1);
    CardEntry:=PLINECARDENTRY(PChar(ATranslateCaps)+(ATranslateCaps^.dwCardListOffset+NumL));
    NumL:=NumL+SizeOF(TLINECARDENTRY);
    Entry:=TCardEntry.Create(CardEntry,ATranslateCaps);
    Add(@Entry);
  end;
end;

destructor TCardList.Destroy;
begin
  inherited;

end;

function TCardList.GetItems(Index: Integer): PCardEntry;
begin
  result:=FList.Items[Index];
end;

{ TCardEntry }

constructor TCardEntry.Create(var AEntry: PLINECARDENTRY;
  ACaps: PLINETRANSLATECAPS);
var Dummy:array of Byte;   
begin
  FPermanentCardID:=AEntry.dwPermanentCardID;
  SetLength(Dummy,AEntry^.dwCardNameSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwCardNameOffset);
  FCardName:=PChar(Dummy);
  FCardNumberDigits:=AEntry.dwCardNumberDigits;
  SetLength(Dummy,AEntry^.dwSameAreaRuleSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwSameAreaRuleOffset);
  FSameAreaRule:=PChar(Dummy);
  SetLength(Dummy,AEntry^.dwLongDistanceRuleSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwLongDistanceRuleOffset);
  FSameAreaRule:=PChar(Dummy);
  SetLength(Dummy,AEntry^.dwInternationalRuleSize+1);
  StrCopy(PChar(Dummy),PChar(ACaps)+AEntry^.dwInternationalRuleOffset);
  FSameAreaRule:=PChar(Dummy);
  FOptions:=[];
  IF AEntry^.dwOptions=LINECARDOPTION_HIDDEN Then FOptions:=FOptions+[coHidden];
  IF AEntry^.dwOptions=LINECARDOPTION_PREDEFINED Then FOptions:=FOptions+[coPredefined];
end;

destructor TCardEntry.Destroy;
begin
  inherited;

end;
{$ENDIF}
{$ENDIF}

end.

