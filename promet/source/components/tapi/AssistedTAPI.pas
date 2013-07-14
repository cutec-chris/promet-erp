{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 3 /4 / 5  /6 / 7                          *}
{*        System          :  Windows 98 /NT / 2000                            *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  19.08.2001                                       *}
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


unit AssistedTAPI;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,TAPI;

type
  TAssistedTAPIEvent=procedure(Sender :TObject) of Object;

type
  TAssistedTAPI = class(TComponent)
  private
    FComment: String;
    FCityCode: PChar;
    FDestAddress: String;
    FAppName: String;
    FCountryCode: PChar;
    FCalledParty: String;
    FOnRequestFailed :TAssistedTAPIEvent;
    FOnInvalDestAddress:TAssistedTAPIEvent;
    FOnRequestQueueFull:TAssistedTAPIEvent;
    FOnNoRequestRecipient:TAssistedTAPIEvent;
    procedure SetAppName(const Value: String);
    procedure SetCalledParty(const Value: String);
    procedure SetComment(const Value: String);
    procedure SetDestAddress(const Value: String);
    function GetCityCode: String;
    function GetCountryCode: String;
    function GetAppName: String;
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    procedure MakeCall;
    property CountryCode:String read GetCountryCode;
    property CityCode:String read GetCityCode;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    { Published-Deklarationen }
    property DestAddress:String read FDestAddress write SetDestAddress;
    property AppName:String read GetAppName write SetAppName nodefault;
    property CalledParty:String read FCalledParty write SetCalledParty;
    property Comment:String read FComment write SetComment;
    property OnRequestFailed:TAssistedTAPIEvent read FOnRequestFailed write FOnRequestFailed;
    property OnInvalDestAddress:TAssistedTAPIEvent read FOnInvalDestAddress write FOnInvalDestAddress;
    property OnRequestQueueFull:TAssistedTAPIEvent read FOnRequestQueueFull write FOnRequestQueueFull;
    property OnNoRequestRecipient:TAssistedTAPIEvent  read FOnNoRequestRecipient write FOnNoRequestRecipient;
  end;


procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

procedure Register;
begin
  RegisterComponents('TAPI', [TAssistedTAPI]);
end;

{ TAssistedTAPI }

constructor TAssistedTAPI.Create(AOwner: TComponent);
begin
  inherited;
  if FAppName='' then FAppName:=TForm(Owner).Caption;
  GetMem(FCountryCode,8);
  GetMem(FCityCode,8);
  if TAPIERR_REQUESTFAILED=tapiGetLocationInfo(FCountryCode,FCityCode)then
  begin
    if Assigned(FOnRequestFailed) then FOnRequestFailed(self);
  end;
end;

destructor TAssistedTAPI.Destroy;
begin
  FreeMem(FCountryCode,8);
  FreeMem(FCityCode,8);
  inherited;
end;

function TAssistedTAPI.GetAppName: String;
begin
  if csDesigning in ComponentState then
  begin
    if FAppName='' then FAppName:=TForm(Owner).Caption;
  end;
  Result:=FAppName;
end;

function TAssistedTAPI.GetCityCode: String;
begin
  Result:=FCityCode;
end;

function TAssistedTAPI.GetCountryCode: String;
begin
  Result:=FCountryCode;
end;

procedure TAssistedTAPI.MakeCall;
var R:LongInt;
begin
  R:=tapiRequestMakeCall(PChar(@FDestAddress),PChar(@FAppName),PChar(@FCalledParty),PChar(@FComment));
  case R of
    TAPIERR_INVALDESTADDRESS  : if Assigned(FOnInvalDestAddress) then FOnInvalDestAddress(self);
    TAPIERR_REQUESTQUEUEFULL  : if Assigned(FOnRequestQueueFull) then FOnRequestQueueFull(self);
    TAPIERR_NOREQUESTRECIPIENT: if Assigned(FOnNoRequestRecipient) then FOnNoRequestRecipient(self);
  end;
end;

procedure TAssistedTAPI.SetAppName(const Value: String);
begin
  FAppName := Value;
end;

procedure TAssistedTAPI.SetCalledParty(const Value: String);
begin
  FCalledParty := Value;
end;

procedure TAssistedTAPI.SetComment(const Value: String);
begin
  FComment := Value;
end;

procedure TAssistedTAPI.SetDestAddress(const Value: String);
begin
  FDestAddress := Value;
end;
{$ENDIF}
{$ENDIF}

end.