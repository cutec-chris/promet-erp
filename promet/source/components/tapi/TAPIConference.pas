{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  15.03.2001                                       *}
{*        Version         :  0.01                                              *}
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
unit TAPIConference;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI,TAPISystem,TAPICall,TAPIHelpFunc,TAPILines,TAPIServices;

{$INCLUDE TAPI.INC}

type
  TConfEventReply=procedure(Sender:TObject;AsyncFunc:TAsyncFunc;Error:Dword)of Object;


  TTAPIConference = class(TTAPIComponent)
  private
    FConfCall:TTAPICall;
    FConsultCall:TTAPICall;
    FService:TTAPILineService;
    FCallParams: TCallParams;
    FInitCall: TTAPICall;
    FInitLine: TTAPILine;
    FNumParties: DWord;
    FOnReply: TConfEventReply;
    //procedure GetCallParams(ACallParams: PLineCallParams);
    procedure SetInitCall(const Value: TTAPICall);
    procedure SetInitLine(const Value: TTAPILine);
  protected
    procedure Notification(AComponent:TComponent; Operation :TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add;
    procedure PrepareAdd;
    procedure Setup;
    procedure SwapHold;
    procedure Reply(AsyncFunc:TAsyncFunc;Error:DWord);
    procedure PerformMsg(Msg: TCMTAPI);override;
  published
    property NumParties:DWord read FNumParties write FNumParties default 0;
    property InitCall:TTAPICall read FInitCall write SetInitCall;
    property InitLine:TTAPILine read FInitLine write SetInitLine;
    property ConfCall:TTAPICall read FConfCall write FConfCall;
    property ConsultCall:TTAPICall read FConsultCall write FConsultCall;
    property Service:TTAPILineService read FService write FService;
    property CallParams:TCallParams read FCallParams write FCallParams;
    property OnReply:TConfEventReply read FOnReply write FOnReply;
  end;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPIConference]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPIConference]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPIConference]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPIConference]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPIConference]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{ TTAPIConference }

procedure TTAPIConference.Add;
var R:Longint;
begin
  R:=lineAddToConference(FConfCall.Handle, FConsultCall.Handle);
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end
  else
  begin
    AppTAPIMgr.AsyncList.Add(afAddToConference,R,self);
  end;
end;


constructor TTAPIConference.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TTAPIConference.Destroy;
begin
  inherited;
end;

procedure TTAPIConference.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
   inherited Notification(AComponent,Operation);
  if Operation=opRemove then
  begin
    if AComponent=FConfCall then
      FConfCall:=nil;
    if AComponent=FConsultCall then
      FConsultCall:=nil;
    if AComponent=FInitCall then
      FInitCall:=nil;
    if AComponent=FService then
      FService:=nil;
    if AComponent=FCallParams then
      FCallParams:=nil;
    if AComponent=FInitLine then
      FInitLine:=nil;
  end;
end;

procedure TTAPIConference.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if dwMsg=LINE_REPLY then Reply(Msg.AsyncFunc^,dwParam2);
  end;
end;

procedure TTAPIConference.PrepareAdd;
var R:longint;
    ALineCallParams: PLineCallParams;
    ConsultHandle:hCall;
begin
  try
    ConsultHandle:=0;
    If Assigned(FCallParams) then
    begin
      GetMem(ALineCallParams,SizeOf(TLineCallParams)+1000);
      FCallParams.GetParamStruct(ALineCallParams)
    end
    else  ALineCallParams:=nil;
    R:=linePrepareAddToConference(FConfCall.Handle,@ConsultHandle,ALineCallParams);
    if R <-1 then
      RaiseTAPILineError(R)
    else
    begin
      AppTAPIMgr.AsyncList.Add(afPrepareAddToConference,R,self);
      FConsultCall.Handle:=ConsultHandle;
    end;
  finally
    If Assigned(ALineCallParams) then FreeMem(ALineCallParams);
  end;
end;



procedure TTAPIConference.Reply(AsyncFunc: TAsyncFunc; Error: DWord);
begin
  if Assigned(FOnReply) then FOnReply(self,AsyncFunc,Error);
end;

procedure TTAPIConference.SetInitCall(const Value: TTAPICall);
begin
  If Value <> FInitCall then
  begin
    FInitCall := Value;
    FInitLine :=nil;
  end;
end;

procedure TTAPIConference.SetInitLine(const Value: TTAPILine);
begin
  If Value <> FInitLine then
  begin
    FInitLine := Value;
    FInitCall :=nil;
  end;
end;

procedure TTAPIConference.Setup;
var R:longint;
    InitCallHandle:hCall;
    InitLineHandle:hLine;
    ConfCallHandle,ConsultCallHandle:hCall;
    Params:PLineCallParams;
begin
  InitCallHandle:=0;
  InitLineHandle:=0;
  ConfCallHandle:=FConfCall.Handle;
  ConsultCallHandle:=FConsultCall.Handle;
  if Assigned(FInitCall) then InitCallHandle:=FInitCall.Handle else InitCall:=nil;
  if Assigned(FInitLine) then InitLineHandle:=FInitLine.Handle else InitLine:=nil;
  GetMem(Params,SizeOf(TLineCallParams));
  try
    if Assigned(FCallParams) then FCallParams.GetParamStruct(Params) else Params:=nil;
    R:=LineSetupConference(InitCallHandle,InitLineHandle,@ConfCallHandle,@ConsultCallHandle,FNumParties,Params);
    if R <-1 then
    begin
      RaiseTAPILineError(R);
    end
    else
    begin
      AppTAPIMgr.AsyncList.Add(afSetupConference,R,self);
      FConfCall.Handle:=ConfCallHandle;
      FConsultCall.Handle:=ConsultCallHandle;
    end;
  finally
    FreeMem(Params);
  end;
end;

procedure TTAPIConference.SwapHold;
var R:LongInt;
begin
  R:=LineSwapHold(FConfCall.Handle,FConsultCall.Handle);
  If R < -1 then RaiseTAPILineError(R)
  else
    AppTAPIMgr.AsyncList.Add(afSwapHold,R,self);
end;

{$ENDIF}
{$ENDIF}
end.