{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5  /6                                 *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  11.09.2002                                       *}
{*        Version         :  0.3                                              *}
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
unit TAPIForward;
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$INCLUDE TAPI.INC}

uses Windows, Classes, TAPI, TAPISystem, TAPILines, TAPIAddress, TAPICall ,TAPIHelpFunc;

type
  //TForwardEventReply=procedure(Sender:TObject;AsyncFunc:TAsyncFunc;Error:Dword)of Object;

  TForwardListItem = class(TCollectionItem)
  private
    FDestCountryCode: DWord;
    FCallerAddress: String;
    FDestAddress: String;
    FForwardMode: TLineForwardMode;
  protected
  public
    constructor Create(Collection:TCollection);override;
    destructor Destroy;override;
  published
    property Mode:TLineForwardMode read FForwardMode write FForwardMode;
    property CallerAddress:String read FCallerAddress write FCallerAddress;
    property DestCountryCode:DWord read FDestCountryCode write FDestCountryCode;
    property DestAddress:String read FDestAddress write FDestAddress;
  end;

  TForwardList=class(TCollection)
  private
    FListStruct:PLineForwardList;
  protected
    function GetListStruct:PLineForwardList;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy;override;
    function Add:TForwardListItem;
    function GetNamePath:  string;override ;
  end;


  TTAPIForward = class(TTAPIComponent)
  private
    FActive:Boolean;
    FLine:TTAPILine;
    FAddress:TTAPIAddress;
    FAllAddresses:Boolean;
    FNumRingsNoAnswer:DWord;
    FConsultCall:TTAPICall;
    FForwardList: TForwardList;
    FOnReply: TCallEventReply;
    FCallParams: TCallParams;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    function MakeForward(Value:Boolean):Boolean;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure Reply(AsyncFunc: TAsyncFunc; Error: DWord);
    property Active:Boolean read GetActive write SetActive default False;
    procedure PerformMsg(Msg: TCMTAPI);override;
  published
    property OnReply:TCallEventReply read FOnReply write FOnReply;
    property AllAddresses:Boolean read FAllAddresses write FAllAddresses default False;
    property Line:TTAPILine read FLine write FLine;
    property NumRingsNoAnswer:DWord read FNumRingsNoAnswer write FNumRingsNoAnswer;
    property Address:TTAPIAddress read FAddress write FAddress;
    property ConsultCall:TTAPICall read FConsultCall write FConsultCall;
    property ForwardList:TForwardList read FForwardList write FForwardList;
    property CallParams :TCallParams read FCallParams write FCallParams;
  end;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr, SysUtils ;


procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPIForward]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPIForward]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPIForward]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPIForward]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPIForward]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;


{ TTAPIForward }

constructor TTAPIForward.Create(AOwner: TComponent);
begin
  inherited;
  FAllAddresses:=False;
  FActive:=False;
  FForwardList:=TForwardList.Create(TForwardListItem);
end;


destructor TTAPIForward.Destroy;
begin
  SetActive(FALSE);
  FForwardList.Free;
  inherited;
end;

function TTAPIForward.GetActive: Boolean;
begin
  Result:=FActive;
end;

function TTAPIForward.MakeForward(Value: Boolean): Boolean;
var R:LongInt;
    ConsCall:PHCall;
    LPCallParams:PLineCallParams;
    AID:DWORD;

begin
  Result:=False;
  LPCallParams:=nil;
  ConsCall:=@FConsultCall.Handle;
  try
    if Assigned(FAddress)=False then
      AID:=DWord(-1)
    else
      AID:=FAddress.ID;
    if Assigned(FCallParams) then CallParams.GetParamStruct(LPCallParams);
    If Value=True then
    begin
       R:=LineForward(FLine.Handle,DWord(FAllAddresses),AID,FForwardList.GetListStruct,FNumRingsNoAnswer,ConsCall,LPCallParams);
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Forward AsynID='+IntToStr(R)));
      {$ENDIF}
      if DWord(R)>DWord($80000000) then
      begin
        RaiseTAPILineError(R);
      end
      else
      begin
        Result:=True;
        AppTAPIMgr.AsyncList.Add(afForward,R,self);
        If Assigned(ConsCall)=False then FConsultCall.Handle:=0; 
      end;
    end
    else
    begin
      //ConsCall:=PHCall(FConsultCall.Handle);
      if FLine.Handle >0 then
      begin
        R:=LineForward(FLine.Handle,DWord(FAllAddresses),AID,nil,FNumRingsNoAnswer,ConsCall,LPCallParams);
        {$IFDEF DEBUG}
        OutputDebugString(PChar('Forward AsynID='+IntToStr(R)));
        {$ENDIF}
        if DWord(R)>DWord($80000000) then
        begin
          RaiseTAPILineError(R);
        end
        else
        begin
          AppTAPIMgr.AsyncList.Add(afForward,R,self);
          {if Assigned(FConsultCall) And Assigned(ConsCall) then
          begin
            if ConsCall^ = 0 then
            FConsultCall.DeallocateCall;
          end; }
        end;
      end;
    end;
  finally
    if Assigned(LPCallParams) then FreeMem(LPCallParams);
  end;
end;


procedure TTAPIForward.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if dwMsg=LINE_REPLY then Reply(Msg.AsyncFunc^,dwParam2);
  end;
end;

procedure TTAPIForward.Reply(AsyncFunc: TAsyncFunc; Error: DWord);
begin
  IF (AsyncFunc=afForward) and (Error<>0) then FActive:=False;
  If Assigned(FOnReply) then FOnReply(FConsultCall,AsyncFunc,Error);
end;

procedure TTAPIForward.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive:=MakeForward(Value);
  end;
end;

{ TForwardList }

function TForwardList.Add: TForwardListItem;
begin
  Result:=TForwardListItem(inherited Add);
end;

constructor TForwardList.Create(ItemClass: TCollectionItemClass);
begin
  inherited;
  GetMem(FListStruct,SizeOf(TLineForwardList)+1000);
  FListStruct.dwTotalSize:=SizeOf(TLineForwardList)+1000;
end;

destructor TForwardList.Destroy;
begin
  FreeMem(FListStruct);
  inherited;
end;

function TForwardList.GetListStruct: PLineForwardList;
var Item:TForwardListItem;
    i,Offset:Integer;
begin
  Offset:=SizeOf(TLineForwardList)+(SizeOf(TLineForward)*(Count-1));
  for i:=0 to Count-1 do
  begin
    Item:=TForwardListItem(inherited Items[i]);
    FListStruct^.ForwardList[i].dwForwardMode:=ForwardModeToInt(Item.FForwardMode);
    FListStruct^.ForwardList[i].dwCallerAddressSize:=0;
    FListStruct^.ForwardList[i].dwCallerAddressOffset:=0;
    StrCopy(PChar(FListStruct)+Offset,PChar(Item.FDestAddress));
    //Offset:=Offset+Length(Item.DestAddress)+1;
    FListStruct^.ForwardList[i].dwDestCountryCode:=0;
    FListStruct^.ForwardList[i].dwDestAddressSize:=Length(Item.FDestAddress)+1;
    FListStruct^.ForwardList[i].dwDestAddressOffset:=Offset;
    Offset:=Offset+Length(Item.DestAddress)+1;
  end;

  FListStruct^.dwNumEntries:=Count;
  result:=FListStruct;
end;

function TForwardList.GetNamePath: string;
begin
  Result:= inherited GetNamePath;
end;

{ TForwardListItem }

constructor TForwardListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TForwardListItem.Destroy;
begin
  inherited;
end;

{$ENDIF}
{$ENDIF}
end.
 