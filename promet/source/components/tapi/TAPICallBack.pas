{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5 / 6 / 7                             *}
{*        System          :  Windows                                          *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 2.2)      *}
{*        Last Update     :  24.11.2002                                       *}
{*        Version         :  1.2                                              *}
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
unit TAPICallBack;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses Windows;

{$INCLUDE TAPI.INC}

procedure LineCallback(hDevice, dwMsg,dwCallbackInstance, dwParam1, dwParam2, dwParam3: DWORD); stdcall;
procedure PhoneCallback(hDevice, dwMsg,dwCallbackInstance, dwParam1, dwParam2, dwParam3: DWORD); stdcall;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses Messages,SysUtils,Forms,Controls,TAPI,classes,TAPISystem,TAPIServices,TAPILines,TAPIDevices,TAPITon,
     TAPIPhone,TAPIAddress,TAPICall,TAPIHelpFunc,TAPIForward,TAPIConference
     {$IFDEF TAPI20}{$IFNDEF FPC},TAPIAgent,ACDProxy{$ENDIF}{$ENDIF};


procedure NotifyComponents(Msg: TCMTAPI);
var i:Integer;
    j:Integer;
    Temp:TComponent;
begin
  for i:= 0 to Screen.FormCount - 1 do
  begin
    for j:=Screen.Forms[i].ComponentCount- 1 downto 0 do
    begin
      Temp:=Screen.Forms[i].Components[j];
      if (Temp is TTAPIComponent) then
      begin
        TTAPIComponent(Temp).PerformMsg(Msg);
      end;
    end;
  end;
end;

{TAPICallBackFunc}

procedure LineCallback(hDevice, dwMsg,dwCallbackInstance, dwParam1, dwParam2, dwParam3: DWORD); stdcall;
var {$IFDEF DEBUG}
    MS:String;
    DT:String;
    {$ENDIF}
    {$IFDEF TAPI20}
    Temp:TComponent;
    I:Integer;
    {$ENDIF}
    AsyncFunc:TAsyncFunc;
    Obj:TObject;
    {$IFNDEF TAPI20}
    //TempCall:TTAPICall;
    {$ENDIF}
    Msg:TMessage;
    PrivMsg:TTAPIRec;
begin
  {$IFDEF DEBUG}
  OutputDebugString(PChar('Callbackinst :('+IntToStr(dwCallbackInstance)+')'));
  case dwMsg of
    LINE_ADDRESSSTATE:         MS:='LINE_ADDRESSSTATE';
    LINE_CALLINFO:             MS:='LINE_CALLINFO';
    LINE_CALLSTATE:            MS:='LINE_CALLSTATE('+IntToStr(dwParam1)+')';
    LINE_CLOSE:                MS:='LINE_CLOSE';
    LINE_DEVSPECIFIC:          MS:='LINE_DEVSPECIFIC';
    LINE_DEVSPECIFICFEATURE:   MS:='LINE_DEVSPECIFICFEATURE';
    LINE_GATHERDIGITS:         MS:='LINE_GATHERDIGITS';
    LINE_GENERATE:             MS:='LINE_GENERATE';
    LINE_LINEDEVSTATE:         MS:='LINE_LINEDEVSTATE ('+IntToStr(dwParam1)+')';
    LINE_MONITORDIGITS:        MS:='LINE_MONITORDIGITS';
    LINE_MONITORMEDIA:         MS:='LINE_MONITORMEDIA';
    LINE_MONITORTONE:          MS:='LINE_MONITORTONE';
    LINE_REPLY:                MS:='LINE_REPLY';
    LINE_REQUEST:              MS:='LINE_REQUEST';
    PHONE_BUTTON:              MS:='PHONE_BUTTON';
    PHONE_CLOSE:               MS:='PHONE_CLOSE';
    PHONE_DEVSPECIFIC:         MS:='PHONE_DEVSPECIFIC';
    PHONE_REPLY:               MS:='PHONE_REPLY';
    PHONE_STATE:               MS:='PHONE_STATE';
    LINE_CREATE:               MS:='LINE_CREATE';
    PHONE_CREATE:              MS:='PHONE_CREATE';
    {$IFDEF TAPI20}
    LINE_AGENTSPECIFIC:        MS:='LINE_AGENTSPECIFIC';
    LINE_AGENTSTATUS:          MS:='LINE_AGENTSTATUS';
    LINE_APPNEWCALL:           MS:='LINE_APPNEWCALL';
    LINE_PROXYREQUEST:         MS:='LINE_PROXYREQUEST';
    LINE_REMOVE:               MS:='LINE_REMOVE';
    PHONE_REMOVE:              MS:='PHONE_REMOVE';
    {$ENDIF}
    {$IFDEF TAPI22}
    LINE_AGENTSESSIONSTATUS:   MS:='LINE_AGENTSESSIONSTATUS';
    LINE_QUEUESTATUS:          MS:='LINE_QUEUESTATUS';
    LINE_AGENTSTATUSEX:        MS:='LINE_AGENTSTATUSEX';
    LINE_GROUPSTATUS:          MS:='LINE_GROUPSTATUS';
    LINE_PROXYSTATUS:          MS:='LINE_PROXYSTATUS';
    {$ENDIF}
    {$IFDEF TAPI30}
    LINE_APPNEWCALLHUB:        MS:='LINE_APPNEWCALLHUB';
    LINE_CALLHUBCLOSE:         MS:='LINE_CALLHUBCLOSE';
    LINE_DEVSPECIFICEX:        MS:='LINE_DEVSPECIFICEX';
    {$ENDIF}
    else MS:='unbekannte Nachricht';
  end;
  case dwMsg of
    LINE_ADDRESSSTATE:         DT:='hLINE';
    LINE_CALLINFO:             DT:='hCALL';
    LINE_CALLSTATE:            DT:='hCALL';
    LINE_CLOSE:                DT:='hLINE';
    LINE_DEVSPECIFIC:          DT:='hLineOrCall';
    LINE_DEVSPECIFICFEATURE:   DT:='hLineOrCall';
    LINE_GATHERDIGITS:         DT:='hCALL';
    LINE_GENERATE:             DT:='hCALL';
    LINE_LINEDEVSTATE:         DT:='hLINE';
    LINE_MONITORDIGITS:        DT:='hCALL';
    LINE_MONITORMEDIA:         DT:='hCALL';
    LINE_MONITORTONE:          DT:='hCALL';
    LINE_REPLY:                DT:='unused';
    LINE_REQUEST:              DT:='unused';
    LINE_CREATE:               DT:='unused';
    {$IFDEF TAPI20}
    LINE_AGENTSPECIFIC:        DT:='hLINE';
    LINE_AGENTSTATUS:          DT:='hLINE';
    LINE_APPNEWCALL:           DT:='hLINE';
    LINE_PROXYREQUEST:         DT:='hLINE';
    LINE_REMOVE:               DT:='unused';
    {$ENDIF}
    {$IFDEF TAPI22}
    LINE_AGENTSESSIONSTATUS:   DT:='hLINE';
    LINE_QUEUESTATUS:          DT:='hLINE';
    LINE_AGENTSTATUSEX:        DT:='hLINE';
    LINE_GROUPSTATUS:          DT:='hLINE';
    LINE_PROXYSTATUS:          DT:='hLINE';
    {$ENDIF}
    {$IFDEF TAPI30}
    LINE_APPNEWCALLHUB:        DT:='hCALL';
    LINE_CALLHUBCLOSE:         DT:='hCALL';
    LINE_DEVSPECIFICEX:        DT:='hLineOrCall';
    {$ENDIF}
    else DT:='unbekannter Device Type';
  end;
  OutputDebugString(PChar(MS+'(Device ['+DT+']:'+IntToStr(hDevice)+')'));
  OutputDebugString(PChar(MS+'(Param1:'+IntToStr(dwParam1)+')'));
  OutputDebugString(PChar(MS+'(Param2:'+IntToStr(dwParam2)+')'));
  OutputDebugString(PChar(MS+'(Param3:'+IntToStr(dwParam3)+')'));
  {$ENDIF}
  PrivMsg.hDevice:=hDevice;
  PrivMsg.dwMsg:=dwMsg;
  PrivMsg.dwCallbackInstance:=dwCallbackInstance;
  PrivMsg.dwParam1:=dwParam1;
  PrivMsg.dwParam2:=dwParam2;
  PrivMsg.dwParam3:=dwParam3;
  Msg.Msg:=CM_TAPI;
  Msg.WParam:=0;
  Msg.LParam:=LongWord(@PrivMsg);
  Msg.Result:=0;
  if dwMsg = LINE_REPLY then
  begin
    {$IFDEF DEBUG}
    OutputDebugString(PChar('LINE_REPLY::Param1:'+IntToStr(dwParam1)+' ERRORCode: '+IntToStr(dwParam2)));
    {$ENDIF}
    Obj:=AppTAPIMgr.AsyncList.GetObject(dwParam1);
    AsyncFunc:=AppTAPIMgr.AsyncList.GetFunction(dwParam1);
    Msg.WParam:=LongWord(@AsyncFunc);
    AppTAPIMgr.AsyncList.Delete(dwParam1);
    TTAPIComponent(Obj).PerformMsg(TCMTAPI(Msg));
    {$IFDEF TAPI20}
    {$IFNDEF FPC}
    if (Obj is TAgentCaps) then
    begin
      TAgentCaps(Obj).Reply(AsyncFunc,dwParam2);
      for i:=AppTAPIMgr.TAPIObjects.Count -1 downto 0 do
      begin
        Temp:=AppTAPIMgr.TAPIObjects.Items[i];
        if (Temp is TTAPILineAgent)then
        begin
          if TTAPILineAgent(Temp).Caps=TAgentCaps(Obj) then
          begin
            if dwParam2=0 then
              TTAPILineAgent(Temp).CapsChange
            else
              TTAPILineAgent(Temp).Close;
            Exit;
          end;
        end;
      end;
    end;
    {$ENDIF}
    {$ENDIF}
  end
  else
    NotifyComponents(TCMTAPI(Msg));

  {$IFDEF DEBUG}
  OutputDebugString('Leave  * *  LineCallBack  * *');
  {$ENDIF}
end;


procedure PhoneCallback(hDevice, dwMsg,dwCallbackInstance, dwParam1, dwParam2, dwParam3: DWORD); stdcall;
var Obj:TObject;
    AsyncFunc:TAsyncFunc;
    {$IFDEF DEBUG}
    MS:String;
    {$ENDIF}
    Msg:TMessage;
    PrivMsg:TTAPIRec;
Begin
   {$IFDEF DEBUG}
   OutputDebugString(PChar('Phone Callbackinst :('+IntToStr(dwCallbackInstance)+')'));
   case dwMsg of
     PHONE_BUTTON:MS:='PHONE_BUTTON';
     PHONE_CLOSE:MS:='PHONE_CLOSE';
     PHONE_DEVSPECIFIC:MS:='PHONE_DEVSPECIFIC';
     PHONE_REPLY:MS:='PHONE_REPLY';
     PHONE_STATE:MS:='PHONE_STATE';
     PHONE_CREATE:MS:='PHONE_CREATE';
     {$IFDEF TAPI20}
     PHONE_REMOVE:MS:='PHONE_REMOVE';
     {$ENDIF}
     else MS:='PHONE_??';
   end;
   OutputDebugString(PChar(MS));
   {$ENDIF}
   PrivMsg.hDevice:=hDevice;
   PrivMsg.dwMsg:=dwMsg;
   PrivMsg.dwCallbackInstance:=dwCallbackInstance;
   PrivMsg.dwParam1:=dwParam1;
   PrivMsg.dwParam2:=dwParam2;
   PrivMsg.dwParam3:=dwParam3;
   Msg.Msg:=CM_TAPI;
   Msg.WParam:=0;
   Msg.LParam:=LongWord(@PrivMsg);
   Msg.Result:=0;
   if dwMsg = PHONE_REPLY then
   begin
     {$IFDEF DEBUG}
     OutputDebugString(PChar('PHONE_RELY::Param1:'+IntToStr(dwParam1)+' ERRORCode: '+IntToStr(dwParam2)));
     {$ENDIF}
     Obj:=AppTAPIMgr.AsyncList.GetObject(dwParam1);
     AsyncFunc:=AppTAPIMgr.AsyncList.GetFunction(dwParam1);
     Msg.WParam:=LongWord(@AsyncFunc);
     AppTAPIMgr.AsyncList.Delete(dwParam1);
     TTAPIComponent(Obj).PerformMsg(TCMTAPI(Msg));
   end
   else
     NotifyComponents(TCMTAPI(Msg));
  {$IFDEF DEBUG}
  OutputDebugString('Leave  * *  PhoneCallBack  * *');
  {$ENDIF}
end;



{$ENDIF}
{$ENDIF}
end.