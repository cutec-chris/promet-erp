{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_tapi;

interface

uses
  Tapi, TAPIServices, TAPIAddress, TAPIDevices, TAPITrans, AssistedTAPI, 
  CallList, DevConf, TAPICall, TAPICallBack, TAPIConference, TAPICurVer, 
  TAPIErr, TAPIForward, TAPIHelpFunc, TAPILines, TAPILists, TAPIMediaCtl, 
  TAPIObj, TAPIPhone, TAPIProv, TAPIRedirect, TAPISystem, TAPIThread, TAPITon, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('laz_tapi', @Register);
end.
