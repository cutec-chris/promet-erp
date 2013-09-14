{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pVisualPrometApp;

interface

uses
  htmltortf, uBaseVisualApplication, ubasevisualapplicationtools, 
  uBaseVisualControls, uFilterTabs, uFormAnimate, uLogWait, uMandantOptions, 
  uNRights, uOptions, uOptionsFrame, uPassword, uPersonalOptions, 
  uProcessOptions, uPrometFrames, uRichFrame, uRowEditor, uSearch, 
  uSyncOptions, uTimeLine, uUserfieldDefOptions, uuseroptions, uwait, 
  uhistoryadditem, uHistoryFrame, uMainTreeFrame, uLinkFrame, uImageFrame, 
  utreeview, uPrometFramesInplaceDB, uprometframesinplace, uListFrame, 
  uSendMail, uEditText, uQuickHelpFrame, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pVisualPrometApp', @Register);
end.
