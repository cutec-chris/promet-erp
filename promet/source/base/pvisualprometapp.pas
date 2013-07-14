{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pVisualPrometApp;

interface

uses
  htmltortf, uBaseVisualApplication, ubasevisualapplicationtools, 
  uBaseVisualControls, uFilterFrame, uFilterTabs, uFormAnimate, uLogWait, 
  uMandantOptions, uNRights, uOptions, uOptionsFrame, uPassword, 
  uPersonalOptions, uProcessOptions, uPrometFrames, uRichFrame, uRowEditor, 
  uSearch, uSelectReport, uSyncOptions, uTimeLine, uUserfieldDefOptions, 
  uuseroptions, uwait, ugridview, uhistoryadditem, uHistoryFrame, 
  uMainTreeFrame, uLinkFrame, uImageFrame, utreeview, uPrometFramesInplaceDB, 
  uprometframesinplace, uListFrame, uSendMail, uEditText, uQuickHelpFrame, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pVisualPrometApp', @Register);
end.
