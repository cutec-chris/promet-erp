program testgui;

{$mode objfpc}{$H+}

uses
//  sheaptrc,
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, Forms, GuiTestRunner, lazmouseandkeyinput, general,
  richmemopackage, pvisualprometapp, uBaseVisualApplication, tLogin, tdbtests,
  ucreatetable,
  utransaction,
  //utgridview,
  udocumentmanagement,
  utzugferd,
  tmemoryleak,
  utfulllogout,
  ulinkcheck,
  upersontest,
  umasterdatatest,
  uprojecttest,
  uordertest,

  tLogout;

{$R *.res}

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
