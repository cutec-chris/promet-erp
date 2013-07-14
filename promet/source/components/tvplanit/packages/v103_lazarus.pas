{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit v103_lazarus;

interface

uses
  VpAbout, VpAlarmDlg, VpBase, VpBaseDS, VpCalendar, VpCanvasUtils, VpConst, 
  VpContactButtons, VpContactEditDlg, VpContactGrid, VpData, VpDateEdit, 
  VpDatePropEdit, VpDayView, VpDBDS, VpDlg, VpEdElem, VpEdFmt, VpEdPop, 
  VpEdShape, VpEvntEditDlg, VpException, VpLEDLabel, VpLocalize, VpMisc, 
  VpMonthView, VpNabEd, VpNavBar, VpPrtFmt, VpPrtFmtCBox, VpPrtFmtDlg, 
  VpPrtFmtEd, VpPrtPrv, VpPrtPrvDlg, VpReg, VpResEditDlg, VpSelResDlg, VpSR, 
  VpTaskEditDlg, VpTaskList, VpTimerPool, VpWavDlg, VpWavPE, VpWeekView, 
  VpXBase, VpXChrFlt, VpXParsr, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VpReg', @VpReg.Register);
end;

initialization
  RegisterPackage('v103_lazarus', @Register);
end.
