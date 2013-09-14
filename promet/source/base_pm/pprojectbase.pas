{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pProjectbase;

interface

uses
  uprojectimport, uTaskEdit, utasks, umaintasks, uprojectpositions, 
  uMeetingFrame, umeetingusers, gsGanttCalendar, uattendanceplan, uGanttView, 
  uTaskPlanOptions, uProjectFlow, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pProjectbase', @Register);
end.
