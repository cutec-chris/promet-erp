{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pProjectbase;

interface

uses
  gsGanttCalendar, uattendanceplan, uAttStatistic, uGanttView, umaintasks, 
  uMeetingFrame, umeetingusers, uProjectFlow, uprojectimport, 
  uprojectpositions, uroughpklanningframe, uTaskEdit, uTaskPlanOptions, 
  utasks, uTaskPlan, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pProjectbase', @Register);
end.
