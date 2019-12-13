unit ubasedbclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynCommons, mORMot;

type

  { TUser }

  TUser = class(TSQLRecord)
  private
    FAcc: RawUTF8;
    FAuthSource: RawUTF8;
    FCustomerNo: RawUTF8;
    FDep: RawUTF8;
    FEmployment: TDateTime;
    FIDCode: RawUTF8;
    FLastLogin: TDateTime;
    Fleaved: TDateTime;
    FLogin: RawUTF8;
    FLoginActive: Boolean;
    FMail: RawUTF8;
    FName: RawUTF8;
    FParent: Int64;
    FPaygroup: Int64;
    fPersNo: RawUTF8;
    FPosition: RawUTF8;
    FRemoteAcc: Boolean;
    FSalt: RawUTF8;
    FType: RawUTF8;
    FUseWorktime: Integer;
    FWeekWorktime: Integer;
    FWorktime: Integer;
    FPassword : RawUTF8;
  published
    property Typ : RawUTF8 index 1 read FType write FType;
    property Parent : Int64 read FParent write FParent;
    property Accountno : RawUTF8 index 20 read FAcc write FAcc;
    property Name : RawUTF8 index 30 read FName write Fname;
    property Password : RawUTF8 index 45 read FPassword write FPassword;
    property Salt : RawUTF8 index 105 read FSalt write FSalt;
    property IdCode : RawUTF8 index 4 read FIDCode write FIdCode;
    property Employment : TDateTime read FEmployment write FEmployment;
    property Leaved : TDateTime read Fleaved write Fleaved;
    property CustomerNo : RawUTF8 index 20 read FCustomerNo write FCustomerno;
    property PersonnelNo : RawUTF8 index 20 read fPersNo write FPersNo;
    property Department : RawUTF8 index 30 read FDep write FDep;
    property Position : RawUTF8 index 30 read FPosition write FPosition;
    property Loginname : RawUTF8 index 30 read FLogin write FLogin;
    property EMail : RawUTF8 index 100 read FMail write FMail;
    property Paygroup : Int64 read FPaygroup write FPaygroup;
    property WorkTime: Integer read FWorktime write FWorktime; //8 wenn NULL
    property WeekWorkTime : Integer read FWeekWorktime write FWeekworktime;//40 wenn NULL
    property USEWORKTIME : Integer read FUseWorktime write FUseWorktime;
    property LOGINACTIVE : Boolean read FLoginActive write FLoginActive;
    property REMOTEACCESS : Boolean read FRemoteAcc write FRemoteAcc;
    property LASTLOGIN : TDateTime read FLastLogin write FLastLogin;
    property AUTHSOURCE : RawUTF8 index 10 read FAuthSource write FAuthSource;
  end;

implementation

end.

