{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
unit uData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubasedbclasses,memds;

type

  { TBaseDBModule }

  TBaseDBModule = class(TComponent)
  public
    ConfigPath : string;
    Mandant : string;
    procedure Connect;virtual;abstract;
    function Load(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True) : Boolean;virtual;abstract;
    function Save(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True) : Boolean;virtual;abstract;
    function Select(Obj: TClass;aDS : TMemDataset;aStart: Integer = 0;aCount : Integer = 100;aFilter: string=''; aFields: string=''): Boolean;virtual;abstract;
    function ExecuteDirect(aStatement: string; aDS: TMemDataset): Boolean; virtual;
    function Delete(Selector: Variant) : Boolean;virtual;abstract;
    function GetID : Int64;virtual;abstract;
    function QuoteField(aField : string) : string;virtual;
    function QuoteValue(aValue : string) : string;virtual;
  end;
  DatasetClass = record
    aName : string;
    aClass : TBaseDBDatasetClass;
  end;

const
  RIGHT_NONE  = -1;
  RIGHT_VIEW  = 0;
  RIGHT_READ  = 1;
  RIGHT_WRITE = 2;
  RIGHT_DELETE= 3;
  RIGHT_PERMIT= 4;

  IMAGE_PERSON             = 1;
  IMAGE_TASK               = 2;
  IMAGE_SUPPLIER           = 20;
  IMAGE_SEARCH             = 8;
  IMAGE_FOLDER             = 19;
  IMAGE_STATISTIC          = 58;
  IMAGE_MESSAGEOPEN        = 5;
  IMAGE_TABLEDIR           = 98;
  IMAGE_MESSAGE            = 36;
  IMAGE_FEED               = 61;
  IMAGE_SCRIPT             = 62;
  IMAGE_MASTERDATA         = 0;
  IMAGE_DOCUMENTS          = 25;
  IMAGE_ORDERS             = 7;
  IMAGE_PRODUCTION         = 18;
  IMAGE_CALLS              = 51;
  IMAGE_CALENDAR           = 4;
  IMAGE_FINANCIAL          = 9;
  IMAGE_PROJECT            = 13;
  IMAGE_BANKING            = 32;// 23;  32
  IMAGE_ACCOUNTS           = 33;// 24;  33
  IMAGE_ACCOUNT            = 34;// 25;  34
  IMAGE_NEWACCOUNT         = 35;// 26; 35
  IMAGE_NEWTRANSFER        = 38;// 28; 38
  IMAGE_ACCOUNTINGQUE      = 37;// 27; 37
  IMAGE_ORDERPAGE          = 25;
  IMAGE_REDORDERPAGE       = 26;
  IMAGE_ORDERLIST          = 24;
  IMAGE_WIKI               = 11;
  IMAGE_PROJECTS           = 2;
  IMAGE_WEBSITE            = 94;
  IMAGE_FAVOURITES         = 14;
  IMAGE_STATISTICS         = 15;
  IMAGE_TIME               = 6;

  TREE_ID_CUSTOMER_UNSORTED   = 32999;
  TREE_ID_MASTERDATA_UNSORTED = 32998;
  TREE_ID_PROJECT_UNSORTED    = 329997;
  TREE_ID_MESSAGES            = 329996;
  TREE_ID_UNKNOWN_MESSAGES    = 329995;
  TREE_ID_SPAM_MESSAGES       = 329994;
  TREE_ID_ARCHIVE_MESSAGES    = 329993;
  TREE_ID_SEND_MESSAGES       = 329992;
  TREE_ID_DELETED_MESSAGES    = 329991;
  TREE_ID_WIKI_UNSORTED       = 329990;
  TREE_ID_LOG_MESSAGES        = 329989;

  ACICON_EDITED   = 0;
  ACICON_MAILNEW  = 1;
  ACICON_MAILANSWERED = 2;
  ACICON_CALL     = 3;
  ACICON_NEWORDER = 4;
  ACICON_ORDERPOSTED = 5;
  ACICON_STATUSCH = 6;
  ACICON_DOCUMENTADDED = 7;
  ACICON_USEREDITED = 8;
  ACICON_RENAMED = 12;
  ACICON_ORERSTATUSCH = 6;
  ACICON_TASKADDED     = 10;
  ACICON_TASKCLOSED    = 9;
  ACICON_DATECHANGED   = 11;
  ACICON_OFFICECHANGED   = 13;
  ACICON_EXTERNALCHANGED   = 14;
resourcestring
  strGuest                       = 'Gast';
  strWebsite                     = 'Webseite';
  strScreenshotName              = 'Screenshot Name';
  strEnterAnName                 = 'enter an Name';
  strProjectProcess              = 'Projekt/Prozess';
  strFor                         = 'für';

var
  DataM : TBaseDBModule = nil;
  GlobalUser : TUser;
  DatasetClasses : array of DatasetClass;

function Data : TBaseDBModule;
procedure RegisterdataSetClass(aName: string;aClass : TBaseDBDatasetClass);

implementation

function Data: TBaseDBModule;
begin
  if not Assigned(DataM) then
    DataM := TBaseDBModule.Create(nil);
  Result := DataM;
end;

procedure RegisterdataSetClass(aName: string; aClass: TBaseDBDatasetClass);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  for i := low(DatasetClasses) to High(DatasetClasses) do
    if DatasetClasses[i].aName=aName then
      begin
        Found := True;
        break;
      end;
  if not Found then
    begin
      SetLength(DatasetClasses,length(DatasetClasses)+1);
      DatasetClasses[length(DatasetClasses)-1].aName:=aName;
      DatasetClasses[length(DatasetClasses)-1].aClass:=aClass;
    end;
end;

{ TBaseDBModule }

function TBaseDBModule.ExecuteDirect(aStatement: string; aDS: TMemDataset
  ): Boolean;
begin
  Result := False;
end;

function TBaseDBModule.QuoteField(aField: string): string;
begin
  Result := '"'+aField+'"';
end;

function TBaseDBModule.QuoteValue(aValue: string): string;
begin
  Result := ''''+aValue+'''';
end;

finalization
  FreeAndNil(GlobalUser);
  //FreeAndNil(DataM);
end.


