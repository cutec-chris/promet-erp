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
unit uBaseDatasetInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
  TSortDirection = (sdAscending, sdDescending, sdIgnored);

  { IBaseDbFilter }

  IBaseDbFilter = interface['{7EBB7ABE-1171-4333-A609-C0F59B1E2C5F}']
    function GetBaseSortDirection: TSortDirection;
    function GetfetchRows: Integer;
    function GetUseBaseSorting: Boolean;
    procedure SetBaseSortDirection(AValue: TSortDirection);
    function GetBaseSorting: string;
    procedure SetBaseSorting(AValue: string);
    procedure SetBaseSortFields(const AValue: string);
    function GetBaseSortFields: string;
    function GetFields: string;
    procedure SetfetchRows(AValue: Integer);
    procedure SetFields(const AValue: string);
    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    function GetFilter: string;
    procedure SetFilter(const AValue: string);
    function GetBaseFilter: string;
    procedure SetBaseFilter(const AValue: string);
    function GetFilterTables: string;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetLocalSortFields: string;
    function GetSortLocal: Boolean;
    procedure SetSortLocal(const AValue: Boolean);
    procedure SetFilterTables(const AValue: string);
    procedure Setlimit(const AValue: Integer);
    procedure SetSortDirection(const AValue: TSortDirection);
    procedure SetSortFields(const AValue: string);
    procedure SetLocalSortFields(const AValue: string);
    function GetUsePermissions: Boolean;
    procedure SetUseBaseSorting(AValue: Boolean);
    procedure SetUsePermisions(const AValue: Boolean);
    function GetDistinct: Boolean;
    procedure SetDistinct(const AValue: Boolean);
    procedure DoExecSQL;
    function NumRowsAffected : Integer;

    property FullSQL : string read GetSQL write SetSQL;
    property Filter : string read GetFilter write SetFilter;
    property FetchRows : Integer read GetfetchRows write SetfetchRows;
    property BaseFilter : string read GetBaseFilter write SetBaseFilter;
    property Limit : Integer read GetLimit write Setlimit;
    property Fields : string read GetFields write SetFields;
    property SortFields : string read GetSortFields write SetSortFields;
    property LocalSortFields : string read GetLocalSortFields write SetLocalSortFields;
    property BaseSortFields : string read GetBaseSortFields write SetBaseSortFields;
    property BaseSorting : string read GetBaseSorting write SetBaseSorting;
    property UseBaseSorting : Boolean read GetUseBaseSorting write SetUseBaseSorting;
    property BaseSortDirection : TSortDirection read GetBaseSortDirection write SetBaseSortDirection;
    property SortDirection : TSortDirection read GetSortDirection write SetSortDirection;
    property Distinct : Boolean read GetDistinct write SetDistinct;
    property SortLocal : Boolean read GetSortLocal write SetSortLocal;
    property FilterTables : string read GetFilterTables write SetFilterTables;
    property UsePermissions : Boolean read GetUsePermissions write SetUsePermisions;
  end;
  IBaseManageDB = interface['{271BD4A2-2720-49DA-90A6-AA64FB2B9862}']
    function GetConnection: TComponent;
    function GetManagedFieldDefs: TFieldDefs;
    function GetManagedIndexDefs: TIndexDefs;
    function GetTableCaption: string;
    function GetTableName: string;
    function GetUpChangedBy: Boolean;
    function GetUpStdFields: Boolean;
    function GetUseIntegrity: Boolean;
    procedure SetUpChangedBy(AValue: Boolean);
    procedure SetUpStdFields(AValue: Boolean);
    procedure SetTableCaption(const AValue: string);
    function CreateTable : Boolean;
    function CheckTable : Boolean;
    function AlterTable : Boolean;
    procedure SetTableName(const AValue: string);
    procedure SetUseIntegrity(AValue: Boolean);
    property ManagedFieldDefs : TFieldDefs read GetManagedFieldDefs;
    property ManagedIndexDefs : TIndexDefs read GetManagedIndexDefs;
    property TableName : string read GetTableName write SetTableName;
    property TableCaption : string read GetTableCaption write SetTableCaption;
    property UseIntegrity : Boolean read GetUseIntegrity write SetUseIntegrity;
    property UpdateStdFields : Boolean read GetUpStdFields write SetUpStdFields;
    property UpdateChangedBy : Boolean read GetUpChangedBy write SetUpChangedBy;
    property DBConnection : TComponent read GetConnection;
  end;

  { IBaseSubDataSets }

  IBaseSubDataSets = interface['{CB011ABE-E465-4BD4-AA49-D3A8852AA012}']
    function GetSubDataSet(aName : string): TComponent;
    function GetCount : Integer;
    function GetSubDataSetIdx(aIdx : Integer): TComponent;
    procedure RegisterSubDataSet(aDataSet : TComponent);
    property SubDataSet[aIdx : Integer] : TComponent read GetSubDataSetIdx;
  end;

  IBaseModifiedDS = interface['{311D0DE7-9248-4412-8195-B69EAB813895}']
    function IsChanged : Boolean;
  end;

implementation

end.

