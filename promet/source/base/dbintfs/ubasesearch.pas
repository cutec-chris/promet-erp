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
unit uBaseSearch;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDBClasses,uIntfStrConsts,variants,db,uBaseDatasetInterfaces;
resourcestring
  strMatchcode                  = 'Suchbegriffe';
  strShortnames                 = 'Kurztexte';
  strIdents                     = 'Idents';
  strFulltext                   = 'Volltext';
const
  SearchLocations : array[0..7] of string  = (strMatchcode,strIdents,strShortnames,strSerial,strCommission,strBarcode,strDescription,strFulltext);
  SearchLocDefault: array[0..7] of Boolean = (True        ,True     ,True         ,True     ,True         ,True      ,True         ,False);
type
  TSearchLocations = array of string;
  TFullTextSearchType = (fsMatchcode=0,fsIdents=1,fsShortnames=2,fsSerial=3,fsCommission=4,fsBarcode=5,fsDescription=6,fsFulltext=7);
  TFullTextSearchTypes = set of TFullTextSearchType;
  TSearchResultItem= procedure(aIdent : string;aName : string;aStatus : string;aActive : Boolean;aLink : string;aPriority : Integer = 0;aItem : TBaseDBList = nil) of object;

  { TSearchHistory }

  TSearchHistory = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure Add(aText,aLink : string);
  end;
  TSearch = class(TComponent)
  private
    FActive: Boolean;
    FBeginSearch: TNotifyEvent;
    FCount: Integer;
    FEndSearch: TNotifyEvent;
    FFullEndSearch: TNotifyEvent;
    FItemFound: TSearchResultItem;
    FNewFound: Boolean;
    FNextLevel: Integer;
    FSearchString: string;
    FSearchTypes: TFullTextSearchTypes;
    FSearchLocations: TSearchLocations;
    FSender: TComponent;
    FUseContains: Boolean;
    FMaxResults: LongInt;
    FSearchHist: TSearchHistory;
    Lists : array of TBaseDBList;
  public
    constructor Create(aSearchTypes : TFullTextSearchTypes;aSearchLocations : TSearchLocations;aUseContains : Boolean = False;aMaxresults : Integer = 0);
    destructor Destroy;override;
    function Start(SearchText: string;aLevel : Integer = 0) : Boolean;
    procedure DoStart(SearchText: string; SearchUnsharp: Boolean=True);
    procedure DoStartAllObjectsSearch(SearchText : string);
    procedure DoStartHistorySearch(SearchText : string);
    procedure Abort;
    property Active : Boolean read FActive;
    property OnBeginItemSearch : TNotifyEvent read FBeginSearch write FBeginSearch;
    property OnItemFound : TSearchResultItem read FItemFound write FItemFound;
    property OnEndItemSearch : TNotifyEvent read FEndSearch write FEndSearch;
    property OnEndSearch : TNotifyEvent read FFullEndSearch write FFullEndSearch;
    property Sender : TComponent read FSender write FSender;
    property Count : Integer read FCount;
    property NextSearchLevel : Integer read FNextLevel;
    property SearchString : string read FSearchString;
    property NewFound : Boolean read FNewFound;
  end;
  TLinkObject = class(TObject)
  private
    FLink: string;
  public
    constructor Create(aLink : string);
    property Link : string read FLink write FLink;
  end;
  procedure AddSearchAbleDataSet(aClass : TBaseDBListClass);
  function GetSearchAbleItems : TSearchLocations;
implementation
uses uBaseApplication, uBaseDbInterface,uOrder,uData;
var SearchAble : array of TBaseDBListClass;
procedure AddSearchAbleDataSet(aClass: TBaseDBListClass);
begin
  Setlength(SearchAble,length(SearchAble)+1);
  SearchAble[length(SearchAble)-1] := aClass;
end;
function GetSearchAbleItems: TSearchLocations;
var
  aSearchAble: TBaseDbList;
  i: Integer;
  DontAdd: Boolean;
  a: Integer;
begin
  for i := 0 to length(SearchAble)-1 do
    begin
      with BaseApplication as IBaseDBInterface do
        aSearchAble := SearchAble[i].Create(nil);
      DontAdd := False;
      for a := 0 to length(Result)-1 do
        if Result[a] = aSearchAble.Caption then
          DontAdd := True;
      if not DontAdd then
        begin
          Setlength(Result,length(Result)+1);
          Result[length(Result)-1] := aSearchAble.Caption;
          aSearchAble.Free;
        end;
    end;
end;
procedure TSearchHistory.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SEARCHHISTORY';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TEXT',ftString,200,False);
            Add('LINK',ftString,400,False);
            Add('CHANGEDBY',ftString,4,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('TEXT','TEXT',[]);
            Add('CHANGEDBY','CHANGEDBY',[]);
            Add('TIMESTAMPD','TIMESTAMPD',[]);
          end;
    end;
end;

procedure TSearchHistory.Add(aText, aLink: string);
begin
  Insert;
  FieldByName('TEXT').AsString:=aText;
  FieldByName('LINK').AsString:=aLink;
  Post;
end;

constructor TLinkObject.Create(aLink: string);
begin
  FLink := aLink;
end;
constructor TSearch.Create(aSearchTypes: TFullTextSearchTypes; aSearchLocations: TSearchLocations;
  aUseContains: Boolean; aMaxresults: Integer);
var
  i: Integer;
  a: Integer;
  aSearchAble: TBaseDbList;
  Found: Boolean;
begin
  SetLength(Lists,0);
  FSearchHist := TSearchHistory.Create(nil);
  FSearchHist.CreateTable;
  FSearchTypes := aSearchTypes;
  FSearchLocations := aSearchLocations;
  FUseContains := aUseContains;
  FMaxResults := aMaxResults;
  with BaseApplication as IBaseDBInterface do
    begin
      for i := 0 to length(SearchAble)-1 do
        begin
          aSearchAble := SearchAble[i].Create(nil);
          Found := False;
          for a := 0 to length(FSearchLocations)-1 do
            begin
              if FSearchLocations[a] = aSearchAble.Caption then
                begin
                  Setlength(Lists,length(Lists)+1);
                  Lists[length(Lists)-1] := aSearchAble;
                  aSearchable.CreateTable;
                  Found := True;
                  break;
                end
            end;
          if not Found then
            aSearchAble.Free;
        end;
    end;
end;
destructor TSearch.Destroy;
var
  i: Integer;
begin
  FSearchHist.Free;
  for i := 0 to length(Lists)-1 do
    Lists[i].Free;
  inherited Destroy;
end;
function TSearch.Start(SearchText: string; aLevel: Integer): Boolean;
begin
  Result := True;
  FNewFound := False;
  FNextLevel := aLevel+1;
  if SearchText<>FSearchString then
    begin
      FSearchString:=SearchText;
      FCount := 0;
    end;
  case aLevel of
  0:DoStartHistorySearch(SearchText);
  1:DoStartAllObjectsSearch(SearchText);
  2:DoStart(SearchText,False);
  3:DoStart(SearchText,True);
  else Result := False;
  end;
end;

procedure TSearch.DoStart(SearchText: string; SearchUnsharp: Boolean);
var
  i: Integer;
  aFilter: String;
  tmp: String;
  aType: TFullTextSearchType;
  aActive: Boolean;
  aPos: TOrderPos;
  aOrder: TOrder;
  aPrio: Integer;
  aLPrio: Integer;
  function EncodeField(Data : TBaseDBModule;Val : string) : string;
  begin
    with Lists[i].DataSet as IBaseManageDB do
      Result := 'UPPER('+Data.QuoteField(TableName)+'.'+Data.QuoteField(Val)+')';
  end;

  function EncodeValue(Data : TBaseDBModule;Val : string) : string;
  begin
    if FUseContains and SearchUnsharp then
      Result := Data.QuoteValue('*'+Data.EscapeString(Val)+'*')
    else
      Result := Data.QuoteValue(Data.EscapeString(Val));
    Result := 'UPPER('+Result+')';
  end;

  function CastText(Data : TBaseDBModule;Val : string) : string;
  begin
    with Lists[i].DataSet as IBaseManageDB do
      Result := 'UPPER(CAST('+Data.QuoteField(TableName)+'.'+Data.QuoteField(Val)+' as VARCHAR(8000)))';
  end;
begin
  if SearchUnsharp then
    begin
      with BaseApplication as IBaseApplication do
        Debug('Search:StartSearch unsharp');
      aPrio := 1000;
    end
  else
    begin
      with BaseApplication as IBaseApplication do
        Debug('Search:StartSearch sharp');
      aPrio := 2000;
    end;
  if not Assigned(FItemFound) then exit;
  FActive := True;
  with BaseApplication as IBaseDBInterface do
    begin
      //Search for registered Searchtytpes
      for i := 0 to length(Lists)-1 do
        begin
          if Assigned(FBeginSearch) then FBeginSearch(Self);
          aFilter := '';
          tmp := Lists[i].Caption;
          with Lists[i].DataSet as IBaseDbFilter do
            begin
              Lists[i].DataSet.Close;
              for aType := low(TFullTextSearchTypes) to High(TFullTextSearchTypes) do
                begin
                  aFilter := '';
                  if not FActive then
                    begin
                      if Assigned(FEndSearch) then FEndSearch(Self);
                      break;
                    end;
                  if (aType = fsMatchcode) and (fsMatchcode in FSearchTypes) and (Lists[i].GetMatchcodeFieldName <> '') then
                    aFilter += ' OR ('+Data.ProcessTerm(EncodeField(Data,Lists[i].GetMatchcodeFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                  if (aType = fsIdents) and (fsIdents in FSearchTypes) then
                    begin
                      aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,Lists[i].GetNumberFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                      if Lists[i].GetBookNumberFieldName <> '' then
                        aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,Lists[i].GetBookNumberFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                    end;
                  if (aType = fsBarcode) and (fsBarcode in FSearchTypes) and (Lists[i].GetBarcodeFieldName <> '') then
                    aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,Lists[i].GetBarcodeFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                  if (aType = fsCommission) and (fsCommission in FSearchTypes) and (Lists[i].GetCommissionFieldName <> '') then
                    aFilter += ' OR ('+Data.ProcessTerm(EncodeField(Data,Lists[i].GetCommissionFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                  if SearchUnsharp then
                    begin
                      if (aType = fsShortnames) and (aType = fsShortnames) and (fsShortnames in FSearchTypes) then
                        aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,Lists[i].GetTextFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                      if (aType = fsDescription) and (fsDescription in FSearchTypes) and (Lists[i].GetDescriptionFieldName <> '') then
                        aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,Lists[i].GetDescriptionFieldName)+' = '+EncodeValue(Data,SearchText))+')';
                      if Lists[i] is TBaseHistory then
                        aFilter += ' AND (('+Data.ProcessTerm(Data.QuoteField('OBJECT')+'='+Data.QuoteValue('PROJECTS@*'))+') OR ('+Data.ProcessTerm(Data.QuoteField('OBJECT')+'='+Data.QuoteValue('MASTERDATA@*'))+'))';
                    end;
                  aFilter := copy(aFilter,pos(' ',aFilter)+1,length(aFilter));
                  aFilter := copy(aFilter,pos(' ',aFilter)+1,length(aFilter));
                  if aFilter <> '' then
                    begin
                      SortFields:='';
                      Filter := aFilter;
                      Limit := FMaxResults;
                      FetchRows:=1;
                      Lists[i].Open;
                      if Assigned(Lists[i].Status) then
                        Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue(Lists[i].Typ));
                      while not Lists[i].DataSet.EOF do
                        begin
                          if Assigned(Lists[i].Status) then
                            begin
                              aActive := Data.States.DataSet.Locate('STATUS',Lists[i].Status.AsString,[loCaseInsensitive]);
                              if aActive then
                                begin
                                  aActive := aActive and (Data.States.FieldByName('ACTIVE').AsString='Y');
                                  aLPrio := aPrio;
                                  if aActive then
                                    aLPrio := aLPrio+500;
                                  FItemFound(Lists[i].Number.AsString,Lists[i].Text.AsString,Lists[i].Status.AsString,aActive,Data.BuildLink(Lists[i].DataSet),aPrio,Lists[i]);
                                  FNewFound:=True;
                                end
                              else
                                begin
                                  FItemFound(Lists[i].Number.AsString,Lists[i].Text.AsString,Lists[i].Status.AsString,True,Data.BuildLink(Lists[i].DataSet),aPrio+300,Lists[i]);
                                  FNewFound:=True;
                                end;
                            end
                          else
                            FItemFound(Lists[i].Number.AsString,Lists[i].Text.AsString,'',True,Data.BuildLink(Lists[i].DataSet),aPrio,Lists[i]);
                          inc(FCount);
                          if not FActive then break;
                          Lists[i].DataSet.Next;
                        end;
                    end;
                end;
            end;
          if Assigned(FEndSearch) then FEndSearch(Self);
        end;
      //Search for Serial Number
      if (fsSerial in FSearchTypes) and (trim(SearchText)<>'') then
        begin
          aPos := TOrderPos.Create(nil);
          try
            Data.SetFilter(aPos,Data.QuoteField('SERIAL')+'='+Data.QuoteValue(SearchText),FMaxResults);
            while not aPos.EOF do
              begin
                aOrder := Torder.Create(nil);
                aOrder.Select(aPos.FieldByName('REF_ID').AsVariant);
                aOrder.Open;
                if aOrder.Count>0 then
                  FItemFound(aOrder.Number.AsString,aOrder.Text.AsString,aOrder.Status.AsString,True,Data.BuildLink(aOrder.DataSet),aPrio+300,aOrder);
                aOrder.Free;
                aPos.Next;
              end;
          except  //maybe table dont exists
          end;
          aPos.Free;
        end;

    end;
  FActive := False;
  if Assigned(FFullEndSearch) then FFullEndSearch(Self);
  with BaseApplication as IBaseApplication do
    Debug('Search:StartSearch End');
end;

procedure TSearch.DoStartAllObjectsSearch(SearchText: string);
var
  aObjects: TObjects;
  aType: TFullTextSearchType;
  aFilter: String;
  aDSClass: TBaseDBDatasetClass;
  i: Integer;
  aLinkDs: TBaseDbList;
  aActive: Boolean;

  function EncodeField(Data : TBaseDBModule;Val : string) : string;
  begin
    with aObjects.DataSet as IBaseManageDB do
      Result := 'UPPER('+Data.QuoteField(TableName)+'.'+Data.QuoteField(Val)+')';
  end;

  function EncodeValue(Data : TBaseDBModule;Val : string) : string;
  begin
    if FUseContains  then
      Result := Data.QuoteValue('*'+Data.EscapeString(Val)+'*')
    else
      Result := Data.QuoteValue(Data.EscapeString(Val));
    Result := 'UPPER('+Result+')';
  end;

  function CastText(Data : TBaseDBModule;Val : string) : string;
  begin
    with aObjects.DataSet as IBaseManageDB do
      Result := 'UPPER(CAST('+Data.QuoteField(TableName)+'.'+Data.QuoteField(Val)+' as VARCHAR(8000)))';
  end;
begin
  if not Assigned(FItemFound) then exit;
  with BaseApplication as IBaseApplication do
    Debug('Search:StartAllObjectsSearch');
  FActive:=True;
  aObjects := TObjects.Create(nil);
  aFilter := '';
  if not FActive then
    begin
      if Assigned(FEndSearch) then FEndSearch(Self);
      exit;
    end;
  if (fsMatchcode in FSearchTypes) and (aObjects.GetMatchcodeFieldName <> '') then
    aFilter += ' OR ('+Data.ProcessTerm(EncodeField(Data,aObjects.GetMatchcodeFieldName)+' = '+EncodeValue(Data,SearchText))+')';
  if (fsIdents in FSearchTypes) then
    begin
      aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,aObjects.GetNumberFieldName)+' = '+EncodeValue(Data,SearchText))+')';
      if aObjects.GetBookNumberFieldName <> '' then
        aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,aObjects.GetBookNumberFieldName)+' = '+EncodeValue(Data,SearchText))+')';
    end;
  if (fsBarcode in FSearchTypes) and (aObjects.GetBarcodeFieldName <> '') then
    aFilter += ' OR ('+Data.ProcessTerm(CastText(Data,aObjects.GetBarcodeFieldName)+' = '+EncodeValue(Data,SearchText))+')';
  if (fsCommission in FSearchTypes) and (aObjects.GetCommissionFieldName <> '') then
    aFilter += ' OR ('+Data.ProcessTerm(EncodeField(Data,aObjects.GetCommissionFieldName)+' = '+EncodeValue(Data,SearchText))+')';
  aFilter := copy(aFilter,pos(' ',aFilter)+1,length(aFilter));
  aFilter := copy(aFilter,pos(' ',aFilter)+1,length(aFilter));
  if aFilter <> '' then
    begin
      aObjects.Filter(aFilter,FMaxResults);
      while not aObjects.EOF do
        begin
          if Data.DataSetFromLink(aObjects.FieldByName('LINK').AsString,aDSClass) then
            begin
              for i := 0 to length(FSearchLocations)-1 do
                begin
                  aLinkDs := TBaseDBList(aDSClass.Create(nil));
                  if FSearchLocations[i] = aLinkDs.Caption then
                    begin
                      if aLinkDs is TBaseDbList then
                        begin
                          aLinkDs.SelectFromLink(aObjects.FieldByName('LINK').AsString);
                          aLinkDs.Open;
                          if aLinkDs.Count>0 then
                            begin
                              if Assigned(aLinkDs.Status) then
                                begin
                                  aActive := Data.States.DataSet.Locate('STATUS;TYPE',VarArrayOf([aLinkDs.Status.AsString,aLinkDs.Typ]),[loCaseInsensitive]);
                                  if aActive then
                                    aActive := aActive and (Data.States.FieldByName('ACTIVE').AsString='Y');
                                  FItemFound(aLinkDs.Number.AsString,aLinkDs.Text.AsString,aLinkDs.Status.AsString,aActive,aObjects.FieldByName('LINK').AsString,3000);
                                  FNewFound:=True;
                                end
                              else
                                begin
                                  FItemFound(aLinkDs.Number.AsString,aLinkDs.Text.AsString,'',True,aObjects.FieldByName('LINK').AsString,3000);
                                  FNewFound:=True;
                                end;
                            end;
                        end;
                    end;
                  aLinkDs.Free;
                end;
            end;
          aObjects.Next;
        end;
    end;
  aObjects.Free;
  FActive := False;
  if Assigned(FFullEndSearch) then FFullEndSearch(Self);
  with BaseApplication as IBaseApplication do
    Debug('Search:EndAllObjectsSearch');
end;

procedure TSearch.DoStartHistorySearch(SearchText: string);
var
  aDs: TDataSet;
  aDSClass: TBaseDBDatasetClass;
  aLinkDs: TBaseDbList;
  i: Integer;
  aActive: Boolean;

  function ItemValid(aLink : string) : Boolean;
  begin
    Result := True;
  end;

begin
  if not Assigned(FItemFound) then exit;
  with BaseApplication as IBaseApplication do
    Debug('Search:StartHistorySearch');
  FActive:=True;
  if Data.IsSQLDB then
    begin
      aDs := Data.GetNewDataSet('select count(*),'+Data.QuoteField('LINK')+' from '+Data.QuoteField('SEARCHHISTORY')+' where '+Data.QuoteField('TEXT')+' = '+Data.QuoteValue(SearchText)+' group by '+Data.QuoteField('LINK')+' order by count(*) desc');
      aDs.Open;
      Data.SetFilter(Data.States,'',0);
      while not aDs.EOF do
        begin
          if Data.DataSetFromLink(aDs.FieldByName('LINK').AsString,aDSClass) then
            begin
              for i := 0 to length(FSearchLocations)-1 do
                begin
                  aLinkDs := TBaseDBList(aDSClass.Create(nil));
                  if FSearchLocations[i] = aLinkDs.Caption then
                    begin
                      if aLinkDs is TBaseDbList then
                        begin
                          aLinkDs.SelectFromLink(aDs.FieldByName('LINK').AsString);
                          aLinkDs.Open;
                          if aLinkDs.Count>0 then
                            begin
                              if Assigned(aLinkDs.Status) then
                                begin
                                  aActive := Data.States.DataSet.Locate('STATUS;TYPE',VarArrayOf([aLinkDs.Status.AsString,aLinkDs.Typ]),[loCaseInsensitive]);
                                  if aActive then
                                    aActive := aActive and (Data.States.FieldByName('ACTIVE').AsString='Y');
                                  if ItemValid(aDs.FieldByName('LINK').AsString) then
                                    begin
                                      FItemFound(aLinkDs.Number.AsString,aLinkDs.Text.AsString,aLinkDs.Status.AsString,aActive,aDs.FieldByName('LINK').AsString,3000);
                                      FNewFound:=True;
                                    end;
                                end
                              else if ItemValid(aDs.FieldByName('LINK').AsString) then
                                FItemFound(aLinkDs.Number.AsString,aLinkDs.Text.AsString,'',True,aDs.FieldByName('LINK').AsString,3000);
                            end;
                        end;
                    end;
                  aLinkDs.Free;
                end;
            end;
          aDs.Next;
        end;
      aDS.Free;
    end;
  FActive := False;
  if Assigned(FFullEndSearch) then FFullEndSearch(Self);
  with BaseApplication as IBaseApplication do
    Debug('Search:EndHistorySearch');
end;

procedure TSearch.Abort;
begin
  FActive := False;
end;
finalization
  Setlength(SearchAble,0);
end.

