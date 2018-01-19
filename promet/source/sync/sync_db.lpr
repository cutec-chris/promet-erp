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
*******************************************************************************}
program sync_db;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  pcmdprometapp, uData, db, uBaseDBInterface, uBaseApplication,
  uBaseCustomApplication, uBaseDbClasses, uSync, uOrder, uPerson, uMasterdata,
  uMessages,Utils,uminiconvencoding,uBaseDatasetInterfaces,utask,uCalendar,
  uProjects,uDocuments,dateutils,uBaseERPDBClasses,uscheme,uprometscripts;
type

  { TSyncDBApp }

  TSyncDBApp = class(TBaseCustomApplication)
  private
    FTempDataSet : TDataSet;
    FTempNewCounter : Integer;
    FTempDataSetName : string;
    FDest : TBaseDBInterface;
    aGlobalTime : TDateTime;
    aLastRowTime : TDateTime;
    FLog : TStringList;
    FTables : TStringList;
    FAddLog : Boolean;
    aFirstSyncedRow : TDateTime;
    function SyncRow(SyncDB : TSyncDB;SyncTbl : TDataSet;SourceDM,DestDM : TBaseDBModule;SyncOut : Boolean = True) : Boolean;
    function SyncTable(SyncDB: TSyncDB; SourceDM, DestDM: TBaseDBModule;
      SyncCount: Integer=0): Integer;
  protected
    procedure DoRun; override;
    function GetSingleInstance : Boolean; override;
    procedure Log(aType: string; aMsg: string);override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;
resourcestring
  strTableNotExists          = 'Tabelle "%s" existiert nicht, übersprungen !';
  strSyncOffsetdontMatch     = 'Synchronisations Offset passt nicht bei "%s" !';
  strRowSyncFailed           = 'Zeile mit der SQL_ID "%s" nicht synchronisierbar Fehler: "%s", übersprungen ! von %s';
  strSyncTable               = 'Synchronisiere %d Zeilen in Richtung %s Tabelle: %s';
  strRowDeleteFailed         = 'Zeile aus "%s" mit der SQL_ID "%s" nicht löschbar Fehler: "%s", übersprungen !';
  strRowDeleteNotFound       = 'Zeile aus "%s" mit der SQL_ID "%s" nicht löschbar Fehler: Die Zeile wurde nicht gefunden !';
  strSyncingMandant          = 'Syncronisiere "%s"';
  strSyncStamp               = 'Stamp: %s, %s, %s';
function TSyncDBApp.SyncRow(SyncDB: TSyncDB; SyncTbl: TDataSet; SourceDM,
  DestDM: TBaseDBModule;SyncOut : Boolean = True) : Boolean;
var
  aSource: TDataSet;
  aDest: TDataSet;
  i: Integer;
  aFieldName: String;
  aDel: TDataSet = nil;
  aDelTable: String;
  DoPost: Boolean = False;
  aSyncError: TSyncItems;
  tmp: String;
  aStream: TStream;
begin
  try
  Result := True;
  if Assigned(FTempDataSet) and (FTempDataSetName = SyncDB.Tables.DataSet.FieldByName('NAME').AsString) and FTempDataSet.Locate('SQL_ID',SyncTbl.FieldByName('SQL_ID').AsVariant,[]) and BaseApplication.HasOption('d','dontupdate') then
    exit
  else if BaseApplication.HasOption('d','dontupdate') then
    begin
      FreeAndNil(FTempDataSet);
      if FTempDataSetName = SyncDB.Tables.DataSet.FieldByName('NAME').AsString then
        inc(FTempNewCounter)
      else FTempNewCounter := 0;
      if FTempNewCounter < 10 then
        begin
          FTempDataSet := DestDM.GetNewDataSet('select '+DestDM.QuoteField('SQL_ID')+' from '+DestDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+' where '+DestDM.QuoteField('SQL_ID')+'>='+DestDM.QuoteValue(SyncTbl.FieldByName('SQL_ID').AsString));
          FTempDataSet.Open;
          FTempDataSetName := SyncDB.Tables.DataSet.FieldByName('NAME').AsString;
        end;
    end;
  if SyncTbl.FieldCount>2 then
    aSource := SyncTbl
  else
    aSource := SourceDM.GetNewDataSet('select * from '+SourceDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+' where '+SourceDM.QuoteField('SQL_ID')+'='+SourceDM.QuoteValue(SyncTbl.FieldByName('SQL_ID').AsString));
  aDest := DestDM.GetNewDataSet('select * from '+DestDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+' where '+DestDM.QuoteField('SQL_ID')+'='+DestDM.QuoteValue(SyncTbl.FieldByName('SQL_ID').AsString),DestDM.MainConnection);
  with aDest as IBaseManageDB do
    UpdateStdFields := False;
  try
    try
      if not aSource.Active then
        aSource.Open;
      aDest.Open;
      if aDest.RecordCount = 0 then
        begin
          aDest.Append;
          DoPost := True;
        end
      else
        begin
          if BaseApplication.HasOption('d','dontupdate') then
            begin
              if FTempNewCounter > 0 then
                begin
                  FTempNewCounter := 0;
                  FreeAndNil(FTempDataSet);
                end;
              FreeAndNil(aSource);
              FreeAndNil(aDest);
              exit;
            end;
          if aDest.FieldByName('TIMESTAMPD').AsDateTime>aSource.FieldByName('TIMESTAMPD').AsDateTime then
            begin
              (BaseApplication as IBaseApplication).Info(Format('Dest is newer than Source, aborting ID:%s',[aSource.FieldByName('SQL_ID').AsString]));
              FreeAndNil(aSource);
              FreeAndNil(aDest);
              exit;
            end;
          aDest.Edit;
          FTempNewCounter := 0;
          FreeAndNil(FTempDataSet);
        end;
      for i := 0 to aDest.FieldCount-1 do
        begin
          aFieldName := aDest.Fields[i].FieldName;
          if (aSource.FieldDefs.IndexOf(aFieldName) > -1)
          and (aDest.FieldDefs.IndexOf(aFieldName) > -1)
          then
            begin
              if (not aSource.FieldByName(aFieldName).IsNull) or (aSource.FieldByName(aFieldName).IsBlob) or (aDest.FieldByName(aFieldName).IsBlob) then
                begin
                  tmp := ConvertEncoding(aSource.FieldByName(aFieldName).AsString,GuessEncoding(aSource.FieldByName(aFieldName).AsString),EncodingUTF8);
                  if (aDest.FieldByName(aFieldName).DataType = ftString)
                  and (aDest.FieldByName(aFieldName).AsVariant <> aSource.FieldByName(aFieldName).AsVariant) then
                    begin
                      aDest.FieldByName(aFieldName).AsString := aSource.FieldByName(aFieldName).AsString;
                      DoPost := True;
                    end
                  else if (aSource.FieldByName(aFieldName).IsBlob) or (aDest.FieldByName(aFieldName).IsBlob) then
                    begin
                      aStream := SourceDM.BlobFieldStream(aSource,aFieldName,SyncDB.Tables.DataSet.FieldByName('NAME').AsString);
                      aStream.Position:=0;
                      DestDM.StreamToBlobField(aStream,aDest,aFieldName,SyncDB.Tables.DataSet.FieldByName('NAME').AsString);
                      aStream.Free;
                    end
                  else if (aDest.FieldByName(aFieldName).AsVariant <> aSource.FieldByName(aFieldName).AsVariant) then
                    begin
                      aDest.FieldByName(aFieldName).AsVariant := aSource.FieldByName(aFieldName).AsVariant;
                      DoPost := True;
                    end;
                end;
            end;
        end;
      if aSource.FieldByName('TIMESTAMPD').AsDateTime > aLastRowTime then
        aLastRowTime:=aSource.FieldByName('TIMESTAMPD').AsDateTime;
      if SyncOut and DoPost then
        if aDest.FieldDefs.IndexOf('TIMESTAMPD') > -1 then
          if not aDest.FieldByName('TIMESTAMPD').IsNull then
            if aDest.FieldByName('TIMESTAMPD').AsDateTime < aFirstSyncedRow then
              aFirstSyncedRow:=aDest.FieldByName('TIMESTAMPD').AsDateTime;
      if aDest.FieldDefs.IndexOf('TIMESTAMPD') > -1 then
        if aDest.FieldByName('TIMESTAMPD').IsNull then
          {$IF FPC_FULLVERSION>20600}
          aDest.FieldByName('TIMESTAMPD').AsDateTime:=LocalTimeToUniversal(Now());
          {$ELSE}
          aDest.FieldByName('TIMESTAMPD').AsDateTime:=Now();
          {$ENDIF}
      if DoPost then
        aDest.Post;
      //TODO-:TimestampD must be not actial Time !!!
    except
      on e : exception do
        begin
          if SyncDB.Tables.DataSet.FieldByName('NAME').AsString = 'DELETEDITEMS' then //Delete Items from DB
            (BaseApplication as IBaseApplication).Warning(Format(strRowSyncFailed,[SyncTbl.FieldByName('SQL_ID').AsString,e.Message,SyncTbl.FieldByName('TIMESTAMPD').AsString]))
          else
            (BaseApplication as IBaseApplication).Error(Format(strRowSyncFailed,[SyncTbl.FieldByName('SQL_ID').AsString,e.Message,SyncTbl.FieldByName('TIMESTAMPD').AsString]));
          aSyncError := TSyncItems.CreateEx(nil,SyncDB.DataModule);
          aSyncError.Insert;
          aSyncError.FieldByName('LOCAL_ID').AsVariant:=SyncTbl.FieldByName('SQL_ID').AsVariant;
          aSyncError.FieldByName('SYNCTYPE').AsString:='sync_db';
          aSyncError.FieldByName('SYNCTABLE').AsString:=SyncDB.Tables.DataSet.FieldByName('NAME').AsString;
          aSyncError.FieldByName('REMOTE_ID').AsString:=SyncTbl.FieldByName('SQL_ID').AsString;
          {$IF FPC_FULLVERSION>20600}
          aSyncError.FieldByName('SYNC_TIME').AsDateTime:=LocalTimeToUniversal(Now());
          {$ELSE}
          aSyncError.FieldByName('SYNC_TIME').AsDateTime:=Now();
          {$ENDIF}
          aSyncError.FieldByName('REMOTE_TIME').AsDateTime:=SyncTbl.FieldByName('TIMESTAMPD').AsDateTime;
          aSyncError.FieldByName('ERROR').AsString:='Y';
          aSyncError.Post;
          aSyncError.Free;
          result := False;
        end;
    end;
    try
      if SyncDB.Tables.DataSet.FieldByName('NAME').AsString = 'DELETEDITEMS' then //Delete Items from DB
        begin
          aDelTable := copy(aSource.FieldByName('LINK').AsString,0,pos('@',aSource.FieldByName('LINK').AsString)-1);
          if DestDM.TableExists(aDelTable) then
            begin
              if pos('.ID',aDelTable) > 0 then
                aDelTable := copy(aDelTable,0,pos('.ID',aDelTable)-1);
              if (aDelTable <> '') and (aDelTable <> 'ACTIVEUSERS') then
                begin
                  aDel := DestDM.GetNewDataSet('select * from '+DestDM.QuoteField(aDelTable)+' where '+DestDM.QuoteField('SQL_ID')+'='+DestDM.QuoteValue(aSource.FieldByName('REF_ID_ID').AsString));
                  adel.Open;
                  if aDel.RecordCount>0 then
                    aDel.Delete;
                end;
            end;
        end;
    except
      on e : exception do
        begin
          if FTables.IndexOf(aDelTable) > -1 then
            begin
              (BaseApplication as IBaseApplication).Info(Format(strRowDeleteFailed,[aDelTable,aSource.FieldByName('REF_ID_ID').AsString,e.Message]));
            end
          else (BaseApplication as IBaseApplication).Info(Format(strRowDeleteFailed,[aDelTable,aSource.FieldByName('REF_ID_ID').AsString,e.Message]));
          aSyncError := TSyncItems.CreateEx(nil,SyncDB.DataModule);
          aSyncError.Insert;
          aSyncError.FieldByName('SYNCTYPE').AsString:='sync_db';
          aSyncError.FieldByName('SYNCTABLE').AsString:=SyncDB.Tables.DataSet.FieldByName('NAME').AsString;
          aSyncError.FieldByName('LOCAL_ID').AsVariant:=aSource.FieldByName('REF_ID_ID').AsVariant;
          aSyncError.FieldByName('REMOTE_ID').AsString:=aSource.FieldByName('REF_ID_ID').AsString;
          {$IF FPC_FULLVERSION>20600}
          aSyncError.FieldByName('SYNC_TIME').AsDateTime:=LocalTimeToUniversal(Now());
          {$ELSE}
          aSyncError.FieldByName('SYNC_TIME').AsDateTime:=Now();
          {$ENDIF}
          aSyncError.FieldByName('ERROR').AsString:='Y';
          aSyncError.Post;
          aSyncError.Free;
        end;
    end;
  finally
    if aSource<>SyncTbl then
      FreeAndNil(aSource);
    FreeAndNil(aDest);
    FreeAndNil(aDel);
  end;

  except
    on e : Exception do
    begin
      Result := False;
      (BaseApplication as IBaseApplication).Info('Exception occoured '+e.Message);
    end;
  end;
end;
function TSyncDBApp.SyncTable(SyncDB: TSyncDB; SourceDM, DestDM: TBaseDBModule;SyncCount : Integer = 0): Integer;
function BuildFilter(aSourceDM,aDestDM : TBaseDBModule;aTime : TDateTime = 0) : string;
var
  aFilter: String;
begin
  if GetOptionValue('sql_id')='' then
    begin
      if SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsString = '' then
        aFilter := ''
      else
        begin
          if aTime = 0 then
            aTime := SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime;
          if aTime > SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime then
            aTime := SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime;
          if BaseApplication.HasOption('w','wholeday') then
            begin
              aFilter := '(('+aSourceDM.QuoteField('TIMESTAMPD')+'=';
              aFilter := aFilter+aSourceDM.DateToFilter(aTime);
              aFilter := aFilter+')) or ('+aSourceDM.QuoteField('TIMESTAMPD')+'>';
              aFilter := aFilter+aSourceDM.DateToFilter(aTime);
              aFilter := aFilter+')';
            end
          else
            begin
              aFilter := '('+aSourceDM.QuoteField('TIMESTAMPD')+'>';
              aFilter := aFilter+aSourceDM.DateTimeToFilter(aTime);
              aFilter := aFilter+')';
            end;
        end;
    end
  else
    begin
      aFilter := '('+aSourceDM.QuoteField('SQL_ID')+'='+Data.QuoteValue(GetOptionValue('sql_id'))+')'
    end;
  Result := aFilter;
end;
var
  aFilter: String;
  aSyncOut,aSyncIn: TDataSet;
  aSQL: String;
  TimeSet: Boolean = False;
  aTableName: String;
  RestoreTime: Boolean = False;
  SetTime : Boolean = False;
  aOldTime: TDateTime;
  aSyncStamps: TSyncStamps;
  aSyncTime: TDateTime;
  bFilter: String;
  bFirstSyncedRow: TDateTime;
  aLastSetTime: TDateTime;
  aSyncOutTime: TDateTime;
  aSQLF: String;
  aTable: String;
  aCRT: TBaseDBDatasetClass;
  tCRT: TBaseDBDataset;

  procedure UpdateTime(DoSetIt : Boolean = True);
  begin
    if (not RestoreTime) and SetTime then
      begin
        SyncDB.Tables.DataSet.Edit;
        if (aLastRowTime>0) and (aOldTime<>aLastRowTime) then
          SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime := aLastRowTime
        else
          SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime := aGlobalTime;
        SyncDB.Tables.DataSet.Post;
      end;
  end;

begin
  Result := 0;
  aTable := SyncDB.Tables.DataSet.FieldByName('NAME').AsString;
  if not ((SyncDB.Tables.DataSet.FieldByName('ACTIVEOUT').AsString = 'Y')
       or (SyncDB.Tables.DataSet.FieldByName('ACTIVE').AsString = 'Y')) then exit;
  if DestDM.DataSetFromName(aTable,aCRT) then
    begin
      tCRT := aCRT.CreateEx(nil,DestDM);
      tCRT.CreateTable;
      tCRT.Open;
      tCRT.Free;
    end;
  if (SyncDB.Tables.DataSet.FieldByName('LOCKEDBY').AsString='') or (SyncDB.Tables.DataSet.FieldByName('LOCKEDAT').AsDateTime<({$IF FPC_FULLVERSION>20600}LocalTimeToUniversal{$ENDIF}(Now())-1)) then
    begin
      SyncDB.Tables.DataSet.Edit;
      SyncDB.Tables.DataSet.FieldByName('LOCKEDBY').AsString:=Utils.GetSystemName;
      SyncDB.Tables.DataSet.FieldByName('LOCKEDAT').AsDateTime:={$IF FPC_FULLVERSION>20600}LocalTimeToUniversal{$ENDIF}(Now());
      SyncDB.Tables.DataSet.Post;
      aFirstSyncedRow:={$IF FPC_FULLVERSION>20600}LocalTimeToUniversal{$ENDIF}(Now());
      aLastSetTime := {$IF FPC_FULLVERSION>20600}LocalTimeToUniversal{$ENDIF}(Now());
      bFirstSyncedRow:=aFirstSyncedRow;
      aTableName := SyncDB.Tables.DataSet.FieldByName('NAME').AsString;
      if (not SourceDM.TableExists(aTableName))
      or (not DestDM.TableExists(aTableName)) then
        begin
          if (SyncDB.Tables.DataSet.FieldByName('ACTIVEOUT').AsString = 'Y')
          or (SyncDB.Tables.DataSet.FieldByName('ACTIVE').AsString = 'Y')
          then
            (BaseApplication as IBaseApplication).Info(Format(strTableNotExists,[aTable]));
          SyncDB.Tables.DataSet.Edit;
          SyncDB.Tables.DataSet.FieldByName('LOCKEDBY').Clear;
          SyncDB.Tables.DataSet.FieldByName('LOCKEDAT').Clear;
          SyncDB.Tables.DataSet.Post;
          exit;
        end;
      aSyncStamps := TSyncStamps.CreateEx(Self,DestDM);
      try
        aFilter := BuildFilter(SourceDM,DestDM);
        bFilter := Data.QuoteField('NAME')+'='+Data.QuoteValue(aTableName);
        if aFilter <> '' then
          bFilter := bFilter+' AND '+aFilter;
        aSyncTime := SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime;
        if SyncDB.Tables.DataSet.FieldByName('ACTIVEOUT').AsString = 'Y' then
          begin
            with aSyncStamps.DataSet as IBaseDbFilter do
              Filter := bFilter;
            aSyncStamps.Open;
            while not aSyncStamps.DataSet.EOF do
              begin
                if aSyncStamps.FieldByName('NAME').AsString=aTableName then
                  if aSyncStamps.DataSet.FieldByName('TIMESTAMPD').AsDateTime < aSyncTime then
                    begin
                      aSyncTime:=aSyncStamps.FieldByName('LTIMESTAMP').AsDateTime;
                      (BaseApplication as IBaseApplication).Info(Format(strSyncStamp,[aSyncStamps.FieldByName('FROM').AsString,aSyncStamps.FieldByName('NAME').AsString,aSyncStamps.FieldByName('LTIMESTAMP').AsString]));
                    end;
                aSyncStamps.DataSet.Next;
              end;
          end;
        aFilter := BuildFilter(SourceDM,DestDM,aSyncTime);
        //First collect the rows to sync (if we do this afterwards we sync everything double)
        if SyncDB.Tables.DataSet.FieldByName('ACTIVEOUT').AsString = 'Y' then //Out
          begin
            if trim(SyncDB.Tables.DataSet.FieldByName('FILTEROUT').AsString) <> '' then
              begin
                if trim(aFilter) <> '' then
                  aFilter := '('+aFilter+') and ('+SyncDB.Tables.DataSet.FieldByName('FILTEROUT').AsString+')'
                else
                  aFilter := '('+SyncDB.Tables.DataSet.FieldByName('FILTEROUT').AsString+')';
              end;
            if (pos('insert',lowercase(aFilter)) > 0)
            or (pos('update',lowercase(aFilter)) > 0)
            or (pos('delete',lowercase(aFilter)) > 0) then exit;

            aSQLF := ' order by '+Data.QuoteField('TIMESTAMPD')+' asc';
            if aFilter <> '' then
              aSQLF := ' where '+aFilter+' order by '+Data.QuoteField('TIMESTAMPD')+' asc';
            if SyncCount=0 then
              aSQL := 'select '+SourceDM.QuoteField('SQL_ID')+','+SourceDM.QuoteField('TIMESTAMPD')+' from '+SourceDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+aSQLF
            else
              begin
                if SourceDM.LimitAfterSelect then
                  aSQL := 'select '+Format(SourceDM.LimitSTMT,[IntToStr(SyncCount)])+' * from '+SourceDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+aSQLF
                else
                  aSQL := 'select * from '+SourceDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+aSQLF+' '+Format(DestDM.LimitSTMT,[IntToStr(SyncCount)]);
              end;
            aSyncOut := SourceDM.GetNewDataSet(aSQL);
            aSyncOut.Open;
          end;
        aFilter := BuildFilter(DestDM,SourceDM,aSyncTime);
        if SyncDB.Tables.DataSet.FieldByName('ACTIVE').AsString = 'Y' then //In
          begin
            if trim(SyncDB.Tables.DataSet.FieldByName('FILTERIN').AsString) <> '' then
              begin
                if trim(aFilter) <> '' then
                  aFilter := '('+aFilter+') and ('+SyncDB.Tables.DataSet.FieldByName('FILTERIN').AsString+')'
                else
                  aFilter := '('+SyncDB.Tables.DataSet.FieldByName('FILTERIN').AsString+')';
              end;
            if (pos('insert',lowercase(aFilter)) > 0)
            or (pos('update',lowercase(aFilter)) > 0)
            or (pos('delete',lowercase(aFilter)) > 0) then exit;

            aSQLF:=' order by '+Data.QuoteField('TIMESTAMPD')+' asc';
            if aFilter <> '' then
              aSQLF := ' where '+aFilter+' order by '+Data.QuoteField('TIMESTAMPD')+' asc';
            if SyncCount=0 then
              begin
                aSQL := 'select '+DestDM.QuoteField('SQL_ID')+','+DestDM.QuoteField('TIMESTAMPD')+' from '+DestDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+aSQLF;
              end
            else
              begin
                if DestDM.LimitAfterSelect then
                  aSQL := 'select '+Format(DestDM.LimitSTMT,[IntToStr(SyncCount)])+' * from '+DestDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+aSQLF
                else
                  aSQL := 'select * from '+DestDM.QuoteField(SyncDB.Tables.DataSet.FieldByName('NAME').AsString)+aSQLF+' '+Format(DestDM.LimitSTMT,[IntToStr(SyncCount)]);
              end;
            aSyncIn := DestDM.GetNewDataSet(aSQL);
            aSyncIn.Open;
          end;
        aSyncOutTime:=aGlobalTime;

        //Then sync them
        if SyncDB.Tables.DataSet.FieldByName('ACTIVEOUT').AsString = 'Y' then //Out
          begin
            aOldTime := SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime;
            aLastRowTime:=aOldTime;
            if aSyncOut.RecordCount > 0 then
              begin
                TimeSet := True;
                SetTime := True;
                (BaseApplication as IBaseApplication).Info(Format(strSyncTable,[aSyncOut.RecordCount,'<',SyncDB.Tables.DataSet.FieldByName('NAME').AsString]));
                (BaseApplication as IBaseApplication).Info(aFilter);
              end;
            try
              if SyncCount>0 then //Use Transactions only when Partially syncing We diont want to lock for an long time
                DestDM.StartTransaction(DestDM.MainConnection);
              while not aSyncOut.EOF do
                begin
                  try
                    SyncRow(SyncDB,aSyncOut,SourceDM,DestDM,True);
                    inc(Result);
                  except
                    begin
                      dec(Result);
                      //RestoreTime := True;
                    end;
                  end;
                  aSyncOut.Next;
                end;
              aSyncOut.Destroy;
              aSyncOutTime := aLastRowTime;
              if SyncCount>0 then
                DestDM.CommitTransaction(DestDM.MainConnection);
            except
              on e : Exception do
                begin
                  if SyncCount>0 then
                    DestDM.RollbackTransaction(DestDM.MainConnection);
                end;
            end;
          end;
        aFilter := BuildFilter(DestDM,SourceDM,aSyncTime);
        if SyncDB.Tables.DataSet.FieldByName('ACTIVE').AsString = 'Y' then //In
          begin
            aOldTime := SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsDateTime;
            aLastRowTime:=aOldTime;
            if (aSyncIn.RecordCount > 0) then
              begin
                SetTime := True;
                (BaseApplication as IBaseApplication).Info(Format(strSyncTable,[aSyncIn.RecordCount,'>',SyncDB.Tables.DataSet.FieldByName('NAME').AsString]));
              end;
            try
              if SyncCount>0 then //Use Transactions only when Partially syncing We diont want to lock for an long time
                SourceDM.StartTransaction(SourceDM.MainConnection);
              while not aSyncIn.EOF do
                begin
                  try
                    SyncRow(SyncDB,aSyncIn,DestDM,SourceDM,False);
                    inc(Result);
                  except
                    begin
                      dec(Result);
                      //RestoreTime:=True;
                    end;
                  end;
                  aSyncIn.Next;
                end;
              FreeAndNil(FTempDataSet);
              FTempNewCounter := 0;
              aSyncIn.Destroy;
              if aSyncOutTime<aLastRowTime then
                aLastRowTime:=aSyncOutTime;
              if SyncCount>0 then //Use Transactions only when Partially syncing We diont want to lock for an long time
                SourceDM.CommitTransaction(SourceDM.MainConnection);
            except
              on e : Exception do
                begin
                  if SyncCount>0 then //Use Transactions only when Partially syncing We diont want to lock for an long time
                    SourceDM.RollbackTransaction(SourceDM.MainConnection);
                end;
            end;
          end;
        UpdateTime;
        if (aFirstSyncedRow<bFirstSyncedRow) and (not RestoreTime) then
          begin
            aSyncStamps.Append;
            aSyncStamps.DataSet.FieldByName('FROM').AsString:=SyncDB.DataSet.FieldByName('NAME').AsString;
            aSyncStamps.DataSet.FieldByName('NAME').AsString:=aTableName;
            aSyncStamps.DataSet.FieldByName('LTIMESTAMP').AsDateTime := aFirstSyncedRow;
            aSyncStamps.DataSet.Post;
          end;
      finally
        aSyncStamps.Free;
        SyncDB.Tables.DataSet.Edit;
        SyncDB.Tables.DataSet.FieldByName('LOCKEDBY').Clear;
        SyncDB.Tables.DataSet.FieldByName('LOCKEDAT').Clear;
        SyncDB.Tables.DataSet.Post;
      end;
    end
  else Info('Table "'+SyncDB.Tables.DataSet.FieldByName('NAME').AsString+'" ist gesperrt von anderem Prozess')
end;
function TSyncDBApp.GetSingleInstance: Boolean;
begin
  Result := False;
end;
procedure TSyncDBApp.Log(aType: string; aMsg: string);
begin
  inherited Log(aType, aMsg);
  if aType <> 'INFO' then FAddLog:=True;
  FLog.Add(aType+':'+aMsg);
end;
procedure TSyncDBApp.DoRun;
var
  SyncDB: TSyncDB;
  aSyncOffs: Integer;
  aTable: TBaseDbdataSet;
  y: word;
  m: word;
  d: word;
  h: word;
  mm: word;
  s: word;
  ss: word;
  LoggedIn: Boolean;
  aMessage: TMessage;
  aRec: db.LargeInt;
  aSyncError: TSyncItems;
  SyncedTables: Integer;
  FSyncedCount: Integer;
  FOldTime: String;
  SyncCount, aSyncCount, FOldSyncCount: Integer;
  BlockSizeReached: Boolean;
  procedure DoCreateTable(aTableC : TClass);
  var
    aTableName: string;
    aTable : TBaseDBDataset;
  begin
    try
      aTable := TBaseDbDataSetClass(aTableC).CreateEx(nil,FDest.GetDB);
      with aTable.DataSet as IBaseManageDB do
        aTableName := TableName;
      with aTable.DataSet as IBaseDbFilter do
        Limit := 1;
      aTable.CreateTable;
      aTable.Open;
    except
    end;
  end;
begin
  FLog := TStringList.Create;
  FTables := TStringList.Create;
  {$IF FPC_FULLVERSION>20600}
  aGlobalTime := LocalTimeToUniversal(Now());
  {$ELSE}
  aGlobalTime := Now();
  {$ENDIF}
  FTempDataSet := nil;
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  Info('sync_db starting...');
  Info('Currentdir:'+GetCurrentDir);
  with BaseApplication,BaseApplication as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
      if not HasOption('m','mandant') then
        raise Exception.Create(strMandantnotSelected);
      if not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user')) then
        raise Exception.Create(strLoginFailed+' '+LastError);
      uData.Data := Data;
    end;
  Info('login ok.');
  DecodeDate(Now(),y,m,d);
  DecodeTime(Now(),h,mm,s,ss);
  SyncDB := TSyncDB.CreateEx(Self,Data);
  SyncDB.CreateTable;
  SyncDB.Open;
  aSyncError := TSyncItems.CreateEx(nil,SyncDB.DataModule);
  aSyncError.CreateTable;
  aSyncError.Free;
  while not SyncDB.DataSet.EOF do
    begin
      FLog.Clear;
      if (HasOption('db') and (GetOptionValue('db')=SyncDB.DataSet.FieldByName('NAME').AsString)) or (not HasOption('db')) then
        begin
          aRec := SyncDb.GetBookmark;
          SyncDB.DataSet.Refresh;
          SyncDB.GotoBookmark(aRec);
          if (SyncDB.DataSet.FieldByName('ACTIVE').AsString <> 'N') or (GetOptionValue('db')=SyncDB.DataSet.FieldByName('NAME').AsString) then
            begin
              Info('starting:'+SyncDB.DataSet.FieldByName('NAME').AsString);
              try
              FAddLog:=False;
              if not SyncDB.CanEdit then SyncDB.DataSet.Edit;
              SyncDB.DataSet.FieldByName('INPROGRESS').AsString := 'Y';
              SyncDB.DataSet.Post;
              with BaseApplication as IBaseDbInterface do
                begin
    //              if Mandants.FieldByName('NAME').AsString <> BaseApplication.GetOptionValue('d','destination') then
    //                if Mandants.Locate('NAME',BaseApplication.GetOptionValue('d','destination'),[])
                    if (not SyncDB.DataSet.FieldByName('PROPERTIES').IsNull) then
                      begin
                        FDest := TBaseDBInterface.Create;
                        FDest.SetOwner(BaseApplication);
                        if not FDest.LoadMandants then
                          raise Exception.Create(strFailedtoLoadMandants);
                        with FDest as IBaseDBInterface do
                          begin
                            if not SyncDB.DataSet.FieldByName('PROPERTIES').IsNull then
                              begin
                                LoggedIn := OpenMandant(copy(SyncDB.DataSet.FieldByName('PROPERTIES').AsString,0,pos(':',SyncDB.DataSet.FieldByName('PROPERTIES').AsString)-1),
                                                   copy(SyncDB.DataSet.FieldByName('PROPERTIES').AsString,pos(':',SyncDB.DataSet.FieldByName('PROPERTIES').AsString)+1,length(SyncDB.DataSet.FieldByName('PROPERTIES').AsString)));
                              end
                            else
                              begin
                                LoggedIn :=  OpenMandant;
                              end;
                            if LoggedIn then
                              begin
                                Info(Format(strSyncingMandant,[SyncDB.DataSet.FieldByName('NAME').AsString]));
                                aSyncOffs := FDest.GetDB.SyncOffset;
                                if SyncDB.DataSet.FieldByName('SYNCOFFS').AsInteger = aSyncOffs then
                                  begin
                                    DoCreateTable(TDeletedItems);
                                    SyncDB.Tables.Open;
                                    if SyncDB.Tables.DataSet.Locate('NAME','USERFIELDDEFS',[loCaseInSensitive]) then
                                      begin
                                        DoCreateTable(TUserfielddefs);
                                        SyncTable(SyncDB,uData.Data,FDest.GetDB);
                                      end;
                                    SyncedTables := (SyncDB.Tables.Count*4);
                                    SyncCount := 0;
                                    aSyncCount := 0;
                                    BlockSizeReached := False;
                                    SyncCount := 0;
                                    aSyncCount := StrToIntDef(GetOptionValue('syncblocks'),0);
                                    SyncedTables:=0;
                                    SyncDB.Tables.DataSet.First;
                                    while not SyncDB.Tables.DataSet.EOF do
                                      begin
                                        if (GetOptionValue('table')='') or (GetOptionValue('table')=SyncDB.Tables.DataSet.FieldByName('NAME').AsString) then
                                          begin
                                            FTables.Add(SyncDB.Tables.DataSet.FieldByName('NAME').AsString);
                                            try
                                              FSyncedCount:=2;
                                              FOldSyncCount:=0;
                                              if SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsString='' then
                                                FOldTime:='a';
                                              while (FOldTime <> SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsString) and (FOldSyncCount<>FSyncedCount) do
                                                begin
                                                  FOldTime := SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsString;
                                                  FOldSyncCount := FSyncedCount;
                                                  FSyncedCount := SyncTable(SyncDB,uData.Data,FDest.GetDB,aSyncCount);
                                                  inc(SyncedTables,FSyncedCount);
                                                end;
                                              if (FOldTime = SyncDB.Tables.DataSet.FieldByName('LTIMESTAMP').AsString) then
                                                begin
                                                  FSyncedCount := SyncTable(SyncDB,uData.Data,FDest.GetDB);
                                                  inc(SyncedTables,FSyncedCount);
                                                end;
                                            except
                                              on e : Exception do
                                                Error(e.Message);
                                            end;
                                          end;
                                        SyncDB.Tables.DataSet.Next;
                                      end;
                                  end;
                                DBLogout;
                              end
                            else
                              begin
                                (BaseApplication as IBaseApplication).Info(strLoginFailed);
                              end;
                          end;
                      end;
                  if FAddLog then
                    begin
                      aMessage := TMessage.CreateEx(Self,Data);
                      aMessage.CreateTable;
                      aMessage.Insert;
                      aMessage.DataSet.FieldByName('SQL_ID').AsVariant := Data.GetUniID;
                      aMessage.DataSet.FieldByName('SUBJECT').AsString:='Synclog '+DateTimeToStr(Now());
                      aMessage.DataSet.FieldByName('TREEENTRY').AsInteger:=TREE_ID_LOG_MESSAGES;
                      aMessage.DataSet.FieldByName('USER').AsString := Data.Users.DataSet.FieldByName('ACCOUNTNO').AsString;
                      aMessage.DataSet.FieldByName('MSG_ID').AsVariant :=  aMessage.DataSet.FieldByName('SQL_ID').AsVariant;
                      aMessage.DataSet.FieldByName('TYPE').AsString := 'LOG';
                      aMessage.DataSet.FieldByName('SENDER').AsString := 'SyncDB';
                      aMessage.DataSet.FieldByName('SENDDATE').AsDateTime := Now();
                      aMessage.DataSet.FieldByName('READ').AsString := 'N';
                      aMessage.DataSet.Post;
                      aMessage.DataSet.FieldByName('DATATYP').AsString:='PLAIN';
                      aMessage.DataSet.FieldByName('DATA').AsString:=FLog.Text;
                      aMessage.Free;
                      FLog.Clear;
                    end;
                end;
              finally
                SyncDB.Edit;
                SyncDB.DataSet.FieldByName('INPROGRESS').AsString := 'N';
                SyncDB.DataSet.Post;
              end;
              Info(SyncDB.FieldByName('NAME').AsString+' sync done.');
            end
          else Info('ignoring:'+SyncDB.FieldByName('NAME').AsString+' (inactive or overrode with --db)');
        end;
      SyncDB.DataSet.Next;
    end;
  FreeAndNil(FTempDataSet);
  SyncDB.Destroy;
  FLog.Free;
  FTables.Free;
  BaseApplication.Terminate;
end;
constructor TSyncDBApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;
var
  Application: TSyncDBApp;
begin
  Application:=TSyncDBApp.Create(nil);
  Application.Run;
  Application.Free;
end.

