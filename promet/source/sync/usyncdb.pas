//Optionen:
//--wholeday (-w) setzt filter für gesamten tag fall in DB noch alte TimestampD Werte enthalten sind
//--dontupdate updatet vorhandene datensätze nicht
unit usyncdb;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, uData, Forms, eventlog, db, uBaseDBInterface,
  uBaseDBClasses, uSync,uIntfStrConsts;
type
  TdmSync = class(TDataModule)
    EventLog: TEventLog;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    FTempDataSet : TDataSet;
    FTempNewCounter : Integer;
    FTempDataSetName : string;
    FDest : TBaseDBInterface;
    aGlobalTime : TDateTime;
    procedure SyncRow(SyncDB : TSyncDB;SyncTbl : TDataSet;SourceDM,DestDM : TBaseDBModule);
    procedure SyncTable(SyncDB : TSyncDB;SourceDM,DestDM : TBaseDBModule);
  public
    { public declarations }
  end; 
var
  dmSync: TdmSync;
implementation
{$R *.lfm}
uses uOrder,uMasterdata,uPerson,lconvencoding,uBaseApplication;
resourcestring
  strTableNotExists          = 'Tabelle "%s" existiert nicht, übersprungen !';
  strSyncOffsetdontMatch     = 'Synchronisations Offset passt nicht bei "%s" !';
  strRowSyncFailed           = 'Zeile mit der SQL_ID "%s" nicht synchronisierbar Fehler: "%s", übersprungen !';
  strSyncTable               = 'Synchronisiere %d Zeilen in Richtung %s Tabelle: %s';
  strRowDeleteFailed         = 'Zeile aus "%s" mit der SQL_ID "%s" nicht löschbar Fehler: "%s", übersprungen !';
  strRowDeleteNotFound       = 'Zeile aus "%s" mit der SQL_ID "%s" nicht löschbar Fehler: Die Zeile wurde nicht gefunden !';
procedure TdmSync.SyncRow(SyncDB : TSyncDB;SyncTbl: TDataSet; SourceDM, DestDM: TBaseDBModule);
var
  aSource: TDataSet;
  aDest: TDataSet;
  i: Integer;
  aFieldName: String;
  aDel: TDataSet = nil;
  aDelTable: String;
  DoPost: Boolean = False;
begin
  if Assigned(FTempDataSet) and (FTempDataSetName = SyncDB.Tables.FieldByName('NAME').AsString) and FTempDataSet.Locate('SQL_ID',SyncTbl.FieldByName('SQL_ID').AsVariant,[]) and Application.HasOption('d','dontupdate') then
    exit
  else if Application.HasOption('d','dontupdate') then
    begin
      FreeAndNil(FTempDataSet);
      if FTempDataSetName = SyncDB.Tables.FieldByName('NAME').AsString then
        inc(FTempNewCounter)
      else FTempNewCounter := 0;
      if FTempNewCounter < 10 then
        begin
          FTempDataSet := DestDM.GetNewDataSet('select '+DestDM.QuoteField('SQL_ID')+' from '+DestDM.QuoteField(SyncDB.Tables.FieldByName('NAME').AsString)+' where '+DestDM.QuoteField('SQL_ID')+'>='+DestDM.QuoteValue(SyncTbl.FieldByName('SQL_ID').AsString));
          FTempDataSet.Open;
          FTempDataSetName := SyncDB.Tables.FieldByName('NAME').AsString;
        end;
    end;
  aSource := SourceDM.GetNewDataSet('select * from '+SourceDM.QuoteField(SyncDB.Tables.FieldByName('NAME').AsString)+' where '+SourceDM.QuoteField('SQL_ID')+'='+SourceDM.QuoteValue(SyncTbl.FieldByName('SQL_ID').AsString));
  aDest := DestDM.GetNewDataSet('select * from '+DestDM.QuoteField(SyncDB.Tables.FieldByName('NAME').AsString)+' where '+DestDM.QuoteField('SQL_ID')+'='+DestDM.QuoteValue(SyncTbl.FieldByName('SQL_ID').AsString));
  with aDest as IBaseManageDB do
    UpdateStdFields := False;
  try
    try
      aSource.Open;
      aDest.Open;
      if aDest.RecordCount = 0 then
        begin
          aDest.Append;
          DoPost := True;
        end
      else
        begin
          if Application.HasOption('d','dontupdate') then
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
              if (not aSource.FieldByName(aFieldName).IsNull) then
                begin
                  if (aDest.FieldByName(aFieldName).DataType = ftString)
                  and (aDest.FieldByName(aFieldName).AsString <> ConvertEncoding(aSource.FieldByName(aFieldName).AsString,GuessEncoding(aSource.FieldByName(aFieldName).AsString),EncodingUTF8)) then
                    begin
                      aDest.FieldByName(aFieldName).AsString := ConvertEncoding(aSource.FieldByName(aFieldName).AsString,GuessEncoding(aSource.FieldByName(aFieldName).AsString),EncodingUTF8);
                      DoPost := True;
                    end
                  else if (aDest.FieldByName(aFieldName).AsVariant <> aSource.FieldByName(aFieldName).AsVariant) then
                    begin
                      aDest.FieldByName(aFieldName).AsVariant := aSource.FieldByName(aFieldName).AsVariant;
                      DoPost := True;
                    end;
                end;
            end;
        end;
      if aDest.FieldDefs.IndexOf('TIMESTAMPD') > -1 then
        if aDest.FieldByName('TIMESTAMPD').IsNull then
          aDest.FieldByName('TIMESTAMPD').AsDateTime:=Now();
      if DoPost then
        aDest.Post;
      //TODO-:TimestampD must be not actial Time !!!
    except
      on e : exception do
        EventLog.Error(Format(strRowSyncFailed,[SyncTbl.FieldByName('SQL_ID').AsString,e.Message]));
    end;
    try
      if SourceDM.QuoteField(SyncDB.Tables.FieldByName('NAME').AsString) = 'DELETEDITEMS' then //Delete Items from DB
        begin
          aDelTable := copy(aSource.FieldByName('LINK').AsString,0,pos('@',aSource.FieldByName('LINK').AsString)-1);
          if aDelTable <> '' then
            begin
              aDel := DestDM.GetNewDataSet('select * from '+DestDM.QuoteField(aDelTable)+' where '+DestDM.QuoteField('SQL_ID')+'='+DestDM.QuoteValue(aSource.FieldByName('REF_ID_ID').AsString));
              adel.Open;
              if aDel.RecordCount>0 then
                aDel.Delete
              else
                EventLog.Error(Format(strRowDeleteNotFound,[aDelTable,aSource.FieldByName('REF_ID_ID').AsString]));
            end;
        end;
    except
      on e : exception do
        EventLog.Error(Format(strRowDeleteFailed,[aDelTable,aSource.FieldByName('REF_ID_ID').AsString,e.Message]));
    end;
  finally
    FreeAndNil(aSource);
    FreeAndNil(aDest);
    FreeAndNil(aDel);
  end;
end;
procedure TdmSync.SyncTable(SyncDB : TSyncDB;SourceDM,DestDM : TBaseDBModule);
  function BuildFilter(SourceDM,DestDM : TBaseDBModule) : string;
  var
    aFilter: String;
  begin
    if SyncDB.Tables.FieldByName('LTIMESTAMP').AsString = '' then
      aFilter := ''
    else
      begin
        if Application.HasOption('w','wholeday') then
          begin
            aFilter := '(('+SourceDM.QuoteField('TIMESTAMPD')+'=';
            aFilter := aFilter+SourceDM.DateToFilter(SyncDB.Tables.FieldByName('LTIMESTAMP').AsDateTime);
            aFilter := aFilter+')) or ('+SourceDM.QuoteField('TIMESTAMPD')+'>';
            aFilter := aFilter+SourceDM.DateToFilter(SyncDB.Tables.FieldByName('LTIMESTAMP').AsDateTime);
            aFilter := aFilter+')';
          end
        else
          begin
            aFilter := '('+SourceDM.QuoteField('TIMESTAMPD')+'>';
            aFilter := aFilter+SourceDM.DateTimeToFilter(SyncDB.Tables.FieldByName('LTIMESTAMP').AsDateTime);
            aFilter := aFilter+')';
          end;
      end;
    Result := aFilter;
  end;
var
  aFilter: String;
  aSync: TDataSet;
  aSQL: String;
  TimeSet: Boolean = False;
  aTableName: String;
begin
  aTableName := SyncDB.Tables.FieldByName('NAME').AsString;
  if (not SourceDM.TableExists(aTableName))
  or (not DestDM.TableExists(aTableName)) then
    begin
      EventLog.Warning(Format(strTableNotExists,[SyncDB.Tables.FieldByName('NAME').AsString]));
      exit;
    end;
  aFilter := BuildFilter(SourceDM,DestDM);
  if SyncDB.Tables.FieldByName('ACTIVEOUT').AsString = 'Y' then //Out
    begin
      if trim(SyncDB.Tables.FieldByName('FILTEROUT').AsString) <> '' then
        begin
          if trim(aFilter) <> '' then
            aFilter := '('+aFilter+') and ('+SyncDB.Tables.FieldByName('FILTEROUT').AsString+')'
          else
            aFilter := '('+SyncDB.Tables.FieldByName('FILTEROUT').AsString+')';
        end;
      if (pos('insert',lowercase(aFilter)) > 0)
      or (pos('update',lowercase(aFilter)) > 0)
      or (pos('delete',lowercase(aFilter)) > 0) then exit;

      aSQL := 'select '+SourceDM.QuoteField('SQL_ID')+','+SourceDM.QuoteField('TIMESTAMPD')+' from '+SourceDM.QuoteField(SyncDB.Tables.FieldByName('NAME').AsString);
      if aFilter <> '' then
        aSQL := aSQL+' where '+aFilter;
      aSync := SourceDM.GetNewDataSet(aSQL);
      aSync.Open;
      if aSync.RecordCount > 0 then
        begin
          TimeSet := True;
          SyncDB.Tables.DataSet.Edit;
          SyncDB.Tables.FieldByName('LTIMESTAMP').AsDateTime := aGlobalTime;
          SyncDB.Tables.DataSet.Post;
          EventLog.Info(Format(strSyncTable,[aSync.RecordCount,'<',SyncDB.Tables.FieldByName('NAME').AsString]));
        end;
      while not aSync.EOF do
        begin
          SyncRow(SyncDB,aSync,SourceDM,DestDM);
          aSync.Next;
        end;
      aSync.Destroy;
    end;
  if SyncDB.Tables.FieldByName('ACTIVE').AsString = 'Y' then //In
    begin
      if trim(SyncDB.Tables.FieldByName('FILTERIN').AsString) <> '' then
        aFilter := '('+aFilter+') and ('+SyncDB.Tables.FieldByName('FILTERIN').AsString+')';
      if (pos('insert',lowercase(aFilter)) > 0)
      or (pos('update',lowercase(aFilter)) > 0)
      or (pos('delete',lowercase(aFilter)) > 0) then exit;

      aSQL := 'select '+DestDM.QuoteField('SQL_ID')+','+DestDM.QuoteField('TIMESTAMPD')+' from '+DestDM.QuoteField(SyncDB.Tables.FieldByName('NAME').AsString);
      if aFilter <> '' then
        aSQL := aSQL+' where '+aFilter;
      aSync := DestDM.GetNewDataSet(aSQL);
      aSync.Open;
      if (aSync.RecordCount > 0) and (not TimeSet) then
        begin
          SyncDB.Tables.DataSet.Edit;
          SyncDB.Tables.FieldByName('LTIMESTAMP').AsDateTime := aGlobalTime;
          SyncDB.Tables.DataSet.Post;
          EventLog.Info(Format(strSyncTable,[aSync.RecordCount,'>',SyncDB.Tables.FieldByName('NAME').AsString]));
        end;
      while not aSync.EOF do
        begin
          try
          SyncRow(SyncDB,aSync,DestDM,SourceDM);
          except
          end;
          aSync.Next;
        end;
      FreeAndNil(FTempDataSet);
      FTempNewCounter := 0;
      aSync.Destroy;
    end;
end;
procedure TdmSync.DataModuleCreate(Sender: TObject);
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
begin
  aGlobalTime := Now();
  FTempDataSet := nil;
  with Application as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  with Application,Application as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
      if not HasOption('m','mandant') then
        raise Exception.Create(strMandantnotSelected);
      if not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user')) then
        raise Exception.Create(strLoginFailed);
      uData.Data := Data;
    end;
  DecodeDate(Now(),y,m,d);
  DecodeTime(Now(),h,mm,s,ss);
  EventLog.FileName := Format('sync_log_%.4d-%.2d-%.2d %.2d_%.2d_%.2d_%.4d.log',[y,m,d,h,mm,s,ss]);
  EventLog.Active:=True;
  SyncDB := TSyncDB.CreateEx(Self,Data);
  SyncDB.Open;
  while not SyncDB.DataSet.EOF do
    begin
      if SyncDB.FieldByName('ACTIVE').AsString <> 'N' then
        begin
          with Application as IBaseDbInterface do
            begin
              if Mandants.FieldByName('NAME').AsString <> Application.GetOptionValue('d','destination') then
                if Mandants.Locate('NAME',Application.GetOptionValue('d','destination'),[])
                or (not SyncDB.FieldByName('PROPERTIES').IsNull) then
                  begin
                    FDest := TBaseDBInterface.Create;
                    FDest.SetOwner(Application);
                    if not FDest.LoadMandants then
                      raise Exception.Create(strFailedtoLoadMandants);
                    with FDest as IBaseDBInterface do
                      begin
                        if not SyncDB.FieldByName('PROPERTIES').IsNull then
                          begin
                            LoggedIn := OpenMandant(copy(SyncDB.FieldByName('PROPERTIES').AsString,0,pos(':',SyncDB.FieldByName('PROPERTIES').AsString)-1),
                                               copy(SyncDB.FieldByName('PROPERTIES').AsString,pos(':',SyncDB.FieldByName('PROPERTIES').AsString)+1,length(SyncDB.FieldByName('PROPERTIES').AsString)));
                          end
                        else
                          begin
                            LoggedIn :=  OpenMandant;
                          end;
                        if LoggedIn then
                          begin
                            aSyncOffs := FDest.GetDB.SyncOffset;
                            if SyncDB.FieldByName('SYNCOFFS').AsInteger = aSyncOffs then
                              begin
                                SyncDB.Tables.Open;
                                if SyncDB.Tables.DataSet.Locate('NAME','USERFIELDDEFS',[loCaseInSensitive]) then
                                  begin
                                    SyncTable(SyncDB,uData.Data,FDest.GetDB);
                                    aTable := TDeletedItems.Create(Self,uData.Data);
                                    aTable.CreateTable;
                                    aTable.Free;
                                    aTable := TOrder.Create(Self,uData.Data);
                                    aTable.CreateTable;
                                    aTable.Open;
                                    TOrder(aTable).Positions.Open;
                                    aTable.Free;
                                    aTable := TPerson.Create(Self,uData.Data);
                                    aTable.CreateTable;
                                    aTable.Open;
                                    aTable.Free;
                                    aTable := TMasterdata.Create(Self,uData.Data);
                                    aTable.CreateTable;
                                    aTable.Open;
                                    aTable.Free;
                                  end;
                                SyncDB.Tables.DataSet.First;
                                while not SyncDB.Tables.DataSet.EOF do
                                  begin
                                    SyncTable(SyncDB,uData.Data,FDest.GetDB);
                                    SyncDB.Tables.DataSet.Next;
                                  end;
                              end
                            else
                              begin
                                with SyncDB.DataSet do
                                  begin
                                    EventLog.Error(Format(strSyncOffsetdontMatch,[SyncDB.FieldByName('NAME').AsString]));
                                    Edit;
                                    FieldByName('ACTIVE').AsString := 'N';
                                    Post;
                                  end;
                              end;
                            DBLogout;
                          end
                        else EventLog.Error(strLoginFailed);
                      end;
                  end;
            end;
        end;
      SyncDB.DataSet.Next;
    end;
  FreeAndNil(FTempDataSet);
  SyncDB.Destroy;
  Application.Terminate;
end;
end.
