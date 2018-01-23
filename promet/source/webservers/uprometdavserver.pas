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
unit uprometdavserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, udavserver, uDocuments, uBaseDbClasses, uCalendar,
  utask,Utils,variants,uBaseDatasetInterfaces, db,synautil,uPerson,uBaseDocPages,
  DateUtils, uimpvcal,uhttputil,math,uBaseDBInterface,fpjson,jsonparser,utimes,
  uwiki,uBaseApplication,fpsqlparser, fpsqlscanner, fpsqltree,uStatistic,uwebreports,
  base64;

type
  { TPrometServerFunctions }
  TPrometServerFunctions = class
    procedure aSocketDestroy(Sender: TObject);
  private
    function FindVirtualDocumentPath(aSocket: TDAVSession; var aRemovedDir,
      aDir: string; var aID: Variant; var aType: string; var aLevel: Integer;
    var aClass: TBaseDBDatasetClass): Boolean;
    procedure CreateDataModule(aSocket : TDAVSession);
  public
    function AddDocumentsToFileList(aSocket: TDAVSession;aFileList: TDAVDirectoryList;
      aDocuments: TDocuments; aPath,aFilter: string) : Boolean;
    procedure AddDocumentToFileList(aSocket: TDAVSession;
      aFileList: TDAVDirectoryList; aDocuments: TDocuments; FullPath: string);
    function ServerDelete(aSocket: TDAVSession; aDir: string): Boolean;
    function ServerMove(aSocket: TDAVSession; aFromDir, aToDir: string): Boolean;
    function ServerGetDirectoryList(aSocket: TDAVSession; aDir: string;
      aDepth: Integer; var aDirList: TDAVDirectoryList): Boolean;
    function ServerGetFile(aSocket: TDAVSession; aDir: string; Stream: TStream;
      var LastModified: TDateTime; var MimeType: string; var eTag: string
  ): Boolean;
    function ServerMkCol(aSocket: TDAVSession; aDir: string): Boolean;
    function ServerPutFile(aSocket: TDAVSession; aDir: string; Stream: TStream;
      var eTag: string; var FStatus: Integer): Boolean;
    function ServerReadAllowed(aSocket: TDAVSession; aDir: string): Boolean;
    function ServerUserLogin(aSocket: TDAVSession; aUser, aPassword: string
      ): Boolean;
  end;

implementation

uses uData;

const
  WebFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );
function BuildISODate(aDate : TDateTime) : string;
var
  bias: Integer;
  h, m: Integer;
begin
  bias := TimeZoneBias;
  if bias >= 0 then
    Result := '+'
  else
    Result := '-';
  bias := Abs(bias);
  h := bias div 60;
  m := bias mod 60;
  Result := FormatDateTime('yyyy-mm-dd',aDate)+'T'+FormatDateTime('hh:nn:ss',aDate,WebFormatSettings)+ Result + SysUtils.Format('%.2d:%.2d', [h, m]);
end;
function TPrometServerFunctions.ServerDelete(aSocket: TDAVSession; aDir: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aFullDir: String;
  aFile: String;
  aParent,aDirPar : Variant;
  aDirs: TTree;
  aCal: TCalendar;
  aTasks: TTaskList;
  aID: Variant;
  aType: string;
  aLevel: Integer;
  aRemovedDir: string;
  aClass: TBaseDBDatasetClass;
begin
  Result := False;
  if aSocket.User='' then exit;
  CreateDataModule(aSocket);
  Result:=True;
  if copy(aDir,0,1)<>'/' then
    aDir := '/'+aDir;
  if copy(aDir,0,7) = '/caldav' then
    begin
      aFullDir := aDir;
      aDir := copy(aDir,9,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aFile := StringReplace(copy(aDir,rpos('/',aDir)+1,length(aDir)),'.ics','',[]);
      aDir := copy(aDir,0,rpos('/',aDir)-1);
      if aDir = 'home' then
        begin
          aParent := TBaseDBModule(aSocket.Data).Users.Id.AsVariant;
          result := True;
        end
      else
        begin
          aDirs := TTree.Create(nil);
          aDirs.Filter(TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue('A'));
          aDirPar := null;
          Result := True;
          while pos('/',aDir)>0 do
            begin
              if aDirs.Locate('PARENT;NAME',VarArrayOf([aDirPar,copy(aDir,0,pos('/',aDir)-1)]),[]) then
                begin
                  aDirPar := aDirs.FieldByName('PARENT').AsVariant;
                  aDir := copy(aDir,pos('/',aDir)+1,length(aDir));
                end
              else
                begin
                  Result := False;
                  aDirs.Free;
                  exit;
                end;
            end;
          aParent := aDirs.Id.AsVariant;
        end;
      aCal := TCalendar.Create(nil);
      aCal.Filter(TBaseDBModule(aSocket.Data).QuoteField('ORIGID')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aFile));
      if (aCal.Count=0) and IsNumeric(aFile) then
        begin
          aCal.Select(aFile);
          aCal.Open;
        end;
      Result := aCal.Count=1;
      if Result then
        begin
          aCal.Delete;
        end
      else
        begin
          aTasks := TTaskList.Create(nil);
          aTasks.Filter(TBaseDBModule(aSocket.Data).QuoteField('ORIGIDS')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aFile));
          if (aTasks.Count=0) and IsNumeric(aFile) then
            begin
              aTasks.Select(aFile);
              aTasks.Open;
              Result := aTasks.Count=1;
              if Result then  //edit task
                begin
                  atasks.Delete;
                end
            end;
          Result := aTasks.Count=1;
          if Result then
            begin
              aTasks.Delete;
            end;
          aTasks.Free;
        end;
      aCal.Free;
    end
  else if FindVirtualDocumentPath(aSocket,aRemovedDir,aDir,aID,aType,aLevel,aClass) then
    begin
      if aLevel = 6 then
        begin
          aDocuments := TDocuments.Create(nil);
          aDocuments.Select(aId,aType,0);
          aDocuments.Open;
          if copy(aDir,length(aDir),1) = '/' then
            aDir := copy(aDir,0,length(aDir)-1);
          if rpos('/',aDir) > 1 then
            Result := aDocuments.OpenPath(copy(aDir,0,rpos('/',aDir)),'/')
          else Result := True;
          if Result then
            begin
              Result := False;
              aDir := copy(aDir,rpos('/',aDir)+1,length(aDir));
              if aDocuments.SelectFile(aDir) then
                begin
                  aDocument := TDocument.Create(nil);
                  aDocument.SelectByNumber(aDocuments.DataSet.FieldByName('NUMBER').AsVariant);
                  aDocument.Open;
                  if aDocument.Count > 0 then
                    begin
                      aDocument.Delete;
                      Result := True;
                    end;
                  aDocument.Free;
                end;
            end;
          aDocuments.Free;
        end;
    end;
end;

function TPrometServerFunctions.ServerMove(aSocket: TDAVSession; aFromDir,
  aToDir: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aFullDir: String;
  aFile: String;
  aParent,aDirPar : Variant;
  aDirs: TTree;
  aCal: TCalendar;
  aTasks: TTaskList;
  aID: Variant;
  aType: string;
  aLevel: Integer;
  aRemovedDir: string;
  aClass: TBaseDBDatasetClass;
  aRemovedDir2: string;
  aID2: Variant;
  aType2: string;
  aLevel2: Integer;
  aClass2: TBaseDBDatasetClass;
  aDocuments2: TDocuments;
begin
  Result := False;
  if aSocket.User='' then exit;
  CreateDataModule(aSocket);
  Result:=True;
  if copy(aFromDir,0,1)<>'/' then
    aFromDir := '/'+aFromDir;
  if copy(aToDir,0,1)<>'/' then
    aToDir := '/'+aToDir;
  if FindVirtualDocumentPath(aSocket,aRemovedDir,aFromDir,aID,aType,aLevel,aClass)
  and FindVirtualDocumentPath(aSocket,aRemovedDir2,aToDir,aID2,aType2,aLevel2,aClass2)
  then
    begin
      if aLevel = 6 then
        begin
          aDocuments := TDocuments.Create(nil);
          aDocuments.Select(aId,aType,0);
          aDocuments.Open;
          if copy(aFromDir,length(aFromDir),1) = '/' then
            aFromDir := copy(aFromDir,0,length(aFromDir)-1);
          if rpos('/',aFromDir) > 1 then
            Result := aDocuments.OpenPath(copy(aFromDir,0,rpos('/',aFromDir)),'/')
          else Result := True;

          if Result then
            begin
              aDocuments2 := TDocuments.Create(nil);
              aDocuments2.Select(aId2,aType2,0);
              aDocuments2.Open;
              if copy(aToDir,length(aToDir),1) = '/' then
                aToDir := copy(aToDir,0,length(aToDir)-1);
              if rpos('/',aToDir) > 1 then
                Result := aDocuments2.OpenPath(copy(aToDir,0,rpos('/',aToDir)),'/')
              else Result := True;
            end;

          if Result then
            begin
              Result := False;
              aFromDir := copy(aFromDir,rpos('/',aFromDir)+1,length(aFromDir));
              aToDir := copy(aToDir,rpos('/',aToDir)+1,length(aToDir));
              if aDocuments.SelectFile(aFromDir) then
                begin
                  aDocument := TDocument.Create(nil);
                  aDocument.SelectByNumber(aDocuments.DataSet.FieldByName('NUMBER').AsVariant);
                  aDocument.Open;
                  while not aDocument.EOF do
                    begin
                      aDocument.Edit;
                      aDocument.FieldByName('REF_ID_ID').AsVariant:=aID2;
                      aDocument.FieldByName('TYPE').AsVariant:=aType2;
                      if aDocuments2.Count>0 then
                        aDocument.FieldByName('PARENT').AsVariant:=aDocuments2.FieldByName('PARENT').AsVariant;
                      Result := True;
                    end;
                  aDocument.Free;
                end;
            end;
          aDocuments.Free;
        end;
    end;
end;

function TPrometServerFunctions.ServerGetDirectoryList(aSocket: TDAVSession; aDir: string;
  aDepth: Integer; var aDirList: TDAVDirectoryList): Boolean;
var
  aItem: TDAVFile;
  aDocuments: TDocuments;
  aFile: String;
  aCal: TCalendar;
  sl: TStringList;
  Stream: TMemoryStream;
  aDirs: TTree;
  aFullDir, aBaseDir, TmpPath, tmp2, tmp3: String;
  IsCalendarUser: Boolean = false;
  aTasks: TTaskList;
  aDel: TDeletedItems;
  aID: Variant;
  aType: string;
  i: Integer;
  aLevel: Integer;
  aRemovedDir: string;
  aClass: TBaseDBDatasetClass;
  tmp: String;
  aDataSet: TBaseDBDataset;
  aWiki: TWikiList;
  aReportType: String;
begin
  Result := false;
  if aSocket.User='' then exit;
  CreateDataModule(aSocket);
  try
  aDir := HTTPDecode(aDir);
  if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then
    begin
      TBaseDBModule(aSocket.Data).Users.Filter('',0);
      if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then exit;
    end;
  aFullDir := aDir;
  TBaseDBModule(aSocket.Data).RefreshUsersFilter;
  //Root
  if aDir = '/' then
    begin
      Result := True;
      aItem := TDAVFile.Create('/',True);
      if TBaseDBModule(aSocket.Data).Users.FieldByName('EMAIL').AsString<>'' then
        aItem.UserAdressSet.Add('mailto:'+TBaseDBModule(aSocket.Data).Users.FieldByName('EMAIL').AsString);
      aItem.UserAdressSet.Add('/caldav/');
      aItem.CalendarHomeSet:='/caldav/';
      aItem.CurrentUserPrincipal:='/users/'+TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString;
      aDirList.Add(aItem);
      if aDepth>0 then
        begin
          aItem := TDAVFile.Create('/files',True);
          aDirList.Add(aItem);
          aItem := TDAVFile.Create('/caldav',True);
          aDirList.Add(aItem);
          aItem := TDAVFile.Create('/carddav',True);
          aDirList.Add(aItem);
          aItem := TDAVFile.Create('/ical',True);
          aDirList.Add(aItem);
          for i := 0 to length(DatasetClasses)-1 do
            if DatasetClasses[i].aClass.InheritsFrom(TBaseDBList) and (
               (DatasetClasses[i].aName='MASTERDATA')
            or (DatasetClasses[i].aName='PROJECT')
            or (DatasetClasses[i].aName='TASK')
            or (DatasetClasses[i].aName='MESSAGE')
            or (DatasetClasses[i].aName='TIMES')
            or (DatasetClasses[i].aName='MEETING')
            or (DatasetClasses[i].aName='ORDER')
            or (DatasetClasses[i].aName='PERSON')
            or (DatasetClasses[i].aName='STATISTICS')
            or (DatasetClasses[i].aName='DOCPAGES')
                ) then
              begin
                aItem := TDAVFile.Create('/'+lowercase(DatasetClasses[i].aName),True);
                aDirList.Add(aItem);
              end
            else writeln('Dataset not used:'+DatasetClasses[i].aName);
        end;
    end
  //Principals
  else if (copy(aDir,0,7) = '/users/')
  then
    begin
      Result := True;
      aFullDir := aDir;
      if copy(aFullDir,length(aFullDir),1) <> '/' then
        aFullDir := aFullDir+'/';
      aItem := TDAVFile.Create(aFullDir,True);
      aDirList.Add(aItem);
      aItem.CalendarHomeSet:='/caldav/';
    end
  //Standard CalDAV Paths
  else if (copy(aDir,0,7) = '/caldav')
       or (copy(aDir,0,19) = '/.well-known/caldav')
  then
    begin
      aFullDir := aDir;
      if copy(aFullDir,length(aFullDir),1) <> '/' then
        aFullDir := aFullDir+'/';
      if (copy(aDir,0,7) = '/caldav') then aBaseDir := '/caldav/';
      if (copy(aDir,0,19) = '/.well-known/caldav') then aBaseDir := '/.well-known/caldav/';
      if copy(aDir,0,7)='/caldav' then
        aDir := copy(aDir,9,length(aDir))
      else if (copy(aDir,0,19) = '/.well-known/caldav') then
        aDir := copy(aDir,21,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aItem := TDAVFile.Create(aFullDir,True);
      aItem.CurrentUserPrincipal:='/users/'+TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString;
      aDirList.Add(aItem);
      if TBaseDBModule(aSocket.Data).Users.DataSet.Active then
        begin
          if (copy(aDir,RPos('/',aDir)+1,length(aDir)) = 'user') then
            begin
              IsCalendarUser := True;
              aDir := copy(aDir,0,rpos('/',aDir)-1);
            end;
          //Add CalDAV Calendars
          aDirs := TTree.Create(nil);
          aDirs.Filter(TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue('A'));
          if not (Assigned(aItem) and (aItem.Path = aBaseDir+aDir+'/')) then
            begin
              aItem := TDAVFile.Create(aBaseDir+'home'+'/',True);
              if Assigned(aDirList) then
                aDirList.Add(aItem)
              else aDirList := aItem;
            end;
          if (aBaseDir+'home'+'/' = aItem.Path) or (aDir = '') then
            begin
              aItem.IsCalendar:=True;
              aItem.IsTodoList:=True;
              aItem.IsCalendarUser:=IsCalendarUser;
              aItem.CurrentUserPrincipal:='/users/'+TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString;
              //Select last SQL_ID as ctag
              aDel := TDeletedItems.Create(nil);
              aDel.ActualLimit:=1;
              aDel.SortFields:='TIMESTAMPD';
              aDel.SortDirection:= sdDescending;
              aDel.Open;
              aCal := TCalendar.Create(nil);
              aCal.SelectByUser(TBaseDBModule(aSocket.Data).Users.Accountno.AsString);
              aCal.ActualLimit:=1;
              aCal.SortFields:='TIMESTAMPD';
              aCal.SortDirection:= sdDescending;
              aCal.Open;
              if aCal.TimeStamp.AsDateTime>aDel.TimeStamp.AsDateTime then
                aItem.Properties.Values['getctag'] := StringReplace(aCal.TimeStamp.AsString,' ','',[rfReplaceAll])
              else
                aItem.Properties.Values['getctag'] := StringReplace(aDel.TimeStamp.AsString,' ','',[rfReplaceAll]);
              aItem.Properties.Values['getetag'] := TBaseDBModule(aSocket.Data).Users.Id.AsString;
              aItem.Properties.Values['getcontenttype'] := 'text/calendar';
              aItem.Properties.Values['displayname'] := aItem.Name;
              aDel.Free;
              if (aDepth>0) and (aDir <> '') then
                begin
                  aCal.SelectByIdAndTime(TBaseDBModule(aSocket.Data).Users.Id.AsVariant,Now()-30,Now()+180);
                  aCal.ActualLimit:=100;
                  aCal.Open;
                  while not aCal.EOF do
                    begin
                      if aCal.FieldByName('ORIGID').AsString<>'' then
                        aItem := TDAVFile.Create(aFullDir+aCal.FieldByName('ORIGID').AsString+'.ics')
                      else
                        aItem := TDAVFile.Create(aFullDir+aCal.Id.AsString+'.ics');
                      aItem.Properties.Values['getetag'] := aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
                      aItem.Properties.Values['getcontenttype'] := 'text/calendar; component=vevent';
                      aDirList.Add(aItem);
                      aCal.Next;
                    end;
                  aTasks := TTaskList.Create(nil);
                  aTasks.SelectActiveByUser(TBaseDBModule(aSocket.Data).Users.Accountno.AsString);
                  aTasks.Open;
                  while not aTasks.EOF do
                    begin
                      if aTasks.FieldByName('ORIGIDS').AsString<>'' then
                        aItem := TDAVFile.Create(aFullDir+aTasks.FieldByName('ORIGIDS').AsString+'.ics')
                      else
                        aItem := TDAVFile.Create(aFullDir+aTasks.Id.AsString+'.ics');
                      aItem.Properties.Values['getetag'] := aTasks.Id.AsString+IntToStr(trunc(frac(aTasks.TimeStamp.AsDateTime)*1000));
                      aItem.Properties.Values['getcontenttype'] := 'text/calendar; component=vtodo';
                      aDirList.Add(aItem);
                      aTasks.Next;
                    end;
                  aTasks.Free;
                end;
              aCal.Free;
            end
          else
            begin
              aDirList.Remove(aItem);
              aItem.Free;
            end;
          while not aDirs.EOF do
            begin
              TmpPath := aBaseDir+aDirs.Text.AsString;
              tmp2 := copy(aFullDir,0,length(TmpPath));
              tmp3 := copy(TmpPath,0,length(tmp2));
              if tmp2=tmp3 then
                begin
                  if not (Assigned(aItem) and (aItem.Path = aBaseDir+aDirs.Text.AsString+'/')) then
                    begin
                      aItem := TDAVFile.Create(aBaseDir+aDirs.Text.AsString+'/',True);
                      if Assigned(aDirList) then
                        aDirList.Add(aItem)
                      else aDirList := aItem;
                    end;
                  if (aBaseDir+aDirs.Text.AsString+'/' = aItem.Path) or (aDir = '') then
                    begin
                      aItem.IsCalendar:=True;
                      aItem.IsCalendarUser:=IsCalendarUser;
                      aItem.CurrentUserPrincipal:='/users/'+TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString;
                      aDel := TDeletedItems.Create(nil);
                      aDel.ActualLimit:=1;
                      aDel.SortFields:='TIMESTAMPD';
                      aDel.SortDirection:= sdDescending;
                      aDel.Open;
                      aCal := TCalendar.Create(nil);
                      aCal.ActualLimit:=1;
                      aCal.Filter(TBaseDBModule(aSocket.Data).QuoteField('REF_ID_ID')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aDirs.Id.AsString));
                      aCal.SortFields:='TIMESTAMPD';
                      aCal.SortDirection:= sdDescending;
                      aCal.Open;
                      if aCal.TimeStamp.AsDateTime>aDel.TimeStamp.AsDateTime then
                        aItem.Properties.Values['getctag'] := StringReplace(aCal.TimeStamp.AsString,' ','',[rfReplaceAll])
                      else
                        aItem.Properties.Values['getctag'] := StringReplace(aDel.TimeStamp.AsString,' ','',[rfReplaceAll]);
                      aItem.Properties.Values['getetag'] := aDirs.Id.AsString;
                      aItem.Properties.Values['getcontenttype'] := 'text/calendar';
                      aItem.Properties.Values['displayname'] := aItem.Name;
                      aItem.CalendarHomeSet:='/caldav/';
                      aDel.Free;
                      if aDepth>0 then
                        begin
                          aCal.SelectByIdAndTime(aDirs.Id.AsVariant,Now(),Now()+90); //3 month in future
                          aCal.ActualLimit:=100;
                          aCal.Open;
                          while not aCal.EOF do
                            begin
                              if aCal.FieldByName('ORIGID').AsString<>'' then
                                aItem := TDAVFile.Create(aFullDir+aCal.FieldByName('ORIGID').AsString+'.ics')
                              else
                                aItem := TDAVFile.Create(aFullDir+aCal.Id.AsString+'.ics');
                              aItem.Properties.Values['D:getetag'] := aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
                              aItem.Properties.Values['D:getcontenttype'] := 'text/calendar; component=vevent';
                              aDirList.Add(aItem);
                              aCal.Next;
                            end;
                        end;
                      aCal.Free;
                    end
                  else aItem.Free;
                end;
              aDirs.Next;
            end;
          aDirs.Free;
          Result:=True;
        end
      else Result := False;
    end
  //Standard CardDAV Paths
  else if (copy(aDir,0,8) = '/carddav')
       or (copy(aDir,0,20) = '/.well-known/carddav')
    then
    begin
      aFullDir := aDir;
      if copy(aFullDir,length(aFullDir),1) <> '/' then
        aFullDir := aFullDir+'/';
      aDir := copy(aDir,10,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      if TBaseDBModule(aSocket.Data).Users.DataSet.Active then
        begin
          if aDir = '' then
            aDirList := TDAVDirectoryList.Create
          else aDirList:=nil;
          if (copy(aDir,RPos('/',aDir)+1,length(aDir)) = 'user') then
            begin
              IsCalendarUser := True;
              aDir := copy(aDir,0,rpos('/',aDir)-1);
            end;
          //Add CardDAV Books
          aDirs := TTree.Create(nil);
          aDirs.Filter(TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue('F'));
          while not aDirs.EOF do
            begin
              aItem := TDAVFile.Create(aFullDir+aDirs.Text.AsString,True);
              if (aDir = aItem.Name) or (aDir = '') then
                begin
                  aItem.IsCalendar:=True;
                  aItem.IsCalendarUser:=IsCalendarUser;
                  aCal := TCalendar.Create(nil);
                  aCal.Filter(TBaseDBModule(aSocket.Data).QuoteField('REF_ID_ID')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aDirs.Id.AsString));
                  //if aCal.TimeStamp.AsDateTime<aDel.TimeStamp.AsDateTime then
                    aItem.Properties.Values['getctag'] := StringReplace(aCal.TimeStamp.AsString,' ','',[rfReplaceAll])
                  //else
                  //  aItem.Properties.Values['getctag'] := Stringreplace(aDel.TimeStamp.AsString,' ','',[rfReplaceAll])
                  ;
                  aItem.Properties.Values['getetag'] := aDirs.Id.AsString;
                  aItem.Properties.Values['getcontenttype'] := 'text/calendar';
                  aItem.Properties.Values['displayname'] := aItem.Name;
                  aItem.CalendarHomeSet:='/caldav/';
                  if Assigned(aDirList) then
                    aDirList.Add(aItem)
                  else aDirList := aItem;
                  if aDepth>0 then
                    begin
                      aCal.SelectByIdAndTime(aDirs.Id.AsVariant,Now(),Now()+90); //3 month in future
                      aCal.ActualLimit:=100;
                      aCal.Open;
                      while not aCal.EOF do
                        begin
                          if aCal.FieldByName('ORIGID').AsString<>'' then
                            aItem := TDAVFile.Create(aFullDir+aCal.FieldByName('ORIGID').AsString+'.ics')
                          else
                            aItem := TDAVFile.Create(aFullDir+aCal.Id.AsString+'.ics');
                          aItem.Properties.Values['D:getetag'] := aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
                          aItem.Properties.Values['D:getcontenttype'] := 'text/calendar; component=vevent';
                          aDirList.Add(aItem);
                          aCal.Next;
                        end;
                    end;
                  aCal.Free;
                end
              else aItem.Free;
              aDirs.Next;
            end;
          aDirs.Free;
          Result:=True;
        end
      else Result := False;
    end
  //Ical Calendar
  else if copy(aDir,0,5) = '/ical' then
    begin
      Result:=True;
      aFullDir := aDir;
      if copy(aFullDir,length(aFullDir),1) <> '/' then
        aFullDir := aFullDir+'/';
      aDir := copy(aDir,6,length(aDir));
      //Add ics file
      aItem := TDAVFile.Create(aFullDir+TBaseDBModule(aSocket.Data).Users.Text.AsString+'.ics',False);
      if (aDir = aItem.Name) or (aDir = '') then
        begin
          aItem.Properties.Values['getcontenttype'] := 'text/calendar';
          aItem.Properties.Values['creationdate'] := BuildISODate(Now());
          aItem.Properties.Values['getlastmodified'] := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss',LocalTimeToGMT(Now()),WebFormatSettings)+' GMT';
          sl := TStringList.Create;
          {
          aCal := TCalendar.Create(nil);
          aCal.SelectByUser(TBaseDBModule(aSocket.Data).Users.Accountno.AsString);
          aCal.Open;
          VCalExport(aCal,sl);
          aCal.Free;
          Stream := TMemoryStream.Create;
          sl.SaveToStream(Stream);
          aItem.Properties.Values['getcontentlength'] := IntToStr(Stream.Size);
          Stream.Free;
          sl.Free;
          }
          if Assigned(aDirList) then
            aDirList.Add(aItem)
          else aDirList := aItem;
        end
      else aItem.Free;
    end
  //Files from Documents/Files
  else if FindVirtualDocumentPath(aSocket,aRemovedDir,aDir,aID,aType,aLevel,aClass) then
    begin
      if aLevel = 6 then
        begin
          if pos('/files/',aRemovedDir)>0 then
            begin
              aDir := copy(aRemovedDir,pos('/files/',aRemovedDir)+7,length(aRemovedDir));
              aDocuments := TDocuments.Create(nil);
              aDocuments.Select(aId,aType,0);
              aDocuments.Open;
              aFile := '';
              if copy(aDir,length(aDir),1) <> '/' then
                begin
                  aFile := ExtractFileName(aDir);
                  aDir := copy(aDir,0,rpos('/',aDir)-1);
                end;
              if aDocuments.OpenPath(aDir,'/') then
                Result := AddDocumentsToFileList(aSocket,aDirList,aDocuments,aRemovedDir+aDir,aFile)
              else
                begin
                  aDir := copy(aDir,0,length(aDir)-1);
                  aFile := HTTPDecode(copy(aDir,rpos('/',aDir)+1,length(aDir)));
                  aDir := copy(aDir,0,rpos('/',aDir));
                  if ((aDir = '') or (aDir = '/') or aDocuments.OpenPath(aDir,'/')) and (aDocuments.Active) then
                    begin
                      aDocuments.DataSet.First;
                      while not aDocuments.DataSet.EOF do
                        begin
                          if aDocuments.FileName = aFile then
                            AddDocumentToFileList(aSocket,aDirList,aDocuments,aRemovedDir+aDir+aFile);
                          aDocuments.DataSet.Next;
                        end;
                    end;
                  Result := True;
                end;
              aDocuments.Free;
            end
          else if copy(aRemovedDir,length(aRemovedDir)-8,9) = '/reports/' then
            begin
              if (aDir = '') or (aDir = '/') then
                begin
                  if copy(aFullDir,length(aFullDir),1) <> '/' then
                    aFullDir := aFullDir+'/';
                  aDataSet := aClass.Create(nil);
                  aDataSet.Select(aId);
                  aDataSet.Open;
                  case aType of
                  'M':aReportType := 'MAS';
                  'P':aReportType := 'PRJ';
                  'C':aReportType := 'CUSD';
                  'S':aReportType := copy(aDataSet.Id.AsString,length(aDataSet.Id.AsString)-3,4); //Statistics
                  'W':aReportType := 'WIKI'; //Wiki
                  'O':aReportType := 'OR'+aDataSet.FieldByName('STATUS').AsString;
                  else aReportType:='';
                  end;
                  aDataSet.Free;
                  TBaseDBModule(aSocket.Data).Reports.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue(aReportType));
                  if (aReportType<>'') then
                    begin
                      with TBaseDBModule(aSocket.Data).Reports do
                        begin
                          First;
                          while not EOF do
                            begin
                              aItem := TDAVFile.Create(aFullDir+FieldByName('NAME').AsString+'.pdf',False);
                              aItem.Properties.Values['getcontenttype'] := 'application/pdf';
                              aItem.Properties.Values['creationdate'] := BuildISODate(Now());
                              aItem.Properties.Values['getlastmodified'] := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss',LocalTimeToGMT(Now()),WebFormatSettings)+' GMT';
                              aDirList.Add(aItem);
                              Next;
                            end;
                        end;
                      Result := True;
                    end;
                end;
            end;
        end
      else if (aLevel=1) and (aDir = '') then
        begin
          if copy(aFullDir,length(aFullDir),1) <> '/' then
            aFullDir := aFullDir+'/';
          aItem := TDAVFile.Create(aFullDir+'by-id',True);
          aDirList.Add(aItem);
          aItem := TDAVFile.Create(aFullDir+'list.json',False);
          aItem.Properties.Values['getcontenttype'] := 'application/json';
          aDirList.Add(aItem);
          TBaseDBModule(aSocket.Data).Tree.Open;
          tmp := TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aType)+' AND '+TBaseDBModule(aSocket.Data).QuoteField('PARENT')+'=0';
          TBaseDBModule(aSocket.Data).Tree.DataSet.Filter:=tmp;
          TBaseDBModule(aSocket.Data).Tree.DataSet.Filtered:=True;
          TBaseDBModule(aSocket.Data).Tree.DataSet.First;
          while not TBaseDBModule(aSocket.Data).Tree.EOF do
            begin
              aItem := TDAVFile.Create(aFullDir+TBaseDBModule(aSocket.Data).Tree.FieldByName('NAME').AsString,True);
              aDirList.Add(aItem);
              TBaseDBModule(aSocket.Data).Tree.Next;
            end;
          Result:=True;
        end
      else if (aLevel=2) then //by-id
        begin
          if copy(aFullDir,length(aFullDir),1) <> '/' then
            aFullDir := aFullDir+'/';
          aItem := TDAVFile.Create(aFullDir+'Help.txt',False);
          aItem.Properties.Values['getcontenttype'] := 'text/text';
          aDirList.Add(aItem);
          Result:=True;
        end
      else if (aLevel=3) and (aDir='') then //dynamic content
        begin
          if copy(aFullDir,length(aFullDir),1) <> '/' then
            aFullDir := aFullDir+'/';
          aItem := TDAVFile.Create(aFullDir+'item.xml',False);
          aItem.Properties.Values['getcontenttype'] := 'text/xml';
          aDirList.Add(aItem);
          aItem := TDAVFile.Create(aFullDir+'item.json',False);
          aItem.Properties.Values['getcontenttype'] := 'application/json';
          aDirList.Add(aItem);
          aWiki := TWikiList.Create(nil);
          case aClass.ClassName of
          'TMasterdata':tmp := 'TArticle'
          else tmp := aClass.ClassName;
          end;
          tmp := copy(tmp,2,length(tmp))+'Frame';
          if aWiki.FindWikiFolder('Promet-ERP-Help/forms/tf'+tmp+'/') then
            begin
              while not aWiki.EOF do
                begin
                  aItem := TDAVFile.Create(aFullDir+aWiki.FieldByName('NAME').AsString+'.html',False);
                  aItem.Properties.Values['getcontenttype'] := 'text/html';
                  aDirList.Add(aItem);
                  aWiki.Next;
                end;
            end;
          aWiki.Free;
          aItem := TDAVFile.Create(aFullDir+'files',true);
          aDirList.Add(aItem);
          aItem := TDAVFile.Create(aFullDir+'reports',true);
          aDirList.Add(aItem);

          Result:=True;
        end
      else if (aLevel=4) and ((aRemovedDir=aFullDir) or (aRemovedDir=aFullDir+'/')) then //Tree Dir
        begin
          if Assigned(aClass) then
            begin
              aDataSet := aClass.Create(nil);
              TBaseDbList(aDataSet).SelectFromTreeEntry(TBaseDBModule(aSocket.Data).Tree.FieldByName('SQL_ID').AsLargeInt);
              aDataSet.Open;
              while not aDataSet.EOF do
                begin
                  aItem := TDAVFile.Create(StringReplace(aFullDir+'/'+TBaseDbList(aDataSet).Number.AsString,'//','/',[rfReplaceAll]),True);
                  aDirList.Add(aItem);
                  aDataSet.Next;
                end;
              aDataSet.Free;
            end;
          TBaseDBModule(aSocket.Data).Tree.DataSet.Filter:=TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aType)+' AND '+TBaseDBModule(aSocket.Data).QuoteField('PARENT')+'='+IntToStr(TBaseDBModule(aSocket.Data).Tree.FieldByName('SQL_ID').AsLargeInt);
          TBaseDBModule(aSocket.Data).Tree.DataSet.First;
          while not TBaseDBModule(aSocket.Data).Tree.EOF do
            begin
              aItem := TDAVFile.Create(StringReplace(aFullDir+'/'+TBaseDBModule(aSocket.Data).Tree.FieldByName('NAME').AsString,'//','/',[rfReplaceAll]),True);
              aDirList.Add(aItem);
              TBaseDBModule(aSocket.Data).Tree.Next;
            end;
          Result := True;
        end;
    end;
  except
    Result:=False;
  end;
end;
function TPrometServerFunctions.ServerGetFile(aSocket: TDAVSession; aDir: string;
  Stream: TStream; var LastModified: TDateTime; var MimeType: string;
  var eTag: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aFileName: String;
  lIndex: Integer;
  sl: TStringList;
  aCal: TCalendar;
  aFullDir: String;
  aFile: String;
  aParent,aDirPar : Variant;
  aDirs: TTree;
  aTasks: TTaskList;
  aID: Variant;
  aType: string;
  aLevel: Integer;
  aRemovedDir: string;
  aSL: TStringList;
  aClass: TBaseDBDatasetClass;
  aDataSet: TBaseDBDataset;
  aSS: TStringStream;
  tmp: String;
  i: Integer;
  aParams: String;
  aParamDec: TStringList;
  aWiki: TWikiList;
  aDS: TDataSet;
  QueryFields: TStringList;
  aStmt: TSQLStatemnt;
  aReportType: String;
  aFStream: TFileStream;
  StatisticResultsDataSet: TDataSet = nil;
  DetailDataSet: TDataSet = nil;
  SubDetailDataSet: TDataSet = nil;
  aDirList: TDAVDirectoryList;
  nStream: TStream;
begin
  Result := False;
  if pos('?',aDir)>0 then
    begin
      aParams := copy(aDir,pos('?',aDir)+1,length(aDir));
      aDir := copy(aDir,0,pos('?',aDir)-1);
    end;
  QueryFields := TStringList.Create;
  QueryFields.Text:=aParams;
  aDir := HTTPDecode(aDir);
  aFullDir := aDir;
  if (aSocket.User='') and (pos('blobdata',aDir)=0) then exit;
  CreateDataModule(aSocket);
  try
  if aDir = 'ical/'+TBaseDBModule(aSocket.Data).Users.Text.AsString+'.ics' then
    begin
      sl := TStringList.Create;
      aCal := TCalendar.Create(nil);
      aCal.SelectByUser(TBaseDBModule(aSocket.Data).Users.Accountno.AsString);
      aCal.Open;
      VCalExport(aCal,sl);
      sl.SaveToStream(Stream);
      aCal.Free;
      sl.Free;
      Result := True;
    end
  else if (copy(aDir,0,7) = 'caldav/')
       or (copy(aDir,0,7) = '/caldav')
       or (copy(aDir,0,19) = '/.well-known/caldav')
       then
    begin
      if copy(aFullDir,0,1) = '/' then
        aFullDir := copy(aFullDir,2,length(aFullDir)-1);
      aDir := copy(aDir,8,length(aDir));
      if copy(aDir,0,1) = '/' then
        aDir := copy(aDir,2,length(aDir)-1);
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aFile := StringReplace(copy(aDir,rpos('/',aDir)+1,length(aDir)),'.ics','',[]);
      aDir := copy(aDir,0,rpos('/',aDir)-1);
      if aDir = 'home' then
        begin
          aParent := TBaseDBModule(aSocket.Data).Users.Id.AsVariant;
          result := True;
        end
      else
        begin
          aDirs := TTree.Create(nil);
          aDirs.Filter(TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue('A'));
          aDirPar := null;
          Result := True;
          while pos('/',aDir)>0 do
            begin
              if aDirs.Locate('PARENT;NAME',VarArrayOf([aDirPar,copy(aDir,0,pos('/',aDir)-1)]),[]) then
                begin
                  aDirPar := aDirs.FieldByName('PARENT').AsVariant;
                  aDir := copy(aDir,pos('/',aDir)+1,length(aDir));
                end
              else
                begin
                  Result := False;
                  aDirs.Free;
                  exit;
                end;
            end;
          aParent := aDirs.Id.AsVariant;
        end;
      aCal := TCalendar.Create(nil);
      aCal.Filter(TBaseDBModule(aSocket.Data).QuoteField('ORIGID')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aFile));
      if (aCal.Count=0) and IsNumeric(aFile) then
        begin
          aCal.Select(aFile);
          aCal.Open;
        end;
      Result := aCal.Count>=1;
      if Result then
        begin
          eTag:=aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
          sl := TStringList.Create;
          VCalExport(aCal,sl);
          sl.SaveToStream(Stream);
          sl.Free;
        end;
      aCal.Free;
      if (not result) and (aDir = 'home') then //check for taks
        begin
          aTasks := TTaskList.Create(nil);
          aTasks.Filter(TBaseDBModule(aSocket.Data).QuoteField('ORIGIDS')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aFile));
          if (aTasks.Count=0) and IsNumeric(aFile) then
            begin
              aTasks.Select(aFile);
              aTasks.Open;
            end;
          Result := aTasks.Count>=1;
          if Result then
            begin
              eTag:=aTasks.Id.AsString+IntToStr(trunc(frac(aTasks.TimeStamp.AsDateTime)*1000));
              sl := TStringList.Create;
              VTodoExport(aTasks,sl);
              sl.SaveToStream(Stream);
              sl.Free;
            end;
          aTasks.Free;
        end;
    end
  else if FindVirtualDocumentPath(aSocket,aRemovedDir,aDir,aID,aType,aLevel,aClass) then
    begin
      Mimetype := '';
      if aLevel=1 then //direkt auf Dataset ebene
        begin
          aParamDec := TStringList.Create;
          aParamDec.Delimiter:='&';
          tmp := copy(aSocket.URI,pos('?',aSocket.URI)+1,length(aSocket.URI))+'&';
          while pos('&',tmp)>0 do
            begin
              aParamDec.Add(copy(tmp,0,pos('&',tmp)-1));
              tmp := copy(tmp,pos('&',tmp)+1,length(tmp));
            end;
          if aDir = 'list.json' then
            begin
              MimeType:='application/json';
              sl := TStringList.Create;
              sl.Add('[');
              aDataSet := aClass.Create(nil);
              if aParamDec.Values['filter']<>'' then
                aDataSet.ActualFilter:=aParamDec.Values['filter'];
              if aDataSet is TTimes then
                begin
                  if aDataSet.ActualFilter <> '' then
                    aDataSet.ActualFilter:=aDataSet.ActualFilter+' AND '+Data.QuoteField('REF_ID')+'='+Data.QuoteValue(Data.Users.Id.AsString)
                  else
                    aDataSet.ActualFilter:=Data.QuoteField('REF_ID')+'='+Data.QuoteField(Data.Users.Id.AsString);
                end;
              aDataSet.ActualLimit:=StrToIntDef(HTTPDecode(aParamDec.Values['limit']),100);
              aDataSet.Open;
              while not aDataSet.EOF do
                begin
                  if sl.Count>1 then
                    sl[sl.Count-1] := sl[sl.Count-1]+',';
                  tmp := '{ "sql_id": '+aDataSet.Id.AsString;
                  for i := 1 to aDataSet.DataSet.Fields.Count-1 do
                    begin
                      if i<aDataSet.DataSet.Fields.Count then tmp += ',';
                      if aDataSet.DataSet.Fields[i].IsNull then
                        tmp += '"'+StringToJSONString(aDataSet.DataSet.Fields[i].FieldName)+'": null'
                      else if (aDataSet.DataSet.FieldDefs[i].DataType=ftDate)
                           or (aDataSet.DataSet.FieldDefs[i].DataType=ftDateTime) then
                        tmp += '"'+StringToJSONString(aDataSet.DataSet.Fields[i].FieldName)+'": "'+synautil.Rfc822DateTime(aDataSet.DataSet.Fields[i].AsDateTime)+'"'
                      else if (aDataSet.DataSet.FieldDefs[i].DataType=ftInteger)
                           or (aDataSet.DataSet.FieldDefs[i].DataType=ftLargeint) then
                        tmp += '"'+StringToJSONString(aDataSet.DataSet.Fields[i].FieldName)+'": '+aDataSet.DataSet.Fields[i].AsString
                      else
                        tmp += '"'+StringToJSONString(aDataSet.DataSet.Fields[i].FieldName)+'": "'+StringReplace(StringToJSONString(aDataSet.DataSet.Fields[i].AsString),'','*',[rfReplaceAll])+'"';
                    end;
                  tmp+=' }';
                  sl.Add(tmp);
                  aDataSet.Next;
                end;
              sl.Add(']');
              sl.SaveToStream(Stream);
              sl.Free;
              Stream.Position:=0;
              aDataSet.Free;
              Result:=True;
            end
          else if (copy(aDir,0,9)='blobdata/') then
            begin
              tmp := copy(aDir,10,length(aDir));
              tmp := Uppercase(copy(tmp,0,pos('/',tmp)-1));
              aDataSet := aClass.Create(nil);
              aDir := ExtractFileName(aDir);
              aDir := copy(aDir,0,pos('.',aDir)-1);
              aDataSet.Select(aDir);
              with aDataSet.DataSet as IBaseDBFilter do
                Fields := '';
              aDataSet.Open;
              if aDataSet.Count>0 then
                begin
                  nStream := TBaseDBModule(aSocket.Data).BlobFieldStream(aDataSet.DataSet,tmp);
                  Stream.CopyFrom(nStream,0);
                  if Stream.Size>0 then
                    Result := True;
                end;
              aDataSet.Free;
            end;
          aParamDec.Free;
        end
      else if aLevel=6 then
        begin
          if pos('/files/',aRemovedDir)>0 then
            begin
              aDocuments := TDocuments.Create(nil);
              try
              aDocuments.Select(aId,aType,0);
              aDocuments.Open;
              if rpos('/',aDir) > 1 then
                Result := aDocuments.OpenPath(copy(aDir,0,rpos('/',aDir)),'/')
              else Result := True;
              if Result then
                begin
                  Result := False;
                  aDir := copy(aDir,rpos('/',aDir)+1,length(aDir));
                  if aDocuments.SelectFile(aDir) then
                    begin
                      aDocument := TDocument.Create(nil);
                      try
                      aDocument.SelectByNumber(aDocuments.DataSet.FieldByName('NUMBER').AsVariant);
                      aDocument.Open;
                      if aDocument.Count > 0 then
                        begin
                          aDocument.CheckoutToStream(Stream);
                          LastModified:=aDocument.LastModified;
                          if MimeType = '' then
                            MimeType := GetMimeTypeforExtension(ExtractFileExt(aDocuments.FileName));
                          Result := True;
                        end;
                      finally
                        aDocument.Free;
                      end;
                    end;
                end;
              finally
                aDocuments.Free;
              end;
            end
          else if pos('/reports/',aRemovedDir)>0 then
            begin
              aDir := copy(aDir,rpos('/',aDir)+1,length(aDir));
              if copy(aFullDir,length(aFullDir),1) <> '/' then
                aFullDir := aFullDir+'/';
              aDataSet := aClass.Create(nil);
              aDataSet.Select(aId);
              aDataSet.Open;
              case aType of
              'M':aReportType := 'MAS';
              'P':aReportType := 'PRJ';
              'C':aReportType := 'CUSD';
              'S':aReportType := copy(aDataSet.Id.AsString,length(aDataSet.Id.AsString)-3,4); //Statistics
//              'S':aReportType := 'SHMD'; //Shema
              'W':aReportType := 'WIKI'; //Wiki
              'O':aReportType := 'OR'+aDataSet.FieldByName('STATUS').AsString;
              else aReportType:='';
              end;
              TBaseDBModule(aSocket.Data).Reports.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue(aReportType));
              TBaseDBModule(aSocket.Data).Reports.DataSet.Refresh;
              //writeln(IntToStr(TBaseDBModule(aSocket.Data).Reports.DataSet.RecordCount),TBaseDBModule(aSocket.Data).Reports.FieldByName('NAME').AsString);
              tmp := copy(aDir,0,rpos('.',aDir)-1);
              if TBaseDBModule(aSocket.Data).Reports.Locate('NAME',tmp,[loCaseInsensitive]) and (aReportType<>'') then
                begin
                  fWebReports := TfWebReports.Create(nil);
                  try
                    with TBaseDBModule(aSocket.Data).Reports.FieldByName('REPORT') as TBlobField do
                      if not TBaseDBModule(aSocket.Data).Reports.FieldByName('REPORT').IsNull then
                        begin
                          with BaseApplication as IBaseApplication do
                            begin
                              TBaseDBModule(aSocket.Data).BlobFieldToFile(TBaseDBModule(aSocket.Data).Reports.DataSet,'REPORT',GetInternalTempDir+'preport.lrf');
                              fWebReports.Report.LoadFromFile(GetInternalTempDir+'preport.lrf');
                              DeleteFile(UniToSys(GetInternalTempDir+'preport.lrf'));
                              Result := True;
                            end;
                        end;
                    if Result then
                      begin
                        if aDataSet is TStatistic then
                          begin
                            if Assigned(StatisticResultsDataSet) then
                              StatisticResultsDataSet.Free;
                            StatisticResultsDataSet := Data.GetNewDataSet(TStatistic(aDataSet).BuildQuerry(QueryFields));
                            StatisticResultsDataSet.Open;
                            for i := 0 to StatisticResultsDataSet.Fields.Count-1 do
                              begin
                                if (StatisticResultsDataSet.Fields[i].DataType=ftString)
                                or (StatisticResultsDataSet.Fields[i].DataType=ftMemo)
                                then
//                                  StatisticResultsDataSet.Fields[i].OnGetText:=@DataSetFieldsFieldsGetText
                                else if (StatisticResultsDataSet.Fields[i].DataType=ftFloat) then
                                  TFloatField(StatisticResultsDataSet.Fields[i]).DisplayFormat:='########.##';
                              end;
                            fWebReports.ManualRegisterDataSet(StatisticResultsDataSet,'StatisticResults',False);
                            if trim(aDataSet.FieldByName('DETAIL').AsString) <> '' then
                              begin
                                if Assigned(DetailDataSet) then
                                  DetailDataSet.Free;
                                DetailDataSet := Data.GetNewDataSet(TStatistic(aDataSet).BuildSQL(aDataSet.FieldByName('DETAIL').AsString),aDataSet.Connection,StatisticResultsDataSet);
                                DetailDataSet.Open;
                                fWebReports.ManualRegisterDataSet(DetailDataSet,'Details',False);
                                if trim(aDataSet.FieldByName('SUBDETAIL').AsString) <> '' then
                                  begin
                                    if Assigned(SubDetailDataSet) then
                                      SubDetailDataSet.Free;
                                    SubDetailDataSet := Data.GetNewDataSet(TStatistic(aDataSet).BuildSQL(aDataSet.FieldByName('SUBDETAIL').AsString),aDataSet.Connection,DetailDataSet);
                                    SubDetailDataSet.Open;
                                    fWebReports.ManualRegisterDataSet(SubDetailDataSet,'SubDetails',False);
                                  end;
                              end;
                          end
                        else
                          fWebReports.RegisterDataSet(aDataSet.DataSet,False);
                        fWebReports.RegisterDataSet(TBaseDBModule(aSocket.Data).Users.DataSet,False);
                        fWebReports.RegisterDataSet(TBaseDBModule(aSocket.Data).PaymentTargets.DataSet,False);
                        fWebReports.RegisterDataSet(TBaseDBModule(aSocket.Data).MandantDetails.DataSet,False);
                        Result:=fWebReports.ExportToPDF(GetTempPath+DirectorySeparator+'rpv.pdf') and FileExists(GetTempPath+DirectorySeparator+'rpv.pdf');
                        if Result then
                          begin
                            aFStream:= TFileStream.Create(GetTempPath+DirectorySeparator+'rpv.pdf',fmOpenRead);
                            Stream.CopyFrom(aFStream,0);
                            Stream.Position:=0;
                            aFStream.Free;
                            DeleteFile(UniToSys(GetTempPath+DirectorySeparator+'rpv.pdf'));
                          end
                        else
                          begin
                            aSocket.Status := 409;
                            MimeType:='text/plain';
                            with BaseApplication as IBaseApplication do
                              Error('Print Error "'+fWebReports.LastError+'"');
                            sl := TStringList.Create;
                            sl.Add(fWebReports.LastError);
                            sl.SaveToStream(TDAVSession(aSocket).OutputData);
                            sl.Free;
                          end;
                      end
                    else
                      begin
                        aSocket.Status := 404;
                        sl := TStringList.Create;
                        sl.Add('Report not found (in database)!');
                        sl.SaveToStream(TDAVSession(aSocket).OutputData);
                        sl.Free;
                      end;
                  finally
                    try
                      fWebReports.Free;
                    except
                    end;
                  end;
                end;
              aDataSet.Free;
            end;
        end
      else if (aLevel=3) then //dynamic content
        begin
          if aDir = 'item.xml' then
            begin
              MimeType:='text/xml';
              aDataSet := aClass.Create(nil);
              aDataSet.Select(aID);
              aDataSet.Open;
              if aDataSet.Count>0 then
                begin
                  aSS := TStringStream.Create(aDataSet.ExportToXML);
                  Stream.CopyFrom(aSS,0);
                  aSS.Free;
                  Result:=True;
                end;
              aDataSet.Free;
            end
          else if aDir = 'item.json' then
            begin
              MimeType:='application/json';
              aDataSet := aClass.Create(nil);
              aDataSet.Select(aID);
              aDataSet.Open;
              if aDataSet.Count>0 then
                begin
                  aSS := TStringStream.Create(aDataSet.ExportToJSON);
                  Stream.CopyFrom(aSS,0);
                  aSS.Free;
                  Result:=True;
                end;
              aDataSet.Free;
            end
          else if pos('.html',aDir)>0 then
            begin
              MimeType:='text/html';
              aWiki := TWikiList.Create(nil);
              case aClass.ClassName of
              'TMasterdata':tmp := 'TArticle'
              else tmp := aClass.ClassName;
              end;
              tmp := copy(tmp,2,length(tmp))+'Frame';
              if aWiki.FindWikiPage('Promet-ERP-Help/forms/tf'+tmp+'/'+copy(aDir,0,pos('.html',aDir)-1)) then
                begin
                  aDataSet := aClass.Create(nil);
                  aDataSet.Select(aID);
                  aDataSet.Open;
                  if aDataSet.Count>0 then
                    begin
                      aWiki.Variables.Values['SQL_ID']:=aID;
                      aWiki.Variables.Values['ID'] := TBaseDbList(aDataSet).Number.AsString;
                      aWiki.Variables.Values['TEXT'] := TBaseDbList(aDataSet).Text.AsString;
                      aSL := TStringList.Create;
                      aSL.Text := aWiki.PageAsHtml();
                      aSL.SaveToStream(Stream);
                      aSL.Free;
                      Result:=True;
                    end;
                end;
              aWiki.Free;
            end;
        end
      else if aLevel=2 then
        begin
          if pos('/by-id/Help.txt',aFullDir)>0 then
            begin
              MimeType:='text/text';
              aSL := TStringList.Create;
              aSL.Add('==Open Documents stored in Promet-ERP/Avamm Files==');
              aSL.Add('');
              aSL.Add('You can add the Number of an Promet-ERP/Avamm Element to this Path and access the Files stored in the Tab "Files".');
              aSL.Add('Not all Elements can be displayed in This Path so none are Displayed.');
              aSL.SaveToStream(Stream);
              aSL.Free;
              Result:=True;
            end;
        end;
      if (not Result) and (aDir = '.json') then
        begin
          aDirList := TDAVDirectoryList.Create;
          TWebDAVMaster(aSocket.Creator).Lock;
          Result := ServerGetDirectoryList(aSocket,StringReplace(aFullDir,'/.json','/',[]),1,aDirList);
          TWebDAVMaster(aSocket.Creator).Unlock;
          sl := TStringList.Create;
          sl.Add('[');
          for i := 0 to aDirList.Count-1 do
            begin
              tmp := '{';
              tmp += '"name":"'+StringToJSONString(aDirList.Files[i].Name)+'"';
              if aDirList.Files[i].IsDir then
                tmp += ',"isdir":true'
              else
                tmp += ',"isdir":false';
              tmp += ',"path":"'+StringToJSONString(aDirList.Files[i].Path)+'"';
              if i= aDirList.Count-1 then
                tmp+=' }'
              else tmp += ' },';
              sl.Add(tmp);
            end;
          sl.Add(']');
          sl.SaveToStream(Stream);
          result := True;
          aDirList.Free;
          sl.Free;
        end;
    end
  else if (aDir = '/sql.json') then //direct SQL Query (very limited for sequrity reasons)
    begin
      aStmt := TSQLStatemnt.Create;
      aStmt.SQL:= AddSQLLimit(queryFields.Values['ql'],100,TBaseDBModule(aSocket.Data));
      try
        if not aStmt.Parse then
          aStmt.SQL:=queryFields.Values['ql'];
        if aStmt.Parse then
          begin
            aDS := TBaseDBModule(aSocket.Data).GetNewDataSet(aStmt.FormatedSQL);
            with aDS as IBaseDBFilter do
              begin
                FetchRows:=20;
                Limit := 100;
              end;
            aDS.Open;
            sl := TStringList.Create;
            sl.Add('[');
            while not aDS.EOF do
              begin
                if sl.Count>1 then
                  sl[sl.Count-1] := sl[sl.Count-1]+',';
                tmp := '{';
                for i := 1 to aDS.Fields.Count-1 do
                  begin
                    if (i<aDS.Fields.Count) and (i>1) then tmp += ',';
                    tmp += '"'+StringToJSONString(aDS.Fields[i].FieldName)+'":"'+StringReplace(StringToJSONString(aDS.Fields[i].AsString),'','*',[rfReplaceAll])+'"';
                  end;
                tmp+=' }';
                sl.Add(tmp);
                aDS.Next;
              end;
            sl.Add(']');
            sl.SaveToStream(Stream);
            aDS.Free;
            result := True;
          end;
      except
        on e : Exception do
          begin
            aSocket.Status := 409;
            MimeType:='text/plain';
            with BaseApplication as IBaseApplication do
              Error('Error "'+e.Message+'"');
            sl := TStringList.Create;
            sl.Add(e.Message);
            sl.SaveToStream(TDAVSession(aSocket).OutputData);
            sl.Free;
          end;
      end;
      aStmt.Free;
    end;
  finally
    QueryFields.Free;
  end;
end;
function TPrometServerFunctions.ServerMkCol(aSocket: TDAVSession; aDir: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  Subfolder: Boolean = False;
  aId: Variant;
  aType: string;
  aLevel: Integer;
  aRemovedDir: string;
  aClass: TBaseDBDatasetClass;
begin
  Result := False;
  if aSocket.User='' then exit;
  CreateDataModule(aSocket);
  if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then
    begin
      TBaseDBModule(aSocket.Data).Users.Filter('',0);
      if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then exit;
    end;
  TBaseDBModule(aSocket.Data).RefreshUsersFilter;
  if FindVirtualDocumentPath(aSocket,aRemovedDir,aDir,aId,aType,aLevel,aClass) then
    begin
      if aLevel=6 then
        begin
          aDocuments := TDocuments.Create(nil);
          aDocuments.Select(aId,aType,0);
          aDocuments.Open;
          if copy(aDir,length(aDir),1)='/' then
            aDir := copy(aDir,0,length(aDir)-1);
          if (rpos('/',aDir) > 1) then
            begin
              Result := aDocuments.OpenPath(copy(aDir,0,rpos('/',aDir)-1),'/');
              Subfolder := True;
            end
          else Result := True;
          if Result then
            begin
              Result := False;
              aDir := copy(aDir,rpos('/',aDir)+1,length(aDir));
              aDocument := TDocument.Create(nil);
              if SubFolder then
                aDocument.BaseParent := aDocuments
              else
                begin
                  aDocument.Ref_ID := aDocuments.Ref_ID;
                  aDocument.BaseTyp:=aDocuments.BaseTyp;
                  aDocument.BaseID:=aDocuments.BaseID;
                  aDocument.BaseVersion:=aDocuments.BaseVersion;
                  aDocument.BaseLanguage:=aDocuments.BaseLanguage;
                end;
              aDocument.Insert;
              aDocument.CreateDirectory(aDir);
              aDocument.Free;
              Result := True;
            end;
          aDocuments.Free;
        end;
    end;
end;
function TPrometServerFunctions.ServerPutFile(aSocket: TDAVSession; aDir: string;
  Stream: TStream; var eTag: string; var FStatus: Integer): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aFileName: String;
  aFStream: TFileStream;
  aFiles: TStrings;
  aDocuments2: TDocuments;
  aFullDir: String;
  aParent,aDirPar : Variant;
  aDirs: TTree;
  aFile: String;
  aCal: TCalendar;
  sl: TStringList;
  aTasks: TTaskList;
  aID: Variant;
  aType: string;
  aLevel: Integer;
  aRemovedDir, aParams: string;
  aClass: TBaseDBDatasetClass;
  Mimetype: String;
  aJParser: TJSONParser;
  aData: TJSONData;
  aDataSet: TBaseDBDataset;
  aField: TJSONStringType;
  a: Integer;
  NotFound: Boolean;
  ss: TStringStream;
begin
  FStatus:=500;
  Result := False;
  if aSocket.User='' then exit;
  CreateDataModule(aSocket);
  if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then
    begin
      TBaseDBModule(aSocket.Data).Users.Filter('',0);
      if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then exit;
    end;
  Result := True;
  if pos('?',aDir)>0 then
    begin
      aParams := copy(aDir,pos('?',aDir)+1,length(aDir));
      aDir := copy(aDir,0,pos('?',aDir)-1);
    end;
  TBaseDBModule(aSocket.Data).RefreshUsersFilter;
  if copy(aDir,0,1)<>'/' then
    aDir := '/'+aDir;
  if (copy(aDir,0,7) = '/caldav')
  or (copy(aDir,0,19) = '.well-known/caldav')
  then
    begin
      aFullDir := aDir;
      aDir := copy(aDir,9,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aFile := StringReplace(copy(aDir,rpos('/',aDir)+1,length(aDir)),'.ics','',[]);
      aDir := copy(aDir,0,rpos('/',aDir)-1);
      if aDir = 'home' then
        begin
          aParent := TBaseDBModule(aSocket.Data).Users.Id.AsVariant;
          result := True;
        end
      else
        begin
          aDirs := TTree.Create(nil);
          aDirs.Filter(TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue('A'));
          aDirPar := null;
          Result := True;
          while pos('/',aDir)>0 do
            begin
              if aDirs.Locate('PARENT;NAME',VarArrayOf([aDirPar,copy(aDir,0,pos('/',aDir)-1)]),[]) then
                begin
                  aDirPar := aDirs.FieldByName('PARENT').AsVariant;
                  aDir := copy(aDir,pos('/',aDir)+1,length(aDir));
                end
              else
                begin
                  Result := False;
                  aDirs.Free;
                  exit;
                end;
            end;
          if not aDirs.Locate('PARENT;NAME',VarArrayOf([aDirPar,aDir]),[]) then
            aDirs.Locate('NAME',VarArrayOf([aDir]),[]);
          aParent := aDirs.Id.AsVariant;
        end;
      sl := TStringList.Create;
      Stream.Position:=0;
      sl.LoadFromStream(Stream);
      aCal := TCalendar.Create(nil);
      aCal.Filter(TBaseDBModule(aSocket.Data).QuoteField('ORIGID')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aFile));
      if (aCal.Count=0) and IsNumeric(aFile) then
        begin
          aCal.Select(aFile);
          aCal.Open;
        end;
      Result := aCal.Count>=1;
      if Result then
        begin
          eTag:=aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
          aCal.Edit;
          if aCal.FieldByName('REF_ID_ID').AsVariant<>aParent then
            aCal.FieldByName('REF_ID_ID').AsVariant:=aParent;
          if not VCalImport(aCal,sl,True) then
            Result := False
          else
            FStatus:=200;
        end
      else
        begin
          //todo ???
          aTasks := TTaskList.Create(nil);
          aTasks.Filter(TBaseDBModule(aSocket.Data).QuoteField('ORIGIDS')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aFile));
          if (aTasks.Count=0) and IsNumeric(aFile) then
            begin
              aTasks.Select(aFile);
              aTasks.Open;
            end;
          Result := aTasks.Count>=1;
          if Result then  //edit task
            begin
              eTag:=aTasks.Id.AsString+IntToStr(trunc(frac(atasks.TimeStamp.AsDateTime)*1000));
              aTasks.Edit;
              if not VTodoImport(aTasks,sl,True) then
                Result := False
              else
                FStatus:=200;
            end;
          if (not Result) and (pos(':vtodo',lowercase(sl.Text))>0) then //new task
            begin
              Result := True;
              aTasks.Insert;
              if not VTodoImport(aTasks,sl,True) then
                result := False;
              eTag:=aTasks.Id.AsString+IntToStr(trunc(frac(aTasks.TimeStamp.AsDateTime)*1000));
              FStatus:=201;
            end;
          atasks.Free;
          if not result then //New file
            begin
              Result := True;
              aCal.Insert;
              aCal.FieldByName('REF_ID_ID').AsVariant:=aParent;
              if not VCalImport(aCal,sl,True) then
                result := False;
              eTag:=aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
              FStatus:=201;
            end;
        end;
      aCal.Free;
      sl.Free;
    end
  else if FindVirtualDocumentPath(aSocket,aRemovedDir,aDir,aID,aType,aLevel,aClass) then
    begin
      Mimetype := '';
      if aLevel=1 then //direkt auf Dataset ebene
        begin
          if aDir = 'list.json' then
            begin
              MimeType:='application/json';
              Stream.Position:=0;
              aJParser := TJSONParser.Create(Stream);
              aData := aJParser.Parse;
              {$IF FPC_FULLVERSION>20600}
              if (aData is TJSONArray) and Assigned(TJSONArray(aData)[0]) and Assigned(TJSONArray(aData)[0].FindPath('sql_id')) then
                begin
                  if aClass = TTimes then
                    aDataSet := TTimes.CreateEx(nil,Data,nil,Data.Users.DataSet)
                  else
                    aDataSet := aClass.Create(nil);
                  aDataSet.Select(TJSONArray(aData)[0].FindPath('sql_id').AsInt64);
                  aDataSet.Open;
                  if aDataSet.Count = 0 then
                    aDataSet.Append
                  else aDataSet.Edit;
                  try
                    NotFound := False;
                    for a := 0 to TJSONArray(aData)[0].Count-1 do
                      begin
                        if TJSONArray(aData)[0] is TJSONObject then
                          aField := TJSONObject(TJSONArray(aData)[0]).Names[a];
                        if Assigned(aDataSet.FieldByName(aField)) then
                          begin
                            if ((TJSONArray(aData)[0].Items[a].Value = '0')
                            or  (TJSONArray(aData)[0].Items[a].Value = '1'))
                            and (aDataSet.FieldByName(aField).Size=1) then
                              begin
                                if TJSONArray(aData)[0].Items[a].Value = '1' then
                                  aDataSet.FieldByName(aField).AsString:='Y'
                                else
                                  aDataSet.FieldByName(aField).AsString:='N';
                              end
                            else if (aDataSet.FieldByName(aField).DataType=ftDateTime) then
                              begin
                                if (TJSONArray(aData)[0].Items[a].Value='')
                                or (TJSONArray(aData)[0].Items[a].Value='null') then
                                  aDataSet.FieldByName(aField).Clear
                                else if (synautil.DecodeRfcDateTime(TJSONArray(aData)[0].Items[a].Value) <> 0) then
                                  aDataSet.FieldByName(aField).AsDateTime:=synautil.DecodeRfcDateTime(TJSONArray(aData)[0].Items[a].Value)
                                else
                                  aDataSet.FieldByName(aField).AsVariant:=TJSONArray(aData)[0].Items[a].Value;
                              end
                            else if aDataSet.FieldByName(aField).AsVariant<>TJSONArray(aData)[0].Items[a].Value then
                              begin
                                if (TJSONArray(aData)[0].Items[a].Value='null') then
                                  aDataSet.FieldByName(aField).Clear
                                else
                                  aDataSet.FieldByName(aField).AsVariant:=TJSONArray(aData)[0].Items[a].Value
                              end;
                          end
                        else if aField = 'sql_id' then
                          begin
                          if aDataSet.Id.AsVariant<>TJSONArray(aData)[0].Items[a].Value then
                            aDataSet.Id.AsVariant:=TJSONArray(aData)[0].Items[a].Value;
                          end
                        else NotFound := True;
                      end;
                    if aDataSet.CanEdit and (not NotFound) then
                      begin
                        try
                          aDataSet.Post;
                          FStatus:=200;
                        except
                          on e : Exception do
                            begin
                              FStatus := 409;
                              MimeType:='text/plain';
                              aDataSet.Cancel;
                              with BaseApplication as IBaseApplication do
                                Error('Error "'+e.Message+'"');
                              sl := TStringList.Create;
                              sl.Add(e.Message);
                              sl.SaveToStream(TDAVSession(aSocket).OutputData);
                              sl.Free;
                            end;
                        end;
                      end
                    else aDataSet.Cancel;
                    aDataSet.Free;
                  except
                    on e : Exception do
                      begin
                        FStatus := 409;
                        MimeType:='text/plain';
                        aDataSet.Cancel;
                        with BaseApplication as IBaseApplication do
                          Error('Error "'+e.Message+'"');
                        sl := TStringList.Create;
                        sl.Add(e.Message);
                        sl.SaveToStream(TDAVSession(aSocket).OutputData);
                        sl.Free;
                        aDataSet.Free;
                      end;
                  end;
                end;
              {$ENDIF}
              aJParser.Free;
              Result:=True;
            end
          else if aDir = 'new/item.json' then
            begin
              Stream.Position:=0;
              ss := TStringStream.Create('');
              ss.CopyFrom(Stream,0);
              try
                aDataSet := aClass.Create(nil);
                aDataSet.ImportFromJSON(ss.DataString);
                aDataSet.Free;
                result := True;
                FStatus:=200;
              except
                on e : Exception do
                  begin
                    FStatus := 409;
                    MimeType:='text/plain';
                    aDataSet.Cancel;
                    with BaseApplication as IBaseApplication do
                      Error('Error "'+e.Message+'"');
                    sl := TStringList.Create;
                    sl.Add(e.Message);
                    sl.SaveToStream(TDAVSession(aSocket).OutputData);
                    sl.Free;
                    aDataSet.Free;
                  end;
              end;
              ss.Free;
            end
          else if aDir = 'new/item.xml' then
            begin
              Stream.Position:=0;
              ss := TStringStream.Create('');
              ss.CopyFrom(Stream,0);
              try
                aDataSet := aClass.Create(nil);
                aDataSet.ImportFromXML(ss.DataString);
                aDataSet.Free;
                result := True;
                FStatus:=200;
              except
                on e : Exception do
                  begin
                    FStatus := 409;
                    MimeType:='text/plain';
                    aDataSet.Cancel;
                    with BaseApplication as IBaseApplication do
                      Error('Error "'+e.Message+'"');
                    sl := TStringList.Create;
                    sl.Add(e.Message);
                    sl.SaveToStream(TDAVSession(aSocket).OutputData);
                    sl.Free;
                    aDataSet.Free;
                  end;
              end;
              ss.Free;
            end;
        end
      else if aLevel=6 then //virtual directories (files,reports)
        begin
          aDocuments := TDocuments.Create(nil);
          aDocuments.Select(aID,aType,0);
          aDocuments.Open;
          if rpos('/',aDir) > 1 then
            Result := aDocuments.OpenPath(copy(aDir,0,rpos('/',aDir)-1),'/')
          else Result := True;
          aDocuments2 := TDocuments.Create(nil);
          aDocuments2.Select(aID,aType,0);
          aDocuments2.Open;
          if rpos('/',aDir) > 1 then
            Result := aDocuments2.OpenPath(copy(aDir,0,rpos('/',aDir)),'/')
          else Result := True;
          if Result then
            begin
              Result := False;
              aDocument := TDocument.Create(nil);
              aFileName := ExtractFileName(aDir);
              if ADocuments2.SelectFile(aFileName) then //Checkin existing File
                begin
                  aDocument.SelectByNumber(aDocuments2.DataSet.FieldByName('NUMBER').AsString);
                  aDocument.Open;
                  ForceDirectories(GetTempDir+'promettemp');
                  aFStream := TFileStream.Create(AppendPathDelim(GetTempDir+'promettemp')+aDocuments2.FileName,fmCreate);
                  Stream.Position:=0;
                  aFStream.CopyFrom(Stream,Stream.Size);
                  aFStream.Free;
                  aFiles := aDocument.CollectCheckInFiles(GetTempDir+'promettemp');
                  aDocument.CheckinFiles(aFiles,GetTempDir+'promettemp');
                  FStatus:=200;
                  Result := True;
                end
              else
                begin //Create new file
                  aDocument.BaseParent := aDocuments;
                  if rpos('.',aFileName) > 0 then
                    aDocument.AddFromStream(copy(aFileName,0,rpos('.',aFileName)-1),copy(ExtractFileExt(aFileName),2,length(aFileName)),Stream)
                  else
                    aDocument.AddFromStream(aFileName,'',Stream);
                  Result := True;
                  FStatus:=200;
                end;
                aDocument.Free;
            end;
          aDocuments.Free;
          aDocuments2.Free;
        end;
    end
  else fStatus := 404;
end;
function TPrometServerFunctions.ServerReadAllowed(aSocket: TDAVSession; aDir: string
  ): Boolean;
var
  aUser: String;
begin
  if aSocket=nil then exit;
  CreateDataModule(aSocket);
  if ((BaseApplication.HasOption('u','user') and (BaseApplication.HasOption('p','password'))
  and (TBaseDBModule(aSocket.Data).Users.Active)
  and ((TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString=BaseApplication.GetOptionValue('u','user')) or (TBaseDBModule(aSocket.Data).Users.FieldByName('LOGINNAME').AsString=BaseApplication.GetOptionValue('u','user')))
  and (TBaseDBModule(aSocket.Data).Users.CheckPasswort(trim(BaseApplication.GetOptionValue('p','password')))))
  ) then
    aSocket.User:=TBaseDBModule(aSocket.Data).Users.Id.AsString;
  Result := aSocket.User<>'';
  if (not Result) and (BaseApplication.HasOption('u','user') and (BaseApplication.HasOption('p','password'))) then
    begin
      TBaseDBModule(aSocket.Data).Users.Open;
      Result := (TBaseDBModule(aSocket.Data).Users.DataSet.Locate('NAME',BaseApplication.GetOptionValue('u','user'),[]) or TBaseDBModule(aSocket.Data).Users.DataSet.Locate('LOGINNAME',BaseApplication.GetOptionValue('u','user'),[])) and (TBaseDBModule(aSocket.Data).Users.Leaved.IsNull);
      if Result then
        Result := TBaseDBModule(aSocket.Data).Users.CheckPasswort(trim(BaseApplication.GetOptionValue('p','password')));
      if Result then
        aSocket.User:=TBaseDBModule(aSocket.Data).Users.Id.AsString;
    end;
  if (not Result) and (pos('login=',aSocket.Parameters.Values['cookie'])>0) then
    begin
      aUser := copy(aSocket.Parameters.Values['cookie'],pos('login=',aSocket.Parameters.Values['cookie'])+6,length(aSocket.Parameters.Values['cookie']));
      aUser := copy(aUser,0,pos(';',aUser)-1);
      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
      Result := TWebDAVMaster(aSocket.Creator).OnUserLogin(aSocket,copy(aUser,0,pos(':',aUser)-1),copy(aUser,pos(':',aUser)+1,length(aUser)));
    end;
  if pos('blobdata/',aDir)>0 then
    Result := True;
  if not Result then
    with BaseApplication as IBaseApplication do
      Info('DAV: read permitted to "'+aDir+'"');
end;
function TPrometServerFunctions.ServerUserLogin(aSocket: TDAVSession; aUser,
  aPassword: string): Boolean;
begin
  CreateDataModule(aSocket);
  if ((BaseApplication.HasOption('u','user') and (BaseApplication.HasOption('p','password'))
  and (TBaseDBModule(aSocket.Data).Users.Active)
  and ((TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString=BaseApplication.GetOptionValue('u','user')) or (TBaseDBModule(aSocket.Data).Users.FieldByName('LOGINNAME').AsString=BaseApplication.GetOptionValue('u','user')))
  and (TBaseDBModule(aSocket.Data).Users.CheckPasswort(trim(BaseApplication.GetOptionValue('p','password')))))
  ) then
    begin
      aSocket.User:=TBaseDBModule(aSocket.Data).Users.Id.AsString;
      Result := True;
      exit;
    end;
  TBaseDBModule(aSocket.Data).Users.Open;
  Result := (TBaseDBModule(aSocket.Data).Users.DataSet.Locate('NAME',aUser,[]) or TBaseDBModule(aSocket.Data).Users.DataSet.Locate('LOGINNAME',aUser,[])) and (TBaseDBModule(aSocket.Data).Users.Leaved.IsNull);
  if (BaseApplication.HasOption('u','user') and (BaseApplication.HasOption('p','password'))) then
    begin
      Result := (TBaseDBModule(aSocket.Data).Users.DataSet.Locate('NAME',BaseApplication.GetOptionValue('u','user'),[]) or TBaseDBModule(aSocket.Data).Users.DataSet.Locate('LOGINNAME',BaseApplication.GetOptionValue('u','user'),[])) and (TBaseDBModule(aSocket.Data).Users.Leaved.IsNull);
      if Result then
        Result := TBaseDBModule(aSocket.Data).Users.CheckPasswort(trim(BaseApplication.GetOptionValue('p','password')));
    end
  else if Result then
    Result := TBaseDBModule(aSocket.Data).Users.CheckPasswort(trim(aPassword));
  if not Result then
    begin
      aSocket.User:='';
      with BaseApplication as IBaseApplication do
        Info('Auth from "'+aUser+'" failed !');
    end
  else aSocket.User:=TBaseDBModule(aSocket.Data).Users.Id.AsString;
end;

procedure TPrometServerFunctions.aSocketDestroy(Sender: TObject);
begin
end;

function TPrometServerFunctions.FindVirtualDocumentPath(aSocket : TDAVSession;var aRemovedDir,
  aDir: string; var aID: Variant; var aType: string; var aLevel: Integer;
  var aClass: TBaseDBDatasetClass): Boolean;
var
  i: Integer;
  DataSet: TBaseDBList;
  tmp: String;
  aParent: Int64;
  aOldFilter: String;
  aOldRec: variant;

  procedure FindArticle;
  begin
    if pos('/',aDir)>0 then
      tmp := copy(aDir,0,pos('/',aDir)-1)
    else tmp := aDir;
    DataSet.SelectFromNumber(tmp);
    DataSet.Open;
    if DataSet.Count=0 then
      begin
        DataSet.Select(tmp);
        DataSet.Open;
      end;
    if Assigned(DataSet.Id) then
      aID:=DataSet.Id.AsVariant;
    aType:= DataSet.GetTyp;
    if DataSet.Count>0 then
      begin
        Result := True;
        aLevel:=3;
        if pos('/',aDir)>0 then
          begin
            aRemovedDir+=copy(aDir,0,pos('/',aDir));
            aDir := copy(aDir,pos('/',aDir)+1,length(aDir));
          end
        else
          begin
            aRemovedDir+=aDir;
            aDir := '';
          end;
      end;
    if (copy(aDir,0,5)='files') and (DataSet.Count>0) then
      begin
        Result := True;
        aLevel:=6;
        aRemovedDir+='files/';
        if pos('/',aDir)>0 then
          aDir := copy(aDir,pos('/',aDir)+1,length(aDir))
        else aDir := '';
      end;
    if (copy(aDir,0,7)='reports') and (DataSet.Count>0) then
      begin
        Result := True;
        aLevel:=6;
        aRemovedDir+='reports/';
        if pos('/',aDir)>0 then
          aDir := copy(aDir,pos('/',aDir)+1,length(aDir))
        else aDir := '';
      end;
  end;

begin
  aLevel:=0;
  Result := False;
  if copy(aDir,0,6)='/files' then
    begin
      aRemovedDir:='/files/';
      aDir := copy(aDir,8,length(aDir));
      aId := 1;
      aType := 'D';
      aLevel:=6;
      Result := True;
    end
  else
    begin
      for i := 0 to length(DatasetClasses)-1 do
        begin
          tmp := DatasetClasses[i].aName;
          tmp := copy(aDir,0,length(tmp)+1);
          if (DatasetClasses[i].aClass.InheritsFrom(TBaseDBList)
          and (tmp='/'+lowercase(DatasetClasses[i].aName))) then
            begin
              aClass:=DatasetClasses[i].aClass;
              aLevel := 1;
              DataSet := TBaseDbList(TBaseDbListClass(DatasetClasses[i].aClass).Create(nil));
              aType:=DataSet.GetTyp;
              Result:=True;
              if pos('/',copy(aDir,2,length(aDir)))>0 then
                aRemovedDir:=copy(aDir,0,pos('/',copy(aDir,2,length(aDir)))+1)
              else aRemovedDir:=aDir;
              aDir := copy(aDir,length(aRemovedDir)+1,length(aDir));
              if (copy(aDir,0,pos('/',aDir)-1)='by-id') or (aDir = 'by-id') then
                begin
                  aLevel:=2;
                  aRemovedDir+='by-id/';
                  aDir := copy(aDir,pos('/',aDir)+1,length(aDir));
                  if aDir = 'by-id' then
                    begin
                      aDir := '';
                    end;
                  if length(aDir)>0 then
                    begin
                      if Assigned(DataSet) then
                        begin
                          FindArticle;
                          Result := True;
                        end;
                    end;
                end
              else //Verzeichnis aus TBaseDBModule(aSocket.Data).Tree??
                begin
                  if pos('/',aDir)>0 then
                    tmp := copy(aDir,0,pos('/',aDir)-1)
                  else tmp := aDir;
                  TBaseDBModule(aSocket.Data).Tree.Open;
                  aParent := 0;
                  aOldFilter := '';
                  TBaseDBModule(aSocket.Data).Tree.DataSet.Filter:=TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aType)+' AND '+TBaseDBModule(aSocket.Data).QuoteField('PARENT')+'='+IntToStr(aParent);
                  TBaseDBModule(aSocket.Data).Tree.DataSet.Filtered:=True;
                  TBaseDBModule(aSocket.Data).Tree.DataSet.First;
                  while TBaseDBModule(aSocket.Data).Tree.DataSet.Locate('NAME',tmp,[]) do
                    begin
                      aOldRec := TBaseDBModule(aSocket.Data).Tree.GetBookmark;
                      aLevel:=4;//Tree Dir
                      aRemovedDir+=tmp+'/';
                      aDir := copy(aDir,length(tmp)+2,length(aDir));
                      aParent := TBaseDBModule(aSocket.Data).Tree.FieldByName('SQL_ID').AsLargeInt;
                      aOldFilter:=TBaseDBModule(aSocket.Data).Tree.DataSet.Filter;
                      TBaseDBModule(aSocket.Data).Tree.DataSet.Filter:=TBaseDBModule(aSocket.Data).QuoteField('TYPE')+'='+TBaseDBModule(aSocket.Data).QuoteValue(aType)+' AND '+TBaseDBModule(aSocket.Data).QuoteField('PARENT')+'='+IntToStr(aParent);
                      TBaseDBModule(aSocket.Data).Tree.DataSet.First;
                      if pos('/',aDir)>0 then
                        tmp := copy(aDir,0,pos('/',aDir)-1)
                      else tmp := aDir;
                    end;
                  TBaseDBModule(aSocket.Data).Tree.DataSet.Filter:=aOldFilter;
                  TBaseDBModule(aSocket.Data).Tree.GotoBookmark(aOldRec);
                  if IsNumeric(tmp) then
                    FindArticle;
                end;
              DataSet.Free;
              break;
            end;
        end;
    end;
end;
procedure TPrometServerFunctions.CreateDataModule(aSocket: TDAVSession);
var
  aType: TBaseDBModuleClass;
begin
  if Assigned(aSocket.Data) then exit;
  {
  aType := TBaseDBModuleClass(uData.Data.ClassType);
  aSocket.Data := aType.Create(aSocket);
  with TBaseDBModule(aSocket.Data) do
    begin
      SetProperties(uData.Data.Properties);
    end;
  }
  aSocket.Data := uData.Data;
  //TODO:select rigth User
  if not Assigned(aSocket.Data) then exit;
  if not Assigned(aSocket) then exit;
  if not Assigned(TBaseDBModule(aSocket.Data).Users) then exit;
  if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then
    begin
      TBaseDBModule(aSocket.Data).Users.Filter('',0);
      if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then exit;
    end;
  TBaseDBModule(aSocket.Data).RefreshUsersFilter;
  TBaseDBModule(aSocket.Data).RegisterLinkHandlers;
end;
function TPrometServerFunctions.AddDocumentsToFileList(aSocket: TDAVSession;
  aFileList: TDAVDirectoryList; aDocuments: TDocuments; aPath, aFilter: string
  ): Boolean;
var
  aFile: TDAVFile;
  lIndex: Integer;
begin
  Result := False;
  aDocuments.DataSet.First;
  while not aDocuments.DataSet.EOF do
    begin
      if (aFilter = '') or (aDocuments.FileName=aFilter) then
        begin
          AddDocumentToFileList(aSocket,aFileList,aDocuments,aPath+aDocuments.FileName);
          Result := True;
        end;
      aDocuments.DataSet.Next;
    end;
end;
procedure TPrometServerFunctions.AddDocumentToFileList(aSocket : TDAVSession;aFileList: TDAVDirectoryList;
  aDocuments: TDocuments;FullPath : string);
var
  aFile: TDAVFile;
  lIndex: Integer;
begin
  aFile := TDAVFile.Create(FullPath,aDocuments.IsDir);
  if aSocket.User='' then exit;
  CreateDataModule(aSocket);
  if not aDocuments.IsDir then
    begin
      //TODO:fix this
      {
      if Assigned(MimeList) then
        begin
          lIndex := MimeList.IndexOf(ExtractFileExt(aDocuments.FileName));
          if lIndex >= 0 then
            aFile.Properties.Values['getcontenttype'] := TStringObject(MimeList.Objects[lIndex]).Str;
        end;
      }
      if aFile.Properties.Values['getcontenttype'] = '' then
        aFile.Properties.Values['getcontenttype'] := GetMimeTypeforExtension(ExtractFileExt(aDocuments.FileName));
      if aFile.Properties.Values['getcontenttype'] = '' then
        aFile.Properties.Values['getcontenttype'] := 'text/plain';
      aFile.Properties.Values['creationdate'] := BuildISODate(aDocuments.CreationDate);
      aFile.Properties.Values['getlastmodified'] := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss',LocalTimeToGMT(aDocuments.LastModified),WebFormatSettings)+' GMT';
      aFile.Properties.Values['getcontentlength'] := IntToStr(max(aDocuments.Size,0));
      aFile.Properties.Values['getetag'] := aDocuments.Number.AsString+IntToStr(trunc(frac(aDocuments.TimeStamp.AsDateTime)*1000));
      aFile.CurrentUser:='/users/'+TBaseDBModule(aSocket.Data).Users.FieldByName('NAME').AsString;
    end;
  aFile.Properties.Values['creationdate'] := BuildISODate(aDocuments.CreationDate);
  aFileList.Add(aFile);
end;

initialization
  RegisterdataSetClass('STATISTICS',TStatistic);
end.

