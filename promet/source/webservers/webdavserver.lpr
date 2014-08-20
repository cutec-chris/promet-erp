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
program webdavserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp, uBaseCustomApplication, lnetbase,
  lNet, uBaseDBInterface, md5,uData,
  pmimemessages,fileutil,lconvencoding,uBaseApplication,
  ulsvnserver, ulwebdavserver, uDocuments, Utils,lMimeTypes,lHTTPUtil,lCommon,lexthttp,
  DateUtils,uimpvcal,uCalendar,uWiki,synautil,uBaseDbClasses,Variants,utask;
type
  TSVNServer = class(TBaseCustomApplication)
    procedure ServerAccess(AMessage: string);
    function ServerDelete(aDir: string): Boolean;
    function ServerGetDirectoryList(aDir: string;aDepth : Integer; var aDirList: TLDirectoryList
      ) : Boolean;
    function ServerGetFile(aDir: string; Stream: TStream;var LastModified : TDateTime;var MimeType : string;var eTag : string): Boolean;
    function ServerMkCol(aDir: string): Boolean;
    function ServerPutFile(aDir: string; Stream: TStream; var eTag: string; var FStatus : TLHttpStatus
      ): Boolean;
    function ServerReadAllowed(aDir: string): Boolean;
    function ServerUserLogin(aUser, aPassword: string): Boolean;
  private
    Server : TLSVNServer;
    procedure AddDocumentsToFileList(aFileList : TLDirectoryList;aDocuments : TDocuments);
    procedure AddDocumentToFileList(aFileList : TLDirectoryList;aDocuments : TDocuments);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
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
procedure TSVNServer.ServerAccess(AMessage: string);
begin
  writeln(AMessage);
end;
function TSVNServer.ServerDelete(aDir: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aFullDir: String;
  aFile: String;
  aParent,aDirPar : Variant;
  aDirs: TTree;
  aCal: TCalendar;
begin
  if copy(aDir,0,1)<>'/' then
    aDir := '/'+aDir;
  if copy(aDir,0,10) = '/calendars' then
    begin
      aFullDir := aDir;
      aDir := copy(aDir,12,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aFile := StringReplace(copy(aDir,rpos('/',aDir)+1,length(aDir)),'.ics','',[]);
      aDir := copy(aDir,0,rpos('/',aDir)-1);
      if aDir = 'home' then
        begin
          aParent := Data.Users.Id.AsVariant;
          result := True;
        end
      else
        begin
          aDirs := TTree.Create(nil,Data);
          aDirs.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('C'));
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
      aCal := TCalendar.Create(nil,Data);
      aCal.Filter(Data.QuoteField('ORIGID')+'='+Data.QuoteValue(aFile));
      if (aCal.Count=0) and IsNumeric(aFile) then
        begin
          aCal.Select(aFile);
          aCal.Open;
        end;
      Result := aCal.Count=1;
      if Result then
        begin
          aCal.Delete;
        end;
      aCal.Free;
    end
  else
    begin
      aDocuments := TDocuments.Create(nil,Data);
      aDocuments.Select(1,'D',0);
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
              aDocument := TDocument.Create(nil,Data);
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
function TSVNServer.ServerGetDirectoryList(aDir: string; aDepth: Integer;
  var aDirList: TLDirectoryList): Boolean;
var
  aItem: TLFile;
  aDocuments: TDocuments;
  aFile: String;
  aCal: TCalendar;
  sl: TStringList;
  Stream: TMemoryStream;
  aDirs: TTree;
  aFullDir: String;
  IsCalendarUser: Boolean = false;
  aTasks: TTaskList;
begin
  Result := false;
  if aDir = '/' then
    begin
      Result := True;
      aDirList := TLDirectoryList.Create;
      aItem := TLFile.Create('calendars',True);
      aItem.IsCalendar:=True;
      aDocuments := TDocuments.Create(nil,Data);
      aDocuments.Select(1,'D',0);
      aDocuments.Open;
      AddDocumentsToFileList(aDirList,aDocuments);
      aDocuments.Free;
      aDirList.Add(aItem);
    end
  else if copy(aDir,0,10) = '/calendars' then
    begin
      aFullDir := aDir;
      aDir := copy(aDir,12,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      if Data.Users.DataSet.Active then
        begin
          if aDir = '' then
            aDirList := TLDirectoryList.Create
          else aDirList:=nil;
          //Add ics file
          aItem := TLFile.Create(Data.Users.Text.AsString+'.ics',False);
          if (aDir = aItem.Name) or (aDir = '') then
            begin
              aItem.Properties.Values['getcontenttype'] := 'text/calendar';
              aItem.Properties.Values['creationdate'] := BuildISODate(Now());
              aItem.Properties.Values['getlastmodified'] := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss',LocalTimeToGMT(Now()),WebFormatSettings)+' GMT';
              sl := TStringList.Create;
              aCal := TCalendar.Create(nil,Data);
              aCal.SelectByUser(Data.Users.Accountno.AsString);
              aCal.Open;
              VCalExport(aCal,sl);
              aCal.Free;
              Stream := TMemoryStream.Create;
              sl.SaveToStream(Stream);
              aItem.Properties.Values['getcontentlength'] := IntToStr(Stream.Size);
              Stream.Free;
              sl.Free;
              if Assigned(aDirList) then
                aDirList.Add(aItem)
              else aDirList := aItem;
            end
          else aItem.Free;
          if (copy(aDir,RPos('/',aDir)+1,length(aDir)) = 'user') then
            begin
              IsCalendarUser := True;
              aDir := copy(aDir,0,rpos('/',aDir)-1);
            end;
          //Add CalDAV Calendars
          aDirs := TTree.Create(nil,Data);
          aDirs.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('C'));
          aItem := TLFile.Create('home',True);
          if (aDir = aItem.Name) or (aDir = '') then
            begin
              aItem.IsCalendar:=True;
              aItem.IsTodoList:=True;
              aItem.IsCalendarUser:=IsCalendarUser;
              //Select last SQL_ID as ctag
              aCal := TCalendar.Create(nil,Data);
              aCal.SelectByUser(Data.Users.Accountno.AsString);
              aCal.ActualLimit:=1;
              aCal.SortFields:='TIMESTAMPD';
              aCal.SortDirection:=sdDescending;
              aCal.Open;
              aItem.Properties.Values['CS:getctag'] := aCal.Id.AsString;
              //aItem.Properties.Values['D:getetag'] := Data.Users.Id.AsString;
              aItem.Properties.Values['D:getcontenttype'] := 'text/calendar';
              if Data.Users.FieldByName('EMAIL').AsString<>'' then
                aItem.UserAdressSet.Add('mailto:'+Data.Users.FieldByName('EMAIL').AsString);
              aItem.UserAdressSet.Add('/calendars/');
              aItem.CalendarHomeSet:='/calendars/';
              if Assigned(aDirList) then
                aDirList.Add(aItem)
              else aDirList := aItem;
              if aDepth>0 then
                begin
                  aCal.SelectByIdAndTime(Data.Users.Id.AsVariant,Now(),Now()+90); //3 month in future
                  aCal.ActualLimit:=100;
                  aCal.Open;
                  while not aCal.EOF do
                    begin
                      if aCal.FieldByName('ORIGID').AsString<>'' then
                        aItem := TLFile.Create(aCal.FieldByName('ORIGID').AsString+'.ics')
                      else
                        aItem := TLFile.Create(aCal.Id.AsString+'.ics');
                      aItem.Properties.Values['D:getetag'] := aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
                      aItem.Properties.Values['D:getcontenttype'] := 'text/calendar; component=vevent';
                      aDirList.Add(aItem);
                      aCal.Next;
                    end;
                  aTasks := TTaskList.Create(nil,Data);
                  aTasks.SelectActiveByUser(Data.Users.Accountno.AsString);
                  aTasks.Open;
                  while not aTasks.EOF do
                    begin
                      if aTasks.FieldByName('ORIGID').AsString<>'' then
                        aItem := TLFile.Create(aTasks.FieldByName('ORIGID').AsString+'.ics')
                      else
                        aItem := TLFile.Create(aTasks.Id.AsString+'.ics');
                      aItem.Properties.Values['D:getetag'] := aTasks.Id.AsString+IntToStr(trunc(frac(aTasks.TimeStamp.AsDateTime)*1000));
                      aItem.Properties.Values['D:getcontenttype'] := 'text/calendar; component=vtodo';
                      aDirList.Add(aItem);
                      aTasks.Next;
                    end;
                  aTasks.Free;
                end;
              aCal.Free;
            end
          else aItem.Free;
          while not aDirs.EOF do
            begin
              aItem := TLFile.Create(aDirs.Text.AsString,True);
              if (aDir = aItem.Name) or (aDir = '') then
                begin
                  aItem.IsCalendar:=True;
                  aItem.IsCalendarUser:=IsCalendarUser;
                  aCal := TCalendar.Create(nil,Data);
                  aCal.Filter(Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(aDirs.Id.AsString));
                  aItem.Properties.Values['CS:getctag'] := aCal.Id.AsString;
                  //aItem.Properties.Values['D:getetag'] := aDirs.Id.AsString;
                  aItem.Properties.Values['D:getcontenttype'] := 'text/calendar';
                  aItem.CalendarHomeSet:=aDir;
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
                            aItem := TLFile.Create(aCal.FieldByName('ORIGID').AsString+'.ics')
                          else
                            aItem := TLFile.Create(aCal.Id.AsString+'.ics');
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
  else
    begin
      aDirList := TLDirectoryList.Create;
      aDocuments := TDocuments.Create(nil,Data);
      aDocuments.Select(1,'D',0);
      if copy(aDir,length(aDir),1) <> '/' then
        aDir := aDir+'/';
      if aDocuments.OpenPath(aDir,'/') then
        begin
          AddDocumentsToFileList(aDirList,aDocuments);
          Result := True;
        end
      else
        begin
          aDir := copy(aDir,0,length(aDir)-1);
          aFile := HTTPDecode(copy(aDir,rpos('/',aDir)+1,length(aDir)));
          aDir := copy(aDir,0,rpos('/',aDir));
          if (aDir = '') or (aDir = '/') or aDocuments.OpenPath(aDir,'/') then
            begin
              aDocuments.DataSet.First;
              while not aDocuments.DataSet.EOF do
                begin
                  if aDocuments.FileName = aFile then
                    AddDocumentToFileList(aDirList,aDocuments);
                  aDocuments.DataSet.Next;
                end;
              Result := True;
            end;
        end;
      aDocuments.Free;
    end;
end;
function TSVNServer.ServerGetFile(aDir: string; Stream: TStream;
  var LastModified: TDateTime; var MimeType: string; var eTag: string): Boolean;
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
begin
  if aDir = 'calendars/'+Data.Users.Text.AsString+'.ics' then
    begin
      sl := TStringList.Create;
      aCal := TCalendar.Create(nil,Data);
      aCal.SelectByUser(Data.Users.Accountno.AsString);
      aCal.Open;
      VCalExport(aCal,sl);
      sl.SaveToStream(Stream);
      aCal.Free;
      sl.Free;
      Result := True;
    end
  else  if copy(aDir,0,10) = '/calendars' then
    begin
      aFullDir := aDir;
      aDir := copy(aDir,12,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aFile := StringReplace(copy(aDir,rpos('/',aDir)+1,length(aDir)),'.ics','',[]);
      aDir := copy(aDir,0,rpos('/',aDir)-1);
      if aDir = 'home' then
        begin
          aParent := Data.Users.Id.AsVariant;
          result := True;
        end
      else
        begin
          aDirs := TTree.Create(nil,Data);
          aDirs.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('C'));
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
      aCal := TCalendar.Create(nil,Data);
      aCal.Filter(Data.QuoteField('ORIGID')+'='+Data.QuoteValue(aFile));
      if (aCal.Count=0) and IsNumeric(aFile) then
        begin
          aCal.Select(aFile);
          aCal.Open;
        end;
      Result := aCal.Count=1;
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
          aTasks := TTaskList.Create(nil,Data);
          if IsNumeric(aFile) then
            begin
              aTasks.Select(aFile);
              aTasks.Open;
              Result := aTasks.Count=1;
              if Result then
                begin
                  eTag:=aTasks.Id.AsString+IntToStr(trunc(frac(aTasks.TimeStamp.AsDateTime)*1000));
                  sl := TStringList.Create;
                  VTodoExport(aTasks,sl);
                  sl.SaveToStream(Stream);
                  sl.Free;
                end;
            end;
          aTasks.Free;
        end;
    end

  else
    begin
      Mimetype := '';
      aDocuments := TDocuments.Create(nil,Data);
      aDocuments.Select(1,'D',0);
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
              aDocument := TDocument.Create(nil,Data);
              aDocument.SelectByNumber(aDocuments.DataSet.FieldByName('NUMBER').AsVariant);
              aDocument.Open;
              if aDocument.Count > 0 then
                begin
                  aDocument.CheckoutToStream(Stream);
                  LastModified:=aDocument.LastModified;
                  if Assigned(MimeList) then
                    begin
                      lIndex := MimeList.IndexOf(ExtractFileExt(aDocuments.FileName));
                      if lIndex >= 0 then
                        MimeType := TStringObject(MimeList.Objects[lIndex]).Str;
                    end;
                  if MimeType = '' then
                    MimeType := GetMimeTypeforExtension(ExtractFileExt(aDocuments.FileName));
                  Result := True;
                end;
              aDocument.Free;
            end;
        end;
    end;
end;
function TSVNServer.ServerMkCol(aDir: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  Subfolder: Boolean = False;
begin
  aDocuments := TDocuments.Create(nil,Data);
  aDocuments.Select(1,'D',0);
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
      aDocument := TDocument.Create(nil,Data);
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
function TSVNServer.ServerPutFile(aDir: string; Stream: TStream;
  var eTag: string; var FStatus: TLHttpStatus): Boolean;
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
begin
  if copy(aDir,0,1)<>'/' then
    aDir := '/'+aDir;
  if copy(aDir,0,10) = '/calendars' then
    begin
      aFullDir := aDir;
      aDir := copy(aDir,12,length(aDir));
      if copy(aDir,length(aDir),1) = '/' then
        aDir := copy(aDir,0,length(aDir)-1);
      aFile := StringReplace(copy(aDir,rpos('/',aDir)+1,length(aDir)),'.ics','',[]);
      aDir := copy(aDir,0,rpos('/',aDir)-1);
      if aDir = 'home' then
        begin
          aParent := Data.Users.Id.AsVariant;
          result := True;
        end
      else
        begin
          aDirs := TTree.Create(nil,Data);
          aDirs.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('C'));
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
      sl := TStringList.Create;
      Stream.Position:=0;
      sl.LoadFromStream(Stream);
      aCal := TCalendar.Create(nil,Data);
      aCal.Filter(Data.QuoteField('ORIGID')+'='+Data.QuoteValue(aFile));
      if (aCal.Count=0) and IsNumeric(aFile) then
        begin
          aCal.Select(aFile);
          aCal.Open;
        end;
      Result := aCal.Count=1;
      if Result then
        begin
          eTag:=aCal.Id.AsString+IntToStr(trunc(frac(aCal.TimeStamp.AsDateTime)*1000));
          aCal.Edit;
          if not VCalImport(aCal,sl,True) then
            Result := False;
        end
      else
        begin
          //todo ???
          aTasks := TTaskList.Create(nil,Data);
          aTasks.Filter(Data.QuoteField('ORIGIDS')+'='+Data.QuoteValue(aFile));
          if (aTasks.Count=0) and IsNumeric(aFile) then
            begin
              aTasks.Select(aFile);
              aTasks.Open;
              Result := aTasks.Count=1;
              if Result then  //edit task
                begin
                  eTag:=aTasks.Id.AsString+IntToStr(trunc(frac(atasks.TimeStamp.AsDateTime)*1000));
                  aTasks.Edit;
                  if not VTodoImport(aTasks,sl,True) then
                    Result := False;
                end
            end;
          if (not Result) and (pos(':vtodo',lowercase(sl.Text))>0) then //new task
            begin
              Result := True;
              aTasks.Insert;
              if not VTodoImport(aTasks,sl,True) then
                result := False;
              eTag:=aTasks.Id.AsString+IntToStr(trunc(frac(aTasks.TimeStamp.AsDateTime)*1000));
              FStatus:=hsCreated;
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
              FStatus:=hsCreated;
            end;
        end;
      aCal.Free;
      sl.Free;
    end
  else
    begin
      aDocuments := TDocuments.Create(nil,Data);
      aDocuments.Select(1,'D',0);
      aDocuments.Open;
      if rpos('/',aDir) > 1 then
        Result := aDocuments.OpenPath(copy(aDir,0,rpos('/',aDir)-1),'/')
      else Result := True;
      aDocuments2 := TDocuments.Create(nil,Data);
      aDocuments2.Select(1,'D',0);
      aDocuments2.Open;
      if rpos('/',aDir) > 1 then
        Result := aDocuments2.OpenPath(copy(aDir,0,rpos('/',aDir)),'/')
      else Result := True;
      if Result then
        begin
          Result := False;
          aDocument := TDocument.Create(nil,Data);
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
            end;
            aDocument.Free;
        end;
      aDocuments.Free;
      aDocuments2.Free;
    end;
end;
function TSVNServer.ServerReadAllowed(aDir: string): Boolean;
begin
  Result := Data.Users.DataSet.Active;
end;
function TSVNServer.ServerUserLogin(aUser, aPassword: string): Boolean;
begin
  Data.Users.Open;
  Result := (Data.Users.DataSet.Locate('NAME',aUser,[]) or Data.Users.DataSet.Locate('LOGINNAME',aUser,[])) and (Data.Users.Leaved.IsNull);
  if Result then
    Result := Data.Users.CheckPasswort(aPassword);
  if not Result then Data.Users.DataSet.Close;
end;
procedure TSVNServer.AddDocumentsToFileList(aFileList: TLDirectoryList;
  aDocuments: TDocuments);
var
  aFile: TLFile;
  lIndex: Integer;
begin
  aDocuments.DataSet.First;
  while not aDocuments.DataSet.EOF do
    begin
      AddDocumentToFileList(aFileList,aDocuments);
      aDocuments.DataSet.Next;
    end;
end;

procedure TSVNServer.AddDocumentToFileList(aFileList: TLDirectoryList;
  aDocuments: TDocuments);
var
  aFile: TLFile;
  lIndex: Integer;
begin
  aFile := TLFile.Create(aDocuments.FileName,aDocuments.IsDir);
  if not aDocuments.IsDir then
    begin
      if Assigned(MimeList) then
        begin
          lIndex := MimeList.IndexOf(ExtractFileExt(aDocuments.FileName));
          if lIndex >= 0 then
            aFile.Properties.Values['getcontenttype'] := TStringObject(MimeList.Objects[lIndex]).Str;
        end;
      if aFile.Properties.Values['getcontenttype'] = '' then
        aFile.Properties.Values['getcontenttype'] := GetMimeTypeforExtension(ExtractFileExt(aDocuments.FileName));
      if aFile.Properties.Values['getcontenttype'] = '' then
        aFile.Properties.Values['getcontenttype'] := 'text/plain';
      aFile.Properties.Values['creationdate'] := BuildISODate(aDocuments.CreationDate);
      aFile.Properties.Values['getlastmodified'] := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss',LocalTimeToGMT(aDocuments.LastModified),WebFormatSettings)+' GMT';
      aFile.Properties.Values['getcontentlength'] := IntToStr(aDocuments.Size);
      aFile.Properties.Values['getetag'] := aDocuments.Number.AsString+IntToStr(trunc(frac(aDocuments.TimeStamp.AsDateTime)*1000));
    end;
  aFile.Properties.Values['creationdate'] := BuildISODate(aDocuments.CreationDate);
  aFileList.Add(aFile);
end;

procedure TSVNServer.DoRun;
var
  y,m,d,h,mm,s,ss: word;
begin
  with Self as IBaseDBInterface do
    DBLogout;
  while not Terminated do
    begin
      Server.CallAction;
      Sleep(10);
    end;
  // stop program loop
  Terminate;
end;
constructor TSVNServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Server := TLSVNServer.Create(Self);
  Server.OnAccess:=@ServerAccess;
  Server.OnGetDirectoryList:=@ServerGetDirectoryList;
  Server.OnMkCol:=@ServerMkCol;
  Server.OnDelete:=@ServerDelete;
  Server.OnPutFile:=@ServerPutFile;
  Server.OnGetFile:=@ServerGetFile;
  Server.OnReadAllowed:=@ServerReadAllowed;
  Server.OnWriteAllowed:=@ServerReadAllowed;
  Server.OnUserLogin:=@ServerUserLogin;
  Login;
  if not Server.Listen(8085) then raise Exception.Create('Cant bind to port !');
  Data.Users.DataSet.Close;
end;
destructor TSVNServer.Destroy;
begin
  Server.Free;
  inherited Destroy;
end;
var
  Application: TSVNServer;
{$R *.res}
begin
  Application:=TSVNServer.Create(nil);
  Application.Run;
  Application.Free;
end.

