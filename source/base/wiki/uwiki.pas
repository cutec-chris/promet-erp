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
unit uWiki;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uDocuments,
  uIntfStrConsts,WikiToHtml,ubasedatasetinterfaces2,rtf2html;
type
  TKeywords = class(TBaseDbDataSet)
  private
    FRef_ID_ID : Int64;
    FKeyword : string;
  published
    property Ref_ID_ID: Int64 read FRef_ID_ID write FRef_ID_ID;
    property Keyword: string index 60 read FKeyword write FKeyword;
  end;

  { TWikiList }

  TWikiList = class(TBaseDBList)
    procedure BasicWikiInclude(Inp: string; var Outp: string; aLevel: Integer=0
      );
    procedure WikiListWikiLink(Inp: string; var Outp: string; aLevel: Integer=0
      );
  private
    FTreeEntry : Int64;
    FName,FCaption,FLanguage,FCreatedBy,FChangedBy,FData : string;
    FActiveTreeID: Variant;
    FKeywords: TKeywords;
    FKeywordDS : TDataSource;
    FOutDir,FOutSub,FOutExt : string;
    FOutTodo: TStringList;
    FVariables : TStringList;
    aDataThere : Boolean;
  protected
  public
    function GetTyp: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName: string;override;
    function FindWikiPage(PageName : string;aDocreate : Boolean = False) : Boolean;
    function FindWikiFolder(PageName : string) : Boolean;
    function GetFullPath : string;
    function isDynamic : Boolean;
    function PageAsHtml(OnlyBody: Boolean=False;UsePath : Boolean = True): string;
    function GenerateKeywords : string;
    function GenerateDescription : string;
    function PageAsText : string;
    property ActiveTreeID : Variant read FActiveTreeID;
    constructor Create(aOwner: TPersistent); override;
    destructor Destroy; override;
    property Keywords : TKeywords read FKeywords;
    function ExportToHTML(aFile: string; aInclude: TWikiIncludeFunc): Boolean;
    property Variables : TStringList read FVariables;
  published
    property TreeEntry: Int64 read FTreeEntry write FTreeEntry;
    property Name: string index 40 read FName write FName;
    property Caption: string index 120 read FCaption write FCaption;
    property Language: string index 3 read FLanguage write FLanguage;
    property CreatedBy: string index 4 read FCreatedBy write FCreatedBy;
    property ChangedBy: string index 4 read FChangedBy write FChangedBy;
    property Data: string read FData write FData;
  end;
implementation
uses Variants,htmltowiki,Utils,uData;

procedure TWikiList.BasicWikiInclude(Inp: string; var Outp: string;
  aLevel: Integer=0);
var
  //aList: TMessageList;
  //aMessage: TMessage;
  aCount : Integer;
  ss: TStringStream;
  aNewList: TWikiList;
  FSQLStream: TStringStream;
  //FSQLScanner: TSQLScanner;
  //FSQLParser: TSQLParser;
  //bStmt: TSQLElement;
  //aTableName: TSQLStringType;
  aClass: TBaseDBDatasetClass;
  aDs: TBaseDbDataSet;
  //aFilter: TSQLStringType;
  aRight: String;
  aLimit: Integer = 10;
  i: Integer;
  aLimitS: String;
  //aElem: TSQLElement;
  //aName: TSQLStringType;
  //aStatistic: TStatistic;
  aSQL: String;
  aRDs: TDataSet = nil;
  tmp: String;
  IncHeader: Boolean;
  aConditionOK : Boolean = True;
  aCondition: String = '';
  aTmpFloat: Extended;
  IsForm: Boolean;
  aInclude: String;
  nInp: String;
  ConvertRTF: Boolean = False;
  //aScript: TBaseScript;
  bScript: String;
  Found: Boolean;
  procedure BuildLinkRow(aBDS : TDataSet);
  var
    aLink: String;
  begin
    //aLink := TBaseDBModule(DataModule).BuildLink(aBDS);
    //Outp+='<li><a href="'+aLink+'" title="'+TBaseDBModule(DataModule).GetLinkDesc(aLink)+#10+TBaseDBModule(DataModule).GetLinkLongDesc(aLink)+'">'+HTMLEncode(TBaseDBModule(DataModule).GetLinkDesc(aLink))+'</a></li>';
  end;
  {
  function BuildTableRow(aBDS : TDataSet;aStmt : TSQLElement) : string;
  var
    aLink: String;
    i: Integer;
    a: Integer;
    aName: TSQLStringType;
    aElem: TSQLElement;
    aLinkBase: String;
  begin
    result := '';
    aLink := TBaseDBModule(DataModule).BuildLink(aBDS);
    Result+='<tr valign="top">';
    if TSQLSelectStatement(aStmt).All then
      begin
        for a := 0 to aDS.DataSet.FieldCount-1 do
          begin
            if (aBDS.Fields[a].DataType=ftString) or (aBDS.Fields[a].DataType=ftMemo) or (aBDS.Fields[a].DataType=ftWideString) then
              Result+='<td align="left">'+aBDS.Fields[a].AsString+'</td>'
            else
              Result+='<td align="left">'+aBDS.Fields[a].Text+'</td>';
          end;
      end
    else
      begin
        for i := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
          begin
            aElem := TSQLSelectStatement(aStmt).Fields[i];
            if aElem is TSQLSelectField then
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([]);
                if copy(uppercase(aName),0,5)='LINK(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aLinkBase := copy(aName,0,pos('(',aName)-1);
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                        aLink := aLinkBase+copy(aLink,pos('@',aLink),length(aLink));
                      end;
                    if pos('.',aName)>0 then
                      aName := copy(aName,rpos('.',aName)+1,length(aName));
                    if (aBDS.FieldDefs.IndexOf(aName)>-1) then
                      Result+='<td align="left"><a href="'+aLink+'" title="'+TBaseDBModule(DataModule).GetLinkDesc(aLink)+#10+TBaseDBModule(DataModule).GetLinkLongDesc(aLink)+'">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</a></td>'
                  end
                else if copy(uppercase(aName),0,4)='RTF(' then
                  begin
                    aName := copy(aName,5,length(aName)-5);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td align="left">'+HTMLEncode(RtfToHtml(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString))+'</td>';
                  end
                else if copy(uppercase(aName),0,5)='ICON(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td align="left"><img src="ICON('+aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString+')"></img></td>';
                  end
                else if copy(uppercase(aName),0,12)='HISTORYICON(' then
                  begin
                    aName := copy(aName,12,length(aName)-11);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td align="left"><img src="HISTORYICON('+aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString+')"></img></td>';
                  end
                else if (aBDS.FieldDefs.IndexOf(copy(aName,rpos('.',aName)+1,length(aName)))>-1) then
                  begin
                    if pos('.',aName)>0 then
                      aName := copy(aName,rpos('.',aName)+1,length(aName));
                    if (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftFloat)
                    or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftInteger)
                    or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftDateTime)
                    then
                      Result+='<td align="right">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].Text)+'</td>'
                    else
                      Result+='<td align="left">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</td>';
                  end
                else if Assigned(TSQLSelectField(aElem).AliasName) then
                  begin
                    aName := TSQLSelectField(aElem).AliasName.GetAsSQL([]);
                    if (aBDS.FieldDefs.IndexOf(aName)>-1) then
                      begin
                        if (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftString) or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftMemo) or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftWideString) then
                          Result+='<td align="left">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</td>'
                        else
                          Result+='<td align="left">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].Text)+'</td>';
                      end;
                  end;
              end;
          end;
      end;
    Result+='</tr>';
  end;
  }
  {
  procedure AddHeader(aStmt : TSQLElement);
  var
    b: Integer;
  begin
    Outp+='<thead><tr>';
    for b := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
      begin
        aElem := TSQLSelectStatement(aStmt).Fields[b];
        if aElem is TSQLSelectField then
          begin
            if Assigned(TSQLSelectField(aElem).AliasName) then
              Outp+='<th><b>'+HTMLEncode(StringReplace(TSQLSelectField(aElem).AliasName.GetAsSQL([sfoSingleQuoteIdentifier]),'''','',[rfReplaceAll]))+'</b></th>'
            else
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([sfoSingleQuoteIdentifier]);
                if copy(uppercase(aName),0,5)='LINK(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                  end;
                if pos('.',aName)>0 then
                  aName := copy(aName,pos('.',aName)+1,length(aName));
                Outp+='<th><b>'+HTMLEncode(StringReplace(aName,'''','',[rfReplaceAll]))+'</b></th>';
              end;
          end;
      end;
    Outp+='</tr></thead>';
  end;
  }
  {
  procedure FilterSQL(aType : Integer;IncHeader : Boolean = False);
  var
    a: Integer;
    aOrderDir: TSQLOrderDirection;
    aOrder: string = '';
    aOrderDirStr: String = 'ASC';
    aRDS: TDataSet;
    i: Integer;
    aTable: TSQLElement;
    NoRights: Boolean;
    aStmt: TSQLElement;
    aRightInt: Integer;
  begin
    FSQLStream := TStringStream.Create(Inp+';');
    FSQLScanner := TSQLScanner.Create(FSQLStream);
    FSQLParser := TSQLParser.Create(FSQLScanner);
    try
    try
      aFilter:='';
      aStmt := FSQLParser.Parse;
      a := 0;
      aTable := TSQLSelectStatement(aStmt).Tables[a];
      if aTable is TSQLSimpleTableReference then
       begin
         aTableName := TSQLSimpleTableReference(aTable).ObjectName.Name;
         aRight := UpperCase(aTableName);
       end;
      if  (aType <> 3)
      and (aType <> 4)
      then
        begin
          if TBaseDBModule(DataModule).ListDataSetFromLink(aTableName+'@',aClass) then
            begin
              if IncHeader then
                AddHeader(aStmt);
              if aType=1 then Outp+='<tbody align="left" valign="top" align="left">';
              aDs := TBaseDBDataset(aClass.Create(Self));
              try
                if Assigned(TSQLSelectStatement(aStmt).Where) then
                  aFilter:=TSQLSelectStatement(aStmt).Where.GetAsSQL([sfoDoubleQuoteIdentifier]);
                if Assigned(TSQLSelectStatement(aStmt).Orderby) and (TSQLSelectStatement(aStmt).Orderby.Count>0) then
                  begin
                    aOrder:=TSQLIdentifierName(TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).Field).Name;
                    aOrderDir := TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).OrderBy;
                  end;
                if ((TBaseDBModule(DataModule).Users.Rights.Right(aRight)>RIGHT_READ) or (TBaseDBModule(DataModule).Users.Rights.Right(aRight)=-1)) and (Assigned(aDS)) then
                  begin
                    if aOrder<>'' then
                      begin
                        if aOrderDir=obAscending then
                          aOrderDirStr := 'ASC'
                        else aOrderDirStr := 'DESC';
                      end;
                    if (aDs.ActualFilter<>'') and (aFilter<>'') then
                      aDs.Filter('('+aDs.ActualFilter+') AND ('+aFilter+')',aLimit)
                    else if (aFilter = '') and (aDs.ActualFilter<>'') then
                      aDs.FilterEx('('+aDs.ActualFilter+')',aLimit,aOrder,aOrderDirStr)
                    else
                      aDs.FilterEx(aFilter,aLimit,aOrder,aOrderDirStr);
                    while not aDS.EOF do
                      begin
                        case aType of
                        0:BuildLinkRow(aDs.DataSet);
                        1:Outp+=BuildTableRow(aDs.DataSet,aStmt);
                        end;
                        aDataThere:=True;
                        aDs.Next;
                      end;
                  end;
              finally
                aDS.Free;
              end;
              if aType=1 then Outp+='</tbody>';
            end
          else //pure SQL
            begin //TODO:better rights check ??
              NoRights := False;
              for a := 0 to TSQLSelectStatement(aStmt).Tables.Count-1 do
                begin
                  aTable := TSQLSelectStatement(aStmt).Tables[a];
                  if aTable is TSQLSimpleTableReference then
                    begin
                      aTableName := TSQLSimpleTableReference(aTable).ObjectName.Name;
                      aRight := UpperCase(aTableName);
                      aRightInt := TBaseDBModule(DataModule).Users.Rights.Right(aRight);
                      if (aRightInt<RIGHT_VIEW) then
                        NoRights := True;
                    end;
                end;
              if not NoRights then
                begin
                  aSQL := TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]);
                  aSQL := ReplaceSQLFunctions(aSQL);
                  if aLimit>0 then
                    aSQL := AddSQLLimit(aSQL,aLimit);
                  aRDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL);
                  try
                  try
                    aRDS.Open;
                    if IncHeader then
                      AddHeader(aStmt);
                    if aType=1 then Outp+='<tbody align="left" valign="top">';
                    while not aRDS.EOF do
                      begin
                        case aType of
                        0:BuildLinkRow(aRDS);
                        1:Outp+=BuildTableRow(aRDs,aStmt);
                        end;
                        aDataThere:=True;
                        aRDs.Next;
                      end;
                  except
                    on e : Exception do
                      begin
                        Outp+='error:'+e.Message+'<br>';
                        aDataThere:=True;
                      end;
                  end;
                  finally
                    aRDS.Free;
                  end;
                end;
            end;
        end
      else
        begin
          aSQL := TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]);
          aSQL := ReplaceSQLFunctions(aSQL);
          if aLimit>0 then
            aSQL := AddSQLLimit(aSQL,aLimit);
          aRDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL);
          aRDS.Open;
          while not aRDS.EOF do
            begin
              if ((TBaseDBModule(DataModule).Users.Rights.Right(aRight)>RIGHT_READ) or (TBaseDBModule(DataModule).Users.Rights.Right(aRight)=-1)) and (Assigned(aRDS)) then
                begin
                  for i := 0 to aRDS.FieldCount-1 do
                    begin
                      if aType=3 then
                        begin
                          if i>0 then Outp+=',';
                          tmp := HTMLEncode(aRDS.Fields[i].AsString);
                          Outp += tmp;
                          if TryStrToFloat(tmp,aTmpFloat) then
                            begin
                              if aTmpFloat<>0 then
                                aDataThere:=True;
                            end
                          else if tmp<>'' then aDataThere:=True;
                        end
                      else if (aType=4) then
                        begin
                          tmp := aRDS.Fields[i].FieldName;
                          Variables.Values[tmp]:=aRDS.Fields[i].AsString;
                          aDataThere:=True;
                        end;
                    end;
                end;
              if aType = 4 then
                begin
                  aNewList := TWikiList.CreateEx(Self,TBaseDBModule(DataModule));
                  if aNewList.FindWikiPage(aInclude) then
                    begin
                      Inp := aNewList.FieldByName('DATA').AsString;
                      for i := 0 to Variables.Count-1 do
                        begin
                          Inp := StringReplace(Inp,'@VARIABLES.'+Variables.Names[i]+'@',Variables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
                          Inp := StringReplace(Inp,'@VARIABLES.'+Variables.Names[i]+':HTTP@',HTTPEncode(PChar(Variables.ValueFromIndex[i])),[rfReplaceAll,rfIgnoreCase]);
                        end;
                      Outp:=Outp+WikiText2HTML(Inp,'','',True);
                    end;
                  aNewList.Free;
                end;
              aRDS.Next;
            end;
          aRDS.Free;
        end;
      FreeAndNil(aStmt);
    except
      on e : Exception do
        begin
          Outp+='error:'+e.Message+'<br>';
          aDataThere:=True;
        end;
    end;
    finally
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
    end;
  end;
  }
begin
  if pos('datathere(',lowercase(Inp))>0 then
    aDataThere:=False;
  if copy(lowercase(Inp),0,3)='if(' then
    begin
      aCondition := copy(Inp,4,pos(';',Inp)-4);
      Inp := copy(Inp,pos(';',Inp)+1,length(Inp));
      Inp := copy(Inp,0,length(Inp)-1);
      if copy(lowercase(aCondition),0,6)='right(' then
        begin
          aConditionOK:=True//TODO TBaseDBModule(DataModule).Users.Rights.Right(copy(aCondition,7,length(aCondition)-7))>=RIGHT_READ;
        end;
    end;
  if copy(lowercase(Inp),0,4)='rtf(' then
    begin
      Inp := copy(Inp,5,length(Inp)-5);
      ConvertRTF := True;
    end;
  if not aConditionOK then exit;
  if Assigned(Variables) then
    for i := 0 to Variables.Count-1 do
      begin
        Inp := StringReplace(Inp,'@VARIABLES.'+Variables.Names[i]+'@',Variables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
        Inp := StringReplace(Inp,'@VARIABLES.'+Variables.Names[i]+':HTTP@',HTTPEncode(PChar(Variables.ValueFromIndex[i])),[rfReplaceAll,rfIgnoreCase]);
      end;
  if Uppercase(copy(Inp,0,6)) = 'BOARD(' then
    begin
      {
      Inp := copy(Inp,7,length(Inp));
      TBaseDBModule(DataModule).SetFilter(TBaseDBModule(DataModule).Tree,'',0,'','ASC',False,True,False);
      if TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME',copy(Inp,0,pos(',',Inp)-1),[loCaseInsensitive]) then
        begin
          Inp := copy(Inp,pos(',',Inp)+1,length(Inp));
          Inp := copy(Inp,0,pos(')',Inp)-1);
          if not  TryStrToInt(Inp,aCount) then aCount := 30;
          aList := TMessageList.Create(nil);
          TBaseDBModule(DataModule).SetFilter(aList,TBaseDBModule(DataModule).QuoteField('TREEENTRY')+'='+TBaseDBModule(DataModule).QuoteValue(VarToStr(TBaseDBModule(DataModule).Tree.Id.AsVariant)),aCount);
          while not aList.DataSet.EOF do
            begin
              if aCount <= 0 then break;
              aMessage := TMessage.CreateEx(Self,TBaseDBModule(DataModule));
              aMessage.Select(aList.Id.AsVariant);
              aMessage.Open;
              if aMessage.Count > 0 then
                begin
                  Outp := Outp+'<b>'+aMessage.FieldByName('SUBJECT').AsString+'</b>';
                  ss := TStringStream.Create('');
                  TBaseDBModule(DataModule).BlobFieldToStream(aMessage.DataSet,'DATA',ss);
                  Outp := Outp+'<br>'+WikiText2HTML(ss.DataString,'','',True)+'<br>'+DateTimeToStr(aMessage.FieldByName('SENDDATE').AsDateTime)+'<br>';
                  ss.Free;
                end;
              aMessage.Free;
              aList.DataSet.Next;
            end;
          aList.Free;
        end;
      }
    end
  else if Uppercase(copy(Inp,0,9)) = 'SQLLINKS(' then
    begin
      {
      Inp := copy(Inp,10,length(Inp)-10);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<ol>';
      FilterSQL(0);
      Outp+='</ol>';
      }
    end
  else if Uppercase(copy(Inp,0,9)) = 'SQLTABLE(' then
    begin
      {
      Inp := copy(Inp,10,length(Inp)-10);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<table>';
      FilterSQL(1);
      Outp+='</table>';
      if pos('error:',Outp)>0 then
        Outp := StringReplace(Outp,'table>','p>',[rfReplaceAll]);
      }
    end
  else if Uppercase(copy(Inp,0,10)) = 'SQLTABLEH(' then
    begin
      {
      Inp := copy(Inp,11,length(Inp)-11);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<table>';
      FilterSQL(1,True);
      Outp+='</table>';
      if pos('error:',Outp)>0 then
        Outp := StringReplace(Outp,'table>','p>',[rfReplaceAll]);
      }
    end
  else if (Uppercase(copy(Inp,0,10)) = 'STATISTIC(')
       or (Uppercase(copy(Inp,0,11)) = 'STATISTICH(')
  then
    begin
      {
      Inp := copy(Inp,11,length(Inp)-11);
      IncHeader := False;
      if copy(Inp,0,1)='(' then
        begin
          Inp := copy(Inp,2,length(Inp));
          IncHeader := True;
        end;
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      aSQL := copy(Inp,0,rpos(' ',Inp)-1);
      if aSQL <> '' then
        aSQL := aSQL+' STATISTICS';
      FSQLStream := TStringStream.Create(aSQL);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      Inp := copy(Inp,rpos(' ',Inp)+1,length(Inp));
      aStatistic := nil;
      aStatistic := TStatistic.Create(Self);
      aRDs := nil;
      try
        aFilter:='';
        bStmt := FSQLParser.Parse;
        if pos('(',Inp)>0 then
          begin
            tmp := copy(Inp,pos('(',Inp)+1,length(Inp)-1);
            tmp := copy(tmp,0,length(tmp)-1);
            Variables.Values[copy(tmp,0,pos('=',tmp)-1)] := copy(tmp,pos('=',tmp)+1,length(tmp));
            Inp := copy(Inp,0,pos('(',Inp)-1);
          end;
        aStatistic.SelectFromLink(Inp);
        aStatistic.Open;
        if aStatistic.Count>0 then
          begin
            aSQL := aStatistic.BuildQuerry(Variables);
            aSQL := ReplaceSQLFunctions(aSQL);
            aRDs := TBaseDBModule(DataModule).GetNewDataSet(aSQL);
            try
              aRDS.Open;
            except
              on e : Exception do
                begin
                  Outp+='error:'+e.Message+'<br>';
                  aDataThere:=True;
                end;
            end;
            Outp+='<table>';
            if IncHeader then
              AddHeader(bStmt);
            Outp+='<tbody align="left" valign="top">';
            while (not aRDS.EOF) and (aLimit>0) do
              begin
                aDataThere:=True;
                Outp+=BuildTableRow(aRDs,bStmt);
                dec(aLimit,1);
                aRDS.Next;
              end;
            Outp+='</tbody>';
            Outp+='</table>';
          end;
      finally
        FreeAndNil(aRds);
        FreeAndNil(aStatistic);
      end;
      FreeAndNil(bStmt);
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
      }
    end
  else if Uppercase(copy(Inp,0,15)) = 'STATISTICVALUE(' then
    begin
      {
      Inp := copy(Inp,16,length(Inp)-16);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      aSQL := copy(Inp,0,rpos(' ',Inp)-1);
      if aSQL <> '' then
        aSQL := aSQL+' STATISTICS';
      FSQLStream := TStringStream.Create(aSQL);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      Inp := copy(Inp,rpos(' ',Inp)+1,length(Inp));
      try
        aFilter:='';
        bStmt := FSQLParser.Parse;
        aStatistic := TStatistic.Create(nil);
        aStatistic.SelectFromLink(Inp);
        aStatistic.Open;
        if aStatistic.Count>0 then
          begin
            aRDs := TBaseDBModule(DataModule).GetNewDataSet(aStatistic.BuildQuerry(Variables));
            try
              aRDS.Open;
            except
              on e : Exception do
                begin
                  Outp+='error:'+e.Message+'<br>';
                  aDataThere:=True;
                end;
            end;
            tmp := BuildTableRow(aRDs,bStmt);
            tmp := StringReplace(tmp,'<tr>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'</tr>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'<td>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'</td>','',[rfReplaceall]);
            Outp+=tmp;
            if TryStrToFloat(tmp,aTmpFloat) then
              if aTmpFloat<>0 then
                aDataThere:=True;
          end;
      finally
        FreeAndNil(aRds);
        FreeAndNil(aStatistic);
      end;
      FreeAndNil(bStmt);
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
      }
    end
  else if (Uppercase(copy(Inp,0,4)) = 'SQL(')
       or (Uppercase(copy(Inp,0,5)) = 'FORM(') then
    begin
      {
      IsForm := (Uppercase(copy(Inp,0,5)) = 'FORM(');
      Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
      if IsForm then
        begin
          aInclude := copy(Inp,0,pos(';',Inp)-1);
          Inp := copy(Inp,pos(';',Inp)+1,length(Inp));
        end;
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      if not IsForm then
        FilterSQL(3)
      else
        begin
          FilterSQL(4);
        end;
      }
    end
  else if (Uppercase(copy(Inp,0,7)) = 'SCRIPT(') then
    begin
      Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
      {
      aScript := TBaseScript.Create(nil);
      aScript.Write:=@aScriptWrite;
      aScript.Writeln:=@aScriptWriteln;
      FScriptContent:='';
      Found := False;
      if pos('(',Inp)=1 then
        begin
          bScript := copy(Inp,0,pos('(',Inp)-1);
          aScript.SelectByName(bScript);
          aScript.Open;
          if aScript.Count>0 then
            begin
              Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
              aScript.Execute(Inp);
              Found := True;
            end;
        end;
      if (not Found) and Assigned(aScript.Script) then
        begin
          aScript.Script.Source:=Inp;
          if not aScript.Script.Execute(Null) then
            FScriptContent:=aScript.Script.Results;
        end;
      aScript.Free;
      Outp := Outp+FScriptContent;
      }
    end
  else
    begin
      aNewList := TWikiList.Create(Self);
      aNewList.Variables.Assign(FVariables);
      nInp := Inp;
      if pos('|',nInp) > 0 then nInp := copy(nInp,0,pos('|',nInp)-1);
      //TODO: nInp := StringReplace(nInp,'%username%',TBaseDBModule(DataModule).Users.Text.AsString,[]);
      if aNewList.FindWikiPage(nInp) and (aLevel < 150) then
        begin
          Outp := Outp+WikiText2HTML(aNewList.FieldByName('DATA').AsString,'','',True,aLevel+1);
        end;
      aNewList.Free;
    end;
  if copy(lowercase(aCondition),0,10)='datathere(' then
    begin
      if not aDataThere then Outp := '';
    end;
  if ConvertRTF then
    begin
      Outp:=RtfToHtml(OutP);
    end;
end;

function TWikiList.GetTyp: string;
begin
  Result := 'W';
end;

function TWikiList.GetTextFieldName: string;
begin
  Result:='CAPTION';
end;
function TWikiList.GetNumberFieldName: string;
begin
  Result:='SQL_ID';
end;

function TWikiList.GetDescriptionFieldName: string;
begin
  Result:='DATA';
end;

function TWikiList.FindWikiPage(PageName: string;aDocreate : Boolean = False): Boolean;
var
  aParent : Int64;
  aID: Variant;
  tmp: String;
begin
  Result := False;
  {
  PageName:=HTMLDecode(PageName);
  if not Assigned(Self) then exit;
  with FKeywords.DataSet as IBaseDbFilter do
    Filter := '';
  aParent := 0;
  FActiveTreeID := aParent;
  PageName := Utils.HTMLDecode(PageName);
  PageName := Utils.HTTPDecode(PageName);
  TBaseDBModule(DataModule).Tree.DataSet.Filter := TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue('W');
  TBaseDBModule(DataModule).Tree.DataSet.Filtered := True;
  if pos('://',PageName) > 0 then exit;
  while pos('/',PageName) > 0 do
    begin
      if TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[])
      or TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[loCaseInSensitive]) then
        begin
          tmp := TBaseDBModule(DataModule).Tree.FieldByName('NAME').AsString;
          PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
          if TBaseDBModule(DataModule).Tree.Id.AsVariant<>Null then
            aParent := TBaseDBModule(DataModule).Tree.Id.AsVariant
          else aParent:=0;
          if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
            Debug('Parent found:'+IntToStr(aParent));
        end
      else
        begin
          if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
            Debug('Parent not found, re-filter ('+copy(PageName,0,pos('/',PageName)-1)+','+IntToStr(aParent)+',W)');
          TBaseDBModule(DataModule).Tree.DataSet.Filtered := False;
          if (TBaseDBModule(DataModule).Tree.ActualFilter<>'') or (not TBaseDBModule(DataModule).Tree.Active) then
            TBaseDBModule(DataModule).SetFilter(TBaseDBModule(DataModule).Tree,'',0,'','ASC',False,True,True);
          TBaseDBModule(DataModule).Tree.Cancel;
          if TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[])
          or TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[loCaseInSensitive]) then
            begin
              PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
              if TBaseDBModule(DataModule).Tree.Id.AsVariant<>Null then
                aParent := TBaseDBModule(DataModule).Tree.Id.AsVariant
              else aParent:=0;
              if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
                Debug('Parent found:'+IntToStr(aParent));
            end
          else if aDocreate then
            begin
              if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
                Debug('Parent not found creating path');
              with TBaseDBModule(DataModule).Tree.DataSet do
                begin
                  Append;
                  FieldByName('TYPE').AsString := 'W';
                  FieldByName('NAME').AsString := copy(PageName,0,rpos('/',PageName)-1);
                  if aParent <> TREE_ID_WIKI_UNSORTED then
                    FieldByName('PARENT').AsVariant := aParent
                  else
                    FieldByName('PARENT').AsInteger := 0;
                  Post;
                  PageName := copy(PageName,rpos('/',PageName)+1,length(PageName));
                  if TBaseDBModule(DataModule).Tree.Id.AsVariant<>Null then
                    aParent := TBaseDBModule(DataModule).Tree.Id.AsVariant
                  else aParent:=0;
                end;
            end
          else break;
        end;
    end;
  TBaseDBModule(DataModule).Tree.DataSet.Filtered := False;
  Result := DataSet.Active and (DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[loCaseInsensitive]));
  if not Result then
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Debug('Page not found, re-filter');
      TBaseDBModule(DataModule).SetFilter(Self,TBaseDBModule(DataModule).QuoteField('TREEENTRY')+'='+TBaseDBModule(DataModule).QuoteValue(IntToStr(aParent)));
      Result := DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[loCaseInsensitive]);
      if not Result then
        begin
          if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
            Debug('Page not found, re-filter 2');
          TBaseDBModule(DataModule).SetFilter(Self,TBaseDBModule(DataModule).QuoteField('NAME')+'='+TBaseDBModule(DataModule).QuoteValue(PageName));
          Result := DataSet.Locate('TREEENTRY;NAME',VarArrayOf([Null,PageName]),[loCaseInsensitive]);
        end;
    end;
  if Result then
    begin
      Keywords.Open;
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Debug('Page found!');
    end;
  FActiveTreeID := aParent;
  }
end;

function TWikiList.FindWikiFolder(PageName: string): Boolean;
var
  aParent: Variant;
begin
  Result := False;
  {
  aParent := 0;
  TBaseDBModule(DataModule).Tree.DataSet.Filter := TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue('W');
  TBaseDBModule(DataModule).Tree.DataSet.Filtered := True;
  if copy(PageName,0,7) = 'http://' then exit;
  while pos('/',PageName) > 0 do
    begin
      TBaseDBModule(DataModule).Tree.Open;
      if TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
      or TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
        begin
          PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
          aParent := TBaseDBModule(DataModule).Tree.Id.AsVariant;
        end
      else
        begin
          if (TBaseDBModule(DataModule).Tree.ActualFilter<>'') or (not TBaseDBModule(DataModule).Tree.Active) then
            TBaseDBModule(DataModule).SetFilter(TBaseDBModule(DataModule).Tree,'',0,'','ASC',False,True,True);
          if TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
          or TBaseDBModule(DataModule).Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
            begin
              PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
              aParent := TBaseDBModule(DataModule).Tree.Id.AsVariant;
            end
          else
            begin
              result := False;
              exit;
            end;
        end;
    end;
  TBaseDBModule(DataModule).Tree.DataSet.Filtered := False;
  TBaseDBModule(DataModule).SetFilter(Self,TBaseDBModule(DataModule).QuoteField('TREEENTRY')+'='+TBaseDBModule(DataModule).QuoteValue(VarToStr(aParent)));
  Result := Count>0;
  }
end;

function TWikiList.GetFullPath: string;
//var
//  aTree: TTree;
begin
  {
  aTree := TTree.CreateEx(Self,DataModule);
  if not Active then exit;
  Result := FieldByName('NAME').AsString;
  aTree.Select(DataSet.FieldByName('TREEENTRY').AsVariant);
  aTree.Open;
  while aTree.Count>0 do
    begin
      Result := aTree.FieldByName('NAME').AsString+'/'+Result;
      aTree.Close;
      aTree.Select(aTree.FieldByName('PARENT').AsVariant);
      aTree.Open;
    end;
  aTree.Free;
  }
end;

function TWikiList.isDynamic: Boolean;
begin
  Result := False;
  if not Active then exit;
  Result := pos('[[INCLUDE:',Uppercase(FieldByName('DATA').AsString))>0;
end;

function TWikiList.PageAsHtml(OnlyBody: Boolean; UsePath: Boolean): string;
var
  aPath: String = '';
  tmp, MetaTags: String;
begin
  if UsePath then
    aPath := GetFullPath;
  WikiToHtml.OnWikiInclude:=@BasicWikiInclude;
  aPath := copy(aPath,0,rpos('/',aPath));
  Result := WikiText2HTML(Data,'',aPath);
  //Result := '<base target="_blank">'+Result;
  if not OnlyBody then
    begin
      try
        tmp := GenerateKeyWords;
        if tmp <> '' then
          MetaTags := '<meta name="keywords" content="'+tmp+'">';
      except
      end;
      tmp := GenerateDescription;
      if tmp <> '' then
        MetaTags += '<meta name="description" content="'+tmp+'">';
      Result := '<html><head><title>'+Caption+'</title>'+MetaTags+'</head><body>'+Result+'</body></html>';
    end;
end;

function TWikiList.GenerateKeywords: string;
begin
  Result := '';
  {
  Keywords.Open;
  Keywords.First;
  while not Keywords.EOF do
    begin
      Result := Result+','+Keywords.FieldByName('KEYWORD').AsString;
      Keywords.Next;
    end;
  Result := copy(Result,2,length(Result));
  }
end;

function TWikiList.GenerateDescription: string;
var
  ReplaceText: String;
begin
  ReplaceText := HTMLEncode(copy(PageAsText,0,200));
  if rpos('.',ReplaceText) > 100 then
    ReplaceText := copy(ReplaceText,0,rpos('.',ReplaceText))
  else if rpos(' ',ReplaceText) > 100 then
    ReplaceText := copy(ReplaceText,0,rpos(' ',ReplaceText))
  else
    ReplaceText := copy(ReplaceText,0,120);
  Result := ReplaceText;
end;

function TWikiList.PageAsText: string;
begin
  Result := StripHTML(PageAsHtml(true));
end;

constructor TWikiList.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner);
  FVariables := TStringList.Create;
  FKeywords := TKeywords.Create(Self);
end;

destructor TWikiList.Destroy;
begin
  FreeAndNil(FVariables);
  FreeAndNil(FKeywords);
  inherited Destroy;
end;

procedure TWikiList.WikiListWikiLink(Inp: string; var Outp: string;
  aLevel: Integer=0);
var
  tmp: String;
  aFN: String;
begin
  Outp := FOutSub+'/'+Inp+FOutExt;
  tmp := FOutDir+'/'+FOutSub+'/'+Inp;
  tmp := copy(tmp,0,rpos('/',tmp)-1);
  tmp := StringReplace(tmp,'/',DirectorySeparator,[rfReplaceAll]);
  tmp := StringReplace(tmp,DirectorySeparator+DirectorySeparator,DirectorySeparator,[rfReplaceAll]);
  ForceDirectories(tmp);
  aFN := StringReplace(StringReplace(FOutDir+'/'+Outp,'/',DirectorySeparator,[rfReplaceAll]),'//','/',[rfReplaceAll]);
  if FOutTodo.IndexOf(Inp)=-1 then
    FOutTodo.Values[Inp] := GetFullPath;
end;

function TWikiList.ExportToHTML(aFile: string; aInclude: TWikiIncludeFunc
  ): Boolean;
var
  sl: TStringList;
  aPage: TWikiList;
  Outp: String;
  aFN: String;
  aRelPath: String;
  aLinkOffs: String;
  aRemPath: String;
  tmp: String;
begin
  Result:=True;
  WikiToHtml.OnWikiLink:=@WikiListWikiLink;
  WikiToHtml.OnWikiInclude:=aInclude;
  FOutDir := ExtractFileDir(aFile);
  FOutSub := copy(ExtractFileName(aFile),0,rpos('.',ExtractFileName(aFile))-1);
  FOutExt := ExtractFileExt(aFile);
  FOutTodo := TStringList.Create;
  sl := TStringList.Create;
  sl.Text := WikiText2HTML(Data,'','');
  sl.SaveToFile(aFile);
  sl.Free;
  while FOutTodo.Count>0 do
    begin
      aPage := TWikiList.Create(nil);
      Outp := FOutSub+'/'+FOutTodo.Names[0]+FOutExt;
      aFN := StringReplace(StringReplace(FOutDir+'/'+Outp,'/',DirectorySeparator,[rfReplaceAll]),'//','/',[rfReplaceAll]);
      if (not FileExists(aFN)) and aPage.FindWikiPage(FOutTodo.Names[0]) then
        begin
          //aRelPath := StringReplace(FileUtil.CreateRelativePath(FOutTodo.Names[0],FOutTodo.ValueFromIndex[0]),DirectorySeparator,'/',[rfReplaceAll]);
          tmp := Outp;
          tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
          aLinkOffs := '';
          aRemPath := '';
          while copy(aRelPath,0,3)='../' do
            begin
              aRemPath := aRemPath+copy(tmp,0,pos('/',tmp)-1);
              tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
              aLinkOffs := aLinkOffs+'../';
              aRelPath:=copy(aRelPath,4,length(aRelPath));
            end;
          sl.Text := WikiText2HTML(aPage.Data,aLinkOffs,aRemPath);
          sl.SaveToFile(aFN);
          sl.Free;
        end;
      FOutTodo.Delete(0);
      aPage.Free;
    end;
  FOutTodo.Free;
end;

initialization
  RegisterdataSetClass('WIKI',TWikiList);
end.

