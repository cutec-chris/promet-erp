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
unit uprometnntp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uLNNTP, uMessages, MimeMess, uMimeMessages, db,
  LConvEncoding;
type
  TPNNTPGroup = class(TNNTPGroup)
  private
    FMessages : TMessageList;
    FTreeEntry : string;
    FGroupName : string;
    FCount : Integer;
    FLastID : LargeInt;
    function GenerateMessage : TMimeMess;
    procedure RefreshFirstID;
    procedure RefreshCount;
  protected
    function GetLastID: LargeInt;override;
    function GetFirstID: LargeInt;override;
    function GetCount: Integer;override;
    function GetMessage(Idx : Integer): TMimeMess;override;
    function GetMessageByID(Idx : string): TMimeMess;override;
    function SelectMessages(aFilter : string) : Boolean;override;
    function GetCreatedAt: TDateTime; override;
    function XOverAnswer : string;override;
    function PostArticle(aArticle : TStrings) : Boolean;override;
  public
    constructor Create(aName : string;cFirstID : LargeInt);override;
    destructor Destroy;override;
  end;

implementation
uses uData,Variants,SynaUtil,uSessionDBClasses;
function TPNNTPGroup.GenerateMessage: TMimeMess;
var
aMessage: TMimeMessage;
begin
  aMessage := TMimeMessage.Create(Self,Data);
  aMessage.Select(FMessages.Id.AsVariant);
  aMessage.Open;
  Result := aMessage.EncodeMessage;
  Result.Header.CustomHeaders.Add('Lines: '+aMessage.DataSet.FieldByName('LINES').AsString);
  Result.Header.CustomHeaders.Add('Xref: localhost '+FGroupName+':'+VarToStr(FMessages.FieldByName('GRP_ID').AsVariant));
  Result.EncodeMessage;
  aMessage.Free;
end;
procedure TPNNTPGroup.RefreshFirstID;
begin
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry),1,'GRP_ID','ASC');
  if FMessages.Count > 0 then
    FFirstID := FMessages.FieldByName('GRP_ID').AsVariant;
end;
procedure TPNNTPGroup.RefreshCount;
begin
  FFIrstID := 0;
  FlastID := 0;
  FCount := 0;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry),0,'GRP_ID','ASC');
  try
    if FMessages.Count > 0 then
      begin
        FFirstID:=FMessages.FieldByName('GRP_ID').AsVariant;
        Fmessages.DataSet.Last;
        FLastID:=FMessages.FieldByName('GRP_ID').AsVariant;
      end;
    FCount := FMessages.Count;
  except
  end;
end;
function TPNNTPGroup.GetLastID: LargeInt;
begin
  Result := FLastID;
  if Result = 0 then
    begin
      RefreshCount;
      Result := FLastID;
    end;
  if Result <= FirstID then Result := FirstID+1;
end;
function TPNNTPGroup.GetFirstID: LargeInt;
begin
  Result := FFirstID;
end;
function TPNNTPGroup.GetCount: Integer;
begin
  RefreshCount;
  Result := FCount;
end;
function TPNNTPGroup.GetMessage(Idx: Integer): TMimeMess;
begin
  Result := nil;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('GRP_ID')+'='+Data.QuoteValue(IntToStr(Idx)),1,'GRP_ID','ASC');
  MessageIdx := -1;
  if FMessages.Count > 0 then
    begin
      MessageIdx := Fmessages.FieldByName('GRP_ID').AsVariant;
      Result := GenerateMessage;
    end;
end;
function TPNNTPGroup.GetMessageByID(Idx: string): TMimeMess;
begin
  Result:=nil;
end;
function TPNNTPGroup.SelectMessages(aFilter: string): Boolean;
var
  Arg1: String;
  Arg2: String;
  Max: Integer;
begin
  Result:=False;
  if pos('-',aFilter) = 0 then exit;
  Arg1 := copy(aFilter,0,pos('-',aFilter)-1);
  Arg2 := copy(aFilter,pos('-',aFilter)+1,length(aFilter));
  Max := 0;
  if trim(Arg2) = '' then
    aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('GRP_ID')+' >= '+Data.QuoteValue(Arg1)
  else
    begin
      aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('GRP_ID')+' >= '+Data.QuoteValue(Arg1)+' AND '+Data.QuoteField('GRP_ID')+' <= '+Data.QuoteValue(Arg2);
      try
        Max := (StrToInt(Arg2)-StrToInt(Arg1))+1;
        aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('GRP_ID')+' >= '+Data.QuoteValue(Arg1);
      except
        aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('GRP_ID')+' >= '+Data.QuoteValue(Arg1)+' AND '+Data.QuoteField('GRP_ID')+' <= '+Data.QuoteValue(Arg2);
        Max := 0;
      end;
    end;
  Data.SetFilter(FMessages,aFilter,Max,'GRP_ID','ASC');
  FMessages.DataSet.First;
  Result := True;
end;
function TPNNTPGroup.GetCreatedAt: TDateTime;
begin
  Result:=inherited GetCreatedAt;
  if Data.Tree.DataSet.Locate('SQL_ID',FTreeEntry,[]) then
    Result := Data.Tree.TimeStamp.AsDateTime;
end;
function TPNNTPGroup.XOverAnswer: string;
var
  FMsg: TMessageList;
  function FindParentRecursive(aParent : Int64) : string;
  var
    bParent : Variant;
  begin
    Result := '';
    FMsg := TMessageList.Create(Self,Data);
    FMsg.SelectByMsgID(aParent);
    FMsg.Open;
    if FMsg.Count > 0 then
      begin
        bParent := FMsg.DataSet.FieldByName('PARENT').AsVariant;
        Result := '<'+FMsg.DataSet.FieldByName('ID').AsString+'>';
        FMsg.Destroy;
        if not VarIsNull(bParent) then
          Result := FindParentrecursive(bParent)+' '+Result;
      end
    else
      FMsg.Destroy;
  end;
begin
  Result := '';
  if not FMessages.DataSet.EOF then
    begin
      Result:=FMessages.FieldByName('GRP_ID').AsString+#9+ConvertEncoding(FMessages.Text.AsString,GuessEncoding(FMessages.Text.AsString),EncodingUTF8)+#9+FMessages.DataSet.FieldbyName('SENDER').AsString+#9+Rfc822DateTime(FMessages.DataSet.FieldbyName('SENDDATE').AsDateTime)+#9+'<'+FMessages.DataSet.FieldbyName('ID').AsString+'>'+#9;
      if not FMessages.DataSet.FieldByName('PARENT').IsNull then
        begin
          Result := Result+FindParentRecursive(FMessages.DataSet.FieldByName('PARENT').AsInteger)+#9;
        end
      else Result := Result+#9;
      if not FMessages.DataSet.FieldByName('LINES').IsNull then
        Result := Result+FMessages.DataSet.FieldByName('LINES').AsString+#9
      else
        Result := Result+'100'+#9;
      if not FMessages.DataSet.FieldByName('LINES').IsNull then
        Result := Result+FMessages.DataSet.FieldByName('SIZE').AsString
      else
        Result := Result+'100';
      Fmessages.DataSet.Next;
    end;
end;
function TPNNTPGroup.PostArticle(aArticle: TStrings): Boolean;
var
  aMsg: TMimeMess;
  aMessage: TMimeMessage;
  aID: String;
  i: Integer;
  aChr: Char;
begin
  Result := False;
  aMsg := TMimeMess.Create;
  aMsg.Lines.Assign(aArticle);
  aMsg.DecodeMessage;
  randomize;
  aID := '';
  for i := 0 to 45 do
    begin
      aChr := chr(ord('0')+random(74));
      if aChr in ['a'..'z','0'..'9','A'..'Z'] then
        aID := aID+aChr;
    end;
  aMsg.Header.MessageID := aID;
  aMessage := TMimeMessage.Create(Self,Data);
  aMessage.Select(0);
  aMessage.Open;
  aMessage.DataSet.Insert;
  aMessage.Dataset.FieldByName('USER').AsString := '*';
  aMessage.Dataset.FieldByName('TYPE').AsString := 'EMAIL';
  aMessage.Dataset.FieldByName('READ').AsString := 'N';
  aMessage.DecodeMessage(aMsg);
  if not Data.Numbers.HasNumberSet('NG.'+aMessage.FieldByName('TREEENTRY').AsString) then
    begin
      Data.Numbers.Insert;
      Data.Numbers.FieldByName('TABLENAME').AsString:='NG.'+Self.FTreeEntry;
      Data.Numbers.FieldByName('TYPE').AsString:='N';
      Data.Numbers.FieldByName('INCR').AsInteger:=1;
      Data.Numbers.FieldByName('ACTUAL').AsVariant:=GetLastID+1;
      Data.Numbers.FieldByName('STOP').AsVariant:=9999999999;
      Data.Numbers.DataSet.Post;
    end;
  if not aMessage.CanEdit then aMessage.DataSet.Edit;
  aMessage.FieldByName('GRP_ID').AsString:=Data.Numbers.GetNewNumber('NG.'+aMessage.FieldByName('TREEENTRY').AsString);
  if not aMessage.CanEdit then aMessage.DataSet.Post;
  aMsg.Free;
  aMessage.DataSet.Post;
  Result := True;
  aMessage.Destroy;
end;
constructor TPNNTPGroup.Create(aName: string;cFirstID : LargeInt);
begin
  FMessages := TMessageList.Create(Self,Data);
  if Data.Tree.DataSet.Locate('NAME',aName,[loCaseInsensitive]) then
    FTreeEntry := Data.Tree.Id.AsString
  else if Data.Tree.DataSet.Locate('NAME',StringReplace(aName,'_',' ',[rfReplaceAll]),[loCaseInsensitive]) then
    FTreeEntry := Data.Tree.Id.AsString;
  FGroupName := aName;
  RefreshFirstID;
  if FMessages.FieldByName('GRP_ID').IsNull then
    inherited Create(StringReplace(FGroupName,' ','_',[rfReplaceAll]),0)
  else
    inherited Create(StringReplace(FGroupName,' ','_',[rfReplaceAll]),FMessages.FieldByName('GRP_ID').AsVariant);
end;
destructor TPNNTPGroup.Destroy;
begin
  if Assigned(FMessages) then
    FMessages.Free;
  inherited Destroy;
end;
end.

