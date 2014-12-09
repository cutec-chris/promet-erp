unit uforum;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpTemplate, FileUtil,
  uMessages,ubaseconfig;
type
  TfmForum = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleGetAction(Sender: TObject; ARequest: TRequest;
      var ActionName: String);
    procedure boardListRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure newthreadRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure ReplaceBoardDetailTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure ReplaceBoardListDetailTags(Sender: TObject;
      const TagString: String; TagParams: TStringList; out ReplaceText: String);
    procedure ReplaceNewThreadTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure ReplaceThreadDetailTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure showboardRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure showthreadRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    Title : string;
    FTemplate : TStringList;
    FMessage : TMessage;
    procedure ReplaceMainTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure DisplayMessage(aMessage : TMessage;TagParams: TStringList;var ReplaceText: String; Recursive : Boolean = True;UserRight : Boolean = True;CanAnswer : Boolean = True;Inner : Boolean = False);
  public
    { public declarations }
    procedure SettemplateParams(aTemplate : TFPTemplate);
    procedure ReplaceStdTags(Sender: TObject; const TagString: String;TagParams: TStringList; out ReplaceText: String);
  end;
var
  fmForum: TfmForum;
implementation
uses ubasefcgiapplication,uBaseApplication,Utils,uData,variants;
{$R *.lfm}
procedure TfmForum.DataModuleCreate(Sender: TObject);
begin
  FTemplate := TStringList.Create;
end;
procedure TfmForum.DataModuleDestroy(Sender: TObject);
begin
  FTemplate.Free;
end;
procedure TfmForum.DataModuleGetAction(Sender: TObject; ARequest: TRequest;
  var ActionName: String);
var
  Result: String;
  Path: String;
begin
  Path := '';
  If (ActionVar<>'') then
    Result:=ARequest.QueryFields.Values[ActionVar];
  If (Result='') then
    begin
      Result := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Result := copy(Result,0,pos('/',Result)-1);
    end;
  if Result = '' then
    Result:=ARequest.GetNextPathInfo;
  if (ARequest.PathInfo = '') or (ARequest.PathInfo = '/') or (Result = 'show') or (Result = 'forum') then
    begin
      Result := 'boardlist';
      Path := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Path := copy(Path,pos('/',Path)+1,length(Path));
      Path := trim(Path);
    end;
  if Actions.FindAction(path) <> nil then
    Result := path;
  ActionName := Result;
end;
procedure TfmForum.boardListRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := 'Forums Liste';
  TFPWebAction(Sender).Template.OnReplaceTag :=@ReplaceBoardListDetailTags;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  Handled := true;
end;
procedure TfmForum.newthreadRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aParent: String;
  NewMessage : TMessage;
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := 'Neuer Thread';
  if (ARequest.QueryFields.Values['Text'] = '')
  then
    begin
      Title := 'Neuer Thread';
      TFPWebAction(Sender).Template.OnReplaceTag :=@ReplaceNewThreadTags;
      AResponse.Content := TFPWebAction(Sender).Template.GetContent;
      Handled := true;
    end
  else
    begin
      if not Assigned(FMessage) then FMessage := TMessage.CreateEx(Self,Data);
      if ARequest.QueryFields.Values['Subject'] = '' then
        if FMessage.Count > 0 then
          begin
            ARequest.QueryFields.Values['Subject'] := 'Re: '+FMessage.FieldByName('SUBJECT').AsString;
            ARequest.QueryFields.Values['Id'] := FMessage.FieldByName('MSG_ID').AsString;
          end;
      FMessage.Insert;
      with FMessage.DataSet do
        begin
          FieldByName('SQL_ID').AsVariant := Data.GetUniID;
          FieldByName('TREEENTRY').AsVariant:=Data.Tree.Id.AsVariant;
          FieldByName('SUBJECT').AsString:=ARequest.QueryFields.Values['Subject'];
          FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
          FieldByName('MSG_ID').AsVariant :=  FieldByName('SQL_ID').AsVariant;
          aParent := ARequest.QueryFields.Values['Parent'];
          if aParent <> '' then
            begin
              FieldByName('PARENT').AsVariant :=  aParent;
            end
          else
            ARequest.QueryFields.Values['Id'] := FieldByName('MSG_ID').AsString;
          FieldByName('TYPE').AsString := 'POST';
          FieldByName('READ').AsString := 'Y';
          FieldByName('SENDER').AsString := Data.Users.FieldByName('NAME').AsString;
          FieldByName('SENDDATE').AsDateTime := Now();
          Post;
          FMessage.Content.Insert;
          FMessage.Content.FieldByName('DATATYP').AsString:='HTML';
          FMessage.Content.FieldByName('DATA').AsString:='<html><body>'+ARequest.QueryFields.Values['Text']+'</body></html>';
          FMessage.Content.DataSet.Post;
        end;
      showthreadRequest(Sender,ARequest,AResponse,Handled);
    end
end;
procedure TfmForum.ReplaceBoardDetailTags(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
var
  aThreads: TMessageList;
  aRow: String;
  aTmpRow: String;
begin
  ReplaceMainTags(Sender,TagString,TagParams,ReplaceText);
  if AnsiCompareText(TagString, 'THREADS_LIST') = 0 then
    begin
      aThreads := TMessageList.CreateEx(Self,Data);
      aThreads.SelectByDir(Data.Tree.FieldByName('ID').AsVariant);
      aThreads.Open;
      ReplaceText := TagParams.Values['HEADER'];
      with aThreads.DataSet do
        begin
          First;
          while not EOF do
            begin
              aRow := TagParams.Values['ONEROW'];
              aTmpRow := aRow;
              aTmpRow := StringReplace(aTmpRow,'~Name',FieldByName('SUBJECT').AsString,[rfReplaceAll]);
              aTmpRow := StringReplace(aTmpRow,'~Link','/forum/showthread?Id='+HTMLEncode(FieldByName('MSG_ID').AsString),[rfReplaceAll]);
              aTmpRow := StringReplace(aTmpRow,'~AvatarURL','/templates/noavatar.png',[rfReplaceAll]);
              ReplaceText := ReplaceText+aTmpRow;
              Next;
            end;
        end;
      aThreads.Free;
      ReplaceText := ReplaceText+StringReplace(TagParams.Values['FOOTER'],'~BoardName',HTMLEncode(Data.Tree.FieldByName('NAME').AsString),[rfReplaceAll]);
    end;
end;
procedure TfmForum.ReplaceBoardListDetailTags(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
var
  aRow: String;
  aTmpRow: String;
begin
  ReplaceMainTags(Sender,TagString,TagParams,ReplaceText);
  if AnsiCompareText(TagString, 'FORUMS_LIST') = 0 then
    begin
      ReplaceText := TagParams.Values['HEADER'];
      Data.SetFilter(Data.Tree,Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'),0,'','ASC',False,True,True);
      with Data.Tree.DataSet do
        begin
          First;
          while not EOF do
            begin
              aRow := TagParams.Values['ONEROW'];
              aTmpRow := aRow;
              aTmpRow := StringReplace(aTmpRow,'~Name',FieldByName('NAME').AsString,[rfReplaceAll]);
              aTmpRow := StringReplace(aTmpRow,'~Link','/forum/showboard?Name='+HTMLEncode(FieldByName('NAME').AsString),[rfReplaceAll]);
              ReplaceText := ReplaceText+aTmpRow;
              Next;
            end;
        end;
      ReplaceText := ReplaceText+TagParams.Values['FOOTER'];
    end;
end;
procedure TfmForum.ReplaceNewThreadTags(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
begin
  ReplaceMainTags(Sender,TagString,TagParams,ReplaceText);
  if AnsiCompareText(TagString, 'THREAD') = 0 then
    begin
      ReplaceText := TagParams.Values['HEADER'];
      ReplaceText := ReplaceText+TagParams.Values['NEWROW'];
    end;
end;
procedure TfmForum.DisplayMessage(aMessage: TMessage;TagParams: TStringList;var ReplaceText: String;
  Recursive : Boolean; UserRight: Boolean;
  CanAnswer: Boolean;Inner : Boolean);
var
  aContent: String;
  SubMessages: TMessage;
  aTmpRow: String;
  SubReplaceText : String = '';
  aUser: String;
begin
  if not aMessage.Content.DataSet.Active then aMessage.Content.DataSet.Open;
  aContent := aMessage.Content.FieldByName('DATA').AsString;
  if aMessage.Content.FieldByName('DATATYP').AsString = 'HTML' then
    begin
      aContent:=StringReplace(aContent,'<html>','',[rfReplaceAll]);
      aContent:=StringReplace(aContent,'</html>','',[rfReplaceAll]);
      aContent:=StringReplace(aContent,'<body>','',[rfReplaceAll]);
      aContent:=StringReplace(aContent,'</body>','',[rfReplaceAll]);
    end
  else
    begin
      aContent:=StringReplace(aContent,#10,'<br>',[rfReplaceAll]);
    end;
  ReplaceText := StringReplace(ReplaceText,'~Content',aContent,[rfReplaceAll]);
  ReplaceText := StringReplace(ReplaceText,'~ParentID',aMessage.Id.AsString,[rfReplaceAll]);
  if not Inner then
    ReplaceText := StringReplace(ReplaceText,'~RowClass',TagParams.Values['ROWCLASSNORMAL'],[rfReplaceAll])
  else
    ReplaceText := StringReplace(ReplaceText,'~RowClass',TagParams.Values['ROWCLASSINNER'],[rfReplaceAll]);
  aUser := TagParams.Values['USERTD'];
  aUser := StringReplace(aUser,'~AvatarURL','/templates/noavatar.png',[rfReplaceAll]);
  if UserRight then
    begin
      ReplaceText := StringReplace(ReplaceText,'~AnswerDirection',TagParams.Values['AnswerDirectionRight'],[rfReplaceAll]);
      ReplaceText := StringReplace(ReplaceText,'~UserRight',aUser,[rfReplaceAll]);
      ReplaceText := StringReplace(ReplaceText,'~UserLeft','',[rfReplaceAll]);
    end
  else
    begin
      ReplaceText := StringReplace(ReplaceText,'~AnswerDirection',TagParams.Values['AnswerDirectionLeft'],[rfReplaceAll]);
      ReplaceText := StringReplace(ReplaceText,'~UserRight','',[rfReplaceAll]);
      ReplaceText := StringReplace(ReplaceText,'~UserLeft',aUser,[rfReplaceAll]);
    end;
  if Recursive then
    begin
      SubMessages := TMessage.CreateEx(Self,Data);
      SubMessages.SelectByParent(aMessage.Id.AsVariant);
      SubMessages.Open;
      SubReplaceText := TagParams.Values['SUBROWHEADER'];
      with SubMessages.DataSet do
        begin
          Last;
          SubMessages.Next;
          while not BOF do
            begin
              aTmpRow := TagParams.Values['BASEROW'];
              DisplayMessage(SubMessages,TagParams,aTmpRow,Recursive,UserRight,CanAnswer,True);
              SubReplaceText := SubReplaceText+aTmpRow;
              SubMessages.Prior;
            end;
        end;
      SubReplaceText:=SubReplaceText+TagParams.Values['SUBROWFOOTER'];
    end;
  ReplaceText := StringReplace(ReplaceText,'~SubAnswers',SubReplaceText,[rfReplaceAll]);
end;
procedure TfmForum.ReplaceThreadDetailTags(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
var
  aTmpRow: String;
  SubMessages: TMessage;
begin
  ReplaceMainTags(Sender,TagString,TagParams,ReplaceText);
  if AnsiCompareText(TagString, 'THREAD') = 0 then
    begin
      ReplaceText := TagParams.Values['HEADER'];
      ReplaceText := StringReplace(ReplaceText,'~ParentID',FMessage.Id.AsString,[rfReplaceAll]);
      with FMessage.DataSet do
        begin
          FMessage.Content.Open;
          aTmpRow := TagParams.Values['BASEROW'];
          DisplayMessage(FMessage,TagParams,aTmpRow,False,False,False);
          ReplaceText:=ReplaceText+aTmpRow;
        end;
      SubMessages := TMessage.CreateEx(Self,Data);
      SubMessages.SelectByParent(Fmessage.Id.AsVariant);
      SubMessages.Open;
      with SubMessages.DataSet do
        begin
          Last;
          SubMessages.Next;
          while not BOF do
            begin
              aTmpRow := TagParams.Values['BASEROW'];
              DisplayMessage(SubMessages,TagParams,aTmpRow);
              ReplaceText := ReplaceText+aTmpRow;
              SubMessages.Prior;
            end;
        end;
      aTmpRow:=TagParams.Values['FOOTER'];
      aTmpRow := StringReplace(aTmpRow,'~ParentID',FMessage.Id.AsString,[rfReplaceAll]);
      ReplaceText := ReplaceText+aTmpRow;
    end;
end;
procedure TfmForum.showboardRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  if ARequest.QueryFields.Values['Name'] <> '' then
    begin
      Title := 'Forum '+ARequest.QueryFields.Values['Name'];
      if Data.Tree.DataSet.Locate('TYPE;NAME',VarArrayOf(['B',ARequest.QueryFields.Values['Name']]),[]) then
        begin
          TFPWebAction(Sender).Template.OnReplaceTag :=@ReplaceBoardDetailTags;
        end;
    end;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  Handled := true;
end;
procedure TfmForum.showthreadRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  if ARequest.QueryFields.Values['Id'] <> '' then
    begin
      if not Assigned(FMessage) then FMessage := TMessage.CreateEx(Self,Data);
      FMessage.SelectByMsgID(StrToInt64(ARequest.QueryFields.Values['Id']));
      FMessage.Open;
      if FMessage.Count > 0 then
        begin
          Title := FMessage.FieldByName('SUBJECT').AsString;
          TFPWebAction(Sender).Template.OnReplaceTag :=@ReplaceThreadDetailTags;
        end;
    end
  else FMessage.DataSet.Close;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  Handled := true;
end;
procedure TfmForum.ReplaceMainTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
begin
  ReplaceStdTags(Sender,TagString,TagParams,ReplaceText);
  ReplaceText := UTF8ToSys(ReplaceText);
end;
procedure TfmForum.SettemplateParams(aTemplate: TFPTemplate);
var
  sl: TStringList;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfmForum,fmForum);
      Self := fmForum;
    end;
  aTemplate.AllowTagParams := True;
  aTemplate.StartDelimiter := '{+';
  aTemplate.EndDelimiter := '+}';
//  if FTemplate.Count = 0 then
    begin
      with BaseApplication as IBaseConfig do
        FTemplate.LoadFromFile(AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'templates')+'mainforumtemplate.html');
      sl := TStringList.Create;
      with BaseApplication as IBaseConfig do
        sl.LoadFromFile(AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'templates')+'ajaxforumtemplate.html');
      FTemplate.Text:=StringReplace(FTemplate.text,'~MainTemplateContent',sl.Text,[]);
      sl.Free;
    end;
  aTemplate.Template := FTemplate.Text;
  aTemplate.OnReplaceTag :=@ReplaceMainTags;
end;
procedure TfmForum.ReplaceStdTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  aRow: string;
  i: Integer;
begin
  if AnsiCompareText(TagString, 'Title') = 0 then
    begin
      ReplaceText := HTMLEncode(Title);
    end
  else if AnsiCompareText(TagString, 'DateTime') = 0 then
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now)
  else if AnsiCompareText(TagString, 'TOPNAVIGATION') = 0 then
    begin
      ReplaceText := Tagparams.Values['HEADER'];
      ReplaceText := ReplaceText+Tagparams.Values['FOOTER'];
    end
  else if AnsiCompareText(TagString, 'SEARCHBAR') = 0 then
    begin
      ReplaceText := TagParams.Values['SEARCHFORM'];
    end;
end;
initialization
  RegisterHTTPModule('forum', TfmForum);
end.

