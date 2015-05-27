unit ueditor;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, Controls, Forms, ActnList, Menus, SynEdit, ComCtrls,
  SynEditSearch, SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterHTML, SynHighlighterXML, SynHighlighterLFM,
  synhighlighterunixshellscript, SynHighlighterPHP, SynHighlighterTeX,
  SynHighlighterSQL, SynHighlighterPerl, SynHighlighterCss,
  SynHighlighterPython, SynHighlighterDiff, SynHighlighterVB, SynHighlighterBat,
  SynHighlighterIni, SynHighlighterJScript, SynHighlighterPo;
type

  { TfEditor }

  TfEditor = class(TForm)
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditSelectAll: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    actEditFindNext: TAction;
    actEditLineEndCrLf: TAction;
    actEditLineEndCr: TAction;
    actEditLineEndLf: TAction;
    ilImageList: TImageList;
    MainMenu1: TMainMenu;
    ActListEdit: TActionList;
    actFileSave: TAction;
    actFileNew: TAction;
    actFileExit: TAction;
    miEditLineEndCr: TMenuItem;
    miEditLineEndLf: TMenuItem;
    miEditLineEndCrLf: TMenuItem;
    miLineEndType: TMenuItem;
    N5: TMenuItem;
    miEncodingOut: TMenuItem;
    miEncodingIn: TMenuItem;
    miEncoding: TMenuItem;
    miFindNext: TMenuItem;
    miDelete: TMenuItem;
    miSelectAll: TMenuItem;
    miRedo: TMenuItem;
    miDeleteContext: TMenuItem;
    miSelectAllContext: TMenuItem;
    miSeparator2: TMenuItem;
    miPasteContext: TMenuItem;
    miCopyContext: TMenuItem;
    miCutContext: TMenuItem;
    miSeparator1: TMenuItem;
    miUndoContext: TMenuItem;
    miFile: TMenuItem;
    New1: TMenuItem;
    pmContextMenu: TPopupMenu;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    miEdit: TMenuItem;
    miUndo: TMenuItem;
    N3: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    N4: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    actSaveAll: TAction;
    Editor: TSynEdit;
    miHighlight: TMenuItem;
    actEditFind: TAction;
    actEditRplc: TAction;
    actConfHigh: TAction;
    miDiv: TMenuItem;
    SynBatSyn1: TSynBatSyn;
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynDiffSyn1: TSynDiffSyn;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynIniSyn1: TSynIniSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPasSyn1: TSynPasSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynPoSyn1: TSynPoSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynTeXSyn1: TSynTeXSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynVBSyn1: TSynVBSyn;
    SynXMLSyn1: TSynXMLSyn;
    tbToolBar: TToolBar;
    tbCopy: TToolButton;
    procedure actEditLineEndCrExecute(Sender: TObject);
    procedure actEditLineEndCrLfExecute(Sender: TObject);
    procedure actEditLineEndLfExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure actSave2Execute(Sender: TObject);
    procedure frmEditorClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
    bChanged:Boolean;
    bNoName: Boolean;
    bSearchBackwards:Boolean;
    bSearchCaseSensitive:Boolean;
    bSearchFromCaret:Boolean;
    bSearchSelectionOnly:Boolean;
    bSearchWholeWords:Boolean;
    bSearchRegExp:Boolean;
    FFileName: UTF8String;
    sSearchText, sReplaceText:String;
    sEncodingIn,
    sEncodingOut,
    sOriginalText: String;

    procedure ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
    procedure SetFileName(const AValue: UTF8String);
  public
    { Public declarations }
    SynEditSearch: TSynEditSearch;
    function LoadFromFile(const aFileName: UTF8String): Boolean;
    function LoadFromStream(aStream : TStream;aExtension : string): Boolean;
    function CanHandleFile(const aFileName : UTF8String) : Boolean;
    procedure UpdateStatus;
    procedure SetEncodingIn(Sender:TObject);
    procedure SetEncodingOut(Sender:TObject);
    procedure SetHighLighter(Sender:TObject);
    procedure UpdateHighlighterStatus;
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    function SaveFile(const aFileName: UTF8String): Boolean;

    property FileName: UTF8String read FFileName write SetFileName;
    property Changed : Boolean read bChanged;
  end;
  procedure ShowEditor(const sFileName:String);
const
  SYNS_XML_DefaultText = 'Default text';
implementation
{$R *.lfm}
uses
  SynEditHighlighter, SynEditTypes, SynEditLines, LCLType,
  uminiconvencoding,uSynEditFiler;
resourcestring
  rsMsgNewFile                = 'new File.txt';
procedure ShowEditor(const sFileName:String);
var
  editor: TfEditor;
begin
  editor := TfEditor.Create(Application);

  if sFileName = '' then
    editor.actFileNew.Execute
  else
  begin
    if not editor.LoadFromFile(sFileName) then
      Exit;
  end;

  editor.ShowOnTop;
end;
procedure TfEditor.FormCreate(Sender: TObject);
var
  i:Integer;
  mi:TMenuItem;
  EncodingsList: TStringList;
begin
  miHighlight.Clear;
  for i := 0 to Self.ComponentCount-1 do
    if Components[i] is TSynCustomHighlighter then
      begin
        mi:=TMenuItem.Create(miHighlight);
        mi.Caption:=TSynCustomHighlighter(Components[i]).GetLanguageName;
        mi.Tag:=i;
        mi.Enabled:=True;
        mi.OnClick:=@SetHighLighter;
        miHighlight.Add(mi);
      end;
// update menu encoding
  miEncodingIn.Clear;
  miEncodingOut.Clear;
  EncodingsList:= TStringList.Create;
  GetSupportedEncodings(EncodingsList);
  for I:= 0 to EncodingsList.Count - 1 do
    begin
      mi:= TMenuItem.Create(miEncodingIn);
      mi.Caption:= EncodingsList[I];
      mi.AutoCheck:= True;
      mi.RadioItem:= True;
      mi.GroupIndex:= 1;
      mi.OnClick:= @SetEncodingIn;
      miEncodingIn.Add(mi);
    end;
  for I:= 0 to EncodingsList.Count - 1 do
    begin
      mi:= TMenuItem.Create(miEncodingOut);
      mi.Caption:= EncodingsList[I];
      mi.AutoCheck:= True;
      mi.RadioItem:= True;
      mi.GroupIndex:= 2;
      mi.OnClick:= @SetEncodingOut;
      miEncodingOut.Add(mi);
    end;
  EncodingsList.Free;
end;
procedure TfEditor.actEditLineEndCrExecute(Sender: TObject);
begin
  with (Editor.Lines as TSynEditLines) do
  FileWriteLineEndType:= sfleCr;
end;
procedure TfEditor.actEditLineEndCrLfExecute(Sender: TObject);
begin
  with (Editor.Lines as TSynEditLines) do
  FileWriteLineEndType:= sfleCrLf;
end;
procedure TfEditor.actEditLineEndLfExecute(Sender: TObject);
begin
  with (Editor.Lines as TSynEditLines) do
  FileWriteLineEndType:= sfleLf;
end;
function TfEditor.CanHandleFile(const aFileName: UTF8String): Boolean;
var
  Extension: String;
  ExtLen: Integer;
  i: Integer;
  Highlighter: TSynCustomHighlighter;
  Filter: String;
  j: SizeInt;
begin
  Result := False;
  Extension := LowerCase(ExtractFileExt(aFileName));
  if (Extension = '.txt')
  or (Extension = '.1st') //readme
  or (Extension = '.ans')
  or (Extension = '.asc')
  or (Extension = '.ascii')
  or (Extension = '.now') //readme
  or (Extension = '.me') //readme
  then
    begin
      Result := True;
      exit;
    end;
  ExtLen := Length(Extension);
  if (ExtLen > 0) then
    begin
      for i := 0 to ComponentCount - 1 do
        begin
          if not (Components[i] is TSynCustomHighlighter) then
            continue;
          Highlighter := TSynCustomHighlighter(Components[i]);
          Filter := LowerCase(Highlighter.DefaultFilter);
          j := Pos('|', Filter);
          if j > 0 then
            begin
              Delete(Filter, 1, j);
              j := Pos(Extension, Filter);
              if (j > 0) and
                 ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';'))
              then
                begin
                  Result := True;
                  break;
                end;
            end;
      end;
    end;
end;
function TfEditor.SaveFile(const aFileName: UTF8String): Boolean;
var
  I: Integer;
  Encoding: String;
  Writer: TSynEditFileWriter;
begin
  Result := False;
  try
    Writer := TSynEditFileWriter.Create(aFileName);
    try
      Encoding := NormalizeEncoding(sEncodingOut);
      Writer.LineEndType := (Editor.Lines as TSynEditLines).FileWriteLineEndType;
      // If file is empty and encoding UTF-8 with BOM then write only BOM
      if (Editor.Lines.Count = 0) and (Encoding = EncodingUTF8BOM) then
        Writer.Write(UTF8ToUTF8BOM(EmptyStr))
      // If file has only one line then write it without line break
      else if Editor.Lines.Count = 1 then
        Writer.Write(ConvertEncoding(Editor.Lines[0], EncodingUTF8, sEncodingOut))
      else if Editor.Lines.Count > 1 then
        begin
          Writer.WriteLine(ConvertEncoding(Editor.Lines[0], EncodingUTF8, sEncodingOut));
          // Special case for UTF-8 and UTF-8 with BOM
          if (Encoding = EncodingUTF8) or (Encoding = EncodingUTF8BOM) then
            begin
              for I := 1 to Editor.Lines.Count - 2 do
                Writer.WriteLine(Editor.Lines[I]);
              // Write last line without line break
              Writer.Write(Editor.Lines[Editor.Lines.Count - 1]);
            end
          else
            begin
              for I := 1 to Editor.Lines.Count - 2 do
                Writer.WriteLine(ConvertEncoding(Editor.Lines[I], EncodingUTF8, sEncodingOut));
              // Write last line without line break
              Writer.Write(ConvertEncoding(Editor.Lines[Editor.Lines.Count - 1], EncodingUTF8, sEncodingOut));
            end;
        end;
    finally
      Writer.Free;
    end;

    Editor.Modified:= False; // needed for the undo stack
    Editor.MarkTextAsSaved;
    Result := True;
  except
    on e: EFCreateError do
    begin
    end;
    on e: EFOpenError do
    begin
    end;
  end;
end;
procedure TfEditor.SetFileName(const AValue: UTF8String);
begin
  if FFileName = AValue then
    Exit;

  FFileName := AValue;
  Caption := FFileName;
end;
function TfEditor.LoadFromFile(const aFileName: UTF8String): Boolean;
var
  Reader: TSynEditFileReader;
  Highlighter: TSynCustomHighlighter;
  ExtLen: integer;
  i, j: integer;
  Filter: string;
  Extension : string;
  Attribute: TSynHighlighterAttributes;
begin
  Result := False;
  try
    Reader := TSynEditFileReader.Create(aFileName);
    try
      Editor.Lines.BeginUpdate;
      try
        Editor.Lines.Clear;
        while not Reader.EOF do
          Editor.Lines.Add(Reader.ReadLine);
      finally
        Editor.Lines.EndUpdate;
      end;
    finally
      with (Editor.Lines as TSynEditLines) do
      FileWriteLineEndType:= Reader.LineEndType;
      case Reader.LineEndType of
        sfleCrLf: actEditLineEndCrLf.Checked:= True;
        sfleCr:   actEditLineEndCr.Checked:= True;
        sfleLf:   actEditLineEndLf.Checked:= True;
      end;
      Reader.Free;
    end;
    Result := True;
  except
    on E: EFCreateError do
      begin
        Exit;
      end;
    on E: EFOpenError do
      begin
        Exit;
      end;
  end;
  sOriginalText := Editor.Lines.Text; // save original text
  sEncodingIn := GuessEncoding(Copy(sOriginalText, 1, 4096));
  ChooseEncoding(miEncodingIn, sEncodingIn);
  sEncodingOut := sEncodingIn; // by default
  ChooseEncoding(miEncodingOut, sEncodingOut);
  if sEncodingIn <> EncodingUTF8 then
    Editor.Lines.Text := ConvertEncoding(sOriginalText, sEncodingIn, EncodingUTF8);
  Extension := LowerCase(ExtractFileExt(aFileName));
  ExtLen := Length(Extension);
  if (ExtLen > 0) then
    begin
      for i := 0 to ComponentCount - 1 do
        begin
          if not (Components[i] is TSynCustomHighlighter) then
            continue;
          Highlighter := TSynCustomHighlighter(Components[i]);
          Filter := LowerCase(Highlighter.DefaultFilter);
          j := Pos('|', Filter);
          if j > 0 then
            begin
              Delete(Filter, 1, j);
              j := Pos(Extension, Filter);
              if (j > 0) and
                 ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';'))
              then
                begin
                  break;
                end;
            end;
          Highlighter := nil;
      end;
    end;
  Editor.Highlighter:=nil;
  if (Highlighter = nil) then Exit;
  Editor.Highlighter:= Highlighter;
  {
  I:= Highlighter.AttrCount - 1;
  repeat
    Attribute:= Highlighter.Attribute[I];
    Dec(I);
  until (I < 0) or SameText(Attribute.StoredName, SYNS_XML_DefaultText);
  Editor.Color:= Attribute.Background;
  Editor.Font.Color:= Attribute.Foreground;
  }
  UpdateHighlighterStatus;
  FileName := aFileName;
  bChanged := False;
  bNoname := False;
  UpdateStatus;
end;
function TfEditor.LoadFromStream(aStream: TStream;aExtension : string): Boolean;
var
  Extension: String;
  ExtLen: Integer;
  i: Integer;
  Highlighter: TSynCustomHighlighter;
  Filter: String;
  j: SizeInt;
  sl: TStringList;
begin
  Result := False;
  try
    sl := TStringList.Create;
    sl.LoadFromStream(aStream);
    Editor.Lines.BeginUpdate;
    try
      Editor.Lines.Clear;
      for i := 0 to sl.Count-1 do
        Editor.Lines.Add(sl[i]);
    finally
      sl.Free;
      Editor.Lines.EndUpdate;
    end;
    Result := True;
  except
    Result := False;
    exit;
  end;
  sOriginalText := Editor.Lines.Text; // save original text
  sEncodingIn := GuessEncoding(Copy(sOriginalText, 1, 4096));
  ChooseEncoding(miEncodingIn, sEncodingIn);
  sEncodingOut := sEncodingIn; // by default
  ChooseEncoding(miEncodingOut, sEncodingOut);
  if sEncodingIn <> EncodingUTF8 then
    Editor.Lines.Text := ConvertEncoding(sOriginalText, sEncodingIn, EncodingUTF8);
  Extension := LowerCase(ExtractFileExt(aExtension));
  ExtLen := Length(Extension);
  if (ExtLen > 0) then
    begin
      for i := 0 to ComponentCount - 1 do
        begin
          if not (Components[i] is TSynCustomHighlighter) then
            continue;
          Highlighter := TSynCustomHighlighter(Components[i]);
          Filter := LowerCase(Highlighter.DefaultFilter);
          j := Pos('|', Filter);
          if j > 0 then
            begin
              Delete(Filter, 1, j);
              j := Pos(Extension, Filter);
              if (j > 0) and
                 ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';'))
              then
                begin
                  break;
                end;
            end;
          Highlighter := nil;
      end;
    end;
  Editor.Highlighter:=nil;
  if (Highlighter = nil) then Exit;
  Editor.Highlighter:= Highlighter;
  {
  I:= Highlighter.AttrCount - 1;
  repeat
    Attribute:= Highlighter.Attribute[I];
    Dec(I);
  until (I < 0) or SameText(Attribute.StoredName, SYNS_XML_DefaultText);
  Editor.Color:= Attribute.Background;
  Editor.Font.Color:= Attribute.Foreground;
  }
  UpdateHighlighterStatus;
  FileName := aExtension;
  bChanged := False;
  bNoname := False;
  UpdateStatus;
end;
procedure TfEditor.actFileNewExecute(Sender: TObject);
begin
  inherited;
  FileName := rsMsgNewFile;
  Editor.Lines.Clear;
  bChanged:=False;
  bNoname:=True;
  UpdateStatus;
end;
procedure TfEditor.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
// this is hack, action hot key not work yet

  case Key of
   VK_F2:
     begin
       actFileSave.Execute;
       Key:=0;
     end;
  // To prevent else one editor or viewer open on key F4 in viewer
  VK_F4:
    begin
      if Shift <> [ssAlt] then
        Key:=0;
    end;
  VK_F3:
    begin
      actEditFindNext.Execute;
      Key:=0;
    end;
   VK_N:
     begin
       if Shift=[ssCtrl] then
       begin
         actFileNew.Execute;
         Key:=0;
       end;
     end;
   VK_S:
     begin
       if Shift=[ssCtrl] then
       begin
         actFileSave.Execute;
         Key:=0;
       end;
     end;
   VK_F:
     begin
       if Shift=[ssCtrl] then
       begin
         actEditFind.Execute;
         Key:=0;
       end;
     end;
   VK_ESCAPE:
     begin
       Close;
       Key:=0;
     end;
  end;
end;
procedure TfEditor.actEditCopyExecute(Sender: TObject);
begin
  editor.CopyToClipboard;
end;
procedure TfEditor.actEditCutExecute(Sender: TObject);
begin
  Editor.CutToClipboard;
end;
procedure TfEditor.actEditPasteExecute(Sender: TObject);
begin
  editor.PasteFromClipboard;
end;
procedure TfEditor.actEditDeleteExecute(Sender: TObject);
begin
  Editor.ClearSelection;
end;
procedure TfEditor.actEditRedoExecute(Sender: TObject);
begin
  editor.Redo;
end;
procedure TfEditor.actEditSelectAllExecute(Sender: TObject);
begin
  editor.SelectAll;
end;
procedure TfEditor.SetHighLighter(Sender:TObject);
var
  Highlighter: TSynCustomHighlighter;
begin
  Highlighter:= TSynCustomHighlighter(Components[TMenuItem(Sender).Tag]);
  UpdateHighlighterStatus;
end;
procedure TfEditor.actEditUndoExecute(Sender: TObject);
begin
  inherited;
  Editor.Undo;
  UpdateStatus;
end;
procedure TfEditor.EditorChange(Sender: TObject);
begin
  inherited;
  bChanged:=True;
  UpdateStatus;
end;
procedure TfEditor.actFileSaveExecute(Sender: TObject);
begin
end;
procedure TfEditor.UpdateStatus;
begin
  {
  if bChanged then
    StatusBar.Panels[0].Text:='*'
  else
    StatusBar.Panels[0].Text:='';
  StatusBar.Panels[1].Text:=Format('%d:%d',[Editor.CaretX, Editor.CaretY]);
//  StatusBar.Panels[2].Text:=IntToStr(Length(Editor.Lines.Text));
  }
end;
procedure TfEditor.SetEncodingIn(Sender: TObject);
begin
  sEncodingIn:= (Sender as TMenuItem).Caption;
  sEncodingOut:= sEncodingIn;
  ChooseEncoding(miEncodingOut, sEncodingOut);
  Editor.Lines.Text:= ConvertEncoding(sOriginalText, sEncodingIn, EncodingUTF8);
end;
procedure TfEditor.SetEncodingOut(Sender: TObject);
begin
  sEncodingOut:= (Sender as TMenuItem).Caption;
end;
procedure TfEditor.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  inherited;
  UpdateStatus;
end;
procedure TfEditor.UpdateHighlighterStatus;
begin
//  if assigned(Editor.Highlighter) then
//    StatusBar.Panels[3].Text:= Editor.Highlighter.GetLanguageName;
end;
procedure TfEditor.actFileExitExecute(Sender: TObject);
begin
  Close;
end;
procedure TfEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=False;
  if bChanged then
    begin
    end;
  CanClose:=True;
end;
procedure TfEditor.EditorKeyPress(Sender: TObject; var Key: Char);
begin
//  inherited;
end;
procedure TfEditor.DoSearchReplaceText(AReplace: boolean;
  ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
//  Statusbar.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if bSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not bSearchFromCaret then
    Include(Options, ssoEntireScope);
  if bSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if bSearchWholeWords then
    Include(Options, ssoWholeWord);
  if bSearchRegExp then
    Include(Options, ssoRegExpr);
  if Editor.SearchReplace(sSearchText, sReplaceText, Options) = 0 then
  begin
    if ssoBackwards in Options then
      Editor.BlockEnd := Editor.BlockBegin
    else
      Editor.BlockBegin := Editor.BlockEnd;
    Editor.CaretXY := Editor.BlockBegin;
  end;
end;
procedure TfEditor.actSave2Execute(Sender: TObject);
begin
  inherited;
  actFileSave.Execute;
end;
procedure TfEditor.frmEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;
procedure TfEditor.ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
var
  I: Integer;
begin
  sEncoding:= NormalizeEncoding(sEncoding);
  for I:= 0 to mnuMenuItem.Count - 1 do
    if SameText(NormalizeEncoding(mnuMenuItem.Items[I].Caption), sEncoding) then
      mnuMenuItem.Items[I].Checked:= True;
end;
end.
