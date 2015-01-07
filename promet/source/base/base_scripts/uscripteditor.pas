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
Created 08.08.2014
*******************************************************************************}
unit uScriptEditor;

interface

uses
  SysUtils, Classes, types, db, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, ComCtrls, ActnList, DbCtrls, DBGrids,
  SynEdit, SynEditTypes, SynHighlighterPas, SynCompletion,LCLType,
  uPSComponent_Default,RegExpr,
  uPSRuntime, uPSDisassembly, uPSUtils,
  uPSComponent, uPSDebugger, uPSComponent_DB, SynEditRegexSearch, 
  SynEditSearch, SynEditMiscClasses, SynEditHighlighter, SynGutterBase, SynEditMarks,
  SynEditMarkupSpecialLine, SynHighlighterSQL,uPSCompiler, uprometscripts,LCLIntf;

type

  { TfScriptEditor }

  TfScriptEditor = class(TForm)
    acNew: TAction;
    acRun: TAction;
    acPause: TAction;
    acReset: TAction;
    acSyntaxcheck: TAction;
    acSave: TAction;
    acDecompile: TAction;
    acStepover: TAction;
    acStepinto: TAction;
    acLogout: TAction;
    acRunRemote: TAction;
    ActionList1: TActionList;
    cbSyntax: TDBComboBox;
    DataSource: TDataSource;
    cbClient: TDBComboBox;
    lName1: TLabel;
    MenuItem6: TMenuItem;
    PopupMenu2: TPopupMenu;
    SelectData: TDatasource;
    gResults: TDBGrid;
    eName: TDBEdit;
    DBGrid1: TDBGrid;
    Debugger: TPSScriptDebugger;
    FindDialog: TFindDialog;
    IFPS3DllPlugin1: TPSDllPlugin;
    ilImageList: TImageList;
    Label1: TLabel;
    lName: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    messages: TListBox;
    Panel1: TPanel;
    pLeft: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pDetail: TPanel;
    HigPascal: TSynPasSyn;
    PopupMenu1: TPopupMenu;
    BreakPointMenu: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    Run1: TMenuItem;
    Splitter2: TSplitter;
    StepOver1: TMenuItem;
    StepInto1: TMenuItem;
    N1: TMenuItem;
    Reset1: TMenuItem;
    N2: TMenuItem;
    Run2: TMenuItem;
    Splitter1: TSplitter;
    Open1: TMenuItem;
    Save1: TMenuItem;
    StatusBar: TStatusBar;
    Decompile1: TMenuItem;
    N5: TMenuItem;
    Pause1: TMenuItem;
    ed: TSynEdit;
    SynEditRegexSearch: TSynEditRegexSearch;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    Searchagain1: TMenuItem;
    N6: TMenuItem;
    Gotolinenumber1: TMenuItem;
    HigSQL: TSynSQLSyn;
    Syntaxcheck1: TMenuItem;
    tmDebug: TTimer;
    ToolBar1: TToolBar;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acDecompileExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acResetExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acRunRemoteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acStepintoExecute(Sender: TObject);
    procedure acStepoverExecute(Sender: TObject);
    procedure acSyntaxcheckExecute(Sender: TObject);
    procedure cbSyntaxSelect(Sender: TObject);
    procedure DebuggerExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure edChange(Sender: TObject);
    procedure edGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure edShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure edSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure BreakPointMenuClick(Sender: TObject);
    procedure DebuggerLineInfo(Sender: TObject; const FileName: String; aPosition, Row, Col: Cardinal);
    procedure Exit1Click(Sender: TObject);
    procedure DebuggerIdle(Sender: TObject);
    procedure DebuggerExecute(Sender: TPSScript);
    procedure DebuggerAfterExecute(Sender: TPSScript);
    procedure DebuggerCompile(Sender: TPSScript);
    procedure FDataSetDataSetAfterScroll(DataSet: TDataSet);
    procedure FDataSetDataSetBeforeScroll(DataSet: TDataSet);
    procedure FDataSetWriteln(const s: string);
    procedure FormCreate(Sender: TObject);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function DebuggerNeedFile(Sender: TObject; const OrginFileName: String; var FileName, Output: String): Boolean;
    procedure DebuggerBreakpoint(Sender: TObject; const FileName: String; bPosition, Row, Col: Cardinal);
    procedure FSynCompletionExecute(Sender: TObject);
    procedure FSynCompletionSearchPosition(var APosition: integer);
    procedure FSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
      );
    procedure MenuItem6Click(Sender: TObject);
    procedure messagesClick(Sender: TObject);
    procedure messagesDblClick(Sender: TObject);
    procedure Gotolinenumber1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Searchagain1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure edDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
    procedure tmDebugTimer(Sender: TObject);
  private
    FSearchFromCaret: boolean;
    FOldStatus : string;
    FActiveLine: Longint;
    FResume: Boolean;
    FSynCompletion: TSynCompletion;
    FActiveFile: string;
    FDataSet : TBaseScript;
    Fuses : TBaseScript;
    FOldUses : TPSOnUses;
    FWasRunning: Boolean;
    ClassImporter: uPSRuntime.TPSRuntimeClassImporter;
    function Compile: Boolean;
    function Execute: Boolean;

    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;
    procedure InternalSleep(MiliSecValue: LongInt);
    procedure SetActiveFile(const Value: string);

    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure SetDataSet(AValue: TBaseScript);

    property aFile: string read FActiveFile write SetActiveFile;
  public
    constructor Create(TheOwner: TComponent); override;
    property DataSet : TBaseScript read FDataSet write SetDataSet;
    function SaveCheck: Boolean;
    function Execute(aScript: string; aConnection: TComponent = nil;DefScript : string=''): Boolean;
  end;
  TMessageObject = class
  public
    X : Integer;
    Y : Integer;
  end;

var
  fScriptEditor: TfScriptEditor;
  fLastScriptEditor: TfScriptEditor;

implementation

uses
  uFrmGotoLine,uData,uBaseApplication,genpascalscript,Utils,uSystemMessage,uStatistic,
  Clipbrd;

{$R *.lfm}

const
  isRunningOrPaused = [isRunning, isPaused];

// options - to be saved to the registry
var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;
  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

resourcestring
  STR_TEXT_NOTFOUND = 'Text not found';
  STR_UNNAMED = 'Unnamed';
  STR_SUCCESSFULLY_COMPILED = 'Erfolgreich kompiliert';
  STR_COMPILE_ERROR = 'Fehler beim kompilieren !';
  STR_SUCCESSFULLY_EXECUTED = 'Erfolgreich ausgeführt';
  STR_RUNTIME_ERROR='[Laufzeitfehler] %s(%d:%d), bytecode(%d:%d): %s'; //Birb
  STR_FORM_TITLE = 'Editor';
  STR_FORM_TITLE_RUNNING = 'Editor - Running';
  STR_INPUTBOX_TITLE = 'Script';
  STR_NOTSAVED = 'Script wurde noch nicht gespeichert, jetzt speichern?';
  strScriptRunning       = 'Das Script wurde gestartet';
function OnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
begin
  if Assigned(fScriptEditor) then
    TPascalScript(fScriptEditor.FDataSet.Script).InternalUses(Sender,Name)
  else if Assigned(flastScriptEditor) then
    TPascalScript(fLastScriptEditor.FDataSet.Script).InternalUses(Sender,Name)
end;

procedure TfScriptEditor.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if ed.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    //MessageBeep(MB_ICONASTERISK);
    Statusbar.SimpleText := STR_TEXT_NOTFOUND;
    if ssoBackwards in Options then
      ed.BlockEnd := ed.BlockBegin
    else
      ed.BlockBegin := ed.BlockEnd;
    ed.CaretXY := ed.BlockBegin;
  end;

end;

procedure TfScriptEditor.SetDataSet(AValue: TBaseScript);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  if AValue = nil then exit;
  DataSource.DataSet := FDataSet.DataSet;
  FDataSet.DataSet.BeforeScroll:=@FDataSetDataSetBeforeScroll;
  FDataSet.DataSet.AfterScroll:=@FDataSetDataSetAfterScroll;
  FDataSet.DataSet.AfterCancel:=@FDataSetDataSetAfterScroll;
end;

constructor TfScriptEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSynCompletion := TSynCompletion.Create(Self);
  FSynCompletion.CaseSensitive := False;
  FSynCompletion.AddEditor(ed);
  FSynCompletion.OnExecute:=@FSynCompletionExecute;
  FSynCompletion.OnUTF8KeyPress:=@FSynCompletionUTF8KeyPress;
  FSynCompletion.OnSearchPosition:=@FSynCompletionSearchPosition;
end;

procedure TfScriptEditor.edSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if Debugger.HasBreakPoint(Debugger.MainFileName, Line) then
  begin
    Special := True;
    if Line = FActiveLine then
    begin
      BG := clWhite;
      FG := clRed;
    end else
    begin
      FG := clWhite;
      BG := clRed;
    end;
  end else
  if Line = FActiveLine then
  begin
    Special := True;
    FG := clWhite;
    bg := clBlue;
  end else Special := False;
end;

procedure TfScriptEditor.edGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
begin
  if (lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal') and (cbClient.Text='') then
    begin
      if Debugger.HasBreakPoint(Debugger.MainFileName, Line) then
        Debugger.ClearBreakPoint(Debugger.MainFileName, Line)
      else
        Debugger.SetBreakPoint(Debugger.MainFileName, Line);
    end
  else
    begin
      if Debugger.HasBreakPoint(Debugger.MainFileName, Line) then
        Debugger.ClearBreakPoint(Debugger.MainFileName, Line);
    end;
  ed.Refresh;
end;

procedure TfScriptEditor.edShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  ASynEdit: TSynEdit;
  EditPos: types.TPoint;
  EditCaret: Classes.TPoint;
  aWord: String;
  i: Integer;
  aCont: TbtString;
begin
 ASynEdit:=ed;
 EditPos:=HintInfo^.CursorPos;
 if not PtInRect(ASynEdit.ClientRect,EditPos) then exit;
 EditCaret:=ASynEdit.PhysicalToLogicalPos(ASynEdit.PixelsToRowColumn(EditPos));
 if (EditCaret.Y<1) then exit;
 aWord := ASynEdit.GetWordAtRowCol(EditCaret);
 for i := 0 to LoadedLibs.Count-1 do
   if lowercase(TLoadedLib(LoadedLibs[i]).Name)=lowercase(aWord) then
     begin
       HintInfo^.HintStr:=TLoadedLib(LoadedLibs[i]).Code;
       HintInfo^.HideTimeout:=30000;
     end;
 if Debugger.Running then
   begin
     aCont := Debugger.GetVarContents(aWord);
     if aCont<>'' then
       begin
         HintInfo^.HintStr:=aWord+':'+aCont;
       end;
   end;
end;

procedure TfScriptEditor.acSyntaxcheckExecute(Sender: TObject);
begin
 Compile;
end;

procedure TfScriptEditor.cbSyntaxSelect(Sender: TObject);
begin
  ed.Highlighter := TSynCustomHighlighter(FindComponent('Hig'+cbSyntax.Text));
  acStepinto.Enabled:=(lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal') and (cbClient.Text='');
  acStepover.Enabled:=(lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal') and (cbClient.Text='');
end;

procedure TfScriptEditor.DebuggerExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  TPascalScript(FDataSet.Script).ClassImporter:=x;
end;

procedure TfScriptEditor.edChange(Sender: TObject);
begin
 FDataSet.Edit;
 FDataSet.FieldByName('SCRIPT').AsString:=ed.Lines.Text;
 FDataSet.FieldByName('FOLDSTATE').AsString:=ed.FoldState;
end;

procedure TfScriptEditor.acDecompileExecute(Sender: TObject);
var
  s: tbtstring;
begin
 if lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal' then
   begin
      if Compile then
        begin
          Debugger.GetCompiled(s);
          IFPS3DataToText(s, s);
          messages.AddItem(s,nil);
        end;
   end
 else if (lowercase(FDataSet.FieldByName('SYNTAX').AsString)='sql') then
   begin
     messages.AddItem(ReplaceSQLFunctions(ed.Lines.Text),nil);
   end;
end;

procedure TfScriptEditor.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
  Application.Terminate;
end;

procedure TfScriptEditor.acNewExecute(Sender: TObject);
begin
  FDataSet.Insert;
end;

procedure TfScriptEditor.acPauseExecute(Sender: TObject);
begin
  if lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal' then
    begin
      if Debugger.Exec.Status = isRunning then
        begin
          Debugger.Pause;
          Debugger.StepInto;
          acRun.Enabled:=True;
        end;
      acStepinto.Enabled:=acPause.Enabled;
      acStepover.Enabled:=acPause.Enabled;
    end
  else
    begin
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
end;

procedure TfScriptEditor.acResetExecute(Sender: TObject);
begin
  if lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal' then
    begin
      if Debugger.Exec.Status in isRunningOrPaused then
        Debugger.Stop;
      acRun.Enabled:=True;
      acPause.Enabled:=false;
      acReset.Enabled:=false;
      acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
      acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
    end
  else
    begin
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
end;

procedure TfScriptEditor.acRunExecute(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
  gResults.Visible := False;
  messages.Visible := True;
  fLastScriptEditor := Self;
  sl := TStringList.Create;
  sl.Text:=FDataSet.FieldByName('SCRIPT').AsString;
  i := 0;
  while i<sl.Count do
    begin
      if copy(sl[i],0,2)='--' then
        sl.Delete(i)
      else inc(i);
    end;
  if Assigned(SelectData.DataSet) then
    begin
      SelectData.DataSet.Free;
      SelectData.DataSet := nil;
    end;
  if lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal' then
    begin
      if Debugger.Running then
      begin
        FResume := True
      end else
      begin
        if Compile then
          begin
            SetCurrentDir(GetHomeDir);
            TPascalScript(FDataSet.Script).Runtime := Debugger.Exec;
            TPascalScript(FDataSet.Script).Compiler := Debugger.Comp;
            Debugger.Execute;
          end;
          acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
          acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
        end;
    end
  else if (lowercase(FDataSet.FieldByName('SYNTAX').AsString)='sql') and (copy(lowercase(sl.Text),0,7)='select ') then
    begin
      gResults.Visible := True;
      messages.Visible := False;
      SelectData.DataSet := Data.GetNewDataSet(FDataSet.FieldByName('SCRIPT').AsString);
      SelectData.DataSet.Open;
    end
  else if lowercase(FDataSet.FieldByName('SYNTAX').AsString)='sql' then
    begin
      messages.Clear;
      FDataSet.writeln := @FDataSetWriteln;
      acSave.Execute;
      if not FDataSet.Execute(Null) then
        messages.AddItem('failed to executing',nil);
    end
  else
    begin
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
  sl.Free;
end;

procedure TfScriptEditor.acRunRemoteExecute(Sender: TObject);
begin
  acSave.Execute;
  with BaseApplication as IBaseApplication do
    TMessageHandler(GetMessageManager).SendCommand(cbClient.Text,'ExecuteScript('+eName.Text+')');
  acRunRemote.Enabled:=false;
  FDataSet.DataSet.Refresh;
  FOldStatus := FDataSet.FieldByName('STATUS').AsString;
  FWasRunning:=False;
  tmDebug.Enabled:=True;
end;

procedure TfScriptEditor.acSaveExecute(Sender: TObject);
begin
  FDataSet.Post;
  ed.Modified := False;
  ed.MarkTextAsSaved;
end;

procedure TfScriptEditor.acStepintoExecute(Sender: TObject);
begin
  if Debugger.Exec.Status in isRunningOrPaused then
    begin
      Debugger.StepInto;
      acRun.Enabled:=True;
      acPause.Enabled:=False;
    end
  else
  begin
    if Compile then
      begin
        Debugger.StepInto;
        Debugger.Execute;
      end;
    Debugger.Comp.OnUses:=FOldUses;
  end;
  acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
  acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
end;

procedure TfScriptEditor.acStepoverExecute(Sender: TObject);
begin
  if Debugger.Exec.Status in isRunningOrPaused then
    begin
      Debugger.StepOver;
      acRun.Enabled:=True;
      acPause.Enabled:=False;
    end
  else
  begin
    if Compile then
    begin
      Debugger.StepInto;
      Debugger.Execute;
    end;
    Debugger.Comp.OnUses:=FOldUses;
  end;
end;

procedure TfScriptEditor.BreakPointMenuClick(Sender: TObject);
var
  Line: Longint;
begin
  Line := Ed.CaretY;
  if Debugger.HasBreakPoint(Debugger.MainFileName, Line) then
    Debugger.ClearBreakPoint(Debugger.MainFileName, Line)
  else
    Debugger.SetBreakPoint(Debugger.MainFileName, Line);
  ed.Refresh;
end;

procedure TfScriptEditor.DebuggerLineInfo(Sender: TObject; const FileName: String; aPosition, Row,
  Col: Cardinal);
begin
  try
  if not FDataSet.Active then exit;
  if lowercase(FDataSet.FieldByName('SYNTAX').AsString)='pascal' then
    begin
      if Debugger.Exec.DebugMode <> dmRun then
      begin
        FActiveLine := Row;
        if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
        begin
          Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
        end;
        ed.CaretY := FActiveLine;
        ed.CaretX := 1;
        ed.Refresh;
      end
      else
        Application.ProcessMessages;
    end
  else
    begin
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
  except
  end;
end;

procedure TfScriptEditor.Exit1Click(Sender: TObject);
begin
  acReset.Execute; //terminate any running script
  if SaveCheck then //check if script changed and not yet saved
    Close;
end;

function TfScriptEditor.Compile: Boolean;
var
  i: Longint;
  mo: TMessageObject;
begin
  TPascalScript(FDataSet.Script).Compiler:=Debugger.Comp;
  TPascalScript(FDataSet.Script).Runtime:=Debugger.Exec;
  Debugger.OnExecImport:=@DebuggerExecImport;
  Debugger.Script.Assign(ed.Lines);
  try
    Result := Debugger.Compile;
  except
    on e : Exception do
      Messages.Items.Add(e.Message);
  end;
  messages.Clear;
  for i := 0 to Debugger.CompilerMessageCount -1 do
    begin
      mo := TMessageObject.Create;
      mo.X:=Debugger.CompilerMessages[i].Col;
      mo.Y:=Debugger.CompilerMessages[i].Row;
      Messages.Items.AddObject(Debugger.CompilerMessages[i].MessageToString,mo);
    end;
  if Result then
    Messages.Items.Add(STR_SUCCESSFULLY_COMPILED)
  else
    begin
      if Debugger.CompilerMessageCount=0 then
        messages.Items.Add(Debugger.ExecErrorToString);
      Messages.Items.Add(STR_COMPILE_ERROR);
      Debugger.Comp.OnUses:=FOldUses;
    end;
end;

procedure TfScriptEditor.DebuggerIdle(Sender: TObject);
begin
  Application.ProcessMessages;
  if FResume then
  begin
    FResume := False;
    Debugger.Resume;
    FActiveLine := 0;
    ed.Refresh;
  end;
end;

procedure TfScriptEditor.DebuggerExecute(Sender: TPSScript);
begin
  Caption := STR_FORM_TITLE_RUNNING;
  acRun.Enabled:=False;
  acReset.Enabled:=True;
  acPause.Enabled:=True;
  acStepinto.Enabled:=acPause.Enabled;
  acStepover.Enabled:=acPause.Enabled;
end;

procedure TfScriptEditor.DebuggerAfterExecute(Sender: TPSScript);
begin
  acRun.Enabled:=True;
  acReset.Enabled:=False;
  acPause.Enabled:=false;
  Caption := STR_FORM_TITLE;
  FActiveLine := 0;
  ed.Refresh;
  ClassImporter.Free;
  Debugger.Comp.OnUses:=FOldUses;
end;

function TfScriptEditor.Execute: Boolean;
begin
  if Debugger.Execute then
  begin
    Messages.Items.Add(STR_SUCCESSFULLY_EXECUTED);
    Result := True; 
  end else
  begin
    messages.Items.Add(Format(STR_RUNTIME_ERROR, [extractFileName(aFile), Debugger.ExecErrorRow,Debugger.ExecErrorCol,Debugger.ExecErrorProcNo,Debugger.ExecErrorByteCodePosition,Debugger.ExecErrorToString])); //Birb
    Result := False;
  end;
end;

procedure TfScriptEditor.DebuggerCompile(Sender: TPSScript);
begin
  FOldUses:=Sender.Comp.OnUses;
  FDataSet.Writeln:=@FDataSetWriteln;
  Sender.Comp.OnUses:=@OnUses;
  OnUses(Sender.Comp,'SYSTEM');
end;

procedure TfScriptEditor.FDataSetDataSetAfterScroll(DataSet: TDataSet);
begin
 ed.Lines.Text:=FDataSet.FieldByName('SCRIPT').AsString;
 aFile := FDataSet.FieldByName('NAME').AsString;
 ed.FoldState := FDataSet.FieldByName('FOLDSTATE').AsString;

 cbSyntaxSelect(cbSyntax);
end;

procedure TfScriptEditor.FDataSetDataSetBeforeScroll(DataSet: TDataSet);
begin
  SaveCheck;
end;

procedure TfScriptEditor.FDataSetWriteln(const s: string);
begin
 messages.Items.Add(S);
 messages.ItemIndex:=messages.Items.Count-1;
 messages.MakeCurrentVisible;
end;

procedure TfScriptEditor.FormCreate(Sender: TObject);
begin
  FDataSet:=nil;
  Fuses := nil;
end;

function TfScriptEditor.InternalParamStr(Param: Integer): String;
begin
  result := '';
end;

function TfScriptEditor.InternalParamCount: Integer;
begin
  Result := 0;
end;

procedure TfScriptEditor.InternalSleep(MiliSecValue: LongInt);
var
  aTime: QWord;
begin
  aTime := GetTickCount64;
  while GetTickCount64-aTime < MiliSecValue do
    Application.ProcessMessages;
end;

//check if script changed and not yet saved//
function TfScriptEditor.SaveCheck: Boolean;
begin
  if ed.Modified then
    begin
      case MessageDlg(STR_NOTSAVED, mtConfirmation, mbYesNoCancel, 0) of
        mrYes:
          begin
            acSave.Execute;
            Result := aFile <> '';
          end;
        mrNo: Result := True;
        else
          Result := False;
      end;
    end else Result := True;
end;

function TfScriptEditor.Execute(aScript: string; aConnection: TComponent;
  DefScript: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfScriptEditor,fScriptEditor);
      Self := fScriptEditor;
    end;
  if not Assigned(FDataSet) then
    begin
      FDataSet := TBaseScript.CreateEx(nil,Data,aConnection);
      FDataSet.CreateTable;
    end;
  FDataSet.Open;
  DataSource.DataSet := FDataSet.DataSet;
  FDataSet.DataSet.BeforeScroll:=@FDataSetDataSetBeforeScroll;
  FDataSet.DataSet.AfterScroll:=@FDataSetDataSetAfterScroll;
  FDataSet.DataSet.AfterCancel:=@FDataSetDataSetAfterScroll;
  if (not FDataSet.Locate('NAME',aScript,[loCaseInsensitive])) or (aScript='') then
    begin
      FDataSet.Insert;
      FDataSet.FieldByName('NAME').AsString:=aScript;
      if DefScript<>'' then
        FDataSet.FieldByName('SCRIPT').AsString:=DefScript;
    end
  else
    FDataSetDataSetAfterScroll(FDataSet.DataSet);
  Result := Showmodal = mrOK;
  if Result then
    FDataSet.Post;
  FDataSet.Close;
end;

procedure TfScriptEditor.edStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar.Panels[0].Text := IntToStr(ed.CaretY)+':'+IntToStr(ed.CaretX);
  acSave.Enabled := ed.Modified or (Assigned(FDataset) and (FDataSet.CanEdit));
end;

function TfScriptEditor.DebuggerNeedFile(Sender: TObject; const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  path: string;
begin
  if not Assigned(Fuses) then
    begin
      Fuses := TBaseScript.Create(nil);
      Fuses.Open;
    end
  else Fuses.DataSet.Refresh;
  Result := Fuses.Locate('NAME',FileName,[loCaseInsensitive]);
  if Result then
    Output:=Fuses.FieldByName('SCRIPT').AsString;
end;

procedure TfScriptEditor.DebuggerBreakpoint(Sender: TObject; const FileName: String; bPosition, Row,
  Col: Cardinal);
begin
 acRun.Enabled:=True;
 acPause.Enabled:=False;
  FActiveLine := Row;
  if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
  begin
    Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
  end;
  ed.CaretY := FActiveLine;
  ed.CaretX := 1;

  ed.Refresh;
  acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
  acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
end;

procedure TfScriptEditor.FSynCompletionExecute(Sender: TObject);
function GetCurWord:string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with TSynCompletion(Sender).Editor do
    begin
      S:=Trim(Copy(LineText, 1, CaretX));
      I:=Length(S);
      while (i>0) and (S[i]<>'.') do Dec(I);
      if (I>0) then
      begin
        J:=i-1;
        //Get table name
        while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
        Result:=trim(Copy(S, j+1, i-j-1));
      end;
    end;
end;
var
  i: Integer;
  aStatement: String;
  s: String;
  aStrings: TStrings;
  ps : PChar;
  aTp: TPSTypeRec;
  aTyp: TPSType;
  a: Integer;
  aVar: PIFVariant;
  sl: TStringList;
begin
  with FSynCompletion.ItemList do
    begin
      Clear;
      s := GetCurWord;
      if cbSyntax.Text='SQL' then
        begin
          if s = '' then
            begin
              for i := 0 to Data.Tables.Count-1 do
                Add(Data.Tables[i]);
              Add('select');
              Add('insert');
              Add('update');
              Add('delete');
              Add('from');
              Add('where');
              Add('into');
              Add('order by');
              Add('group by');
            end;
        end
      else if cbSyntax.Text='Pascal' then
        begin
          sl := TStringList.Create;
          try
            SplitRegExpr('(.*):(.*);',FDataSet.FieldByName('SCRIPT').AsString,sl);
            for a := 0 to sl.Count-1 do
              Add(sl[a]);
          except
          end;
          sl.Free;
        end;
    end;
end;

procedure TfScriptEditor.FSynCompletionSearchPosition(var APosition: integer);
var
  i: Integer;
begin
  for i := 0 to FSynCompletion.ItemList.Count-1 do
    if Uppercase(copy(FSynCompletion.ItemList[i],0,length(FSynCompletion.CurrentString))) = Uppercase(FSynCompletion.CurrentString) then
      begin
        aPosition := i;
        FSynCompletion.TheForm.Position:=i-1;
        FSynCompletion.TheForm.Position:=i;
        exit;
      end;
end;

procedure TfScriptEditor.FSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if (length(UTF8Key)=1) and (System.Pos(UTF8Key[1],FSynCompletion.EndOfTokenChr)>0) then
    begin
      FSynCompletion.TheForm.OnValidate(Sender,UTF8Key,[]);
      UTF8Key:='';
    end
end;

procedure TfScriptEditor.MenuItem6Click(Sender: TObject);
begin
  if messages.ItemIndex>-1 then
    Clipboard.AsText:=messages.Items[messages.ItemIndex];
end;

procedure TfScriptEditor.messagesClick(Sender: TObject);
var
  mo: TObject;
begin
  if messages.ItemIndex>-1 then
    begin
      mo := messages.Items.Objects[messages.ItemIndex];
      if Assigned(mo) then
        begin
          ed.CaretY:=TMessageObject(mo).Y;
          ed.CaretX:=TMessageObject(mo).X;
        end;
    end;
end;

procedure TfScriptEditor.SetActiveFile(const Value: string);
begin
  FActiveFile := Value;
  Debugger.MainFileName := ExtractFileName(FActiveFile);
  if Debugger.MainFileName = '' then
    Debugger.MainFileName := STR_UNNAMED;
end;

function GetErrorRowCol(const inStr: string): TPoint;
var
  Row:string;
  Col:string;
  p1,p2,p3:integer;
begin
  p1:=Pos('(',inStr);
  p2:=Pos(':',inStr);
  p3:=Pos(')',inStr);
  if (p1>0) and (p2>p1) and (p3>p2) then
  begin
    Row := Copy(inStr, p1+1,p2-p1-1);
    Col := Copy(inStr, p2+1,p3-p2-1);
    Result.x := StrToInt(Trim(Col));
    Result.y := StrToInt(Trim(Row));
  end
  else
  begin
    Result.x := 1;
    Result.y := 1;
  end
end;

procedure TfScriptEditor.messagesDblClick(Sender: TObject);
begin
  ed.CaretXY := GetErrorRowCol(messages.Items[messages.ItemIndex]);
  ActiveControl:=ed;
end;

procedure TfScriptEditor.Gotolinenumber1Click(Sender: TObject);
begin
  with TfrmGotoLine.Create(self) do
  try
    Char := ed.CaretX;
    Line := ed.CaretY;
    ShowModal;
    if ModalResult = mrOK then
      ed.CaretXY := CaretXY;
  finally
    Free;
    ed.SetFocus;
  end;
end;

procedure TfScriptEditor.Find1Click(Sender: TObject);
begin
end;

procedure TfScriptEditor.Searchagain1Click(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TfScriptEditor.Replace1Click(Sender: TObject);
begin
end;

procedure TfScriptEditor.edDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
 if AFiles.Count>=1 then
  if SaveCheck then //check if script changed and not yet saved
    begin
      FDataSet.Insert;
      ed.ClearAll;
      ed.Lines.LoadFromFile(AFiles[0]);
      ed.Modified := True;
      aFile := AFiles[0];
    end;
end;

procedure TfScriptEditor.tmDebugTimer(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
 FDataSet.DataSet.Refresh;
 if FDataSet.FieldByName('STATUS').AsString=FOldStatus then exit;
 FOldStatus:=FDataSet.FieldByName('STATUS').AsString;
 if FDataSet.FieldByName('STATUS').AsString='R' then
   begin
     messages.AddItem(strScriptRunning,nil);
     FWasRunning := True;
   end
 else if FWasRunning then
   begin
     acRunRemote.Enabled:=True;
     tmDebug.Enabled:=False;
     sl := TStringList.Create;
     sl.Text:=FDataSet.FieldByName('LASTRESULT').AsString;
     for i := 0 to sl.Count-1 do
       messages.AddItem(sl[i],nil);
     sl.Free;
   end;
end;

end.

