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
  SysUtils, Classes, db, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, ComCtrls, ActnList, DbCtrls, DBGrids,
  SynEdit, SynEditTypes, SynHighlighterPas,
  uPSComponent_StdCtrls, uPSComponent_Forms,
  uPSComponent_Default, uPSComponent_Controls,
  uPSRuntime, uPSDisassembly, uPSUtils,
  uPSComponent, uPSDebugger, SynEditRegexSearch, 
  SynEditSearch, SynEditMiscClasses, SynEditHighlighter, SynGutterBase, SynEditMarks, SynEditMarkupSpecialLine;

type
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
    ActionList1: TActionList;
    cbSyntax: TDBComboBox;
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    Debugger: TPSScriptDebugger;
    FindDialog: TFindDialog;
    IFPS3DllPlugin1: TPSDllPlugin;
    ilImageList: TImageList;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    pDetail: TPanel;
    pashighlighter: TSynPasSyn;
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
    Exit1: TMenuItem;
    messages: TListBox;
    Splitter1: TSplitter;
    N3: TMenuItem;
    N4: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    StatusBar: TStatusBar;
    Decompile1: TMenuItem;
    N5: TMenuItem;
    IFPS3CE_Controls1: TPSImport_Controls;
    IFPS3CE_DateUtils1: TPSImport_DateUtils;
    IFPS3CE_Std1: TPSImport_Classes;
    IFPS3CE_Forms1: TPSImport_Forms;
    IFPS3CE_StdCtrls1: TPSImport_StdCtrls;
    Pause1: TMenuItem;
    ed: TSynEdit;
    SynEditRegexSearch: TSynEditRegexSearch;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    Searchagain1: TMenuItem;
    N6: TMenuItem;
    Gotolinenumber1: TMenuItem;
    Syntaxcheck1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure acDecompileExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acResetExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acStepintoExecute(Sender: TObject);
    procedure acStepoverExecute(Sender: TObject);
    procedure acSyntaxcheckExecute(Sender: TObject);
    procedure edGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure edSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure BreakPointMenuClick(Sender: TObject);
    procedure DebuggerLineInfo(Sender: TObject; const FileName: String; aPosition, Row, Col: Cardinal);
    procedure Exit1Click(Sender: TObject);
    procedure DebuggerIdle(Sender: TObject);
    procedure DebuggerExecute(Sender: TPSScript);
    procedure DebuggerAfterExecute(Sender: TPSScript);
    procedure DebuggerCompile(Sender: TPSScript);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure Decompile1Click(Sender: TObject);
    function DebuggerNeedFile(Sender: TObject; const OrginFileName: String; var FileName, Output: String): Boolean;
    procedure DebuggerBreakpoint(Sender: TObject; const FileName: String; bPosition, Row, Col: Cardinal);
    procedure Pause1Click(Sender: TObject);
    procedure messagesDblClick(Sender: TObject);
    procedure Gotolinenumber1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Searchagain1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure edDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
  private
    FSearchFromCaret: boolean;
    FActiveLine: Longint;
    FResume: Boolean;
    FActiveFile: string;
    function Compile: Boolean;
    function Execute: Boolean;

    procedure Writeln(const s: string);
    procedure Readln(var s: string);
    procedure SetActiveFile(const Value: string);

    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);

    property aFile: string read FActiveFile write SetActiveFile;
  public
    function SaveCheck: Boolean;
    function Execute(aScript : string = '') : Boolean;
  end;

var
  fScriptEditor: TfScriptEditor;

implementation

uses
  uFrmGotoLine;

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
  STR_SUCCESSFULLY_COMPILED = 'Succesfully compiled';
  STR_SUCCESSFULLY_EXECUTED = 'Succesfully executed';
  STR_RUNTIME_ERROR='[Runtime error] %s(%d:%d), bytecode(%d:%d): %s'; //Birb
  STR_FORM_TITLE = 'Editor';
  STR_FORM_TITLE_RUNNING = 'Editor - Running';
  STR_INPUTBOX_TITLE = 'Script';
  STR_DEFAULT_PROGRAM = 'Program test;'#13#10'begin'#13#10'end.';
  STR_NOTSAVED = 'File has not been saved, save now?';

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
  if Debugger.HasBreakPoint(Debugger.MainFileName, Line) then
    Debugger.ClearBreakPoint(Debugger.MainFileName, Line)
  else
    Debugger.SetBreakPoint(Debugger.MainFileName, Line);
  ed.Refresh;
end;

procedure TfScriptEditor.acSyntaxcheckExecute(Sender: TObject);
begin
 Compile;
end;

procedure TfScriptEditor.acDecompileExecute(Sender: TObject);
var
  s: tbtstring;
begin
  if Compile then
  begin
    Debugger.GetCompiled(s);
    IFPS3DataToText(s, s);
    //debugoutput.output.Lines.Text := s;
    //debugoutput.visible := true;
  end;
end;

procedure TfScriptEditor.acPauseExecute(Sender: TObject);
begin
  if Debugger.Exec.Status = isRunning then
    begin
      Debugger.Pause;
      Debugger.StepInto;
      acRun.Enabled:=True;
    end;
  acStepinto.Enabled:=acPause.Enabled;
  acStepover.Enabled:=acPause.Enabled;
end;

procedure TfScriptEditor.acResetExecute(Sender: TObject);
begin
  if Debugger.Exec.Status in isRunningOrPaused then
    Debugger.Stop;
  acRun.Enabled:=True;
  acPause.Enabled:=false;
  acReset.Enabled:=false;
  acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
  acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
end;

procedure TfScriptEditor.acRunExecute(Sender: TObject);
begin
  if Debugger.Running then
  begin
    FResume := True
  end else
  begin
    if Compile then
      Debugger.Execute;
  end;
  acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
  acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
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
begin
  Debugger.Script.Assign(ed.Lines);
  Result := Debugger.Compile;
  messages.Clear;
  for i := 0 to Debugger.CompilerMessageCount -1 do
  begin
    Messages.Items.Add(Debugger.CompilerMessages[i].MessageToString);
  end;
  if Result then
    Messages.Items.Add(STR_SUCCESSFULLY_COMPILED);
end;

procedure TfScriptEditor.DebuggerIdle(Sender: TObject);
begin
  Application.ProcessMessages; //Birb: don't use Application.HandleMessage here, else GUI will be unrensponsive if you have a tight loop and won't be able to use Run/Reset menu action
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
  Debugger.SetVarToInstance('SELF', Self);
  Debugger.SetVarToInstance('APPLICATION', Application);
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

procedure TfScriptEditor.Writeln(const s: string);
begin
  //debugoutput.output.Lines.Add(S);
  //debugoutput.Visible := True;
end;

procedure TfScriptEditor.DebuggerCompile(Sender: TPSScript);
begin
  Sender.AddMethod(Self, @TfScriptEditor.Writeln, 'procedure writeln(s: string)');
  Sender.AddMethod(Self, @TfScriptEditor.Readln, 'procedure readln(var s: string)');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('Application', 'TApplication');
end;

procedure TfScriptEditor.Readln(var s: string);
begin
  s := InputBox(STR_INPUTBOX_TITLE, '', '');
end;

procedure TfScriptEditor.New1Click(Sender: TObject);
begin
  if SaveCheck then //check if script changed and not yet saved
  begin
    ed.ClearAll;
    ed.Lines.Text := STR_DEFAULT_PROGRAM;
    ed.Modified := False;
    aFile := '';
  end;
end;

procedure TfScriptEditor.Open1Click(Sender: TObject);
begin
  if SaveCheck then //check if script changed and not yet saved
    begin
    end;
end;

procedure TfScriptEditor.Save1Click(Sender: TObject);
begin
  if aFile <> '' then
  begin
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end else ;//SaveAs1Click(nil);
end;

//check if script changed and not yet saved//
function TfScriptEditor.SaveCheck: Boolean;
begin
  if ed.Modified then
  begin
    case MessageDlg(STR_NOTSAVED, mtConfirmation, mbYesNoCancel, 0) of
      mrYes:
        begin
          Save1Click(nil);
          Result := aFile <> '';
        end;
      mrNo: Result := True;
      else
        Result := False;
    end;
  end else Result := True;
end;

function TfScriptEditor.Execute(aScript: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfScriptEditor,fScriptEditor);
      Self := fScriptEditor;
    end;
  Result := Showmodal = mrOK;
end;

procedure TfScriptEditor.edStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar.Panels[0].Text := IntToStr(ed.CaretY)+':'+IntToStr(ed.CaretX)
end;

procedure TfScriptEditor.Decompile1Click(Sender: TObject);
begin

end;

function TfScriptEditor.DebuggerNeedFile(Sender: TObject; const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  path: string;
  f: TFileStream;
begin
  if aFile <> '' then
    Path := ExtractFilePath(aFile)
  else
    Path := ExtractFilePath(ParamStr(0));
  Path := Path + FileName;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    Result := false;
    exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
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

procedure TfScriptEditor.Pause1Click(Sender: TObject);
begin

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
  //if Copy(messages.Items[messages.ItemIndex],1,7)= '[Error]' then
  //begin
    ed.CaretXY := GetErrorRowCol(messages.Items[messages.ItemIndex]);
    ed.SetFocus;
  //end;
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
    ed.ClearAll;
    ed.Lines.LoadFromFile(AFiles[0]);
    ed.Modified := False;
    aFile := AFiles[0];
  end;
end;

end.

