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
Created 30.10.2014
*******************************************************************************}
unit ucommandline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, uPrometFrames, LCLType,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, ActnList,uspeakinginterface,
  uBaseDbClasses,uIntfStrConsts;

type

  { TfCommandline }

  TfCommandline = class(TPrometMainFrame)
    acNewScript: TAction;
    acEditScript: TAction;
    ActionList1: TActionList;
    Input: TMemo;
    Output: TMemo;
    Label1: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    pAdmin: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure acEditScriptExecute(Sender: TObject);
    procedure acNewScriptExecute(Sender: TObject);
    procedure FSpeakingInterfacaceWriteln(const s: string);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FSpeakingInterfacace : TSpeakingInterface;
  public
    { public declarations }
    function CheckSentence(s : string) : Boolean;
    procedure ShowFrame; override;
    procedure SetRights;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation
{$R *.lfm}
uses uData,uScriptEditor,Dialogs,uBaseDBInterface,
  uBaseVisualApplication;
procedure TfCommandline.InputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    begin
      Key := 0;
      Input.text := '';
      Output.ScrollBy(0,-100);
    end
  else if Key = VK_UP then
    begin
    end
  else if Key = VK_DOWN then
    begin
    end;
end;

procedure TfCommandline.FSpeakingInterfacaceWriteln(const s: string);
begin
  Output.Lines.Add('<'+s);
end;

procedure TfCommandline.acNewScriptExecute(Sender: TObject);
var
  aName: String;
begin
  if InputQuery(strName,strName,aName) then
    begin
      fScriptEditor.Execute('CmdLn.'+aName,Null,nil,'uses promet;'+lineending
                                              +'  function CheckSentence(Sentence : string) : Boolean;'+lineending
                                              +'  begin'+lineending
                                              +'    Result := False;'+lineending
                                              +'  end;'+lineending
                                              +'begin'+lineending
                                              +'end.');
      FSpeakingInterfacace.DataSet.Refresh;
    end;
end;

procedure TfCommandline.acEditScriptExecute(Sender: TObject);
begin
  if FSpeakingInterfacace.Count>0 then
    begin
      fScriptEditor.Execute(FSpeakingInterfacace.FieldByName('NAME').AsString,Null);
      FSpeakingInterfacace.DataSet.Refresh;
    end;
end;

function TfCommandline.CheckSentence(s: string): Boolean;
begin
  s := StringReplace(StringReplace(s,#10,'',[rfReplaceAll]),#13,'',[rfReplaceAll]);
  Output.Append('>'+s);
  Result := FSpeakingInterfacace.CheckSentence(s);
  if not Result then
    Output.Append('<'+strSentenceNotValid);
end;

procedure TfCommandline.ShowFrame;
begin
  inherited ShowFrame;
  Input.SetFocus;
  if not Assigned(FSpeakingInterfacace) then
    begin
      FSpeakingInterfacace := TSpeakingInterface.Create(nil);
      FSpeakingInterfacace.Writeln:=@FSpeakingInterfacaceWriteln;
    end;
end;

procedure TfCommandline.SetRights;
begin
  pAdmin.Visible:=Data.Users.Rights.Right('OPTIONS')>RIGHT_READ;
end;

destructor TfCommandline.Destroy;
begin
  if Assigned(FSpeakingInterfacace) then
    FSpeakingInterfacace.Destroy;
  inherited Destroy;
end;

constructor TfCommandline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfCommandline);
end.

