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
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, uPrometFrames, LCLType,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, ActnList;

type

  { TfCommandline }

  TfCommandline = class(TPrometMainFrame)
    acNewScript: TAction;
    acEditScript: TAction;
    ActionList1: TActionList;
    Input: TSynMemo;
    Label3: TLabel;
    Output: TSynMemo;
    Panel1: TPanel;
    Panel4: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    function CheckSentence(s : string) : Boolean;
    procedure ShowFrame; override;
  end;

implementation

{$R *.lfm}

procedure TfCommandline.InputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    begin
      Key := 0;
      CheckSentence(Input.Text);
      Input.text := '';
      Output.ScrollBy(0,-100);
    end;
end;

function TfCommandline.CheckSentence(s: string): Boolean;
begin
  Output.Append(s);
end;

procedure TfCommandline.ShowFrame;
begin
  inherited ShowFrame;
  Input.SetFocus;
end;

end.

