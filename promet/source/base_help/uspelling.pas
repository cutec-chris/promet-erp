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
Created 13.03.2014
*******************************************************************************}
unit uspelling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ActnList,uBaseVisualControls,Aspell,contnrs;

type

  { TfSpellCheck }

  TfSpellCheck = class(TForm)
    acAbort: TAction;
    acUse: TAction;
    ActionList1: TActionList;
    cbWords: TComboBox;
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure acAbortExecute(Sender: TObject);
    procedure acUseExecute(Sender: TObject);
  private
    { private declarations }
    FMemo: TCustomMemo;
  public
    { public declarations }
    function Execute(aMemo : TCustomMemo;aStart : Integer) : Boolean;
  end;

var
  fSpellCheck: TfSpellCheck;

implementation

{$R *.lfm}

{ TfSpellCheck }

procedure TfSpellCheck.acUseExecute(Sender: TObject);
begin
  FMemo.SelText:=cbWords.Text;
  ModalResult:=mrOK;
end;

procedure TfSpellCheck.acAbortExecute(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

function TfSpellCheck.Execute(aMemo: TCustomMemo; aStart: Integer): Boolean;
var
  aProc: TAspellProcess;
  aList: TObjectList;
  i: Integer;
  aEntry: TSpellingError;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfSpellCheck,fSpellCheck);
      Self := fSpellCheck;
    end;
  FMemo := aMemo;
  aProc := TAspellProcess.Create('none','de');
  aList := TObjectList.create;
  aProc.CheckString(copy(aMemo.Text,aStart,length(amemo.Text)),aList);
  for i := 0 to aList.Count-1 do
    begin
      aEntry := TSpellingError(aList[i]);
      cbWords.Items.Delimiter:=',';
      cbWords.Items.DelimitedText := aEntry.Suggestions;
      aMemo.SelStart:=aEntry.Offset;
      aMemo.SelLength:=length(aEntry.Word);
      cbWords.Text:=aEntry.Word;
      FMemo.SetFocus;
      if ShowModal = mrAbort then break;
    end;
  aList.Free;
  aProc.Free;
end;

end.

