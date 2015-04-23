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
unit uDocumentAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids, DbCtrls, Buttons, ExtCtrls,
  uIntfStrConsts,db,LCLType, ButtonPanel, uDocuments,uBaseDBInterface,uBaseDatasetInterfaces;

type

  { TfDocumentAction }

  TfDocumentAction = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbDontUseStarter: TDBCheckBox;
    DBNavigator1: TDBNavigator;
    MimeTypesDS: TDatasource;
    DocumentActionDS: TDatasource;
    dgStandartActions: TDBGrid;
    lStandartActions: TLabel;
    cbCheckoutCompleteDir: TDBCheckBox;
    cbCheckInAlsoNewFiles: TDBCheckBox;
    rbUseSpecialAction: TRadioButton;
    rbUseStandardAction: TRadioButton;
    lActionCommand: TLabel;
    mActionCommand: TDBMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bOKClick(Sender: TObject);
    procedure rbUseSpecialActionClick(Sender: TObject);
    procedure rbUseStandardActionClick(Sender: TObject);
    procedure rbUseSpecialActionChange(Sender: TObject);
    procedure cbCheckoutCompleteDirChange(Sender: TObject);
  private
    { private declarations }
    FDocuments : TDocument;
  public
    { public declarations }
    procedure SetLanguage;
    procedure Execute(aDocuments : TDocument);
  end;

var
  fDocumentAction: TfDocumentAction;

implementation
{$R *.lfm}
uses uData;

resourcestring
  strCommand                    = 'Command';
  strFile                       = 'File';


{ TfDocumentAction }

procedure TfDocumentAction.bOKClick(Sender: TObject);
begin
  if (FDocuments.DocumentActions.DataSet.State <> dsEdit) or (FDocuments.DocumentActions.DataSet.State <> dsInsert) then
    FDocuments.DocumentActions.DataSet.Edit;
  FDocuments.DocumentActions.DataSet.Post;
  if FDocuments.DocumentActions.FieldByName('ACTIONCMD').AsString = '' then
    FDocuments.DocumentActions.DataSet.Delete;
  Close;
end;

procedure TfDocumentAction.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfDocumentAction.rbUseSpecialActionClick(Sender: TObject);
begin
  FDocuments.DocumentActions.DataSet.Edit;
  FDocuments.DocumentActions.FieldByName('ACTION').AsString := 'S';
end;

procedure TfDocumentAction.rbUseStandardActionClick(Sender: TObject);
begin
  FDocuments.DocumentActions.DataSet.Edit;
  FDocuments.DocumentActions.FieldByName('ACTION').AsString := 'D';
end;

procedure TfDocumentAction.rbUseSpecialActionChange(Sender: TObject);
begin
  if rbUseSpecialAction.Checked then
    begin
      dgStandartActions.Enabled := False;
      mActionCommand.Enabled := True;
      lActionCommand.Caption := strCommand;
    end;
  if rbUseStandardAction.Checked then
    begin
      dgStandartActions.Enabled := True;
      mActionCommand.Enabled := True;
      lActionCommand.Caption := strFile;
      //TODO:Add files
    end;
end;

procedure TfDocumentAction.cbCheckoutCompleteDirChange(Sender: TObject);
begin
  cbCheckinAlsoNewFiles.Enabled := cbCheckoutCompleteDir.Enabled;
  rbUseSpecialAction.Enabled := cbCheckoutCompleteDir.Enabled;
  rbUseStandardAction.Enabled := cbCheckoutCompleteDir.Enabled;
end;
procedure TfDocumentAction.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDocumentAction,fDocumentAction);
      Self := fDocumentAction;
    end;
end;
procedure TfDocumentAction.Execute(aDocuments : TDocument);
var
  ID: String;
  ActControl: TWinControl;
begin
  FDocuments:=aDocuments;
  DocumentActionDS.DataSet := FDocuments.DocumentActions.DataSet;
  if not FDocuments.DocumentActions.DataSet.Active then
    FDocuments.DocumentActions.DataSet.Open;
  MimeTypesDS.DataSet := FDocuments.MimeTypes.DataSet;
  FDocuments.MimeTypes.Open;
  if FDocuments.IsDir then
    begin
      cbCheckoutCompleteDir.Enabled := True;
      cbCheckinAlsoNewFiles.Enabled := True;
      dgStandartActions.Enabled := True;
      mActionCommand.Enabled := True;
    end
  else
    begin
      cbCheckoutCompleteDir.Enabled := False;
      cbCheckinAlsoNewFiles.Enabled := False;
      mActionCommand.Enabled := False;
    end;
  ID := FDocuments.FieldByName('NUMBER').AsString;
  with FDocuments.DataSet as IBaseDBFilter do
    Data.SetFilter(FDocuments.DocumentActions,Data.ProcessTerm(Data.QuoteField('NUMBER')+'='+Data.QuoteValue(ID)));
  FDocuments.DocumentActions.DataSet.First;
  if FDocuments.DocumentActions.DataSet.RecordCount = 0 then
    begin
      FDocuments.DocumentActions.DataSet.Append;
      FDocuments.DocumentActions.FieldByName('NUMBER').AsString := ID;
    end;
  rbUseSpecialAction.Checked := FDocuments.DocumentActions.FieldByName('ACTION').AsString = 'S';
  rbUseStandardAction.Checked := FDocuments.DocumentActions.FieldByName('ACTION').AsString = 'D';
  ActControl := Screen.ActiveControl;
  if Showmodal = mrOK then
    begin
      if (FDocuments.DocumentActions.DataSet.State = dsEdit)
      or (FDocuments.DocumentActions.DataSet.State = dsInsert) then
        begin
          FDocuments.DocumentActions.FieldByName('NUMBER').AsString := ID;
          FDocuments.DocumentActions.DataSet.Post;
        end;
    end;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
end;

initialization

end.

