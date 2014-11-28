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
Created 11.06.2012
*******************************************************************************}
unit uscriptimport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ExtCtrls, Buttons, EditBtn, db;

type
  TImporterCapability = (icImport,icExport);
  TImporterCapabilities = set of TImporterCapability;

  { TfScriptImport }

  TfScriptImport = class(TForm)
    bpButtons: TButtonPanel;
    cbFormat: TComboBox;
    EditButton1: TEditButton;
    Label1: TLabel;
    Label2: TLabel;
    lInfo: TLabel;
    Panel1: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure FormCreate(Sender: TObject);
  private
    FAppend: Boolean;
    FConfigDir: string;
    FFilter: string;
    FTraget: TDataSource;
    FTyp : TImporterCapability;
    procedure CheckAll;
    { private declarations }
  public
    { public declarations }
    function Execute(Typ : TImporterCapability;DefaultFormat : string = '') : Boolean;
  end;

var
  fScriptImport: TfScriptImport;

resourcestring
  strPleaseenteranFormatName            = 'Bitte geben Sie einen Namen für das Format an !';
  strImporterclassnotFound              = 'Datenquellenklasse wurde nicht gefunden !';
  strPleasSelectanImportfilebeforeEdit  = 'Bitte wählen Sie eine Datenquellenklasse !';
  strSelectAnFormat                     = 'Bitte wählen Sie ein Datenformat, oder erstellen Sie ein neues. Geben Sie dazu einen Namen für das neue Format ein, und klicken Sie den Konfigurieren Knopf.';
  strSelectAnDataSource                 = 'Bitte wählen Sie eine Datenquelle';
  strCreateAnFormat                     = 'Bitte erstellen Sei ein Datenformat. Geben Sie dazu einen Namen für das neue Format ein, und klicken Sie den Konfigurieren Knopf.';
  strConfigureDataSource                = 'Setzen Sie die Quelle / Einstellungen der Datenquelle und klicken Sie OK';
  strDataImport                         = 'Datenimport';
  strDataExport                         = 'Datenexport';
  strDataSource                         = 'Datenquelle';
  strDataDestination                    = 'Datenausgabe';

implementation

{$R *.lfm}

{ TfScriptImport }

procedure TfScriptImport.FormCreate(Sender: TObject);
begin

end;

procedure TfScriptImport.CheckAll;
begin

end;

function TfScriptImport.Execute(Typ: TImporterCapability; DefaultFormat: string
  ): Boolean;
begin

end;

end.

