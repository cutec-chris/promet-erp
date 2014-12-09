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
*******************************************************************************}
unit uMandantOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  Dialogs, Buttons, ExtDlgs, ComCtrls, db, uOptionsFrame, uBaseDBClasses,
  uBaseDBInterface;

type

  { TfMandantOptions }

  TfMandantOptions = class(TOptionsFrame)
    bExportConfiguration: TButton;
    bTreeEntrys: TButton;
    eAccount: TDBEdit;
    eFax: TDBEdit;
    eInstitute: TDBEdit;
    eInternet: TDBEdit;
    eMail: TDBEdit;
    eSortcode: TDBEdit;
    eTel1: TDBEdit;
    eTel10: TDBEdit;
    eTel11: TDBEdit;
    eTel12: TDBEdit;
    eTel2: TDBEdit;
    eTel3: TDBEdit;
    eTel4: TDBEdit;
    eTel5: TDBEdit;
    eTel6: TDBEdit;
    eTel7: TDBEdit;
    eTel8: TDBEdit;
    eTel9: TDBEdit;
    iImage: TDBImage;
    eName: TDBEdit;
    iPreview: TDBImage;
    Label1: TLabel;
    lAccount: TLabel;
    lAdress: TLabel;
    lFax: TLabel;
    lInstitute: TLabel;
    lInternet: TLabel;
    lMail: TLabel;
    lMandantDetails: TLabel;
    lName: TLabel;
    lSortCode: TLabel;
    lTel1: TLabel;
    lTel10: TLabel;
    lTel11: TLabel;
    lTel12: TLabel;
    lTel13: TLabel;
    lTel2: TLabel;
    lTel3: TLabel;
    lTel4: TLabel;
    lTel5: TLabel;
    lTel6: TLabel;
    lTel7: TLabel;
    lTel8: TLabel;
    lTel9: TLabel;
    mAdress: TDBMemo;
    MandantDetailDS: TDatasource;
    OpenPictureDialog: TOpenPictureDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pMandantDetails: TPanel;
    sbAddImage: TSpeedButton;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure bExportConfigurationClick(Sender: TObject);
    procedure bTreeEntrysClick(Sender: TObject);
    procedure sbAddImageClick(Sender: TObject);
  private
    { private declarations }
    aConnection: TComponent;
    aMandant: TMandantDetails;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;
implementation
{$R *.lfm}
uses uData,uImpCSV,uOrder,uDocuments;

procedure TfMandantOptions.bExportConfigurationClick(Sender: TObject);
var
  OutputDir: String;
  aOrder: TOrder;
  aTemplates: TDocumentTemplates;
begin
  if SelectDirectoryDialog.Execute then
    begin
      OutputDir := SelectDirectoryDialog.FileName;
      ForceDirectoriesUTF8(OutputDir);
      CSVExport(OutputDir+DirectorySeparator+'countries.csv',';',Data.Countries.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'currency.csv',';',Data.Currency.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'dispatchtypes.csv',';',Data.Dispatchtypes.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'filters.csv',';',Data.Filters.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'forms.csv',';',Data.Forms.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'languages.csv',';',Data.Languages.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'numbers.csv',';',Data.Numbers.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'orderpostyp.csv',';',Data.orderPosTyp.DataSet);
      aOrder := TOrder.CreateEx(Self,Data);
      aOrder.Select(0);
      aOrder.Open;
      aOrder.OrderType.Open;
      CSVExport(OutputDir+DirectorySeparator+'ordertype.csv',';',aOrder.OrderType.DataSet);
      aOrder.Free;
      CSVExport(OutputDir+DirectorySeparator+'paymenttargets.csv',';',Data.PaymentTargets.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'numbers.csv',';',Data.Numbers.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'pricetypes.csv',';',Data.Pricetypes.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'reports.csv',';',Data.Reports.DataSet);
      aTemplates := TDocumentTemplates.Create(nil);
      CSVExport(OutputDir+DirectorySeparator+'templates.csv',';',aTemplates.DataSet);
      aTemplates.Free;
      CSVExport(OutputDir+DirectorySeparator+'texttyp.csv',';',Data.Texttyp.DataSet);
      try
        CSVExport(OutputDir+DirectorySeparator+'userfielddefs.csv',';',Data.Userfielddefs.DataSet);
      except
      end;
      CSVExport(OutputDir+DirectorySeparator+'vat.csv',';',Data.Vat.DataSet);
      try
        //CSVExport(OutputDir+DirectorySeparator+'statistic.csv',';',Data.Statistic.DataSet);
      except
      end;
      CSVExport(OutputDir+DirectorySeparator+'states.csv',';',Data.States.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'units.csv',';',Data.Units.DataSet);
      CSVExport(OutputDir+DirectorySeparator+'storagetype.csv',';',Data.StorageType.DataSet);
    end;
end;

procedure TfMandantOptions.bTreeEntrysClick(Sender: TObject);
var
  aTree: TTree;
begin
  aTree := TTree.Create(nil);
  with aTree.DataSet as IBaseDBFilter do
    begin
      UsePermissions:=False;
    end;
  aTree.Open;
  aTree.ImportStandartEntrys;
  aTree.Free;
end;

procedure TfMandantOptions.sbAddImageClick(Sender: TObject);
begin
  if OpenpictureDialog.Execute then
    begin
      if not aMandant.CanEdit then
        MandantDetailDS.DataSet.Edit;
      iPreview.Picture.LoadFromFile(OpenPictureDialog.FileName);
    end;
end;

constructor TfMandantOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aMandant := TMandantDetails.CreateEx(Self,Data,aConnection);
  MandantDetailDS.DataSet := aMandant.DataSet;
end;

destructor TfMandantOptions.Destroy;
begin
  aMandant.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfMandantOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aMandant.CreateTable;
  aMandant.Open;
end;

procedure TfMandantOptions.CommitTransaction;
begin
  if aMandant.CanEdit then aMandant.DataSet.Post;
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfMandantOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

