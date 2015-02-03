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

unit uSelectReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, DBGrids, db, Printers, LR_Class, LR_Desgn, LR_View, Spin, LR_DBSet,
  Process, LR_Prntr, LR_ChBox, LR_Shape, LR_RRect, LR_BarC, LR_E_TXT, LR_E_HTM,
  LR_E_CSV, lr_e_pdf, LR_DB_Zeos, LRDialogControls, Variants, UTF8process,
  ExtCtrls, DbCtrls, LCLType, uExtControls, FileUtil, uBaseApplication, uOrder,
  lr_richview, lr_tachart, SynBeautifier, PrintersDlgs,uBaseERPDBClasses,uBaseDbClasses,
  Utils;

type
  TfrDesignerHackForm = TfrDesignerForm;
  TSendMessage = procedure(aReport : TfrReport;aMail : string;aSubject : string;aText : string;var isPrepared : Boolean) of object;

  { TfSelectReport }

  TfSelectReport = class(TForm)
    bClose: TBitBtn;
    bEditText: TBitBtn;
    bBook: TBitBtn;
    bShippingOutput: TBitBtn;
    bPreview: TBitBtn;
    bPrint: TBitBtn;
    bDesign: TBitBtn;
    cbPrinter: TComboBox;
    cbInfo: TComboBox;
    frBarCodeObject1: TfrBarCodeObject;
    frChartObject1: TfrChartObject;
    frCheckBoxObject1: TfrCheckBoxObject;
    frCSVExport1: TfrCSVExport;
    frDesigner1: TfrDesigner;
    frHTMExport1: TfrHTMExport;
    frRichViewObject1: TfrRichViewObject;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    frShapeObject2: TfrShapeObject;
    frTextExport1: TfrTextExport;
    frTNPDFExport1: TfrTNPDFExport;
    gReports: TExtDBGrid;
    Label1: TLabel;
    LRDialogControls1: TLRDialogControls;
    lrZeosData1: TlrZeosData;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Reports: TDatasource;
    dnReport: TDBNavigator;
    ePages: TEdit;
    lInfoOutput: TLabel;
    lCopies: TLabel;
    lPages: TLabel;
    lPrinter: TLabel;
    SaveDialog: TSaveDialog;
    eCopies: TSpinEdit;
    SpeedButton1: TSpeedButton;
    procedure ApplicationIBaseDBInterfaceReportsAfterInsert(DataSet: TDataSet);
    procedure ApplicationIBaseDBInterfaceReportsDataSetAfterScroll(
      DataSet: TDataSet);
    procedure ApplicationIBaseDBInterfaceReportsDataSetBeforeScroll(
      DataSet: TDataSet);
    procedure ApplicationIBaseDBInterfaceTfrDesignerFormShow(Sender: TObject);
    procedure bBookClick(Sender: TObject);
    procedure bEditTextClick(Sender: TObject);
    procedure bShippingOutputClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bDesignClick(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
    procedure cbPrinterSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FBooked: Boolean;
    FDS: TBaseDBDataset;
    FOnSendMessage: TSendMessage;
    FReport: TfrReport;
    FSavePossible: Boolean;
    fType: string;
    FUpdated : Boolean;
    FOldShow : TNotifyEvent;
    procedure SetDataSet(AValue: TBaseDBDataset);
    procedure SetReport(const AValue: TfrReport);
    procedure SetType(const AValue: string);
    { private declarations }
  public
    { public declarations }
    property ReportType : string read fType write SetType;
    property Report : TfrReport read FReport write SetReport;
    property DataSet : TBaseDBDataset read FDS write SetDataSet;
    procedure SetLanguage;
    procedure CheckButtons;
    procedure SetupDB;
    procedure DoSetup;
    property Booked : Boolean read FBooked;
    property SavePossible : Boolean read FSavePossible write FSavePossible;
    function LoadReport : Boolean;
    function Execute : Boolean;
    property OnSendMessage : TSendMessage read FOnSendMessage write FOnSendMessage;
    constructor Create(TheOwner: TComponent); override;
  end;

var
  fSelectReport: TfSelectReport;
  DefaultPrinterTypes : Integer;

implementation
{$R *.lfm}
uses
  uIntfStrConsts,uError,uData,
  uLogWait,uBaseDbInterface,uDocuments,uPerson,uSendMail,uEditText,umeeting,
  ubaseconfig;

resourcestring
  strDispatchTypenotfound       = 'Versandart nicht gefunden !';
  strNoOutput                   = 'keine Ausgabe';
  strFileExport                 = 'Dateiausgabe';
  strCantPrepareReport          = 'Das aufbereiten des Beleges ist fehlgeschlagen !';
  strNoMailAddress              = 'Der Kontakt hat keine e-Mail Adresse';
  strDefaultPrinter             = 'Standarddrucker';
  strDocumentDefaultPrinter     = 'Drucker aus Beleg';
  strPreparingReport            = 'Ausdruck wird vorbereitet...';
  strPrintingReport             = 'Ausdruck wird gedruckt...';
  strBooking                    = 'Vorgang wird gebucht...';
  strGeneratingMail             = 'e-Mail wird generiert...';
  strPrint                      = 'Ausgabe';
  strPrintBook                  = 'Ausgabe+Buchen';
  strPostingFailed              = 'Fehler beim Buchen !';
  strMeeting                    = 'Besprechung';
procedure TfSelectReport.bCloseClick(Sender: TObject);
begin
  Close;
end;
procedure TfSelectReport.FormShow(Sender: TObject);
begin
  Data.Reports.Open;
  SetupDB;
  bPreview.Enabled := (Data.Reports.Count > 0) and (not Data.Reports.FieldByName('REPORT').IsNull);
  if bPreview.Enabled then
    bPreview.SetFocus;
  bDesign.Visible := Data.Users.Rights.Right('REPORTS') > RIGHT_READ;
  dnReport.Visible := Data.Users.Rights.Right('REPORTS') > RIGHT_READ;
  bEditText.Visible := Data.Users.Rights.Right('REPORTS') > RIGHT_READ;
  gReports.ReadOnly:=not (Data.Users.Rights.Right('REPORTS') > RIGHT_READ);
end;
procedure TfSelectReport.bShippingOutputClick(Sender: TObject);
var
  SH : IShipableDataSet;
begin
  if Assigned(FDS) then
    if Supports(FDS, IShipableDataSet, SH) then
      SH.ShippingOutput;
end;
procedure TfSelectReport.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ApplicationIBaseDBInterfaceReportsDataSetBeforeScroll(Data.Reports.DataSet);
end;
procedure TfSelectReport.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfSelectReport.bEditTextClick(Sender: TObject);
begin
  fEditText.SetLanguage;
  fEditText.mText.Lines.Text:=Data.Reports.FieldByName('TEXT').AsString;
  fEditText.ShowModal;
  if not ((Data.Reports.DataSet.State = dsEdit) and (Data.Reports.DataSet.State = dsInsert)) then
    Data.Reports.DataSet.Edit;
  Data.Reports.FieldByName('TEXT').AsString := fEditText.mText.Lines.Text;
end;
procedure TfSelectReport.bBookClick(Sender: TObject);
var
  Stream: TMemoryStream;
  aName: String;
  NotPrintable: Boolean;
  Res: TPostResult;
  aDocument: TDocument;
  SH : IPostableDataSet;
begin
  NotPrintable := True;
  if Data.RecordCount(Data.Reports) = 0 then
    NotPrintable := True
  else
    begin
      with Data.Reports.FieldByName('REPORT') as TBlobField do
        if not Data.Reports.FieldByName('REPORT').IsNull then
          begin
            NotPrintable := False;
            try
              with BaseApplication as IBaseApplication do
                begin
                  Data.BlobFieldToFile(Data.Reports.DataSet,'REPORT',GetInternalTempDir+'preport.lrf');
                  Report.LoadFromFile(GetInternalTempDir+'preport.lrf');
                end;
            except
              NotPrintable := True;
            end;
          end;
    end;
  if Assigned(FDS) and Supports(FDS, IPostableDataSet, SH) then
    begin
      Res := SH.DoPost;
      FBooked := Res = prSuccess;
      if Res = prFailed then
        begin
          fError.ShowError(strPostingFailed+' '+SH.FailMessage);
          close;
          exit;
        end
      else if (not NotPrintable) and Report.PrepareReport and (Res = prSuccess) then
        begin
          if FDS is TOrder then
            begin
              aName := TOrder(FDS).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDS).FieldByName('NUMBER').AsString;
              Stream := TmemoryStream.Create;
              Report.EMFPages.SaveToStream(Stream);
              Stream.Position:=0;
              aDocument := TDocument.CreateEx(Self,Data);
              aDocument.Select(0);
              aDocument.Open;
              aDocument.Ref_ID:=FDS.Id.AsVariant;
              aDocument.BaseID:=FDS.FieldByName('ORDERNO').AsString;
              aDocument.BaseTyp:='O';
              aDocument.BaseLanguage:=Null;
              aDocument.Baseversion:=Null;
              aDocument.ParentID:=0;
              aDocument.AddFromStream(
                                   aName,
                                   'emfp',
                                   Stream,
                                   '',
                                   Now());
              Stream.Free;
              aDocument.Free;
            end;
          Close;
          exit;
        end
      else if NotPrintable and (Res = prSuccess) then
        Close;
    end;
end;
procedure TfSelectReport.ApplicationIBaseDBInterfaceReportsAfterInsert(
  DataSet: TDataSet);
begin
  DataSet.FieldByName('TYPE').AsString:=FType;
end;
procedure TfSelectReport.ApplicationIBaseDBInterfaceReportsDataSetAfterScroll(
  DataSet: TDataSet);
var
  tmp: TCaption;
begin
  //New Type
  tmp := cbPrinter.Text+';';
  tmp := tmp+'N'+';';
  tmp := tmp+cbInfo.Text+';';
  tmp := tmp+IntToStr(eCopies.Value)+';';
  with Application as IBaseDBInterface do
    tmp := DBConfig.ReadString('REPORTD:'+Data.Reports.Id.AsString,tmp);
  cbPrinter.ItemIndex := cbPrinter.Items.IndexOf(copy(tmp,0,pos(';',tmp)-1));
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  cbInfo.Text:=copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  eCopies.Value:=StrToIntDef(copy(tmp,0,pos(';',tmp)-1),1);
end;
procedure TfSelectReport.ApplicationIBaseDBInterfaceReportsDataSetBeforeScroll(
  DataSet: TDataSet);
var
  tmp: string;
begin
  tmp := cbPrinter.Text+';';
  tmp := tmp+'N'+';';
  tmp := tmp+cbInfo.Text+';';
  tmp := tmp+IntToStr(eCopies.Value)+';';
  try
    with Application as IBaseDBInterface do
      DBConfig.WriteString('REPORTD:'+Data.Reports.Id.AsString,tmp);
  except
  end;
end;
procedure TfSelectReport.ApplicationIBaseDBInterfaceTfrDesignerFormShow(
  Sender: TObject);
var
  i: Integer;
begin
  FOldShow(Sender);
  if not TfrDesignerForm(LR_Class.frDesigner).Pan7.Checked then
    begin
      TfrDesignerForm(LR_Class.frDesigner).Pan7.Click;
      Application.ProcessMessages;
      TfrDesignerForm(LR_Class.frDesigner).Pan5.Click;
      for i := 0 to TfrDesignerHackForm(LR_Class.frDesigner).ComponentCount-1 do
         if TfrDesignerHackForm(LR_Class.frDesigner).Components[i] is TfrObjectInspector then
           begin
             TPanel(TfrDesignerHackForm(LR_Class.frDesigner).Components[i]).Height:=LR_Class.frDesigner.Height-160;
             TPanel(TfrDesignerHackForm(LR_Class.frDesigner).Components[i]).Anchors:=[akTop, akLeft, akBottom];
           end;
    end;
end;
procedure TfSelectReport.bDeleteClick(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    Data.Reports.DataSet.Delete;
end;
procedure TfSelectReport.bDesignClick(Sender: TObject);
var
  BaseApplication : IBaseApplication;
  NewRect: TRect;
begin
  if not Supports(Application, IBaseApplication, BaseApplication) then exit;
  with Application as IBaseDbInterface do
    begin
      try
        if not Data.Reports.FieldByName('REPORT').IsNull then
          begin
            with BaseApplication as IBaseApplication do
              begin
                Data.BlobFieldToFile(Data.Reports.DataSet,'REPORT',GetInternalTempDir+'preport.lrf');
                Report.LoadFromFile(GetInternalTempDir+'preport.lrf');
              end;
          end
        else
          Report.Clear; //better so ? or should the last report stay open ?
      except
        Report.Clear; //better so ? or should the last report stay open ?
      end;
      if not Assigned(LR_Class.frDesigner)  and Assigned(LR_Class.ProcedureInitDesigner)  then
        ProcedureInitDesigner();
      if Assigned(LR_Class.frDesigner) then
        begin
          TfrDesignerForm(LR_Class.frDesigner).HelpMenu.Visible := False; //Hide default Help
          with Application as IBaseConfig do
            Config.ReadRect('ReportEditor',NewRect,TfrDesignerForm(LR_Class.frDesigner).BoundsRect);
          if (NewRect.Right-NewRect.Left < 200) or (NewRect.Right-NewRect.Left > Screen.Width) then
            begin
              NewRect.Left := 100;
              NewRect.Right := Screen.Width-200;
            end;
          if (NewRect.Bottom-NewRect.Top < 200) or (NewRect.Bottom-NewRect.Top > Screen.Height) then
            begin
              NewRect.Right := Screen.height-200;
              NewRect.Top:=100;
            end;
          TfrDesignerForm(LR_Class.frDesigner).BoundsRect := NewRect;
          if not Assigned(FOldShow) then
            begin
              FOldShow:=TfrDesignerForm(LR_Class.frDesigner).OnShow;
              TfrDesignerForm(LR_Class.frDesigner).OnShow:=@ApplicationIBaseDBInterfaceTfrDesignerFormShow;
            end;
//          TfrDesignerHackForm(LR_Class.frDesigner).EditorForm.M2.Beautifier := TSynBeautifier.Create(Application);
        end;
      Report.DesignReport;
      with Application as IBaseConfig do
        Config.WriteRect('ReportEditor',TfrDesignerForm(LR_Class.frDesigner).BoundsRect);
      with BaseApplication as IBaseApplication do
        Report.SaveToFile(GetInternalTempDir+'preport.lrf');
      Data.Reports.DataSet.Edit;
      with BaseApplication as IBaseApplication do
        Data.FileToBlobField(GetInternalTempDir+'preport.lrf',Data.Reports.DataSet,'REPORT');
      if Data.Reports.DataSet.FieldByName('NAME').IsNull then
        begin
          Data.Reports.DataSet.FieldByName('NAME').AsString:=strStandard;
          Data.Languages.Open;
          Data.Languages.DataSet.Locate('DEFAULTLNG','Y',[]);
          Data.Reports.DataSet.FieldByName('LANGUAGE').AsString:=Data.Languages.DataSet.FieldByName('ISO6391').AsString;
        end;
      Data.Reports.DataSet.Post;
    end;
end;
procedure TfSelectReport.bNewClick(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    Data.Reports.DataSet.Append;
end;
procedure TfSelectReport.bPreviewClick(Sender: TObject);
var
  pv: TfrPreviewForm;
  BaseApplication : IBaseApplication;
  NewRect: TRECT;
begin
  if not Supports(Application, IBaseApplication, BaseApplication) then exit;
  Screen.Cursor:=crHourglass;
  Printer.PrinterIndex:=-1;//Default Printer
  with Application as IBaseDbInterface do
    begin
      LoadReport;
      pv := TfrPreviewForm.Create(Self);
      pv.WindowState:=wsNormal;
      with Application as IBaseConfig do
        Config.ReadRect('ReportPreview',NewRect,pv.BoundsRect);

      if (NewRect.Right-NewRect.Left < 200) or (NewRect.Right-NewRect.Left > Screen.Width) then
        begin
          NewRect.Left := 100;
          NewRect.Right := Screen.Width-200;
        end;
      if (NewRect.Bottom-NewRect.Top < 200) or (NewRect.Bottom-NewRect.Top > Screen.Height) then
        begin
          NewRect.Right := Screen.height-200;
          NewRect.Top:=100;
        end;
      pv.BoundsRect := NewRect;
      try
        if Report.PrepareReport then
          begin
            Screen.Cursor:=crDefault;
            pv.Show_Modal(Report);
          end;
      except
        on e : Exception do
          begin
            fError.ShowError(e.Message);
            Screen.Cursor:=crDefault;
          end;
      end;
      with Application as IBaseConfig do
        Config.WriteRect('ReportPreview',pv.BoundsRect);
      pv.Free;
      bPrint.Setfocus;
      Screen.Cursor:=crDefault;
    end;
end;
procedure TfSelectReport.bPrintClick(Sender: TObject);
var
  s: String;
  i: Integer;
  NotPrintable: Boolean;
  Res: Boolean;
  aName: String;
  OldOpen: Boolean;
  isPrepared: Boolean = False;
  Stream : TMemoryStream;
  pRes: TPostResult;
  aCustomer : TPerson;
  {$IFDEF HASMESSAGING}
  fMessageEdit : TfmessageEdit;
  {$ENDIF}
  aDocument : TDocument;
  SH : IPostableDataSet;
  eMail: String;
  aUser: TUser;
  a: Integer;
  aFile: String;
begin
  Res := false;
  if not Assigned(fLogWaitForm) then
    Application.CreateForm(TfLogWaitForm,fLogWaitForm);
  fLogWaitForm.Show;
  fLogWaitform.ShowInfo(strPreparingReport);
  NotPrintable := True;
  with Application as IBaseDbInterface do
    begin
      if Data.Reports.Count = 0 then
        NotPrintable := True
      else
        begin
          NotPrintable := Loadreport = false;
        end;
      if Assigned(FDS) and Supports(FDS, IPostableDataSet, SH) then
        begin
          fLogWaitForm.ShowInfo(strBooking);
          pRes := SH.DoPost;
          FBooked := pRes = prSuccess;
          if pRes = prFailed then
            begin
              close;
              fLogWaitForm.Hide;
              exit;
            end
          else if (not NotPrintable) and Report.PrepareReport and (pRes = prSuccess) then
            begin
              aName := 'Document';
              if FDS is TOrder then
                begin
                  if (copy(fType,0,2) = 'OR') and TOrder(FDS).OrderType.DataSet.Locate('STATUS',copy(fType,3,length(fType)),[]) then
                    aName := TOrder(FDS).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDS).FieldByName('NUMBER').AsString;
                  isPrepared := True;
                  Stream := TmemoryStream.Create;
                  Report.EMFPages.SaveToStream(Stream);
                  Stream.Position:=0;
                  aDocument := TDocument.CreateEx(Self,Data);
                  aDocument.Select(0);
                  aDocument.Open;
                  aDocument.Ref_ID:=FDS.Id.AsVariant;
                  aDocument.BaseID:=FDS.FieldByName('ORDERNO').AsString;
                  aDocument.BaseTyp:='O';
                  aDocument.BaseLanguage:=Null;
                  aDocument.Baseversion:=Null;
                  aDocument.ParentID:=0;
                  aDocument.AddFromStream(
                                       aName,
                                       'emfp',
                                       Stream,
                                       '',
                                       Now());
                  Stream.Free;
                  aDocument.Free;
                end;
            end;
        end
      else Res := True;
      if (NotPrintable) then
        begin
          fLogWaitForm.Hide;
          exit;
        end;
      fLogWaitForm.ShowInfo(strPrintingReport);
      aName := 'Document';
      if (copy(fType,0,2) = 'OR') and (FDS is TOrder) then
        aName :=TOrder(FDS).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDS).FieldByName('NUMBER').AsString;
      if Report.Title='' then
        Report.Title:=aName;
      if cbPrinter.Text = '<'+strFileExport+'>' then
        begin
          s := '';
          FOR i := 0 TO frFiltersCount - 1 DO
            s := s + frFilters[i].FilterDesc + '|';
          WITH SaveDialog DO
            BEGIN
              Filter := s;
              FilterIndex := 1;
              IF Execute THEN
                begin
                  if isPrepared or Report.PrepareReport then
                    begin
                      isPrepared := True;
                      Report.ExportTo(frFilters[FilterIndex - 1].ClassRef, UniToSys(ChangeFileExt(FileName, Copy(frFilters[FilterIndex - 1].FilterExt, 2, 255))));
                      Res := True;
                    end
                  else fError.ShowWarning(strCantPrepareReport);
                end;
            END;
        end
      else if cbPrinter.Text = '<'+streMail+'>' then
        begin
          eMail := '';
          fLogWaitform.ShowInfo('e-Mail wird generiert...');
          aCustomer := TPerson.CreateEx(Self,Data);
          if (FDS is TOrder) and (TOrder(FDS).Address.Count > 0) then
            begin
              aCustomer.SelectByAccountNo(TOrder(FDS).Address.FieldByName('ACCOUNTNO').AsString);
              aCustomer.Open;
              aCustomer.ContactData.Open;
              if aCustomer.ContactData.DataSet.Locate('TYPE','MAIL',[]) then
                begin
                  eMail := aCustomer.ContactData.FieldByName('DATA').AsString;
                end;
            end;
          aCustomer.Destroy;
          if Assigned(FOnSendMessage) then
            begin
              FOnSendMessage(Report,eMail,Report.Title,Data.Reports.FieldByName('TEXT').AsString,isPrepared);
              Res := True;
            end;
        end
      else if cbPrinter.Text = '<'+strExterneMail+'>' then
        begin
          eMail:='';
          aCustomer := TPerson.CreateEx(Self,Data);
          if (FDS is TOrder) and (TOrder(FDS).Address.Count > 0) then
            begin
              aCustomer.SelectByAccountNo(TOrder(FDS).Address.FieldByName('ACCOUNTNO').AsString);
              aCustomer.Open;
              aCustomer.ContactData.Open;
              if aCustomer.ContactData.DataSet.Locate('TYPE','MAIL',[]) then
                begin
                  eMail := aCustomer.ContactData.FieldByName('DATA').AsString;
                end;
            end;
          aCustomer.Destroy;
          aName := 'Document';
          if (copy(fType,0,2) = 'OR') and (FDS is TOrder) then
            aName := TOrder(FDS).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDS).FieldByName('NUMBER').AsString;
          if FDS is TMeetings then
            begin
              Report.Title := strMeeting+' '+TMeetings(FDS).Text.AsString;
              with TMeetings(FDS).Users do
                begin
                  First;
                  aUser := TUser.Create(nil);
                  while not EOF do
                    begin
                      aUser.Select(FieldbyName('USER_ID').AsVariant);
                      aUser.Open;
                      if (aUser.Count=1) and (aUser.Id.AsVariant<>Data.Users.Id.AsVariant) then
                        if aUser.FieldByName('EMAIL').AsString<>'' then
                          eMail:=eMail+','+aUser.FieldByName('EMAIL').AsString;
                      next;
                    end;
                  eMail:=copy(eMail,2,length(eMail));
                  aUser.Free;
                end;
            end;
          if Report.Title='' then
            Report.Title:='PrometERP-'+aName;
          FOR i := 0 TO frFiltersCount - 1 DO
            if pos('PDF',Uppercase(frFilters[i].FilterDesc)) > 0 then
              if isPrepared or Report.PrepareReport then
                begin
                  isPrepared := True;
                  with BaseApplication as IBaseApplication do
                    aFile := GetInternalTempDir+ValidateFileName(Report.Title)+'.pdf';
                  Report.ExportTo(frFilters[i].ClassRef,aFile);
                  DoSendMail(Report.Title,Data.Reports.FieldByName('TEXT').AsString, aFile,'','','',eMail);
                  res := True;
                end
              else fError.ShowWarning(strCantPrepareReport);
        end
      else if cbPrinter.Text = '<'+strDefaultPrinter+'>' then
        begin
          aName := 'Document';
          if (copy(fType,0,2) = 'OR') and (FDS is TOrder) then
            aName := TOrder(FDS).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDS).FieldByName('NUMBER').AsString;
          Report.Title:='PrometERP-'+aName;
          Prn.PrinterIndex := -1;
          if (not isPrepared) and (not Report.PrepareReport) then
            begin
              fLogWaitForm.Hide;
              fError.ShowWarning(strCantPrepareReport);
              exit;
            end
          else isPrepared := True;
          Report.PrintPreparedReport(ePages.Text,eCopies.Value);
          Res := True;
        end
      else if cbPrinter.Text = '<'+strDocumentDefaultPrinter+'>' then
        begin
          aName := 'Document';
          if (copy(fType,0,2) = 'OR') and (FDS is TOrder) then
            aName := TOrder(FDS).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDS).FieldByName('NUMBER').AsString;
          Report.Title:='PrometERP-'+aName;
          if (not isPrepared) and (not Report.PrepareReport) then
            begin
              isPrepared := True;
              fLogWaitForm.Hide;
              fError.ShowWarning(strCantPrepareReport);
              exit;
            end
          else isPrepared := True;
          Report.PrintPreparedReport(ePages.Text,eCopies.Value);
          Res := True;
        end
      else if cbPrinter.Text = '<'+strNoOutput+'>' then
        Res := True
      else
        begin
          Report.Title:='PrometERP-'+aName;
           if Printer.Printers.Count = 0 then
             begin
               fLogWaitForm.Hide;
               exit;
             end;
           if cbPrinter.ItemIndex > -1 then
             begin
               Prn.PrinterIndex := cbPrinter.ItemIndex-DefaultPrinterTypes;
             end;
          if (not isPrepared) and (not Report.PrepareReport) then
            begin
              fLogWaitForm.Hide;
              fError.ShowWarning(strCantPrepareReport);
              exit;
            end
          else isPrepared := True;
          Report.PrintPreparedReport(ePages.Text,eCopies.Value);
          Res := True;
        end;
      if Res then
        begin
          if cbInfo.Text = '<'+streMail+'>' then
            begin
              fLogWaitform.ShowInfo('e-Mail wird generiert...');
              aCustomer := TPerson.CreateEx(Self,Data);
              if (FDS is TOrder) and (TOrder(FDS).Address.Count > 0) then
                begin
                  aCustomer.SelectByAccountNo(TOrder(FDS).Address.FieldByName('ACCOUNTNO').AsString);
                  aCustomer.Open;
                  aCustomer.ContactData.Open;
                  if aCustomer.ContactData.DataSet.Locate('TYPE;ACTIVE',VarArrayOf(['MAIL','Y']),[])
                  or aCustomer.ContactData.DataSet.Locate('TYPE',VarArrayOf(['MAIL']),[])
                  then
                    begin
                      if Assigned(FOnSendMessage) then
                        begin
                          FOnSendMessage(Report,aCustomer.ContactData.FieldByName('DATA').AsString,aName,Data.Reports.FieldByName('TEXT').AsString,isPrepared);
                          Res := True;
                        end;
                    end
                  else
                    ShowMessage(strNoMailAddress);
                end;
              aCustomer.Destroy;
            end;
          Modalresult := mrOK;
          Close;
        end;
      fLogWaitForm.Hide;
    end;
end;
procedure TfSelectReport.cbPrinterSelect(Sender: TObject);
begin
  eCopies.Enabled := not ((cbPrinter.Text = '<'+strFileExport+'>') or (cbPrinter.Text = '<'+strNoOutput+'>'));
  ePages.Enabled := not ((cbPrinter.Text = '<'+strFileExport+'>') or (cbPrinter.Text = '<'+strNoOutput+'>'));
  if cbPrinter.ItemIndex>DefaultPrinterTypes then
    Prn.PrinterIndex := cbPrinter.ItemIndex-DefaultPrinterTypes;
end;
procedure TfSelectReport.FormCreate(Sender: TObject);
begin
  FType := 'NOTH';
  Data.MandantDetails.CreateTable;
  Data.Reports.CreateTable;
end;
procedure TfSelectReport.SpeedButton1Click(Sender: TObject);
begin
  if PrinterSetupDialog1.Execute then
    begin
      cbPrinter.ItemIndex := Prn.PrinterIndex+DefaultPrinterTypes;
    end;
end;
procedure TfSelectReport.SetType(const AValue: string);
var
  tmp: String;
  OrderType: LongInt;
begin
  if not Assigned(fSelectReport) then
    begin
      Application.CreateForm(TfSelectReport,fSelectReport);
      Self := fSelectReport;
    end;
  DataSet:=nil;
//  if fType=AValue then exit;
  with Application as IBaseDBInterface do
    begin
      with Data.Reports.DataSet as IBaseDbFilter do
        Filter := Data.QuoteField('TYPE')+'='+Data.QuoteValue(AValue);
      if Data.Users.Options.DataSet.ControlsDisabled then exit;
      if not Data.Reports.DataSet.Active then
        Data.Reports.DataSet.Open;
      ApplicationIBaseDBInterfaceReportsDataSetAfterScroll(Data.Reports.DataSet);
      fType:=AValue;
    end;
end;
procedure TfSelectReport.SetReport(const AValue: TfrReport);
begin
  if not Assigned(fSelectReport) then
    begin
      Application.CreateForm(TfSelectReport,fSelectReport);
      Self := fSelectReport;
    end;
  if FReport=AValue then exit;
  FReport:=AValue;
end;

procedure TfSelectReport.SetDataSet(AValue: TBaseDBDataset);
begin
  if FDS=AValue then Exit;
  FDS:=AValue;
  CheckButtons;
end;

procedure TfSelectReport.SetLanguage;
var
  i: Integer;
begin
  if not Assigned(fSelectReport) then
    begin
      Application.CreateForm(TfSelectReport,fSelectReport);
      Self := fSelectReport;
    end;
  cbPrinter.Items.Clear;
  cbPrinter.Items.Add('<'+strDocumentDefaultPrinter+'>');
  if Printer.Printers.Count > 0 then
    cbPrinter.Items.Add('<'+strDefaultPrinter+'>');
  cbPrinter.Items.Add('<'+strNoOutput+'>');
  if Assigned(FOnSendMessage) then
    cbPrinter.Items.Add('<'+streMail+'>');
  cbPrinter.Items.Add('<'+strExterneMail+'>');
  cbPrinter.Items.Add('<'+strFileExport+'>');
  DefaultPrinterTypes := cbPrinter.Items.Count;
  cbPrinter.Items.AddStrings(Printer.Printers);
  if Printer.Printers.Count > 0 then
    cbPrinter.Text := '<'+strDocumentDefaultPrinter+'>'
  else
    cbPrinter.Text := '<'+strFileExport+'>';
  cbInfo.Clear;
  cbInfo.Items.Add('<'+strNoOutput+'>');
  if Assigned(FOnSendMessage) then
    cbInfo.Items.Add('<'+streMail+'>');
  CheckButtons;
end;

procedure TfSelectReport.CheckButtons;
begin
  if Assigned(FDS) and Supports(FDS, IPostableDataSet) then
    bPrint.Caption:=strPrintBook;
  bBook.Visible:=Assigned(FDS) and Supports(FDS, IPostableDataSet);
  bShippingOutput.Visible:=Assigned(FDS) and Supports(FDS, IShipableDataSet);
end;

procedure TfSelectReport.SetupDB;
begin
  with Application as IBaseDbInterface do
    begin
      Reports.DataSet := Data.Reports.DataSet;
      Reports.DataSet.AfterInsert:=@ApplicationIBaseDBInterfaceReportsAfterInsert;
      Reports.DataSet.BeforeScroll:=@ApplicationIBaseDBInterfaceReportsDataSetBeforeScroll;
      Reports.DataSet.AfterScroll:=@ApplicationIBaseDBInterfaceReportsDataSetAfterScroll;
    end;
end;

procedure TfSelectReport.DoSetup;
begin
  if not Assigned(fSelectReport) then
    begin
      Application.CreateForm(TfSelectReport,fSelectReport);
      Self := fSelectReport;
    end;
  gReports.Columns[1].PickList.Clear;
  {//TODO:
  with Data.Languages.DataSet do
    begin
      First;
      while not eof do
        begin
          gReports.Columns[1].PickList.Add(Format('%-4s%s',[FieldByName('ISO6391').AsString,FieldByName('LANGUAGE').AsString]));
          next;
        end;
    end;
    }
end;

function TfSelectReport.LoadReport : Boolean;
begin
  Result := False;
  if not Data.Reports.DataSet.Active then
    Data.Reports.Open;
  with Application as IBaseDbInterface do
    begin
      try
        with Data.Reports.FieldByName('REPORT') as TBlobField do
          if not Data.Reports.FieldByName('REPORT').IsNull then
            begin
              with BaseApplication as IBaseApplication do
                begin
                  Data.BlobFieldToFile(Data.Reports.DataSet,'REPORT',GetInternalTempDir+'preport.lrf');
                  Report.LoadFromFile(GetInternalTempDir+'preport.lrf');
                end;
            end
          else
            begin
              Screen.Cursor:=crDefault;
              exit;
            end;
        Result := True;
      except
        Screen.Cursor:=crDefault;
        exit;
      end;
    end;
end;

function TfSelectReport.Execute: Boolean;
var
  ActControl: TWinControl;
begin
  if not Assigned(fSelectReport) then
    begin
      Application.CreateForm(TfSelectReport,fSelectReport);
      Self := fSelectReport;
    end;
  FUpdated := False;
  FBooked := False;
  ActControl := Screen.ActiveControl;
  if bBook.Visible and Assigned(Report) and (not SavePossible) then
    Report.PreviewButtons:=[pbZoom, pbFind, pbExit]
  else if Assigned(Report) then
    Report.PreviewButtons:=[pbZoom, pbSave, pbPrint, pbFind, pbExit];
  Show;
  Application.ProcessMessages; // Preview ist meisst Modal unter diversen Umständen gibts Probleme mit 2 Modalen Forms übereinander
  while Visible do
    begin
      Application.ProcessMessages;
      sleep(100);
    end;
  Result := ModalResult = mrOK;
  FDS := nil;
  if not Result then
    begin
      try
        if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
      except
      end;
    end;
end;

constructor TfSelectReport.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOldShow:=nil;
  FSavePossible:=False;
end;

initialization

end.

