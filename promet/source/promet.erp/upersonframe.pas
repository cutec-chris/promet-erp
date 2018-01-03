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
unit uPersonFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LR_DBSet, LR_Class, Forms, Controls, ComCtrls,
  ExtCtrls, StdCtrls, DbCtrls, Buttons, db, uPrometFrames, uExtControls,
  uFilterFrame, DBGrids, Grids, Graphics, ActnList, uIntfstrconsts,
  uBaseDBClasses, Dialogs, Menus, StdActns, Utils, uDocumentFrame, variants,uBaseDatasetInterfaces;
type

  { TfPersonFrame }

  TfPersonFrame = class(TPrometMainFrame)
    acSave: TAction;
    acCancel: TAction;
    acSetTreeDir: TAction;
    acShowTreeDir: TAction;
    acClose: TAction;
    acStartTimeRegistering: TAction;
    acCopy: TAction;
    acPaste: TAction;
    acDelete: TAction;
    acNewOrder: TAction;
    acAddAsOrder: TAction;
    acExport: TAction;
    acImport: TAction;
    acRights: TAction;
    acPrint: TAction;
    acCombineItems: TAction;
    acPasteImage: TAction;
    acAddImage: TAction;
    acScreenshot: TAction;
    acDeleteThumb: TAction;
    ActionList1: TActionList;
    bAssignTree: TSpeedButton;
    bChangeNumber: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    bExecute: TSpeedButton;
    bShowTree: TSpeedButton;
    cbLanguage: TExtDBCombobox;
    cbStatus: TComboBox;
    cbSupplier: TDBCheckBox;
    Customers: TDatasource;
    dnNavigator: TDBNavigator;
    eName: TDBMemo;
    eCustomerNumber: TDBEdit;
    eMatchCode: TExtDBEdit;
    ExportDialog: TSaveDialog;
    ExtRotatedLabel1: TExtRotatedLabel;
    gbTree: TGroupBox;
    History: TDatasource;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lCustomerName: TLabel;
    lCustomerof: TLabel;
    lFirmName: TLabel;
    lHint: TLabel;
    lMatchCode: TLabel;
    MandantDetails: TDatasource;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miCopy: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miStartTimeregistering: TMenuItem;
    ImportDialog: TOpenDialog;
    pToolBar: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pComponents: TPanel;
    pcPages: TExtMenuPageControl;
    iPerson: TImage;
    pCont: TPanel;
    PHistory: TfrDBDataSet;
    PList: TfrDBDataSet;
    pmAction: TPopupMenu;
    pNav1: TPanel;
    pPreviewImage: TPanel;
    Report: TfrReport;
    sbAddImage: TSpeedButton;
    sbAddImage1: TSpeedButton;
    sbClipboardToImage: TSpeedButton;
    sbClipboardToImage1: TSpeedButton;
    sbMenue: TSpeedButton;
    sbMenue1: TSpeedButton;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tsCustomerCont: TTabSheet;
    procedure acAddAsOrderExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCombineItemsExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteThumbExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acNewOrderExecute(Sender: TObject);
    procedure acPasteImageExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acSetTreeDirExecute(Sender: TObject);
    procedure AddNewEmployee(Sender: TObject);
    procedure bChangeNumberClick(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure CustomersDataChange(Sender: TObject; Field: TField);
    procedure CustomersStateChange(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eNameExit(Sender: TObject);
    procedure FContListDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FContListgListEditButtonClick(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchValidateItem(aLink: string): Boolean;
    procedure lFirmNameClick(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
    procedure SearchEmployee(Sender: TObject);
    procedure TfListFrameFListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TfListFrameFListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TfListFrameFListViewDetails(Sender: TObject);
    procedure TPersonCustomerContDataSetAfterPost(aDataSet: TDataSet);
    procedure tsCustomerContShow(Sender: TObject);
  private
    { private declarations }
    FContList : TfFilter;
    FCustomerOf: string;
    FEmployeeOf: string;
    FEditable : Boolean;
    Reopen: Boolean;
    FDocumentFrame: TfDocumentFrame;
    FMeasurement: TMeasurement;
    procedure AddMeasurement(Sender: TObject);
    procedure AddAddress(Sender: TObject);
    procedure AddFinance(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure AddImages(Sender: TObject);
    procedure AddLinks(Sender: TObject);
    procedure AddText(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    procedure AddList(Sender: TObject);
  protected
    procedure SetDataSet(const AValue: TBaseDBDataset);override;
    procedure DoOpen;override;
    function SetRights : Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure AddAddress;
    property EmployeeOf : string read FEmployeeOf write FEmployeeOf;
    property CustomerOf : string read FCustomerOf write FCustomerOf;
  end;
implementation
uses uData, uPerson, uBaseVisualControls, uBaseDBInterface, uAddressFrame,
  uHistoryFrame, uImageFrame, uPersonFinance, uLinkFrame, uMessageEdit,
  LCLIntf,uDocuments,uListFrame,uTextFrame,uMainTreeFrame,uSearch,
  uOrderFrame,uOrder,VpData,uCalendarFrame, uImpVCard,uPrometFramesInplace,
  uNRights,uSelectReport,uBaseVisualApplication,uWiki,uWikiFrame,
  uLanguageUtils,uthumbnails,Clipbrd,uscreenshotmain,uBaseApplication,
  uscriptimport,umeasurements;
{$R *.lfm}
resourcestring
  strAddress                    = 'Adresse';
  strChangeCustomerNumer        = 'Kundennumer ändern';
  strEmployees                  = 'Mitarbeiter/Mitglieder';
  strInfo                       = 'Info';
  strInsertEventForBirthday     = 'Möchten Sie für diesen Geburtstag einen Eintrag in Ihren Kalender erzeugen ?';
  strBirthdayFrom               = 'Geburtstag von %s';
  strNewPerson                  = 'neuer Kontakt';
  strAddedFromEmployees         = 'Dies ist ein neuer Mitarbeiter/Mitglied das aus einem Kontakt erstellt wurde. Sobald Sie speichern, wird zum ursprünglichen Kontakt zurückgesprungen und ein Eintrag in dessen Mitarbeitern gemacht.';
  strAddNewPersonsFromDragDrop  = 'fügen Sie weitere Mitarbeiter/Mitglieder per Drag&Drop aus der Suche ein';

  scTelephone                   = 'TEL  Telefon';
  scBusinessPhone               = 'TELB Telefon Geschäftlich';
  scPrivatePhone                = 'TELP Telefon Privat';
  scMobilephone                 = 'CEL  Mobiltelefon';
  scBusinessMobilephone         = 'CELB Mobiltelefon Geschäftlich';
  scPrivateMobilephone          = 'CELP Mobiltelefon Privat';
  scSkype                       = 'SKP  Skype';
  scFax                         = 'FAX  Fax';
  scMail                        = 'MAIL e-Mail';
  scBusinessMail                = 'MLB  e-Mail Geschäftlich';
  scPrivateMail                 = 'MLP  e-Mail Privat';
  scInternet                    = 'INT  Homepage';
  scBirthday                    = 'BIR  Geburtstag';

procedure TfPersonFrame.FContListDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  tmp: string;
  i: Integer;
  Found: Boolean;
  aRect: TRect;
begin
  inherited;
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if Column.FieldName = 'TYPE' then
        begin
          if (copy(Column.Field.AsString,0,4) = 'MAIL')
          or (copy(Column.Field.AsString,0,3) = 'MLB')
          then
            fVisualControls.Images.Draw(TDBGrid(Sender).Canvas,Rect.Left+1,Rect.Top+1,IMAGE_MESSAGE)
          else
          if (copy(Column.Field.AsString,0,3) = 'CEL')
          or (copy(Column.Field.AsString,0,3) = 'TEL')
          or (copy(Column.Field.AsString,0,3) = 'TLB')
          then
            fVisualControls.Images.Draw(TDBGrid(Sender).Canvas,Rect.Left+1,Rect.Top+1,IMAGE_CALLS)
          ;
          aRect := Rect;
          aRect.Left:=aRect.Left+19;
          Found := False;
          if Column.Field.AsString <> '' then
          for i := 0 to Column.PickList.Count-1 do
            if trim(copy(Column.PickList[i],0,pos(' ',Column.PickList[i])-1)) = trim(Column.Field.AsString) then
              begin
                tmp := Column.PickList[i];
                tmp := copy(tmp,pos(' ',tmp)+1,length(tmp))+' ('+copy(tmp,0,pos(' ',tmp)-1)+')';
                Canvas.TextRect(aRect,aRect.Left,0,tmp);
                Found := True;
              end;
          if not Found then
            Canvas.TextRect(Rect,Rect.Left,0,Column.Field.AsString);
        end
      else
        DefaultDrawColumnCell(Rect, DataCol, Column, State);
      end;
end;
procedure TfPersonFrame.FContListgListEditButtonClick(Sender: TObject);
var
  fEditMail: TfMessageEdit;
begin
  if not Assigned(FContList.gList.SelectedField) then exit;
  if (copy(TPerson(DataSet).ContactData.FieldByName('TYPE').AsString,0,3) = 'TEL')
  or (copy(TPerson(DataSet).ContactData.FieldByName('TYPE').AsString,0,3) = 'CEL') then
    begin
//      CallPhone(TPerson(DataSet).ContactData.FieldByName('DATA').AsString);
    end
  else if (copy(TPerson(DataSet).ContactData.FieldByName('TYPE').AsString,0,3) = 'INT') then
    begin
      OpenURL(TPerson(DataSet).ContactData.FieldByName('DATA').AsString);
    end
  else if (copy(TPerson(DataSet).ContactData.FieldByName('TYPE').AsString,0,4) = 'MAIL')
       or (copy(TPerson(DataSet).ContactData.FieldByName('TYPE').AsString,0,2) = 'ML') then
       begin
         fEditMail := TfMessageEdit.Create(nil);
         fEditMail.SendMailTo('"'+TPerson(DataSet).FieldByName('NAME').AsString+'" <'+TPerson(DataSet).ContactData.FieldByName('DATA').AsString+'>');
       end;
end;

function TfPersonFrame.fSearchOpenItem(aLink: string): Boolean;
begin
  if MessageDlg(Format(strCombiteItems,[Data.GetLinkDesc(Data.BuildLink(Dataset.DataSet)),Data.GetLinkDesc(aLink)]),mtConfirmation,[mbYes,mbNo],0) = mrNo then exit;
  TPerson(DataSet).CombineItems(aLink);
end;

function TfPersonFrame.fSearchValidateItem(aLink: string): Boolean;
begin
  Result := aLink <> Data.BuildLink(DataSet.DataSet);
end;

procedure TfPersonFrame.lFirmNameClick(Sender: TObject);
var
  aPerson: TPerson;
begin
  aPerson := TPerson.CreateEx(Self,Data);
  Data.SetFilter(aPerson,Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(copy(lFirmName.Caption,pos(' (',lFirmName.Caption)+2,length(lFirmName.Caption)-(pos(' (',lFirmName.Caption))-2)));
  if aPerson.Count > 0 then
    Data.GotoLink(Data.BuildLink(aPerson.DataSet));
  aPerson.Free;
end;
procedure TfPersonFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfPersonFrame.SearchEmployee(Sender: TObject);
begin

end;

procedure TfPersonFrame.TfListFrameFListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
  procedure AddPosition(aLink : string);
  var
    aPersons: TPersonList;
    aEmployee: String;
    aName: String;
  begin
    aPersons := TPersonList.CreateEx(Self,Data);
    aPersons.SelectFromLink(aLink);
    aPersons.Open;
    aEmployee := aPersons.FieldByName('ACCOUNTNO').AsString;
    aName := aPersons.FieldByName('NAME').AsString;
    aPersons.free;
    if (aEmployee<>'') and (aName<>'') then
      begin
        if (TPerson(DataSet).Employees.Changed) then
          TPerson(DataSet).Employees.Post;
        if not (TPerson(DataSet).Employees.Canedit) then
          TPerson(DataSet).Employees.Append;
        TPerson(DataSet).Employees.FieldByName('EMPLOYEE').AsString := aEmployee;
        TPerson(DataSet).Employees.FieldByName('NAME').AsString := aName;
        TPerson(DataSet).Employees.DataSet.Post;
      end;
  end;

var
  aLinks: String;
begin
  if Source is TDragEntry then
    begin
      aLinks := TDragEntry(Source).Links;
      while pos(';',aLinks)>0 do
        begin
          AddPosition(copy(aLinks,0,pos(';',aLinks)-1));
          aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
        end;
      AddPosition(aLinks);
    end;
end;
procedure TfPersonFrame.TfListFrameFListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Source is TDragEntry then
    begin
      Accept := pos('CUSTOMERS' ,TDragEntry(Source).Links)>0;
      exit;
    end;
end;
procedure TfPersonFrame.TfListFrameFListViewDetails(Sender: TObject);
var
  aPersonList: TPersonList;
begin
  aPersonList := TPersonList.CreateEx(Self,Data);
  Data.SetFilter(aPersonList,'"ACCOUNTNO"='+Data.QuoteValue(TPerson(DataSet).Employees.FieldByName('EMPLOYEE').AsString));
  fMainTreeFrame.OpenLink(Data.BuildLink(aPersonList.DataSet),Self);
  aPersonList.Free;
end;
procedure TfPersonFrame.TPersonCustomerContDataSetAfterPost(aDataSet: TDataSet);
var
  aDate: TDateTime;
  Event: TVpEvent;
  tmp: String;
  fCalendar: TfCalendarFrame;
begin
  if aDataSet.ControlsDisabled then exit;
  if uppercase(trim(aDataSet.FieldByName('TYPE').AsString)) = 'BIR' then
    begin
      if not TryStrToDate(aDataSet.FieldByName('DATA').AsString,aDate) then exit;
      tmp := aDataSet.FieldByName('LINK').AsString;
      fCalendar := TfCalendarFrame.Create(Self);
      fCalendar.OpenDir(Data.Users.Id.AsInteger);
      Data.SetFilter(fCalendar.DataSet,Data.QuoteField('ID')+'='+Data.QuoteValue(copy(tmp,pos('@',tmp)+1,length(tmp))));
      if Data.RecordCount(fCalendar.DataSet) > 0 then
        fCalendar.DataSet.DataSet.Delete
      else if MessageDlg(strInsertEventForBirthday,mtConfirmation,[mbYes,mbNo],0) = mrNo then exit;
      aDataSet.DisableControls;
      aDataSet.Edit;
      Event := fCalendar.DataStore.Resource.Schedule.AddEvent (fCalendar.DataStore.GetNextID(''),Int(aDate), Int(aDate)+0.9999999);
      Event.AllDayEvent    :=True;                   // Ganztägig
      Event.Category       :=8;                       // ist Geburtstag
      Event.Description    :=Format(strBirthdayFrom,[DataSet.FieldByName('NAME').AsString]);
      Event.RepeatCode     :=rtYearlyByDate;
      Event.RepeatRangeEnd :=IncMonth(aDate,12*120);

      Event.AlarmSet :=True;
      Event.AlarmAdvType :=atDays;
      Event.AlarmAdv :=5;
      aDataSet.FieldByName('LINK').Asstring := 'CALENDAR@'+IntToStr(Event.RecordID);
      aDataSet.Post;
      fCalendar.DataStore.PostEvents;
      aDataSet.EnableControls;
      fCalendar.Free;
    end;
end;

procedure TfPersonFrame.tsCustomerContShow(Sender: TObject);
begin
  if Assigned(FContList) then
    FContList.ShowFrame;
end;

procedure TfPersonFrame.AddMeasurement(Sender: TObject);
begin
  TfMeasurementFrame(Sender).DataSet := FMeasurement;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfPersonFrame.bChangeNumberClick(Sender: TObject);
var
  str: String;
begin
  str := DataSet.FieldByName('ACCOUNTNO').AsString;
  if InputQuery(strChangeCustomerNumer,strnewNumber,str) and (str <> DataSet.FieldByName('ACCOUNTNO').AsString) then
    begin
      with DataSet.DataSet do
        begin
          Edit;
          FieldbyName('ACCOUNTNO').AsString:=str;
        end;
    end;
end;
procedure TfPersonFrame.cbStatusSelect(Sender: TObject);
var
  tmp: String;
begin
  tmp := copy(cbStatus.text,pos('(',cbStatus.text)+1,length(cbStatus.text));
  tmp := copy(tmp,0,pos(')',tmp)-1);
  if not FDataSet.CanEdit then FDataSet.DataSet.Edit;
  FDataSet.FieldByName('STATUS').AsString:=tmp;
  acSave.Execute;
  DoOpen;
end;
procedure TfPersonFrame.CustomersDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if Field.FieldName = 'NAME' then
    TabCaption := Field.AsString;
end;
procedure TfPersonFrame.CustomersStateChange(Sender: TObject);
begin
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfPersonFrame.eNameChange(Sender: TObject);
var
  tmp: String;
begin
  TabCaption := eName.Text;
  tmp := StringReplace(UpperCase(StringReplace(ValidateFileName(eName.Text),'_','',[rfReplaceAll])),' ','',[rfReplaceAll]);
  tmp := StringReplace(tmp,'-','',[rfReplaceAll]);
  if DataSet.CanEdit and DataSet.Changed then
    begin
      if (copy(tmp,0,length(eMatchCode.Text)) = eMatchCode.Text)
      or ((length(tmp) < length(eMatchCode.text)) and (copy(eMatchCode.Text,0,length(tmp)) = tmp)) then
        eMatchCode.Text := copy(tmp,0,eMatchCode.Field.Size);
    end;
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfPersonFrame.eNameExit(Sender: TObject);
begin
  if pcPages.CanFocus then
    pcPages.SetFocus;
end;
procedure TfPersonFrame.acSaveExecute(Sender: TObject);
begin
  Save;
  if FEmployeeOf <> '' then
    begin
      if (TTabSheet(Self.Parent).PageIndex-1 > -1)
      and (TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].ControlCount > 0)
      and (TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].Controls[0] is TfPersonFrame) then
        begin
          with TPerson(TfPersonFrame(TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].Controls[0]).DataSet).Employees.DataSet do
            begin
              Append;
              FieldByName('EMPLOYEE').AsString := DataSet.FieldByName('ACCOUNTNO').AsString;
              FieldByName('NAME').AsString := DataSet.FieldByName('NAME').AsString;
              Post;
            end;
        end;
      CloseFrame;
    end;
  if FCustomerOf <> '' then
    begin
      if (TTabSheet(Self.Parent).PageIndex-1 > -1)
      and (TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].ControlCount > 0)
      and (TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].Controls[0] is TfOrderFrame) then
        begin
          with TOrder(TfOrderFrame(TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].Controls[0]).DataSet) do
            begin
              if Address.CanEdit then
                Address.DataSet.Cancel;
              if not CanEdit then Dataset.Edit;
              Address.DataSet.Append;
              Address.Assign(TPerson(Self.DataSet));
              TfOrderFrame(TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].Controls[0]).GotoPosition;
              TfOrderFrame(TTabSheet(Self.Parent).PageControl.Pages[TTabSheet(Self.Parent).PageIndex-1].Controls[0]).RefreshAddress;
            end;
        end;
      CloseFrame;
    end;
end;

procedure TfPersonFrame.acScreenshotExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Application.ProcessMessages;
  Application.CreateForm(TfScreenshot,fScreenshot);
  with BaseApplication as IBaseApplication do
    fScreenshot.SaveTo:=AppendPathDelim(GetInternalTempDir)+'screenshot.jpg';
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  if DataSet.State=dsInsert then
    begin
      DataSet.Post;
      DataSet.Edit;
    end;
  pcPages.AddTab(TfImageFrame.Create(Self),False);
  aSheet := pcPages.GetTab(TfImageFrame);
  if Assigned(aSheet) then
    begin
      Application.ProcessMessages;
      with TfImageFrame(aSheet.Controls[0]) do
        begin
          if not DataSet.CanEdit then
            DataSet.Insert;
          with BaseApplication as IBaseApplication do
            iPreview.Picture.LoadFromFile(AppendPathDelim(GetInternalTempDir)+'screenshot.jpg');
          DataSet.Post;
        end;
      aThumbnails := TThumbnails.Create(nil);
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      while aThumbnails.Count>0 do
        aThumbnails.Delete;
      TPerson(DataSet).GenerateThumbnail;
      aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
      aThumbnails.Open;
      if aThumbnails.Count>0 then
        begin
          aStream := TMemoryStream.Create;
          Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
          aStream.Position:=0;
          iPerson.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
          aStream.Free;
          acPasteImage.Visible:=False;
          acAddImage.Visible:=False;
          acScreenshot.Visible:=False;
          acDeleteThumb.Visible:=True;
        end
      else
        begin
          iPerson.Picture.Clear;
          acPasteImage.Visible:=True;
          acAddImage.Visible:=True;
          acScreenshot.Visible:=True;
        end;
      aThumbnails.Free;
    end;

  Application.MainForm.Show;
end;

procedure TfPersonFrame.acSetTreeDirExecute(Sender: TObject);
begin
  if fMainTreeFrame.GetTreeEntry = -1 then exit;
  with DataSet.DataSet do
    begin
      Edit;
      FieldbyName('TREEENTRY').AsVariant:=fMainTreeFrame.GetTreeEntry;
      fMainTreeFrame.tvMain.Selected.Collapse(true);
    end;
end;
procedure TfPersonFrame.AddNewEmployee(Sender: TObject);
var
  aCust : string;
  Tel: String = '';
  Mail: String;
  aFrame: TfPersonFrame;
begin
  aCust := DataSet.FieldByName('ACCOUNTNO').AsString;
  if TPerson(DataSet).ContactData.DataSet.Locate('TYPE','TEL',[loCaseInsensitive, loPartialKey]) then
    Tel := TPerson(DataSet).ContactData.FieldByName('DATA').AsString;
  if rpos('-',Tel) > 0 then
    Tel := copy(Tel,0,rpos('-',Tel));
  if TPerson(DataSet).ContactData.DataSet.Locate('TYPE','MAIL',[loCaseInsensitive, loPartialKey]) then
    Mail := TPerson(DataSet).ContactData.FieldByName('DATA').AsString;
  Mail := copy(Mail,rpos('@',Mail),length(Mail));
  Application.ProcessMessages;
  aFrame := TfPersonFrame.Create(Self);
  fMainTreeFRame.pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
  if Tel <> '' then
    begin
      TPerson(aFrame.DataSet).ContactData.DataSet.Append;
      TPerson(aFrame.DataSet).ContactData.FieldByName('TYPE').AsString:='TEL';
      TPerson(aFrame.DataSet).ContactData.FieldByName('DATA').AsString:=Tel;
    end;
  if Mail <> '' then
    begin
      TPerson(aFrame.DataSet).ContactData.DataSet.Append;
      TPerson(aFrame.DataSet).ContactData.FieldByName('TYPE').AsString:='MAIL';
      TPerson(aFrame.DataSet).ContactData.FieldByName('DATA').AsString:=Mail;
    end;
  aFrame.eName.SetFocus;
  aFrame.pcPages.PageIndex:=0;
  aFrame.lHint.Caption:=strAddedFromEmployees;
  aFrame.lHint.Visible:=True;
  aFrame.EmployeeOf := aCust;
end;
procedure TfPersonFrame.acCancelExecute(Sender: TObject);
begin
  Abort;
end;
procedure TfPersonFrame.acAddAsOrderExecute(Sender: TObject);
var
  aOrderType: TOrderTyp;
  aFrame: TfOrderFrame;
  aLinkFrame: TfLinkFrame = nil;
  aLinkIndex: Integer;
  i: Integer;
begin
  Application.ProcessMessages;
  aOrderType := TOrderTyp.CreateEx(Self,Data);
  Data.SetFilter(aOrderType,Data.QuoteField('STATUSNAME')+'='+Data.QuoteValue(copy(TMenuItem(Sender).Caption,length(strNewOrder)+1,length(TMenuItem(Sender).Caption))));
  if (aOrderType.Count > 0) and Assigned(FDocumentFrame) then
    begin
      Application.ProcessMessages;
      aFrame := TfOrderFrame.Create(Self);
      fMainTreeFrame.pcPages.AddTab(aFrame);
      Application.ProcessMessages;
      aFrame.SetLanguage;
      aFrame.New(aOrderType.FieldByName('STATUS').AsString);
      TOrder(aFrame.DataSet).Address.DataSet.Insert;
      TOrder(aFrame.DataSet).Address.Assign(DataSet);
      aFrame.RefreshAddress;
      Application.ProcessMessages;
      aFrame.ShowPreview(FDocumentFrame.DataSet.Id.AsInteger);
      Application.ProcessMessages;
      if FDocumentFrame.acCopyAsLink.Execute then
        begin
          aFrame.pcHeader.AddTab(TfLinkFrame.Create(aFrame),False);
          for i := 0 to aFrame.pcHeader.PageCount-2 do
            if aFrame.pcHeader.Pages[i].ControlCount = 1 then
              if aFrame.pcHeader.Pages[i].Controls[0] is TfLinkFrame then
                aLinkFrame := TfLinkFrame(aFrame.pcHeader.Pages[i].Controls[0]);
          if Assigned(aLinkFrame) then
            aLinkFrame.acPasteLinks.Execute;
        end;
    end;
  aOrderType.Destroy;
end;

procedure TfPersonFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;

procedure TfPersonFrame.acCombineItemsExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.OnValidateItem:=@fSearchValidateItem;
  fSearch.AllowSearchTypes(strCustomers);
  fSearch.eContains.Text:=eName.Text;
  fSearch.Execute(True,'COMBPERSON','');
end;

procedure TfPersonFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      Application.ProcessMessages;
      DataSet.Delete;
      Abort;
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;

procedure TfPersonFrame.acDeleteThumbExecute(Sender: TObject);
var
  aThumbnails: TThumbnails;
begin
  aThumbnails := TThumbnails.Create(nil);
  aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
  aThumbnails.Open;
  while aThumbnails.Count>0 do
    aThumbnails.Delete;
  aThumbnails.Free;
  iPerson.Picture.Clear;
  acDeleteThumb.Visible:=False;
  acScreenshot.Visible:=True;
  acPasteImage.Visible:=True;
  acAddImage.Visible:=True;
end;

procedure TfPersonFrame.acExportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icExport,'C',FDataSet) then
    DataSet.DataSet.Refresh;
end;
procedure TfPersonFrame.acImportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icImport,'C',FDataSet) then
    DataSet.DataSet.Refresh;
end;
procedure TfPersonFrame.acNewOrderExecute(Sender: TObject);
var
  aFrame: TfOrderFrame;
begin
  aFrame := TfOrderFrame.Create(Self);
  fMainTreeFrame.pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
  TOrder(aFrame.DataSet).Address.DataSet.Insert;
  TOrder(aFrame.DataSet).Address.Assign(DataSet);
  aFrame.RefreshAddress;
  aFrame.GotoPosition;
end;

procedure TfPersonFrame.acPasteImageExecute(Sender: TObject);
var
  aSheet: TTabSheet;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
begin
  if Clipboard.HasPictureFormat then
    begin
      pcPages.AddTab(TfImageFrame.Create(Self),False);
      aSheet := pcPages.GetTab(TfImageFrame);
      if Assigned(aSheet) then
        begin
          Application.ProcessMessages;
          TfImageFrame(aSheet.Controls[0]).acPaste.Execute;
          TfImageFrame(aSheet.Controls[0]).DataSet.Post;
          aThumbnails := TThumbnails.Create(nil);
          aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
          aThumbnails.Open;
          while aThumbnails.Count>0 do
            aThumbnails.Delete;
          TPerson(DataSet).GenerateThumbnail;
          aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
          aThumbnails.Open;
          if aThumbnails.Count>0 then
            begin
              aStream := TMemoryStream.Create;
              Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
              aStream.Position:=0;
              iPerson.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
              aStream.Free;
              acPasteImage.Visible:=False;
              acAddImage.Visible:=False;
              acScreenshot.Visible:=False;
              acDeleteThumb.Visible:=True;
            end
          else
            begin
              iPerson.Picture.Clear;
              acPasteImage.Visible:=True;
              acAddImage.Visible:=True;
              acScreenshot.Visible:=True;
            end;
          aThumbnails.Free;
        end;
    end;
end;

procedure TfPersonFrame.acPrintExecute(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  fSelectReport.Report := Report;
  fSelectReport.SetLanguage;
  if Supports(FDataSet, IBaseHistory, Hist) then
    History.DataSet := Hist.GetHistory.DataSet;
  MandantDetails.DataSet:=Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  PList.DataSet := DataSet.DataSet;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      fSelectReport.ReportType := 'CUS';
    end;
  fSelectReport.Showmodal;
end;

procedure TfPersonFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
constructor TfPersonFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  Reopen := False;
  FContList := TfFilter.Create(Self);
  with FContList do
    begin
      FilterType:='CL';
      PTop.Visible := False;
      DefaultRows:='GLOBALWIDTH:%;TYPE:141;DATA:141;DESCR:142;ACTIVE:25;';
      Parent := pCont;
      Align := alClient;
      Show;
      gList.PopupMenu:=nil;
      gList.OnEditButtonClick:=@FContListgListEditButtonClick;
    end;
  FContList.ShowFrame;
  FContList.OnDrawColumnCell:=@FContListDrawColumnCell;
  cbLanguage.Items.Clear;
  Data.Languages.Open;
  eName.WantTabs:=False;
  with Data.Languages.DataSet do
    begin
      First;
      while not eof do
        begin
          cbLanguage.Items.Add(Format('%-4s%s',[FieldByName('ISO6391').AsString,FieldByName('LANGUAGE').AsString]));
          next;
        end;
    end;
  {$ifdef DARWIN}
  cbStatus.Style:=csDropdown;
  {$endif}
end;
destructor TfPersonFrame.Destroy;
begin
  FContList.Hide;
  FContList.DataSet := nil;
  FContList.Parent := nil;
  FContList.Free;
  if Assigned(DataSet) then
    begin
      DataSet.Destroy;
      DataSet := nil;
    end;
  inherited Destroy;
end;
function TfPersonFrame.OpenFromLink(aLink: string) : Boolean;
begin
  inherited;
  Result := False;
  DataSet := TPerson.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@CustomersStateChange;
  TPerson(DataSet).SelectFromLink(aLink);
  DataSet.Open;
  if FDataSet.Count > 0 then
    begin
      TabCaption := TPerson(FDataSet).Text.AsString;
      DoOpen;
      Result := True;
    end;
end;
procedure TfPersonFrame.New;
begin
  Inherited;
  DataSet := TPerson.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@CustomersStateChange;
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
  acSave.Enabled := False;
  acCancel.Enabled:= False;
  TabCaption := strNewPerson;
end;
procedure TfPersonFrame.AddAddress(Sender : TObject);
begin
  TfAddressFrame(Sender).DataSet := TPerson(FDataSet).Address;
  TfAddressFrame(Sender).Person := TPerson(FDataSet);
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='PERSON';
  TfHistoryFrame(Sender).DataSet := TPerson(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.AddImages(Sender: TObject);
begin
  TfImageFrame(Sender).DataSet := TPerson(FDataSet).Images;
  TPerson(FDataSet).Images.Open;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='PERSON';
  TfLinkFrame(Sender).DataSet := TPerson(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.AddText(Sender: TObject);
begin
  TfTextFrame(Sender).DataSource.DataSet := TPerson(FDataSet).DataSet;
  TfTextFrame(Sender).mText.DataField := 'INFO';
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
  aItem: TMenuItem;
  aOrderType: TOrderTyp;
  bItem: TMenuItem;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'C',DataSet.FieldByName('ACCOUNTNO').AsString,Null,Null,0);
    end;
  FDocumentFrame := TfDocumentFrame(Sender);
  aItem := TMenuItem.Create(TfDocumentFrame(Sender).pmDocumentAction);
  aItem.Caption:=strNewVoucher;
  TfDocumentFrame(Sender).pmDocumentAction.Items.Add(aItem);
  aOrderType := TOrderTyp.CreateEx(Self,Data);
  aOrderType.Open;
  Data.SetFilter(aOrderType,'('+Data.QuoteField('SI_ORDER')+' = ''Y'')');
  aOrderType.DataSet.First;
  while not aOrderType.DataSet.EOF do
    begin
      bItem := TMenuItem.Create(aItem);
      bItem.Caption:=strNewOrder+aOrderType.FieldByName('STATUSNAME').AsString;
      bItem.OnClick:=@acAddAsOrderExecute;
      aItem.Add(bItem);
      aOrderType.DataSet.Next;
    end;
  TfDocumentFrame(Sender).BaseElement := FDataSet;
  aOrderType.Free;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  acSave.Enabled:=False;
  acCancel.Enabled:=False;
  if not Assigned(AValue) then exit;
  Customers.DataSet := AValue.DataSet;
end;
procedure TfPersonFrame.DoOpen;
var
  aDS: TDataSet;
  s: TStream;
  GraphExt: String;
  i,a: Integer;
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  aType: Char;
  tmp: String;
  aFound: Boolean;
  aWiki: TWikiList;
  aWikiPage: TfWikiFrame;
  aWikiIdx: Integer;
  aID: String;
  aThumbnails: TThumbnails;
  aStream: TMemoryStream;
  oldIndex: Integer;
begin
  oldIndex := pcPages.TabIndex;
  FContList.pTop.Hide;
  FContList.Editable:=True;
  FContList.DataSet := TPerson(FDataSet).ContactData;
  dnNavigator.DataSource := FContList.List;
  SetRights;
  cbStatus.Items.Clear;
  cbStatus.Text := '';
  aType := 'C';
  if not Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      Data.SetFilter(Data.States,'');
      aFound := Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]);
    end
  else aFound := True;
  if aFound then
    begin
      cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
      cbStatus.Text := Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')';
    end
  else cbStatus.Text:=FDataSet.FieldByName('STATUS').AsString;
  tmp := trim(Data.States.FieldByName('DERIVATIVE').AsString);
  if (length(tmp) = 0) or (tmp[length(tmp)] <> ';') then
    tmp := tmp+';';
  if tmp <> ';' then
    begin
      while pos(';',tmp) > 0 do
        begin
          if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,copy(tmp,0,pos(';',tmp)-1)]),[loCaseInsensitive]) then
            cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
        end;
    end
  else
    begin
      Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType));
      with Data.States.DataSet do
        begin
          First;
          while not eof do
            begin
              if cbStatus.Items.IndexOf(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')') = -1 then
                cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
              Next;
            end;
        end;
    end;

  //TODO:-Bug changes Column.MaxSize to Column-1.MaxSize
  for i := 0 to FContList.gList.Columns.Count-1 do
    if FContList.gList.Columns[i].FieldName = 'DATA' then
      begin
        FContList.gList.Columns[i].ButtonStyle:=cbsEllipsis;
      end;
  with FContList.gList do
    begin
      Columns[0].Picklist.Clear;
      Columns[0].PickList.Add(scTelephone);
      Columns[0].PickList.Add(scBusinessphone);
      Columns[0].PickList.Add(scPrivatephone);
      Columns[0].PickList.Add(scMobilephone);
      Columns[0].PickList.Add(scBusinessMobilephone);
      Columns[0].PickList.Add(scPrivateMobilephone);
      Columns[0].PickList.Add(scSkype);
      Columns[0].PickList.Add(scMail);
      Columns[0].PickList.Add(scBusinessMail);
      Columns[0].PickList.Add(scPrivateMail);
      Columns[0].PickList.Add(scFax);
      Columns[0].PickList.Add(scInternet);
      Columns[0].PickList.Add(scBirthday);
    end;
  TPerson(FDataSet).ContactData.DataSet.AfterPost:=@TPersonCustomerContDataSetAfterPost;
  TranslateNavigator(dnNavigator);

  TPerson(DataSet).Address.Open;
  pcPages.NewFrame(TfAddressFrame,(FDataSet.State = dsInsert) or (TPerson(DataSet).Address.Count > 0),strAddress,@AddAddress);

  pcPages.NewFrame(TfTextFrame,not TPerson(DataSet).FieldByName('INFO').IsNull,strInfo,@AddText);

  TPerson(DataSet).History.Open;
  pcPages.NewFrame(TfHistoryFrame,TPerson(DataSet).History.Count > 0,strHistory,@AddHistory);

  aThumbnails := TThumbnails.Create(nil);
  aThumbnails.SelectByRefId(DataSet.Id.AsVariant);
  aThumbnails.Open;
  if aThumbnails.Count>0 then
    begin
      aStream := TMemoryStream.Create;
      Data.BlobFieldToStream(aThumbnails.DataSet,'THUMBNAIL',aStream);
      aStream.Position:=0;
      iPerson.Picture.LoadFromStreamWithFileExt(aStream,'jpg');
      aStream.Free;
      acPasteImage.Visible:=False;
      acAddImage.Visible:=False;
      acScreenshot.Visible:=False;
      acDeleteThumb.Visible:=True;
    end
  else
    begin
      iPerson.Picture.Clear;
      acPasteImage.Visible:=True;
      acAddImage.Visible:=True;
      acScreenshot.Visible:=True;
      acDeleteThumb.Visible:=False;
    end;
  pcPages.NewFrame(TfImageFrame,(FDataSet.State = dsInsert) or (aThumbnails.Count > 0),strImages,@AddImages);
  aThumbnails.Free;

  TPerson(DataSet).Banking.Open;
  pcPages.NewFrame(TfPersonFinance,TPerson(DataSet).Banking.Count > 0,strFinance,@AddFinance);

  TPerson(DataSet).Links.Open;
  pcPages.NewFrame(TfLinkFrame,(TPerson(DataSet).Links.Count > 0),strLinks,@AddLinks);

  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if Assigned(pcPages.GetTab(TfDocumentFrame)) then
    pcPages.GetTab(TfDocumentFrame).Free;
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) and (not Assigned(pcPages.GetTab(TfDocumentFrame))) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data,DataSet.Connection);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsInteger,'C',DataSet.FieldByName('ACCOUNTNO').AsString,Null,Null);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          aDocFrame.DataSet := aDocuments;
          pcPages.AddTab(aDocFrame,False);
        end;
    end;

  TPerson(DataSet).Employees.Open;
  pcPages.NewFrame(TfListFrame,(TPerson(DataSet).Employees.Count > 0),strEmployees,@AddList,False,strEmployees);

  pcPages.AddTabClass(TfMeasurementFrame,strMeasurement,@AddMeasurement);
  if not Reopen then
    begin
      FreeAndNil(FMeasurement);
      try
        FMeasurement := TMeasurement.CreateEx(nil,Data,DataSet.Connection,DataSet.DataSet);
        FMeasurement.CreateTable;
        FMeasurement.Open;
        if FMeasurement.Count>0 then
          pcPages.AddTab(TfMeasurementFrame.Create(Self),False);
      except
      end;
    end;

  if Data.IsSQLDb then
    begin
      aDS := Data.GetNewDataSet('select '+Data.QuoteField('CUSTOMERS')+'.'+Data.QuoteField('ACCOUNTNO')+','+Data.QuoteField('CUSTOMERS')+'.'+Data.QuoteField('NAME')+' from '+Data.QuoteField('EMPLOYEES')+' inner join '+Data.QuoteField('CUSTOMERS')+' on '+Data.QuoteField('EMPLOYEES')+'.'+Data.QuoteField('REF_ID')+'='+Data.QuoteField('CUSTOMERS')+'.'+Data.QuoteField('SQL_ID')+' where '+Data.QuoteField('EMPLOYEES')+'.'+Data.QuoteField('EMPLOYEE')+'='+Data.QuoteValue(Customers.DataSet.FieldByName('ACCOUNTNO').AsString),DataSet.Connection);
      aDS.Open;
      if aDS.RecordCount > 0 then
        begin
          lCustomerOf.Visible:=True;
          lFirmName.Visible:=True;
          lFirmName.Caption:=aDS.FieldByName('NAME').AsString+' ('+aDS.FieldByName('ACCOUNTNO').AsString+')';
        end;
      aDS.Close;
      aDS.Free;
    end;
  with Application as TBaseVisualApplication do
    AddTabClasses('PER',pcPages);
  with Application as TBaseVisualApplication do
    AddTabs(pcPages);
  acNewOrder.Visible:=Data.Users.Rights.Right('ORDERS') > RIGHT_READ;
  if (DataSet.State<> dsInsert) and (DataSet.Id.AsVariant<>Null) and (not Assigned(pcPages.GetTab(TfWikiFrame))) then
    begin
      aWiki := TWikiList.Create(nil);
      if aWiki.FindWikiFolder('Promet-ERP-Help/forms/'+Self.ClassName+'/') then
        begin
          while not aWiki.EOF do
            begin
              aWikiPage := TfWikiFrame.Create(Self);
              aID := IntToStr(Int64(DataSet.Id.AsVariant));
              aWikiPage.Variables.Values['SQL_ID'] := aID;
              aWikiPage.Variables.Values['ID'] := TBaseDbList(DataSet).Number.AsString;
              aWikiPage.Variables.Values['TEXT'] := TBaseDbList(DataSet).Text.AsString;
              aWikiIdx := -1;
              if Assigned(TBaseDbList(DataSet).Status) then
                aWikiPage.Variables.Values['STATUS'] := TBaseDbList(DataSet).Status.AsString;
              if aWikiPage.OpenWikiPage('Promet-ERP-Help/forms/'+Self.ClassName+'/'+aWiki.FieldByName('NAME').AsString) then
                begin
                  aWikiIdx := pcPages.AddTab(aWikiPage,False,aWiki.FieldByName('CAPTION').AsString);
                  aWikiPage.SetRights(FEditable);
                  aWikiPage.LeftBar:=True;
                end
              else FreeAndNil(aWikiPage);
              if Assigned(aWikiPage) then
                begin
                  if (aWiki.FieldByName('CAPTION').AsString = strOverview) or (Uppercase(aWiki.FieldByName('NAME').AsString)='OVERVIEW')  then
                    begin
                      pcPages.Pages[aWikiIdx+1].PageIndex:=0;
                      pcPages.PageIndex:=0;
                    end;
                end;
              aWiki.Next;
            end;
        end;
      aWiki.Free;
    end;
  if FDataSet.State = dsInsert then
    pcPages.PageIndex:=1
  else
    pcPages.PageIndex:=oldIndex;
  eName.SetFocus;
  pcPages.Change;
  inherited DoOpen;
end;
function TfPersonFrame.SetRights: Boolean;
begin
  FEditable := ((Data.Users.Rights.Right('CUSTOMERS') > RIGHT_READ));
  Result := FEditable;
  acDelete.Enabled:=FEditable and (Data.Users.Rights.Right('CUSTOMERS') > RIGHT_WRITE);
  acPaste.Enabled:=FEditable;
  acRights.Enabled:=Data.Users.Rights.Right('CUSTOMERS') >= RIGHT_PERMIT;

  pComponents.Enabled := FEditable;
  FContList.Editable := FEditable;
  dnNavigator.Enabled:=Feditable;
  ArrangeToolBar(pToolbar,ActionList1,'Person');
end;
procedure TfPersonFrame.AddList(Sender: TObject);
var
  aButton: TSpeedButton;
  aBitmap: TBitmap;
  aLabel: TLabel;
begin
  with TfListFrame(Sender) do
    begin
      FList.FilterType:='EMPLOYEES';
      FList.DefaultRows:='GLOBALWIDTH:%;NAME:200;DEPARTMENT:100;POSITION:100;';
      FList.pTop.Visible:=False;
      FList.DataSet := TPerson(DataSet).Employees;
      DataSource.DataSet := TPerson(DataSet).Employees.DataSet;
      FList.gList.OnDragOver:=@TfListFrameFListDragOver;
      FList.gList.OnDragDrop:=@TfListFrameFListDragDrop;
      FList.gList.DragMode:=dmAutomatic;
      FList.OnViewDetails:=@TfListFrameFListViewDetails;
      dnNavigator.VisibleButtons:=[nbDelete,nbEdit,nbPost,nbCancel,nbRefresh];
      dnNavigator.Top := dnNavigator.Top+22;
      dnNavigator.Height := dnNavigator.Height-22;
      aButton := TSpeedButton.Create(TfListFrame(Sender));
      aButton.Top:=dnNavigator.Top-22;
      aButton.Left := dnNavigator.Left;
      aButton.Height := 22;
      aButton.Width := dnNavigator.Width;
      aButton.Parent := dnNavigator.Parent;
      aBitmap := TBitmap.Create;
      fVisualControls.Images.GetBitmap(73,aBitmap);
      aButton.Glyph.Assign(aBitmap);
      aBitmap.Free;
      aButton.OnClick:=@AddNewEmployee;
      aButton.Hint:=strNewPerson;
      FList.DestroyDataSet:=False;
      aLabel := TLabel.Create(TfListFrame(Sender));
      aLabel.Parent := TfListFrame(Sender);
      aLabel.Caption:=strAddNewPersonsFromDragDrop;
      aLabel.Align:=alTop;
      aLabel.Color:=clInfoBk;
      aLabel.Font.Color:=clInfoText;
      aLabel.BorderSpacing.Around:=8;
    end;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.AddFinance(Sender: TObject);
begin
  TfPersonFinance(Sender).DataSet := FDataSet;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfPersonFrame.SetLanguage;
begin
end;
procedure TfPersonFrame.AddAddress;
begin
  pcPages.AddTab(TfAddressFrame.Create(Self));
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfPersonFrame);
end.

