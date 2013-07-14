unit UMakeDoc;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PReport, ComCtrls, ExtCtrls, Menus, PRAnnotation, PdfDoc, PdfTypes,
  LResources;

type
  TContentsElement = class;

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    CreatePDF1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    PReport1: TPReport;
    PRText93: TPRText;
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    TabSheet14: TTabSheet;
    TabSheet15: TTabSheet;
    TabSheet16: TTabSheet;
    TabSheet17: TTabSheet;
    CoverPage: TPRPage;
    PRLayoutPanel1: TPRLayoutPanel;
    PRText2: TPRText;
    PRLayoutPanel2: TPRLayoutPanel;
    PRText1: TPRText;
    ContentsPage: TPRPage;
    PRLayoutPanel4: TPRLayoutPanel;
    PRRect6: TPRRect;
    PRRect7: TPRRect;
    PRRect8: TPRRect;
    PRRect10: TPRRect;
    PRText1Contents: TPRText;
    PRPage3: TPRPage;
    PRLayoutPanel3: TPRLayoutPanel;
    PRRect1: TPRRect;
    PRRect2: TPRRect;
    PRRect3: TPRRect;
    PRText6: TPRText;
    PRRect4: TPRRect;
    PRTextIntro: TPRText;
    PRRect5: TPRRect;
    PRText1_1: TPRText;
    PRText9: TPRText;
    PRText1_2: TPRText;
    PRText259: TPRText;
    PRText1_3: TPRText;
    PRText261: TPRText;
    PRText262: TPRText;
    PRText263: TPRText;
    PRPage4: TPRPage;
    PRLayoutPanel5: TPRLayoutPanel;
    PRRect9: TPRRect;
    PRRect11: TPRRect;
    PRRect12: TPRRect;
    PRRect13: TPRRect;
    PRText21: TPRText;
    PRRect14: TPRRect;
    PRTextCompRef: TPRText;
    PRText2_1: TPRText;
    PRText24: TPRText;
    PRText2_1_1: TPRText;
    PRText26: TPRText;
    PRText27: TPRText;
    PRText28: TPRText;
    PRText29: TPRText;
    PRText30: TPRText;
    PRText31: TPRText;
    PRText32: TPRText;
    PRText36: TPRText;
    PRText37: TPRText;
    PRText38: TPRText;
    PRText39: TPRText;
    PRText40: TPRText;
    PRText41: TPRText;
    PRText42: TPRText;
    PRText43: TPRText;
    PRText44: TPRText;
    PRText45: TPRText;
    PRText46: TPRText;
    PRText47: TPRText;
    PRText48: TPRText;
    PRText49: TPRText;
    PRText50: TPRText;
    PRText33: TPRText;
    PRText34: TPRText;
    PRText35: TPRText;
    PRText83: TPRText;
    PRText84: TPRText;
    PRText85: TPRText;
    PRPage5: TPRPage;
    PRLayoutPanel6: TPRLayoutPanel;
    PRRect15: TPRRect;
    PRRect16: TPRRect;
    PRRect17: TPRRect;
    PRRect18: TPRRect;
    PRText62: TPRText;
    PRText63: TPRText;
    PRText64: TPRText;
    PRText53: TPRText;
    PRText54: TPRText;
    PRText55: TPRText;
    PRText56: TPRText;
    PRText58: TPRText;
    PRText59: TPRText;
    PRText60: TPRText;
    PRText61: TPRText;
    PRText65: TPRText;
    PRText66: TPRText;
    PRText67: TPRText;
    PRText86: TPRText;
    PRText87: TPRText;
    PRText88: TPRText;
    PRText77: TPRText;
    PRText78: TPRText;
    PRText79: TPRText;
    PRPage7: TPRPage;
    PRLayoutPanel7: TPRLayoutPanel;
    PRRect19: TPRRect;
    PRRect20: TPRRect;
    PRRect21: TPRRect;
    PRRect22: TPRRect;
    PRText2_2_1: TPRText;
    PRText102: TPRText;
    PRText103: TPRText;
    PRText104: TPRText;
    PRText105: TPRText;
    PRText106: TPRText;
    PRText107: TPRText;
    PRText108: TPRText;
    PRText109: TPRText;
    PRText110: TPRText;
    PRText111: TPRText;
    PRText112: TPRText;
    PRText113: TPRText;
    PRText114: TPRText;
    PRText115: TPRText;
    PRText2_2_2: TPRText;
    PRText96: TPRText;
    PRText97: TPRText;
    PRText119: TPRText;
    PRText120: TPRText;
    PRText121: TPRText;
    PRText122: TPRText;
    PRText123: TPRText;
    PRText124: TPRText;
    PRText125: TPRText;
    PRText2_2: TPRText;
    PRText90: TPRText;
    PRText91: TPRText;
    PRText92: TPRText;
    PRPage8: TPRPage;
    PRLayoutPanel8: TPRLayoutPanel;
    PRRect23: TPRRect;
    PRRect24: TPRRect;
    PRRect25: TPRRect;
    PRRect26: TPRRect;
    PRText2_3: TPRText;
    PRText152: TPRText;
    PRText153: TPRText;
    PRText154: TPRText;
    PRText155: TPRText;
    PRText156: TPRText;
    PRText2_3_1: TPRText;
    PRText164: TPRText;
    PRText165: TPRText;
    PRText166: TPRText;
    PRText2_3_2: TPRText;
    PRText100: TPRText;
    PRText101: TPRText;
    PRText116: TPRText;
    PRText117: TPRText;
    PRText118: TPRText;
    PRText126: TPRText;
    PRText127: TPRText;
    PRPage9: TPRPage;
    PRLayoutPanel9: TPRLayoutPanel;
    PRRect27: TPRRect;
    PRRect28: TPRRect;
    PRRect29: TPRRect;
    PRRect30: TPRRect;
    PRText2_4: TPRText;
    PRText133: TPRText;
    PRText134: TPRText;
    PRText135: TPRText;
    PRText136: TPRText;
    PRText137: TPRText;
    PRText2_4_1: TPRText;
    PRText139: TPRText;
    PRText140: TPRText;
    PRText141: TPRText;
    PRText2_4_2: TPRText;
    PRText143: TPRText;
    PRText144: TPRText;
    PRText145: TPRText;
    PRText146: TPRText;
    PRText147: TPRText;
    PRText148: TPRText;
    PRText149: TPRText;
    PRText150: TPRText;
    PRText157: TPRText;
    PRText158: TPRText;
    PRText159: TPRText;
    PRText160: TPRText;
    PRText161: TPRText;
    PRText162: TPRText;
    PRText167: TPRText;
    PRText168: TPRText;
    PRText169: TPRText;
    PRText170: TPRText;
    PRText171: TPRText;
    PRText172: TPRText;
    PRText173: TPRText;
    PRText174: TPRText;
    PRText175: TPRText;
    PRPage10: TPRPage;
    PRLayoutPanel17: TPRLayoutPanel;
    PRRect67: TPRRect;
    PRRect68: TPRRect;
    PRRect69: TPRRect;
    PRRect70: TPRRect;
    PRText2_5: TPRText;
    PRText281: TPRText;
    PRText287: TPRText;
    PRText319: TPRText;
    PRText320: TPRText;
    PRText321: TPRText;
    PRText2_5_1: TPRText;
    PRText323: TPRText;
    PRText324: TPRText;
    PRText325: TPRText;
    PRText326: TPRText;
    PRText327: TPRText;
    PRText328: TPRText;
    PRText329: TPRText;
    PRText330: TPRText;
    PRText331: TPRText;
    PRText348: TPRText;
    PRText349: TPRText;
    PRText350: TPRText;
    PRText354: TPRText;
    PRText355: TPRText;
    PRText356: TPRText;
    PRPage11: TPRPage;
    PRLayoutPanel14: TPRLayoutPanel;
    PRRect54: TPRRect;
    PRRect55: TPRRect;
    PRRect56: TPRRect;
    PRRect58: TPRRect;
    PRText380: TPRText;
    PRText381: TPRText;
    PRText382: TPRText;
    PRText383: TPRText;
    PRText384: TPRText;
    PRText385: TPRText;
    PRText386: TPRText;
    PRText387: TPRText;
    PRText388: TPRText;
    PRText389: TPRText;
    PRRect76: TPRRect;
    PRText390: TPRText;
    PRText391: TPRText;
    PRText392: TPRText;
    PRText393: TPRText;
    PRText394: TPRText;
    PRText395: TPRText;
    PRPage12: TPRPage;
    PRLayoutPanel10: TPRLayoutPanel;
    PRRect31: TPRRect;
    PRRect32: TPRRect;
    PRRect33: TPRRect;
    PRRect34: TPRRect;
    PRText2_6: TPRText;
    PRText179: TPRText;
    PRText180: TPRText;
    PRText181: TPRText;
    PRText182: TPRText;
    PRText183: TPRText;
    PRText2_6_1: TPRText;
    PRText185: TPRText;
    PRText186: TPRText;
    PRText187: TPRText;
    PRText196: TPRText;
    PRText197: TPRText;
    PRText198: TPRText;
    PRText199: TPRText;
    PRText200: TPRText;
    PRText201: TPRText;
    PRText202: TPRText;
    PRText203: TPRText;
    PRText239: TPRText;
    PRText295: TPRText;
    PRText296: TPRText;
    PRText297: TPRText;
    PRText298: TPRText;
    PRText299: TPRText;
    PRText300: TPRText;
    PRText301: TPRText;
    PRText302: TPRText;
    PRText303: TPRText;
    PRText304: TPRText;
    PRText305: TPRText;
    PRText306: TPRText;
    PRText307: TPRText;
    PRText308: TPRText;
    PRText309: TPRText;
    PRText310: TPRText;
    PRText311: TPRText;
    PRText312: TPRText;
    PRPage13: TPRPage;
    PRLayoutPanel11: TPRLayoutPanel;
    PRRect35: TPRRect;
    PRRect36: TPRRect;
    PRRect37: TPRRect;
    PRRect38: TPRRect;
    PRText2_7: TPRText;
    PRText191: TPRText;
    PRText192: TPRText;
    PRText193: TPRText;
    PRText194: TPRText;
    PRText195: TPRText;
    PRText2_7_1: TPRText;
    PRText206: TPRText;
    PRText207: TPRText;
    PRText208: TPRText;
    PRText209: TPRText;
    PRText210: TPRText;
    PRText211: TPRText;
    PRText212: TPRText;
    PRText213: TPRText;
    PRText214: TPRText;
    PRText215: TPRText;
    PRText216: TPRText;
    PRText217: TPRText;
    PRText3: TPRText;
    PRText204: TPRText;
    PRText240: TPRText;
    PRPage14: TPRPage;
    PRLayoutPanel12: TPRLayoutPanel;
    PRRect39: TPRRect;
    PRRect40: TPRRect;
    PRRect41: TPRRect;
    PRRect42: TPRRect;
    PRText2_8: TPRText;
    PRText246: TPRText;
    PRText247: TPRText;
    PRText248: TPRText;
    PRText249: TPRText;
    PRText250: TPRText;
    PRText2_8_1: TPRText;
    PRText252: TPRText;
    PRText253: TPRText;
    PRText254: TPRText;
    PRText255: TPRText;
    PRText256: TPRText;
    PRText257: TPRText;
    PRText258: TPRText;
    PRText260: TPRText;
    PRText264: TPRText;
    PRText265: TPRText;
    PRText273: TPRText;
    PRText274: TPRText;
    PRPage15: TPRPage;
    PRLayoutPanel13: TPRLayoutPanel;
    PRRect43: TPRRect;
    PRRect44: TPRRect;
    PRRect45: TPRRect;
    PRRect46: TPRRect;
    PRText2_9: TPRText;
    PRText221: TPRText;
    PRText222: TPRText;
    PRText223: TPRText;
    PRText2_9_1: TPRText;
    PRText227: TPRText;
    PRText228: TPRText;
    PRText229: TPRText;
    PRText230: TPRText;
    PRText231: TPRText;
    PRText232: TPRText;
    PRText233: TPRText;
    PRText234: TPRText;
    PRText235: TPRText;
    PRText236: TPRText;
    PRText237: TPRText;
    PRText238: TPRText;
    PRRect47: TPRRect;
    PRText224: TPRText;
    PRText225: TPRText;
    PRRect48: TPRRect;
    PRText266: TPRText;
    PRRect49: TPRRect;
    PRRect50: TPRRect;
    PRText267: TPRText;
    PRText268: TPRText;
    PRRect51: TPRRect;
    PRRect52: TPRRect;
    PRText269: TPRText;
    PRText270: TPRText;
    PRRect57: TPRRect;
    PRPage16: TPRPage;
    PRLayoutPanel18: TPRLayoutPanel;
    PRRect72: TPRRect;
    PRRect73: TPRRect;
    PRRect74: TPRRect;
    PRRect75: TPRRect;
    PRText2_10: TPRText;
    PRText357: TPRText;
    PRText358: TPRText;
    PRText359: TPRText;
    PRText2_10_1: TPRText;
    PRText361: TPRText;
    PRText362: TPRText;
    PRText363: TPRText;
    PRText364: TPRText;
    PRText365: TPRText;
    PRText366: TPRText;
    PRText367: TPRText;
    PRText368: TPRText;
    PRText369: TPRText;
    PRText370: TPRText;
    PRText371: TPRText;
    PRText372: TPRText;
    PRLabel2: TPRLabel;
    PRRect53: TPRRect;
    PREllipse1: TPREllipse;
    PREllipse2: TPREllipse;
    PREllipse3: TPREllipse;
    PREllipse4: TPREllipse;
    PREllipse5: TPREllipse;
    PREllipse6: TPREllipse;
    PRPage17: TPRPage;
    PRLayoutPanel16: TPRLayoutPanel;
    PRRect63: TPRRect;
    PRRect64: TPRRect;
    PRRect65: TPRRect;
    PRRect66: TPRRect;
    PRText2_11: TPRText;
    PRText282: TPRText;
    PRText283: TPRText;
    PRText284: TPRText;
    PRText2_11_1: TPRText;
    PRText291: TPRText;
    PRText292: TPRText;
    PRText293: TPRText;
    PRText294: TPRText;
    PRText313: TPRText;
    PRText314: TPRText;
    PRText315: TPRText;
    PRText316: TPRText;
    PRAnnotation1: TPRAnnotation;
    PRAnnotation2: TPRAnnotation;
    PRText288: TPRText;
    PRText289: TPRText;
    PRText290: TPRText;
    PRText317: TPRText;
    PRText318: TPRText;
    PRPage20: TPRPage;
    PRLayoutPanel15: TPRLayoutPanel;
    PRRect59: TPRRect;
    PRRect60: TPRRect;
    PRRect61: TPRRect;
    PRRect62: TPRRect;
    PRText220: TPRText;
    PRTextCopyright: TPRText;
    PRLabel3: TPRLabel;
    PRLabel4: TPRLabel;
    PRLabel6: TPRLabel;
    PRLabel7: TPRLabel;
    PRLabel9: TPRLabel;
    PRLabel5: TPRLabel;
    PRLabel8: TPRLabel;
    PRLabel10: TPRLabel;
    PRRect71: TPRRect;
    PRLabel11: TPRLabel;
    PRLabel1: TPRLabel;
    PRLabel22: TPRLabel;
    PRLabel23: TPRLabel;
    PRLabel24: TPRLabel;
    PRLabel25: TPRLabel;
    PRLabel26: TPRLabel;
    PRLabel27: TPRLabel;
    PRLabel28: TPRLabel;
    PRLabel29: TPRLabel;
    PRLabel30: TPRLabel;
    PRLabel31: TPRLabel;
    PRLabel32: TPRLabel;
    PRLabel33: TPRLabel;
    PRLabel34: TPRLabel;
    PRLabel35: TPRLabel;
    PRLabel36: TPRLabel;
    PRLabel21: TPRLabel;
    PRGridPanel1: TPRGridPanel;
    lblSectionNo: TPRLabel;
    lblSectionName: TPRLabel;
    PRText5: TPRText;
    PRText7: TPRText;
    PRText8: TPRText;
    PRText10: TPRText;
    PRText11: TPRText;
    TabSheet18: TTabSheet;
    PRPage6: TPRPage;
    PRLayoutPanel19: TPRLayoutPanel;
    PRRect77: TPRRect;
    PRRect78: TPRRect;
    PRRect79: TPRRect;
    PRRect80: TPRRect;
    PRText94: TPRText;
    PRText95: TPRText;
    PRText98: TPRText;
    PRText99: TPRText;
    PRText128: TPRText;
    PRText129: TPRText;
    PRText130: TPRText;
    PRText131: TPRText;
    PRText132: TPRText;
    PRText138: TPRText;
    PRText176: TPRText;
    PRText177: TPRText;
    PRText178: TPRText;
    PRLabel12: TPRLabel;
    PRLabel13: TPRLabel;
    PRLabel14: TPRLabel;
    PRLabel15: TPRLabel;
    PRText4: TPRText;
    PRText13: TPRText;
    PRLabel16: TPRLabel;
    PRLabel17: TPRLabel;
    TabSheet19: TTabSheet;
    PRPage18: TPRPage;
    PRLayoutPanel20: TPRLayoutPanel;
    PRRect81: TPRRect;
    PRRect82: TPRRect;
    PRRect83: TPRRect;
    PRRect84: TPRRect;
    PRLabel18: TPRLabel;
    PRText2_12: TPRText;
    PRText14: TPRText;
    PRText15: TPRText;
    PRText16: TPRText;
    PRText2_12_2: TPRText;
    TabSheet20: TTabSheet;
    PRPage19: TPRPage;
    PRLayoutPanel21: TPRLayoutPanel;
    PRRect85: TPRRect;
    PRRect86: TPRRect;
    PRRect87: TPRRect;
    PRRect88: TPRRect;
    PRLabel19: TPRLabel;
    PRText70: TPRText;
    PRText71: TPRText;
    PRText72: TPRText;
    PRText73: TPRText;
    PRText74: TPRText;
    PRText12: TPRText;
    PRText17: TPRText;
    PRLabel20: TPRLabel;
    PRLabel37: TPRLabel;
    PRText188: TPRText;
    PRText189: TPRText;
    PRLabel38: TPRLabel;
    PRLabel39: TPRLabel;
    PRText190: TPRText;
    PRText219: TPRText;
    PRLabel40: TPRLabel;
    PRLabel41: TPRLabel;
    PRLabel42: TPRLabel;
    PRLabel43: TPRLabel;
    PRText18: TPRText;
    PRLabel44: TPRLabel;
    PRLabel45: TPRLabel;
    PRText19: TPRText;
    PRLabel46: TPRLabel;
    PRLabel47: TPRLabel;
    PRText20: TPRText;
    PRLabel48: TPRLabel;
    PRLabel49: TPRLabel;
    PRText22: TPRText;
    PRLabel50: TPRLabel;
    PRLabel51: TPRLabel;
    PRText23: TPRText;
    PRLabel52: TPRLabel;
    PRLabel53: TPRLabel;
    PRText25: TPRText;
    PRLabel54: TPRLabel;
    PRLabel55: TPRLabel;
    PRText51: TPRText;
    PRLabel56: TPRLabel;
    PRLabel57: TPRLabel;
    PRText52: TPRText;
    PRText57: TPRText;
    PRLabel58: TPRLabel;
    PRLabel59: TPRLabel;
    PRText68: TPRText;
    PRText69: TPRText;
    PRText75: TPRText;
    PRLabel60: TPRLabel;
    PRLabel61: TPRLabel;
    PRLabel62: TPRLabel;
    PRLabel63: TPRLabel;
    PRLabel64: TPRLabel;
    PRLabel65: TPRLabel;
    PRLabel66: TPRLabel;
    PRLabel67: TPRLabel;
    PRLabel68: TPRLabel;
    PRLabel69: TPRLabel;
    PRText76: TPRText;
    PRText80: TPRText;
    PRText81: TPRText;
    PRText82: TPRText;
    PRText89: TPRText;
    PRLabel70: TPRLabel;
    PRLabel71: TPRLabel;
    procedure PRLayoutPanel2BeforePrint(Sender: TObject;
      ACanvas: TPRCanvas; Rect: TRect);
    procedure PRLayoutPanel2AfterPrint(Sender: TObject;
      ACanvas: TPRCanvas; Rect: TRect);
    procedure CreatePDF1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure PRLayoutPanelBeforePrint(Sender: TObject;
      ACanvas: TPRCanvas; Rect: TRect);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PRGridPanel1BeforePrintChild(Sender: TObject;
      ACanvas: TPRCanvas; ACol, ARow: Integer; Rect: TRect);
    procedure CoverPagePrintPage(Sender: TObject; ACanvas: TPRCanvas);
    procedure PageControl1Change(Sender: TObject);
  private
    FCurrentOutline: array[0..5] of TPROutlineEntry;
    FContentsList: TList;
    FPos: integer;
    procedure CreateContentsList;
    function FindLink(AItem: TPRItem): TContentsElement;
  public
    { Public êÈåæ }
  end;

  TContentsElement = class(TObject)
  private
    FContentsIndex: string;
    FTitle: string;
    FData: TPdfDictionary;
    FTarget: TPRItem;
  public
    property ContentsIndex: string read FContentsIndex write FContentsIndex;
    property Title: string read FTitle write FTitle;
    property Data: TPdfDictionary read FData write FData;
    property Target: TPRItem read FTarget write FTarget;
  end;

var
  Form1: TForm1;

implementation


procedure TForm1.PRLayoutPanel2BeforePrint(Sender: TObject;
  ACanvas: TPRCanvas; Rect: TRect);
begin
  // change the horizontal scaling of th font.
  ACanvas.SetHorizontalScaling(80);
  PRLayoutPanelBeforePrint(Sender, ACanvas, Rect);
end;

procedure TForm1.PRLayoutPanel2AfterPrint(Sender: TObject;
  ACanvas: TPRCanvas; Rect: TRect);
begin
  // restore the horizontal scaling of th font.
  ACanvas.SetHorizontalScaling(100);
end;

procedure TForm1.CreateContentsList;
var
  APage: TPRPage;
  APanel: TPRPanel;
  AControl: TControl;
  i, j, k: integer;
  FChapterIndex: integer;
  FContentsElement: TContentsElement;
  S: string;
begin
  // clear the contents list.
  for i := FContentsList.Count - 1 downto 0 do
    TContentsElement(FContentsList.Items[i]).Free;

  // create new contents list.
  FChapterIndex := 0;
  for i := 0 to PageControl1.PageCount do
  begin
    APage := TPRPage(Self.FindComponent('PRPage' + IntToStr(i)));
    if (APage <> nil) and (APage.Controls[0] is TPRPanel) then
    begin
      APanel := TPRPanel(APage.Controls[0]);
      for j := 0 to APanel.ControlCount - 1 do
      begin
        AControl := APanel.Controls[j];
        if AControl.Tag = 2 then
        begin
          FContentsElement := TContentsElement.Create;
          with FContentsElement do
          begin
            if AControl is TPRText then
              Title := TPRText(AControl).Text
            else
            if AControl is TPRLabel then
              Title := TPRLabel(AControl).Caption
            else
              raise Exception.CreateFmt('invalid target control %s', [AControl.ClassName]);
            if (Title <> 'Contents') and (Title <> 'Copyright') then
            begin
              inc(FChapterIndex);
              FContentsList.Add(TContentsElement.Create);
              Title := 'Chapter' + IntToStr(FChapterIndex) + ' ' + Title;
              Target := TPRItem(AControl);
              FContentsList.Add(FContentsElement);
            end
            else
              FContentsElement.Free;
          end;
        end
        else
        if (AControl.Tag = 3) or (AControl.Tag = 4) then
        begin
          FContentsElement := TContentsElement.Create;
          with FContentsElement do
          begin
            if AControl is TPRText then
              S := TPRText(AControl).Text
            else
            if AControl is TPRLabel then
              S := TPRLabel(AControl).Caption
            else
              raise Exception.CreateFmt('invalid target control %s', [AControl.ClassName]);
            k := Pos(' ', S);
            if k < 1 then
              raise Exception.CreateFmt('invalid contents title text %s', [S]);
            ContentsIndex := Copy(S, 1, k);
            Title := Trim(Copy(S, k, Length(S) - k + 1));
            Target := TPRItem(AControl);
          end;
          FContentsList.Add(FContentsElement);
        end;
      end;
    end;
  end;
end;

procedure TForm1.CreatePDF1Click(Sender: TObject);
var
  APage: TPRPage;
  i: integer;
begin
  if not SaveDialog1.Execute then Exit;
  with PReport1 do
  begin
    FileName := SaveDialog1.FileName;
    BeginDoc;

    FCurrentOutline[0] := OutlineRoot;
    OutlineRoot.Opened := true;
    Print(CoverPage);

    CreateContentsList;

    // print index of contents.
    FPos := 0;
    while FPos < FContentsList.Count do
    begin
      Print(ContentsPage);
      PRText1Contents.Text := '';
      PRText1Contents.Tag := 0;
    end;

    for i := 2 to PageControl1.PageCount - 1 do
    begin
      APage := TPRPage(PageControl1.Pages[i].Controls[0]);
      if APage <> nil then
        Print(APage);
    end;
    EndDoc;

    for i := FContentsList.Count - 1 downto 0 do
      TContentsElement(FContentsList.Items[i]).Free;
    FContentsList.Clear;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage(POWER_PDF_VERSION_STR + #13#10 + POWER_PDF_COPYRIGHT);
end;

procedure TForm1.PRLayoutPanelBeforePrint(Sender: TObject;
  ACanvas: TPRCanvas; Rect: TRect);
var
  FDestination: TPRDestination;
  i, j: integer;
  FLevel: integer;
  FControlList: TList;
  FPRText: TPRText;
  TmpYPos: integer;
  ItemIndex: integer;
  FTextWidth: Single;
  Element: TContentsElement;
begin
  // printting page number
  if PReport1.PageNumber > 1 then
    with ACanvas do
    begin
      SetFont('Times-Roman', 8);
      FTextWidth := TextWidth(IntToStr(PReport1.PageNumber - 1));
      TextOut((PageWidth - FTextWidth) / 2 + 3, 30, IntToStr(PReport1.PageNumber - 1));
    end;

  // sorting the Items of the page by Top property.
  FControlList := TList.Create;
  with (Sender as TPRPanel) do
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TPRText) and (Controls[i].Tag > 0) then
    begin
      TmpYPos := Controls[i].Top;
      ItemIndex := -1;
      for j := 0 to FControlList.Count - 1 do
        if TControl(FControlList[j]).Top > TmpYPos then
        begin
          ItemIndex := j;
          Break;
        end;
      if ItemIndex = -1 then
        FControlList.Add(Controls[i])
      else
        FControlList.Insert(ItemIndex, Controls[i]);
    end;

  for i := 0 to FControlList.Count - 1 do
    if TPRText(FControlList[i]).Tag > 0 then
    begin
      // getting outline level from the Tag property.
      FPRText := TPRText(FControlList[i]);
      FLevel := FPRText.Tag;
      if FCurrentOutline[FLevel - 1] <> nil then
      begin
        FCurrentOutline[FLevel] := FCurrentOutline[FLevel - 1].AddChild;
        with FCurrentOutline[FLevel] do
        begin
          if FLevel = 1 then
            Opened := true;
          Title := FPRText.Text;
          FDestination := PReport1.CreateDestination;
          Dest := FDestination;
        end;
        with FDestination do
        begin
          DestinationType := dtXYZ;
          Top := FPRText.Top;
          Left := FPRText.Left;
          Zoom := 0;
        end;

        // setting the destination object to the link-annotation.
        Element := FindLink(TPRText(FControlList[i]));
        if Element <> nil then
          Element.Data.AddItem('Dest', FDestination.Data.GetValue);
      end;
    end;

  FControlList.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FContentsList := TList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := FContentsList.Count - 1 downto 0 do
    TContentsElement(FContentsList.Items[i]).Free;
  FContentsList.Free;
end;

procedure TForm1.PRGridPanel1BeforePrintChild(Sender: TObject;
  ACanvas: TPRCanvas; ACol, ARow: Integer; Rect: TRect);
begin
  if FPos < FContentsList.Count then
  with TContentsElement(FContentsList[FPos]) do
    begin
      if ContentsIndex = '' then
      begin
        lblSectionName.FontBold := true;
        lblSectionNo.FontSize := 12;
        lblSectionName.FontSize := 12;
        lblSectionName.Top := 0;
      end
      else
      begin
        lblSectionName.FontBold := false;
        lblSectionNo.FontSize := 11;
        lblSectionName.FontSize := 11;
        lblSectionNo.Top := 3;
        lblSectionName.Top := 3;
      end;
      lblSectionNo.Caption := ContentsIndex;
      lblSectionName.Caption := Title;
      with Rect do
        Data := ACanvas.PdfCanvas.Doc.CreateAnnotation(asLink,
            _PdfRect(Left, ACanvas.PageHeight - Top, Right, ACanvas.PageHeight - Bottom));
      with Data do
        AddItem('Border', TPdfArray.CreateNumArray(nil, [0, 0, 0]));
    end
  else
  begin
    lblSectionNo.Caption := '';
    lblSectionName.Caption := '';
  end;
  inc(FPos);
end;

procedure TForm1.CoverPagePrintPage(Sender: TObject; ACanvas: TPRCanvas);
begin
  with PReport1 do
  begin
    OpenAction := CreateDestination;
    OpenAction.DestinationType := dtXYZ;
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

end;

function TForm1.FindLink(AItem: TPRItem): TContentsElement;
var
  i: integer;
  Element: TContentsElement;
begin
  result := nil;
  for i := FContentsList.Count - 1 downto 0 do
  begin
    Element := TContentsElement(FContentsList.Items[i]);
    if Element.Target = AItem then result := Element;
  end;
end;

initialization
  {$i UMakeDoc.lrs}

end.
