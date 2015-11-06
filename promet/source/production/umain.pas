unit umain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, uExtControls,
  uIntfStrConsts, db, memds, FileUtil, IpHtml, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage, uOrder,
  uBaseDbInterface,uBaseDbClasses,uprometscripts,uDocuments;
type
  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acExecuteStep: TAction;
    acPrepare: TAction;
    acLoadOrder: TAction;
    acSearchMasterdata: TAction;
    acSearchOrder: TAction;
    acSave: TAction;
    acAbort: TAction;
    acNextStep: TAction;
    ActionList1: TActionList;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel7: TBevel;
    BitBtn1: TBitBtn;
    BitBtn3: TSpeedButton;
    BitBtn4: TSpeedButton;
    Button1: TButton;
    cbVersion: TComboBox;
    eOrder: TEdit;
    ipHTML: TIpHtmlPanel;
    ipHTML1: TIpHtmlPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pNav1: TPanel;
    rbNoData: TRadioButton;
    rbOrder: TRadioButton;
    rbList: TRadioButton;
    rbArticle: TRadioButton;
    sbMenue: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Splitter1: TSplitter;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tvStep: TTreeView;
    procedure acLoadOrderExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acNextStepExecute(Sender: TObject);
    procedure acSearchMasterdataExecute(Sender: TObject);
    procedure eOrderKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function SetOrderfromSearch(aLink: string): Boolean;
    procedure tvStepSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FOrder : TOrder;
    procedure DoOpen;
    procedure FindNextStep;
    function LoadStep : Boolean;
  public
    { public declarations }
    procedure DoCreate;
  end;
  TProdTreeData = class
  public
    Position : Int64;
    Script : TBaseScript;
    Documents : TDocuments;

    PreText : TStringList;
    WorkText : TStringList;
    constructor Create;
    destructor Destroy; override;
    function CheckContent : Boolean;
    procedure LoadScript(aScript : string;aVersion : Variant);
  end;

var
  fMain: TfMain;
resourcestring
  strNoOrderFound                       = 'Es wurde kein Auftrag oder Artikel gefunden der zum Suchkriterium passt !';
  strDoPick                             = 'kommissionieren';
implementation
{$R *.lfm}
uses uBaseApplication, uData,uMasterdata,uSearch,variants,uBaseERPDBClasses;

constructor TProdTreeData.Create;
begin
  PreText := TStringList.Create;
  WorkText := TStringList.Create;
end;

destructor TProdTreeData.Destroy;
begin
  PreText.Free;
  WorkText.Free;
  inherited Destroy;
end;

function TProdTreeData.CheckContent: Boolean;
begin
  Result := False;
  if (WorkText.Text<>'')
  or (PreText.Text<>'')
  or (Assigned(Script) and (Script.Count>0)) then
    Result := True;
end;

procedure TProdTreeData.LoadScript(aScript: string; aVersion: Variant);
begin
  if not Assigned(Script) then
    Script := TBaseScript.Create(nil);
  Script.SelectByName(aScript);
  Script.Open;
  if not Script.Locate('VERSION',aVersion,[]) then
    Script.Close;
end;

procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Production');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
var
  a: TOrder;
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  FOrder := TOrder.Create(nil);
end;

procedure TfMain.acLoadOrderExecute(Sender: TObject);
var
  aMasterdata: TMasterdata;
begin
  //Try to select by Orderno
  FOrder.SelectFromNumber('');
  if IsNumeric(eOrder.Text) then
    FOrder.SelectFromNumber(eOrder.Text);
  FOrder.Open;
  if (FOrder.Count=0) and (IsNumeric(eOrder.Text)) then
    begin
      //Try to select by Commission
      FOrder.SelectFromNumber(eOrder.Text);
    end;
  FOrder.Open;
  if FOrder.Count=0 then
    begin
      //Find Article and Create Order
      aMasterdata := TMasterdata.Create(nil);
      aMasterdata.SelectFromNumber(eOrder.Text);
      aMasterdata.Open;
      if cbVersion.Enabled and (cbVersion.Text<>'') then
        aMasterdata.Locate('VERSION',cbVersion.Text,[]);
      if aMasterdata.Count>0 then
        begin
          FOrder.OrderType.Open;
          if FOrder.OrderType.Locate('SI_PROD;TYPE',VarArrayOf(['Y',7]),[]) then
            begin
              FOrder.Insert;
              FOrder.Positions.Insert;
              //FOrder.Status.AsString:=FOrder.OrderType.FieldByName('STATUS').AsString;
              FOrder.Positions.Assign(aMasterdata);
              FOrder.Positions.Post;
              FOrder.Post;
            end;
        end;
      aMasterdata.Free;
    end;
  if FOrder.Count=0 then
    begin
      Showmessage(strNoOrderFound);
      eOrder.SelectAll;
      eOrder.SetFocus;
    end
  else DoOpen;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.acNextStepExecute(Sender: TObject);
begin
  FindNextStep;
end;

procedure TfMain.acSearchMasterdataExecute(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@SetOrderfromSearch;
  fSearch.AllowSearchTypes(strOrders+','+strMasterdata);
  fSearch.Execute(True,'PRODSE','');
end;

procedure TfMain.eOrderKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    begin
      acLoadOrder.Execute;
    end
  else
    begin
      cbVersion.Text:='';
      cbVersion.Enabled := False;
    end;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FOrder.Free;
  with Application as IBaseApplication do
    begin
      Data.ActiveUsers.DataSet.AfterScroll:=nil;
      SaveConfig;
      DoExit;
    end;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
  if Assigned(Data) then
    begin
    end;
end;

function TfMain.SetOrderfromSearch(aLink: string): Boolean;
var
  aMasterdata: TMasterdata;
begin
  aMasterdata := TMasterdata.Create(nil);
  aMasterdata.SelectFromLink(aLink);
  aMasterdata.Open;
  eOrder.Text:=aMasterdata.Number.AsString;
  cbVersion.Text:=aMasterdata.Version.AsString;
  aMasterdata.Select(aMasterdata.Number.AsString);
  aMasterdata.Open;
  cbVersion.Enabled:=aMasterdata.Count>1;
  cbVersion.Items.Clear;
  aMasterdata.First;
  while not aMasterdata.EOF do
    begin
      cbVersion.Items.Add(aMasterdata.Version.AsString);
      aMasterdata.Next;
    end;
  aMasterdata.Free;
end;

procedure TfMain.tvStepSelectionChanged(Sender: TObject);
begin
  if Assigned(tvStep.Selected) then
    begin
      if FOrder.Positions.Locate('SQL_ID',TProdTreeData(tvStep.Selected.Data).Position,[]) then
        LoadStep
      else rbNoData.Checked:=True;
    end;
end;

procedure TfMain.DoOpen;
var
  nNode: TTreeNode;
  nComm : TTreeNode = nil;

  function GetParentNode : TTreeNode;
  var
    aNode: TTreeNode;
  begin
    result := nil;
    aNode := nil;
    if tvStep.Items.Count>0 then
      aNode := tvStep.Items[0];
    while Assigned(aNode) do
      begin
        if TProdTreeData(aNode.Data).Position=FOrder.Positions.FieldByName('PARENT').AsVariant then
          begin
            Result := aNode;
            break;
          end;
        aNode := aNode.GetNext;
      end;
    if tvStep.Items.Count>0 then
      begin
        case FOrder.Positions.PosTyp.FieldByName('TYPE').AsInteger of
        0,1,2:
          begin
            if not Assigned(nComm) then
              begin
                nComm := tvStep.Items.AddChildObject(GetParentNode,strDoPick,TProdTreeData.Create);
                nComm.ImageIndex:=43;
                nComm.SelectedIndex:=nComm.ImageIndex;
              end;
            Result := nComm
          end;
        else nComm := nil;
        end;
      end;
  end;

begin
  eOrder.Enabled:=FOrder.Count>0;
  cbVersion.Enabled:=cbVersion.Enabled and (FOrder.Count>0);
  tvStep.Enabled:=FOrder.Count>0;
  tvStep.Items.Clear;
  if FOrder.Count>0 then
    begin
      FOrder.Positions.Open;
      FOrder.Positions.First;
      while not FOrder.Positions.EOF do
        begin
          nNode := tvStep.Items.AddChildObject(GetParentNode,FOrder.Positions.FieldByName('SHORTTEXT').AsString,TProdTreeData.Create);
          case FOrder.Positions.PosTyp.FieldByName('TYPE').AsInteger of
          0,1,2:nNode.ImageIndex:=14;//Artikel
          3:nNode.ImageIndex:=49;//Text
          9:nNode.ImageIndex:=57;//Montage/Argeitsgang
          end;
          nNode.SelectedIndex:=nNode.ImageIndex;
          TProdTreeData(nNode.Data).Position:=FOrder.Positions.Id.AsVariant;
          FOrder.Positions.Next;
        end;
    end;
  FindNextStep;
end;

procedure TfMain.FindNextStep;
begin

end;

function TfMain.LoadStep: Boolean;
var
  nOrder: TOrder;
  aMasterdata: TMasterdata;
  TreeData : TProdTreeData;
begin
  Result := False;
  rbNoData.Checked:=True;
  TreeData := TProdTreeData(tvStep.Selected.Data);
  TreeData.WorkText.Clear;
  TreeData.PreText.Clear;
  FreeAndNil(TreeData.Script);
  FreeAndNil(TreeData.Documents);
  //Information in Order
  if (FOrder.Positions.FieldByName('SCRIPT').AsString<>'') or (FOrder.Positions.FieldByName('TEXT').AsString<>'') then
    begin
      TreeData.LoadScript(FOrder.Positions.FieldByName('SCRIPT').AsString,FOrder.Positions.FieldByName('SCRIPTVER').AsVariant);
      TreeData.WorkText.Text:=FOrder.Positions.FieldByName('TEXT').AsString;
      Result := TreeData.CheckContent;
      rbOrder.Checked:=True;
    end;
  //Information in Piecelist
  if not Result then
    begin
      nOrder := TOrder.Create(nil);
      //Unseren Auftrag nochmal öffnen damit wir die Position wechseln können
      nOrder.Select(FOrder.Id.AsVariant);
      nOrder.Open;
      nOrder.Positions.Open;
      if nOrder.Positions.Locate('SQL_ID',FOrder.Positions.FieldByName('PARENT').AsVariant,[]) then //Vorgängerposition finden (zu fertigender Artikel)
        begin
          aMasterdata := TMasterdata.Create(nil);
          aMasterdata.Select(nOrder.Positions.FieldByName('IDENT').AsString);
          aMasterdata.Open;
          //nach Artikel/Version/Sprache suchen
          if not aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,nOrder.Positions.FieldByName('VERSION').AsString,nOrder.Positions.FieldByName('LANGUAGE').AsString]),[]) then
            begin
              //nach Artikel/Version suchen (Sprache ignorieren)
              if not aMasterdata.Locate('ID;VERSION',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,nOrder.Positions.FieldByName('VERSION').AsString]),[]) then
                aMasterdata.Close;
            end;
          if aMasterdata.Active then
            begin
              aMasterdata.Positions.Open;
              //Positionsnummer in Stückliste finden
              if aMasterdata.Positions.Locate('POSNO',FOrder.Positions.FieldByName('TPOSNO').AsString,[]) then
                begin
                  TreeData.LoadScript(aMasterdata.Positions.FieldByName('SCRIPT').AsString,aMasterdata.Positions.FieldByName('SCRIPTVER').AsVariant);
                  TreeData.WorkText.Text:=aMasterdata.Positions.FieldByName('TEXT').AsString;
                  Result := TreeData.CheckContent;
                  rbList.Checked:=Result;
                end;
            end;
          aMasterdata.Free;
        end;
      nOrder.Free;
    end;
  if not Result then
    begin
      aMasterdata := TMasterdata.Create(nil);
      aMasterdata.Select(FOrder.Positions.FieldByName('IDENT').AsString);
      aMasterdata.Open;
      if not aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([FOrder.Positions.FieldByName('IDENT').AsString,FOrder.Positions.FieldByName('VERSION').AsString,FOrder.Positions.FieldByName('LANGUAGE').AsString]),[]) then
        begin
          //nach Artikel/Version suchen (Sprache ignorieren)
          if not aMasterdata.Locate('ID;VERSION',VarArrayOf([FOrder.Positions.FieldByName('IDENT').AsString,FOrder.Positions.FieldByName('VERSION').AsString]),[]) then
            aMasterdata.Close;
        end;
      if aMasterdata.Active then
        begin
          TreeData.LoadScript(aMasterdata.FieldByName('SCRIPT').AsString,aMasterdata.FieldByName('SCRIPTVER').AsVariant);
          if not Assigned(uBaseERPDBClasses.TextTyp) then
            uBaseERPDBClasses.TextTyp := TTextTypes.Create(nil);
          Texttyp.Open;
          if TextTyp.Locate('TYP','7',[]) then
            begin
              aMasterdata.Texts.Open;
              if aMasterdata.Texts.Locate('TEXTTYPE',TextTyp.DataSet.RecNo,[]) then
                TreeData.WorkText.Text:=aMasterdata.Texts.FieldByName('TEXT').AsString;
            end;
          Result := TreeData.CheckContent;
          rbArticle.Checked:=Result;
        end;
      aMasterdata.Free;
    end;
end;

initialization
end.
