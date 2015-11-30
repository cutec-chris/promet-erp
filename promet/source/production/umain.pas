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
Created 08.10.2015
*******************************************************************************}
unit umain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, uExtControls,
  uIntfStrConsts, db, memds, FileUtil, ipHTML, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage, uOrder,
  uBaseDbInterface,uBaseDbClasses,uprometscripts,uDocuments,uprometpascalscript,
  genpascalscript,genscript;
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
    acCloseOrder: TAction;
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
    ipWorkHTML: TIpHtmlPanel;
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
    Panel3: TPanel;
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
    procedure acCloseOrderExecute(Sender: TObject);
    procedure acExecuteStepExecute(Sender: TObject);
    procedure acLoadOrderExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acNextStepExecute(Sender: TObject);
    procedure acPrepareExecute(Sender: TObject);
    procedure acSearchMasterdataExecute(Sender: TObject);
    procedure eOrderKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function SetOrderfromSearch(aLink: string): Boolean;
    procedure TreeDataScriptScriptRunLine(Sender: TScript; Module: string;
      aPosition, aRow, aCol: Integer);
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
    procedure ScriptWriteln(const s: string);
    function TPrometPascalScriptUses(Sender: TPascalScript;
      const Name: String; OnlyAdditional: Boolean): Boolean;
  public
    Position : Int64;
    Script : TBaseScript;
    Documents : TDocuments;

    PreText : TStringList;
    WorkText : TStringList;
    ScriptOutput : TStringList;
    Prepared : Boolean;
    constructor Create;
    destructor Destroy; override;
    function CheckContent : Boolean;
    procedure ShowData;
    procedure LoadScript(aScript : string;aVersion : Variant);
    procedure LoadDocuments(aID : largeInt;aType : string;aTID : string;aVersion : Variant;aLanguage : Variant);
  end;
  TSimpleIpHtml = class(TIpHtml)
    procedure SimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  protected
  public
    property OnGetImageX;
    constructor Create;
  end;

var
  fMain: TfMain;
resourcestring
  strNoOrderFound                       = 'Es wurde kein Auftrag oder Artikel gefunden der zum Suchkriterium passt !';
  strDoPick                             = 'kommissionieren';
implementation
{$R *.lfm}
uses uBaseApplication, uData,uMasterdata,uSearch,variants,uBaseERPDBClasses,
  uBaseVisualControls,uprometpythonscript;

procedure InternalSleep(MiliSecValue: LongInt); StdCall;
var
  aTime: Int64;
begin
  aTime := GetTicks;
  while (GetTicks-aTime) < MiliSecValue do
    Application.ProcessMessages;
end;

procedure TSimpleIpHtml.SimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  TreeData: TProdTreeData;
  ms: TMemoryStream;
  aPicture: TPicture;
  aURL: String;
  Path: String;
  aNumber: integer;
  NewPath: String;
  Result: TMemoryStream;
  tmp: String;
begin
  Picture:=nil;
  if Assigned(fMain.tvStep.Selected) then
    begin
      TreeData := TProdTreeData(fMain.tvStep.Selected.Data);
      if not Assigned(TreeData.Documents) then exit;
      Path := URL;
      if copy(uppercase(Path),0,5)='ICON(' then
        begin
          if TryStrToInt(copy(Path,6,length(Path)-6),aNumber) then
            begin
              ms := TMemoryStream.Create;
              Picture := TPicture.Create;
              fVisualControls.Images.GetBitmap(aNumber,Picture.Bitmap);
              Picture.SaveToStreamWithFileExt(ms,'png');
              NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
              ms.Position:=0;
              Result := ms;
              ms.Position:=0;
              aPicture := TPicture.Create;
              aPicture.LoadFromStreamWithFileExt(ms,'png');
              Picture := aPicture;
            end;
        end
      else if copy(uppercase(Path),0,12)='HISTORYICON(' then
        begin
          tmp := copy(Path,13,length(Path)-13);
          if TryStrToInt(tmp,aNumber) then
            begin
              ms := TMemoryStream.Create;
              Picture := TPicture.Create;
              fVisualControls.HistoryImages.GetBitmap(aNumber,Picture.Bitmap);
              Picture.SaveToStreamWithFileExt(ms,'png');
              NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
              ms.Position:=0;
              Result := ms;
              ms.Position:=0;
              aPicture := TPicture.Create;
              aPicture.LoadFromStreamWithFileExt(ms,'png');
              Picture := aPicture;
            end;
        end
      else
        begin
          TreeData.Documents.Open;
          aURL := copy(URL,0,rpos('.',URL)-1);
          if TreeData.Documents.Locate('NAME',aURL,[loCaseInsensitive]) then
            begin
              ms := TMemoryStream.Create;
              Data.BlobFieldToStream(TreeData.Documents.DataSet,'DOCUMENT',ms);
              ms.Position:=0;
              aPicture := TPicture.Create;
              aPicture.LoadFromStreamWithFileExt(ms,TreeData.Documents.FieldByName('EXTENSION').AsString);
              Picture := aPicture;
            end;
        end;
    end;
end;

constructor TSimpleIpHtml.Create;
begin
  inherited;
  OnGetImageX:=@SimpleIpHtmlGetImageX;
end;

procedure TProdTreeData.ScriptWriteln(const s: string);
var
  aTxt: String;
begin
  if copy(s,0,7)='**STEP ' then
    ScriptOutput.Add('<img src="ICON(22)"></img><i>'+copy(s,8,length(s))+'</i><br>')
  else if copy(s,0,10)='**STEPEND ' then
    begin
      aTxt := ScriptOutput[ScriptOutput.Count-1];
      aTxt := copy(aTxt,30,length(aTxt)-29-8);
      ScriptOutput.Delete(ScriptOutput.Count-1);
      ScriptOutput.Add('<img src="ICON(74)"></img><span>'+aTxt+' -> '+copy(s,11,length(s))+'</span><br>')
    end
  else if copy(s,0,8)='**ERROR ' then
    begin
      aTxt := ScriptOutput[ScriptOutput.Count-1];
      aTxt := copy(aTxt,30,length(aTxt)-29-8);
      ScriptOutput.Delete(ScriptOutput.Count-1);
      ScriptOutput.Add('<img src="ICON(75)"></img><b>'+aTxt+' -> '+copy(s,9,length(s))+'</b><br>')
    end
  else ScriptOutput.Add(s);
  ShowData;
  fMain.ipWorkHTML.Repaint;
  fMain.ipWorkHTML.Scroll(hsaEnd);
  Application.ProcessMessages;
end;

function TProdTreeData.TPrometPascalScriptUses(Sender: TPascalScript;
  const Name: String; OnlyAdditional: Boolean): Boolean;
begin
  if lowercase(Name) = 'production' then
    begin
      //HideWorkText
      //ClearScriptOutput
      Result := True;
    end;
end;

constructor TProdTreeData.Create;
begin
  PreText := TStringList.Create;
  WorkText := TStringList.Create;
  ScriptOutput := TStringList.Create;
  Prepared:=False;
end;

destructor TProdTreeData.Destroy;
begin
  PreText.Free;
  WorkText.Free;
  ScriptOutput.Free;
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

procedure TProdTreeData.ShowData;
var
  aHTML: TSimpleIpHtml;
  ss: TStringStream;
begin
  fMain.acPrepare.Enabled:=PreText.Text<>'';
  fMain.acPrepare.Checked:=(not Prepared) and (PreText.Text<>'');
  if fMain.acPrepare.Checked then
    begin
      aHTML := TSimpleIPHtml.Create;
      if pos('<body',lowercase(PreText.Text))=0 then
        ss := TStringStream.Create('<body>'+UniToSys(PreText.Text)+'</body>')
      else
        ss := TStringStream.Create(UniToSys(PreText.Text));
      aHTML.LoadFromStream(ss);
      ss.Free;
      fMain.ipWorkHTML.SetHtml(aHTML);
    end
  else
    begin
      aHTML := TSimpleIPHtml.Create;
      if pos('<body',lowercase(WorkText.Text))=0 then
        ss := TStringStream.Create('<body>'+UniToSys(WorkText.Text+'<br><br>'+ScriptOutput.Text)+'</body>')
      else
        ss := TStringStream.Create(UniToSys(WorkText.Text+ScriptOutput.Text));
      aHTML.LoadFromStream(ss);
      ss.Free;
      fMain.ipWorkHTML.SetHtml(aHTML);
    end;
  if Assigned(Script) and (Script.Count>0) then
    fMain.acExecuteStep.Enabled:=(Prepared or ((PreText.Text=''))) and ((not Assigned(Script) or (not Script.Script.IsRunning)));
end;

procedure TProdTreeData.LoadScript(aScript: string; aVersion: Variant);
begin
  if not Assigned(Script) then
    Script := TBaseScript.Create(nil);
  Script.SelectByName(aScript);
  Script.Open;
  Script.Writeln:=@ScriptWriteln;
  if Assigned(Script.Script) then
    if Script.Script is TPrometPascalScript then
      begin
        TPrometPascalScript(Script.Script).OnUses:=@TPrometPascalScriptUses;
      end;
  if not Script.Locate('VERSION',aVersion,[]) then
    Script.Close;
end;

procedure TProdTreeData.LoadDocuments(aID: largeInt; aType: string;
  aTID: string; aVersion: Variant; aLanguage: Variant);
begin
  if not Assigned(Documents) then
    Documents := TDocuments.Create(nil);
  Documents.Select(aID,aType,aTID,aVersion,aLanguage);
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
      FOrder.SelectFromCommission(eOrder.Text);
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
  if FOrder.Count>0 then
    eOrder.Text:=FOrder.Number.AsString;
  if FOrder.Count=0 then
    begin
      Showmessage(strNoOrderFound);
      eOrder.SelectAll;
      eOrder.SetFocus;
    end
  else DoOpen;
end;

procedure TfMain.acCloseOrderExecute(Sender: TObject);
begin
  tvStep.Items.Clear;
  ipWorkHTML.SetHtml(nil);
  eOrder.Enabled:=True;
  cbVersion.Enabled:=True;
  acSearchMasterdata.Enabled:=True;
  acSearchOrder.Enabled:=True;
  acLoadOrder.Enabled:=True;
  acCloseOrder.Enabled:=False;
end;

procedure TfMain.acExecuteStepExecute(Sender: TObject);
var
  TreeData: TProdTreeData;
begin
  if Assigned(fMain.tvStep.Selected) then
    begin
      TreeData := TProdTreeData(fMain.tvStep.Selected.Data);
      acExecuteStep.Enabled:=False;
      TreeData.ScriptOutput.Clear;
      TreeData.Script.ActualObject := FOrder;
      TreeData.Script.Script.OnRunLine:=@TreeDataScriptScriptRunLine;
      if Assigned(TreeData.Script) then
        if not TreeData.Script.Execute(Null) then
          begin
            if not Assigned(TreeData.Script.Script) then
              TreeData.ScriptOutput.Add('<b>Ausführung fehlgeschlagen:Scripttyp unbekannt</b>')
            else
              TreeData.ScriptOutput.Add('<b>Ausführung fehlgeschlagen:'+TreeData.Script.Script.Results+'</b>');
            TreeData.ShowData;
          end;
      acExecuteStep.Enabled:=True;
    end;
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

procedure TfMain.acPrepareExecute(Sender: TObject);
var
  TreeData: TProdTreeData;
begin
  if Assigned(fMain.tvStep.Selected) then
    begin
      TreeData := TProdTreeData(fMain.tvStep.Selected.Data);
      TreeData.Prepared:=not TreeData.Prepared;
      TreeData.ShowData;
    end;
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
  if aMasterdata.Count>0 then
    begin
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
    end;
  aMasterdata.Free;
end;

procedure TfMain.TreeDataScriptScriptRunLine(Sender: TScript; Module: string;
  aPosition, aRow, aCol: Integer);
begin
  Application.ProcessMessages;
end;

procedure TfMain.tvStepSelectionChanged(Sender: TObject);
var
  Res: Boolean;
begin
  if Assigned(tvStep.Selected) then
    begin
      fMain.acExecuteStep.Enabled:=False;
      if FOrder.Positions.Locate('SQL_ID',TProdTreeData(tvStep.Selected.Data).Position,[]) then
        Res := LoadStep
      else Res := False;
      if not Res then
        begin
          ipWorkHTML.SetHtml(nil);
          rbNoData.Checked:=True;
        end;
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
  eOrder.Text:=FOrder.Number.AsString;
  FreeAndNil(FOrder);
  FOrder := TOrder.Create(nil);
  cbVersion.Enabled:=cbVersion.Enabled and (FOrder.Count>0);
  tvStep.Enabled:=FOrder.Count>0;
  tvStep.Items.Clear;
  FOrder.SelectFromNumber(eOrder.Text);
  FOrder.Open;
  if FOrder.Count>0 then
    begin
      FOrder.Positions.Close;
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
  if tvStep.Items.Count>0 then
    begin
      tvStep.Selected:=tvStep.Items[0].GetNext;
      tvStep.Items[0].Expanded:=True;
      FindNextStep;
    end;
  eOrder.Enabled:=False;
  cbVersion.Enabled:=False;
  acSearchMasterdata.Enabled:=False;
  acSearchOrder.Enabled:=False;
  acLoadOrder.Enabled:=False;
  acCloseOrder.Enabled:=True;
  tvStep.Enabled:=True;
end;

procedure TfMain.FindNextStep;
begin
  while (Assigned(tvStep.Selected)) and (not LoadStep) do
    begin
      if tvStep.Selected.ImageIndex=43 then //Kommissionieren
        tvStep.Selected:=tvStep.Selected.GetNextSibling
      else
        tvStep.Selected:=tvStep.Selected.GetNext;
    end;
end;

function TfMain.LoadStep: Boolean;
var
  nOrder: TOrder;
  aMasterdata: TMasterdata;
  TreeData : TProdTreeData;
  aPosID: String;
  aTexts: TBoilerplate;
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
      if FOrder.Positions.DataSet.FieldDefs.IndexOf('ORDERNO') <> -1 then
        aPosID := FOrder.Positions.FieldByName('ORDERNO').AsString+FOrder.Positions.FieldByName('POSNO').AsString
      else
        aPosID := FOrder.Positions.FieldByName('SQL_ID').AsString+FOrder.Positions.FieldByName('POSNO').AsString;
      TreeData.LoadDocuments(FOrder.Positions.Id.AsVariant,'P',aPosId,Null,Null);
      TreeData.WorkText.Text:=FOrder.Positions.FieldByName('TEXT').AsString;
      if FOrder.Positions.FieldByName('PREPTEXT').AsString<>'' then
        begin
          aTexts := TBoilerplate.Create(nil);
          aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(FOrder.Positions.FieldByName('PREPTEXT').AsString));
          if aTexts.Count>0 then
            TreeData.PreText.Text:=aTexts.FieldByName('TEXT').AsString;
          aTexts.Free;
        end;
      if FOrder.Positions.FieldByName('WORKTEXT').AsString<>'' then
        begin
          aTexts := TBoilerplate.Create(nil);
          aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(FOrder.Positions.FieldByName('WORKTEXT').AsString));
          if aTexts.Count>0 then
            TreeData.WorkText.Text:=aTexts.FieldByName('TEXT').AsString;
          aTexts.Free;
        end;
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
                  if aMasterdata.Positions.DataSet.FieldDefs.IndexOf('ORDERNO') <> -1 then
                    aPosID := aMasterdata.Positions.FieldByName('ORDERNO').AsString+aMasterdata.Positions.FieldByName('POSNO').AsString
                  else
                    aPosID := aMasterdata.Positions.FieldByName('SQL_ID').AsString+aMasterdata.Positions.FieldByName('POSNO').AsString;
                  TreeData.LoadDocuments(aMasterdata.Positions.Id.AsVariant,'P',aPosId,Null,Null);
                  if aMasterdata.Positions.FieldByName('PREPTEXT').AsString<>'' then
                    begin
                      aTexts := TBoilerplate.Create(nil);
                      aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.Positions.FieldByName('PREPTEXT').AsString));
                      if aTexts.Count>0 then
                        TreeData.PreText.Text:=aTexts.FieldByName('TEXT').AsString;
                      aTexts.Free;
                    end;
                  if aMasterdata.Positions.FieldByName('WORKTEXT').AsString<>'' then
                    begin
                      aTexts := TBoilerplate.Create(nil);
                      aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.Positions.FieldByName('WORKTEXT').AsString));
                      if aTexts.Count>0 then
                        TreeData.WorkText.Text:=aTexts.FieldByName('TEXT').AsString;
                      aTexts.Free;
                    end;
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
          TreeData.LoadDocuments(aMasterdata.Id.AsVariant,aMasterdata.GetTyp,aMasterdata.FieldByName('ID').AsString,aMasterdata.FieldByName('VERSION').AsVariant,aMasterdata.FieldByName('LANGUAGE').AsVariant);
          if aMasterdata.FieldByName('PREPTEXT').AsString<>'' then
            begin
              aTexts := TBoilerplate.Create(nil);
              aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.FieldByName('PREPTEXT').AsString));
              if aTexts.Count>0 then
                TreeData.PreText.Text:=aTexts.FieldByName('TEXT').AsString;
              aTexts.Free;
            end;
          if aMasterdata.FieldByName('WORKTEXT').AsString<>'' then
            begin
              aTexts := TBoilerplate.Create(nil);
              aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.FieldByName('WORKTEXT').AsString));
              if aTexts.Count>0 then
                TreeData.WorkText.Text:=aTexts.FieldByName('TEXT').AsString;
              aTexts.Free;
            end;
          Result := TreeData.CheckContent;
          rbArticle.Checked:=Result;
        end;
      aMasterdata.Free;
    end;
  if Result then
    begin
      TreeData.ShowData;
    end;
end;

initialization
  genpascalscript.DoSleep:=@InternalSleep;
end.
