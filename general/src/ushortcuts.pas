unit uShortcuts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  VirtualStringTree, ExtCtrls, Buttons, ActnList,dom,xmlread,xmlwrite,VirtualTrees,
  uGeneralStrConsts,LCLProc,LCLType,utils;

type

  PSTreeEntry = ^TSTreeEntry;
  TSTreeEntry = record
    Obj : TAction;
    Text : string;
  end;

  { TfShortcuts }

  TfShortcuts = class(TForm)
    Bevel1: TBevel;
    bOK: TButton;
    bGetKey: TButton;
    bDeleteKey: TButton;
    pFunction: TPanel;
    pShortcut: TPanel;
    pHeader: TPanel;
    Splitter1: TSplitter;
    tvMain: TVirtualStringTree;
    procedure bDeleteKeyClick(Sender: TObject);
    procedure bGetKeyClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
  private
    { private declarations }
    Lists : TList;
  public
    { public declarations }
    procedure RegisterActionList(ActLst : TActionList);
    procedure Load;
    procedure Save;
    procedure SetLanguage;
  end; 

var
  fShortcuts: TfShortcuts;

implementation

{ TfShortcuts }

procedure TfShortcuts.FormCreate(Sender: TObject);
begin
  Lists := TList.Create;
  tvMain.NodeDataSize := sizeof(TSTreeEntry);
  tvMain.Header.Columns.Add;
  tvMain.Header.Columns.Add;
end;

procedure TfShortcuts.bGetKeyClick(Sender: TObject);
begin
  if not bGetKey.Enabled then exit;
  bGetKey.Enabled := False;
end;

procedure TfShortcuts.bDeleteKeyClick(Sender: TObject);
begin
  if Assigned(tvMain.FocusedNode) and Assigned(tvMain.GetNodeData(tvMain.FocusedNode)) then
    PSTreeEntry(tvMain.GetNodeData(tvMain.FocusedNode))^.Obj.ShortCut := scNone;
  tvMain.Invalidate;
end;

procedure TfShortcuts.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfShortcuts.FormDestroy(Sender: TObject);
begin
  Lists.Free;
end;

procedure TfShortcuts.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not bGetKey.Enabled) and Assigned(tvMain.FocusedNode) and Assigned(tvMain.GetNodeData(tvMain.FocusedNode)) then
    begin
      PSTreeEntry(tvMain.GetNodeData(tvMain.FocusedNode))^.Obj.ShortCut := KeyToShortCut(Key, Shift);
      tvMain.Invalidate;
      bGetKey.Enabled := True;
    end;
end;

procedure TfShortcuts.FormShow(Sender: TObject);
var
  i: Integer;
  TN,TN2: PVirtualNode;
  a: Integer;
begin
  tvMain.Clear;
  for i := 0 to Lists.Count-1 do
    begin
      if TActionList(Lists[i]).Owner is TForm then
        begin
          TN := tvMain.AddChild(nil);
          PSTreeEntry(tvMain.getNodeData(TN))^.Text := TForm(TActionList(Lists[i]).Owner).Caption;
          for a := 0 to TActionList(Lists[i]).ActionCount-1 do
            begin
              TN2 := tvMain.AddChild(TN);
              PSTreeEntry(tvMain.GetNodeData(TN2))^.Obj := TAction(TActionList(Lists[i]).Actions[a]);
              PSTreeEntry(tvMain.GetNodeData(TN2))^.Text := TAction(TActionList(Lists[i]).Actions[a]).Caption;
            end;
          tvMain.Expanded[TN] := True;
        end;
    end;
end;

procedure TfShortcuts.Splitter1Moved(Sender: TObject);
begin
  tvMain.Header.Columns[0].Width := pFunction.Width;
  tvMain.Header.Columns[1].Width := tvMain.Width-pFunction.Width;
end;

procedure TfShortcuts.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data : PSTreeEntry;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    exit;
  if Column = 0 then
    CellText := Data^.Text
  else
    CellText := '';
  if Assigned(Data^.Obj) and (Column = 1) then
    CellText := ShortCutToText(Data^.Obj.ShortCut);
end;

procedure TfShortcuts.RegisterActionList(ActLst: TActionList);
begin
  Lists.Add(ActLst);
end;

procedure TfShortcuts.Load;
var
  i: Integer;
  doc : TXMLDocument;
  rootnode : TDOMNode;
  listnode : TDOMNode;
  shortnode : TDOMElement;
  a: Integer;
  Filename : string;
begin
  if Application.HasOption('c','config-path') then
    FileName := Application.GetOptionValue('c','config-path')+DirectorySeparator+'shortcuts.xml'
  else
    FileName := GetConfigDir(copy(ExtractFileName(Application.Exename),0,length(ExtractFileName(Application.Exename))-length(ExtractFileExt(ExtractFileName(Application.Exename)))))+DirectorySeparator+'shortcuts.xml';
  if not FileExists(Filename) then
    exit;
  ReadXMLFile(doc,GetConfigDir(copy(Application.Exename,0,length(Application.Exename)-length(ExtractFileExt(Application.Exename))))+Directoryseparator+'shortcuts.xml');
  RootNode := doc.FindNode('ShortCuts');
  if not Assigned(RootNode) then
    exit;
  for i := 0 to Lists.Count-1 do
    with TActionList(Lists[i]) do
      begin
        listnode := rootnode.FindNode(Owner.Name+'.'+Name);
        if Assigned(listnode) then
          for a := 0 to ActionCount-1 do
            if listnode.FindNode(Actions[a].Name) <> nil then
              begin
                shortnode := listnode.FindNode(Actions[a].Name) as TDomElement;
                TAction(Actions[a]).ShortCut := StrToInt(shortnode.AttribStrings['ShortCut']);
              end;
      end;
end;

procedure TfShortcuts.Save;
var
  doc : TXMLDocument;
  listnode : TDOMElement;
  shortnode : TDOMElement;
  rootnode : TDOMNode;
  i: Integer;
  a: Integer;
begin
  doc := TXMLDocument.Create;
  rootNode := doc.CreateElement('ShortCuts');
  doc.AppendChild(rootnode);
  for i := 0 to Lists.Count-1 do
    with TActionList(Lists[i]) do
      begin
        listNode := doc.CreateElement(TActionList(Lists[i]).Owner.Name+'.'+TActionList(Lists[i]).Name);
        rootnode.AppendChild(listnode);
        for a := 0 to ActionCount-1 do
          if TAction(Actions[a]).Shortcut <> 0 then
            begin
              shortnode := doc.CreateElement(TAction(Actions[a]).Name);
              shortnode.AttribStrings['ShortCut'] := IntToStr(TAction(Actions[a]).ShortCut);
              listnode.AppendChild(shortnode);
            end;
      end;
   WriteXMLFile(doc,GetConfigDir(copy(Application.Exename,0,length(Application.Exename)-length(ExtractFileExt(Application.Exename))))+Directoryseparator+'shortcuts.xml');
end;

procedure TfShortcuts.SetLanguage;
begin
  Caption := strShortcuts;
  pFunction.Caption := strFunction;
  pShortCut.Caption := strShortcut;
  bOK.caption := strOK;
  bGetKey.Caption := strGetKey;
  bDeleteKey.Caption := strDeleteKey;
end;

initialization
  {$I ushortcuts.lrs}

end.

