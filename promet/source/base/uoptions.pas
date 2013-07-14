{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ButtonPanel,uOptionsFrame;

resourcestring
  strGeneralOptions                             = 'Globale Einstellungen';
  strPersonalOptions                            = 'Persönliche Einstellungen';

type
  { TfOptions }

  TfOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    pFrame: TPanel;
    pRight: TPanel;
    pHeader: TPanel;
    Splitter1: TSplitter;
    tvMain: TTreeView;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tvMainSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FormsList: TList;
  public
    { public declarations }
    procedure RegisterOptionsFrame(aFrame : TOptionsFrame;aName : string;aCategory : string = '');
    destructor Destroy;override;
    procedure SetLanguage;
  end; 

var
  fOptions: TfOptions;

implementation

{$R *.lfm}

{ TfOptions }

procedure TfOptions.tvMainSelectionChanged(Sender: TObject);
begin
  if pFrame.ControlCount > 0 then
    pFrame.RemoveControl(pFrame.Controls[0]);
  pHeader.Visible:=False;
  if tvMain.Selected = nil then exit;
  if tvMain.Selected.Data = nil then
    begin
      if tvMain.Selected.Count > 0 then
        tvMain.Selected := tvMain.Selected.Items[0];
      exit;
    end;
  with TOptionsFrame(tvMain.Selected.Data) do
    begin
      Parent := pFrame;
      Align := alClient;
      Show;
      pHeader.Caption:=tvMain.Selected.Text;
      pHeader.Visible:=True;
      if not InTransaction then StartTransaction;
      pFrame.Caption:='';
    end;
end;

procedure TfOptions.OKButtonClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := tvMain.Items.GetFirstNode;
  while Assigned(aNode) do
    begin
      if Assigned(aNode.Data) then
        if TOptionsFrame(aNode.Data).InTransaction then
          TOptionsFrame(aNode.Data).CommitTransaction;
      aNode := aNode.GetNext;
    end;
end;
procedure TfOptions.CancelButtonClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := tvMain.Items.GetFirstNode;
  while Assigned(aNode) do
    begin
      if Assigned(aNode.Data) then
        if TOptionsFrame(aNode.Data).InTransaction then
          TOptionsFrame(aNode.Data).RollbackTransaction;
      aNode := aNode.GetNext;
    end;
end;
procedure TfOptions.FormCreate(Sender: TObject);
begin
  FormsList := TList.Create;
end;
procedure TfOptions.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FormsList.Count-1 do
    TOptionsFrame(FormsList[i]).Destroy;
  FormsList.Free;
end;
procedure TfOptions.RegisterOptionsFrame(aFrame: TOptionsFrame;aName : string;aCategory : string = '');
var
  aParentNode: TTreeNode;
  aNode: TTreeNode;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfOptions,fOptions);
      Self := fOptions;
    end;
  aParentNode := nil;
  if aCategory <> '' then
    begin
      aParentNode := tvMain.Items.FindTopLvlNode(aCategory);
      if not Assigned(aParentNode) then
        aParentNode := tvMain.Items.Add(nil,aCategory);
    end;
  aNode := tvMain.Items.AddChildObject(aParentNode,aName,aFrame);
  FormsList.Add(aFrame);
  tvMain.FullExpand;
end;
destructor TfOptions.Destroy;
begin
  inherited Destroy;
end;
procedure TfOptions.SetLanguage;
begin

end;

end.
