unit uSchemenodeproperties;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  usimplegraph, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, CheckLst, ButtonPanel,
  EditBtn, Buttons, ActnList;

type

  { TfNodeProperties }

  TfNodeProperties = class(TForm)
    acPasteFromLink: TAction;
    ActionList1: TActionList;
    ButtonPanel1: TButtonPanel;
    eLink: TEditButton;
    Label1: TLabel;
    Label4: TLabel;
    NodeShape: TRadioGroup;
    Colors: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    NodeBodyColor: TPanel;
    NodeBorderColor: TPanel;
    btnChangeFont: TButton;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    NodeText: TMemo;
    GroupBox1: TGroupBox;
    OpenPictureDialog: TOpenPictureDialog;
    cbAlignment: TComboBox;
    BodyColor: TShape;
    BorderColor: TShape;
    cbLayout: TComboBox;
    edtMargin: TEdit;
    SpeedButton1: TSpeedButton;
    UpDownMargin: TUpDown;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Styles: TGroupBox;
    FillStyle: TComboBox;
    Label8: TLabel;
    BorderStyle: TComboBox;
    Label9: TLabel;
    GroupBox3: TGroupBox;
    btnChangBkgnd: TButton;
    btnClearBackground: TButton;
    btnBackgroundMargins: TButton;
    procedure acPasteFromLinkExecute(Sender: TObject);
    procedure eLinkButtonClick(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    procedure NodeBodyColorClick(Sender: TObject);
    procedure NodeBorderColorClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure btnChangBkgndClick(Sender: TObject);
    procedure btnClearBackgroundClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBackgroundMarginsClick(Sender: TObject);
  private
    Backgnd: Integer;
    S: TEvsSimpleGraph;
    N: TEvsGraphObjectList; // w2m - for apply button
    MarginRect: TRect;
    procedure ListRegistredNodeClasses;
    procedure ApplyChanges;
    //procedure SetObjectOptions(Value: TEvsGraphObjectOptions);
    //function GetObjectOptions: TEvsGraphObjectOptions;
    //procedure SetNodeOptions(Value: TEvsGraphNodeOptions);
    //function GetNodeOptions: TEvsGraphNodeOptions;
  public
    class function Execute(Nodes: TEvsGraphObjectList): Boolean;
  end;

function PrettyNodeClassName(const AClassName: string): string;

implementation

uses uSearch,Clipbrd,uBaseVisualApplication;

{$R *.lfm}

function PrettyNodeClassName(const AClassName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 2 to Length(AClassName) do
  begin
    if(UpCase(AClassName[I]) = AClassName[I]) and (Result <> '') then
      Result := Result + ' ' + AClassName[I]
    else
      Result := Result + AClassName[I]
  end;
  Result := StringReplace(Result, ' Node', '', []);
end;

{ TfNodeProperties }

class function TfNodeProperties.Execute(Nodes: TEvsGraphObjectList): Boolean;
begin
  Result := False;
  with Create(Application) do
  try
    N := Nodes;
    ListRegistredNodeClasses;
    with TEvsGraphNode(Nodes[0]) do
    begin
      S := Owner;
      case Alignment of
        taLeftJustify: cbAlignment.ItemIndex := 0;
        taCenter: cbAlignment.ItemIndex := 1;
        taRightJustify: cbAlignment.ItemIndex := 2;
      end;
      case Layout of
        tlTop: cbLayout.ItemIndex := 0;
        tlCenter: cbLayout.ItemIndex := 1;
        tlBottom: cbLayout.ItemIndex := 2;
      end;
      UpDownMargin.Position := Margin;
      eLink.Text := '';
      if pos('[Link:',Text)>0 then
        begin
          NodeText.Lines.Text := copy(Text,0,pos('[Link:',Text)-1);
          eLink.Text := copy(Text,pos('[Link:',Text)+6,length(Text));
          eLink.Text := copy(eLink.Text,0,length(eLink.Text)-1);
        end
      else
        NodeText.Lines.Text := Text;
      if Nodes.Count = 1 then
        NodeShape.ItemIndex := NodeShape.Items.IndexOfObject(TObject(ClassType))
      else
        NodeShape.ItemIndex := -1;
      BodyColor.Brush.Color := Brush.Color;
      BorderColor.Brush.Color := Pen.Color;
      FillStyle.ItemIndex := Ord(Brush.Style);
      BorderStyle.ItemIndex := Ord(Pen.Style);
      FontDialog.Font := Font;
      MarginRect := BackgroundMargins;
      //SetObjectOptions(Options);
      //SetNodeOptions(NodeOptions);
    end;
    if ShowModal = mrOK then
    begin
      ApplyChanges;
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TfNodeProperties.ListRegistredNodeClasses;
var
  I: Integer;
  NodeClass: TEvsGraphNodeClass;
begin
  for I := 0 to TEvsSimpleGraph.NodeClassCount - 1 do
  begin
    NodeClass := TEvsSimpleGraph.NodeClasses(I);
    NodeShape.Items.AddObject(PrettyNodeClassName(NodeClass.ClassName),
      TObject(NodeClass));
  end;
end;

procedure TfNodeProperties.ApplyChanges;
var
  I: Integer;
begin
  S.BeginUpdate;
  try
    for I := 0 to N.Count - 1 do
      with TEvsGraphNode(N[I]) do
      begin
        BeginUpdate;
        try
          case cbAlignment.ItemIndex of
            0: Alignment := taLeftJustify;
            1: Alignment := taCenter;
            2: Alignment := taRightJustify;
          end;
          case cbLayout.ItemIndex of
            0: Layout := tlTop;
            1: Layout := tlCenter;
            2: Layout := tlBottom;
          end;
          Margin := UpDownMargin.Position;
          if NodeText.Lines.Text <> N[0].Text then
            Text := NodeText.Lines.Text;
          if eLink.Text<>'' then
            Text := Text+'[Link:'+eLink.Text+']';
          Brush.Color := BodyColor.Brush.Color;
          Pen.Color := BorderColor.Brush.Color;
          Brush.Style := TBrushStyle(FillStyle.ItemIndex);
          Pen.Style := TPenStyle(BorderStyle.ItemIndex);
          Font := FontDialog.Font;
          if Backgnd = 1 then
            Background.LoadFromFile(OpenPictureDialog.FileName)
          else if Backgnd = 2 then
            Background.Graphic := nil;
          BackgroundMargins := MarginRect;
          //Options := GetObjectOptions;
          //NodeOptions := GetNodeOptions;
        finally
          EndUpdate;
        end;
        if NodeShape.ItemIndex >= 0 then
          ConvertTo(TEvsSimpleGraph.NodeClasses(NodeShape.ItemIndex));
      end;
  finally
    S.EndUpdate;
    Backgnd := 0;
  end;
end;

procedure TfNodeProperties.NodeBodyColorClick(Sender: TObject);
begin
  ColorDialog.Color := BodyColor.Brush.Color;
  if ColorDialog.Execute then
    BodyColor.Brush.Color := ColorDialog.Color;
end;

procedure TfNodeProperties.eLinkButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=fSearchOpenItem;
  fSearch.Execute(True,'SHEME','');
end;

procedure TfNodeProperties.acPasteFromLinkExecute(Sender: TObject);
var
  Stream: TStringStream;
begin
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          eLink.Text:=Stream.DataString;
        end;
      Stream.Free;
    end;
end;

function TfNodeProperties.fSearchOpenItem(aLink: string): Boolean;
begin
  eLink.Text:=aLink;
end;

procedure TfNodeProperties.NodeBorderColorClick(Sender: TObject);
begin
  ColorDialog.Color := BorderColor.Brush.Color;
  if ColorDialog.Execute then
    BorderColor.Brush.Color := ColorDialog.Color;
end;

procedure TfNodeProperties.btnChangeFontClick(Sender: TObject);
begin
  FontDialog.Execute;
end;

procedure TfNodeProperties.btnChangBkgndClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    Backgnd := 1;
end;

procedure TfNodeProperties.btnClearBackgroundClick(Sender: TObject);
begin
  Backgnd := 2;
end;

procedure TfNodeProperties.btnApplyClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TfNodeProperties.FormCreate(Sender: TObject);
begin
  SetBounds(Screen.Width - Width - 30, 50, Width, Height);
end;
{
function TfNodeProperties.GetObjectOptions: TEvsGraphObjectOptions;
var
  Option: TEvsGraphObjectOption;
begin
  Result := [];
  for Option := Low(TEvsGraphObjectOption) to High(TEvsGraphObjectOption) do
    if AllOptions.Checked[Ord(Option)] then
      Include(Result, Option);
end;

procedure TfNodeProperties.SetObjectOptions(Value: TEvsGraphObjectOptions);
var
  Option: TEvsGraphObjectOption;
begin
  for Option := Low(TEvsGraphObjectOption) to High(TEvsGraphObjectOption) do
    AllOptions.Checked[Ord(Option)] := Option in Value;
end;
function TfNodeProperties.GetNodeOptions: TEvsGraphNodeOptions;
var
  Option: TEvsGraphNodeOption;
begin
  Result := [];
  for Option := Low(TEvsGraphNodeOption) to High(TEvsGraphNodeOption) do
    if AllOptions.Checked[Ord(Option) + Ord(High(TEvsGraphObjectOption)) + 1] then
      Include(Result, Option);
end;

procedure TfNodeProperties.SetNodeOptions(Value: TEvsGraphNodeOptions);
var
  Option: TEvsGraphNodeOption;
begin
  for Option := Low(TEvsGraphNodeOption) to High(TEvsGraphNodeOption) do
    AllOptions.Checked[Ord(Option) + Ord(High(TEvsGraphObjectOption)) + 1] := Option in Value;
end;
}

procedure TfNodeProperties.btnBackgroundMarginsClick(Sender: TObject);
begin
  //TMarginDialog.Execute(MarginRect);
end;

end.

