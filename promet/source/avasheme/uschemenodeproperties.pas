unit uSchemenodeproperties;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  usimplegraph, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, CheckLst, ButtonPanel;

type

  { TfNodeProperties }

  TfNodeProperties = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
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

//uses MarginsProp;

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
          Text := NodeText.Lines.Text;
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

