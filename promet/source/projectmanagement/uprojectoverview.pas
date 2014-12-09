unit uprojectoverview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, uprometframesinplace,
  uMainTreeFrame,uIntfStrConsts,uProjects,uBaseDBInterface,uBaseDbClasses;

type
  TfObjectStructureFrame = class(TPrometInplaceFrame)
    procedure FrameEnter(Sender: TObject);
    function FTreeOpen(aEntry: TTreeEntry): Boolean;
  private
    FObject: TBaseDbList;
    { private declarations }
    FTree : TfMainTree;
    procedure SetObject(AValue: TBaseDbList);
  public
    { public declarations }
    property ParentObject : TBaseDbList read FObject write SetObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
uses uData;
procedure TfObjectStructureFrame.FrameEnter(Sender: TObject);
begin
end;

function TfObjectStructureFrame.FTreeOpen(aEntry: TTreeEntry): Boolean;
var
  aDataSet: TBaseDBDataset;
begin
  case aEntry.Typ of
  etCustomer,etEmployee,etArticle,etProject,etProcess:
    begin
      aDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
      aDataSet.Free;
    end;
  end;
end;

procedure TfObjectStructureFrame.SetObject(AValue: TBaseDbList);
var
  Node1: TTreeNode;
begin
  if FObject=AValue then Exit;
  FObject:=AValue;
  FTree.tvMain.Items.Clear;
  with FTree do
    begin
      Node1 := tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := FObject.GetBookmark;
      with FObject.DataSet as IBaseManageDB do
        TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(FObject.GetBookmark));
      TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(FObject.ClassType);
      TTreeEntry(Node1.Data).Text[0] := FObject.Text.AsString+' ('+FObject.Number.AsString+')'+' ['+FObject.Status.AsString+']';
      case FObject.ClassName of
      'TProject':TTreeEntry(Node1.Data).Typ := etProject;
      end;
      Node1.HasChildren:=True;
      Node1.Expanded:=True;
    end;
end;

constructor TfObjectStructureFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTree :=TfMainTree.Create(Self);
  Ftree.Parent := Self;
  FTree.Align := alClient;
  FTree.OnOpen:=@FTreeOpen;
  Caption:=strStructure;
end;

destructor TfObjectStructureFrame.Destroy;
begin
  FTree.Free;
  inherited Destroy;
end;

end.

