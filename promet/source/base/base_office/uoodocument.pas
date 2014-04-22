unit uOODocument; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, zipper,
  XMLRead, DOM, Dialogs, XMLWrite, FileUtil;

type

  { TODFDocument }

  TODFDocument = class(TList)
  private
    Content:txmldocument;
    FFilename : string;
    function GetValues(idx : Integer): string;
    procedure SetValues(idx : Integer; const AValue: string);
  public
    constructor Create(Filename : string);
    procedure Save;
    destructor Destroy;override;
    property Values[idx : Integer] : string read GetValues write SetValues;
  end;

implementation

{ TODFDocument }

function TODFDocument.GetValues(idx : Integer): string;
begin
  if idx < Count then
    Result := TDOMNode(Items[idx]).TextContent;
end;

procedure TODFDocument.SetValues(idx : Integer; const AValue: string);
var
  Parent: TDOMElement;
  Oldtext: WideString;
begin
  if idx < Count then
    begin
      Oldtext := TDOMNode(Items[idx]).TextContent;
      Parent := TDOMElement(TDOMNode(Items[idx]).ParentNode);
      TDOMNode(Items[idx]).TextContent := AValue;
      TDOMElement(TDOMNode(Items[idx]).ParentNode).SetAttribute('prom_modified','1');
    end;
end;

constructor TODFDocument.Create(Filename: string);
var
  ms : TMemoryStream;
  UnZip : TUnZipper;
  iNode: TDOMNode;
  a: Integer;
  i: Integer;

  procedure ScanChilds(iNode : TDOMNode);
  var
    i: Integer;
  begin
    for i := 0 to iNode.ChildNodes.Count-1 do
      begin
        if iNode.NodeName = 'text:placeholder' then
          Add(iNode);
//        if iNode.NodeName = 'text:database-display' then
//          Add(iNode);
//        if iNode.NodeName = 'text:variable-set' then
//          Add(iNode);
        if iNode.ChildNodes[i].ChildNodes.Count > 0 then
          ScanChilds(iNode.ChildNodes[i]);
      end;
  end;
begin
  inherited Create;
  UnZip := TUnZipper.Create;
  Unzip.FileName := Filename;
  for i := 0 to Unzip.Entries.Count-1 do
    if Unzip.Entries.Entries[i].DisplayName = 'content.xml' then
      begin
        ReadXMLFile(Content,Unzip.Entries.Entries[i].Stream);
        break;
      end;
  UnZip.Free;
  iNode := Content.FirstChild.FindNode('office:body');
  iNode := iNode.FirstChild;
  ScanChilds(iNode);
end;

procedure TODFDocument.Save;
var
  Zip: TZipper;
  s: String;
  ms: TMemoryStream;
  iNode: TDOMNode;

  procedure ScanChilds(iNode : TDOMNode);
  var
    i: Integer;
    newText: String;
  begin
    if TDOMElement(iNode).hasAttribute('prom_modified') then
      begin
        newText := '';
        for i := 0 to TDOMElement(iNode).ChildNodes.Count-1 do
          NewText := NewText+' '+iNode.ChildNodes[i].TextContent;
        iNode.TextContent:=NewText;
        TDOMElement(iNode).RemoveAttribute('prom_modified');
      end;
    for i := 0 to iNode.ChildNodes.Count-1 do
      begin
        if iNode.ChildNodes[i].ChildNodes.Count > 0 then
          ScanChilds(iNode.ChildNodes[i]);
      end;
  end;
begin
  iNode := Content.FirstChild.FindNode('office:body');
  iNode := iNode.FirstChild;
  ScanChilds(iNode);
  Zip := TZipper.Create;
  Zip.FileName := FFilename;
//  Zip.StoreOptions:=[soReplace];
//  Zip.ForceType:=True;
//  Zip.ArchiveType:=atZip;
//  Zip.OpenArchive(FFilename);
//  Zip.ForceType:=True;
//  Zip.ArchiveType:=atZip;
  ms := TMemoryStream.Create;
  WriteXMLFile(Content,ms);
  ms.Position:=0;
  Zip.Entries.AddFileEntry(ms,'content.xml');
  ms.free;
  Zip.ZipAllFiles;
  Zip.Free;
end;

destructor TODFDocument.Destroy;
begin
  Content.Free;
  inherited Destroy;
end;

end.

