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
Created 07.12.2013
*******************************************************************************}
unit udetailview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs,
  uBaseDbClasses,Utils,wikitohtml,uDocuments,variants,db;

type

  { TfDetailView }

  TfDetailView = class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    procedure TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  private
    { private declarations }
    FDataSet : TBaseDBDataset;
  public
    { public declarations }
    function Execute(aDataSet: TBaseHistory): Boolean;
  end;
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

var
  fDetailView: TfDetailView;

implementation
uses uData,LCLIntf;
{$R *.lfm}

{ TfDetailView }

procedure TfDetailView.TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aDocument: TDocument;
  ms: TMemoryStream;
begin
  aDocument := TDocument.Create(nil);
  aDocument.Select(FDataSet.Id.AsVariant,'H',0);
  aDocument.Open;
  if aDocument.DataSet.RecordCount > 0 then
    begin
      if not aDocument.DataSet.Locate('NAME',VarArrayOf([copy(ExtractFileName(URL),0,rpos('.',ExtractFileName(URL))-1)]),[]) then
        aDocument.DataSet.Locate('NAME',VarArrayOf([copy(ExtractFileName(URL),0,rpos('.',ExtractFileName(URL))-1)]),[loPartialKey]);
      ms := TMemoryStream.Create;
      Data.BlobFieldToStream(aDocument.DataSet,'DOCUMENT',ms);
      ms.Position := 0;
      Picture := TPicture.Create;
      try
        Picture.LoadFromStreamWithFileExt(ms,ExtractFileExt(URL));
        Picture.Graphic.Transparent:=False;
      except
      end;
      ms.Free;
    end;
  aDocument.Free;
end;

procedure TfDetailView.IpHtmlPanel1HotClick(Sender: TObject);
begin
  if IpHtmlPanel1.HotNode is TIpHtmlNodeA then
    if ((Pos('://', TIpHtmlNodeA(IpHtmlPanel1.HotNode).HRef) > 0) or (pos('www',lowercase(TIpHtmlNodeA(IpHtmlPanel1.HotNode).HRef)) > 0)) then
      OpenURL(TIpHtmlNodeA(IpHtmlPanel1.HotNode).HRef);
end;

procedure TfDetailView.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=True;
end;

function TfDetailView.Execute(aDataSet: TBaseHistory) : Boolean;
var
  aHTML: TIpHtml;
  ss: TStringStream;
  function BBCodeToHTML(aStr : string) : string;
  begin
    Result := aStr;
    Result := StringReplace(HTMLEncode(Result),#10,'<br>',[rfReplaceAll]);
    Result := '<html><body>'+WikiText2HTML(result)+'</body></html>';
  end;

begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDetailView,fDetailView);
      Self := fDetailView;
    end;
  FDataSet := aDataSet;
  aHTML := TSimpleIpHtml.Create;
  TSimpleIpHtml(aHTML).OnGetImageX:=@TSimpleIpHtmlGetImageX;
  ss := TStringStream.Create(BBCodeToHTML(aDataSet.FieldByName('ACTION').AsString));
  aHTML.LoadFromStream(ss);
  ss.Free;
  IpHtmlPanel1.SetHtml(aHTML);
  Show;
end;

end.

