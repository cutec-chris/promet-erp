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
  uBaseDbClasses,Utils,wikitohtml,uDocuments,variants;

type

  { TfDetailView }

  TfDetailView = class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
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
uses uData;
{$R *.lfm}

{ TfDetailView }

procedure TfDetailView.TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aDocument: TDocument;
  ms: TMemoryStream;
begin
  aDocument := TDocument.Create(nil,data);
  aDocument.Select(FDataSet.Id.AsVariant,'H',0);
  aDocument.Open;
  if aDocument.DataSet.Locate('NAME',VarArrayOf([copy(ExtractFileName(URL),0,rpos('.',ExtractFileName(URL))-1)]),[]) then
    begin
      if aDocument.DataSet.RecordCount > 0 then
        begin
          ms := TMemoryStream.Create;
          Data.BlobFieldToStream(aDocument.DataSet,'DOCUMENT',ms);
          ms.Position := 0;
          Picture := TPicture.Create;
          try
            Picture.LoadFromStreamWithFileExt(ms,ExtractFileExt(URL));
          except
          end;
          ms.Free;
        end;
    end;
  aDocument.Free;
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
  Result := ShowModal=mrOK;
end;

end.

