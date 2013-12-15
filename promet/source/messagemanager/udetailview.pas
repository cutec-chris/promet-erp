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
  uBaseDbClasses,Utils,wikitohtml;

type

  { TfDetailView }

  TfDetailView = class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aDataSet: TBaseHistory): Boolean;
  end;

var
  fDetailView: TfDetailView;

implementation

{$R *.lfm}

{ TfDetailView }

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
  aHTML := TIpHtml.Create;
  ss := TStringStream.Create(BBCodeToHTML(aDataSet.FieldByName('ACTION').AsString));
  aHTML.LoadFromStream(ss);
  ss.Free;
  IpHtmlPanel1.SetHtml(aHTML);
  Result := ShowModal=mrOK;
end;

end.

