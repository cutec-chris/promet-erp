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
Created 10.02.2014
*******************************************************************************}
 program import_document;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this },db, Utils, FileUtil, Forms, uData,
  uIntfStrConsts, pcmdprometapp, uBaseCustomApplication, uBaseApplication,
  uDocuments, uBaseDocPages,uOCR, pocr,Graphics;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
    procedure PrometCmdAppException(Sender: TObject; E: Exception);
  private
    mailaccounts : string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.PrometCmdAppException(Sender: TObject; E: Exception);
begin
  writeln('Exception'+e.Message);
end;

procedure PrometCmdApp.DoRun;
var
  aType: String;
  aDocPage: TDocPages;
  Texts: TList;
  aPic: TPicture;
  aDoc: TDocument;
  aFullStream: TMemoryStream;
  AInfo: TSearchRec;
  aText: TStringList;
  aUnpaper: TUnPaperProcess;
  i: Integer;
  ss: TStringStream;
  aFolder: String;
  aDocs: TDocPages;

  procedure DoOCROnActualDoc;
  var
    a: Integer;
  begin
    aDoc := TDocument.Create(nil,Data);
    aDoc.SelectByReference(aDocPage.Id.AsVariant);
    aDoc.Open;
    if aDoc.Count>0 then
      begin
        writeln('OCR on '+aDocPage.FieldByName('NAME').AsString);
        try
          Texts := DoOCR(aDoc);
          aText := TStringList.Create;
          for a := 0 to Texts.Count-1 do
            begin
              FixText(TStringList(Texts[a]));
              atext.AddStrings(TStringList(Texts[a]));
            end;
          aDocPage.Edit;
          aDocPage.FieldByName('FULLTEXT').AsString:=aText.Text;
          aDocPage.Post;
          aDoc.Edit;
          aDoc.FieldByName('FULLTEXT').AsString:=aText.Text;
          aDoc.Post;
        except
          on e : Exception do
            writeln('Error:'+e.Message);
        end;
        aText.Free;
        for a := 0 to Texts.Count-1 do
          TStringList(Texts[a]).Free;
        Texts.Free;
      end;
    aDoc.Free;
  end;

begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB

  aType := GetOptionValue('t','type');
  if HasOption('doocr') then
    begin
      aDocPage := TDocPages.Create(nil,Data);
      aDocPage.Typ:='D';
      aDocPage.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('D')+' AND '+Data.ProcessTerm(Data.QuoteField('FULLTEXT')+'='+Data.QuoteValue('')));

      while not aDocPage.EOF do
        begin
          DoOCROnActualDoc;
          break;
          aDocPage.Next;
        end;

      aDocPage.Free;
    end
  else
    begin
      aFolder :=GetOptionValue('f','folder');
      if aType = '' then aType := 'D';
      if aFolder<>'' then
        aFolder := AppendPathDelim(aFolder);
      while FindFirstUTF8(aFolder+'*.jpg',faAnyFile,AInfo)=0 do
        begin
          writeln('importing File '+AInfo.Name);
          try
            aDocPage := TDocPages.Create(nil,Data);
            aDocPage.AddFromFile(aFolder+AInfo.Name);
            aDocPage.Edit;
            aDocPage.FieldByName('TYPE').AsString:=aType;
            aDocPage.Post;
          except
            on e : Exception do
              begin
                writeln('Error:'+e.Message);
                Application.Terminate;
                exit;
              end;
          end;

          DoOCRonActualDoc;

          aDocPage.Free;
          DeleteFileUTF8(aFolder+AInfo.Name);
          FindCloseUTF8(AInfo);
        end;
    end;

  // stop program loop
  Terminate;
end;

constructor PrometCmdApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  OnException:=@PrometCmdAppException;
end;

destructor PrometCmdApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: PrometCmdApp;

begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

