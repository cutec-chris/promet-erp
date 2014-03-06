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
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB

  aType := GetOptionValue('t','type');
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

      aDoc := TDocument.Create(nil,Data);
      aDoc.SelectByReference(aDocPage.Id.AsVariant);
      aDoc.Open;
      if aDoc.Count>0 then
        begin
          writeln('OCR on '+AInfo.Name);
          Texts := DoOCR(aDoc);
          aText := TStringList.Create;
          for i := 0 to Texts.Count-1 do
            begin
              FixText(TStringList(Texts[i]));
              atext.AddStrings(TStringList(Texts[i]));
            end;
          aDocPage.Edit;
          aDocPage.FieldByName('FULLTEXT').AsString:=aText.Text;
          aDocPage.Post;
          aText.Free;
          for i := 0 to Texts.Count-1 do
            TStringList(Texts[i]).Free;
          Texts.Free;
        end;
      aDoc.Free;

      aDocPage.Free;
      DeleteFileUTF8(aFolder+AInfo.Name);
      FindCloseUTF8(AInfo);
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

