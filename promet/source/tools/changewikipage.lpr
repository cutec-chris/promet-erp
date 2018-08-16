program changewikipage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp ,uBaseCustomApplication,
  uBaseDBInterface,uWiki,uData,uBaseApplication
  { you can add units after this };

resourcestring
  strParametersMissing           = 'A parameter is missing !';
  strmandantnotFound             = 'Mandant not found !';

type

  { TChangeWikiPage }

  TChangeWikiPage = class(TBaseCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TChangeWikiPage }

procedure TChangeWikiPage.DoRun;
var
  ErrorMsg: String;
  tmp: String;
  aWiki: TWikiList;
  aPage: TStringList;
begin
  // parse parameters
  if HasOption('h','help') or (ParamCount < 2) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  Info('changewikipage starting...');
  with BaseApplication as IBaseDbInterface do
    begin
      Info('loading mandants...');
      if not LoadMandants then
        begin
          Error(strFailedtoLoadMandants);
          raise Exception.Create(strFailedtoLoadMandants);
          Terminate;
        end;
      if not HasOption('m','mandant') then
        begin
          Error(strMandantnotSelected);
          raise Exception.Create(strMandantnotSelected);
          Terminate;
        end;
      Info('login...');
      if not DBLogin(GetOptionValue('m','mandant'),'',False,False) then
        begin
          Error(strLoginFailed+' '+LastError);
          raise Exception.Create(strLoginFailed+' '+LastError);
          Terminate;
        end;
      uData.DataM := Data;
    end;
  Info('changewikipage login successful');
  aWiki := TWikiList.Create(nil);
  aPage := TStringList.Create;
  try
    aPage.LoadFromFile(Params[ParamCount]);
  except
    Writeln('File not found "'+Params[ParamCount]+'" !');
    Terminate;
    Exit;
  end;
  if aWiki.FindWikiPage(Params[ParamCount-1],True) then
    begin
      if not aWiki.CanEdit then
        aWiki.DataSet.Edit;
      aWiki.DataSet.FieldByName('DATA').AsString := aPage.Text;
      aWiki.DataSet.FieldByName('TIMESTAMPD').AsDateTime:=Now();
      aWiki.DataSet.Post;
      Writeln('Wiki Page "'+Params[ParamCount-1]+'" changed !');
    end
  else
    Writeln('Wiki Page "'+Params[ParamCount-1]+'" not found !');
  aPage.Free;
  aWiki.Free;
  DoExit;
  // stop program loop
  Terminate;
end;

constructor TChangeWikiPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TChangeWikiPage.Destroy;
begin
  inherited Destroy;
end;

procedure TChangeWikiPage.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExtractFileName(ExeName),' Page File');
end;

var
  Application: TChangeWikiPage;
begin
  Application:=TChangeWikiPage.Create(nil);
  Application.Run;
  Application.Free;
end.

