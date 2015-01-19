program cmdwizardmandant;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp ,uBaseCustomApplication,
  uBaseDBInterface,uBaseApplication,Utils
  { you can add units after this };

resourcestring
  strParametersMissing           = 'A parameter is missing !';
  strmandantnotFound             = 'Mandant not found !';

type

  { TCmdWizard }

  TCmdWizard = class(TBaseCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCmdWizard }

procedure TCmdWizard.DoRun;
var
  ErrorMsg: String;
  tmp: String;
  mSettings: TStringList;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hbdns','help'+#13+'create'+#13+'delete'+#13+'name'+#13+'settings');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  StopOnException := True;
  with Self as IBaseDbInterface do
    begin
      if not LoadMandants then
        begin
          ShowException(Exception.Create(strFailedtoLoadMandants));
          Terminate;
          exit;
        end;
    end;

  if HasOption('b','create') then
    begin
      if (not HasOption('s','settings'))
      or (not HasOption('n','name')) then
        begin
          ShowException(Exception.Create(strParametersMissing));
          Terminate;
          exit;
        end;
      with Self as IBaseDbInterface do
        begin
          mSettings := TStringList.Create;
          tmp:=GetOptionValue('s','settings');
          mSettings.Add(copy(tmp,0,pos(':',tmp)-1));
          tmp := copy(tmp,pos(':',tmp)+1,length(tmp));
          mSettings.Add(tmp);
          mSettings.SaveToFile(AppendPathDelim(MandantPath)+GetOptionValue('n','name')+MandantExtension);
          mSettings.Free;
        end;
    end
  else if HasOption('d','delete') then
    begin
      if (not HasOption('n','name')) then
        begin
          ShowException(Exception.Create(strParametersMissing));
          Terminate;
          exit;
        end;
      with Self as IBaseDbInterface do
        begin
          with BaseApplication as IBaseDBInterface do
            if FileExists(AppendPathDelim(MandantPath)+GetOptionValue('n','name')+MandantExtension) then
              DeleteFile(AppendPathDelim(MandantPath)+GetOptionValue('n','name')+MandantExtension);
        end;
    end
  else WriteHelp;

  DoExit;
  // stop program loop
  Terminate;
end;

constructor TCmdWizard.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCmdWizard.Destroy;
begin
  inherited Destroy;
end;

procedure TCmdWizard.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExtractFileName(ExeName),' -h');
  writeln('  --create (-b) creates an new Mandant, requires --name,--settings');
  writeln('  --delete (-d) deletes an existing Mandant, requires --name');
end;

var
  Application: TCmdWizard;
begin
  Application:=TCmdWizard.Create(nil);
  Application.Title:='CmdWizardMandant';
  Application.Run;
  Application.Free;
end.
