program pscriptedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uerror, uScriptEditor, uBaseApplication,uData,uBaseDBInterface,
  pvisualprometapp, uBaseVisualApplication,uPerson;

{$R *.res}

var
  aPerson: TPerson;
begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  with Application as IBaseDbInterface do
    LoadMandants;
  Application.CreateForm(TfScriptEditor,fScriptEditor);
  fScriptEditor.Hide;
  with Application as IBaseApplication do
    begin
      RestoreConfig;
      if Application.HasOption('database') then
        Login;
      if copy(ParamStr(Paramcount),0,1)<>'-' then
        fScriptEditor.Execute(ParamStr(Paramcount))
      else
        fScriptEditor.Execute('');
    end;
end.

