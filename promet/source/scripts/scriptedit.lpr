program scriptedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uerror, uScriptEditor, uBaseApplication,uData,uBaseDBInterface,
  pvisualprometapp, uBaseVisualApplication;

{$R *.res}

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
      if Login then
        begin
          fScriptEditor.Execute(ParamStr(Paramcount));
        end;
    end;
end.

