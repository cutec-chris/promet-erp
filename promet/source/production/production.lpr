program production;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,sysutils,Utils,process, // this includes the LCL widgetset
  Forms, uerror, general,
umain, pvisualprometapp, uBaseVisualApplication, uselectorder;

{$R *.res}

var
  PlainName : string;
  FindRec: TSearchRec;
  NewName: TFilename;
  aProcess: TProcess;
  i: Integer;

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  {$ifdef WINDOWS}
  PlainName := copy(Application.Exename,0,length(Application.Exename)-length(ExtractFileExt(Application.Exename)));
  IF FindFirst(UniToSys(PlainName+'.*'+ExtractFileExt(Application.Exename)), faDirectory, FindRec) = 0 THEN
    REPEAT
      IF ((FindRec.Name <> '.') AND (FindRec.Name <> '..')) THEN
        begin
          if FindRec.Name <> ExtractFileName(Application.ExeName) then
            NewName := FindRec.Name;
        end;
    UNTIL FindNext(FindRec) <> 0;
  FindClose(FindRec);
  if (NewName <> ExtractFileName(Application.ExeName)) and (NewName<>'') and (pos('_',NewName)=0) then
    begin
      aProcess := Tprocess.Create(nil);
      aProcess.Options:=[];
      aProcess.CommandLine:=AppendPathDelim(Application.Location)+NewName;
      for i :=  1 to Paramcount do
        aProcess.CommandLine := aProcess.CommandLine+' '+ParamStr(i);
      aProcess.Execute;
      aProcess.Free;
      Application.Terminate;
      exit;
    end;
  {$endif}
  Application.Initialize;
  fMain.DoCreate;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfSelectOrder, fSelectOrder);
  Application.Run;
end.

