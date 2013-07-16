program prometerp;

{$mode objfpc}{$H+}

uses
//  heaptrc,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, lazreport, turbopoweripro, uMain, pvisualprometapp,FileUtil,
  Process
  { you can add units after this }
  ,uBaseVisualApplication, pphones, richmemopackage, zvdatetimectrls, general;

{$R *.res}

var
  PlainName: String;
  FindRec: TSearchRec;
  NewName: TFilename;
  aProcess: TProcess;
begin
  Application.Free;
  PlainName := copy(Application.Exename,0,length(Application.Exename)-length(ExtractFileExt(Application.Exename)));
  IF FindFirstUTF8(PlainName+'*'+ExtractFileExt(Application.Exename), faDirectory, FindRec) = 0 THEN
    REPEAT
      IF ((FindRec.Name <> '.') AND (FindRec.Name <> '..')) THEN
        begin
          if FindRec.Name <> ExtractFileName(Application.ExeName) then
            NewName := FindRec.Name;
        end;
    UNTIL FindNextUTF8(FindRec) <> 0;
  FindCloseUTF8(FindRec);
  if (NewName <> ExtractFileName(Application.ExeName)) and (NewName<>'') then
    begin
      aProcess := Tprocess.Create(nil);
      aProcess.Options:=[];
      aProcess.CommandLine:=AppendPathDelim(Application.Location)+NewName;
      aProcess.Execute;
      aProcess.Free;
      halt(0);
      exit;
    end;
  Application := TBaseVisualApplication.Create(nil);
  Application.Title:='Promet-ERP';
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
