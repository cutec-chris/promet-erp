program portablestarter;
uses
  process,sysutils,Windows;
var
  aProcess: TProcess;
  FindRec: TSearchRec;
  aDir: String;

{$R *.res}

begin
  aDir := ExtractFileDir(ParamStr(0))+DirectorySeparator;
  if FindFirst(aDir+'App'+DirectorySeparator+'promet'+DirectorySeparator+'promet*'+ExtractFileExt(ParamStr(0)), faDirectory, FindRec) = 0 then
    begin
      aProcess := TProcess.Create(nil);
      aProcess.CommandLine:=aDir+'App'+DirectorySeparator+'promet'+DirectorySeparator+findRec.Name+' "--config-path='+aDir+DirectorySeparator+'Data'+DirectorySeparator+'config'+'"';
      aProcess.Options:=aProcess.Options+[poWaitOnExit];
      aProcess.Execute;
    end
  else MessageBox(0,PChar('no App found ! ('+aDir+'App'+DirectorySeparator+'promet'+DirectorySeparator+'promet*'+ExtractFileExt(ParamStr(0))+')'),'Error',0);
  SysUtils.FindClose(FindRec);
end.

