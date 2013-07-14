program checkout;
{$mode objfpc}{$H+}
uses
 {$IFDEF UNIX}
 cthreads,
 {$ENDIF}
 Classes, SysUtils, CustApp,
 Interfaces
 { you can add units after this }, db, Utils, general, FileUtil, Forms,
 uData, uBaseCustomApplication, uOrder, uMasterdata,
 uPerson, pcmdprometapp,uDocuments,uBaseApplication,uBaseDBInterface,
 ProcessUtils;

type
 TCheckout = class(TBaseCustomApplication)
 private
 protected
   function GetSingleInstance : Boolean; override;
   procedure DoRun; override;
   procedure WriteMessage(s : string);
   procedure WriteError(s : string);
 public
   constructor Create(TheOwner: TComponent); override;
 end;
function IsDirectoryEmpty(const directory : string) : boolean;
var
 searchRec :TSearchRec;
begin
 try
   result := (FindFirst(directory+'\*.*', faAnyFile, searchRec) = 0) AND
             (FindNext(searchRec) = 0) AND
             (FindNext(searchRec) <> 0) ;
 finally
   FindClose(searchRec) ;
 end;
end;
function TCheckout.GetSingleInstance: Boolean;
begin
 Result:=False;
end;
procedure TCheckout.DoRun;
var
  aConnection: TComponent;
  aDocument: TDocument;
  aFileName: String;
  Failed: Boolean;
  res: String;
  procedure DoExit;
  begin
   aDocument.Free;
   aConnection.Free;
   // stop program loop
   Terminate;
  end;
begin
 Login;
 aConnection := Data.GetNewConnection;
 aDocument := TDocument.Create(Self,Data,aConnection);
 aDocument.SelectByNumber(36038085); //SelectByLink(GetOptionValue('l','link'));
 aDocument.Open;
 if aDocument.Count > 0 then
   begin
     with aDocument.DataSet do
       begin
         First;
         aFileName := aDocument.FileName;
         Last;
         while not BOF do
           begin
             if FieldByName('REVISION').AsInteger <= StrToIntDef(GetOptionValue('r','revision'),0) then break
             else Prior;
           end;
         while not BOF do
           begin
             if FieldByName('FULL').AsString = 'Y' then break
             else Prior;
           end;
         if FieldByName('FULL').AsString <> 'Y' then
           begin
             WriteError(rsNoFullDocumentAvalible);
             DoExit;
             exit;
           end;
         if not FileExistsUTF8(aFileName) then
           begin
             with BaseApplication as IBaseDbInterface do
               Data.BlobFieldToFile(aDocument.DataSet,'DOCUMENT',aFileName);
           end;
         WriteMessage(Format('File %s with Revision %s -> OK',[aFileName,FieldByName('REVISION').AsString]));
         while not EOF do //<TargetRevision
           begin
             Next;
             if FieldByName('REVISION').AsInteger > StrToIntDef(GetOptionValue('r','revision'),0) then break;
             with BaseApplication as IBaseDbInterface do
               Data.BlobFieldToFile(aDocument.DataSet,'DOCUMENT','diff.patch');
             Failed := True;
             {$IFDEF WINDOWS}
             res := ExecProcessEx('"'+AppendPathDelim(AppendPathDelim(ExtractFilePath(Paramstr(0)))+'tools')+'bspatch'+ExtractFileExt(ParamStr(0))+'" "'+aFilename+'" "'+aFilename+'.patched" "diff.patch"','');
             {$ELSE}
             res := ExecProcessEx('"'+'bspatch'+ExtractFileExt(ParamStr(0))+'" "'+aFilename+'" "'+aFilename+'.patched" "diff.patch"','');
             {$ENDIF}
             DeleteFile('diff.patch');
             Failed := not FileExists(aFilename+'.patched');
             if not Failed then
               begin
                 DeleteFileUTF8(aFilename);
                 failed := not RenameFileUTF8(aFilename+'.patched',aFileName);
               end;
             if Failed then
               begin
                 WriteMessage(Format('Patching to Revision %s -> failed.',[FieldByName('REVISION').AsString]));
                 break;
               end
             else
               WriteMessage(Format('Patching to Revision %s -> OK',[FieldByName('REVISION').AsString]));
           end;
       end;
   end
 else WriteError(rsDocumentNotFound);
 DoExit;
end;
procedure TCheckout.WriteMessage(s: string);
begin
 writeln(s);
end;
procedure TCheckout.WriteError(s: string);
begin
 writeln('ERROR:'+s);
end;
constructor TCheckout.Create(TheOwner: TComponent);
begin
 inherited Create(TheOwner);
 StopOnException:=True;
end;
var
 Application: TCheckout;
begin
 Application:=TCheckout.Create(nil);
 Application.Title:='Checkout';
 Application.Run;
 Application.Free;
end.

