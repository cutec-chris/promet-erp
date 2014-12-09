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
Created 22.10.2013
*******************************************************************************}
unit ucameraimport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ButtonPanel, process, UTF8Process,ProcessUtils,uIntfStrConsts,
  umanagedocframe;

type

  { TfCameraimport }

  TfCameraimport = class(TForm)
    Button1: TButton;
    bImport: TButton;
    Button2: TButton;
    ButtonPanel1: TButtonPanel;
    cbCamera: TComboBox;
    cbDelete: TCheckBox;
    Label1: TLabel;
    lvPhotos: TListView;
    procedure bImportClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbCameraSelect(Sender: TObject);
  private
    { private declarations }
    FDoc: TfManageDocFrame;
  public
    { public declarations }
    function ImportAvalibe : Boolean;
    function Execute(aDocMan : TfManageDocFrame;aTyp : string) : Boolean;
  end;

var
  fCameraimport: TfCameraimport;

implementation
uses uBaseDocPages,Utils,uData,uBaseDbClasses,usimpleprocess,uBaseApplication;
{$R *.lfm}

{ TfCameraimport }

procedure TfCameraimport.cbCameraSelect(Sender: TObject);
var
  aProcess: TProcessUTF8;
  sl: TStringList;
  i: Integer;
  aItem: TListItem;
  tmp: String;
begin
  aProcess := TProcessUTF8.Create(Self);
  sl := TStringList.Create;
  try
    aProcess.CommandLine:='gphoto2 -L';
    aProcess.Options:=[poUsePipes,poWaitOnExit];
    aProcess.Execute;
    sl.LoadFromStream(aProcess.Output);
  finally
    aProcess.Free;
  end;
  i := 0;
  lvPhotos.Clear;
  while i < sl.Count do
    begin
      if (copy(sl[i],0,1)='#') and (pos('image/',sl[i])>0) then
        begin
          tmp := sl[i];
          tmp := trim(copy(tmp,pos(' ',tmp)+1,length(tmp)));
          tmp := copy(tmp,0,pos(' ',tmp)-1);
          aItem := lvPhotos.Items.Add;
          aItem.Checked:=True;
          aItem.Caption:=sl[i];
          inc(i);
        end
      else
        sl.Delete(i);
    end;
end;

procedure TfCameraimport.bImportClick(Sender: TObject);
var
  aProcess: TProcessUTF8;
  AInfo: TSearchRec;
  atmp: String;
  aFile: String;
  NewFileName: String;
  sl: TStringList;
  extn: String;
  aSecFile: String;
begin
  if lvPhotos.Selected=nil then exit;
  with BaseApplication as IBaseApplication do
    begin
      If FindFirstUTF8(AppendPathDelim(GetInternalTempDir)+'raw_*',faAnyFile,AInfo)=0 then
        Repeat
          With aInfo do
            begin
              If (Attr and faDirectory) <> faDirectory then
                DeleteFileUTF8(AppendPathDelim(GetInternalTempDir)+AInfo.Name);
            end;
        Until FindNext(ainfo)<>0;
      FindClose(aInfo);
    end;
  atmp := lvPhotos.Selected.Caption;
  atmp := copy(atmp,2,pos(' ',atmp)-2);
  sl := TStringList.Create;
  aProcess := TProcessUTF8.Create(Self);
  with BaseApplication as IBaseApplication do
    aProcess.CurrentDirectory:=GetInternalTempDir;
  try
    aProcess.CommandLine:='gphoto2 --get-raw-data='+atmp;
    aProcess.Options:=[poUsePipes,poWaitOnExit];
    aProcess.Execute;
    sl.LoadFromStream(aProcess.Output);
  finally
    aProcess.Free;
  end;
  with BaseApplication as IBaseApplication do
    begin
      If FindFirstUTF8(AppendPathDelim(GetInternalTempDir)+'raw_*',faAnyFile,AInfo)=0 then
        Repeat
          With aInfo do
            begin
              If (Attr and faDirectory) <> faDirectory then
                begin
                  aFile := AppendPathDelim(GetInternalTempDir)+AInfo.Name;
                end;
            end;
        Until FindNext(ainfo)<>0;
      FindCloseUTF8(AInfo);
    end;
  sl.Free;
  if not FileExists(aFile) then
    begin
      with BaseApplication as IBaseApplication do
        NewFileName := AppendPathDelim(GetInternalTempDir)+ExtractFileName(aFile);
      {$ifdef linux}
      ExecProcess('gvfs-copy "'+aFile+'" "'+NewFileName+'"');
      {$endif}
      if not FileExists(NewFileName) then
        Showmessage(Format(strCantAccessFile,[aFile]));
    end
  else NewFileName:=aFile;
  if FileExists(NewFileName) then
    begin
      TDocPages(FDoc.DataSet).AddFromFile(NewFileName);
      if not TDocPages(FDoc.DataSet).CanEdit then TDocPages(FDoc.DataSet).DataSet.Edit;
      TDocPages(FDoc.DataSet).FieldByName('TYPE').AsString:=FDoc.Typ;
      TDocPages(FDoc.DataSet).Post;
           aFile := NewFileName;
          extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aFile)));
          if (extn = '.cr2')
          or (extn = '.crw')
          or (extn = '.dng')
          or (extn = '.raw')
          or (extn = '.erf')
          or (extn = '.raf')
          or (extn = '.3fr')
          or (extn = '.fff')
          or (extn = '.dcr')
          or (extn = '.dcs')
          or (extn = '.kdc')
          or (extn = '.rwl')
          or (extn = '.mef')
          or (extn = '.mfw')
          or (extn = '.iiq')
          or (extn = '.mrw')
          or (extn = '.mdc')
          or (extn = '.nef')
          or (extn = '.nrw')
          or (extn = '.orf')
          or (extn = '.rw2')
          or (extn = '.pef')
          or (extn = '.srw')
          or (extn = '.x3f')
          or (extn = '.cs1')
          or (extn = '.cs4')
          or (extn = '.cs16')
          or (extn = '.srf')
          or (extn = '.sr2')
          or (extn = '.arw')
          then
            begin
              if FileExistsUTF8(copy(aFile,0,rpos('.',aFile)-1)+'.jpg') then
                aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.jpg'
              else if FileExistsUTF8(copy(aFile,0,rpos('.',aFile)-1)+'.JPG') then
                aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.JPG'
              else if FileExistsUTF8(copy(aFile,0,rpos('.',aFile)-1)+'.Jpg') then
                aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.Jpg'
              else aSecFile:='';
              if aSecFile <> '' then
                begin
                  {$ifdef linux}
                  try
                    ExecProcess('gvfs-rm "'+aSecFile+'"');
                  except
                    DeleteFileUTF8(aSecFile);
                  end;
                  {$else}
                  DeleteFileUTF8(aSecFile);
                  {$endif}
                end;
            end;
          if FileExistsUTF8(copy(NewFileName,0,length(NewFileName)-length(extn))+'.ufraw') then
            DeleteFileUTF8(copy(NewFileName,0,length(NewFileName)-length(extn))+'.ufraw');
          DeleteFileUTF8(NewFileName);
          if NewFileName<>aFile then
            begin
              {$ifdef linux}
              ExecProcess('gvfs-rm "'+aFile+'"');
              {$endif}
            end;
     if cbDelete.Checked then
        begin
          sl := TStringList.Create;
          aProcess := TProcessUTF8.Create(Self);
          with BaseApplication as IBaseApplication do
            aProcess.CurrentDirectory:=GetInternalTempDir;
          try
            aProcess.CommandLine:='gphoto2 --delete-file='+atmp;
            aProcess.Options:=[poUsePipes,poWaitOnExit];
            aProcess.Execute;
            sl.LoadFromStream(aProcess.Output);
          finally
            aProcess.Free;
          end;
          sl.Free;
        end;
      if Sender <> nil then
        FDoc.acRefresh.Execute;
      lvPhotos.Selected.Checked:=False;
    end;
  if Sender <> nil then
    cbCameraSelect(nil);
end;

procedure TfCameraimport.Button1Click(Sender: TObject);
var
  i: Integer;
  oldDelete: Boolean;
begin
  oldDelete := cbDelete.Checked;
  cbDelete.Checked:=False;
  i := 0;
  if lvPhotos.ItemIndex>-1 then i := lvPhotos.ItemIndex;
  for i := i to lvPhotos.Items.Count-1 do
    begin
      if lvPhotos.Items[i].Checked then
        begin
          lvPhotos.Selected := lvPhotos.Items[i];
          lvPhotos.Selected.MakeVisible(True);
          Application.ProcessMessages;
          bImport.Click;
        end;
    end;
  cbDelete.Checked:=oldDelete;
end;

procedure TfCameraimport.Button2Click(Sender: TObject);
var
  i: Integer;
  tmp: String;
  aDS: TDocPages;
  aDS2: TDeletedItems;
begin
  Button2.Enabled := False;
  aDS := TDocPages.Create(nil);
  aDS2 := TDeletedItems.Create(nil);
  for i := 0 to lvPhotos.Items.Count-1 do
    begin
      if lvPhotos.Items[i].Checked then
        begin
          lvPhotos.Selected := lvPhotos.Items[i];
          lvPhotos.Selected.MakeVisible(True);
          Application.ProcessMessages;
          tmp := lvPhotos.Items[i].Caption;
          Data.SetFilter(aDS,Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')='+Data.QuoteValue('*'+Uppercase(tmp))));
          //Data.SetFilter(aDS2,Data.ProcessTerm('UPPER('+Data.QuoteField('LINK')+')='+Data.QuoteValue('*'+Uppercase(tmp)+'*')));
          lvPhotos.Items[i].Checked:=(aDS.Count=0) and (aDS2.Count=0);
        end;
    end;
  aDS.Free;
  aDS2.Free;
  Button2.Enabled := True;
end;

function TfCameraimport.ImportAvalibe: Boolean;
var
  aProcess: TProcessUTF8;
  sl: TStringList;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfCameraimport,fCameraimport);
      Self := fCameraimport;
    end;
  Result := False;
  aProcess := TProcessUTF8.Create(Self);
  sl := TStringList.Create;
  try
    try
    aProcess.CommandLine:='gphoto2 --auto-detect';
    aProcess.Options:=[poUsePipes,poWaitOnExit];
    aProcess.Execute;
    sl.LoadFromStream(aProcess.Output);
    except
    end;
  finally
    aProcess.Free;
  end;
  Result := sl.Count>0;
  if sl.Count>0 then
    sl.Delete(0);
  if sl.Count>0 then
    sl.Delete(0);
  cbCamera.Clear;
  cbCamera.Items.Assign(sl);
  sl.Free;
end;

function TfCameraimport.Execute(aDocMan: TfManageDocFrame; aTyp: string
  ): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfCameraimport,fCameraimport);
      Self := fCameraimport;
    end;
  FDoc := aDocMan;
  ImportAvalibe;//Check for Cameras
  Result := ShowModal=mrOK;
end;

end.

