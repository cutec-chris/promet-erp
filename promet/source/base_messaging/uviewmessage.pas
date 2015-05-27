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
info@cu-tec.de
*******************************************************************************}
{-TODO : Encoding in HTML Mails stimmt nicht }
unit uViewMessage;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil,  Forms, ExtCtrls, StdCtrls, IpHtml,
  uData, Graphics, Menus, uIntfStrConsts, DB, Variants, Utils, LCLIntf,
  uMessages, uminiconvencoding;
type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;
  TfViewMessage = class(TFrame)
    iContent: TScrollBox;
    iiContent: TImage;
    ipHTMLContent: TIpHtmlPanel;
    mContent: TMemo;
    moCopy: TMenuItem;
    pmCopy: TPopupMenu;
    procedure FrameResize(Sender: TObject);
    procedure ipHTMLContentHotClick(Sender: TObject);
    procedure moCopyClick(Sender: TObject);
    procedure OnGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  private
    { private declarations }
    FActiveList : TDataSet;
    Messages: TMessage;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure ShowMessage(ListDataSet : TDataSet;ShowLetter : Boolean = False);
    property Message : TMessage read Messages;
  end;
  TLoadHTMLProcess = class(TThread)
  private
    FDone: Boolean;
    FExcept: Boolean;
    FHTML : TSimpleIPHTML;
    ss : TStream;
    FView : TfViewMessage;
  public
    constructor Create(View : TfViewMessage;HTML : TSimpleIPHTML;sss : TStream);
    procedure Execute;override;
    property Done : Boolean read FDone;
  end;
implementation
{$R *.lfm}
uses uDocuments,LCLProc,wikitohtml,uBaseDbClasses,uBaseApplication,synautil;
resourcestring
  strMessagenotDownloaded       = 'Die Naricht wurde aus Sicherheitsgründen nicht heruntergeladen !';
  strOpenToViewItem             = 'Bitte klicken Sie doppelt auf diesen Eintrag um ihn anzuzeigen';

procedure TfViewMessage.OnGetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  ss: TStringStream;
  tmp: String;
  aDocument: TDocument;
begin
  Picture := TPicture.Create;
  Picture.Bitmap.Height:=0;
  Picture.Bitmap.Width := 0;
  Picture.Bitmap.Transparent:=True;
  if Uppercase(copy(url,0,4)) = 'CID:' then
    begin
      try
        try
          aDocument := TDocument.CreateEx(Self,Data);
          Data.SetFilter(aDocument,Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' and '+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(Messages.Content.Id.AsString));
          if aDocument.DataSet.Locate('TYPE;REF_ID_ID',VarArrayOf(['N',Messages.Content.id.AsString]),[loPartialKey]) then
            begin
              aDocument.DataSet.First;
              while not aDocument.DataSet.EOF do
                begin
                  ss := TStringStream.Create('');
                  Data.BlobFieldToStream(aDocument.DataSet,'FULLTEXT',ss);
                  tmp := StringReplace(StringReplace(ss.DataString,'<','',[]),'>','',[]);
                  if ValidateFileName(tmp) = copy(ValidateFileName(URL),5,length(ValidateFileName(URL))) then
                    begin
                      with BaseApplication as IBaseApplication do
                        begin
                          Data.BlobFieldToFile(aDocument.DataSet,'DOCUMENT',GetInternalTempDir+copy(url,5,length(url))+'.'+aDocument.FieldByName('EXTENSION').AsString);
                          try
                            Picture.LoadFromFile(GetInternalTempDir+copy(url,5,length(url))+'.'+aDocument.FieldByName('EXTENSION').AsString);
                          except
                            FreeAndnil(Picture);
                          end;
                          DeleteFileUTF8(GetInternalTempDir+copy(url,5,length(url))+'.'+aDocument.FieldByName('EXTENSION').AsString);
                          if Assigned(Picture) and (Picture.Width = 0) then
                            FreeAndNil(Picture);
                        end;
                    end;
                  ss.Free;
                  aDocument.DataSet.Next;
                end;
            end;
        except
        end;
      finally
        aDocument.Free;
      end;
    end
  else
    begin
      try
        try
          aDocument := TDocument.CreateEx(Self,Data);
          Data.SetFilter(aDocument,Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' and '+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(Messages.Content.Id.AsString));
          if aDocument.Count>0 then
            begin
              aDocument.DataSet.First;
              while not aDocument.DataSet.EOF do
                begin
                  if aDocument.FileName = url then
                    begin
                      ss := TStringStream.Create('');
                      with BaseApplication as IBaseApplication do
                        begin
                          Data.BlobFieldToFile(aDocument.DataSet,'DOCUMENT',GetInternalTempDir+copy(url,5,length(url))+'.'+aDocument.FieldByName('EXTENSION').AsString);
                          try
                            Picture.LoadFromFile(GetInternalTempDir+copy(url,5,length(url))+'.'+aDocument.FieldByName('EXTENSION').AsString);
                          except
                            FreeAndnil(Picture);
                          end;
                          DeleteFileUTF8(GetInternalTempDir+copy(url,5,length(url))+'.'+aDocument.FieldByName('EXTENSION').AsString);
                        end;
                      if Assigned(Picture) and (Picture.Width = 0) then
                        FreeAndNil(Picture);
                    end;
                  ss.Free;
                  aDocument.DataSet.Next;
                end;
            end;
        except
        end;
      finally
        aDocument.Free;
      end;
    end;
end;
constructor TfViewMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Messages := TSpecialMessage.CreateEx(Self,Data);
end;
destructor TfViewMessage.Destroy;
begin
  Messages.Free;
  inherited Destroy;
end;
procedure TfViewMessage.moCopyClick(Sender: TObject);
begin
  ipHTMLContent.CopyToClipboard;
end;
procedure TfViewMessage.ipHTMLContentHotClick(Sender: TObject);
var
  nUrl: String;
begin
  if ipHTMLContent.HotNode is TIpHtmlNodeA then
    begin
      Application.ProcessMessages;
      nUrl := TIpHtmlNodeA(IpHtmlContent.HotNode).HRef;
      OpenURL(nUrl);
      Application.ProcessMessages;
    end;
end;
procedure TfViewMessage.FrameResize(Sender: TObject);
begin
  {
  if Assigned(Data) then
    if Assigned(Data.MessageIdx) then
      if Assigned(Data.MessageIdx.DataSet) then
        if (Data.MessageIdx.DataSet.Active) then
          if (Data.MessageIdx.FieldByName('TYPE').AsString = 'LETTE') then
            if iiContent.Picture.Width > 0 then
              iiContent.Height:=(iiContent.Width*iiContent.Picture.Height) div iiContent.Picture.Width;
  }
end;
procedure TfViewMessage.ShowMessage(ListDataSet : TDataSet;ShowLetter : Boolean = False);
var
  ss: TStringStream;
  NewHTML: TSimpleIpHtml;
  ID: String;
  proc: TLoadHTMLProcess;
  i: Integer;
  sl: TStringList;
  tmp: String;
  aEncoding: String;
begin
  FActiveList := ListDataSet;
  if Messages.DataSet.ControlsDisabled then exit;
  Messages.Select(ListDataSet.FieldByName('SQL_ID').AsVariant);
  Messages.Open;
  if not Messages.DataSet.Active then exit;
  Messages.Content.Open;
  if Data.RecordCount(Messages.Content) > 0 then
   begin
     if Uppercase(Messages.Content.FieldByName('DATATYP').AsString) = 'PLAIN' then
       begin
         ipHTMLContent.Visible:=false;
         ss := TStringStream.Create('');
         Data.BlobFieldToStream(Messages.Content.DataSet,'DATA',ss);
         sl := TStringList.Create;
         sl.Text:=SysToUni(ConvertEncoding(ss.DataString,GuessEncoding(ss.DataString),EncodingUTF8));
         sl.TextLineBreakStyle := tlbsCRLF;
         mContent.Lines.Assign(sl);
         sl.Free;
         ss.Free;
         mContent.Visible:=True;
       end
     else if UpperCase(Messages.Content.FieldByName('DATATYP').AsString) = 'HTML' then
       begin
         mContent.Visible:=false;
         try
           ss:=TStringStream.Create('');
           Data.BlobFieldToStream(Messages.Content.DataSet,'DATA',ss);
           ss.Position := 0;
           tmp := ss.DataString;
           aEncoding := GuessEncoding(tmp);
           if pos('ENCODING',Uppercase(tmp)) = 0 then
             tmp := ConvertEncoding(tmp,aEncoding,EncodingUTF8);
           tmp := HTMLEncodeTagless(tmp);
           ss.Free;
           ss:=TStringStream.Create(tmp);
           NewHTML:=TSimpleIpHtml.Create; // Beware:Will be freed automatically by IpHtmlPanel1
           proc := TLoadHTMLProcess.Create(Self,NewHTML,ss);
           for i := 0 to 1000 do
             begin
               if Proc.Done then
                 break;
               if Assigned(proc.FatalException) then
                 begin
                   Proc.Terminate;
                   sl := TStringList.Create;
                   sl.Text:=tmp;
                   with BaseApplication as IBaseApplication do
                     sl.SaveToFile(GetInternalTempDir+'aerror.html');
                   sl.Free;
                   FreeAndNil(NewHTML);
                   break;
                 end;
               Application.ProcessMessages;
               sleep(1);
             end;
           if i > 100 then
             begin
               proc.Terminate;
               NewHTML := nil;
               exit;
             end;
           Proc.Free;
           ss.Free;
           try
             ipHTMLContent.SetHtml(NewHTML);
             ipHTMLContent.Visible:=true;
             Application.ProcessMessages;
           except
             FreeAndNil(NewHTML);
             try
               ipHTMLContent.SetHtml(nil);
             except
               sl := TStringList.Create;
               sl.Text:=tmp;
               with BaseApplication as IBaseApplication do
                 sl.SaveToFile(GetInternalTempDir+'aerror.html');
               sl.Free;
             end;
           end;
         except
           sl := TStringList.Create;
           sl.Text:=tmp;
           with BaseApplication as IBaseApplication do
             sl.SaveToFile(GetInternalTempDir+'aerror.html');
           sl.Free;
           ss.Free;
         end;
       end
     else if UpperCase(Messages.FieldByName('TYPE').AsString) = 'WIKI' then
       begin
         mContent.Visible:=false;
         try
           ss:=TStringStream.Create('');
           Data.BlobFieldToStream(Messages.Content.DataSet,'DATA',ss);
           ss.Position := 0;
           tmp := '<html><body>'+WikiText2HTML(ss.DataString,'','',True)+'</body></html>';

           aEncoding := GuessEncoding(tmp);
           if pos('ENCODING',Uppercase(tmp)) = 0 then
             tmp := char($EF)+char($BB)+char($BF)+SysToUni(ConvertEncoding(tmp,aEncoding,EncodingUTF8));
           ss.Free;
           ss:=TStringStream.Create(tmp);
           NewHTML:=TSimpleIpHtml.Create; // Beware:Will be freed automatically by IpHtmlPanel1
           proc := TLoadHTMLProcess.Create(Self,NewHTML,ss);
           for i := 0 to 1000 do
             begin
               if Proc.Done then
                 break;
               if Assigned(proc.FatalException) then
                 begin
                   Proc.Terminate;
                   FreeAndNil(NewHTML);
                   break;
                 end;
               Application.ProcessMessages;
               sleep(1);
             end;
           if i > 100 then
             begin
               proc.Terminate;
               NewHTML := nil;
               exit;
             end;
           Proc.Free;
           ss.Free;
           try
             ipHTMLContent.SetHtml(NewHTML);
           except
             FreeAndNil(NewHTML);
             try
               ipHTMLContent.SetHtml(nil);
             except
             end;
           end;
         except
           ss.Free;
         end;
         ipHTMLContent.Visible:=true;
       end
     else
       begin
         ipHTMLContent.Visible:=false;
         ss := TStringStream.Create('');
         Data.BlobFieldToStream(Messages.Content.DataSet,'DATA',ss);
         mContent.Lines.Text:='Unknown Datatype ('+Messages.Content.FieldByName('DATATYP').AsString+'):'+ss.DataString;
         ss.Free;
         mContent.Visible:=True;
       end;
   end
  else
   begin
     ipHTMLContent.Visible:=false;
     mContent.Visible:=false;
     if ListDataSet.FieldByName('TYPE').AsString = 'LETTE' then
       begin
         if ShowLetter then
           begin
             Data.SetFilter(Messages.Documents,'');
             {
             if Data.GotoID(Data.MessageIdx.FieldByName('ID').AsString) then
               begin
                 iContent.Visible := True;
                 ID := Data.Documents.FieldByName('NUMBER').AsString;
                 Data.SetFilter(Data.Documents,'"NUMBER"='+ID);
                 Data.Documents.DataSet.Last;
                 Data.DataModule.BlobFieldToFile(Data.Documents.DataSet,'DOCUMENT',GetInternalTempDir+'messagetmp.jpg');
                 iiContent.Picture.LoadFromFile(GetInternalTempDir+'messagetmp.jpg');
                 iiContent.Height:=(iiContent.Width*iiContent.Picture.Height) div iiContent.Picture.Width;
                 iiContent.Stretch:=True;
                 DeleteFileUTF8(GetInternalTempDir+'messagetmp.jpg');
               end;
             }
           end;
       end;
   end;
end;
constructor TLoadHTMLProcess.Create(View : TfViewMessage;HTML: TSimpleIPHTML; sss: TStream);
begin
  FView := View;
  FHTML := HTML;
  ss := sss;
  FDone := False;
  FExcept := False;
  if not BaseApplication.HasOption('disablethreads') then
    inherited Create(false)
  else
    Execute;
end;

procedure TLoadHTMLProcess.Execute;
var
  aBitmap : TBitmap;
begin
  FHTML.OnGetImageX:=@FView.OnGetImage;
  try
    aBitmap := TBitmap.Create;
    FHTML.LoadFromStream(ss);
    aBitmap.Width:=100;
    aBitmap.Height:=100;
    //FHTML.Render(aBitmap.Canvas,Rect(0,0,100,100),True,Point(0,10));
    aBitmap.free;
    FDone := True;
  except
    FreeAndNil(FHTML);
    FDone := False;
  end;
end;
initialization

end.

