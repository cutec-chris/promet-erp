unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList,Clipbrd,process;

type
  TForm1 = class(TForm)
    acAdd: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    lbFormats: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure acAddExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses LCLIntf,LCLType;
{$R *.lfm}

procedure TForm1.acAddExecute(Sender: TObject);
var
  aFormat: LongWord;
  i: Integer;
  aMime: String;
  aProc: TProcess;
  aExt: String;
  aStream: TFileStream;
  aMStream: TMemoryStream;
  aFStream: TFileStream;
  aOK: Boolean;
begin
  lbFormats.Clear;
  for i := 0 to Clipboard.FormatCount-1 do
    begin
      aMStream := TMemoryStream.Create;
      try
        aOK := Clipboard.GetFormat(Clipboard.Formats[i],aMStream);
      except
        aOK := false;
      end;
      if aOK  then
        begin
          aFormat := Clipboard.Formats[i];
          aMime := ClipboardFormatToMimeType(aFormat);
          lbFormats.Items.Add(aMime);
          {
          aProc := TProcess.Create(nil);
          aProc.CurrentDirectory:=AppendPathDelim(Application.Location)+'tools';
          if copy(aMime,0,6) = 'image/' then
            begin
              aExt := copy(aMime,pos('/',aMime)+1,length(aMime));
              if pos('-',aExt)>0 then
                aExt := copy(aExt,pos('-',aExt)+1,length(aExt));
              aFStream := TFileStream.Create(AppendPathDelim(GetTempDir)+'clip_img.'+aExt,fmCreate);
              aMStream.Position:=0;
              aFStream.CopyFrom(aMStream,0);
              aFStream.Free;
                begin
                  aProc.CommandLine := 'convert '+AppendPathDelim(GetTempDir)+'clip_img.'+aExt+' '+AppendPathDelim(GetTempDir)+'clip_img.jpg';
                  aProc.Execute;

                end;
              aStream.Free;
            end;
          aProc.Free;
          }
        end;
      aMStream.Free;
    end;
  Image1.Picture.Clear;
  if Clipboard.HasPictureFormat then
    Image1.Picture.LoadFromClipboardFormat(Clipboard.FindPictureFormatID);
  Memo1.Text := Clipboard.AsText;
end;

end.

