unit uwindowsshellpreviews;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ShObjIdlQuot,ShellObjHelper,Graphics,windows,ActiveX, Comobj,ShellApi;

function DisplayThumbnailImageFromFile(const FileName: string;ColorDepth : Integer=32;Width : Integer = 200;Height : Integer = 200) : Graphics.TBitmap;

implementation

function DisplayThumbnailImageFromFile(const FileName: string;ColorDepth : Integer=32;Width : Integer = 200;Height : Integer = 200) : Graphics.TBitmap;
var
  XtractImage: IExtractImage;
  Icon: TIcon;
  Flags: DWORD;
  RT: IRunnableTask;
begin
  Flags := 0;//GetLocationFlags;

  Result := nil;
  Icon := nil;
  try
    if GetExtractImageItfPtr(FileName, XTractImage) and
      ExtractImageGetFileThumbnail(XtractImage, Width, Height,
        ColorDepth, Flags, RT, Result) then
    begin
      if (Flags and IEIFLAG_CACHE) <> 0 then
        //Memo1.Lines.Add('Extractor does not cache the thumbnail.');
      if (Flags and IEIFLAG_GLEAM) <> 0 then
        //Memo1.Lines.Add('The image has a gleam.');
      if (Flags and IEIFLAG_NOSTAMP) <> 0 then
        //Memo1.Lines.Add('Extractor does not want an icon stamp on the thumbnail.');
      if (Flags and IEIFLAG_NOBORDER) <> 0 then
        //Memo1.Lines.Add('Extractor does not want an a border around the thumbnail.');
    end else if GetFileLargeIcon(FileName, Icon) then
    begin
      //Memo1.Lines.Text := 'Thumbnail is not available. Default icon displayed.';
      Image1.Picture.Assign(Icon);
    end;
  finally
    Icon.Free;
    Bmp.Free;
  end;
end;

end.

