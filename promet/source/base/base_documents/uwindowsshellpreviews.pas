unit uwindowsshellpreviews;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DisplayThumbnailImageFromFile(const FileName: string);

implementation

procedure DisplayThumbnailImageFromFile(const FileName: string);
var
  XtractImage: IExtractImage;
  Bmp: TBitmap;
  Icon: TIcon;
  ColorDepth: Integer;
  Flags: DWORD;
  RT: IRunnableTask;
begin
  case ComboBox1.ItemIndex of
    0: ColorDepth := 4;
    1: ColorDepth := 8;
    2: ColorDepth := 16;
    3: ColorDepth := 24;
    else ColorDepth := 32;
  end;

  Flags := GetLocationFlags;

  Bmp := nil;
  Icon := nil;
  try
    if GetExtractImageItfPtr(FileName, XTractImage) and
      ExtractImageGetFileThumbnail(XtractImage, Image1.Width, Image1.Height,
        ColorDepth, Flags, RT, Bmp) then
    begin
      Image1.Picture.Assign(Bmp);
      Memo1.Lines.Clear;
      if (Flags and IEIFLAG_CACHE) <> 0 then
        Memo1.Lines.Add('Extractor does not cache the thumbnail.');
      if (Flags and IEIFLAG_GLEAM) <> 0 then
        Memo1.Lines.Add('The image has a gleam.');
      if (Flags and IEIFLAG_NOSTAMP) <> 0 then
        Memo1.Lines.Add('Extractor does not want an icon stamp on the thumbnail.');
      if (Flags and IEIFLAG_NOBORDER) <> 0 then
        Memo1.Lines.Add('Extractor does not want an a border around the thumbnail.');
    end else if GetFileLargeIcon(FileName, Icon) then
    begin
      Memo1.Lines.Text := 'Thumbnail is not available. Default icon displayed.';
      Image1.Picture.Assign(Icon);
    end;
  finally
    Icon.Free;
    Bmp.Free;
  end;
end;

end.

