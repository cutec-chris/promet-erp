unit uvisualtests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, uCrossRelation, IntfGraphics, LCLIntf,
  mouseandkeyinput, LCLType, Graphics, GraphType, Controls, LCLProc,
  Forms;

function FindImageOnScreen(aImageFile : string;DoRaise : Boolean = True) : TPoint;
function WaitForImageOnScreen(aImageFile : string;aTimeout : Integer) : Boolean;
function DblClick(aPoint : TPoint) : Boolean;
function LeftClick(aPoint : TPoint) : Boolean;
function RightClick(aPoint : TPoint) : Boolean;

implementation

function CompareDirect(ImgSrc, ImgIn: TLazIntfImage;Offset : TPoint):Single;
var
  P1,P2:PRGBQuadArray;
  y: Integer;
  x: Integer;
begin
  Result := 100;
  for y:=0 to ImgSrc.Height-1 do
  begin
    P1:=ImgSrc.GetDataLineStart(y);
    P2:=ImgIn.GetDataLineStart(y+Offset.y);
    if CompareMem(Pointer(P1),Pointer(P2)+Offset.x*sizeof(TRGBQuad),ImgSrc.Width*sizeof(TRGBQuad)) = False then
      begin
        Result := 0;
        break;
      end;
  end;
end;

function FindImageOnScreen(aImageFile: string;DoRaise : Boolean = True): TPoint;
var
  aScreen: TLazIntfImage;
  aImage: TLazIntfImage;
  aRes: Single;
  aFound : TPoint;
  x: Integer;
  y: Integer;
  aMin : Single = 00;
  Description: TRawImageDescription;
  aTmp: TLazIntfImage;
begin
  Result := Point(0,0);
  aTmp := TLazIntfImage.Create(0,0);
  aTmp.LoadFromDevice(GetDC(0));
  Description.Init_BPP32_B8G8R8A8_BIO_TTB(aTmp.Width,aTmp.Height);
  aScreen := TLazIntfImage.Create(0,0);
  aScreen.DataDescription := Description;
  aScreen.CopyPixels(aTmp);
  aTmp.Free;
  aImage := TLazIntfImage.Create(0,0);
  aImage.DataDescription := Description;
  aImage.LoadFromFile(aImageFile);

  for y := 0 to (aScreen.Height-aImage.Height) do
    for x := 0 to (aScreen.Width-aImage.Width) do
      begin
        aRes := CompareDirect(aImage,aScreen,Point(x,y));
//        aRes := ZNCC(aImage,aScreen,Point(x,y));
        if aRes > aMin then
          begin
            aMin := aRes;
            aFound := Point(x+aImage.Width div 2,y+aImage.Height div 2);
            break;
          end;
      end;
  if aMin < 95 then
    begin
      if DoRaise then
        raise Exception.Create('Image '+aImageFile+' not found on Desktop !')
    end
  else
    Result := aFound;
  aImage.Free;
  aScreen.Free;
end;

function WaitForImageOnScreen(aImageFile: string; aTimeout: Integer): Boolean;
var
  aTime: DWORD;
  aPoint: TPoint;
begin
  aTime := GetTickCount;
  aPoint := FindImageOnScreen(aImageFile,False);
  while ((aPoint.x = 0) and (aPoint.y = 0)) and (GetTickCount-aTime < aTimeOut) do
    begin
      Application.ProcessMessages;
      aPoint := FindImageOnScreen(aImageFile,False);
    end;
  Result := (aPoint.x <> 0) or (aPoint.y <> 0);
end;

function DblClick(aPoint: TPoint): Boolean;
begin
  MouseInput.DblClick(mbLeft,[],aPoint.X,aPoint.y);
end;

function LeftClick(aPoint: TPoint): Boolean;
begin
  MouseInput.Click(mbLeft,[],aPoint.X,aPoint.y);
end;

function RightClick(aPoint: TPoint): Boolean;
begin
  MouseInput.Click(mbRight,[],aPoint.X,aPoint.y);
end;

end.
