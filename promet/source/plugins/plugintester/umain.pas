unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls,uWlxModule,Utils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FileNameEdit1: TFileNameEdit;
    Image1: TImage;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Modules: TWLXModuleList;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Info: TSearchRec;
begin
  Modules := TWLXModuleList.Create;
  If FindFirst (UniToSys(ExtractFilePath(ParamStr(0))+DirectorySeparator+'plugins'+DirectorySeparator+'*.wlx'),faAnyFile and faDirectory,Info)=0 then
    begin
      Repeat
        With Info do
          begin
            if (Attr and faDirectory) <> faDirectory then
              Modules.Add(ExtractFilePath(ParamStr(0))+DirectorySeparator+'plugins'+DirectorySeparator+Name);
          end;
      Until FindNext(info)<>0;
    end;
  FindClose(Info);

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  aName: String;
  aFileName: String;
  Result: Boolean;
  e: String;
  i: Integer;
  aMod: TWlxModule;
  ThumbFile: String;
  aWidth: Integer;
  aHeight: Integer;
  aBitmap: TBitmap;
  Found: Boolean=false;
begin
  Result := False;
  aWidth := 100;
  aHeight := 100;
  aName := FileNameEdit1.FileName;
  aFileName := FileNameEdit1.FileName;
  e := lowercase (ExtractFileExt(aName));
  if (e <> '') and (e[1] = '.') then
    System.delete (e,1,1);
  for i := 0 to Modules.Count-1 do
    begin
      aMod := Modules.GetWlxModule(i);
      if aMod.LoadModule then
        if (pos('EXT="'+Uppercase(e)+'"',aMod.CallListGetDetectString)>0) or (pos('EXT="*"',aMod.CallListGetDetectString)>0) then
          begin
            try
              ThumbFile := aMod.CallListGetPreviewBitmapFile(aFileName,GetTempPath,aWidth,aHeight,'');
              if ThumbFile='' then
                begin
                  aBitmap := TBitmap.Create;
                  aBitmap.Handle:=aMod.CallListGetPreviewBitmap(aFileName,aWidth,aHeight,'');
                  if aBitmap.Handle<>0 then
                    begin
                      aBitmap.SaveToFile(GetTempPath+'thumb.bmp');
                      ThumbFile := GetTempPath+'thumb.bmp';
                    end;
                  aBitmap.Free;
                end;
              Found := True;
            except
              aMod.UnloadModule;
            end;
          end;
      if Found then Image1.Picture.LoadFromFile(GetTempPath+'thumb.bmp');
    end;
end;

end.

