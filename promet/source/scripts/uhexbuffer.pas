unit uHexBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils;

type
  THexfileVersion = (hv16Bit, hv32Bit);
  TLastUpdate = (luNone, luHex, luBin);

type

  { THexBuffer }

  THexBuffer = class(TComponent)
  private
    FEmbedded: Boolean;
    FFilename: string;
    { Private-Deklarationen }
    FHexfileVersion: THexfileVersion;
    FDataSize: integer;
    FDataBin: array of byte;
    FDataUsed: array of Boolean;
    FDataHex: TStringList;
    FInvalid: Boolean;
    FLastUpdate: TLastUpdate;
    FAutoUpdate: boolean;
    FHexLineLen: integer;
    FUnusedBytes: byte;
    FUpdating: boolean;
    function GetDataHexCount: integer;
    procedure SetDataBin(Index: integer; const AValue: integer);
    procedure SetFilename(const AValue: string);
    procedure SetHexfileVersion(hv: THexfileVersion);
    procedure SetDataSize(ds: integer);
    function GetDataBin(i: integer): integer;
    function GetDataUsed(i: integer): boolean;
    procedure SetDataUsed(i: integer; used: boolean);
    function GetDataHex(i: integer): string;
    procedure SetDataHex(i: integer; s: string);
    procedure SetAutoUpdate(au: boolean);
    procedure SetHexLineLen(ll: integer);
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
    procedure SaveHexFile(Filename: TFilename);
    procedure LoadHexFile(Filename: TFilename);
    procedure SaveBinFile(Filename: TFilename);
    procedure LoadBinStream(aStream : TStream);
    procedure SaveBinStream(aStream : TStream);
    procedure LoadBinFile(Filename: TFilename);
    procedure DataBinToBuffer(var Buffer);
    function SaveToFile(Filename : TFilename) : Boolean;
    function LoadFromFile(FileName : TFilename) : Boolean;
    procedure BufferToDataBin(var Buffer);
    procedure DataUsedToBuffer(var Buffer);
    procedure BufferToDataUsed(var Buffer);
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetHexfileSize: integer;
    property DataHexCount: integer read GetDataHexCount;
    property DataBin[Index: integer]: integer read GetDataBin write SetDataBin;
    property DataUsed[Index: integer]: boolean read GetDataUsed write SetDataUsed;
    property DataHex[Index: integer]: string read GetDataHex write SetDataHex;
    procedure ReadFromString(Data : string);
    procedure SaveToString(var Data : string);
  published
    { Published-Deklarationen }
    property HexfileVersion: THexfileVersion read FHexfileVersion write SetHexfileVersion;
    property DataSize: integer read FDataSize write SetDataSize;
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate;
    property HexLineLen: integer read FHexLineLen write SetHexLineLen;
    property UnusedBytes: byte read FUnusedBytes write FUnusedBytes;
    property Filename : string read FFilename write SetFilename;
    property Embedded : Boolean read FEmbedded write FEmbedded;
    property Invalid : Boolean read FInvalid;
  end;

resourcestring
  strFeatureNotImplemented              = 'nicht Implementiert !';
  strDataBinIndexOutOfRange             = 'Index von DataBin überschreitet das Limit';
  strIndexfromdataUsedOutOfRange        = 'Index von DataUsed überschreitet das Limit';
  strFaultChecksum                      = 'Fehlerhafte Prüfsumme in Hex-File gefunden!';
  strFaultFileEnd                       = 'Fehlerhafte Ende-Kennung in Hex-File gefunden!';
  strInvalidBlock                       = 'Ungültigen Block in Hex-File gefunden!';
  strInvalidHexFile                     = 'Ungültige Hex-Datei! ":" erwartet!';
  strFileToBig                          = 'Hex Datei zu groß';

implementation

{ THexBuffer }

function HexDigitToInt(c: char): integer;
begin
  HexDigitToInt:= -1;
  if ((c>='0') and (c<='9')) then HexDigitToInt:= byte(c)-byte('0');
  if ((c>='A') and (c<='F')) then HexDigitToInt:= byte(c)+10-byte('A');
end;

function HexToInt(s: shortstring): integer;
begin
  HexToInt:= 16*HexDigitToInt(s[1])+HexDigitToInt(s[2]);
end;

function THexBuffer.GetDataHexCount: integer;
begin
  GetDataHexCount:= FDataHex.Count;
end;

procedure THexBuffer.SetDataBin(Index: integer; const AValue: integer);
begin
  if (Index>=0) and (Index<FDataSize) then
  begin
    FDataBin[Index]:= AValue;
    FLastUpdate:= luBin;
  end
  else
    raise Exception.Create(strDataBinIndexOutOfRange);
  if DataUsed[Index] and FAutoUpdate and (not FUpdating) then Update;
end;

procedure THexBuffer.SetFilename(const AValue: string);
begin
  if AValue = FFilename then exit;
  FInvalid := False;
  if not LoadFromFile(AValue) then
    FInvalid := True;
  FFilename := AValue;
  try
    DataSize := GetHexfileSize;
    Update;
  except
    FInvalid := True;
  end;
end;

procedure THexBuffer.SetHexfileVersion(hv: THexfileVersion);
begin
  if hv=hv32Bit then
    raise Exception.Create(strFeatureNotImplemented);
  FHexfileVersion:= hv;
end;

procedure THexBuffer.SetDataSize(ds: integer);
begin
  if ds<>FDataSize then
  begin
    Setlength(FDataBin,0);
    Setlength(FDataUsed,0);
    FDataBin:= nil;
    FDataUsed:= nil;
    FLastUpdate:= luHex;
    if ds<>0 then
    begin
      FDataSize:= ds;
      Setlength(FDataBin,FDataSize);
      Setlength(FDataUsed,FDataSize);
      FillChar(FDataBin[0],FDataSize,UnusedBytes);
      FillChar(FDataUsed[0],FDataSize*SizeOf(boolean),false);
    end;
  end;
  if FAutoUpdate and (not FUpdating) then Update;
end;

function THexBuffer.GetDataBin(i: integer): integer;
begin
  GetDataBin:= FDataBin[i];
end;

function THexBuffer.GetDataUsed(i: integer): boolean;
begin
  GetDataUsed:= FDataUsed[i];
end;

procedure THexBuffer.SetDataUsed(i: integer; used: boolean);
begin
  if (i>=0) and (i<FDataSize) then
  begin
    FDataUsed[i]:= used;
    FLastUpdate:= luBin;
  end
  else
    raise Exception.Create(strIndexfromdataUsedOutOfRange);
  if FAutoUpdate and (not FUpdating) then Update;
end;

function THexBuffer.GetDataHex(i: integer): string;
begin
  if i<DataHexCount then
    GetDataHex:= FDataHex.Strings[i]
  else
    GetDataHex:= '';
end;

procedure THexBuffer.SetDataHex(i: integer; s: string);
begin
  while i>=DataHexCount do FDataHex.Add('');
  FDataHex.Strings[i]:= s;
  FLastUpdate:= luHex;
  if FAutoUpdate and (not FUpdating) then Update;
end;

procedure THexBuffer.SetAutoUpdate(au: boolean);
begin
  FAutoUpdate:= au;
  if FAutoUpdate and (not FUpdating) then Update;
end;

procedure THexBuffer.SetHexLineLen(ll: integer);
begin
  if ll=FHexLineLen then Exit;
  FHexLineLen:= ll;
  if FAutoUpdate and (not FUpdating) then
  begin
    if FLastUpdate=luHex then Update;
    FLastUpdate:= luBin;
    Update;
  end;
  if (not FAutoUpdate) and (FLastUpdate<>luHex) then
    FLastUpdate:= luBin;
end;

constructor THexBuffer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataHex:= TStringList.Create;
  if FHexLineLen=0 then
    FHexLineLen:= 16;
  FEmbedded := False;
end;

destructor THexBuffer.Destroy;
begin
  FDataHex.Destroy;
  inherited Destroy;
end;

procedure THexBuffer.Update;
var
  numbytes: integer;
  startaddr: integer;
  datastring: string;
  checksum: integer;
  blocktype: integer;
  i, j: integer;
  s: string;
  segmentstart: Integer;
begin
  if FLastUpdate=luNone then Exit;

  BeginUpdate;

  if FLastUpdate=luBin then
    begin
      FDataHex.Clear;
      numbytes:= 0;
      checksum:= 0;
      startaddr:= 0;
      segmentstart:=0;
      for i:= 0 to FDataSize-1 do
        begin
          if DataUsed[i] then
            begin
              if numbytes=0 then
                begin
                  startaddr:= i;
                  if startaddr > $FFFF then
                    begin
                      if segmentstart <> (startaddr shr 16) then
                        begin
                          segmentstart:=startaddr shr 16;
                          checksum:= 0;
                          checksum:= checksum-(segmentstart div 256);
                          checksum:= checksum-(segmentstart mod 256);
                          checksum:= checksum-2;
                          checksum:= checksum-4;
                          FDataHex.Add(':02000004'+IntToHex(segmentstart,4)+IntToHex(byte(checksum),2));
                        end;
                      startaddr := startaddr and $FFFF;
                    end;
                  datastring:= '';
                  checksum:= 0;
                end;
              inc(numbytes);
              datastring:= datastring+IntToHex(DataBin[i],2);
              checksum:= checksum-DataBin[i];
              if numbytes=FHexLineLen then
                begin
                  checksum:= checksum-numbytes;
                  checksum:= checksum-(startaddr div 256);
                  checksum:= checksum-(startaddr mod 256);
                  datastring:= ':'+IntToHex(numbytes,2)+IntToHex(startaddr div 256,2)+
                    IntToHex(startaddr mod 256,2)+'00'+datastring+
                    IntToHex(byte(checksum),2);
                  FDataHex.Add(datastring);
                  numbytes:= 0;
                end;
            end
          else if numbytes>0 then
            begin
              checksum:= checksum-numbytes;
              checksum:= checksum-(startaddr div 256);
              checksum:= checksum-(startaddr mod 256);
              datastring:= ':'+IntToHex(numbytes,2)+IntToHex(startaddr div 256,2)+
                IntToHex(startaddr mod 256,2)+'00'+datastring+
                IntToHex(byte(checksum),2);
              FDataHex.Add(datastring);
              numbytes:= 0;
            end;
        end;
      if numbytes>0 then
        begin
          checksum:= checksum-numbytes;
          checksum:= checksum-(startaddr div 256);
          checksum:= checksum-(startaddr mod 256);
          datastring:= ':'+IntToHex(numbytes,2)+IntToHex(startaddr div 256,2)+
            IntToHex(startaddr mod 256,2)+'00'+datastring+
            IntToHex(byte(checksum),2);
          FDataHex.Add(datastring);
        end;
      FDataHex.Add(':00000001FF');
    end
  else if FLastUpdate=luHex then
    begin
      segmentstart := 0;
      FillChar(FDataBin[0],FDataSize,UnusedBytes);
      FillChar(FDataUsed[0],FDataSize*SizeOf(boolean),false);
      for i:= 0 to FDataHex.Count-1 do
        begin
          s:= FDataHex.Strings[i];
          if s[1]=':' then
            begin
              checksum:= 0;
              numbytes:= HexToInt(s[2]+s[3]);
              checksum:= checksum-numbytes;
              startaddr:= 256*HexToInt(s[4]+s[5])+HexToInt(s[6]+s[7]);
              checksum:= checksum-(startaddr div 256);
              checksum:= checksum-(startaddr mod 256);
              blocktype:= HexToInt(s[8]+s[9]);
              checksum:= checksum-blocktype;
              if blocktype=0 then
                begin
                  for j:= 0 to numbytes-1 do
                    begin
                      try
                        DataBin[segmentstart+startaddr+j]:= HexToInt(s[10+j*2]+s[11+j*2]);
                        DataUsed[segmentstart+startaddr+j]:= true;
                      except
                        break;
                      end;
                      checksum:= checksum-DataBin[segmentstart+startaddr+j];
                    end;
                  if byte(checksum)<>HexToInt(s[10+numbytes*2]+s[11+numbytes*2]) then
                    raise Exception.Create(strFaultChecksum);
                end
              else if blocktype=4 then
                begin
                  segmentstart := (HexToInt(s[10]+s[11]) shl 8) + HexToInt(s[12]+s[13]);
                  segmentstart := segmentstart shl 16;
                end
              else if blocktype=1 then
                begin
                  if s<>':00000001FF' then
                    raise Exception.Create(strFaultFileEnd);
                  break;
                end
              else
                raise Exception.Create(strInvalidBlock);
            end
          else
            raise Exception.Create(strInvalidHexFile);
        end;
    end;
  FLastUpdate:= luNone;
  EndUpdate;
end;

procedure THexBuffer.SaveHexFile(Filename: TFilename);
begin
  FDataHex.SaveToFile(UniToSys(Filename));
end;

procedure THexBuffer.LoadHexFile(Filename: TFilename);
begin
  FDataHex.LoadFromFile(UniToSys(Filename));
  FLastUpdate:= luHex;
  if FAutoUpdate and (not FUpdating) then Update;
end;

procedure THexBuffer.SaveBinFile(Filename: TFilename);
var
  TheStream: TFileStream;
begin
  TheStream:=TFileStream.Create(FileName,fmCreate);
  try
    SaveBinStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure THexBuffer.LoadBinStream(aStream: TStream);
var
  i: Integer;
begin
  SetDataSize(aStream.Size-aStream.Position);
  aStream.Read(FDataBin[0],aStream.Size-aStream.Position);
  FLastUpdate:=luBin;
  for i := low(FDataBin) to high(FDataBin) do
    FDataUsed[i] := True;
  //if FAutoUpdate and (not FUpdating) then
  Update;
end;

procedure THexBuffer.SaveBinStream(aStream: TStream);
var
  i: Integer;
begin
  for i := low(FDataBin) to high(FDataBin) do
    aStream.WriteByte(FDataBin[i]);
end;

procedure THexBuffer.LoadBinFile(Filename: TFilename);
var
  TheStream: TFileStream;
begin
  TheStream:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadBinStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure THexBuffer.DataBinToBuffer(var Buffer);
begin
  Move(FDataBin,Buffer,FDataSize);
end;

function THexBuffer.SaveToFile(Filename: TFilename): Boolean;
begin
  Result := True;
  case lowercase(ExtractFileExt(Filename)) of
  '.hex','.eep','.a90':SaveHexFile(FileName);
  '.bin':SaveBinFile(FileName);
  else Result := False;
  end;
end;

function THexBuffer.LoadFromFile(FileName: TFilename): Boolean;
begin
  Result := True;
  case lowercase(ExtractFileExt(Filename)) of
  '.hex','.eep','.a90':LoadHexFile(FileName);
  '.bin':LoadBinFile(FileName);
  else Result := False;
  end;
end;

procedure THexBuffer.BufferToDataBin(var Buffer);
begin
  Move(Buffer,FDataBin,FDataSize);
  FLastUpdate:= luBin;
  if FAutoUpdate and (not FUpdating) then Update;
end;

procedure THexBuffer.DataUsedToBuffer(var Buffer);
begin
  Move(FDataUsed,Buffer,FDataSize);
end;

procedure THexBuffer.BufferToDataUsed(var Buffer);
begin
  Move(Buffer,FDataUsed,FDataSize);
  FLastUpdate:= luBin;
  if FAutoUpdate and (not FUpdating) then Update;
end;

procedure THexBuffer.BeginUpdate;
begin
  FUpdating:= true;
end;

procedure THexBuffer.EndUpdate;
begin
  FUpdating:= false;
  if FAutoUpdate and (FLastUpdate<>luNone) then Update;
end;

function THexBuffer.GetHexfileSize: integer;
var
  numbytes: integer;
  startaddr: integer;
  checksum: integer;
  blocktype: integer;
  i, j: integer;
  s: string;
  size: integer;
  segmentstart: Integer;
begin
  size:= 0;
  segmentstart := 0;
  for i:= 0 to FDataHex.Count-1 do
    begin
      s:= FDataHex.Strings[i];
      if s[1]=':' then
        begin
          checksum:= 0;
          numbytes:= HexToInt(s[2]+s[3]);
          checksum:= checksum-numbytes;
          startaddr:= 256*HexToInt(s[4]+s[5])+HexToInt(s[6]+s[7]);
          checksum:= checksum-(startaddr div 256);
          checksum:= checksum-(startaddr mod 256);
          blocktype:= HexToInt(s[8]+s[9]);
          checksum:= checksum-blocktype;
          if blocktype=0 then
            begin
              if size < segmentstart+startaddr+numbytes then
                size:= segmentstart+startaddr+numbytes;
              for j:= 0 to numbytes-1 do
                checksum:= checksum-HexToInt(s[10+j*2]+s[11+j*2]);
              if byte(checksum)<>HexToInt(s[10+numbytes*2]+s[11+numbytes*2]) then
                raise Exception.Create(strFaultChecksum);
            end
          else if blocktype=4 then
            begin
              segmentstart := (HexToInt(s[10]+s[11]) shl 8) + HexToInt(s[12]+s[13]);
              segmentstart := segmentstart shl 16;
            end
          else if blocktype=1 then
            begin
              if s<>':00000001FF' then
                raise Exception.Create(strFaultFileEnd);
              break;
            end
          else
            raise Exception.Create(strInvalidBlock);
        end
      else
        raise Exception.Create(strInvalidHexFile);
    end;
  if size > $FFFFFF then
    raise Exception.Create(strFileToBig);
  GetHexfileSize:= size;
end;

procedure THexBuffer.ReadFromString(Data: string);
begin
  FInvalid := false;
  while pos(';',Data) > 0 do
    begin
      if copy(Data,0,pos(':',Data)-1) = 'Filename' then
        begin
          Data := copy(Data,pos(':',Data)+1,length(Data));
          FFileName := copy(Data,0,pos(';',Data)-1);
          if FileExists(UniToSys(FFileName)) then
            LoadHexFile(FFileName);
        end
      else if copy(Data,0,pos(':',Data)-1) = 'Embedded' then
        begin
          Data := copy(Data,pos(':',Data)+1,length(Data));
          FEmbedded := copy(Data,0,pos(';',Data)-1) = 'true';
        end
      else if (copy(Data,0,pos(':',Data)-1) = 'Contents') and FEmbedded then
        begin
          Data := copy(Data,pos(':',Data)+1,length(Data));
          FDataHex.Text:=copy(Data,0,pos(';',Data)-1);
          FLastUpdate := luHex;
          try
            DataSize := getHexFileSize;
            Update;
          except
            FInvalid := True;
          end;
        end;
      Data := copy(Data,pos(';',Data)+1,length(Data));
    end;
end;

procedure THexBuffer.SaveToString(var Data: string);
begin
  if FEmbedded then
    Data := 'Embedded:true;'
  else
    Data := 'Embedded:false;';
  if Embedded then
    Data := Data+'Filename:'+ExtractFileName(FFilename)+';'
  else
    Data := Data+'Filename:'+FFilename+';';
  if FEmbedded then
    Data := Data+'Contents:'+FDataHex.Text+';';
end;

end.

