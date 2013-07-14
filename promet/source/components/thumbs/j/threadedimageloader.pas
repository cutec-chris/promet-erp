unit threadedimageLoader;

//22.6.2010 Theo

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, types, syncobjs, Forms, Graphics, fileutil, FPimage;

type

  EThreadedImageLoaderError = class(Exception);
  TLoadState = (lsEmpty, lsLoading, lsLoaded, lsError);

  { TThreadedImage }
  TLoaderThread = class;

  TThreadedImage = class
  private
    FOnThreadDone: TNotifyEvent;
    FOnThreadStart: TNotifyEvent;
    FArea: TRect;
    FBitmap: TBitmap;
    FHeight: integer;
    FImage: TFPMemoryImage;
    FOnLoaded: TNotifyEvent;
    FOnLoadURL: TNotifyEvent;
    FURL: UTF8String;
    FLoadState: TLoadState;
    FThread: TLoaderThread;
    FRect: TRect;
    FWidth: integer;
    FMultiThreaded: Boolean;
    function GetImage: TFPMemoryImage;
    function GetRect: TRect;
    procedure ThreadTerm(Sender: TObject);
    property OnThreadDone: TNotifyEvent read FOnThreadDone write FOnThreadDone;
    property OnThreadStart: TNotifyEvent read FOnThreadStart write FOnThreadStart;

  public
    constructor Create; overload;
    constructor Create(URL: UTF8String); overload;
    destructor Destroy; override;
    function Load(Reload: Boolean = False): Boolean;
    procedure FreeImage;
    property Image: TFPMemoryImage read GetImage;
    property Bitmap: TBitmap read FBitmap write FBitmap;
    property LoadState: TLoadState read FLoadState write FLoadState;
    property URL: UTF8String read FURL write FURL;
    property Left: integer read FRect.Left write FRect.Left;
    property Top: integer read FRect.Top write FRect.Top;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property Rect: TRect read GetRect;
    property Area: TRect read fArea write fArea;
  published
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnLoadURL: TNotifyEvent read FOnLoadURL write FOnLoadURL;
  end;

  { TLoaderThread }

  TLoaderThread = class(TThread)
  private
    fRef: TThreadedImage;
  protected
    procedure Execute; override;
  end;

  { TImageLoaderManager }

  TImageLoaderManager = class
  private
    FActiveIndex: integer;
    FFreeInvisibleImage: Boolean;
    fList: TObjectList;
    FMultiThreaded: boolean;
    FQueue: TList;
    FOnLoaded: TNotifyEvent;
    FOnLoadURL: TNotifyEvent;
    FReload: Boolean;
    FMaxThreads: integer;
    FThreadsFree: integer;
    procedure NextInQueue;
    procedure ThreadDone(Sender: TObject);
    procedure ThreadStart(Sender: TObject);
    function GetActiveItem: TThreadedImage;
    procedure SetActiveIndex(const AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function AddImage(URL: UTF8String): TThreadedImage;
    procedure LoadAll;
    procedure LoadRect(ARect: TRect);
    procedure StartQueue;
    function ItemIndexFromPoint(Point: TPoint): integer;
    function ItemFromPoint(Point: TPoint): TThreadedImage;
    procedure Clear;
    procedure FreeImages;
    function CountItems: integer;
    function ThreadsIdle:boolean;
    property List: TObjectList read fList write fList;
    property Reload: Boolean read FReload write FReload;
    property FreeInvisibleImage: Boolean read FFreeInvisibleImage write FFreeInvisibleImage;
    procedure Sort(stpye: byte);
    function ItemFromIndex(AValue: integer): TThreadedImage;
    property ActiveIndex: integer read FActiveIndex write SetActiveIndex;
    property ActiveItem: TThreadedImage read GetActiveItem;

  published
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnLoadURL: TNotifyEvent read FOnLoadURL write FOnLoadURL;
    property MultiThreaded: boolean read FMultiThreaded write FMultiThreaded;
  end;

var CSImg: TCriticalSection;

implementation

uses Math;

var CS: TCriticalSection;


{ TThreadedImage }

procedure TThreadedImage.ThreadTerm(Sender: TObject);
var aW, aH: integer;
begin
  FLoadState := lsEmpty;
  if FImage <> nil then
  begin
    aW := fImage.Width;
    aH := fImage.Height;
    if (aW > 0) and (aH > 0) then
    begin
      if fBitmap = nil then fBitmap := TBitmap.Create;
      try
        try
          CSImg.Acquire;
          fBitmap.Assign(fImage);
          fBitmap.Transparent:=false;
          FLoadState := lsError;
          FreeAndNil(fImage);
        except
          if fMultiThreaded then if Assigned(fOnThreadDone) then OnThreadDone(Self);
        end;
      finally
        CSImg.Release;
      end;
      FLoadState := lsLoaded;
      if fMultiThreaded then if Assigned(fOnLoaded) then OnLoaded(Self);
    end;
  end;
  if fMultiThreaded then if Assigned(fOnThreadDone) then OnThreadDone(Self);
end;

function TThreadedImage.GetRect: TRect;
begin
  fRect.Right := fRect.Left + fWidth;
  fRect.Bottom := fRect.Top + fHeight;
  Result := fRect;
end;


function TThreadedImage.GetImage: TFPMemoryImage;
begin
  Result := FImage;
end;

constructor TThreadedImage.Create;
begin
  FLoadState := lsEmpty;
  fMultiThreaded := False;
  FBitmap := nil;
  Fimage := nil;
end;

constructor TThreadedImage.Create(URL: UTF8String);
begin
  fURL := URL;
  Create;
end;

destructor TThreadedImage.Destroy;
begin
  fBitmap.free;
  inherited Destroy;
end;

function TThreadedImage.Load(Reload: Boolean): Boolean;
begin
  Result := True;
  if URL = '' then begin Result := False; exit; end;

  if (fLoadState = lsEmpty) or Reload then
  begin
    fLoadState := lsLoading;
    if Fimage = nil then
    begin
      FImage := TFPMemoryImage.Create(0, 0);
      FImage.UsePalette := false;
    end;

    if not fMultiThreaded then
    begin
      if Assigned(fOnLoadURL) then OnLoadURL(Self) else Image.LoadFromFile(UTF8ToSys(URL));
      ThreadTerm(self);
    end else
    begin
      fThread := TLoaderThread.Create(true);
      if Assigned(fOnThreadStart) then OnThreadStart(Self);
      fThread.fRef := Self;
      fThread.FreeOnTerminate := true;
      fThread.OnTerminate := @ThreadTerm;
      fThread.Resume;
    end;
  end else Result := false;
end;

procedure TThreadedImage.FreeImage;
begin
  fLoadState := lsEmpty;
  FreeAndNil(fBitmap);
end;


{ TLoaderThread }


procedure TLoaderThread.Execute;
begin
  if Assigned(fRef.OnLoadURL) then fRef.OnLoadURL(fRef) else
  begin
    fRef.Image.LoadFromFile(UTF8ToSys(fRef.URL));
  end;
end;


{ TImageLoaderManager }


function TImageLoaderManager.GetActiveItem: TThreadedImage;
begin
  Result := ItemFromIndex(fActiveIndex);
end;

procedure TImageLoaderManager.SetActiveIndex(const AValue: integer);
begin
  if (AValue > -1) and (AValue < fList.Count) then FActiveIndex := AValue;
end;

constructor TImageLoaderManager.Create;
begin
  fMultiThreaded := False;
  FList := TObjectList.Create;
  FQueue := TList.Create;
  FReload := false;
  FFreeInvisibleImage := false;
  FMaxThreads := 8;
  FThreadsFree := FMaxThreads;
end;

destructor TImageLoaderManager.Destroy;
begin
  FQueue.free;
  FList.free;
  inherited Destroy;
end;

function TImageLoaderManager.AddImage(URL: UTF8String): TThreadedImage;
begin
  Result := TThreadedImage.Create(URL);
  Result.FMultiThreaded := FMultiThreaded;
  if Assigned(FOnLoadURL) then Result.OnLoadURL := FOnLoadURL;
  if Assigned(FOnLoaded) then Result.OnLoaded := FOnLoaded;
  Result.OnThreadDone := @ThreadDone;
  Result.OnThreadStart := @ThreadStart;
  FList.Add(Result);
end;

procedure TImageLoaderManager.LoadAll;
var i: integer;
begin
  if not FMultiThreaded then
    for i := 0 to fList.Count - 1 do TThreadedImage(fList[i]).Load(FReload) else
  begin
    for i := 0 to fList.Count - 1 do
      if fQueue.IndexOf(TThreadedImage(fList[i])) < 0 then fQueue.Add(TThreadedImage(fList[i]));
    StartQueue;
  end;
end;

procedure TImageLoaderManager.LoadRect(ARect: TRect);
var i: integer;
  Dum: TRect;
begin
  FQueue.Clear;

  if not FMultiThreaded then
  begin
    for i := 0 to fList.Count - 1 do
      if IntersectRect(Dum, ARect, TThreadedImage(fList[i]).Rect) then
        TThreadedImage(fList[i]).Load(FReload) else if FFreeInvisibleImage then
        TThreadedImage(fList[i]).FreeImage;
  end else
  begin
    for i := 0 to fList.Count - 1 do
    begin
      if IntersectRect(Dum, ARect, TThreadedImage(fList[i]).Rect) then
      begin
        if fQueue.IndexOf(TThreadedImage(fList[i])) < 0 then fQueue.Add(TThreadedImage(fList[i]))
      end else if FFreeInvisibleImage then TThreadedImage(fList[i]).FreeImage;
    end;
    StartQueue;
  end;
end;

procedure TImageLoaderManager.StartQueue;
begin
  NextInQueue;
end;

procedure TImageLoaderManager.NextInQueue;
var i: integer;
begin
  if (fQueue.Count > 0) and (FThreadsFree > 0) then
  begin
    i := Min(fQueue.Count - 1, fThreadsFree);
    while i > -1 do
    begin
      if not TThreadedImage(fQueue[i]).Load(fReload) then
      begin
        fQueue.Delete(i);
        if fQueue.Count > i then inc(i);
      end;
      dec(i);
    end;
  end;
end;


procedure TImageLoaderManager.ThreadDone(Sender: TObject);
var idx: integer;
begin
  Inc(FThreadsFree);
  idx := fQueue.IndexOf(Sender);
  if idx > -1 then fQueue.Delete(idx);
  NextInQueue;
end;

procedure TImageLoaderManager.ThreadStart(Sender: TObject);
begin
  Dec(FThreadsFree);
end;


function TImageLoaderManager.ItemIndexFromPoint(Point: TPoint): integer;
var i: integer;
  aRect: TRect;
begin
  Result := -1;
  for i := 0 to fList.Count - 1 do
    if TThreadedImage(fList[i]).LoadState = lsLoaded then
    begin
      aRect := TThreadedImage(fList[i]).Rect;
      OffsetRect(aRect, TThreadedImage(fList[i]).Area.Left, TThreadedImage(fList[i]).Area.Top);
      if PtInRect(aRect, Point) then
      begin
        Result := i;
        break;
      end;
    end;
end;

function TImageLoaderManager.ItemFromPoint(Point: TPoint): TThreadedImage;
begin
  Result := ItemFromIndex(ItemIndexFromPoint(Point));
end;


procedure TImageLoaderManager.Clear;
begin
  FList.Clear;
end;

procedure TImageLoaderManager.FreeImages;
var i: integer;
begin
  try
    CSImg.Acquire;
    for i := 0 to fList.Count - 1 do TThreadedImage(fList[i]).FreeImage;
  finally
    CSImg.Release;
  end;
end;

function TImageLoaderManager.CountItems: integer;
begin
  CS.Acquire;
  try
    Result := fList.Count;
  finally
    CS.Release;
  end;
end;

function TImageLoaderManager.ThreadsIdle: boolean;
begin
  Result:=fThreadsFree=fMaxThreads;
end;

function sComp(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TThreadedImage(Item1).URL, TThreadedImage(Item2).URL);
end;

procedure TImageLoaderManager.Sort(stpye: byte);
begin
  fList.Sort(@sComp);
end;

function TImageLoaderManager.ItemFromIndex(AValue: integer): TThreadedImage;
begin
  if (AValue > -1) and (AValue < fList.Count) then Result := TThreadedImage(fList[AValue]) else
    Result := nil;
end;

initialization

  CS := TCriticalSection.Create;
  CSImg := TCriticalSection.Create;

finalization

  CSImg.free;
  CS.Free;


end.
