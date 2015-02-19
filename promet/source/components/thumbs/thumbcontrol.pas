// OnReleaseItem um das Object am Pointer zu Freen

unit thumbcontrol;

//22.6.2010 Theo
//Git push 30.1.2013

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, scrollingcontrol, ThreadedImageLoader, types,
  Graphics, fpImage, FPReadJPEGthumb, fpthumbresize,LResources,
  FileUtil, Dialogs, GraphType, LCLIntf, Controls,LMessages;


type
  TLayoutStyle = (LsAuto, LsAutoSize, LsHorizFixed, LsVertFixed, LsHorizAutoSize, LsVertAutoSize, LsGrid);
  TInternalLayoutStyle = (IlsHorz, IlsVert, IlsGrid);
  TSelectStyle = (ssAlpha, ssFrame);

  TSelectItemEvent = procedure(Sender: TObject; Item: TThreadedImage) of object;
  TLoadFileEvent = procedure(Sender: TObject; URL: string; out Stream: TStream) of object;
  TLoadPointerEvent = procedure(Sender: TObject; P: Pointer; out Stream: TStream) of object;
  TDrawItemEvent = procedure(Sender: TObject; Item: TThreadedImage;aRect : TRect) of object;

{ TThumbControl }

  TThumbControl = class(TScrollingControl)
  private
    FAfterDraw: TDrawItemEvent;
    FColorSelect: TColor;
    FOnDestroy: TNotifyEvent;
    fOnItemIndexChanged: TSelectItemEvent;
    FOnDrawCaption: TDrawItemEvent;
    FArrangeStyle: TLayoutStyle;
    FCaptionHeight: Integer;
    FIls: TInternalLayoutStyle;
    fContentWidth: integer;
    fContentHeight: integer;
    fResizeFactor: Double;
    FSelectStyle: TSelectStyle;
    fShiftSelectStartIDX: Integer;
    FDirectory: UTF8String;
    fMngr: TImageLoaderManager;
    FOnLoadFile: TLoadFileEvent;
    FOnLoadPointer: TLoadPointerEvent;
    FOnScrolled: TNotifyEvent;
    FThumbFrameWith: Integer;
    FShowCaptions: Boolean;
    fAutoSort: boolean;
    fThumbWidth: integer;
    fThumbHeight: integer;
    FURLList: TStringList;
    fMouseStartPos: TPoint;
    fDragIDX: Integer;
    fUserThumbWidth: integer;
    fUserThumbHeight: integer;
    fBackground: TBitmap;
    fThumbDistance: integer; //Distance between thumbnails
    fThumbLeftOffset: integer; //first frame left offset
    fThumbTopOffset: integer;
    fOnSelectItem: TSelectItemEvent;
    fWindowCreated: Boolean;
    fColorActiveFrame: TColor;
    fColorFont: TColor;
    fColorFontSelected: TColor;
    fGridThumbsPerLine: integer;
    function GetDraggedItem: TThreadedImage;
    function GetFreeInvisibleImages: boolean;
    function GetList: TList;
    function GetMultiThreaded: boolean;
    function GetSelectedList: TList;
    function GetURLList: UTF8String;
    procedure SetOnLoadFile(AValue: TLoadFileEvent);
    procedure SetOnLoadPointer(AValue: TLoadPointerEvent);
    procedure SetSelectStyle(AValue: TSelectStyle);
    procedure SetThumbFrameWith(AValue: integer);
    procedure SetThumbLeftOffset(AValue: integer);
    procedure SetThumbTopOffset(AValue: integer);
    function TColorToBGRInteger(Val: TColor): Integer;
    procedure Init;
    procedure SetArrangeStyle(const AValue: TLayoutStyle);
    procedure SetAutoSort(AValue: boolean);
    procedure SetCaptionHeight(AValue: Integer);
    procedure SetColorSelect(AValue: TColor);
    procedure SetDirectory(const AValue: UTF8String);
    procedure SetFreeInvisibleImages(const AValue: boolean);
    procedure SetMultiThreaded(const AValue: boolean);
    procedure SetShowCaptions(const AValue: Boolean);
    procedure SetThumbDistance(const AValue: integer);
    procedure SetThumbHeight(const AValue: integer);
    procedure SetThumbWidth(const AValue: integer);
    procedure AsyncFocus(Data: PtrInt);
    procedure SetURLList(const AValue: UTF8String);
    procedure DoClick(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure ItemIndexChanged(Sender: TObject);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure BoundsChanged; override;
    procedure Paint; override;
    procedure DblClick; override;
    procedure ImgLoadURL(Sender: TObject);
    procedure ImgLoadPointer(Sender: TObject);
    procedure ImgRepaint(Sender: TObject);
    procedure Search;
    procedure FileFoundEvent(FileIterator: TFileIterator);
    procedure CreateWnd; override;
    procedure DoSelectItem; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure UpdateDims;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {:Use this function when you need the item from control coordinates (Mouse etc.)}
    function ItemFromPoint(APoint: TPoint): TThreadedImage;
    function AddImage(Image: TThreadedImage): Integer;
    procedure DeleteSelectedImages;
    procedure ScrollIntoView;
    procedure LoadSelectedBitmap(ABitmap:TBitmap);
    procedure ClearImageList;
    procedure AddImagePointer(P: Pointer);
    procedure Arrange;
    property URLList: UTF8String read GetURLList write SetURLList;
    property ImageLoaderManager: TImageLoaderManager read fMngr;
  published
    property Directory: UTF8String read FDirectory write SetDirectory stored True nodefault;
    property ThumbWidth: integer read fUserThumbWidth write SetThumbWidth;
    property ThumbHeight: integer read fUserThumbHeight write SetThumbHeight;
    property ThumbDistance: integer read fThumbDistance write SetThumbDistance;
    property ThumbFrameWith: integer read FThumbFrameWith write SetThumbFrameWith;
    property ThumbLeftOffset: integer read fThumbLeftOffset write SetThumbLeftOffset;
    property ThumbTopOffset: integer read fThumbTopOffset write SetThumbTopOffset;
    {:If you set ThumbResizeFactor to <>0 , the images will be resized with this Factor
      If they are still to Big after applying the Factor, they will bo resiized to fit ThumbWith and ThumbHeight}
    property ThumbResizeFactor: Double read fResizeFactor write fResizeFactor;
    {:If you set MultiThreaded to true, the images will be loaded in the "background" not blocking the application.
      Warning: The debugger GDB may not like this setting. Be careful in OnLoadFile in this mode}
    property MultiThreaded: boolean read GetMultiThreaded write SetMultiThreaded;
    {:Show/Hide the filename captions.}
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions;
    {:Different modes, basically horizontal, vertical and grid plus autosize and auto layout modes, depending on the size of the control}
    property Layout: TLayoutStyle read FArrangeStyle write SetArrangeStyle;
    property SelectStyle: TSelectStyle read FSelectStyle write SetSelectStyle;
    {:Do not keep in memory the bitmaps that are currently invisble. (Slower but less resource hungry).}
    property FreeInvisibleImages: boolean read GetFreeInvisibleImages write SetFreeInvisibleImages;
    {:Event triggered when a thumbnail is doubleclicked or selected using the enter key.}
    property OnSelectItem: TSelectItemEvent read fOnSelectItem write fOnSelectItem;
    {:Event triggered when ActiveIndex is changed.}
    property OnItemIndexChanged: TSelectItemEvent read fOnItemIndexChanged write fOnItemIndexChanged;
     {:Event when image stream data is required. Useful for loading data via http, ftp etc.
      Warning: if MultiThreaded=true, this happens in a separate thread context.}
    property AutoSort : boolean read fAutoSort write SetAutoSort;
    {:Sort URL by name}
    property OnLoadFile: TLoadFileEvent read FOnLoadFile write SetOnLoadFile;
    property OnLoadPointer: TLoadPointerEvent read FOnLoadPointer write SetOnLoadPointer;
    property OnScrolled : TNotifyEvent read FOnScrolled write FOnScrolled;
    property AfterDraw : TDrawItemEvent read FAfterDraw write FAfterDraw;
    property OnDrawCaption: TDrawItemEvent read FOnDrawCaption write FOnDrawCaption;
    property SelectedList: TList read GetSelectedList;
    property List: TList read GetList;
    property DragedItem: TThreadedImage read GetDraggedItem;
    property ScrollBars;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property ColorActiveFrame: TColor read fColorActiveFrame write fColorActiveFrame;
    property ColorSelect: TColor read FColorSelect write SetColorSelect;
    property ColorFont: TColor read fColorFont write fColorFont;
    property ColorFontSelected: TColor read fColorFontSelected write fColorFontSelected;
    property CaptionHight: Integer read FCaptionHeight write SetCaptionHeight;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnUTF8KeyPress;


  end;

var frame: TPortableNetworkGraphic;

procedure Register;

implementation

uses LCLType, Forms, fontfinder,
fpreadgif,FPReadPSD,FPReadPCX,FPReadTGA; //just register them


function ShortenString(AValue: string; Width: integer; ACanvas: TCanvas): string;
var len, slen: integer;
  NewLen: integer;
begin
  len := ACanvas.TextWidth(AValue);
  if len > width then
  begin
    NewLen := ((Length(AValue) * width) div len) - 1;
    slen := Length(AValue);
//  Result:=Copy(AValue,1,NewLen); //End crop
    Result := Copy(AValue, 1, (NewLen) div 2) + '..' + Copy(AValue, slen - ((NewLen) div 2), slen);
  end else Result := AValue;
end;


{ TThumbControl }

procedure TThumbControl.SetDirectory(const AValue: UTF8String);
begin
  if GetMultiThreaded and (not fMngr.ThreadsIdle) then
  Repeat
    Application.ProcessMessages;
  until fMngr.ThreadsIdle;

  if AValue = '' then fDirectory := 'none' else fDirectory := AValue;
  if (fDirectory <> 'none') and (fDirectory <> '') then
    if DirectoryExistsUTF8(AValue) then
    begin
      if (csLoading in ComponentState) then exit;
      Init;
      Invalidate;
    end;
end;


procedure TThumbControl.Init;
begin
  if not (csDesigning in ComponentState) then
  begin
    fMngr.Clear;
    Search;
    if fAutoSort then fMngr.Sort(0);
    Arrange;
  end else
  begin
    fMngr.Clear;
    fMngr.AddImage('');
    fMngr.AddImage('');
    fMngr.AddImage('');
    Arrange;
  end;
end;



procedure TThumbControl.SetFreeInvisibleImages(const AValue: boolean);
begin
  fMngr.FreeInvisibleImage := AValue;
end;


procedure TThumbControl.SetURLList(const AValue: UTF8String);
var i: integer;
begin
  fMngr.Clear;
  FURLList.Text := AValue;
  for i := 0 to FURLList.Count - 1 do fMngr.AddImage(FURLList[i]);
  if fAutoSort then fMngr.Sort(0);
  Arrange;
end;

procedure TThumbControl.DoClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Idx: Integer;
begin
  Idx := fMngr.ItemIndexFromPoint(Point(X + HScrollPosition, Y + VScrollPosition));
  if Idx > -1 then begin
    if Shift=[] then begin
      fMngr.DeselectAll;
      fMngr.ActiveIndex := Idx;
      Invalidate;
    end else if Shift=[ssShift] then begin
      fMngr.SetActiveIndexAndSelectBetween(Idx);
      Invalidate;
    end else if Shift=[ssCtrl] then begin
      fMngr.SetActiveIndexAndChangeSelected(Idx);
      Invalidate;
    end;
  end;
  SetFocus;
end;

function TThumbControl.GetMultiThreaded: boolean;
begin
  if Assigned(fMngr) then Result := fMngr.MultiThreaded;
end;

function TThumbControl.GetSelectedList: TList;
begin
  if Assigned(fMngr) then Result:=fMngr.SelectedList;
end;

function TThumbControl.GetFreeInvisibleImages: boolean;
begin
  Result := fMngr.FreeInvisibleImage;
end;

function TThumbControl.GetList: TList;
begin
  if Assigned(fMngr) then Result:=fMngr.List;
end;

function TThumbControl.GetDraggedItem: TThreadedImage;
begin
  Result := fMngr.ItemFromIndex(fDragIDX);
end;

function TThumbControl.GetURLList: UTF8String;
begin
  Result := FURLList.Text;
end;

procedure TThumbControl.SetOnLoadFile(AValue: TLoadFileEvent);
begin
  FOnLoadFile:=AValue;
  if csDesigning in ComponentState then exit;
  if Assigned(fMngr) then begin
    if Assigned(FOnLoadFile) then begin
      fMngr.OnLoadURL:=@ImgLoadURL;
    end else begin
      fMngr.OnLoadURL:=Nil;
    end;
  end;
end;

procedure TThumbControl.SetOnLoadPointer(AValue: TLoadPointerEvent);
begin
  if FOnLoadPointer=AValue then Exit;
  FOnLoadPointer:=AValue;
  if Assigned(fMngr) then begin
    if Assigned(FOnLoadPointer) then begin
      fMngr.OnLoadPointer:=@ImgLoadPointer;
    end else begin
      fMngr.OnLoadPointer:=Nil;
    end;
  end;
end;

procedure TThumbControl.SetSelectStyle(AValue: TSelectStyle);
begin
  if FSelectStyle <> AValue then begin
    FSelectStyle := AValue;
    if not (csLoading in ComponentState) then begin
      Arrange;
      Invalidate;
    end;
  end;
end;

procedure TThumbControl.SetThumbFrameWith(AValue: integer);
begin
  if FThumbFrameWith<>AValue then begin
    FThumbFrameWith:=AValue;
    if Assigned(fMngr) then fMngr.ThumbFrameWith:=FThumbFrameWith;
    if not (csLoading in ComponentState) then begin
      Arrange;
      Invalidate;
    end;
  end;
end;

procedure TThumbControl.SetThumbLeftOffset(AValue: integer);
begin
  if fThumbLeftOffset <> AValue then
  begin
    fThumbLeftOffset := AValue;
    if not (csLoading in ComponentState) then
    begin
      Arrange;
      Invalidate;
    end;
  end;
end;

procedure TThumbControl.SetThumbTopOffset(AValue: integer);
begin
  if fThumbTopOffset <> AValue then
  begin
    fThumbTopOffset := AValue;
    if not (csLoading in ComponentState) then
    begin
      Arrange;
      Invalidate;
    end;
  end;
end;

function TThumbControl.TColorToBGRInteger(Val: TColor): Integer;
begin
  Result:=((Val and $00ff0000) shr 16) or
          (Val and $0000ff00) or
          ((Val and $000000ff) shl 16);
end;

procedure TThumbControl.SetArrangeStyle(const AValue: TLayoutStyle);
begin
  if FArrangeStyle <> AValue then
  begin
    FArrangeStyle := AValue;
    if not (csLoading in ComponentState) then
    begin
      Arrange;
      Invalidate;
    end;
  end;
end;

procedure TThumbControl.SetAutoSort(AValue: boolean);
begin
  if fAutoSort=AValue then Exit;
  fAutoSort:=AValue;
  fMngr.Sort(0);
end;

procedure TThumbControl.SetCaptionHeight(AValue: Integer);
begin
  if FCaptionHeight=AValue then Exit;
  FCaptionHeight:=AValue;
  if Assigned(fMngr) then fMngr.CaptionHeight:=FCaptionHeight;
  Arrange;
end;

procedure TThumbControl.SetColorSelect(AValue: TColor);
begin
  if FColorSelect<>AValue then begin
    FColorSelect:=AValue;
    if Assigned(fMngr) then begin
      fMngr.SelectColor:=FColorSelect;
    end;
  end;
end;

procedure TThumbControl.SetMultiThreaded(const AValue: boolean);
begin
  if Assigned(fMngr) then fMngr.MultiThreaded := AValue;
end;


procedure TThumbControl.SetShowCaptions(const AValue: Boolean);
begin
  FShowCaptions := AValue;
  if not (csLoading in ComponentState) then
  begin
    Arrange;
    Invalidate;
  end;
end;

procedure TThumbControl.SetThumbDistance(const AValue: integer);
begin
  if fThumbDistance <> AValue then
  begin
    fThumbDistance := AValue;
    if not (csLoading in ComponentState) then
    begin
      Arrange;
      Invalidate;
    end;
  end;
end;

procedure TThumbControl.SetThumbHeight(const AValue: integer);
begin
  if fThumbHeight <> AValue then
  begin
    fThumbHeight := AValue;
    fUserThumbHeight := fThumbHeight;
    if not (csLoading in ComponentState) then
    begin
      Arrange;
      Invalidate;
    end;
  end;
end;

procedure TThumbControl.SetThumbWidth(const AValue: integer);
begin
  if fThumbWidth <> AValue then
  begin
    fThumbWidth := AValue;
    fUserThumbWidth := AValue;
    if not (csLoading in ComponentState) then
    begin
      Arrange;
      Invalidate;
    end;
  end;
end;


procedure TThumbControl.CreateWnd;
begin
  inherited CreateWnd;
  fWindowCreated := true;
  Init;
end;

procedure TThumbControl.DoSelectItem;
begin
  if Assigned(fOnSelectItem) then OnSelectItem(Self, fMngr.ActiveItem);
end;

procedure TThumbControl.ItemIndexChanged(Sender: TObject);
begin
  if Assigned(fOnItemIndexChanged) then OnItemIndexChanged(Self, fMngr.ActiveItem);
end;

procedure TThumbControl.AsyncFocus(Data: PtrInt);
begin
  SetFocus;
end;

procedure TThumbControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Shift=[] then begin
    case key of
      VK_LEFT: begin fMngr.ActiveIndex := fMngr.ActiveIndex - 1; ScrollIntoView; end;
      VK_RIGHT: begin fMngr.ActiveIndex := fMngr.ActiveIndex + 1; ScrollIntoView; end;

      VK_UP: if FIls = IlsGrid then
        begin
          fMngr.ActiveIndex := fMngr.ActiveIndex - fGridThumbsPerLine; ScrollIntoView;
        end else
        begin fMngr.ActiveIndex := fMngr.ActiveIndex - 1;
          ScrollIntoView;
        end;

      VK_DOWN: if FIls = IlsGrid then
        begin
          fMngr.ActiveIndex := fMngr.ActiveIndex + fGridThumbsPerLine; ScrollIntoView;
        end else
        begin fMngr.ActiveIndex := fMngr.ActiveIndex + 1;
          ScrollIntoView;
        end;
      VK_RETURN: DoSelectItem;
      VK_PRIOR: if (FIls = IlsVert) or (FIls = IlsGrid) then
          VScrollPosition := VScrollPosition - ClientHeight else HScrollPosition := HScrollPosition - ClientWidth;
      VK_NEXT: if (FIls = IlsVert) or (FIls = IlsGrid) then
          VScrollPosition := VScrollPosition + ClientHeight else HScrollPosition := HScrollPosition + ClientWidth;
      VK_SPACE: fMngr.SetActiveIndexAndChangeSelected(fMngr.ActiveIndex);
    end;
  end else if Shift=[ssShift] then begin
    case key of
      VK_SHIFT: fShiftSelectStartIDX := fMngr.ActiveIndex;
      VK_LEFT:
      begin
        fMngr.ActiveIndex := fMngr.ActiveIndex - 1; ScrollIntoView;
        if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
      end;
      VK_RIGHT:
      begin
        fMngr.ActiveIndex := fMngr.ActiveIndex + 1; ScrollIntoView;
        if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
      end;
      VK_UP: if FIls = IlsGrid then
        begin
          fMngr.ActiveIndex := fMngr.ActiveIndex - fGridThumbsPerLine; ScrollIntoView;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end else
        begin
          fMngr.ActiveIndex := fMngr.ActiveIndex - 1;
          ScrollIntoView;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end;
      VK_DOWN: if FIls = IlsGrid then
        begin
          fMngr.ActiveIndex := fMngr.ActiveIndex + fGridThumbsPerLine; ScrollIntoView;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end else
        begin
          fMngr.ActiveIndex := fMngr.ActiveIndex + 1;
          ScrollIntoView;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end;
      VK_PRIOR: if (FIls = IlsVert) or (FIls = IlsGrid) then
        begin
          VScrollPosition := VScrollPosition - ClientHeight;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end else begin
          HScrollPosition := HScrollPosition - ClientWidth;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end;
      VK_NEXT: if (FIls = IlsVert) or (FIls = IlsGrid) then
        begin
          VScrollPosition := VScrollPosition + ClientHeight;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end else begin
          HScrollPosition := HScrollPosition + ClientWidth;
          if fShiftSelectStartIDX>-1 then fMngr.SelectBetween(fShiftSelectStartIDX,fMngr.ActiveIndex);
        end;
    end;
  end else if Shift=[ssCtrl] then begin
    case Key of
			ord('A'): fMngr.SelectAll;
			ord('D'): fMngr.DeselectAll;
    end;
  end;
  Invalidate;
  Application.QueueAsyncCall(@AsyncFocus, 0);
end;

procedure TThumbControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Shift=[ssShift] then begin
    case Key of
      VK_SHIFT: fShiftSelectStartIDX:=-1;
    end;
  end;
  inherited KeyUp(Key, Shift);
  SetFocus;
end;

procedure TThumbControl.ScrollIntoView;
var itm: TThreadedImage;
  Dum, ARect: TRect;
begin
  itm := fMngr.ActiveItem;
  if itm <> nil then
  begin
    ARect := ClientRect;
    OffsetRect(ARect, HScrollPosition, VScrollPosition);

    if IntersectRect(Dum, ARect, itm.Rect) then exit;

    HScrollPosition := 0;
    VScrollPosition := 0;

    if (FIls = IlsHorz) then
      if Abs(Arect.Left - Itm.Rect.Left) > (Arect.Right - Itm.Rect.Right) then
        HScrollPosition := itm.Rect.Right - ClientWidth + FThumbFrameWith + fThumbDistance else
        HScrollPosition := itm.Rect.Left - ClientWidth - FThumbFrameWith - fThumbDistance + ClientWidth;

    if (FIls = IlsVert) or (FIls = IlsGrid) then
      if Abs(Arect.Top - Itm.Rect.Top) > (Arect.Bottom - Itm.Rect.Bottom) then
        VScrollPosition := itm.Rect.Bottom - ClientHeight + FThumbFrameWith + fThumbDistance else
        VScrollPosition := itm.Rect.Top - ClientHeight - FThumbFrameWith - fThumbDistance + ClientHeight;
    UpdateDims;
  end;
end;



procedure TThumbControl.BoundsChanged;
begin
  inherited BoundsChanged;
  if fWindowCreated and not ((csLoading in ComponentState)) and Visible then
  begin
    Arrange;
    UpdateDims;
    ScrollIntoView;
  end;
end;


procedure TThumbControl.Paint;
var i, tlen: integer;
  aRect, BorderRect, Dum: TRect;
  UrlStr: string;
  Clipped: boolean;
  Cim: TThreadedImage;
begin
  begin
    if Canvas.Clipping {$ifdef LCLQt} and false {$endif} then
    begin
      ARect := Canvas.ClipRect;
      Clipped := not EqualRect(ARect, ClientRect);
    end else
    begin
      ARect := ClientRect;
      Clipped := false;
    end;

    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    OffsetRect(aRect, HScrollPosition, VScrollPosition);
    if not clipped then fMngr.LoadRect(ARect);

    Canvas.Brush.color :=  $F1F1F1;

    for i := 0 to fmngr.List.Count - 1 do begin
      Cim := TThreadedImage(fmngr.List[i]);
      BorderRect := Cim.Rect;
      if IntersectRect(Dum, BorderRect, aRect) then begin
        if Cim.LoadState = lsLoaded then begin
          // Draw Thumb
          case FSelectStyle of
            ssFrame:
            begin
              if Cim.Selected then begin
                BorderRect := Cim.Rect;
                InflateRect(BorderRect, FThumbFrameWith, FThumbFrameWith);
                OffSetRect(BorderRect, -HScrollPosition, -VScrollPosition);
                Canvas.Brush.Color := FColorSelect;
                Canvas.FillRect(BorderRect);
              end;
              Canvas.Draw(Cim.Left + Cim.Area.Left - HScrollPosition,
                Cim.Top + Cim.Area.Top - VScrollPosition,
                Cim.Bitmap);
            end;
            ssAlpha:
            begin
              if (Cim.Selected) then begin
                Canvas.Draw(Cim.Left + Cim.Area.Left - HScrollPosition,
                  Cim.Top + Cim.Area.Top - VScrollPosition,
                  Cim.BitmapSelected);
              end else begin
                Canvas.Draw(Cim.Left + Cim.Area.Left - HScrollPosition,
                  Cim.Top + Cim.Area.Top - VScrollPosition,
                  Cim.Bitmap);
              end;
            end;
          end;
        end;

        if (i=fMngr.ActiveIndex) and Focused then
        begin
          BorderRect := Cim.Rect;
          OffSetRect(BorderRect, -HScrollPosition, -VScrollPosition);
          Canvas.Brush.Style := bsClear;
          Canvas.Pen.Color := fColorActiveFrame;
          Canvas.Pen.Width :=1;
          Canvas.Rectangle(BorderRect);
          InflateRect(BorderRect,-1,-1);
          Canvas.Rectangle(BorderRect);
          InflateRect(BorderRect,-1,-1);
          Canvas.Rectangle(BorderRect);
          Canvas.Brush.Style := bsSolid;
        end;

        if fShowCaptions then begin
          // Draw Caption
          if Assigned(FOnDrawCaption) then begin
            BorderRect:= Rect(Cim.Rect.Left,Cim.Rect.Bottom+FThumbFrameWith,Cim.Rect.Right,Cim.Rect.Bottom+fCaptionHeight+FThumbFrameWith);
            OffSetRect(BorderRect, -HScrollPosition, -VScrollPosition);
            OnDrawCaption(Self,Cim,BorderRect);
          end else begin
            if Cim.URL = '' then
              UrlStr := ShortenString('Undefined', Cim.Width, Canvas) else
              UrlStr := ShortenString(ExtractFileName(Cim.URL),
                Cim.Width, Canvas);
            tlen := (Cim.Width - Canvas.TextWidth(UrlStr)) div 2;
            Canvas.Font.color := fColorFont;
            Canvas.Brush.Style := bsClear;
            Canvas.TextOut(
              Cim.Left - HScrollPosition + 1 + tlen,
              Cim.Height + Cim.Top - VScrollPosition - 1,
              UrlStr);
            Canvas.Brush.Style := bsSolid;
          end;
        end;
      if Assigned(AfterDraw) then begin
          BorderRect := Cim.Rect;
          OffSetRect(BorderRect, -HScrollPosition, -VScrollPosition);
          AfterDraw(Self,Cim,BorderRect);
        end;
      end;
    end;
  end;
  inherited Paint;
end;

procedure TThumbControl.DblClick;
begin
  DoSelectItem;
  inherited DblClick;
end;

function GetFPReaderMask: string;
var i, j: integer;
  sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  sl.Delimiter := ';';
  for i := 0 to ImageHandlers.Count - 1 do
  begin
    sl.DelimitedText := ImageHandlers.Extentions[ImageHandlers.TypeNames[i]];
    for j := 0 to sl.Count - 1 do Result := Result + '*.' + sl[j] + ';';
  end;
  sl.free;
end;

procedure TThumbControl.Search;
var fi: TFileSearcher;
begin
  fi := TFileSearcher.Create;
  fi.OnFileFound := @FileFoundEvent;
//fi.Search(FDirectory, GraphicFileMask(TGraphic), false);
  try
    fi.Search(FDirectory, GetFPReaderMask, false);
  finally
    fi.free;
  end;
end;

procedure TThumbControl.FileFoundEvent(FileIterator: TFileIterator);
begin
  fMngr.AddImage(FileIterator.FileName);
end;


procedure TThumbControl.Arrange;
var i, x, y, aDim: integer;
begin
  if FArrangeStyle = LsHorizFixed then FIls := IlsHorz else
    if FArrangeStyle = LsVertFixed then FIls := IlsVert else
      if FArrangeStyle = LsGrid then FIls := IlsGrid;
  if (FArrangeStyle = LsAuto) or (FArrangeStyle = LsAutoSize) then
  begin
    if width > height then FIls := IlsHorz else FIls := IlsVert;
    if (width > 2 * fUserThumbWidth + 4 * FThumbFrameWith) and
    (Height > 2 * fUserThumbHeight + 4 * FThumbFrameWith) then FIls := IlsGrid;
  end;

  if (FArrangeStyle = LsHorizFixed) or (FArrangeStyle = LsVertFixed) or
    (FArrangeStyle = LsGrid) or (FArrangeStyle = LsAuto) or (FIls = IlsGrid) then
  begin
    if (fUserThumbWidth <> fThumbWidth) or (fUserThumbHeight <> fThumbHeight) then
    begin
      fThumbHeight := fUserThumbHeight;
      fThumbWidth := fUserThumbWidth;
      fMngr.FreeImages;
      Invalidate;
    end;
  end;

  if (FArrangeStyle = LsHorizAutoSize) or ((FArrangeStyle = LsAutoSize) and (FIls = IlsHorz)) then
  begin
    aDim := fThumbHeight;
    fThumbHeight := ClientHeight - FThumbFrameWith * 2 - fCaptionHeight - 2 * fThumbTopOffset - 20;
    fThumbWidth := Round(fThumbHeight / fUserThumbHeight * fUserThumbWidth);
    FIls := IlsHorz;
    if ADim <> fThumbHeight then
    begin
      fMngr.FreeImages;
      Invalidate;
    end;
  end;

  if (FArrangeStyle = LsVertAutoSize) or ((FArrangeStyle = LsAutoSize) and (FIls = IlsVert)) then
  begin
    aDim := fThumbWidth;
    fThumbWidth := ClientWidth - FThumbFrameWith * 2 - 2 * fThumbLeftOffset - 20;
    fThumbHeight := round(fThumbWidth / fUserThumbWidth * fUserThumbHeight);
    fThumbHeight := round(fThumbWidth / fUserThumbWidth * fUserThumbHeight);
    FIls := IlsVert;
    if ADim <> fThumbWidth then
    begin
      fMngr.FreeImages;
      Invalidate;
    end;
  end;

  //if FShowPictureFrame then
  //begin
  //  fBackground.SetSize(fThumbWidth + FThumbFrameWith * 2, fThumbHeight + FThumbFrameWith * 2);
  //  fBackground.Canvas.Brush.FPColor := TColorToFPColor(Color);
  //  fBackground.Canvas.FillRect(0, 0, fBackground.Width, fBackground.Height);
  //  fBackground.Canvas.StretchDraw(Rect(0, 0, fBackground.Width, fBackground.Height), frame);
  //end;

  if FIls = IlsHorz then
  begin
    for i := 0 to fMngr.List.Count - 1 do
    begin
      TThreadedImage(fMngr.List[i]).Width := fThumbWidth;
      TThreadedImage(fMngr.List[i]).Height := fThumbHeight;
      TThreadedImage(fMngr.List[i]).Left := i * (fThumbWidth + fThumbDistance +
      FThumbFrameWith * 2) +  fThumbLeftOffset + FThumbFrameWith;
      TThreadedImage(fMngr.List[i]).Top := fThumbTopOffset + FThumbFrameWith;
    end;
    fContentWidth := fMngr.List.Count * (fThumbWidth + fThumbDistance + FThumbFrameWith * 2) + fThumbLeftOffset;
    fContentHeight := fThumbHeight + FThumbFrameWith * 2 + fThumbTopOffset;
  end;

  if FIls = IlsVert then
  begin
    for i := 0 to fMngr.List.Count - 1 do
    begin
      TThreadedImage(fMngr.List[i]).Width := fThumbWidth;
      TThreadedImage(fMngr.List[i]).Height := fThumbHeight;
      TThreadedImage(fMngr.List[i]).Left := fThumbLeftOffset + FThumbFrameWith;
      TThreadedImage(fMngr.List[i]).Top := i * (fThumbHeight + fThumbDistance +
      fCaptionHeight + FThumbFrameWith * 2) + fThumbTopOffset + FThumbFrameWith;
    end;
    fContentHeight := (fMngr.List.Count) * (fThumbHeight + fCaptionHeight + fThumbDistance +
    FThumbFrameWith * 2) + fThumbTopOffset;
    fContentWidth := fThumbWidth + FThumbFrameWith * 2 + fThumbLeftOffset;
  end;

  if FIls = IlsGrid then
  begin
    SmallStep := fThumbHeight+fThumbDistance+fCaptionHeight+FThumbFrameWith*2;
    LargeStep := (ClientHeight div (fThumbHeight+fThumbDistance+fCaptionHeight+FThumbFrameWith*2))*
                 (fThumbHeight+fThumbDistance+fCaptionHeight+FThumbFrameWith*2);
    y := 0;
    x := 0;
    fGridThumbsPerLine := ClientWidth div (fThumbWidth + fThumbDistance + FThumbFrameWith * 2);
    if fGridThumbsPerLine<1 then fGridThumbsPerLine:=1;
    for i := 0 to fMngr.List.Count - 1 do
    begin
      if (i > 0) then
        if (i mod fGridThumbsPerLine = 0) then
        begin
          inc(y, (fThumbHeight + fThumbDistance + fCaptionHeight + FThumbFrameWith * 2));
          x := 0;
        end
        else inc(x);
      TThreadedImage(fMngr.List[i]).Width := fThumbWidth;
      TThreadedImage(fMngr.List[i]).Height := fThumbHeight;
      TThreadedImage(fMngr.List[i]).Left := x * (fThumbWidth + fThumbDistance + FThumbFrameWith * 2) +
      fThumbLeftOffset + FThumbFrameWith;
      TThreadedImage(fMngr.List[i]).Top := y + fThumbTopOffset + FThumbFrameWith;
    end;
    fContentHeight := (fMngr.List.Count div fGridThumbsPerLine + 1) * (fThumbHeight +
    fCaptionHeight + fThumbDistance + FThumbFrameWith * 2) + fThumbTopOffset;
    fContentWidth := ClientWidth;
  end;


  if fContentWidth <= ClientWidth then HScrollPosition := 0;
  if fContentHeight <= ClientHeight then VScrollPosition := 0;
  UpdateDims;
end;

constructor TThumbControl.Create(AOwner: TComponent);
//var ff: TFontFinder;
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);

  FURLList := TStringList.create;

  //ff := TFontFinder.Create;
  ////Font.Name := ff.FindAFontFromDelimitedString('Trebuchet MS,Schumacher Clean');
  ////Font.size := 7;
  //Font.Name := ff.FindAFontFromDelimitedString('Arial');
  //Font.size := 14;
  //ff.free;

  fColorActiveFrame:=clRed;
  fColorFont:=clBlack;
  fColorFontSelected:=clwhite;
  FCaptionHeight:=8;

  fWindowCreated := false;
  DoubleBuffered := true;
  fThumbWidth := 80;
  fThumbHeight := 80;
  fResizeFactor:=0;
  fUserThumbWidth := fThumbWidth;
  fUserThumbHeight := fThumbHeight;
  FSelectStyle:=ssAlpha;

  fContentWidth := GetControlClassDefaultSize.cx;
  fContentHeight := GetControlClassDefaultSize.cy;

  fThumbDistance := 5;
  fThumbLeftOffset := fThumbDistance;
  fThumbTopOffset := fThumbDistance;
  FThumbFrameWith := 5;

  FShowCaptions := true;

  FColorSelect:=$00FFBFBF;

  fMngr := TImageLoaderManager.Create;
  fMngr.OnNeedRepaint := @ImgRepaint;
  fMngr.OnItemIndexChanged:= @ItemIndexChanged;
  fMngr.SelectColor:=FColorSelect;
  fMngr.CaptionHeight:=FCaptionHeight;
  fMngr.ThumbFrameWith:=FThumbFrameWith;
  fAutoSort:=true;

  SmallStep := fThumbWidth;
  LargeStep := fThumbWidth * 4;

  fBackground := TBitmap.Create;
  fDirectory := GetUserDir;
end;

destructor TThumbControl.Destroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  FURLList.Free;
  fMngr.Free;
  fBackground.Free;
  inherited Destroy;
end;

procedure TThumbControl.UpdateDims;
begin
  if (VScrollInfo.nMax <> fContentHeight) or (VScrollInfo.nPage <> ClientHeight) then
  begin
    fVScrollInfo.nPage := ClientHeight;
    fVScrollInfo.nMax := fContentHeight;
    CanShowV := VScrollInfo.nMax > VScrollInfo.nPage;
    UpdateVScrollInfo;
  end;
  if (HScrollInfo.nMax <> fContentWidth) or (HScrollInfo.nPage <> ClientWidth) then
  begin
    fHScrollInfo.nPage := ClientWidth;
    fHScrollInfo.nMax := fContentWidth;
    CanShowH := HScrollInfo.nMax > HScrollInfo.nPage;
    UpdateHScrollInfo;
  end;
end;

procedure TThumbControl.DoEnter;
begin
  Repaint;
  inherited DoEnter;
end;

procedure TThumbControl.DoExit;
begin
  Repaint;
  inherited DoExit;
end;

procedure TThumbControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  fMouseStartPos.x:=X;
  fMouseStartPos.y:=Y;
  fDragIDX:= fMngr.ItemIndexFromPoint(Point(X + HScrollPosition, Y + VScrollPosition));
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TThumbControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Shift=[ssLeft] then begin
    if fDragIDX>-1 then begin
      if (ABS(fMouseStartPos.x-X)>20) or (ABS(fMouseStartPos.y-Y)>20) then begin
        if fMngr.SelectedList.Count>1 then begin
          DragCursor:=crMultiDrag;
        end else begin
          DragCursor:=crDrag;
        end;
        BeginDrag(True,0);
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TThumbControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DoClick(Button,Shift,X,Y);
  inherited MouseUp(Button, Shift, X, Y);
end;

function TThumbControl.ItemFromPoint(APoint: TPoint): TThreadedImage;
begin
  Result := fMngr.ItemFromPoint(Point(APoint.X + HScrollPosition, APoint.Y + VScrollPosition));
end;

function TThumbControl.AddImage(Image: TThreadedImage): Integer;
begin
  Result:=fMngr.AddThreadedImage(Image);
  //ImgRepaint(Image);
end;

procedure TThumbControl.DeleteSelectedImages;
begin
  fMngr.DeleteSelectedImages;
  Arrange;
end;

procedure TThumbControl.LoadSelectedBitmap(ABitmap:TBitmap);
var fi:TFPMemoryImage;
itm:TThreadedImage;
begin
  itm:=fMngr.ActiveItem;
  if itm=nil then exit;
  fi:=TFPMemoryImage.create(0,0);
  fi.UsePalette:=false;
  try
    fi.LoadFromFile(UTF8ToSys(itm.URL));
    ABitmap.Assign(fi);
  finally
    fi.free;
  end;
end;

procedure TThumbControl.ClearImageList;
begin
  fMngr.Clear;
end;

procedure TThumbControl.AddImagePointer(P: Pointer);
begin
  fMngr.AddImagePointer(P);
end;


procedure TThumbControl.ImgLoadURL(Sender: TObject);
var Ext, Fn: string;
  Img, IRes: TFPMemoryImage;
  rdjpegthumb: TFPReaderJPEG;
  area: TRect;
  Strm: TStream;
begin
  IRes := nil;
  Strm := nil;
  TThreadedImage(Sender).LoadState := lsError;
  Fn := TThreadedImage(Sender).URL;
  Ext := LowerCase(ExtractFileExt(LowerCase(Fn)));
  if (Ext = '.jpg') or (Ext = '.jpeg') then
  begin
    Img := TFPMemoryImage.Create(0, 0);
    Img.UsePalette := false;
    rdjpegthumb := TFPReaderJPEG.Create;
    rdjpegthumb.MinHeight := fThumbHeight;
    rdjpegthumb.MinWidth := fThumbWidth;
    try
      if Assigned(FOnLoadFile) then OnLoadFile(Sender, Fn, Strm);
      if Strm <> nil then
      begin
        Img.LoadFromStream(Strm, rdjpegthumb);
        Strm.free;
      end else Img.LoadFromFile(UTF8ToSys(Fn), rdjpegthumb);
      IRes := DoThumbResize(Img, fThumbWidth, fThumbHeight, area);
      try
        CSImg.Acquire;
        if TThreadedImage(Sender).Image <> nil
          then
        begin
          TThreadedImage(Sender).Image.Assign(IRes);
          TThreadedImage(Sender).Area := Area;
        end;
      finally
        CSImg.Release;
      end;
    finally
      if Assigned(IRes) then
        IRes.free;
      rdjpegthumb.free;
      Img.free;
    end;
  end else
  begin
    Img := TFPMemoryImage.Create(0, 0);
    Img.UsePalette := false;
    try
      if Assigned(FOnLoadFile) then OnLoadFile(Sender, Fn, Strm);
      if Strm <> nil then
      begin
        Img.LoadFromStream(Strm);
        Strm.free;
      end else Img.LoadFromFile(UTF8ToSys(Fn));
      IRes := DoThumbResize(Img, fThumbWidth, fThumbHeight, area);
      try
        CSImg.Acquire;
        if TThreadedImage(Sender).Image <> nil then
        begin
          TThreadedImage(Sender).Image.Assign(IRes);
          TThreadedImage(Sender).Area := Area;
        end;
      finally
        CSImg.Release;
      end;
    finally
      IRes.free;
      Img.free;
    end;
  end;
  TThreadedImage(Sender).LoadState := lsLoading;
end;

procedure TThumbControl.ImgLoadPointer(Sender: TObject);
var
  Img, IRes: TFPMemoryImage;
  Area: TRect;
  Strm: TStream;
begin
  Strm := nil;
  TThreadedImage(Sender).LoadState := lsError;

  Img := TFPMemoryImage.Create(0, 0);
  Img.UsePalette := false;
  if Assigned(FOnLoadPointer) then OnLoadPointer(Sender,TThreadedImage(Sender).Pointer,Strm);
  if Strm <> nil then begin
    try
      Img.LoadFromStream(Strm);
      Strm.free;

      if fResizeFactor=0 then begin
        IRes:=DoThumbResize(Img,fThumbWidth,fThumbHeight,Area);
      end else begin
        IRes:=DoThumbResizeFactor(Img,fThumbWidth,fThumbHeight,fResizeFactor,Area);
      end;

      try
        CSImg.Acquire;
        if TThreadedImage(Sender).Image <> nil then
        begin
          TThreadedImage(Sender).Image.Assign(IRes);
          TThreadedImage(Sender).Area := Area;
        end;
      finally
        CSImg.Release;
      end;
    finally
      IRes.free;
      Img.free;
    end;
  end else begin
    Area.Left:=0;
    Area.Top:=0;
    Area.Bottom:=fThumbHeight;
    Area.Right:=fThumbWidth;
    IRes:=TFPMemoryImage.create(fThumbWidth,fThumbHeight);
    TThreadedImage(Sender).Image.Assign(IRes);
    TThreadedImage(Sender).Area := Area;
  end;
  TThreadedImage(Sender).LoadState := lsLoading;
end;


procedure TThumbControl.ImgRepaint(Sender: TObject);
var aRect: TRect;
begin
  aRect := TThreadedImage(Sender).Rect;
  OffSetRect(aRect, -HScrollPosition, -VScrollPosition);
  InflateRect(ARect, 2, 2);
  InvalidateRect(Handle, @aRect, false);
end;

class function TThumbControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 260;
  Result.CY := 140;
end;


procedure Register;
begin
  RegisterComponents('Misc', [TThumbControl]);
end;

initialization
{$I thumbctrl.lrs}
{$I images.lrs}

finalization

end.

