unit uExtControls;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, DbCtrls, DBGrids, Controls, Grids, Dialogs, LCLType,
  StdCtrls, Graphics, LMessages, LCLProc, LCLIntf, ComCtrls, Menus, Forms,
  ExtCtrls,uModifiedDS;
type
  THackDBGrid = class(TDBGrid);
  THackGrid = class(TCustomGrid);
  TExtRotatedLabel = class(TLabel)
  protected
    procedure Loaded; override;
  published
    constructor Create(TheOwner: TComponent); override;
  end;
  TExtDBCombobox = class(TDBComboBox)
  private
    FCol,FRow : Integer;
    FGrid: TCustomGrid;
  protected
    procedure DropDown; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure UpdateData(Sender: TObject); override;
    procedure DataChange(Sender: TObject); override;
    procedure Select;override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
  published
    constructor Create(TheOwner: TComponent); override;
  end;

  TExtCombobox = class(TComboBox)
  private
    FCol,FRow : Integer;
    FGrid: TCustomGrid;
  protected
    procedure DropDown; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Select;override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
  published
    constructor Create(TheOwner: TComponent); override;
  end;

  { TExtDBGrid }

  TExtDBGrid = class(TDBGrid)
    procedure FDownTimerTimer(Sender: TObject);
    procedure FExtEditorExit(Sender: TObject);
    procedure FExtPickListEditorExit(Sender: TObject);
  private
    FCachedEditing: Boolean;
    FExtPickListEditor : TExtDBComboBox;
    FScrollSyncControl: TWinControl;
    FUseExtPicklist: Boolean;
    FWantReturns: Boolean;
    FOldEditor : TWinControl;
    FMouseDowns : Integer;
    FDownTimer : TTimer;
  protected
    procedure EditorShow(const SelAll: boolean); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure DrawRow(ARow: Integer); override;
    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    procedure MoveSelection; override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
       ); override;
    function EditingAllowed(ACol: Integer=-1): Boolean; override;
    procedure DblClick; override;
    procedure SelectEditor;override;
    function SelectCell(aCol, aRow: Integer): boolean; override;
  public
    property Editor;
    property ScrollSyncControl : TWinControl read FScrollSyncControl write FScrollSyncControl;
    property UseExtPicklist : Boolean read FUseExtPicklist write FUseExtPicklist;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  published
    property WantReturns : Boolean read FWantReturns write FWantReturns;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property CachedEditing : Boolean read FCachedEditing write FCachedEditing;
  end;
  TExtDBEdit = class(TDBEdit)
  private
    FCanvas : TControlCanvas;
  protected
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  end;
  TGetCellWidth = procedure(aCol : Integer;var aNewWidth : Integer) of Object;

  { TExtStringgrid }

  TExtStringgrid = class(TStringGrid)
    procedure FDownTimerTimer(Sender: TObject);
    procedure FExtEditorExit(Sender: TObject);
  private
    FAfterDrawCell: TOnDrawCell;
    FBeforeEnterEdit: TNotifyEvent;
    FEnterEdit: TNotifyEvent;
    FExtPickListEditor : TExtComboBox;
    FCachedEditing: Boolean;
    FGetCellWidth: TGetCellWidth;
    FMouseDowns : Integer;
    FDownTimer : TTimer;
    FUseExtPicklist: Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
       ); override;
    function EditingAllowed(ACol: Integer=-1): Boolean; override;
    procedure SelectEditor;override;
    procedure DblClick; override;
    procedure AutoAdjustColumn(aCol: Integer); override;
    procedure DrawCell(aCol,aRow:Integer; aRect:TRect; aState:TGridDrawState); override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;override;
    function SelectCell(aCol, aRow: Integer): boolean; override;
    procedure EditordoSetValue; override;
    procedure EditorShow(const SelAll: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property UseExtPicklist : Boolean read FUseExtPicklist write FUseExtPicklist;
  published
    property OnAfterDrawCell: TOnDrawCell read FAfterDrawCell write FAfterDrawCell;
    property OnGetCellWidth: TGetCellWidth read FGetCellWidth write FGetCellWidth;
    property CachedEditing : Boolean read FCachedEditing write FCachedEditing;
    property OnEnterEdit : TNotifyEvent read FEnterEdit write FEnterEdit;
    property BeforeEnterEdit : TNotifyEvent read FBeforeEnterEdit write FBeforeEnterEdit;
  end;
  TFrameClass = class of TFrame;

  TFrameRecord = record
    FrameClass : TFrameClass;
    Name : string;
    AddFunction : TNotifyEvent;
    MultiblePages : Boolean;
  end;
  TExtControlFrame = class(TFrame)
  private
    FTabCaption: string;
    procedure SetTabCaption(const AValue: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property TabCaption : string read FTabCaption write SetTabCaption;
    procedure ShowFrame;virtual;
    procedure FrameAdded;virtual;
    procedure DoRefresh;virtual;
  end;

  { TExtMenuPageControl }

  TExtMenuPageControl = class(TPageControl)
    procedure CloseFrameClick(Sender: TObject);
    procedure ExtMenuPageControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure FCloseMenuPopup(Sender: TObject);
    procedure FFrameClassesMenuItemClick(Sender: TObject);
    procedure FMenuClose(Sender: TObject);
  private
    FMenu: TPopupMenu;
    FCloseMenu : TPopupMenu;
    FNewPage: TTabSheet;
    FNewCustomPage: TMenuItem;
    FMoveTab : Boolean;
    FFrameClasses : array of TFrameRecord;
    FDontChange : Boolean;
    FNewTabImageIndex: Integer;
    FTabTypes: string;
    procedure SetNewTabImageIndex(const AValue: Integer);
    procedure RefreshMenue;
  protected
    procedure Change; override;
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure DoAutoSize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function AddTab(aFrame : TFrame;SetActive : Boolean = True;NewName : string = '';aImageIndex : Integer = -1;UseFunction : Boolean = True) : Integer;
    function GetTab(aFrameClass : TFrameClass) : TTabSheet;
    procedure AddTabClass(aFrameClass : TFrameClass;aName : string;aAddFunction : TNotifyEvent = nil;aImageIndex : Integer = -1;aMultiblePages : Boolean = False);
    procedure CanHaveCustomTabs(aAddFunction : TNotifyEvent = nil);
    procedure ClearTabClasses;
    procedure CloseAll;
    procedure WillRemoveTab(aPage : TTabSheet);
    property TabTypes : string read FTabTypes write FTabTypes;
  published
    property NewTabImageIndex : Integer read FNewTabImageIndex write SetNewTabImageIndex;
  end;

procedure Register;
implementation
{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}
resourcestring
  strNewTab                           = 'Neues Tab';
  strClose                            = 'Schließen';
  strNewCustomPage                    = 'Tab erzeugen';

procedure Register;
begin
  RegisterComponents('Data Controls',[TExtDBComboBox]);
  RegisterComponents('Data Controls',[TExtDBGrid]);
  RegisterComponents('Data Controls',[TExtDBEdit]);
  RegisterComponents('Additional',[TExtStringGrid]);
  RegisterComponents('Additional',[TExtRotatedLabel]);
  RegisterComponents('Common Controls',[TExtMenuPageControl]);
end;
procedure AutoSizeComboboxList(Targetbox: TCustomComboBox);
var temp, max, itemscounter: integer;
    bmp : Graphics.TBitmap;
begin
  bmp := Graphics.TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(Targetbox.Font);
    max :=Targetbox.width;
    for itemscounter := 0 to Targetbox.Items.count-1 do begin
      temp := bmp.Canvas.TextWidth(Targetbox.Items[itemscounter]);
      if temp > max then
        max := temp;
    end;
    {$IFDEF WINDOWS}
    if Targetbox.Items.Count > Targetbox.DropDownCount then   //wenn eine Scrollbar auf der seite benötigt wird dann
      Inc(max, GetSystemMetrics(SM_CXVSCROLL)); //verbreitere die combobox um breite der scrollbar
    Sendmessage(Targetbox.Handle, CB_SETDROPPEDWIDTH, max+20,0); //ich hab +20 genommen da es sonst direkt am letzten buchstaben pickt und das sieht hässlich aus. Natürlich kann man auch das +20 ganz weg lassen oder nur die zahl verändern.
    {$ENDIF}
  finally
    bmp.Free;
  end;
end;

{ TExtCombobox }

procedure TExtCombobox.DropDown;
begin
  AutoSizeComboboxList(Self);
  inherited DropDown;
end;

procedure TExtCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
  VK_LEFT,VK_RIGHT:
    begin
      DroppedDown := False;
      FGrid.SetFocus;
      FGrid.EditorkeyDown(Self, key, shift);
      if FGrid<>nil then
        FGrid.EditingDone;
    end;
  end;
  if (Key = VK_UP) and (ItemIndex=0) then
    begin
      DroppedDown := False;
      FGrid.SetFocus;
      FGrid.EditorkeyDown(Self, key, shift);
      if FGrid<>nil then
        FGrid.EditingDone;
    end;
end;

procedure TExtCombobox.Select;
begin
  if FGrid<>nil then
    begin
      if THackDBGrid(FGrid).EditorIsReadOnly then
        exit;
      if (pos(' ',Text) > 0) and (copy(Text,0,pos(' ',Text)-1) = UpperCase(copy(Text,0,pos(' ',Text)-1))) then
        THackDBGrid(FGrid).SetEditText(FCol, FRow, copy(Text,0,pos(' ',text)-1))
      else
        THackDBGrid(FGrid).SetEditText(FCol, FRow, Text);
      THackDBGrid(FGrid).PickListItemSelected(Self);
      if FGrid<>nil then
        FGrid.EditingDone;
    end;
  inherited Select;
end;

procedure TExtCombobox.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=Text;
end;

procedure TExtCombobox.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TExtCombobox.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

constructor TExtCombobox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.Sorted:=True;
end;

procedure TExtRotatedLabel.Loaded;
begin
  inherited Loaded;
  {$IFDEF WINDOWS}
  Font.Orientation:=900;
  Self.Layout := tlBottom;
  {$ELSE}
  Font.Orientation:=-900;
  Self.Layout := tlTop;
  {$ENDIF}
end;

constructor TExtRotatedLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Autosize := False;
end;
procedure TExtControlFrame.SetTabCaption(const AValue: string);
begin
  if FTabCaption=AValue then exit;
  FTabCaption:=AValue;
  if Parent is TTabSheet then
    TTabSheet(Parent).Caption:=FTabCaption;
end;

constructor TExtControlFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := '';
end;
procedure TExtControlFrame.ShowFrame;
var
  aPages: TComponent;
begin
  aPages := FindComponent('pcPages');
  if Assigned(aPages) and (aPages is TExtMenuPageControl) then
    if Assigned(TExtMenuPageControl(aPages).ActivePage) then
      if (TExtMenuPageControl(aPages).ActivePage.ControlCount>0) and (TExtMenuPageControl(aPages).ActivePage.Controls[0] is TExtControlFrame) then
        TExtControlFrame(TExtMenuPageControl(aPages).ActivePage.Controls[0]).ShowFrame;
end;

procedure TExtControlFrame.FrameAdded;
begin

end;

procedure TExtControlFrame.DoRefresh;
begin
end;
procedure TExtMenuPageControl.FFrameClassesMenuItemClick(Sender: TObject);
var
  aFrameClass: TFrameRecord;
  aFrame: TFrame;
  aNewPage: TTabSheet;
begin
  aNewPage := TTabSheet.Create(Self);
  Self.Visible:=False;
  aNewPage.PageControl := Self;
  aNewPage.Parent := Self;
  aNewPage.Tag:=999;
  aFrameClass := FFrameClasses[TMenuItem(Sender).Tag];
  aNewPage.ImageIndex:=TMenuItem(Sender).ImageIndex;
  aNewPage.Caption := aFrameClass.Name;
  aFrame := aFrameClass.FrameClass.Create(nil);
  if Assigned(aFrame) and (aFrame is TExtControlFrame) then
    TExtControlFrame(aFrame).TabCaption:=aFrameClass.Name;
  FDontChange := True;
  FNewPage.PageIndex:=Self.PageCount-1;
  FDontChange := False;
  FMenu.Close;
  Self.ActivePage := aNewPage;
  aFrame.Parent := aNewPage;
  aFrame.Align:=alClient;
  aFrame.Show;
  if Assigned(aFrameClass.AddFunction) then
    aFrameClass.AddFunction(aFrame);
  if not aFrameClass.MultiblePages then
    TMenuItem(Sender).Enabled:=False;
  FNewPage.ImageIndex:=FNewTabImageIndex;
  Self.Visible:=True;
  if ActivePage.Controls[0] is TExtControlFrame then
    TExtControlFrame(ActivePage.Controls[0]).ShowFrame;
  RefreshMenue;
  Self.SetFocus;
end;
procedure TExtMenuPageControl.CloseFrameClick(Sender: TObject);
var
  aPage: TTabSheet;
  aCont: TControl;
  i: Integer;
  aMenu: TMenuItem;
  aControl: TComponent;
begin
  if Sender = nil then exit; //Bug with ActionLists
  try
    if Sender is TTabSheet then
      aPage := Sender as TTabSheet
    else
      aPage := Self.ActivePage;
  except
    exit;
  end;
  if aPage.Tag<>999 then exit;//user Tab
  for i := 0 to FMenu.Items.Count-1 do
     if Fmenu.Items[i].Caption = aPage.Caption then
       FMenu.Items[i].Enabled := True;
  Self.TabIndex:=Self.TabIndex-1;
  if (aPage.ControlCount > 0) and (aPage.Controls[0] is TFrame) then
    begin
      try
        aCont := aPage.Controls[0];
        aCont.Hide;
        aCont.Parent := nil;
        FreeAndNil(aCont);
      except
      end;
    end;
  FreeAndNil(aPage);
  if not Assigned(ActivePage) then exit;
  if (ActivePage.ControlCount > 0) and (ActivePage.Controls[0] is TFrame) then
    begin
      if TFrame(ActivePage.Controls[0]).CanFocus then
        TFrame(ActivePage.Controls[0]).SetFocus;
      if (ActivePage.Controls[0] is TExtControlFrame) then
        TExtControlFrame(ActivePage.Controls[0]).ShowFrame;
    end;
end;

procedure TExtMenuPageControl.ExtMenuPageControlContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  TH: SmallInt;
  Y: LongInt;
begin
  Y := Self.ScreenToControl(Mouse.CursorPos).Y;
  {$IF FPC_FULLVERSION<20602}
  TH := 0;
  {$ELSE}
  TH := Self.TabHeight;
  {$ENDIF}
  if TH= 0 then TH := 25;
  Handled := not (Y <= TH);
end;

procedure TExtMenuPageControl.FCloseMenuPopup(Sender: TObject);
var
  aPage: TTabSheet;
begin
  FCloseMenu.Items[0].Enabled:=False;
  if Sender = nil then exit; //Bug with ActionLists
  try
    if Sender is TTabSheet then
      aPage := Sender as TTabSheet
    else
      aPage := Self.ActivePage;
  except
    exit;
  end;
  if aPage.Tag<>999 then exit;//user Tab
  FCloseMenu.Items[0].Enabled:=True;
end;

procedure TExtMenuPageControl.FMenuClose(Sender: TObject);
begin
  Self.PageIndex:=0;
end;
procedure TExtMenuPageControl.SetNewTabImageIndex(const AValue: Integer);
begin
  if FNewTabImageIndex=AValue then exit;
  FNewTabImageIndex:=AValue;
  FNewPage.ImageIndex:=FNewTabImageIndex;
end;
procedure TExtMenuPageControl.RefreshMenue;
begin
  if (Assigned(ActivePage) and (ActivePage.TabIndex = 0))
  or (ActivePage = FNewPage)
  or (Assigned(ActivePage) and (ActivePage.Tag = 1))
  then
    Self.Popupmenu := nil
  else
    Self.PopupMenu := FCloseMenu;
end;
procedure TExtMenuPageControl.Change;
var
  x: LongInt;
  i: Integer;
begin
  inherited Change;
  if FDontChange then exit;
  if Self.ActivePage = FNewPage then
    begin
      x := TabRect(ActivePage.TabIndex).Left;
      {$IFDEF LCLCARBON}
      for i := 0 to PageCount-1 do
         begin
           x += length(Pages[i].Caption)*6;
           x += 20;
         end;
      x := x div 2;
      x := x+Width div 2;
      x -= (length(FnewPage.Caption)*6)+20;
      x := Self.ClientToScreen(Classes.Point(x,0)).x-1;
      {$ENDIF}
      {$IFNDEF LCLCarbon}
      x := Self.ClientToScreen(Classes.Point(x,0)).x-1;
      {$ENDIF}
      {$IFDEF LCLGtk2}
      x := x-Left;
      {$ENDIF}
      FMenu.PopUp(x,Self.ClientToScreen(Classes.Point(0,0)).y-2);
    end
  else if Assigned(ActivePage) and (ActivePage.ControlCount > 0) and (ActivePage.Controls[0] is TFrame) then
    begin
      if TFrame(ActivePage.Controls[0]).CanFocus then
        TFrame(ActivePage.Controls[0]).SetFocus;
      if (ActivePage.Controls[0] is TExtControlFrame) then
        TExtControlFrame(ActivePage.Controls[0]).ShowFrame;
      RefreshMenue;
    end;
end;
procedure TExtMenuPageControl.InsertControl(AControl: TControl; Index: integer
  );
begin
  inherited InsertControl(AControl, Index);
  if Assigned(FnewPage) then
    FMoveTab := True;
end;
procedure TExtMenuPageControl.DoAutoSize;
begin
  inherited DoAutoSize;
end;
procedure TExtMenuPageControl.Loaded;
begin
  inherited Loaded;
  if FMoveTab then
    FNewPage.PageIndex:=Self.PageCount-1;
end;
constructor TExtMenuPageControl.Create(AOwner: TComponent);
var
  aNewItem: TMenuItem;
begin
  inherited Create(AOwner);
  FNewCustomPage:=nil;
  FNewTabImageIndex := 0;
  FMenu := TPopupMenu.Create(Self);
  Fmenu.OnClose:=@FMenuClose;
  FNewPage := TTabSheet.Create(Self);
  FNewPage.PageControl := Self;
  FNewPage.Caption:=strNewTab;

  FCloseMenu := TPopupMenu.Create(Self);
  FCloseMenu.OnPopup:=@FCloseMenuPopup;
  aNewItem := TMenuItem.Create(FCloseMenu);
  aNewItem.Caption:=strClose;
  aNewItem.OnClick:=@CloseFrameClick;
  FCloseMenu.Items.Add(aNewItem);
  Self.OnContextPopup:=@ExtMenuPageControlContextPopup;
end;
destructor TExtMenuPageControl.Destroy;
begin
  CloseAll;
  FMenu.Free;
  inherited Destroy;
end;
function TExtMenuPageControl.AddTab(aFrame: TFrame;SetActive : Boolean = True;NewName : string = '';aImageIndex : Integer = -1;UseFunction : Boolean = True) : Integer;
var
  aNewPage: TTabSheet;
  i: Integer;
  OldIndex: LongInt;
  Found: Boolean;
  a: Integer;
begin
  Result := -1;
  if not SetActive then
    Visible := False;
  OldIndex := Self.PageIndex;
  aNewPage := TTabSheet.Create(Self);
  aNewPage.Tag:=999;
  aNewPage.PageControl := Self;
  aNewPage.ImageIndex := aImageIndex;
  FDontChange := True;
  FNewPage.PageIndex:=Self.PageCount-1;
  try
    aNewPage.PageIndex:=OldIndex+1;
  except
    aNewPage.PageIndex:=OldIndex+1;
  end;
  FDontChange := False;
  if SetActive then
    Self.ActivePage := aNewPage;
  aFrame.Parent := aNewPage;
  aFrame.Align:=alClient;
  aFrame.Show;
  Found := False;
  if UseFunction then
    for i := 0 to length(FFrameClasses)-1 do
       if aFrame.ClassName = FFrameClasses[i].FrameClass.ClassName then
         begin
           if NewName = '' then
             aNewPage.Caption:=FFrameClasses[i].Name
           else
             begin
               aNewPage.Caption := NewName;
               TExtControlFrame(aFrame).TabCaption := NewName;
             end;
           if Assigned(FFrameClasses[i].AddFunction) then
             begin
               FFrameClasses[i].AddFunction(aFrame);
             end;
           if not FFrameClasses[i].MultiblePages then
             for a := 0 to FMenu.Items.Count-1 do
                if Fmenu.Items[a].Caption = aNewPage.Caption then
                  FMenu.Items[a].Enabled := False;
           Found := True;
         end;
  if not Found then
    begin
      if (NewName = '') and (aFrame is TExtControlFrame) then
        aNewPage.Caption:=TExtControlFrame(aFrame).TabCaption
      else
        aNewPage.Caption := NewName;
    end;
  if not SetActive then
    PageIndex := OldIndex;
  FNewPage.ImageIndex:=FNewTabImageIndex;
  Visible := True;
  FNewPage.ImageIndex:=FNewTabImageIndex;
  if (aFrame is TExtControlFrame) then
    TExtControlFrame(aFrame).ShowFrame;
  if (aFrame is TExtControlFrame) then
    TExtControlFrame(aFrame).FrameAdded;
  RefreshMenue;
  Result := Self.PageIndex;
end;
function TExtMenuPageControl.GetTab(aFrameClass: TFrameClass): TTabSheet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Self.PageCount-1 do
    if (Self.Pages[i].ControlCount > 0) and (Self.Pages[i].Controls[0] is aFrameClass) then
      begin
        Result := Self.Pages[i];
        break;
      end;
end;
procedure TExtMenuPageControl.AddTabClass(aFrameClass : TFrameClass;
  aName : string;aAddFunction : TNotifyEvent = nil;aImageIndex : Integer = -1;aMultiblePages : Boolean = False);
var
  MenuItem: TMenuItem;
begin
  Setlength(FFrameClasses,length(FFrameClasses)+1);
  FMenu.Images := Images;
  with FFrameClasses[length(FFrameClasses)-1] do
    begin
      FrameClass := aFrameClass;
      Name := aName;
      AddFunction := aAddFunction;
      MultiblePages := aMultiblePages;
      MenuItem := TMenuItem.Create(FMenu);
      MenuItem.Caption:=aName;
      MenuItem.OnClick:=@FFrameClassesMenuItemClick;
      MenuItem.Tag:=length(FFrameClasses)-1;
      MenuItem.ImageIndex:=aImageIndex;
      FMenu.Items.Add(MenuItem);
      FNewPage.TabVisible:=True;
    end;
end;

procedure TExtMenuPageControl.CanHaveCustomTabs(aAddFunction: TNotifyEvent);
begin
  if not Assigned(FNewCustomPage) then
    begin
      FNewCustomPage := TMenuItem.Create(FMenu);
      FNewCustomPage.Caption:=strNewCustomPage;
      FNewCustomPage.OnClick:=aAddFunction;
      FMenu.Items.Add(FNewCustomPage);
    end;
end;

procedure TExtMenuPageControl.ClearTabClasses;
begin
  Setlength(FFrameClasses,0);
  FMenu.Items.Clear;
  FNewPage.TabVisible:=False;
end;
procedure TExtMenuPageControl.CloseAll;
var
  i: Integer;
  aPage: TTabSheet;
begin
  Fmenu.Items.Clear;
  TabIndex:=0;
  i := 0;
  while i < Self.PageCount-1 do
    begin
      if Pages[i].Tag=999 then
        begin
          try
            if Pages[i].ControlCount > 0 then
              Pages[i].Controls[0].Free;
          except
            on e : exception do
              debugln('Error during Page.Close: '+e.Message);
          end;
          aPage := Pages[i];
          aPage.Free;
        end
      else inc(i);
    end;
end;
procedure TExtMenuPageControl.WillRemoveTab(aPage: TTabSheet);
var
  i: Integer;
begin
  for i := 0 to FMenu.Items.Count-1 do
     if Fmenu.Items[i].Caption = aPage.Caption then
       FMenu.Items[i].Enabled := True;
end;
constructor TExtDBCombobox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.Sorted:=True;
end;
procedure TExtDBCombobox.DropDown;
begin
  AutoSizeComboboxList(Self);
  inherited DropDown;
end;
procedure TExtDBCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_RETURN:
      if DroppedDown then
        begin
          if (FGrid=nil) then
            Key := 0;
          DroppedDown := False;
          if Key<>0 then
            begin
              if FGrid<>nil then
                FGrid.EditorkeyDown(Self, key, shift);
              Key:=0;
            end;
        end
      else
        begin
          if FGrid<>nil then
            FGrid.EditorkeyDown(Self, key, shift);
          FGrid.SetFocus;
        end;
    else if FGrid<>nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;
end;
procedure TExtDBCombobox.UpdateData(Sender: TObject);
begin
  if (pos(' ',Text) > 0) and (copy(Text,0,pos(' ',Text)-1) = UpperCase(copy(Text,0,pos(' ',Text)-1))) then
    Text := copy(Text,0,pos(' ',Text)-1);
  inherited UpdateData(Sender);
  DataChange(Sender);
end;
procedure TExtDBCombobox.DataChange(Sender: TObject);
var
  i: Integer;
begin
  inherited DataChange(Sender);
  if not Assigned(Field) then exit;
  for i := 0 to Items.Count-1 do
    if trim(copy(Items[i],0,Field.Size)) = trim(Text) then
      Text := items[i];
end;
procedure TExtDBCombobox.Select;
begin
  if FGrid<>nil then
    begin
      if THackDBGrid(FGrid).EditorIsReadOnly then
        exit;
      if Assigned(Field) then
        begin
          if (pos(' ',Text) > 0) and (copy(Text,0,pos(' ',Text)-1) = UpperCase(copy(Text,0,pos(' ',Text)-1))) then
            THackDBGrid(FGrid).SetEditText(FCol, FRow, copy(Text,0,pos(' ',text)-1))
          else
            THackDBGrid(FGrid).SetEditText(FCol, FRow, Text);
        end;
      THackDBGrid(FGrid).PickListItemSelected(Self);
    end;
  inherited Select;
end;
procedure TExtDBCombobox.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col := FCol;
  Msg.Row := FRow;
  if Assigned(Field) then
    Msg.Value:=copy(Text,0,Field.Size);
end;
procedure TExtDBCombobox.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;
procedure TExtDBCombobox.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;
procedure TExtDBGrid.FDownTimerTimer(Sender: TObject);
begin
  FDownTimer.Enabled:=False;
  if FMouseDowns = 1 then
    begin
      if EditingAllowed(Col) and CanEditShow then
        begin
          SelectEditor;
          if (Editor<>nil) and (not Editor.Visible) then
            EditorMode:=True;
        end;
    end;
  FMouseDowns:=0;
end;
procedure TExtDBGrid.FExtEditorExit(Sender: TObject);
begin
  UpdateData;
//  EditorMode := false;
  if Sender is TCustomEdit then
    TCustomEdit(Sender).MaxLength:=0;
  TWinControl(Sender).OnExit:=nil;
end;
procedure TExtDBGrid.FExtPickListEditorExit(Sender: TObject);
begin
  UpdateData;
//  EditorMode := false;
  if Sender is TCustomEdit then
    TCustomEdit(Sender).MaxLength:=0;
  TWinControl(Sender).OnExit:=nil;
end;
procedure TExtDBGrid.SelectEditor;
begin
  inherited SelectEditor;
  if FUseExtPicklist then
    begin
      if Assigned(SelectedColumn) and Assigned(Editor) and (Editor is TPickListCellEditor) then
        Editor := FExtPickListEditor;
      if Assigned(FExtPickListEditor) and (Editor = FExtPickListEditor) then
        begin
          FExtPickListEditor.DataField := SelectedColumn.FieldName;
          FExtPickListEditor.DataSource := DataSource;
          FExtPickListEditor.Items.Assign(SelectedColumn.PickList);
          Editor.BoundsRect := CellRect(Col,Row);
          FExtPickListEditor.Text := SelectedColumn.Field.AsString;
        end;
    end;
//  if Assigned(FOldEditor) and (FOldEditor is TCustomEdit) then
//    TCustomEdit(FOldEditor).MaxLength := 0;
  if Assigned(Editor) and (SelectedColumn.ButtonStyle <> cbsEllipsis) then
    begin
//      FOldEditor := Editor;
      Editor.OnExit:=@FExtPickListEditorExit;
    end;
end;
function TExtDBGrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  if (aRow <> Row) or (aCol <> Col) then
    FMouseDowns:=0;
  Result:=inherited SelectCell(aCol, aRow);
end;
procedure TExtDBGrid.EditorShow(const SelAll: boolean);
begin
  if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
    if (DataSource.DataSet.State <> dsEdit) and (DataSource.DataSet.State <> dsInsert) then
      DataSource.DataSet.Edit;
  inherited EditorShow(SelAll);
  if Assigned(Editor) then
    begin
      Editor.OnExit:=@FExtEditorExit;
    end;
end;
procedure TExtDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  SetNull: Boolean = False;
  IManage : IBaseModifiedDS;
begin
  case KEY of
    VK_DELETE:exit;
    VK_RETURN:
      if not FWantReturns then
        begin
          if not Datasource.DataSet.EOF then
            DataSource.DataSet.MoveBy(1);
          if not Self.ReadOnly then
            if DataSource.DataSet.EOF then
              DataSource.DataSet.Append;
          SetNull := True;
        end;
//      Key := VK_DOWN;
    VK_ESCAPE:
      begin
        if (DataSource.DataSet.State = dsInsert) or (DataSource.DataSet.State = dsEdit) then
          SetNull := True;
      end;
    VK_UP:
      begin
        if (DataSource.DataSet.State = dsInsert) and (DataSource.DataSet.Modified) then
          begin
            if Supports(DataSource.DataSet,IBaseModifiedDS,IManage) then
              begin
                if not IManage.IsChanged then
                  begin
                    DataSource.DataSet.Delete;
                    Key := 0;
                  end;
              end;
          end;
      end;
    VK_DOWN:
      begin
        if (DataSource.DataSet.State = dsInsert) and (DataSource.DataSet.Modified) then
          begin
            if Supports(DataSource.DataSet,IBaseModifiedDS,IManage) then
              begin
                if not IManage.IsChanged then
                  begin
                    Key := 0;
                  end;
              end;
          end;
      end;
  end;
  inherited KeyDown(Key, Shift);
//  if Assigned(FScrollSyncControl) and (THackDBGrid(FScrollSyncControl).LeftCol <> LeftCol) then
//    THackDBGrid(FScrollSyncControl).LeftCol:=LeftCol;
  if SetNull then Key := 0;
end;
procedure TExtDBGrid.DrawRow(ARow: Integer);
begin
  inherited DrawRow(ARow);
end;
procedure TExtDBGrid.WMHScroll(var Message: TLMHScroll);
begin
  if Assigned(FScrollSyncControl) then
    FScrollSyncControl.Dispatch(Message);
  inherited;
end;
procedure TExtDBGrid.MoveSelection;
begin
  inherited MoveSelection;
  if Assigned(FScrollSyncControl) and (THackDBGrid(FScrollSyncControl).LeftCol <> LeftCol) then
    THackDBGrid(FScrollSyncControl).LeftCol:=LeftCol;
end;
function TExtDBGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result:=inherited CanGridAcceptKey(Key, Shift);
  if Key = 16 then Result := False;
end;
procedure TExtDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Y > GCache.GridHeight then exit;
  if (Button <> mbRight) and FCachedEditing then
    begin
      Inc(FMouseDowns);
      //debugln(IntToStr(FMouseDowns)+' clicks');
      if not Assigned(FDownTimer) then
        begin
          FDownTimer:=TTimer.Create(Self);
          FDownTimer.Interval:=300;
          FDownTimer.OnTimer:=@FDownTimerTimer;
          FDownTimer.Enabled:= False;
        end;
      if (FDownTimer.Enabled = False) and (FMouseDowns > 0) then
        if MouseToCell(Classes.Point(X,Y)).Y=Row then
          begin
            FDownTimer.Enabled:=True;
          end;
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;
function TExtDBGrid.EditingAllowed(ACol: Integer): Boolean;
begin
  Result:=inherited EditingAllowed(ACol);
  if Result and (FMouseDowns > 0) and (Assigned(FDownTimer)) and (FDownTimer.Enabled)
  and (ACol > 0) and (ACol <  Columns.Count) and (Columns[ACol-1].ButtonStyle <> cbsCheckboxColumn)
  and (Columns[ACol-1].PickList.Count = 0) then
    Result := False;
end;
procedure TExtDBGrid.DblClick;
begin
  if FCachedEditing and Assigned(FDownTimer) then
    FDownTimer.Enabled:=False;
  inherited DblClick;
end;
constructor TExtDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCachedEditing := True;
  FExtPickListEditor := TExtDBComboBox.Create(Self);
  FWantReturns := False;
  FExtPickListEditor.Visible:=False;
  FUseExtPicklist := True;
end;
destructor TExtDBGrid.Destroy;
begin
  FExtPickListEditor.Free;
  inherited Destroy;
end;
procedure TExtStringgrid.FDownTimerTimer(Sender: TObject);
begin
  FDownTimer.Enabled:=False;
  if FMouseDowns = 1 then
    EditorMode:=True;
  FMouseDowns:=0;
end;
procedure TExtStringgrid.FExtEditorExit(Sender: TObject);
begin
  if Visible and EditorMode then
    EditorMode := false;
  TWinControl(Sender).OnExit:=nil;
end;
procedure TExtStringgrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Y > GCache.GridHeight then exit;
  if (Button <> mbRight) and FCachedEditing then
    begin
      Inc(FMouseDowns);
      if not Assigned(FDownTimer) then
        begin
          FDownTimer:=TTimer.Create(Self);
          FDownTimer.Interval:=300;
          FDownTimer.OnTimer:=@FDownTimerTimer;
          FDownTimer.Enabled:= False;
        end;
      if (FDownTimer.Enabled = False) and (FMouseDowns > 0) then
        if (MouseToCell(Classes.Point(X,Y)).Y=Row) and (MouseToCell(Classes.Point(X,Y)).X=Col) then
          begin
            FDownTimer.Enabled:=True;
          end;
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;
function TExtStringgrid.EditingAllowed(ACol: Integer): Boolean;
begin
  Result:=inherited EditingAllowed(ACol);
  if (FMouseDowns > 0) and (Assigned(FDownTimer)) and (FDownTimer.Enabled)
  and (Col < ColCount) and (Columns[Col-1].ButtonStyle <> cbsCheckboxColumn) then
    Result := False;
end;
procedure TExtStringgrid.AutoAdjustColumn(aCol: Integer);
var
  aNewWidth : Integer = 0;
begin
  inherited AutoAdjustColumn(aCol);
  if Assigned(FGetCellWidth) then
    begin
      FGetCellWidth(aCol,aNewWidth);
      if aNewWidth > ColWidths[aCol] then
        ColWidths[aCol] := aNewWidth;
      Self.HeaderSized(True,aCol);
    end;
end;
procedure TExtStringgrid.SelectEditor;
begin
  inherited SelectEditor;
  if FUseExtPicklist then
    begin
      if Assigned(SelectedColumn) and Assigned(Editor) and (Editor is TPickListCellEditor) then
        Editor := FExtPickListEditor;
      if Assigned(FExtPickListEditor) and (Editor = FExtPickListEditor) then
        begin
          FExtPickListEditor.Items.Assign(SelectedColumn.PickList);
          Editor.BoundsRect := CellRect(Col,Row);
          FExtPickListEditor.Text := Cells[Col,Row];
        end;
    end;
  if Assigned(Editor) then
    begin
      Editor.OnExit:=@FExtEditorExit;
    end;
end;
procedure TExtStringgrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  if Assigned(FAfterDrawCell) then
    FAfterDrawCell(Self,aCol,aRow,aRect,aState);
end;

function TExtStringgrid.CanGridAcceptKey(Key: Word; Shift: TShiftState
  ): Boolean;
begin
  Result:=inherited CanGridAcceptKey(Key, Shift);
  if Key = 16 then Result := False;
end;
function TExtStringgrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  if (aRow <> Row) or (aCol <> Col) then
    FMouseDowns:=0;
  Result:=inherited SelectCell(aCol, aRow);
end;
procedure TExtStringgrid.EditordoSetValue;
begin
  inherited EditordoSetValue;
  SetEditText(Col,Row,GetEditText(Col,Row));
end;

procedure TExtStringgrid.EditorShow(const SelAll: boolean);
begin
  if Assigned(FBeforeEnterEdit) then FBeforeEnterEdit(Self);
  inherited EditorShow(SelAll);
  if Assigned(FEnterEdit) then FEnterEdit(Self);
end;

procedure TExtStringgrid.DblClick;
begin
  if Assigned(FDownTimer) then
    FDownTimer.Enabled:=False;
  inherited DblClick;
end;
constructor TExtStringgrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCachedEditing := True;
  FDownTimer := nil;
  FExtPickListEditor := TExtComboBox.Create(Self);
  FExtPickListEditor.Visible:=False;
  FUseExtPicklist := True;
end;
destructor TExtStringgrid.Destroy;
begin
  FreeAndNil(FDownTimer);
  inherited Destroy;
end;
procedure TExtDBEdit.Paint;
begin
  with FCanvas do
    begin
      Brush.Color := clRed;
      Polygon([Classes.Point(Width-4,0),Classes.Point(Width,0),Classes.Point(Width,4)]);
    end;
end;
procedure TExtDBEdit.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
//      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;
constructor TExtDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;
destructor TExtDBEdit.Destroy;
begin
  inherited;
  FCanvas.Free;
end;
end.
