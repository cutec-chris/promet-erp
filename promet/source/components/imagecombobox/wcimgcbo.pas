{
  Watt Computers Delphi Image ComboBox Component v1.0

  Copyright (c) 1997-2000 James Watt

  This notice may not be removed from or altered in any source distribution.

  Watt Computers products in source code or object form (including but not
  limited to .PAS, .DCU, .OBJ), in whole or in part, modified or unmodified,
  may not be redistributed for profit or as part of another commercial or
  shareware software package without express written permission from me.

  This software is provided 'as-is', without any express or implied warranty.
  In no event shall the author be held liable for any damages arising from the
  use of this software.
}

unit wcimgcbo;

interface

uses
    LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls;

type
    { TImageComboBox }
    TImageComboBox = class(TCustomComboBox)
    private
        FImages: TImageList;
        FShowCaptions: boolean;
        FAutoCaptions: boolean;
        procedure SetImageList(Value: TImageList);
        procedure SetAutoCaptions(Value: boolean);
        function GetItemBitmap: TBitmap;
    protected
        constructor Create(AOwner: TComponent); override;
    public
        procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    published
        property AutoCaptions: boolean read FAutoCaptions write SetAutoCaptions;
        property Images: TImageList read FImages write SetImageList;
        property ShowCaptions: boolean read FShowCaptions write FShowCaptions;
        property ItemBitmap: TBitmap read GetItemBitmap;
        property Color;
        property Ctl3D;
        property DragMode;
        property DragCursor;
        property DropDownCount;
        property Enabled;
        property Font;
        property ImeMode;
        property ImeName;
        property Items;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property Visible;
        property OnChange;
        property OnClick;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnDrawItem;
        property OnDropDown;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMeasureItem;
        property OnStartDrag;
    end;

implementation

{. TImageComboBox .............................................................}

constructor TImageComboBox.Create(AOwner: TComponent);
begin
    inherited;
    Style := csOwnerDrawVariable;
    FImages := nil;
    FShowCaptions := True;
    FAutoCaptions := True;
end;

{..............................................................................}

procedure TImageComboBox.SetImageList(Value: TImageList);
begin
    if Value <> FImages then
        FImages := Value;
    if Assigned(Value) then begin
        ItemHeight := Value.Height + 2;
        if FAutoCaptions then
            while Items.Count < Value.Count do
                Items.Add(' ' + IntToStr(Items.Count));
    end
    else
        if FAutoCaptions then
            Items.Clear;
end;

{..............................................................................}

procedure TImageComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
    FBitmap: TBitmap;
begin
    if Assigned(OnDrawItem) then
        OnDrawItem(Self, Index, Rect, State)
    else begin
        Canvas.FillRect(Rect);
        if Assigned(FImages) then begin
            if FImages.Count > Index then begin
                FBitmap := TBitmap.Create;
                FImages.GetBitmap(Index, FBitmap);
                Canvas.BrushCopy(
                    Bounds(Rect.Left + 4, Rect.Top, FImages.Width, FImages.Height),
                    FBitmap,
                    Bounds(0, 0, FImages.Width, FImages.Height),
                    FBitmap.TransparentColor);
                if FShowCaptions then
                    Canvas.TextOut(Rect.Left + 4 + FImages.Width, Rect.Top, Items[Index]);
                FBitmap.Free;
            end
            else
                if FShowCaptions then
                    Canvas.TextOut(Rect.Left + 4 + FImages.Width, Rect.Top, Items[Index])
        end
        else
            if FShowCaptions then
                Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index])
    end;
end;

{..............................................................................}

procedure TImageComboBox.SetAutoCaptions(Value: boolean);
begin
    FAutoCaptions := Value;
    if Value and Assigned(FImages) then begin
        if Items.Count > 0 then
            if MessageDlg('Clear existing items?', mtInformation, [mbYes, mbNo], 0) <> mrYes then begin
                FAutoCaptions := False;
                Exit;
            end;
        Items.Clear;
        while Items.Count < FImages.Count do
            Items.Add(' ' + IntToStr(Items.Count));
    end;
end;

{..............................................................................}

function TImageComboBox.GetItemBitmap: TBitmap;
begin
    Result := nil;
    if (ItemIndex > -1) and (ItemIndex < FImages.Count) then begin
        Result := TBitmap.Create;
        FImages.GetBitmap(ItemIndex, Result);
    end;
end;

{..............................................................................}

end.

