{
   TDBDatePicker control for Lazarus
  - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January 2010
   Зоран Вученовић, јануара 2010.


TDBDatePicker is data-aware version of TDatePicker control.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see COPYING.TXT.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope this control will be useful.
}
unit DBDatePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DatePicker, db, DBCtrls, LCLType;

type

  { TDBDatePicker }

  TDBDatePicker = class(TCustomDatePicker)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetField: TField;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure ConfirmChanges; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property ArrowShape;
    property ShowCalendar;
    property ShowCheckBox;
    property Checked;
    property UseDefaultDateSeparator;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property ReadOnly;
    property AutoSize;
    property Font;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property BorderStyle;
    property BorderSpacing;
    property Enabled;
    property Color;
    property TheDateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property LeadingZeros;
    property ShowHint;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
 // events:
    property OnChange;
    property OnDropDown;
    property OnCloseUp;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnResize;
    property OnUTF8KeyPress;
  end;

implementation

{ TDBDatePicker }

function TDBDatePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBDatePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBDatePicker.SetDataField(const AValue: string);
begin
    FDataLink.FieldName := AValue;
  //  F := GetField;
    //if Assigned(F) then begin

    //end;
  //end;
end;

procedure TDBDatePicker.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, AValue);
end;

procedure TDBDatePicker.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then begin
    //FUpdatingTheDate := True;
    //try
      if FDataLink.Field.IsNull then begin
        TheDate := NullDate;
      end else begin
              // Using the SetTheDateJumpMinMax procedure, instead of property
        SetTheDateJumpMinMax(FDataLink.Field.AsDateTime); // assignment allows
              // this control to display dates from database whose value falls
              // outside of MinDate and MaxDate interval.
              // Note that user still cannot enter such values in the control.
      end;
    //finally
    //  FUpdatingTheDate := False;
    //end;
  end;
end;

procedure TDBDatePicker.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then begin
    if IsNullDate then
      FDataLink.Field.AsVariant := Null
    else
      FDataLink.Field.AsDateTime := TheDate;
  end;
end;

function TDBDatePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBDatePicker.Change;
begin
  //if not FUpdatingTheDate then begin
    if FDataLink.Edit then begin
      FDataLink.Modified;
      inherited Change; // calls OnChange event handler
    end else
      DataChange(Self); // reverts user changes
  //end;
end;

procedure TDBDatePicker.ConfirmChanges;
begin
  inherited ConfirmChanges;

  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
end;

constructor TDBDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //TextForNullDate := 'NULL';
  //FUpdatingTheDate := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TDBDatePicker.Destroy;
begin
  FDataLink.OnUpdateData := nil;
  FDataLink.OnDataChange := nil;
  FreeAndNil(FDataLink);

  inherited Destroy;
end;

end.
