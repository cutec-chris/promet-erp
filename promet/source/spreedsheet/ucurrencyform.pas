unit ucurrencyform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TCurrencyForm }

  TCurrencyForm = class(TForm)
    ButtonBevel: TBevel;
    BtnAdd: TBitBtn;
    BtnCancel: TBitBtn;
    BtnDelete: TBitBtn;
    BtnOK: TBitBtn;
    CurrencyListbox: TListBox;
    LblInfo: TLabel;
    ButtonPanel: TPanel;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    function GetCurrencySymbol: String;
    procedure SetCurrencySymbol(const AValue: String);
  public
    { public declarations }
    property CurrencySymbol: String read GetCurrencySymbol write SetCurrencySymbol;
  end;

var
  CurrencyForm: TCurrencyForm;

implementation

{$R *.lfm}

uses
  fpsCurrency;


{ TCurrencyForm }

procedure TCurrencyForm.BtnAddClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  s := InputBox('Input', 'Currency symbol:', '');
  if s <> '' then begin
    i := CurrencyListbox.Items.IndexOf(s);
    if i = -1 then
      i := CurrencyListbox.Items.Add(s);
    CurrencyListbox.ItemIndex := i;
  end;
end;

procedure TCurrencyForm.BtnDeleteClick(Sender: TObject);
begin
  if CurrencyListbox.ItemIndex > -1 then
    CurrencyListbox.Items.Delete(CurrencyListbox.ItemIndex);
end;

procedure TCurrencyForm.BtnOKClick(Sender: TObject);
begin
  RegisterCurrencies(CurrencyListbox.Items, true);
end;

procedure TCurrencyForm.FormCreate(Sender: TObject);
begin
  GetRegisteredCurrencies(CurrencyListbox.Items);
  CurrencyListbox.ItemIndex := CurrencyListbox.Items.Count-1;
end;

function TCurrencyForm.GetCurrencySymbol: String;
var
  index: Integer;
begin
  index := CurrencyListbox.ItemIndex;
  if index > -1 then
    Result :=  CurrencyListbox.Items[index]
  else
    Result := '';
end;

procedure TCurrencyForm.SetCurrencySymbol(const AValue: String);
begin
  CurrencyListbox.ItemIndex := CurrencyListbox.Items.IndexOf(AValue);
end;

end.



