unit uOrderTypeOptions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DBGrids, Buttons,
  DbCtrls, ExtCtrls, ComCtrls, db, uBaseDBClasses, uBaseERPDBClasses,
  uOptionsFrame, uOrder;
type

  { TforderTypeOptions }

  TforderTypeOptions = class(TOptionsFrame)
    cbIsDerivate: TDBCheckBox;
    cbSIAcc: TDBCheckBox;
    cbSIINVO: TDBCheckBox;
    cbSIINVR: TDBCheckBox;
    cbSIOrder: TDBCheckBox;
    cbSIPos: TDBCheckBox;
    cbSIProd: TDBCheckBox;
    cbTextTyp: TComboBox;
    DBNavigator1: TDBNavigator;
    eDefaultPosTyp: TDBEdit;
    eDerivates: TDBEdit;
    eNumberset: TDBEdit;
    eOrderType: TDBComboBox;
    gbVisibility: TGroupBox;
    gOrderStatus: TDBGrid;
    lDefaultPosTyp: TLabel;
    lDerivates: TLabel;
    lvImages: TListView;
    lNumberset: TLabel;
    lType: TLabel;
    OrderTypeDS: TDatasource;
    rgAddCHist: TDBRadioGroup;
    rgAddDunning: TDBRadioGroup;
    rgAddINVO: TDBRadioGroup;
    rgAddINVR: TDBRadioGroup;
    rgAddJournal: TDBRadioGroup;
    rgAddreserved: TDBRadioGroup;
    rgAddSerials: TDBRadioGroup;
    rgAddStorage: TDBRadioGroup;
    rgAddToMainorder: TDBRadioGroup;
    procedure cbTextTypChange(Sender: TObject);
  private
    { private declarations }
    aConnection: TComponent;
    aStates: TOrderTyp;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
{$R *.lfm}
uses uData,uBaseVisualControls;
procedure TforderTypeOptions.cbTextTypChange(Sender: TObject);
begin
  if not aStates.CanEdit then aStates.DataSet.Edit;
  aStates.FieldByName('TEXTTYP').AsInteger := cbTextTyp.ItemIndex;
  if cbTextTyp.ItemIndex = -1 then
    aStates.FieldByName('TEXTTYP').Clear;
end;
constructor TforderTypeOptions.Create(TheOwner: TComponent);
var
  aItem: TListItem;
  i: Integer;
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aStates := TOrderTyp.Create(Self,Data,aConnection);
  OrderTypeDS.DataSet := aStates.DataSet;
  Data.TextTyp.CreateTable;
  Data.Texttyp.Open;
  Data.TextTyp.DataSet.First;
  while not Data.TextTyp.DataSet.EOF do
    begin
      cbTextTyp.Items.Add(Data.TextTyp.FieldByName('NAME').AsString);
      Data.TextTyp.DataSet.Next;
    end;
  with fVisualControls.StatusImages do
    begin
      for i := 0 to fVisualControls.StatusImages.Count-1 do
        begin
          aItem := lvImages.Items.Add;
          aItem.Caption:='';
          aItem.ImageIndex:=i;
        end;
    end;
end;
destructor TforderTypeOptions.Destroy;
begin
  aStates.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;
procedure TforderTypeOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aStates.Open;
end;
procedure TforderTypeOptions.CommitTransaction;
begin
  aStates.CascadicPost;
  Data.Commit(aConnection);
  inherited CommitTransaction;
end;
procedure TforderTypeOptions.RollbackTransaction;
begin
  aStates.CascadicCancel;
  Data.Rollback(aConnection);
  inherited RollbackTransaction;
end;
end.

