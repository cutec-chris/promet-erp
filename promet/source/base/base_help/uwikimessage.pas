unit uWikiMessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Buttons, StdCtrls, DbCtrls, db, uExtControls, uMessages,
  uIntfStrConsts;

type

  { TfWikiMessage }

  TfWikiMessage = class(TForm)
    Bevel4: TBevel;
    Bevel5: TBevel;
    bItalic: TSpeedButton;
    bSend: TSpeedButton;
    Datasource: TDatasource;
    Content: TDatasource;
    eTitle: TDBEdit;
    eWikiPage: TDBMemo;
    ipHTML: TIpHtmlPanel;
    Label3: TLabel;
    Label4: TLabel;
    Panel6: TPanel;
    Panel7: TPanel;
    pcTabs: TExtMenuPageControl;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    tsPreview: TTabSheet;
    ToolBar1: TPanel;
    tsContent: TTabSheet;
    procedure bSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FConnection: TComponent;
    FDataSet: TMessage;
    { private declarations }
    procedure AddDocuments(Sender: TObject);
  public
    { public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure New(aDir : Variant);
    property DataSet : TMessage read FDataSet;
    property Connection : TComponent read FConnection;
  end;

var
  fWikiMessage: TfWikiMessage;

implementation
{$R *.lfm}
uses uData,uDocumentFrame,uBaseDBInterface;
procedure TfWikiMessage.bSendClick(Sender: TObject);
begin
  FDataSet.CascadicPost;
  FDataSet.Content.CascadicPost;
  {with Application as IBaseDbInterface do
    if UseTransactions then
      Data.CommitTransaction(FConnection);}
  Self.Close;
end;

procedure TfWikiMessage.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if DataSet.CanEdit then
    begin
      DataSet.CascadicCancel;
      {with Application as IBaseDbInterface do
        if UseTransactions then
          Data.Rollback(FConnection);}
    end;
  CloseAction := caFree;
end;

procedure TfWikiMessage.AddDocuments(Sender: TObject);
begin

end;
constructor TfWikiMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := Data.GetNewConnection;
  FDataSet := TMessage.CreateEx(Self,Data,FConnection);
end;
destructor TfWikiMessage.Destroy;
begin
  FDataset.Free;
  inherited Destroy;
end;
procedure TfWikiMessage.New(aDir : Variant);
begin
  {if UseTransactions then
    Data.StartTransaction(FConnection);}
  FDataSet.Insert;
  with DataSet.DataSet do
    begin
      FieldByName('SQL_ID').AsVariant := Data.GetUniID;
      FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
      FieldByName('MSG_ID').AsVariant := FieldByName('SQL_ID').AsVariant;
      FieldByName('TYPE').AsString := 'WIKI';
      FieldByName('READ').AsString := 'N';
      FieldByName('SENDER').AsString := Data.Users.FieldByName('NAME').AsString;
      FieldByName('SENDDATE').AsDateTime := Now();
      FieldByName('TREEENTRY').AsVariant := aDir;
    end;
  FDataSet.Content.Insert;
  with DataSet.Content.DataSet do
    begin
      FieldByName('ID').AsString := FDataSet.FieldByName('ID').AsString;
      FieldByName('DATATYP').AsString := 'WIKI';
    end;
  Datasource.DataSet := FDataSet.DataSet;
  Content.DataSet := FDataSet.Content.DataSet;
  pcTabs.AddTabClass(TfDocumentFrame,strAttatchments,@AddDocuments);
  Show;
end;
end.

