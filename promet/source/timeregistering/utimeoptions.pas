unit uTimeOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, StdCtrls, ExtCtrls,
  EditBtn, Buttons, uOptionsFrame;

type

  { TfTimeOptions }

  TfTimeOptions = class(TOptionsFrame)
    bAddJob: TSpeedButton;
    bDeleteJob: TSpeedButton;
    cbCategory: TComboBox;
    cbMinimizeonStartTime: TCheckBox;
    cbStartwithStandart: TCheckBox;
    cbWorkTimeMessage: TCheckBox;
    cbExternalTimeEditing: TCheckBox;
    eJob: TComboBox;
    eLink: TEditButton;
    eProject: TEditButton;
    GroupBox1: TGroupBox;
    iLink: TImage;
    Label1: TLabel;
    lActiveEntry: TLabel;
    lEntry: TLabel;
    lJob1: TLabel;
    lNotes: TLabel;
    lProject: TLabel;
    mNotes: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure eLinkButtonClick(Sender: TObject);
    procedure eLinkEditingDone(Sender: TObject);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectEditingDone(Sender: TObject);
    function fSearchOpenEntry(aLink: string): Boolean;
    function fSearchOpenItem(aLink: string): Boolean;
  private
    { private declarations }
    FStandartProject : string;
    FStandartLink : string;
    FStandartCategory : string;
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
{$R *.lfm}
uses uBaseApplication, uSearch,uData,uIntfStrConsts,uBaseDbInterface;

procedure TfTimeOptions.eProjectEditingDone(Sender: TObject);
begin
  FStandartproject := eProject.Text;
end;

function TfTimeOptions.fSearchOpenEntry(aLink: string): Boolean;
begin
  eLink.Text:=Data.GetLinkDesc(aLink);
  FStandartLink := aLink;
  Result := True;
end;

function TfTimeOptions.fSearchOpenItem(aLink: string): Boolean;
begin
  eProject.Text:=Data.GetLinkDesc(aLink);
  FStandartProject := aLink;
  Result := True;
end;

procedure TfTimeOptions.eProjectButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'TIMEPROJ',strSearchfromTimeregisteringMode);
end;

procedure TfTimeOptions.eLinkEditingDone(Sender: TObject);
begin
  FStandartLink := eLink.Text;
end;

procedure TfTimeOptions.eLinkButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenEntry;
  fSearch.Execute(True,'TIMELINK',strSearchfromTimeregisteringMode);
end;

procedure TfTimeOptions.StartTransaction;
begin
  inherited StartTransaction;
  with Application as IBaseDBInterface do
    begin
      cbMinimizeonStartTime.Checked := DBConfig.ReadString('TIMEREGCLOSEONSTART','Y') <> 'N';
      cbStartwithStandart.Checked := DBConfig.ReadString('TIMESTANDARTSTART','Y') = 'Y';
      cbWorkTimeMessage.Checked := DBConfig.ReadString('WORKTIMEMESSAGE','N') = 'Y';
      cbExternalTimeEditing.Checked := DBConfig.ReadString('EXTERNALTIMEEDITING','N') = 'Y';
      eJob.Items.Text:=DBConfig.ReadString('JOBS','');
      eProject.Text:=Data.GetLinkDesc(DBConfig.ReadString('TIMEPROJECT',''));
      FStandartProject := DBConfig.ReadString('TIMEPROJECT','');
      eLink.Text:=Data.GetLinkDesc(DBConfig.ReadString('TIMELINK',''));
      FStandartLink := DBConfig.ReadString('TIMELINK','');
      eJob.Text:=DBConfig.ReadString('TIMEJOB','');
      mNotes.Text:=DBConfig.ReadString('TIMENOTES','');
      cbCategory.Text:=DBConfig.ReadString('TIMECAT','');
    end;
end;

procedure TfTimeOptions.CommitTransaction;
begin
  with Application as IBaseDBInterface do
    begin
      if not cbMinimizeonStartTime.Checked then
        DBConfig.WriteString('TIMEREGCLOSEONSTART','N')
      else
        DBConfig.WriteString('TIMEREGCLOSEONSTART','');
      if cbWorkTimeMessage.Checked then
        DBConfig.WriteString('WORKTIMEMESSAGE','Y')
      else
        DBConfig.WriteString('WORKTIMEMESSAGE','N');
      if not cbStartwithStandart.Checked then
        DBConfig.WriteString('TIMESTANDARTSTART','N')
      else
        DBConfig.WriteString('TIMESTANDARTSTART','');
      if not cbExternalTimeEditing.Checked then
        DBConfig.WriteString('EXTERNALTIMEEDITING','N')
      else
        DBConfig.WriteString('EXTERNALTIMEEDITING','Y');
      DBConfig.WriteString('TIMEPROJECT',FStandartProject);
      DBConfig.WriteString('TIMELINK',FStandartLink);
      DBConfig.WriteString('TIMEJOB',eJob.Text);
      DBConfig.WriteString('TIMENOTES',mNotes.Text);
      DBConfig.WriteString('TIMECAT',cbCategory.Text);
    end;
  inherited CommitTransaction;
end;

procedure TfTimeOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
end;

initialization

end.

