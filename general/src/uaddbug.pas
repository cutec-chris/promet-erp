unit uAddBug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,uGeneralStrConsts,Utils;

type

  { TfaDDbUG }

  TfaDDbUG = class(TForm)
    bAbort: TButton;
    bSubmit: TButton;
    cbReplicable: TComboBox;
    cbSeverity: TComboBox;
    eMail: TEdit;
    eName: TEdit;
    eSummary: TEdit;
    lAdditionalInfos: TLabel;
    lDescription: TLabel;
    lReplicable: TLabel;
    lSeverity: TLabel;
    lSummary: TLabel;
    lYourmail: TLabel;
    lYourname: TLabel;
    mAdditionalInfos: TMemo;
    mDescription: TMemo;
    procedure bAbortClick(Sender: TObject);
    procedure bSubmitClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
  end; 

var
  faDDbUG: TfaDDbUG;

implementation

uses uBugtracker;

{ TfaDDbUG }

procedure TfaDDbUG.bSubmitClick(Sender: TObject);
var
  outp : string;
begin
  if eName.Text = '' then
    begin
      Showmessage(strFieldNamecannotbeclear);
      exit;
    end;
  if not ((pos('@',eMail.Text) > 0) and (pos('.',eMail.Text) > 0)) then
    begin
      Showmessage(strFieldMailcannotbeclear);
      exit;
    end;
  if eSummary.Text = '' then
    begin
      Showmessage(strFieldSummarycannotbeclear);
      exit;
    end;
  if mDescription.Lines.Text = '' then
    begin
      Showmessage(strFieldDescriptioncannotbeclear);
      exit;
    end;
  if cbSeverity.ItemIndex = -1 then
    begin
      Showmessage(strFieldSeveritycannotbeclear);
      exit;
    end;
  if cbReplicable.ItemIndex = -1 then
    begin
      Showmessage(strFieldReplicablecannotbeclear);
      exit;
    end;
  fBugtracker.Properties.StoredValue['USERNAME'] := eName.Text;
  fBugtracker.Properties.StoredValue['EMAIL'] := eMail.Text;
  outp := 'http://'+fBugtracker.TrackerURL+'/bug_report.php?m_id=0&project_id='+IntToStr(fBugtracker.ProjectID)+'&handler_id=0&category=1&reproducibility=';
  case cbReplicable.ItemIndex of
  0:outp := outp+'10';
  1:outp := outp+'30';
  2:outp := outp+'50';
  3:outp := outp+'70';
  4:outp := outp+'90';
  5:outp := outp+'100';
  end;
  outp := outp+'&severity=';
  case cbSeverity.ItemIndex of
  0:outp := outp+'10';
  1:outp := outp+'20';
  2:outp := outp+'30';
  3:outp := outp+'40';
  4:outp := outp+'50';
  5:outp := outp+'60';
  6:outp := outp+'70';
  7:outp := outp+'80';
  end;
  outp := outp+'&summary='+HTTPEncode(eSummary.Text);
  outp := outp+'&description='+HTTPEncode(mDescription.Lines.Text);
  outp := outp+'&additional_info='+HTTPEncode(mAdditionalInfos.Lines.Text);
  outp := outp+'&custom_field_1='+HTTPEncode(eName.Text);
  outp := outp+'&custom_field_2='+HTTPEncode(eMail.Text);
  fBugtracker.http.Clear;
  fBugtracker.http.HTTPMethod('GET',outp);
  fBugTracker.Refresh;
  Close;
end;

procedure TfaDDbUG.SetLanguage;
begin
  bSubmit.Caption := strSubmit;
  bAbort.Caption := strAbort;
  lReplicable.Caption := strReplicable;
  lSeverity.Caption := strSeverity;
  lDescription.Caption := strDescription;
  lAdditionalInfos.Caption := strAdditionalInfos;
  lSummary.Caption := strSummary;
  lYourname.caption := strSubmittername;
  lYourmail.Caption := strSubmittermail;
end;

procedure TfaDDbUG.bAbortClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I uaddbug.lrs}

end.

