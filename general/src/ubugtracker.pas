unit uBugtracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, httpsend,Clipbrd,uGeneralStrConsts,Utils,
  XMLPropStorage,htmlconvert,FileUtil;

type

  { TfBugTracker }
  TfBugTracker = class(TForm)
    bAdd: TButton;
    bClose: tbutton;
    cbFilter: TComboBox;
    lFilter: TLabel;
    sgBugList: TStringGrid;
    Properties: TXMLPropStorage;
    procedure aaddclick(sender: tobject);
    procedure bcloseclick(sender: tobject);
    procedure cbFilterSelect(Sender: TObject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formshow(sender: tobject);
    procedure sgbuglistdblclick(sender: tobject);
  private
    { private declarations }
    FUsername : string;
  public
    { public declarations }
    http: THttpSend;
    TrackerURL : string;
    ProjectID : Integer;
    procedure Refresh;
    procedure SetLanguage(Lang : string);
    procedure AddBug(Additional : string);
  end;

var
  fBugTracker: TfBugTracker;

const
  list_start                = '<td><input type="checkbox" name="bug_arr[]" value="';
  id_end                    = '" />';
  severity_start            = '</td><td class="center">1</td><td class="center">';
  severity_end              = '</td><td class="center"><u><a title="';
  status_start              = '">';
  status_end                = '</td><td class="center">';
  summary_start             = '</td><td class="left">';
  summary_end               = '</td></tr>';

  reproduceable_start       = '<!-- Reproducibility -->';
  reproduceable_next        = '<td>';
  reproduceable_end         = '</td>';
  description_start         = '<!-- Description -->';
  description_next          = '<td colspan="5">';
  description_end           = '</td>';
  additional_start          = '<!-- Additional Information -->';
  additional_next           = '<td colspan="5">';
  additional_end            = '</td>';
  user_start                = '<!-- Custom Fields -->';
  user_next                 = '<td colspan="5">';
  user_end                  = '</td>';
  mail_next                 = '<td colspan="5">';
  mail_end                  = '</td>';
  note_start                = '<tr class="bugnote" name="';
  note_timedate_start       = '<span class="small">';
  note_timedate_end         = '</span><br />';
  note_text_start           = '<td class="bugnote-note-public">';
  note_text_end             = '</td>';
  history_start             = '<tr class="row-';
  history_timedate_start    = '<td class="small-caption">';
  history_timedate_end      = '</td>';
  history_name_start        = '<td class="small-caption">';
  history_entry_start       = '<td class="small-caption">';
  history_entry_end         = '</tr>';

  replicable_start          = '<select tabindex="2" name="reproducibility">';
  replicable_1_start        = '<option value="10">';
  replicable_1_end          = '</option>';
  replicable_2_start        = '<option value="30">';
  replicable_2_end          = '</option>';
  replicable_3_start        = '<option value="50">';
  replicable_3_end          = '</option>';
  replicable_4_start        = '<option value="70">';
  replicable_4_end          = '</option>';
  replicable_5_start        = '<option value="90">';
  replicable_5_end          = '</option>';
  replicable_6_start        = '<option value="100">';
  replicable_6_end          = '</option>';
  severity__start           = '<select tabindex="3" name="severity">';
  severity_1_start          = '<option value="10">';
  severity_1_end            = '</option>';
  severity_2_start          = '<option value="20">';
  severity_2_end            = '</option>';
  severity_3_start          = '<option value="30">';
  severity_3_end            = '</option>';
  severity_4_start          = '<option value="40">';
  severity_4_end            = '</option>';
  severity_5_start          = '<option value="50" selected="selected" >';
  severity_5_end            = '</option>';
  severity_6_start          = '<option value="60">';
  severity_6_end            = '</option>';
  severity_7_start          = '<option value="70">';
  severity_7_end            = '</option>';
  severity_8_start          = '<option value="80">';
  severity_8_end            = '</option>';

  filter_allopen            = '/view_all_set.php?type=3&source_query_id=7';
  filter_allopenfeature     = '/view_all_set.php?type=3&source_query_id=9';
  filter_allopenbugs        = '/view_all_set.php?type=3&source_query_id=12';
  filter_allclosed          = '/view_all_set.php?type=3&source_query_id=11';
  filter_all                = '/view_all_set.php?type=3&source_query_id=-1';

implementation

uses uviewaddbug,uaddnote,uaddbug;

{ TfBugTracker }

procedure tfbugtracker.formcreate(sender: tobject);
begin
  http := THttpSend.Create;
  http.Timeout := 1700;
  if Application.HasOption('c','config-path') then
    Properties.FileName := UTF8ToSys(Application.GetOptionValue('c','config-path')+DirectorySeparator+'bugtracker.xml')
  else
    Properties.FileName := UTF8Tosys(GetConfigDir(copy(ExtractFileName(Application.Exename),0,length(ExtractFileName(Application.Exename))-length(ExtractFileExt(ExtractFileName(Application.Exename)))))+DirectorySeparator+'bugtracker.xml');
  Properties.FileName := UTF8ToSys(Properties.FileName);
  Properties.Restore;
  fViewAddBug := TfViewAddBug.Create(Self);
  fAddNote := TfAddNote.Create(Self);
  fAddBug := TfAddBug.Create(Self);
end;

procedure tfbugtracker.aaddclick(sender: tobject);
begin
  AddBug('');
end;

procedure tfbugtracker.bcloseclick(sender: tobject);
begin
  Close;
end;

procedure TfBugTracker.cbFilterSelect(Sender: TObject);
begin
  Refresh;
end;

procedure tfbugtracker.formdestroy(sender: tobject);
begin
  Properties.Save;
  http.Free;
end;

procedure tfbugtracker.formshow(sender: tobject);
begin
  Refresh;
end;

procedure tfbugtracker.sgbuglistdblclick(sender: tobject);
var
  tmp : string;
  sl : TStringList;
begin
  sl := TStringList.Create;
  with fViewAddBug do
    begin
      Caption := strViewFeatureRequestorBug;
      http.Clear;
      http.HTTPMethod('GET','http://'+TrackerURL+'/view.php?id='+sgBugList.Cells[0,sgBugList.Row]);
      sl.LoadFromStream(http.Document);
      tmp := sl.Text;
      lID.Caption := sgBugList.Cells[0,sgBugList.Row];
      cbSeverity.Text := sgBugList.Cells[1,sgBugList.Row];
      eSummary.Text := sgBugList.Cells[3,sgBugList.Row];
      tmp := copy(tmp,pos(reproduceable_start,tmp)+length(reproduceable_start),length(tmp));
      tmp := copy(tmp,pos(reproduceable_next,tmp)+length(reproduceable_next),length(tmp));
      cbReplicable.Text := trim(copy(tmp,0,pos(reproduceable_end,tmp)-1));
      tmp := copy(tmp,pos(description_start,tmp)+length(description_start),length(tmp));
      tmp := copy(tmp,pos(description_next,tmp)+length(description_next),length(tmp));
      mDescription.Text := StringReplace(trim(copy(tmp,0,pos(description_end,tmp)-1)),'<br />',#13,[rfreplaceAll]);
      tmp := copy(tmp,pos(additional_start,tmp)+length(additional_start),length(tmp));
      tmp := copy(tmp,pos(additional_next,tmp)+length(additional_next),length(tmp));
      mAdditionalInfos.Text := StringReplace(trim(copy(tmp,0,pos(additional_end,tmp)-1)),'<br />',#13,[rfreplaceAll]);
      tmp := copy(tmp,pos(user_start,tmp)+length(user_start),length(tmp));
      tmp := copy(tmp,pos(user_next,tmp)+length(user_next),length(tmp));
      eName.text := trim(copy(tmp,0,pos(user_end,tmp)-1));
      tmp := copy(tmp,pos(mail_next,tmp)+length(mail_next),length(tmp));
      eMail.text := HTMLToTXT(trim(copy(tmp,0,pos(mail_end,tmp)-1)));
{      tvNotes.Clear;
      while pos(note_start,tmp) > 0 do
        begin
          tmp := copy(tmp,pos(note_start,tmp)+length(note_start),length(tmp));
          TN := tvNotes.AddChild(nil);
          tvNotes.MultiLine[TN] := True;
          tmp := copy(tmp,pos(note_timedate_start,tmp)+length(note_timedate_start),length(tmp));
          PNoteEntry(tvNotes.GetNodeData(TN))^.TimeDate := trim(copy(tmp,2,pos(note_timedate_end,tmp)-3));
          tmp := copy(tmp,pos(note_text_start,tmp)+length(note_text_start),length(tmp));
          PNoteEntry(tvNotes.GetNodeData(TN))^.Note := trim(copy(tmp,0,pos(note_text_end,tmp)-1));
        end;}
      sgHistory.RowCount := 1;
      //1. Eintrag überspringen
      if pos(history_start,tmp) > 0 then
        begin
          tmp := copy(tmp,pos(history_start,tmp)+length(history_start),length(tmp));
          tmp := copy(tmp,pos(history_timedate_start,tmp)+length(history_timedate_start),length(tmp));
          tmp := copy(tmp,pos(history_name_start,tmp)+length(history_name_start),length(tmp));
          tmp := copy(tmp,pos(history_entry_start,tmp)+length(history_entry_start),length(tmp));
        end;
      while pos(history_start,tmp) > 0 do
        begin
          tmp := copy(tmp,pos(history_start,tmp)+length(history_start),length(tmp));
          sgHistory.RowCount := sgHistory.RowCount+1;
          tmp := copy(tmp,pos(history_timedate_start,tmp)+length(history_timedate_start),length(tmp));
          sgHistory.Cells[0,sgHistory.RowCount-1] := trim(copy(tmp,0,pos(history_timedate_end,tmp)-1));
          tmp := copy(tmp,pos(history_name_start,tmp)+length(history_name_start),length(tmp));
          tmp := copy(tmp,pos(history_entry_start,tmp)+length(history_entry_start),length(tmp));
          sgHistory.Cells[1,sgHistory.RowCount-1] := StringReplace(StringReplace(StringReplace(StringReplace(HTMLDecode(HTMLToTXT(trim(copy(tmp,0,pos(history_entry_end,tmp)-1)))),#13,'',[rfReplaceAll]),#10,'',[rfReplaceAll]),#6,'',[rfReplaceAll]),#9,'',[rfReplaceAll]);
        end;
      Showmodal;
    end;
  sl.Free;
end;

procedure TfBugTracker.Refresh;
var
  tmp : string;
  sl : TStringList;
begin
  sgBugList.RowCount := 1;
  sl := TStringList.Create;
  http.Clear;
  http.HTTPMethod('GET','http://'+TrackerURL+'/login.php?username='+FUsername+'&password='+FUsername);
  if http.ResultCode >= 400 then
    begin
      http.HTTPMethod('GET','http://'+TrackerURL+'/login.php?username='+FUsername+'&password='+FUsername);
      if http.ResultCode >= 400 then
        begin
          Close;
          exit;
        end;
    end;
  http.Clear;
  http.HTTPMethod('GET','http://'+TrackerURL+'/set_project.php?project_id='+IntToStr(ProjectID));
  if http.ResultCode >= 400 then
    http.HTTPMethod('GET','http://'+TrackerURL+'/set_project.php?project_id='+IntToStr(ProjectID));
  HTTP.clear;
  case cbFilter.ItemIndex of
  0:http.HTTPMethod('GET','http://'+TrackerURL+filter_allopen);
  1:http.HTTPMethod('GET','http://'+TrackerURL+filter_allopenfeature);
  2:http.HTTPMethod('GET','http://'+TrackerURL+filter_allopenbugs);
  3:http.HTTPMethod('GET','http://'+TrackerURL+filter_allclosed);
  4:http.HTTPMethod('GET','http://'+TrackerURL+filter_all);
  else
    http.HTTPMethod('GET','http://'+TrackerURL+filter_allopen);
  end;
  http.Clear;
  http.HTTPMethod('GET','http://'+TrackerURL+'/print_all_bug_page.php');
  if http.ResultCode >= 400 then
    http.HTTPMethod('GET','http://'+TrackerURL+'/print_all_bug_page.php');
  sl.LoadFromStream(http.Document);
  tmp := sl.Text;
  if tmp = '' then
    begin
      Showmessage(strLogintoBugtrackerfailed);
      Close;
      exit;
    end;
  sl.Free;
  while pos(list_start,tmp) > 0 do
    begin
      tmp := copy(tmp,pos(list_start,tmp)+length(list_start),length(tmp));
      sgBugList.RowCount := sgBugList.RowCount+1;
      sgBugList.Cells[0,sgBugList.RowCount-1] := HTMLToTXT(copy(tmp,0,pos(id_end,tmp)-1));
      tmp := copy(tmp,pos(severity_start,tmp)+length(severity_start),length(tmp));
      sgBugList.Cells[1,sgBugList.RowCount-1] := HTMLToTXT(copy(tmp,0,pos(severity_end,tmp)-1));
      tmp := copy(tmp,pos(severity_end,tmp)+length(severity_end),length(tmp));
      tmp := copy(tmp,pos(status_start,tmp)+length(status_start),length(tmp));
      sgBugList.Cells[2,sgBugList.RowCount-1] := HTMLToTXT(copy(tmp,0,pos(status_end,tmp)-1));
      tmp := copy(tmp,pos(summary_start,tmp)+length(summary_start),length(tmp));
      sgBugList.Cells[3,sgBugList.RowCount-1] := HTMLToTXT(copy(tmp,0,pos(summary_end,tmp)-1));
    end;
end;

procedure TfBugTracker.SetLanguage(Lang: string);
begin
  FUserName := lowercase(Lang);
  if FUsername = '' then
    FUsername := 'english';
  fViewAddBug.SetLanguage;
  fAddNote.SetLanguage;
  fAddBug.SetLanguage;
  bAdd.Caption := strAdd;
  bClose.Caption := strClose;
  Caption := strBugtracker;
  sgBugList.Columns[0].Title.Caption := strID;
  sgBugList.Columns[1].Title.Caption := strSeverity;
  sgBugList.Columns[2].Title.Caption := strStatus;
  sgBugList.Columns[3].Title.Caption := strSummary;
  cbFilter.Items.Clear;
  cbFilter.Items.Add(strAllOpen);
  cbFilter.Items.Add(strAllOpenFeaturerequests);
  cbFilter.Items.Add(strAllOpenBugs);
  cbFilter.Items.Add(strAllClosed);
  cbFilter.Items.Add(strAllAll);
  lFilter.Caption := strFilter;
end;

procedure TfBugTracker.AddBug(Additional: string);
var
  tmp : string;
  sl : TStringList;
//  tn : PVirtualNode;
begin
  sl := TStringList.Create;
  with fAddBug do
    begin
      Caption := strAddFeatureRequestorBug;
      cbReplicable.Text := '';
      cbSeverity.Text := '';
      eSummary.Text := '';
      mDescription.Lines.Clear;
      mAdditionalInfos.Enabled := True;
      mAdditionalInfos.Lines.text := Additional;
      eName.Enabled := True;
      eMail.Enabled := True;
      eName.Text := Properties.StoredValue['USERNAME'];
      eMail.Text := Properties.StoredValue['EMAIL'];
      http.Clear;
      http.HTTPMethod('GET','http://'+TrackerURL+'/bug_report_page.php');
      sl.LoadFromStream(http.Document);
      tmp := sl.Text;
      cbReplicable.Items.Clear;
      tmp := copy(tmp,pos(replicable_start,tmp)+length(replicable_start),length(tmp));
      tmp := copy(tmp,pos(replicable_1_start,tmp)+length(replicable_1_start),length(tmp));
      cbReplicable.Items.Add(trim(copy(tmp,0,pos(replicable_1_end,tmp)-1)));
      tmp := copy(tmp,pos(replicable_2_start,tmp)+length(replicable_2_start),length(tmp));
      cbReplicable.Items.Add(trim(copy(tmp,0,pos(replicable_2_end,tmp)-1)));
      tmp := copy(tmp,pos(replicable_3_start,tmp)+length(replicable_3_start),length(tmp));
      cbReplicable.Items.Add(trim(copy(tmp,0,pos(replicable_3_end,tmp)-1)));
      tmp := copy(tmp,pos(replicable_4_start,tmp)+length(replicable_4_start),length(tmp));
      cbReplicable.Items.Add(trim(copy(tmp,0,pos(replicable_4_end,tmp)-1)));
      tmp := copy(tmp,pos(replicable_5_start,tmp)+length(replicable_5_start),length(tmp));
      cbReplicable.Items.Add(trim(copy(tmp,0,pos(replicable_5_end,tmp)-1)));
      tmp := copy(tmp,pos(replicable_6_start,tmp)+length(replicable_6_start),length(tmp));
      cbReplicable.Items.Add(trim(copy(tmp,0,pos(replicable_6_end,tmp)-1)));
      cbSeverity.Items.Clear;
      tmp := copy(tmp,pos(severity__start,tmp)+length(severity__start),length(tmp));
      tmp := copy(tmp,pos(severity_1_start,tmp)+length(severity_1_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_1_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_2_start,tmp)+length(severity_2_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_2_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_3_start,tmp)+length(severity_3_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_3_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_4_start,tmp)+length(severity_4_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_4_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_5_start,tmp)+length(severity_5_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_5_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_6_start,tmp)+length(severity_6_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_6_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_7_start,tmp)+length(severity_7_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_7_end,tmp)-1)));
      tmp := copy(tmp,pos(severity_8_start,tmp)+length(severity_8_start),length(tmp));
      cbSeverity.Items.Add(trim(copy(tmp,0,pos(severity_8_end,tmp)-1)));
      Height := 252;
      HorzScrollBar.Visible:=False;
      VertScrollBar.Visible:=False;
      Showmodal;
    end;
  sl.Free;
end;

initialization
  {$I ubugtracker.lrs}

end.



