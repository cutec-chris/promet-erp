unit uviewaddbug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, Buttons,
  ExtCtrls,Utils,uGeneralStrConsts,Clipbrd;

type

  { TfViewAddBug }

  TfViewAddBug = class(TForm)
    bAdd: TButton;
    bClose: TButton;
    Bevel1: TBevel;
    cbReplicable: TComboBox;
    cbSeverity: TComboBox;
    eMail: TEdit;
    eName: TEdit;
    eSummary: TEdit;
    lAdditionalInfos: TLabel;
    lDescription: TLabel;
    lHistory: TLabel;
    lID: TLabel;
    lIDLabel: TLabel;
    lReplicable: TLabel;
    lSeverity: TLabel;
    lSummary: TLabel;
    lYourmail: TLabel;
    lYourname: TLabel;
    mAdditionalInfos: TMemo;
    mDescription: TMemo;
    sgHistory: TStringGrid;
    procedure baddclick(sender: tobject);
    procedure ccloseclick(sender: tobject);
    procedure formcreate(sender: tobject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
  end; 

var
  fViewAddBug: TfViewAddBug;

implementation

uses uBugTracker,uAddNote;

{ TfViewAddBug }

procedure tfviewaddbug.formcreate(sender: tobject);
begin
//  tvNotes.NodeDataSize := sizeof(TNoteEntry);
end;
{
procedure tfviewaddbug.tvnotesgettext(sender: tbasevirtualtree; node: pvirtualnode; column: tcolumnindex; texttype: tvsttexttype; var celltext: widestring);
var
  Data : PNoteEntry;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    case Column of
    0:Celltext := Data^.TimeDate;
    1:Celltext := Data^.Note;
    end;
end;

procedure tfviewaddbug.tvnotesresize(sender: tobject);
begin
  tvNotes.Header.Columns[1].Width := tvNotes.Width-60;
end;
}
procedure tfviewaddbug.setlanguage;
begin
  lReplicable.Caption := strReplicable;
  lIDLabel.Caption := strID;
  lSeverity.Caption := strSeverity;
  lDescription.Caption := strDescription;
  lAdditionalInfos.Caption := strAdditionalInfos;
//  lNotes.Caption := strNotice;
  lHistory.Caption := strHistory;
  bAdd.Caption := strAddNote;
  bClose.Caption := strClose;
  lSummary.Caption := strSummary;
  sgHistory.Columns[0].Title.Caption := strDate;
  sgHistory.Columns[1].Title.Caption := strChange;
  lYourname.caption := strSubmittername;
  lYourmail.Caption := strSubmittermail;
end;

procedure tfviewaddbug.ccloseclick(sender: tobject);
begin
  Close;
end;

procedure tfviewaddbug.baddclick(sender: tobject);
{var
  TN: PVirtualNode;}
begin
{  fAddNote.mNote.Lines.Text := '';
  fAddNote.Showmodal;
  fBugTracker.http.Clear;
  if fAddNote.Add then
    begin
      fBugtracker.http.HTTPMethod('GET','http://'+fBugtracker.TrackerURL+'/bugnote_add.php?bug_id='+lID.Caption+'&bugnote_text='+HTTPEncode(fAddNote.mNote.Lines.Text));
      TN := tvNotes.AddChild(nil);
      tvNotes.MultiLine[TN] := True;
      PNoteEntry(tvNotes.GetNodeData(TN))^.TimeDate := '';
      PNoteEntry(tvNotes.GetNodeData(TN))^.Note := fAddNote.mNote.Lines.Text;
   end;}
end;

initialization
  {$I uviewaddbug.lrs}

end.

