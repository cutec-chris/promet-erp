unit ufollow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, ExtCtrls, types;

type

  { TfFollow }

  TfFollow = class(TForm)
    bNew: TBitBtn;
    bDelete: TBitBtn;
    ButtonPanel1: TButtonPanel;
    eFilter: TEdit;
    IdleTimer1: TIdleTimer;
    Label1: TLabel;
    lbFollow: TListBox;
    procedure bDeleteClick(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure eFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    procedure IdleTimer1Timer(Sender: TObject);
    procedure lbFollowDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FOK : Boolean;
  public
    { public declarations }
    function Execute : Boolean;
    procedure Refresh;
  end;

var
  fFollow: TfFollow;

implementation
{$R *.lfm}
uses uData,uSearch,uBaseDBInterface,uBaseSearch,uMessages,uPerson,
  uMasterdata,uProjects,uWiki;

procedure TfFollow.bNewClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(False,'FOLLOWERS','');
end;

procedure TfFollow.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfFollow.eFilterChange(Sender: TObject);
begin
  IdleTimer1.Enabled:=False;
  IdleTimer1.Enabled:=True;
end;

procedure TfFollow.FormCreate(Sender: TObject);
begin
  //Messages
  if Data.Users.Rights.Right('MESSAGES') > RIGHT_VIEW then
    begin
      AddSearchAbleDataSet(TMessageList);
    end;
  //Add Contacts
  if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_VIEW then
    begin
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
    end;
  //Add Masterdata stuff
  if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_VIEW) then
    begin
      AddSearchAbleDataSet(TMasterdataList);
    end;
  //Projects
  if (Data.Users.Rights.Right('PROJECTS') > RIGHT_VIEW) then
    begin
      AddSearchAbleDataSet(TProjectList);
    end;
  //Wiki
  if (Data.Users.Rights.Right('WIKI') > RIGHT_VIEW) then
    begin
      AddSearchAbleDataSet(TWikiList);
    end;
end;

function TfFollow.fSearchOpenItem(aLink: string): Boolean;
var
  aLinks: String;
begin
  aLinks := fSearch.GetLink(true);
  while pos(';',aLinks)>0 do
    begin
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
      Data.Users.Follows.Insert;
      Data.Users.Follows.FieldByName('LINK').AsString:=aLink;
      Data.Users.Follows.DataSet.Post;
      lbFollow.Items.Add(aLink);
    end;
end;

procedure TfFollow.IdleTimer1Timer(Sender: TObject);
begin
  IdleTimer1.Enabled:=False;
  Data.Users.Follows.Filter(Data.ProcessTerm('UPPER("LINK")=UPPER(''*'+eFilter.Text+'*'')'));
  Refresh;
end;

procedure TfFollow.lbFollowDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with TListBox(Control).Canvas do
    begin
      FillRect(aRect);
      TextOut(aRect.Left + 1, aRect.Top + 1, Data.GetLinkDesc(TListBox(Control).Items[Index]));
    end;
end;

procedure TfFollow.OKButtonClick(Sender: TObject);
begin
  FOK := True;
  Close;
end;

procedure TfFollow.bDeleteClick(Sender: TObject);
begin
  if Data.Users.Follows.DataSet.Locate('LINK',lbFollow.Items[lbFollow.ItemIndex],[]) then
    begin
      Data.Users.Follows.DataSet.Delete;
      lbFollow.Items.Delete(lbFollow.ItemIndex);
    end;
end;

function TfFollow.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfFollow,fFollow);
      Self := fFollow;
    end;
  FOK := False;
  Data.Users.Follows.Open;
  Refresh;
  Show;
  while Visible do
    begin
      Application.ProcessMessages;
      sleep(100);
    end;
  Result := FOK;
  Data.Users.Follows.Filter('');
end;

procedure TfFollow.Refresh;
begin
  lbFollow.Clear;
  with Data.Users.Follows do
    begin
      First;
      while not EOF do
        begin
          lbFollow.Items.Add(FieldByName('LINK').AsString);
          Next;
        end;
    end;
end;

end.

