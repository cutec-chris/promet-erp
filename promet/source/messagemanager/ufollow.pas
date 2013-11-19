unit ufollow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, types;

type
  TfFollow = class(TForm)
    bNew: TBitBtn;
    bDelete: TBitBtn;
    ButtonPanel1: TButtonPanel;
    lbFollow: TListBox;
    procedure bDeleteClick(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
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

procedure TfFollow.FormCreate(Sender: TObject);
begin
  //Messages
  if Data.Users.Rights.Right('MESSAGES') > RIGHT_NONE then
    begin
      AddSearchAbleDataSet(TMessageList);
    end;
  //Add Contacts
  if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
    begin
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
    end;
  //Add Masterdata stuff
  if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE) then
    begin
      AddSearchAbleDataSet(TMasterdataList);
    end;
  //Projects
  if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
    begin
      AddSearchAbleDataSet(TProjectList);
    end;
  //Wiki
  if (Data.Users.Rights.Right('WIKI') > RIGHT_NONE) then
    begin
      AddSearchAbleDataSet(TWikiList);
    end;
end;

function TfFollow.fSearchOpenItem(aLink: string): Boolean;
begin
  lbFollow.Items.Add(fSearch.GetLink);
  Data.Users.Follows.Insert;
  Data.Users.Follows.FieldByName('LINK').AsString:=fSearch.GetLink;
  Data.Users.Follows.DataSet.Post;
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
  Data.Users.Follows.Open;
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
  Result := Showmodal = mrOK;
end;

end.

