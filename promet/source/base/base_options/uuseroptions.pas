unit uuseroptions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Buttons, DbCtrls, StdCtrls,
  ComCtrls, ExtCtrls, Menus, uOptionsFrame, db, uBaseDbClasses,
  uBaseVisualControls, DBZVDateTimePicker;
type
  TUserTreeEntry = class
    Rec : Int64;
    DataSource : TDataSource;
  end;

  { TfUserOptions }

  TfUserOptions = class(TOptionsFrame)
    bNewGroup: TSpeedButton;
    bNewUser: TSpeedButton;
    bResetPassword: TBitBtn;
    bSaveUser: TSpeedButton;
    cbPosition: TDBComboBox;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    eCustomerNumber2: TDBEdit;
    eCustomerNumber3: TDBEdit;
    eEmploymentDate: TDBZVDateTimePicker;
    eLeaveDate: TDBZVDateTimePicker;
    lCustomerNumber3: TLabel;
    lCustomerNumber4: TLabel;
    Paygroups: TDatasource;
    DBLookupComboBox1: TDBLookupComboBox;
    eCustomerNumber: TDBEdit;
    eCustomerNumber1: TDBEdit;
    eDepartment: TDBEdit;
    eIDCode: TDBEdit;
    eUsername: TDBEdit;
    ilState: TImageList;
    lCustomerNumber: TLabel;
    lCustomerNumber1: TLabel;
    lCustomerNumber2: TLabel;
    lDepartment: TLabel;
    lEmploymentDate: TLabel;
    lIDCode: TLabel;
    lLeaveDate: TLabel;
    lPosition: TLabel;
    lRights: TLabel;
    lUsername: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miDeleteRight: TMenuItem;
    miNone: TMenuItem;
    miRead: TMenuItem;
    pmRights: TPopupMenu;
    pUser: TPanel;
    SpeedButton1: TSpeedButton;
    tvRights: TTreeView;
    tvUsers: TTreeView;
    UsersDS: TDatasource;
    procedure bNewGroupClick(Sender: TObject);
    procedure bNewUserClick(Sender: TObject);
    procedure bResetPasswordClick(Sender: TObject);
    procedure bSaveUserClick(Sender: TObject);
    procedure eUsernameChange(Sender: TObject);
    procedure miRightsClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure tvUsersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvUsersDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvUsersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvUsersSelectionChanged(Sender: TObject);
  private
    { private declarations }
    aConnection : TComponent;
    aUsers : TUser;
    aPaygroups: TPayGroups;
    procedure UpdateRights;
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
uses uData,Variants,upaygroups;
procedure TfUserOptions.tvUsersSelectionChanged(Sender: TObject);
begin
  if not Assigned(tvUsers.Selected) then exit;
  if (aUsers.FieldByName('SQL_ID').AsVariant <> Null)
  and (aUsers.FieldByName('SQL_ID').AsVariant <> TUserTreeEntry(tvUsers.Selected.Data).Rec) then
    aUsers.GotoBookmark(TUserTreeEntry(tvUsers.Selected.Data).Rec);
  UpdateRights;
end;
procedure TfUserOptions.UpdateRights;
var
  aNode: TTreeNode;
begin
  aNode := tvRights.Items[0];
  while Assigned(aNode) do
    begin
      aNode.StateIndex:=aUsers.Rights.Right(aNode.Text,False,False);
      aNode := aNode.GetNext;
    end;
end;
procedure TfUserOptions.bNewGroupClick(Sender: TObject);
var
  Node1: TTreeNode;
begin
  Node1 := tvUsers.Items.AddChildObject(nil,'New Group',TUserTreeEntry.Create);
  tvRights.Selected := Node1;
  Node1.ImageIndex:=18;
  Node1.SelectedIndex:=18;
  aUsers.DataSet.Append;
  aUsers.FieldByName('ACCOUNTNO').AsString := Data.Numbers.GetNewNumber('USERS');
  aUsers.FieldByName('TYPE').AsString := 'G';
  aUsers.FieldByName('NAME').AsString := 'New Group';
  aUsers.DataSet.Post;
  TUserTreeEntry(Node1.Data).Rec := aUsers.GetBookmark;
  TUserTreeEntry(Node1.Data).DataSource := UsersDS;
  Node1.HasChildren:=True;
  UpdateRights;
end;
procedure TfUserOptions.bNewUserClick(Sender: TObject);
var
  Node1: TTreeNode;
begin
  Node1 := tvUsers.Items.AddChildObject(nil,'New User',TUserTreeEntry.Create);
  Node1.ImageIndex:=21;
  Node1.SelectedIndex:=21;
  aUsers.DataSet.Append;
  aUsers.FieldByName('EMPLOYMENT').AsDateTime := Date;
  aUsers.FieldByName('ACCOUNTNO').AsString := Data.Numbers.GetNewNumber('USERS');
  aUsers.FieldByName('NAME').AsString := 'New User';
  aUsers.DataSet.Post;
  TUserTreeEntry(Node1.Data).Rec := aUsers.GetBookmark;
  TUserTreeEntry(Node1.Data).DataSource := UsersDS;
  try
    tvRights.Selected := Node1;
  except
  end;
  UpdateRights;
end;
procedure TfUserOptions.bResetPasswordClick(Sender: TObject);
begin
  aUsers.DataSet.Edit;
  aUsers.FieldByName('PASSWORD').AsString := '';
  aUsers.DataSet.Post;
end;
procedure TfUserOptions.bSaveUserClick(Sender: TObject);
begin
  if (aUsers.State = dsInsert) or (aUsers.State = dsEdit) then
    aUsers.dataSet.Post;
end;
procedure TfUserOptions.eUsernameChange(Sender: TObject);
begin
  if Assigned(tvUsers.Selected) and ((aUsers.State = dsEdit) or (aUsers.State = dsInsert)) then
    tvUsers.Selected.Text:=eUsername.Text;
end;
procedure TfUserOptions.miRightsClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := tvRights.Items[0];
  while Assigned(aNode) do
    begin
      if (aNode.MultiSelected) or (aNode.Selected) then
        begin
          Data.SetFilter(aUsers.Rights,'"RIGHTNAME"='+Data.QuoteValue(Uppercase(aNode.Text)),0,'','ASC',False,False);
          if not aUsers.Rights.DataSet.Locate('RIGHTNAME',VarArrayOf([Uppercase(aNode.Text)]),[loCaseInsensitive]) then
            begin
              aUsers.Rights.DataSet.Append;
              if aUsers.Rights.DataSet.FieldDefs.IndexOf('USER') > -1 then
                aUsers.Rights.FieldByName('USER').AsString := aUsers.FieldByName('ACCOUNTNO').AsString;
              aUsers.Rights.FieldByName('RIGHTNAME').AsString := Uppercase(aNode.Text);
            end
          else
            aUsers.Rights.DataSet.Edit;
          aUsers.Rights.FieldByName('RIGHTS').AsInteger := TMenuItem(Sender).ImageIndex;
          aUsers.Rights.DataSet.Post;
          if TMenuItem(Sender).ImageIndex = -1 then
            aUsers.Rights.DataSet.Delete;
        end;
      aNode := aNode.GetNext;
    end;
  UpdateRights;
end;

procedure TfUserOptions.SpeedButton1Click(Sender: TObject);
begin
  if not Assigned(fPaygroups) then
    Application.CreateForm(TfPaygroups,fPaygroups);
  fPaygroups.Paygroups.DataSet := Paygroups.DataSet;
  fPaygroups.ShowModal;
end;

procedure TfUserOptions.tvUsersDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceNode: TTreeNode;
  DestNode: TTreeNode;
begin
  SourceNode := TTreeView(Sender).Selected;
  DestNode := tvUsers.GetNodeAt(X,Y);
  if Assigned(DestNode) then
    begin
      aUsers.GotoBookmark(TUserTreeEntry(SourceNode.Data).Rec);
      aUsers.DataSet.Edit;
      aUsers.FieldByName('PARENT').AsInteger:=TUserTreeEntry(DestNode.Data).Rec;
      aUsers.DataSet.Post;
      SourceNode.Delete;
      DestNode.Collapse(True);
    end;
end;
procedure TfUserOptions.tvUsersDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SourceNode: TTreeNode;
  DestNode: TTreeNode;
begin
  Accept := False;
  SourceNode := TTreeView(Sender).Selected;
  DestNode := tvUsers.GetNodeAt(X,Y);
  if Assigned(DestNode) and Assigned(SourceNode) then
    begin
      if ((SourceNode.ImageIndex = 21) or (SourceNode.ImageIndex = 18)) and (DestNode.ImageIndex = 18) then
        Accept := True;
    end;
end;
procedure TfUserOptions.tvUsersExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  aUser : LongInt;
  Node1: TTreeNode;
  aParent: LongInt;
begin
  Node.DeleteChildren;
  aParent := TUserTreeEntry(Node.Data).Rec;
  aUser := aUsers.GetBookmark;
  with aUsers.DataSet do
    begin
      First;
      while not EOF do
        begin
          if FieldByName('PARENT').AsInteger = aParent then
            begin
              Node1 := tvUsers.Items.AddChildObject(Node,FieldByName('NAME').AsString,TUserTreeEntry.Create);
              TUserTreeEntry(Node1.Data).Rec := aUsers.GetBookmark;
              TUserTreeEntry(Node1.Data).DataSource := aUsers.DataSet.DataSource;
              if FieldByName('TYPE').AsString = 'G' then
                begin
                  Node1.ImageIndex := 18;
                  Node1.SelectedIndex := 18;
                  Node1.HasChildren:=True;
                end
              else
                begin
                  Node1.ImageIndex := 21;
                  Node1.SelectedIndex := 21;
                end;
            end;
          Next;
        end;
    end;
  aUsers.GotoBookmark(aUser);
end;
constructor TfUserOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aUsers := TUser.CreateEx(Self,Data,aConnection);
  UsersDS.DataSet := aUsers.DataSet;
  aPaygroups := TPayGroups.CreateEx(nil,Data,aConnection);
  aPaygroups.CreateTable;
  Paygroups.DataSet := aPaygroups.DataSet;
end;
destructor TfUserOptions.Destroy;
begin
  aUsers.Destroy;
  aPaygroups.Destroy;
  try
    aConnection.Destroy;
  except
  end;
  inherited Destroy;
end;
procedure TfUserOptions.StartTransaction;
var
  Node1: TTreeNode;
begin
  inherited;
  Data.StartTransaction(aConnection);
  aUsers.Open;
  aPaygroups.Open;
  with aUsers.DataSet do
    begin
      First;
      while not EOF do
        begin
          if FieldByName('PARENT').IsNull then
            begin
              Node1 := tvUsers.Items.AddChildObject(nil,FieldByName('NAME').AsString,TUserTreeEntry.Create);
              TUserTreeEntry(Node1.Data).Rec := aUsers.GetBookmark;
              TUserTreeEntry(Node1.Data).DataSource := aUsers.DataSet.DataSource;
              if FieldByName('TYPE').AsString = 'G' then
                begin
                  Node1.ImageIndex := 18;
                  Node1.SelectedIndex := 0;
                  Node1.HasChildren:=True;
                end
              else
                begin
                  Node1.ImageIndex := 21;
                  Node1.SelectedIndex := 21;
                end;
            end;
          Next;
        end;
    end;
end;
procedure TfUserOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited;
end;
procedure TfUserOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited;
end;
end.

