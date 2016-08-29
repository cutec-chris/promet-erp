{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}
unit utimeregistration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ActnList, Buttons, uPrometFrames, uFilterFrame,
  uBaseDbClasses, uTimes, ugridview, db;

type

  { TfTimeRegistration }

  TfTimeRegistration = class(TPrometMainFrame)
    acDelete: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TSpeedButton;
    Button3: TButton;
    cbUser: TComboBox;
    deFrom: TDateEdit;
    deTo: TDateEdit;
    lTo: TLabel;
    lFrom: TLabel;
    lUser: TLabel;
    pTimeList: TPanel;
    pToolbar: TPanel;
    procedure acDeleteExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbUserSelect(Sender: TObject);
    procedure deFromAcceptDate(Sender: TObject; var ADate: TDateTime;
      var AcceptDate: Boolean);
    procedure eLinkButtonClick(Sender: TObject);
    procedure FListFilterChanged(Sender: TObject);
    procedure FTimesDataSetBeforePost(aDataSet: TDataSet);
    function SetListLinkfromSearch(aLink: string): Boolean;
  private
    { private declarations }
    FList: TfFilter;
    FUser : TUser;
    FTimes : TTimes;
    FRef : Variant;
    procedure UpdateEditors;
    procedure RemoveEditors;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  fTimeRegistration: TfTimeRegistration;

implementation

uses uData,uEnterTime,uSearch,uIntfStrConsts,uProjects,Grids,uBaseDBInterface;

{$R *.lfm}

{ TfTimeRegistration }

procedure TfTimeRegistration.cbUserSelect(Sender: TObject);
var
  aFilter: String;
begin
  FTimes.Close;
  if cbUser.Text=Data.Users.Text.AsString then
    begin
      aFilter := Data.QuoteField('REF_ID')+'='+Data.QuoteValue(Data.Users.Id.AsString);
      FRef := Data.Users.Id.AsVariant;
    end
  else if FUser.Locate('NAME',cbUser.Text,[]) then
    begin
      aFilter := Data.QuoteField('REF_ID')+'='+Data.QuoteValue(FUser.Id.AsString);
      FRef := FUser.Id.AsVariant;
    end;
  aFilter := aFilter+' AND '+Data.QuoteField('START')+'>'+Data.DateTimeToFilter(trunc(deFrom.Date))+' AND '+Data.QuoteField('START')+'<'+Data.DateTimeToFilter(trunc(deTo.Date)+1);
  FTimes.Filter(aFilter);
  FList.DataSet:=FTimes;
end;

procedure TfTimeRegistration.Button1Click(Sender: TObject);
begin
  deFrom.Date:=deFrom.Date-1;
  deTo.Date:=deTo.Date-1;
  cbUserSelect(nil);
end;

procedure TfTimeRegistration.acDeleteExecute(Sender: TObject);
begin
  if (MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes) then
    FDataSet.Delete;
end;

procedure TfTimeRegistration.Button2Click(Sender: TObject);
begin
  deFrom.Date:=deFrom.Date+1;
  deTo.Date:=deTo.Date+1;
  cbUserSelect(nil);
end;

procedure TfTimeRegistration.deFromAcceptDate(Sender: TObject;
  var ADate: TDateTime; var AcceptDate: Boolean);
begin
  cbUserSelect(nil);
end;

procedure TfTimeRegistration.eLinkButtonClick(Sender: TObject);
var
  aLink: String;
  aProject: String;
begin
  fSearch.SetLanguage;
  if (FList.gList.SelectedColumn.FieldName = 'LINK') then
    begin
      fSearch.OnOpenItem:=@SetListLinkfromSearch;
      if fSearch.Execute(True,'TIMELINK',strSearchfromTimeregisteringMode) then
        FList.gList.SelectedColumn.Field.AsString := fSearch.GetLink;
      fList.gList.SetFocus;
    end
  else if FList.gList.SelectedColumn.FieldName = 'PROJECT' then
    begin
      fSearch.OnOpenItem:=@SetListLinkfromSearch;
      fSearch.AllowSearchTypes(strProjects);
      if fSearch.Execute(True,'TIMEPROJ',strSearchfromTimeregisteringMode) then
        FList.gList.SelectedColumn.Field.AsString := fSearch.GetLink;
      fList.gList.SetFocus;
    end;
end;

procedure TfTimeRegistration.FListFilterChanged(Sender: TObject);
begin
  RemoveEditors;
  UpdateEditors;
end;

procedure TfTimeRegistration.FTimesDataSetBeforePost(aDataSet: TDataSet);
begin
  aDataSet.FieldByName('REF_ID').AsVariant:=FRef;
end;

function TfTimeRegistration.SetListLinkfromSearch(aLink: string): Boolean;
var
  aProject: TProject;
begin
  FList.gList.EditorMode:=False;
  FList.gList.SelectedColumn.Field.Text:=aLink;
  if FList.gList.SelectedColumn.FieldName='PROJECT' then
    begin
      aProject := TProject.Create(nil);
      aProject.SelectFromLink(aLink);
      aProject.Open;
      if aProject.Count>0 then
        FList.gList.DataSource.DataSet.FieldByName('PROJECTID').AsVariant:=aProject.Id.AsVariant;
      aProject.Free;
    end;
  FList.gList.DataSource.DataSet.FieldByName('TASKID').Clear;
  FList.gList.EditorMode:=True;
  Result := True;
end;

procedure TfTimeRegistration.UpdateEditors;
var
  i: Integer;
begin
  with FList do
    begin
      for i := 0 to gList.Columns.Count-1 do
        begin
          if (gList.Columns[i].FieldName = 'LINK')
          or (gList.Columns[i].FieldName = 'PROJECT') then
            gList.Columns[i].ButtonStyle:=cbsEllipsis;
          if gList.Columns[i].FieldName = 'NOTE' then
            gList.Columns[i].ReadOnly:=False;
        end;
      gList.OnEditButtonClick:=@eLinkButtonClick;
    end;
end;

procedure TfTimeRegistration.RemoveEditors;
var
  i: Integer;
begin
  with FList do
    begin
      for i := 0 to gList.Columns.Count-1 do
        begin
          if gList.Columns[i].ButtonStyle = cbsEllipsis then
            gList.Columns[i].ButtonStyle:=cbsAuto;
          gList.Columns[i].PickList.Clear;
          gList.Columns[i].ReadOnly:=False;
        end;
    end;
end;

constructor TfTimeRegistration.Create(AOwner: TComponent);
var
  aRight: Integer;
begin
  inherited Create(AOwner);
  FList := TfFilter.Create(Self);
  FList.Parent := pTimeList;
  FList.Align:=alClient;
  FList.FilterType:='TMREGISTR';
  FList.DefaultRows:='GLOBALWIDTH:%;START:60;PROJECT:100;JOB:100;DURATION:60;';
  FList.Editable:=True;
  FList.OnGetCellText:=@fEnterTime.FListGetCellText;
  FList.OnFilterChanged:=@FListFilterChanged;
  FList.pTop.Visible:=False;
  cbUser.Clear;
  cbUser.Items.Add(Data.Users.Text.AsString);

  aRight := Data.Users.Rights.Right('TIMEREG',True);
  if aRight > RIGHT_READ then
    begin
      FUser := TUser.Create(nil);
      FUser.Filter(Data.QuoteField('LOGINACTIVE')+'<>'+Data.QuoteValue('Y'));
      FUser.Open;
      FUser.First;
      while not FUser.EOF do
        begin
          cbUser.Items.Add(FUser.Text.AsString);
          FUser.Next;
        end;
    end;

  deFrom.Date:=now();
  deTo.Date:=now();

  FTimes := TTimes.Create(nil);
  FTimes.DataSet.BeforePost:=@FTimesDataSetBeforePost;
  cbUser.Text:=Data.Users.Text.AsString;
  cbUserSelect(cbUser);
end;

destructor TfTimeRegistration.Destroy;
begin
  FUser.Free;
  FTimes.Free;
  FList.DataSet:=nil;
  FList.Free;
  inherited Destroy;
end;

end.

