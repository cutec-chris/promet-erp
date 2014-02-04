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
Created 03.12.2011
*******************************************************************************}
unit umain;
{$mode objfpc}{$H+}
interface
uses
  LResources, Forms, Controls, Buttons, Menus, ActnList, StdCtrls, uExtControls,
  ComCtrls, ExtCtrls,uTasks, uMainTreeFrame, Classes;
type
  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acNewProject: TAction;
    acProjects: TAction;
    ActionList1: TActionList;
    bFfwd: TToolButton;
    bSearch: TSpeedButton;
    eContains: TEdit;
    lbResults: TListBox;
    MainMenu: TMainMenu;
    miView: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pcPages: TExtMenuPageControl;
    pSearch: TPanel;
    tvMain: TPanel;
    Splitter2: TSplitter;
    tbMenue: TToolButton;
    ToolBar1: TToolBar;
    tsHelp: TTabSheet;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acNewProjectExecute(Sender: TObject);
    procedure acProjectsExecute(Sender: TObject);
    function fMainTreeFrameOpen(aEntry: TTreeEntry): Boolean;
    function fMainTreeFrameOpenFromLink(aLink: string; aSender: TObject
      ): Boolean;
    procedure fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure AddProjectList(Sender: TObject);
  public
    { public declarations }
    procedure DoCreate;
  end;
var
  fMain: TfMain;
implementation
uses uBaseApplication, uData, uBaseDbInterface,uWikiFrame,
  uDocuments,uProjectFrame,uFilterFrame,uIntfStrConsts,
  uProjects,uPrometFrames,uBaseDbClasses;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('ProjectManagement');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
var
  WikiFrame: TfWikiFrame;
  Node: TTreeNode;
  miNew: TMenuItem;
  aDocuments: TDocuments;
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  WikiFrame := TfWikiFrame.Create(Self);
  WikiFrame.Parent := tsHelp;
  WikiFrame.Align := alClient;
  WikiFrame.OpenWikiPage('Promet-ERP-Help/index',True);
  WikiFrame.SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
  //Add Search Node
  {$region}
  Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
  Node.Height := 34;
  TTreeEntry(Node.Data).Typ := etSearch;
  miNew := TmenuItem.Create(miView);
  miView.Add(miNew);
  miNew.Action := fMainTreeFrame.acSearch;
  {$endregion}
  aDocuments := TDocuments.Create(Self,Data);
  aDocuments.CreateTable;
  aDocuments.Destroy;
  //Projects
  uProjectFrame.AddToMainTree(acNewProject,nil);
  pcPages.AddTabClass(TfFilter,strProjectList,@AddProjectList,Data.GetLinkIcon('PROJECTS@'),True);
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  pcPages.CloseAll;
  with Application as IBaseApplication do
    Logout;
end;

procedure TfMain.acNewProjectExecute(Sender: TObject);
begin

end;

procedure TfMain.acProjectsExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = False;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TProjectList) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('PROJECTS@'),False);
      AddprojectList(aFrame);
      aFrame.Open;
    end;
end;
function TfMain.fMainTreeFrameOpen(aEntry: TTreeEntry): Boolean;
var
  aDataSet: TBaseDBDataset;
begin
  if not Assigned(aEntry) then
    exit;
  Screen.Cursor:=crHourglass;
  Application.ProcessMessages;
  case aEntry.Typ of
  etSalesList:
    begin
      aEntry.Action.Execute;
    end;
  etCustomerList,etCustomers,etArticleList,etOrderList,
  etTasks,etMyTasks,etProjects,etCalendar,etMyCalendar,
  etMessages,etMessageDir:
    begin
      pcPages.SetFocus;
    end;
  etCustomer,etEmployee,etProject:
    begin
      aDataSet := aEntry.DataSourceType.Create(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
      aDataSet.Free;
    end;
  etWikiPage:
    begin
      aDataSet := aEntry.DataSourceType.Create(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
      aDataSet.Free;
    end;
  end;
end;
function TfMain.fMainTreeFrameOpenFromLink(aLink: string; aSender: TObject
  ): Boolean;
var
  aFrame: TPrometMainFrame;
  tmp: String;
  tmp1: String;
  Found: Boolean;
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  if copy(aLink,0,8) = 'CUSTOMER' then
    begin
      {
      aFrame := TfPersonFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
      }
    end
  else if copy(aLink,0,5) = 'WIKI@' then
    begin
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfWikiFrame) then
        aFrame := TfWikiFrame(pcPages.ActivePage.Controls[0])
      else
        begin
          aFrame := TfWikiFrame.Create(Self);
          pcPages.AddTab(aFrame);
          aFrame.SetLanguage;
          //aFrame.SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
        end;
      aFrame.OpenFromLink(aLink);
      Result := True;
    end
  else if (copy(aLink,0,9) = 'PROJECTS@')
       or (copy(aLink,0,12) = 'PROJECTS.ID@') then
    begin
      aFrame := TfProjectFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
    end
  ;
  Screen.Cursor:=crDefault;
end;
procedure TfMain.fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
var
  Found: Boolean = False;
  i: Integer;
  aFrame: TPrometMainFrame;
  New: TMenuItem;
begin
  case aEntry.Typ of
  {
  etCustomerList,etCustomers:
    begin
      acContact.Execute;
    end;
  etTasks,etMyTasks:
    begin
      acTasks.Execute;
    end;
  }
  etProjects:
    begin
      acProjects.Execute;
    end;
  end;
end;
procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(tsHelp) and (tsHelp.ControlCount > 0) then
    tsHelp.Controls[0].Destroy;
  pcPages.CloseAll;
  with Application as IBaseApplication do
    begin
      SaveConfig;
      DoExit;
    end;
end;
procedure TfMain.FormCreate(Sender: TObject);
begin
  uMainTreeFrame.fMainTreeFrame := TfMainTree.Create(Self);
  fMainTreeFrame.pcPages := pcPages;
  fMainTreeFrame.Parent := tvMain;
  fMainTreeFrame.Align:=alClient;
  fMainTreeFrame.SearchOptions:='PROJECTMANAGEMENT';
//  fMainTreeFrame.OnNewFromLink:=@fMainTreeFrameNewFromLink;
  fMainTreeFrame.OnOpenFromLink:=@fMainTreeFrameOpenFromLink;
  fMainTreeFrame.OnOpen:=@fMainTreeFrameOpen;
  fMainTreeFrame.OnSelectionChanged:=@fMainTreeFrameSelectionChanged;
end;
procedure TfMain.FormDestroy(Sender: TObject);
begin
  uMainTreeFrame.fMainTreeFrame.Destroy;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
  if Assigned(Data) then
    begin
    end;
end;
procedure TfMain.AddProjectList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      Caption := strProjectList;
      FilterType:='P';
      DefaultRows:='GLOBALWIDTH:%;ID:70;NAME:100;STATUS:60;';
      Dataset := TProjectList.Create(nil,Data);
      gList.OnDrawColumnCell:=nil;
      AddToolbarAction(acNewProject);
    end;
end;

initialization
  {$I umain.lrs}
end.
