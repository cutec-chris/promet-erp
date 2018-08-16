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
Created 01.07.2012
*******************************************************************************}
unit uhelpviewer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uBaseApplication, ubaseDbInterface, uWikiFrame;
type
  TfMain = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    fWikiFrame: TfWikiFrame;
    function OpenWikiLink(aLink : string;Sender : TObject) : Boolean;
  public
    { public declarations }
    procedure DoCreate;
  end; 
var
  fMain: TfMain;
implementation
{$R *.lfm}
uses uData, uWiki, Utils;
resourcestring
  strNoHelpDatabaseFound = 'es wurden keine Hilfedaten (help.db) gefunden !';

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fWikiFrame.Destroy;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  if Assigned(fWikiFrame) then exit;
  with Application as IBaseDBInterface do
    begin
      if (not FileExists('help.db')) or (not OpenMandant('SQL','sqlite-3;localhost;help.db;;x')) then
        if (not FileExists(AppendPathDelim(Application.Location)+'help.db')) or (not OpenMandant('SQL','sqlite-3;localhost;'+AppendPathDelim(Application.Location)+'help.db;;x')) then
          begin
            Showmessage(strNoHelpDatabaseFound+lineending+LastError);
            Application.Terminate;
            exit;
          end;
      uData.DataM := Data;
      Data.Users.DataSet.Open;
    end;
  Data.RegisterLinkHandler('WIKI@',@OpenWikiLink,TWikiList);
  fWikiFrame := TfWikiFrame.Create(Self);
  fWikiFrame.Parent := Self;
  fWikiFrame.Align := alClient;
  fWikiFrame.Show;
  fWikiFrame.SetRights(Application.HasOption('edit'));
  if not Application.HasOption('s','startpage') then
    fWikiFrame.OpenWikiPage('Admin-Book/index',True)
  else fWikiFrame.OpenWikiPage(Application.GetOptionValue('s','startpage'),True);
end;
function TfMain.OpenWikiLink(aLink: string; Sender: TObject): Boolean;
var
  bLink: String;
begin
  bLink := copy(aLink,6,length(aLink));
  if rpos('{',bLink) > 0 then
    bLink := copy(bLink,0,rpos('{',bLink)-1)
  else if rpos('(',bLink) > 0 then
    bLink := copy(bLink,0,rpos('(',bLink)-1);
  Result := fWikiFrame.OpenWikiPage(bLink);
end;
procedure TfMain.DoCreate;
var
  IA : IBaseApplication;
begin
  if Supports(Application,IBaseApplication,IA) then
    IA.SetConfigName('HelpViewer');
  with Application as IBaseDbInterface do
    LoadMandants;
end;

end.

