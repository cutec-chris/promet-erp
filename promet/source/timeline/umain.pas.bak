unit umain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, ZVDateTimePicker,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage,ugridview,uHistoryFrame,
  uExtControls;
type

  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    ActionList1: TActionList;
    ActiveUsers: TDatasource;
    Commands: TDatasource;
    MainMenu: TMainMenu;
    mEntry: TMemo;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miNewMandant: TMenuItem;
    miOptions: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    function FContListDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    SysCommands : TSystemCommands;
    FTimeLine : TfGridView;
    aHistoryFrame: TfHistoryFrame;
  public
    { public declarations }
    procedure DoCreate;
  end;
var
  fMain: TfMain;
implementation
uses uBaseApplication, uData, uBaseDbInterface, uOrder,uMessages,uBaseERPDBClasses;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Timeline');
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
  FTimeLine := TfGridView.Create(Self);
  FTimeLine.Parent := Panel2;
  FTimeLine.Align:=alClient;
  FTimeLine.Show;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  FTimeLine.DataSet := TBaseHistory.Create(Self,Data);
  FTimeLine.DataSet.Open;
  aHistoryFrame := TfHistoryFrame.Create(Self);
  with FTimeLine do
    begin
      BaseName:='PTLINE';
      DefaultRows:='GLOBALWIDTH:530;ACTIONICON:30;ACTION:200;REFERENCE:100;COMMISSION:100;TIMESTAMPD:100;';
      Align := alClient;
      SortDirection:=sdAscending;
      SortField:='TIMESTAMPD';
      TextField:='ACTION';
      ReadOnly:=True;
      FilterRow:=True;
      Show;
      OnDrawColumnCell:=@FContListDrawColumnCell;
    end;
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  Data.ActiveUsers.DataSet.AfterScroll:=nil;
  FreeAndnil(SysCommands);
  with Application as IBaseApplication do
    Logout;
end;
function TfMain.FContListDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState): Boolean;
begin
  with (Sender as TCustomGrid), Canvas do
    begin
      Result := True;
      Canvas.FillRect(Rect);
      if gdSelected in State then
        Canvas.Font.Color:=clHighlightText
      else
        Canvas.Font.Color:=clWindowText;
      if (Column.FieldName = 'ACTIONICON') then
        begin
          if not (TExtStringGrid(Sender).Cells[Column.Index+1,DataCol] = '') then
            aHistoryFrame.HistoryImages.Draw(Canvas,Rect.Left,Rect.Top,StrToIntDef(TExtStringGrid(Sender).Cells[Column.Index+1,DataCol],-1));
        end
      else
        Result := False;
    end;
end;
procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTimeLine.DataSet.Free;
  FTimeLine.free;
  aHistoryFrame.Free;
  with Application as IBaseApplication do
    begin
      Data.ActiveUsers.DataSet.AfterScroll:=nil;
      FreeAndnil(SysCommands);
      SaveConfig;
      DoExit;
    end;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
  if Assigned(Data) then
    ActiveUsers.DataSet := Data.ActiveUsers.DataSet;
end;

initialization
  {$I umain.lrs}

end.