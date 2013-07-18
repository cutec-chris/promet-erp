{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uPrometFrames;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, uBaseDbInterface, uBaseDbClasses, uExtControls,
  Dialogs, Controls, ExtCtrls,uQuickHelpFrame;
type

  { TPrometMainFrame }

  TPrometMainFrame = class(TExtControlFrame)
  private
    FLink: string;
  protected
    FDataSet: TBaseDBDataSet;
    FConnection: TComponent;
    FHelpPanel : TPanel;
    FQuickHelpFrame: TfQuickHelpFrame;
    procedure SetDataSet(const AValue: TBaseDBDataset);virtual;
    procedure DoCloseFrame(Data : PtrInt);
    procedure DoExit; override;
    procedure DoOpen;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    property Connection : TComponent read FConnection;
    property DataSet : TBaseDBDataSet read FDataSet write SetDataSet;
    procedure CloseConnection(Ask : Boolean = True);virtual;
    function OpenFromLink(aLink : string) : Boolean;virtual;
    property Link : string read FLink;
    procedure New;virtual;
    procedure SetLanguage;virtual;abstract;
    procedure CloseFrame;
    function ShowHint(var HintStr: string;var CanShow: Boolean; var HintInfo: THintInfo) : Boolean;virtual;
    function HasHelp : Boolean;
    procedure AddHelp(aWindow : TWinControl);
    property HelpView : TfQuickHelpFrame read FQuickHelpFrame write FQuickHelpFrame;
  end;
implementation
uses ComCtrls, uIntfStrConsts,LCLType,LCLIntf,uWiki,uData,uBaseApplication;
procedure TPrometMainFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  if FDataSet=AValue then exit;
  FDataSet:=AValue;
end;
procedure TPrometMainFrame.DoCloseFrame(Data: PtrInt);
begin
  try
    TExtMenuPageControl(TTabSheet(Data).PageControl).CloseFrameClick(TTabSheet(Data));
  except
  end;
end;
procedure TPrometMainFrame.DoExit;
begin
  inherited DoExit;
  if Assigned(Parent) and (Parent is TTabSheet) and (TTabSheet(Parent).Visible) and (TTabSheet(Parent).PageControl is TExtMenuPageControl) and Parent.CanFocus then
    TTabSheet(Parent).PageControl.SetFocus;
end;

procedure TPrometMainFrame.DoOpen;
begin
  if HasHelp then AddHelp(Self);
end;

constructor TPrometMainFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FQuickHelpFrame:=nil;
end;
destructor TPrometMainFrame.Destroy;
begin
  if Assigned(FConnection) then
    begin
      CloseConnection;
      FreeAndNil(FConnection);
    end;
  if Assigned(FDataSet) then
    begin
      raise Exception.Create('DataSet Assigned !'+Self.ClassName);
      FreeAndNil(FDataSet);
    end;
  inherited Destroy;
end;
procedure TPrometMainFrame.New;
begin
  if Assigned(FConnection) then
    begin
      CloseConnection;
      FConnection.Free;
    end;
  with Application as IBaseDbInterface do
    FConnection := Data.GetNewConnection;
end;
procedure TPrometMainFrame.CloseConnection(Ask : Boolean = True);
begin
  if not Assigned(FConnection) then exit;
  if Assigned(DataSet) and DataSet.Changed then
    begin
      if Ask and (MessageDlg(strItem+' '+TBaseDBList(DataSet).Text.AsString+' ('+TBaseDbList(DataSet).Number.AsString+')',strItemnotSaved,mtInformation,[mbYes,mbNo],0) = mrYes) then
        begin
          DataSet.CascadicPost;
          with Application as IBaseDbInterface do
            begin
              Data.Commit(FConnection);
            end;
        end
      else
        begin
          DataSet.CascadicCancel;
          with Application as IBaseDbInterface do
            begin
              Data.Rollback(FConnection);
            end;
        end;
    end
  else
    begin
      with Application as IBaseDbInterface do
        Data.Rollback(FConnection);
    end;
  with Application as IBaseDbInterface do
    Data.Disconnect(FConnection);
//  FreeAndNil(FConnection);
end;
function TPrometMainFrame.OpenFromLink(aLink: string): Boolean;
begin
  FLink := aLink;
end;
procedure TPrometMainFrame.CloseFrame;
begin
  if (Parent is TTabSheet) and (TTabSheet(Parent).Visible) and (TTabSheet(Parent).PageControl is TExtMenuPageControl) then
    begin
      if TTabSheet(Parent).PageControl.ActivePage <> TTabSheet(Parent) then exit;
      Application.QueueAsyncCall(@DoCloseFrame,PtrInt(Parent));
    end;
end;
function TPrometMainFrame.ShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo): Boolean;
begin
  Result := False;
end;

function TPrometMainFrame.HasHelp: Boolean;
var
  aWiki: TWikiList;
begin
  with Application as IBaseApplication do
    if not QuickHelp then
      begin
        Result := False;
        exit;
      end;
  with Application as IBaseDbInterface do
    if DBConfig.ReadString('QUICKHELP','YES') = 'NO'  then
      begin
        Result := False;
        exit;
      end;
  aWiki := TWikiList.Create(nil,Data);
  with BaseApplication as IBaseApplication do
    Result := aWiki.FindWikiPage(Appname+'-Help/workflows/'+lowercase(ClassName));
  aWiki.Free;
end;

procedure TPrometMainFrame.AddHelp(aWindow: TWinControl);
var
  aWiki: TWikiList;
begin
  if Assigned(FQuickHelpFrame) then exit;
  aWiki := TWikiList.Create(nil,Data);
  with BaseApplication as IBaseApplication do
  if aWiki.FindWikiPage(Appname+'-Help/workflows/'+lowercase(ClassName)) then
    begin
      FQuickHelpFrame := TfQuickHelpFrame.Create(nil);
      if not FQuickHelpFrame.OpenWikiPage(aWiki) then
        FreeAndNil(FQuickHelpFrame)
      else
        begin
          FQuickHelpFrame.Parent:=Self;
          FQuickHelpFrame.Align:=alTop;
          FQuickHelpFrame.BorderSpacing.Around:=8;
        end;
    end;
  aWiki.Free;
end;

end.

