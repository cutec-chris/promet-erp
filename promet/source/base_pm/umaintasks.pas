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
unit umaintasks;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, uPrometFrames,uTasks,uBaseDbClasses,
  uIntfStrConsts,Dialogs,uBaseDBInterface;
type
  { TfMainTaskFrame }
  TfMainTaskFrame = class(TPrometMainFrame)
  private
    { private declarations }
    FTasks : TfTaskFrame;
  protected
    procedure SetConnection(AValue: TComponent); override;
  public
    { public declarations }
    procedure SetDataSet(const AValue: TBaseDBDataset); override;
    property Tasks : TfTaskFrame read FTasks;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRefresh; override;
    procedure ShowFrame; override;
  end;

implementation
{$R *.lfm}

procedure TfMainTaskFrame.SetConnection(AValue: TComponent);
begin
  inherited SetConnection(AValue);
  FTasks.Connection := Avalue;
end;

procedure TfMainTaskFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  inherited SetDataSet(AValue);
  DoOpen;
  FTasks.DataSet := AValue;
end;

{ TfMainTaskFrame }
constructor TfMainTaskFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTasks := TfTaskFrame.Create(Self);
  //fTasks.UseTransactions := True;
  FTasks.GridView.TreeField := 'LPARENT';
  FTasks.Parent := Self;
  fTasks.Align:=alClient;
  fTasks.Show;
end;
destructor TfMainTaskFrame.Destroy;
begin
  if Assigned(FConnection) then
    begin
      if Assigned(DataSet) then
        begin
          if FTasks.acSave.Enabled and (MessageDlg(strTasks,strItemnotSaved,mtInformation,[mbYes,mbNo],0) = mrYes) then
            begin
              DataSet.CascadicPost;
              with Application as IBaseDbInterface do
                begin
                  if FTasks.UseTransactions then
                    Data.CommitTransaction(FConnection);
                end;
            end
          else
            begin
              DataSet.CascadicCancel;
              with Application as IBaseDbInterface do
                begin
                  if FTasks.UseTransactions then
                    Data.RollbackTransaction(FConnection);
                end;
            end;
        end
      else
        begin
          with Application as IBaseDbInterface do
            if FTasks.UseTransactions then
              Data.RollbackTransaction(FConnection);
        end;
      with Application as IBaseDbInterface do
        Data.Disconnect(FConnection);
      FreeAndNil(FConnection);
    end;
  FTasks.Free;
  inherited Destroy;
end;
procedure TfMainTaskFrame.DoRefresh;
begin
  FTasks.DoRefresh;
end;

procedure TfMainTaskFrame.ShowFrame;
begin
  FTasks.ShowFrame;
end;

end.

