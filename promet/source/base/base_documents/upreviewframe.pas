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
Created 01.06.2006
*******************************************************************************}
unit uPreviewFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, Buttons,
  ActnList, StdCtrls, LR_View, LR_Class, uPrometFrames, uDocuments, db, uEditor,
  uExtControls, Graphics, types,uBaseDbClasses;
type

  { TfPreview }
  TLoadThread = class;
  TfPreview = class(TPrometMainFrame)
      acOCR: TAction;
      acFindSubject: TAction;
      acFindDate: TAction;
      ActionList1: TActionList;
      Bevel1: TBevel;
      bZoomIn: TSpeedButton;
      bZoomOut: TSpeedButton;
      cbRevision: TComboBox;
      ExtRotatedLabel8: TExtRotatedLabel;
      frPreview: TfrPreview;
      frReport: TfrReport;
      iHourglass: TImage;
      Label1: TLabel;
      mText: TMemo;
      PaintBox1: TPaintBox;
      Panel2: TPanel;
      pcPages: TPageControl;
      pfrPreview: TPanel;
      pLeft: TPanel;
      pToolbar: TToolBar;
      pImageControls: TPanel;
      sbImage: TPanel;
      ScrollBar1: TScrollBar;
      ScrollBar2: TScrollBar;
      ScaleTimer: TTimer;
      SpeedButton1: TSpeedButton;
      SpeedButton2: TSpeedButton;
      SpeedButton3: TSpeedButton;
      ToolBar2: TToolBar;
      tsImage: TTabSheet;
      tsText: TTabSheet;
      procedure acFindDateExecute(Sender: TObject);
      procedure acFindSubjectExecute(Sender: TObject);
      procedure acOCRExecute(Sender: TObject);
      procedure bZoomInClick(Sender: TObject);
      procedure bZoomOutClick(Sender: TObject);
      procedure cbRevisionSelect(Sender: TObject);
      procedure frPreviewMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure frPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer);
      procedure frPreviewMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure iPreviewMouseWheelDown(Sender: TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
      procedure iPreviewMouseWheelUp(Sender: TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
      procedure PaintBox1Paint(Sender: TObject);
      procedure ScaleTimerTimer(Sender: TObject);
      procedure ScrollBar2Change(Sender: TObject);
      procedure tsImageShow(Sender: TObject);
    private
      FResetZoom: Boolean;
      FZoomW: Boolean;
      { private declarations }
      StartX,
      StartY,
      MoveX,
      MoveY: Integer;
      IsMoved: Boolean;
      FScale : real;
      aLoading : Boolean;
      FEditor : TfEditor;
      FID : LargeInt;
      FImage : TBitmap;
      FScaledImage : TBitmap;
      FScaled : Boolean;
      aThread: TLoadThread;
      procedure DoScalePreview;
    public
      { public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function CanHandleType(aExtension : string) : Boolean;
      function LoadFromDocuments(aID : LargeInt) : Boolean;
      function LoadFromStream(aStream : TStream;aExtension : string) : Boolean;
      procedure Clear;
      function ExtractText(aStream : TStream;aExtension : string) : Boolean;
      procedure AddToolbarAction(aAction : TAction);
      property ResetZoom : Boolean read FResetZoom write FResetZoom;
      property ZoomWidth : Boolean read FZoomW write FZoomW;
    end;

  { TLoadThread }

  TLoadThread = class(TThread)
  private
    FAbort: Boolean;
    FID : LargeInt;
    FRev : Integer;
    aStream: TMemoryStream;
    aSStream: TStringStream;
    FFrame : TfPreview;
    aDocument: TDocument;
    aTransaction: TComponent;
    procedure SetAbort(AValue: Boolean);
    procedure StartLoading;
    procedure LoadFromStream;
    procedure LoadText;
    procedure FillRevision;
    procedure EndLoading;
    procedure getConnection;
  public
    procedure Execute;override;
    constructor Create(aFrame: TfPreview; aID: Int64;aRevision : Integer = -1);
    property DoAbort : Boolean read FAbort write SetAbort;
  end;
implementation
uses uData, UTF8Process, Process,uOCR,Utils,Dialogs,uBaseApplication;
procedure TLoadThread.StartLoading;
begin
  FFrame.iHourglass.Visible:=True;
  Application.ProcessMessages;
end;

procedure TLoadThread.SetAbort(AValue: Boolean);
begin
  if FAbort=AValue then Exit;
  FAbort:=AValue;
end;

procedure TLoadThread.LoadFromStream;
begin
  FFrame.pcPages.ShowTabs:=False;
  FFrame.tsText.TabVisible := False;
  FFrame.LoadFromStream(aStream,aDocument.FieldByName('EXTENSION').AsString);
end;
procedure TLoadThread.LoadText;
begin
  if aSStream.Size > 0 then
    begin
      FFrame.pcPages.ShowTabs:=True;
      FFrame.tsText.TabVisible := True;
      FFrame.pcPages.ActivePage := FFrame.tsImage;
      aSStream.Position:=0;
      FFrame.mText.Lines.LoadFromStream(aSStream);
    end
  else
    begin
      if FFrame.ExtractText(aStream,aDocument.FieldByName('EXTENSION').AsString) then
        begin
          with aDocument.DataSet do
            begin
              aSStream.Free;
              aSStream := TStringStream.Create('');
              FFrame.mText.Lines.SaveToStream(aSStream);
              aSStream.Position:=0;
              Data.StreamToBlobField(aSStream,aDocument.DataSet,'FULLTEXT');
              if aDocument.CanEdit then
                aDocument.DataSet.Post;
            end;
        end;
    end;
end;
procedure TLoadThread.FillRevision;
begin
  if FFrame.cbRevision.Items.Count = 0 then
    begin
      with aDocument.DataSet do
        begin
          aDocument.DataSet.First;
          repeat
            begin
              FFrame.cbRevision.Items.Insert(0,Format('%d %s',[FieldByName('REVISION').AsInteger,FieldByName('TIMESTAMPD').AsString]));
              aDocument.DataSet.Next;
            end
          until aDocument.DataSet.EOF;
          if FFRame.cbRevision.Items.Count>2 then
            FFrame.cbRevision.Items.Delete(0);
          FFrame.cbRevision.ItemIndex:=0;
        end;
    end;
end;

procedure TLoadThread.EndLoading;
begin
  FFrame.iHourglass.Visible:=False;
  FFrame.iHourglass.Width:=FFrame.iHourglass.Width-1;
  FFrame.iHourglass.Width:=FFrame.iHourglass.Width+1;
  Application.ProcessMessages;
end;

procedure TLoadThread.getConnection;
begin
  aTransaction := Data.GetNewConnection;
end;

procedure TLoadThread.Execute;
var
  aNumber: Integer;
label aExit;
begin
  Synchronize(@getConnection);
  aDocument := TDocument.CreateEx(nil,Data,aTransaction);
  aDocument.SelectByID(FID);
  aDocument.Open;
  if aDocument.Count > 0 then
    begin
      Synchronize(@StartLoading);
      aNumber := aDocument.FieldByName('NUMBER').AsInteger;
      aDocument.SelectByNumber(aNumber);
      if DoAbort then goto aExit;
      aDocument.Open;
      if DoAbort then goto aExit;
      Synchronize(@FillRevision);
      if DoAbort then goto aExit;
      if aDocument.Size>(15*1024*1024) then goto aExit; //to big for preview
      aStream := TMemoryStream.Create;
      aDocument.CheckoutToStream(aStream,FRev);
      if not DoAbort then
        begin
          Synchronize(@LoadFromStream);
          aSStream := TStringStream.Create('');
          if not DoAbort then
            begin
              Data.BlobFieldToStream(aDocument.DataSet,'FULLTEXT',aSStream);
              Synchronize(@LoadText);
            end;
        end;
      aSStream.Free;
      aStream.Free;
    end;
aExit:
  aDocument.Free;
  aTransaction.Free;
  //if FFrame.aThread = Self then
    begin
      FFrame.aLoading := False;
      FFrame.aThread:=nil;
      Synchronize(@EndLoading);
    end;
end;
constructor TLoadThread.Create(aFrame : TfPreview;aID: Int64;aRevision : Integer = -1);
begin
  FAbort:=False;
  FID := aID;
  FRev := aRevision;
  FFrame := aFrame;
  FreeOnTerminate:=True;
  if not BaseApplication.HasOption('disablethreads') then
    inherited Create(False)
  else
    Execute;
end;

{$R *.lfm}
procedure TfPreview.bZoomInClick(Sender: TObject);
begin
  if sbImage.Visible then
    begin
      FScale := FScale*1.1;
      DoScalepreview;
    end
  else if pfrPreview.Visible then
    begin
      frPreview.Zoom:=frPreview.Zoom*1.1;
    end;
end;

procedure TfPreview.acOCRExecute(Sender: TObject);
var
  aDoc: TDocument;
  Texts: TOCRPages;
  aText: TStringList;
  i: Integer;
begin
  aDoc := TDocument.Create(nil);
  aDoc.SelectByID(FID);
  aDoc.Open;
  if aDoc.Count>0 then
    begin
      Texts := DoOCR(aDoc);
      aText := TStringList.Create;
      for i := 0 to Texts.Count-1 do
        begin
          FixText(TStringList(Texts[i]));
          atext.AddStrings(TStringList(Texts[i]));
        end;
      aDoc.Edit;
      aDoc.FieldByName('FULLTEXT').AsString:=aText.Text;
      aDoc.Post;
      aText.Free;
      for i := 0 to Texts.Count-1 do
        TStringList(Texts[i]).Free;
      Texts.Free;
    end;
  aDoc.Free;
end;

procedure TfPreview.acFindSubjectExecute(Sender: TObject);
var
  aStart: Integer;
  aLen: Integer;
  aText: String;
begin
  aText := uOCR.GetTitleEx(mText.Lines,0,aStart,aLen);
  if aText <> '' then
    begin
      mtext.SelStart:=aStart;
      mText.SelLength:=aLen;
    end;
end;

procedure TfPreview.acFindDateExecute(Sender: TObject);
var
  aDate: TDateTime;
  aStart: Integer;
  aLen: Integer;
begin
  aDate := uOCR.GetDateEx(mText.Lines,aStart,aLen);
  if aDate > 0 then
    begin
      mtext.SelStart:=aStart;
      mText.SelLength:=aLen;
    end;
end;

procedure TfPreview.bZoomOutClick(Sender: TObject);
begin
  if sbImage.Visible then
    begin
      FScale := FScale/1.1;
      DoScalepreview;
    end
  else if pfrPreview.Visible then
    begin
      frPreview.Zoom:=frPreview.Zoom/1.1;
    end;
end;

procedure TfPreview.cbRevisionSelect(Sender: TObject);
begin
  if aLoading then exit;
  aThread := TLoadThread.Create(Self,FID,StrToIntDef(copy(cbRevision.Text,0,pos(' ',cbRevision.Text)-1),0));
  Clear;
end;
procedure TfPreview.frPreviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      IsMoved := True;
      StartX  := X;
      StartY  := Y;
      MoveX   := X;
      MoveY   := Y;
    end;
end;
procedure TfPreview.frPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    begin
      IsMoved:=False;
      exit;
    end;
  if isMoved then
    begin
      ScrollBar1.Position:=ScrollBar1.Position-round((X - MoveX));
      ScrollBar2.Position:=ScrollBar2.Position-round((Y - MoveY));
      MoveX := X;
      MoveY := Y;
    end;
end;
procedure TfPreview.frPreviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      IsMoved := False;
    end;
end;
procedure TfPreview.iPreviewMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := False;
  if ssCtrl in Shift then
    begin
      FScale := FScale/1.1;
      DoScalepreview;
      Handled := True;
    end
  else
    ScrollBar2.Position:=ScrollBar2.Position+15;
end;
procedure TfPreview.iPreviewMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := False;
  if ssCtrl in Shift then
    begin
      FScale := FScale*1.1;
      DoScalepreview;
      Handled := True;
    end
  else
    ScrollBar2.Position:=ScrollBar2.Position-15;
end;

procedure TfPreview.PaintBox1Paint(Sender: TObject);
var
  RectDest, RectSource: TRect;
begin
  RectDest:=Rect(0, 0, PaintBox1.Width, PaintBox1.Height);
  if FScaled then
    begin
      RectSource:=Rect(
        ScrollBar1.Position,
        ScrollBar2.Position,
        Scrollbar1.Position+round(PaintBox1.Width),
        ScrollBar2.Position+round(PaintBox1.Height));
      PaintBox1.Canvas.CopyRect(RectDest, FScaledImage.Canvas, RectSource);
    end
  else
    begin
      RectSource:=Rect(
        ScrollBar1.Position,
        ScrollBar2.Position,
        Scrollbar1.Position+round(PaintBox1.Width/FScale),
        ScrollBar2.Position+round(PaintBox1.Height/FScale));
      PaintBox1.Canvas.CopyRect(RectDest, FImage.Canvas, RectSource);
    end;
end;

procedure TfPreview.ScaleTimerTimer(Sender: TObject);
begin
  ScaleTimer.Enabled:=False;
  FScaledImage.Width:=round(FImage.Width*FScale);
  FScaledImage.Height:=round(FImage.Height*FScale);
  FScaledImage.Canvas.StretchDraw(Rect(0,0,FScaledImage.Width,FScaledImage.Height),FImage);
  FScaled:=True;
  iHourglass.Visible:=False;
end;

procedure TfPreview.ScrollBar2Change(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TfPreview.tsImageShow(Sender: TObject);
begin
  pfrPreview.Visible:=not pfrpreview.Visible;
  pfrPreview.Visible:=not pfrpreview.Visible;
end;

procedure TfPreview.DoScalePreview;
var
  amax: Integer;
begin
  iHourglass.Visible:=True;
  FScaled := False;
  ScaleTimer.Enabled:=True;
  amax := abs(round(FImage.Width-1-PaintBox1.Width*FScale));
  if aMax >0 then
    ScrollBar1.Max:=aMax;
  amax := abs(round(FImage.Height-1-PaintBox1.Height*FScale));
  if aMax > 0 then
    ScrollBar2.Max:=aMax;
  PaintBox1.Invalidate;
end;

constructor TfPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TfEditor.Create(Self);
  FResetZoom:=True;
  FScale:=1;
  FImage := TBitmap.Create;
  FScaledImage := TBitmap.Create;
  Clear;
end;

destructor TfPreview.Destroy;
begin
  FScaledImage.Free;
  FImage.Free;
  FEditor.Free;
  inherited Destroy;
end;

function TfPreview.CanHandleType(aExtension: string): Boolean;
begin
  Result := False;
  if (aExtension = 'JPG')
  or (aExtension = 'JPEG')
  or (aExtension = 'PNG')
  or (aExtension = 'BMP')
  or (aExtension = 'GIF')
  or (aExtension = 'TGA')
  then
    Result := True
  else if (aExtension = 'PDF') then
    Result := True
  else if (aExtension = 'EMFP') then
    Result := True
  else if FEditor.CanHandleFile('.'+aExtension) then
    Result := True
  else if (aExtension = 'AVI')
    or (aExtension = 'FLV')
    or (aExtension = 'MPG')
    or (aExtension = 'MP4')
    or (aExtension = 'WEBM')
    or (aExtension = '3GP')
    or (aExtension = 'MOV')
    or (aExtension = 'WMV')
    or (aExtension = 'RM')
    then
      Result := True
  ;
end;
function TfPreview.LoadFromDocuments(aID: LargeInt): Boolean;
var
  aDocument: TDocument;
begin
  Result := True;
  cbRevision.Clear;
  if aLoading and Assigned(aThread) then
    begin
      aThread.DoAbort:=True;
      aThread := nil;
    end;
  aLoading := True;
  FID := aID;
  aThread := TLoadThread.Create(Self,aID);
end;
function TfPreview.LoadFromStream(aStream: TStream; aExtension: string) : Boolean;
var
  aFStream: TFileStream;
  aFilename: String;
  aProcess: TProcessUTF8;
  aPic: TPicture;
begin
  Result := false;
  if aLoading and Assigned(aThread) then
    begin
      aThread.DoAbort:=True;
      aThread := nil;
    end;
  aExtension := Uppercase(aExtension);
  if (aExtension = 'JPG')
  or (aExtension = 'JPEG')
  or (aExtension = 'PNG')
  or (aExtension = 'BMP')
  or (aExtension = 'GIF')
  or (aExtension = 'TGA')
  then
    begin
      aStream.Position:=0;
      try
        aPic := TPicture.Create;
        aPic.LoadFromStreamWithFileExt(aStream,aExtension);
        if FResetZoom then
          begin
            if (APic.Width > aPic.Height) or FZoomW then
              FScale := Width/aPic.Width
            else
              FScale := Height/aPic.Height;
          end;
        FImage.Assign(aPic.Bitmap);
        aPic.Free;
        sbImage.Visible:=True;
        pImageControls.Visible:=True;
        FEditor.Hide;
        pfrPreview.Visible:=False;
        Result := True;
        ScrollBar1.Position:=0;
        ScrollBar2.Position:=0;
      except
        sbImage.Visible:=False;
      end;
    end
  else if (Uppercase(aExtension) = 'EMFP') then
    begin
      try
        aStream.Position:=0;
        frReport.EMFPages.LoadFromStream(aStream);
        frReport.ShowPreparedReport;
        pImageControls.Visible:=False;
        sbImage.Visible:=false;
        FEditor.Hide;
        pfrPreview.Visible:=True;
//        spPreview.Visible:=True;
        frPreview.PageWidth;
        Result := True;
      except
      end;
    end
  else if (Uppercase(aExtension) = 'PDF') then
    begin
      try
        with BaseApplication as IBaseApplication do
          begin
            aFilename := GetInternalTempDir+'rpv.'+aExtension;
            aFStream := TFileStream.Create(GetInternalTempDir+'rpv.'+aExtension,fmCreate);
          end;
        aStream.Position:=0;
        aFStream.CopyFrom(aStream,aStream.Size);
        aFStream.Free;
        aProcess := TProcessUTF8.Create(Self);
        {$IFDEF WINDOWS}
        aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
        {$ELSE}
        aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
        {$ENDIF}
        aProcess.ShowWindow := swoHide;
        aProcess.CommandLine := Format('pdftopng'+ExtractFileExt(Application.ExeName)+' -l 1 %s %s',[aFileName,aFilename]);
        aProcess.CurrentDirectory := AppendPathDelim(AppendPathDelim(Application.Location)+'tools');
        {$IFDEF WINDOWS}
        aProcess.CommandLine := aProcess.CurrentDirectory+aProcess.CommandLine;
        {$ENDIF}
        aProcess.Execute;
        aProcess.Free;
        SysUtils.DeleteFile(aFileName);
        aPic := TPicture.Create;
        aPic.LoadFromFile(aFileName+'-000001.png');
        if FResetZoom then
          begin
            if (APic.Width > aPic.Height) or FZoomW then
              FScale := Width/aPic.Width
            else
              FScale := Height/aPic.Height;
          end;
        FImage.Assign(aPic.Bitmap);
        aPic.Free;
        pImageControls.Visible:=True;
        SysUtils.DeleteFile(aFileName+'-000001.png');
        Result := True;
        sbImage.Visible:=True;
        FEditor.Hide;
        pfrPreview.Visible:=False;
      except
        on e : Exception do
          begin
            SysUtils.DeleteFile(aFileName+'-000001.png');
          end;
      end;
      if not Result then
        begin
          try
            aProcess := TProcessUTF8.Create(Self);
            {$IFDEF WINDOWS}
            aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
            {$ELSE}
            aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
            {$ENDIF}
            aProcess.ShowWindow := swoHide;
            aProcess.CommandLine := Format({$IFDEF WINDOWS}'gswin32'+{$ELSE}'gs'+{$ENDIF}' -q -dBATCH -dMaxBitmap=300000000 -r100 -sPAPERSIZE=a4 -dNOPAUSE -dSAFER -sDEVICE=bmp16m -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -dFirstPage=1 -dLastPage=1 -sOutputFile=%s %s -c quit',[aFileName+'.bmp',aFileName]);
            aProcess.CurrentDirectory := AppendPathDelim(AppendPathDelim(Application.Location)+'tools');
            {$IFDEF WINDOWS}
            aProcess.CommandLine := aProcess.CurrentDirectory+aProcess.CommandLine;
            {$ENDIF}
            aProcess.Execute;
            aProcess.Free;
            SysUtils.DeleteFile(aFileName);
            aPic := TPicture.Create;
            aPic.LoadFromFile(aFilename+'.bmp');
            if FResetZoom then
              begin
                if (APic.Width > aPic.Height) or FZoomW then
                  FScale := Width/aPic.Width
                else
                  FScale := Height/aPic.Height;
              end;
            FImage.Assign(aPic.Bitmap);
            aPic.Free;
            pImageControls.Visible:=True;
            SysUtils.DeleteFile(aFileName+'.bmp');
            Result := True;
            sbImage.Visible:=True;
            FEditor.Hide;
            pfrPreview.Visible:=False;
          except
            on e : Exception do
              begin
                SysUtils.DeleteFile(aFileName);
                SysUtils.DeleteFile(aFileName+'.bmp');
              end;
          end;
        end;
    end
  else if FEditor.CanHandleFile('.'+aExtension) then
    begin
      sbImage.Visible:=false;
      aStream.Position:=0;
      pfrPreview.Visible:=false;
      if FEditor.LoadFromStream(aStream,'.'+aExtension) then
        begin
          FEditor.Align:=alClient;
          FEditor.BorderStyle:=bsNone;
          FEditor.Parent := tsImage;
          FEditor.Show;
        end
      else
        FEditor.Hide;
      tsImage.TabVisible:=True;
      tsText.TabVisible:=False;
    end
  else //Try to Use Imagemagick to determinate the file typ and render it
    begin
      try
        with BaseApplication as IBaseApplication do
          begin
            aFilename := GetInternalTempDir+'rpv.'+aExtension;
            aFStream := TFileStream.Create(GetInternalTempDir+'rpv.'+aExtension,fmCreate);
          end;
        aStream.Position:=0;
        aFStream.CopyFrom(aStream,aStream.Size);
        aFStream.Free;
        aProcess := TProcessUTF8.Create(Self);
        {$IFDEF WINDOWS}
        aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
        {$ELSE}
        aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
        {$ENDIF}
        aProcess.ShowWindow := swoHide;
        aProcess.CommandLine := Format('convert'+ExtractFileExt(Application.ExeName)+' %s[1] -resize %d -alpha off +antialias "%s"',[aFileName,500,afileName+'.bmp']);
        aProcess.CurrentDirectory := Application.Location+'tools';
        {$IFDEF WINDOWS}
        aProcess.CommandLine := aProcess.CurrentDirectory+aProcess.CommandLine;
        {$ENDIF}
        aProcess.Execute;
        aProcess.Free;
        SysUtils.DeleteFile(aFileName);
        aPic := TPicture.Create;
        aPic.LoadFromFile(afileName+'.bmp');
        if FResetZoom then
          begin
            if (APic.Width > aPic.Height) or FZoomW then
              FScale := Width/aPic.Width
            else
              FScale := Height/aPic.Height;
          end;
        FImage.Assign(aPic.Bitmap);
        aPic.Free;
        SysUtils.DeleteFile(aFileName+'.bmp');
        sbImage.Visible:=True;
        pImageControls.Visible:=True;
        pfrPreview.Visible:=False;
        FEditor.Hide;
        tsImage.TabVisible:=True;
        tsText.TabVisible:=False;
        Result := True;
      except
        SysUtils.DeleteFile(aFileName);
        SysUtils.DeleteFile(aFileName+'.bmp');
      end;
    end;
  DoScalePreview;
end;
procedure TfPreview.Clear;
begin
  pcPages.ActivePage:=tsImage;
  pcPages.ShowTabs:=False;
  tsText.TabVisible := False;
end;
function TfPreview.ExtractText(aStream: TStream; aExtension: string): Boolean;
var
  aFilename: String;
  aFStream: TFileStream;
  aProcess: TProcessUTF8;
begin
  Result := False;
  if (Uppercase(aExtension) = 'PDF') then
    begin
      try
        with BaseApplication as IBaseApplication do
          begin
            aFilename := GetInternalTempDir+'rpv.'+aExtension;
            aFStream := TFileStream.Create(GetInternalTempDir+'rpv.'+aExtension,fmCreate);
          end;
        aStream.Position:=0;
        aFStream.CopyFrom(aStream,aStream.Size);
        aFStream.Free;
        aProcess := TProcessUTF8.Create(Self);
        {$IFDEF WINDOWS}
        aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
        {$ELSE}
        aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
        {$ENDIF}
        aProcess.ShowWindow := swoHide;
        aProcess.CommandLine := Format('pdftotext %s %s',[aFileName,aFileName+'.txt']);
        aProcess.CurrentDirectory := Application.Location+'tools';
        aProcess.Execute;
        aProcess.Free;
        SysUtils.DeleteFile(aFileName);
        if FileExists(aFileName+'.txt') then
          begin
            mText.Lines.LoadFromFile(aFileName+'.txt');
            tsText.TabVisible := True;
            pcPages.ShowTabs:=True;
            Result := True;
          end;
        SysUtils.DeleteFile(aFileName+'.txt');
        sbImage.Visible:=True;
        FEditor.Hide;
        pfrPreview.Visible:=False;
      except
        SysUtils.DeleteFile(aFileName);
        SysUtils.DeleteFile(aFileName+'.txt');
      end;
    end;
end;
procedure TfPreview.AddToolbarAction(aAction: TAction);
var
  Toolbutton: TToolButton;
begin
  pToolbar.Width:=pToolbar.Width+30;
  Toolbutton :=TToolButton.Create(pToolBar);
  Toolbutton.Parent  := pToolBar;
  ToolButton.Action := aAction;
end;

end.

