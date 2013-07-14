{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uPreviewFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, Buttons,
  ActnList, StdCtrls, LR_View, LR_Class, uPrometFrames, uDocuments, db, uEditor;
type

  { TfPreview }
  TLoadThread = class;
  TfPreview = class(TPrometMainFrame)
      bZoomIn: TSpeedButton;
      bZoomOut: TSpeedButton;
      cbRevision: TComboBox;
      frPreview: TfrPreview;
      frReport: TfrReport;
      iHourglass: TImage;
      iPreview: TImage;
      Label1: TLabel;
      mText: TMemo;
      pImageControls: TPanel;
      pfrPreview: TPanel;
      pcPages: TPageControl;
      sbImage: TScrollBox;
      ToolBar2: TToolBar;
      tsText: TTabSheet;
      tsImage: TTabSheet;
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
      procedure tsImageShow(Sender: TObject);
    private
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
    end;
  TLoadThread = class(TThread)
  private
    FAbort: Boolean;
    FID : LargeInt;
    FRev : Integer;
    aStream: TMemoryStream;
    aSStream: TStringStream;
    FFrame : TfPreview;
    aDocument: TDocument;
    procedure SetAbort(AValue: Boolean);
    procedure StartLoading;
    procedure LoadFromStream;
    procedure LoadText;
    procedure FillRevision;
    procedure EndLoading;
  public
    procedure Execute;override;
    constructor Create(aFrame: TfPreview; aID: Int64;aRevision : Integer = -1);
    property DoAbort : Boolean read FAbort write SetAbort;
  end;
implementation
uses uData, UTF8Process, Process;
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

procedure TLoadThread.Execute;
var
  aTransaction: TComponent;
  aNumber: Integer;
label aExit;
begin
  aTransaction := Data.GetNewConnection;
  aDocument := TDocument.Create(nil,Data,aTransaction);
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
  inherited Create(False);
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
      sbImage.DoubleBuffered := True;
    end;
end;
procedure TfPreview.frPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if isMoved then
    begin
      if (X < MoveX) and ((iPreview.Left + iPreview.Width + 25) > sbImage.Width) then
        iPreview.Left := iPreview.Left + (X - StartX);

      if (X > MoveX) and (iPreview.Left < 0) then
        iPreview.Left := iPreview.Left + (X - StartX);

      if (Y < MoveY) and ((iPreview.Top + iPreview.Height + 25) > sbImage.Height) then
        iPreview.Top := iPreview.Top + (Y - StartY);

      if (Y > MoveY) and (iPreview.Top < 0) then
        iPreview.Top := iPreview.Top + (Y - StartY);
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
      sbImage.DoubleBuffered := False;
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
    end;
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
    end;
end;

procedure TfPreview.tsImageShow(Sender: TObject);
begin
  pfrPreview.Visible:=not pfrpreview.Visible;
  pfrPreview.Visible:=not pfrpreview.Visible;
end;

procedure TfPreview.DoScalePreview;
begin
  iPreview.Height:=round(iPreview.Picture.Height*FScale);
  iPreview.Width:=round(iPreview.Picture.Width*FScale);
end;

constructor TfPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TfEditor.Create(Self);
end;

destructor TfPreview.Destroy;
begin
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
        iPreview.Picture.LoadFromStreamWithFileExt(aStream,aExtension);
        if iPreview.Picture.Width > iPreview.Picture.Height then
          FScale := Width/iPreview.Picture.Width
        else
          FScale := Height/iPreview.Picture.Height;
        sbImage.Visible:=True;
        pImageControls.Visible:=True;
        FEditor.Hide;
        pfrPreview.Visible:=False;
        Result := True;
        iPreview.Left:=0;
        iPreview.Top:=0;
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
        aFilename := getTempDir+'rpv.'+aExtension;
        aFStream := TFileStream.Create(getTempDir+'rpv.'+aExtension,fmCreate);
        aStream.Position:=0;
        aFStream.CopyFrom(aStream,aStream.Size);
        aFStream.Free;
        aProcess := TProcessUTF8.Create(nil);
        {$IFDEF WINDOWS}
        aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
        {$ELSE}
        aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
        {$ENDIF}
        aProcess.ShowWindow := swoHide;
        aProcess.CommandLine := Format({$IFDEF WINDOWS}AppendPathDelim(Application.Location+'tools')+'gswin32'+{$ELSE}'gs'+{$ENDIF}' -q -dBATCH -dMaxBitmap=300000000 -dNOPAUSE -dSAFER -sDEVICE=bmp16m -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -dFirstPage=1 -dLastPage=1 -sOutputFile=%s %s -c quit',[aFileName+'.bmp',aFileName]);
        aProcess.CurrentDirectory := Application.Location+'tools';
        aProcess.Execute;
        aProcess.Free;
        SysUtils.DeleteFile(aFileName);
        iPreview.Picture.LoadFromFile(aFileName+'.bmp');
        pImageControls.Visible:=True;
        SysUtils.DeleteFile(aFileName+'.bmp');
        Result := True;
        sbImage.Visible:=True;
        FEditor.Hide;
        pfrPreview.Visible:=False;
        if iPreview.Picture.Width > iPreview.Picture.Height then
          FScale := Width/iPreview.Picture.Width
        else
          FScale := Height/iPreview.Picture.Height;
      except
        SysUtils.DeleteFile(aFileName);
        SysUtils.DeleteFile(aFileName+'.bmp');
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
    end
  else //Try to Use Imagemagick to determinate the file typ and render it
    begin
      try
        aFilename := getTempDir+'rpv.'+aExtension;
        aFStream := TFileStream.Create(getTempDir+'rpv.'+aExtension,fmCreate);
        aStream.Position:=0;
        aFStream.CopyFrom(aStream,aStream.Size);
        aFStream.Free;
        aProcess := TProcessUTF8.Create(nil);
        {$IFDEF WINDOWS}
        aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
        {$ELSE}
        aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
        {$ENDIF}
        aProcess.ShowWindow := swoHide;
        aProcess.CommandLine := Format({$IFDEF WINDOWS}AppendPathDelim(Application.Location+'tools')+{$ENDIF}'convert "%s" -resize %d -alpha off +antialias "%s"',[aFileName,500,afileName+'.bmp']);
        aProcess.CurrentDirectory := Application.Location+'tools';
        aProcess.Execute;
        aProcess.Free;
        SysUtils.DeleteFile(aFileName);
        iPreview.Picture.LoadFromFile(afileName+'.bmp');
        SysUtils.DeleteFile(aFileName+'.bmp');
        sbImage.Visible:=True;
        pImageControls.Visible:=True;
        pfrPreview.Visible:=False;
        FEditor.Hide;
        if iPreview.Picture.Width > iPreview.Picture.Height then
          FScale := Width/iPreview.Picture.Width
        else
          FScale := Height/iPreview.Picture.Height;
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
  pcPages.ShowTabs:=False;
  tsText.TabVisible := False;
  iPreview.Picture.Clear;
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
        aFilename := getTempDir+'rpv.'+aExtension;
        aFStream := TFileStream.Create(getTempDir+'rpv.'+aExtension,fmCreate);
        aStream.Position:=0;
        aFStream.CopyFrom(aStream,aStream.Size);
        aFStream.Free;
        aProcess := TProcessUTF8.Create(nil);
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
        if iPreview.Picture.Width > iPreview.Picture.Height then
          FScale := Width/iPreview.Picture.Width
        else
          FScale := Height/iPreview.Picture.Height;
      except
        SysUtils.DeleteFile(aFileName);
        SysUtils.DeleteFile(aFileName+'.txt');
      end;
    end;
end;

end.
