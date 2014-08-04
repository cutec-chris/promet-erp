unit uError;


{$mode objfpc}{$H+}


interface


uses

  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,uGeneralStrConsts,LCLProc, LCLType,FileUtil,LConvEncoding;

type
  { TfError }
  TfError = class(TForm)
    bBacktrace: TBitBtn;
    bSendToAdmin: TBitBtn;
    bOK: TBitBtn;
    mError: TMemo;
    procedure bBacktraceClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    function GetMessage(aFile : string;aMessage : string) : string;
  public
    { public declarations }
    procedure ShowError(Msg : string = '');
    procedure ShowWarning(Msg : string = '');
    procedure SetLanguage;
  end;

var
  fError: TfError;

implementation

{ TfError }
procedure TfError.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfError.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

function TfError.GetMessage(aFile: string; aMessage: string): string;
var
  sl: TStringList;
  tmp: String;
  varname: String;
  tmp1: String;
  aRes: String;
  i: Integer;
  varvalue: String;
begin
  try
    aRes := ConvertEncoding(aMessage,GuessEncoding(aMessage),EncodingUTF8);
    Result := aRes;
    if FileExistsUTF8(AppendPathDelim(Application.Location)+aFile) then
      begin
        sl := TStringList.Create;
        sl.LoadFromFile(UTF8ToSys(AppendPathDelim(Application.Location)+aFile));
        for i := 0 to sl.Count-1 do
          begin
            aMessage:=ConvertEncoding(aMessage,GuessEncoding(aMessage),EncodingUTF8);
            aMessage := StringReplace(aMessage,#10,'',[rfReplaceAll]);
            aMessage := StringReplace(aMessage,#13,'',[rfReplaceAll]);
            tmp := sl.Names[i];
            Result := sl.ValueFromIndex[i];
            while pos('@',tmp)>0 do
              begin
                if pos('@',copy(tmp,pos('@',tmp)+1,length(tmp)))>0 then
                  begin
                    if copy(tmp,0,pos('@',tmp)-1) = copy(aMessage,0,pos('@',tmp)-1) then
                      begin
                        aMessage := copy(aMessage,pos('@',tmp),length(aMessage));
                        tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
                        varname := copy(tmp,0,pos('@',tmp)-1);
                        tmp := copy(tmp,pos('@',tmp)+1,length(tmp));
                        if pos('@',tmp)>0 then
                          tmp1 := copy(tmp,0,pos('@',tmp)-1)
                        else
                          tmp1 := tmp;
                        varvalue := copy(aMessage,0,pos(tmp1,aMessage)-1);
                        aMessage:=copy(aMessage,length(varvalue)+1,length(aMessage));
                        Result := StringReplace(Result,'@'+varname+'@',varvalue,[rfReplaceAll])
                      end
                    else
                      begin
                        Result := aRes;
                        break;
                      end;
                  end
                else
                  begin
                    Result := aRes;
                    break;
                  end;
              end;
            if (aMessage='') or (aMessage=tmp) then break
            else
              begin
                aMessage:=aRes;
                Result := aRes;
              end;
          end;
        sl.Free;
      end;
  except
    Result := aRes;
  end;
end;

procedure TfError.ShowError(Msg: string = '');
var
  aMsg : string;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfError,fError);
      Self := fError;
    end;
  SetLanguage;
  mError.Lines.Clear;
  mError.Font.Color:=clred;
  bSendToAdmin.Visible:=True;
  if Msg <> '' then
    aMsg := GetMessage('errors.txt',Msg)+lineending;
  if ExceptObject <> nil then
    begin
      aMsg := aMsg+lineending+strOriginalException+Exception(ExceptObject).Message;
      aMsg := aMsg+lineending+strDescription;
    end;
  Debugln(aMsg);
  mError.Lines.Text := trim(aMsg);
  try
    //bBacktrace.Visible:=ExceptObject <> nil;
  except
  end;
  Showmodal;
end;

procedure TfError.ShowWarning(Msg: string);
var
  aMsg: String;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfError,fError);
      Self := fError;
    end;
  SetLanguage;
  mError.Lines.Clear;
  mError.Font.Color:=clWindowText;
  bBacktrace.Visible:=False;
  bSendToAdmin.Visible:=False;
  if Msg <> '' then
    aMsg := GetMessage('warnings.txt',Msg)+lineending;
  Debugln(aMsg);
  mError.Lines.Text := trim(aMsg);
  Showmodal;
end;

procedure TfError.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfError,fError);
      Self := fError;
    end;
  try
    bOK.Caption := strOK;
    bBacktrace.Caption := strBacktrace;

    Caption := strError;
  except
  end;
end;

procedure TfError.bBacktraceClick(Sender: TObject);
var
  aMsg : string = '';
  FrameCount: LongInt;
  Frames: PPointer;
  FrameNumber: Integer;
begin
  try
    if ExceptAddr <> nil then
      begin
        aMsg := aMsg+lineending+strStackTrace;
        aMsg := aMsg+lineending+BackTraceStrFunc(ExceptAddr);
        FrameCount:=ExceptFrameCount;
        Frames:=ExceptFrames;
        for FrameNumber := 0 to FrameCount-1 do
          aMsg := aMsg+lineending+BackTraceStrFunc(Frames[FrameNumber]);
      end;
  except
  end;
  mError.Lines.Text:=mError.Lines.Text+lineending+aMsg;
end;

initialization
  {$I uerror.lrs}

end.




