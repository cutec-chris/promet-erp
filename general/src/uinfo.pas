unit uInfo;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,uGeneralStrConsts,
  ComCtrls,Utils,ubenchmark,ExtCtrls;

type
  { TfInfo }
  TfInfo = class(TForm)
    bClose: TBitBtn;
    Bevel1: TBevel;
    bBenchmark: TButton;
    lHarddiskrating: TLabel;
    lMemoryRating: TLabel;
    lCPURating: TLabel;
    lRating: TLabel;
    lValue: TLabel;
    lHarddiskValue: TLabel;
    lHarddisk: TLabel;
    lLanguageValue: TLabel;
    lLanguage: TLabel;
    lMemoryValue: TLabel;
    lMemory: TLabel;
    lCPUValue: TLabel;
    lCPU: TLabel;
    lOperatingSystemValue: TLabel;
    lOperatingSystem: TLabel;
    lVersion: tlabel;
    lTimeout: TLabel;
    lName: TLabel;
    lCopyright: TLabel;
    mChanges: TMemo;
    mInfo: TMemo;
    PageControl1: TPageControl;
    tsSystem: TTabSheet;
    tsChanges: TTabSheet;
    tsLicense: TTabSheet;
    procedure bBenchmarkClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Version : real;
    Revision : Integer;
    Timeout : TDate;
    ProgramName : string;
    Copyright,
    InfoText : string;
    procedure SetLanguage;
    function Execute : Boolean;
  end;

var
  fInfo: TfInfo;

implementation

uses LCLIntf;

{ TfInfo }

procedure TfInfo.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfInfo.bBenchmarkClick(Sender: TObject);
var
  mTransferRate: Extended;
  fReadTime: Extended = 0;
  fWriteTime: Extended = 0;
  WKIPS: Extended;
  DryTime: Int64;
begin
  bBenchmark.Enabled := False;
  mTransferRate := GetMemoryTransferRate(100000);
  lMemoryRating.Caption := FormatFloat('0.0',getMemoryRating(mTransferRate))+' ('+FormatFloat('0.0',mTransferRate)+'Mb/s)';
  Application.Processmessages;
  GetHardDiskTransferRates(50,fReadTime,fWriteTime);
  lHarddiskRating.Caption := FormatFloat('0.0',GetHardDiskRating(fReadTime,fWriteTime))+' ('+FormatFloat('0.0',fReadTime)+' Mb/s,'+FormatFloat('0.0',fWriteTime)+' Mb/s)';
  Application.Processmessages;
  WKIPS := GetWhetstone(10);
  DryTime := GetDryStone(10*20);
  if WKIPS > 10000 then
    lCPURating.Caption := FormatFloat('0.0',GetProcessorRating(WKIPS,(DryTime/(200))))+' ('+FormatFloat('0.0',WKIPS/10)+' WMIPS,'+IntToStr(DryTime)+'ms)'
  else
    lCPURating.Caption := FormatFloat('0.0',GetprocessorRating(WKIPS,(DryTime/(200))))+' ('+FormatFloat('0.0',WKIPS)+' WKIPS,'+IntToStr(DryTime)+'ms)';
  bBenchmark.Enabled := True;
end;

procedure TfInfo.FormCreate(Sender: TObject);
var
  r: Extended;
begin
  if FileExists(ExtractFilePath(Application.Exename)+'changes.txt') then
    mChanges.Lines.LoadFromFile(ExtractFilePath(Application.Exename)+'changes.txt');
  lMemoryValue.Caption := SizeToText(GetMemorySize);
  lOperatingSystemValue.Caption := getOSVersion;
  r := CalcCPUSpeed;
  if r > -1 then
    lCPUValue.Caption := FormatFloat('0.0',r)+' Mhz'
  else if r = -1 then
    lCPUValue.Caption := strNoX86CPU
  else
    lCPUValue.Caption := strNottested;
  lCPURating.Caption := FormatFloat('0.0',r*0.001);
  lLanguageValue.Caption := GetSystemLang;
  lMemoryRating.Caption := strNotTested;
  lHarddiskRating.Caption := strNotTested;
  lCPURating.Caption := strNotTested;
end;

procedure TfInfo.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfInfo,fInfo);
      Self := fInfo;
    end;
  lVersion.Caption := StringReplace(Format('Version %f Build %d',[Version,Revision]),',','.',[rfReplaceAll]);
  lName.Caption := ProgramName;
  tsLicense.caption := strLicense;
  tsChanges.Caption := strChanges;
  if Timeout <> 0 then
    lTimeout.Caption := strTimedOut+DateToStr(Timeout)
  else
    lTimeOut.Caption := '';
  lCopyright.Caption := Copyright;
  mInfo.Lines.Text := InfoText;
  lValue.Caption := strValue;
  lOperatingSystem.Caption := strOperatingSystem;
  lCPU.Caption := strCPU;
  lHarddisk.Caption := strHarddisk;
  lMemory.Caption := strMemory;
  lRating.Caption := strRating;
  bBenchmark.Caption := strBenchmark;
  tsSystem.Caption := strSystem;
  lLanguage.Caption := strLanguage;
end;

function TfInfo.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfInfo,fInfo);
      Self := fInfo;
    end;
  SetLanguage;
  Result := Showmodal = mrOK;
end;

initialization
  {$I uinfo.lrs}

end.
