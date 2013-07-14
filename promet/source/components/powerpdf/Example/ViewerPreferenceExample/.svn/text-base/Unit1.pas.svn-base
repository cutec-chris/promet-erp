unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PReport, PdfDoc, ComCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    Button1: TButton;
    PRPage1: TPRPage;
    PReport1: TPReport;
    PRLayoutPanel1: TPRLayoutPanel;
    PRLabel1: TPRLabel;
    PRLabel2: TPRLabel;
    PRLabel3: TPRLabel;
    PRLabel4: TPRLabel;
    PRLabel5: TPRLabel;
    PRLabel6: TPRLabel;
    PRLabel7: TPRLabel;
    PRLabel8: TPRLabel;
    PRLabel9: TPRLabel;
    PRLabel10: TPRLabel;
    PRLabel11: TPRLabel;
    PRLabel12: TPRLabel;
    PRLabel13: TPRLabel;
    PRLabel14: TPRLabel;
    PRLabel15: TPRLabel;
    PRLabel16: TPRLabel;
    PRLabel17: TPRLabel;
    PRLabel18: TPRLabel;
    PRLabel19: TPRLabel;
    PRLabel20: TPRLabel;
    PRLabel21: TPRLabel;
    PRLabel22: TPRLabel;
    PRLabel23: TPRLabel;
    PRLabel24: TPRLabel;
    PRLabel25: TPRLabel;
    PRLabel26: TPRLabel;
    PRLabel27: TPRLabel;
    HideToolbar: TCheckBox;
    HideMenubar: TCheckBox;
    HideWindowUI: TCheckBox;
    FitWindow: TCheckBox;
    CenterWindow: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure HideToolbarClick(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with PReport1 do
  begin
    BeginDoc;
    Print(PRPage1);
    EndDoc;
  end;
  ShellExecute(Self.Handle, 'Open', 'default.pdf', '', '', SW_SHOW);
end;

procedure TForm1.HideToolbarClick(Sender: TObject);
begin
  with PReport1 do
    if TCheckBox(Sender).Name = 'HideToolbar' then
      if TCheckBox(Sender).Checked then
        ViewerPreference := ViewerPreference + [vpHideToolbar]
      else
        ViewerPreference := ViewerPreference - [vpHideToolbar]
    else
    if TCheckBox(Sender).Name = 'HideMenubar' then
      if TCheckBox(Sender).Checked then
        ViewerPreference := ViewerPreference + [vpHideMenubar]
      else
        ViewerPreference := ViewerPreference - [vpHideMenubar]
    else
    if TCheckBox(Sender).Name = 'HideWindowUI' then
      if TCheckBox(Sender).Checked then
        ViewerPreference := ViewerPreference + [vpHideWindowUI]
      else
        ViewerPreference := ViewerPreference - [vpHideWindowUI]
    else
    if TCheckBox(Sender).Name = 'FitWindow' then
      if TCheckBox(Sender).Checked then
        ViewerPreference := ViewerPreference + [vpFitWindow]
      else
        ViewerPreference := ViewerPreference - [vpFitWindow]
    else
    if TCheckBox(Sender).Name = 'CenterWindow' then
      if TCheckBox(Sender).Checked then
        ViewerPreference := ViewerPreference + [vpCenterWindow]
      else
        ViewerPreference := ViewerPreference - [vpCenterWindow];
end;

end.
