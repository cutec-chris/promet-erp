unit uDocumentCheckin;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, StdCtrls, Buttons, Utils;
type
  TfDocumentCheckin = class(TForm)
    ButtonPanel1: TButtonPanel;
    clFiles: TCheckListBox;
    eHide: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mComment: TMemo;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FFiles : TStringList;
    FOrigFiles : TStrings;
    FDirectory : String;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(aFiles : TStrings;Directory: string) : Boolean;
    procedure RefreshList;
  end; 
var
  fDocumentCheckin: TfDocumentCheckin;
implementation
{$R *.lfm}
resourcestring
  strChanged                            = '(geändert)';
  strNew                                = '(neu)';

procedure TfDocumentCheckin.SpeedButton1Click(Sender: TObject);
begin
  RefreshList;
end;

constructor TfDocumentCheckin.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFiles := TStringList.Create;
end;

destructor TfDocumentCheckin.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TfDocumentCheckin.Execute(aFiles: TStrings;Directory: string): Boolean;
var
  i: Integer;
  bFiles: TStringList;
  aIdx: Integer;
  tmp: String;
begin
  if aFiles.Count = 0 then
    begin
      Result := True;
      exit;
    end;
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDocumentCheckin,fDocumentCheckin);
      Self := fDocumentCheckin;
    end;
  FOrigFiles := aFiles;
  FDirectory := Directory;
  RefreshList;
  Result := False;
  if Showmodal = mrOK then
    begin
      bFiles := TStringList.Create;
      for i := 0 to FOrigFiles.Count-1 do
        begin
          tmp := copy(FOrigFiles.Names[i],length(Directory)+1,length(FOrigFiles.Names[i]));
          aidx := clFiles.Items.IndexOf(tmp);
          if aidx = -1 then
            aidx := clFiles.Items.IndexOf(tmp+' '+strChanged);
          if aidx = -1 then
            aidx := clFiles.Items.IndexOf(tmp+' '+strNew);
        if aidx > -1 then
          if clFiles.Checked[aidx] then
            begin
              tmp := aFiles[i];
              bFiles.Add(tmp);
            end;
        end;
      aFiles.Assign(bFiles);
      bFiles.Free;
      Result := True;
    end;
end;

procedure TfDocumentCheckin.RefreshList;
var
  aTypes: TStringList;
  i: Integer;
  aIdx: Integer;
  function TypeThere : Boolean;
  var
    Found: Boolean;
    a: Integer;
  begin
    Found := False;
    for a := 0 to aTypes.Count-1 do
      if lowercase(aTypes[a])=lowercase(ExtractFileExt(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i])))) then
        begin
          Found := True;
          break;
        end;
    Result := Found;
  end;

begin
  clFiles.Clear;
  mComment.Clear;
  FFiles.Clear;
  aTypes := TStringList.Create;
  aTypes.Delimiter:=',';
  aTypes.DelimitedText:=eHide.Text;
  for i := 0 to FOrigFiles.Count-1 do
    begin
      if FOrigFiles.ValueFromIndex[i] = 'C' then
        begin
          if not TypeThere then
            begin
              aIdx := clFiles.Items.Add(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i]))+' '+strChanged);
              clFiles.Checked[aIdx] := True;
            end;
          FFiles.Add(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i]))+' '+strChanged);
        end
      else if FOrigFiles.ValueFromIndex[i] = 'N' then
        begin
          if not TypeThere then
            begin
              aIdx := clFiles.Items.Add(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i]))+' '+strNew);
              clFiles.Checked[aIdx] := False;
            end;
          FFiles.Add(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i]))+' '+strNew);
        end
      else
        begin
          if not TypeThere then
            begin
              clFiles.Items.Add(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i])));
            end;
          FFiles.Add(copy(FOrigFiles.Names[i],length(FDirectory)+1,length(FOrigFiles.Names[i])));
        end;
    end;
end;

end.
