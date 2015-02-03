unit uimportshotwell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ZConnection, ZDataset, uBaseApplication,uBaseDBInterface,db;

type
  TForm1 = class(TForm)
    DirectoryEdit1: TFileNameEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    ZConnection1: TZConnection;
    zData: TZReadOnlyQuery;
    procedure DirectoryEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses uBaseDocPages,uData;
procedure TForm1.DirectoryEdit1AcceptFileName(Sender: TObject; var Value: String
  );
var
  aDocPages: TDocPages;
  Fmt     : TFormatSettings;
begin
  fmt.ShortDateFormat:='yyyy-mm-dd';
  fmt.DateSeparator  :='-';
  fmt.LongTimeFormat :='hh:nn:ss';
  fmt.TimeSeparator  :=':';
  if FileExists(Value) then
    begin
      ZConnection1.Database:=Value;
      ZConnection1.Connect;
      zData.Active:=True;
      aDocPages := TDocPages.Create(nil);
      Data.Tree.Open;
      with zData do
        begin
          First;
          while not EOF do
            begin
              try
              ListBox1.Items.Add(FieldByName('filename').AsString);
              ListBox1.ItemIndex:=ListBox1.Items.Count-1;
              ListBox1.MakeCurrentVisible;
              Application.ProcessMessages;
              if FileExists(FieldByName('filename').AsString) then
                begin
                  aDocPages.AddFromFile(FieldByName('filename').AsString);
                  aDocPages.DataSet.Edit;
                  aDocPages.FieldByName('ORIGDATE').AsDateTime:=StrToDateTime(FieldByName('ORIGDATE').AsString,fmt);
                  aDocPages.Post;
                end;
              except
                ListBox1.Items.Add('fehler');
              end;
              Next;
            end;
        end;
      aDocPages.Free;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('importshotwell');
      with Application as IBaseDbInterface do
        LoadMandants;
      Login;
    end;
end;

end.

