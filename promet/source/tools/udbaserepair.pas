unit udbaserepair;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, dbf, FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCheck: TButton;
    Dbf: TDbf;
    DbfNew: TDbf;
    deDirectory: TDirectoryEdit;
    lbTables: TListBox;
    mLog: TMemo;
    procedure bCheckClick(Sender: TObject);
    procedure deDirectoryAcceptDirectory(Sender: TObject; var Value: String);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.bCheckClick(Sender: TObject);
var
  i: Integer;
  a: Integer;
begin
  for i := 0 to lbTables.Count-1 do
    if lbTables.Selected[i] then
      begin
        a := 1;
        while FileExistsUTF8(AppendPathDelim(deDirectory.Directory)+'_'+lbTables.Items[i]+'_'+IntToStr(i)+'.dbf') do
          inc(a);
        RenameFileUTF8(AppendPathDelim(deDirectory.Directory)+lbTables.Items[i]+'.dbf',AppendPathDelim(deDirectory.Directory)+'_'+lbTables.Items[i]+'_'+IntToStr(a)+'.dbf');
        if FileExistsUTF8(AppendPathDelim(deDirectory.Directory)+lbTables.Items[i]+'.dbt') then
          RenameFileUTF8(AppendPathDelim(deDirectory.Directory)+lbTables.Items[i]+'.dbt',AppendPathDelim(deDirectory.Directory)+'_'+lbTables.Items[i]+'_'+IntToStr(a)+'.dbt');
        if FileExistsUTF8(AppendPathDelim(deDirectory.Directory)+lbTables.Items[i]+'.mdx') then
          RenameFileUTF8(AppendPathDelim(deDirectory.Directory)+lbTables.Items[i]+'.mdx',AppendPathDelim(deDirectory.Directory)+'_'+lbTables.Items[i]+'_'+IntToStr(a)+'.mdx');
        Dbf.TableName:=AppendPathDelim(deDirectory.Directory)+'_'+lbTables.Items[i]+'_'+IntToStr(a)+'.dbf';
        mLog.Lines.Add('opening table '+lbTables.Items[i]);
        Dbf.Open;
        Dbf.TryExclusive;
        mLog.Lines.Add('done.');
        mLog.Lines.Add('creating new table');
        DbfNew.FieldDefs.Assign(Dbf.FieldDefs);
        DbfNew.IndexDefs.Assign(Dbf.IndexDefs);
        DbfNew.TableName:=AppendPathDelim(deDirectory.Directory)+lbTables.Items[i]+'.dbf';
        DbfNew.TableLevel:=Dbf.TableLevel;
        DbfNew.CreateTable;
        DbfNew.Open;
        DbfNew.TryExclusive;
        mLog.Lines.Add('done.');
        mLog.Lines.Add('copying data');
        Dbf.First;
        while not Dbf.EOF do
          begin
            DbfNew.Append;
            for a := 0 to Dbf.FieldDefs.Count-1 do
              if DbfNew.Fields.FindField(Dbf.FieldDefs[a].Name) <> nil then
                DbfNew.Fields.FindField(Dbf.FieldDefs[a].Name).AsVariant := Dbf.Fields[a].AsVariant;
            DbfNew.Post;
            Dbf.Next;
          end;
        Dbf.Close;
        mLog.Lines.Add('done.');
        mLog.Lines.Add('regenerating indexes');
        DbfNew.RegenerateIndexes;
        mLog.Lines.Add('done.');
        mLog.Lines.Add('closing table'+lbTables.Items[i]);
        DbfNew.EndExclusive;
        DbfNew.Close;
        mLog.Lines.Add('done.');
        lbTables.Selected[i]:=false;
      end;
end;

procedure TForm1.deDirectoryAcceptDirectory(Sender: TObject;
  var Value: String);
Var Info : TSearchRec;
  i: Integer;
begin
  lbTables.Clear;
  If FindFirstUTF8(AppendPathDelim(Value)+'*.dbf',faAnyFile,Info)=0 then
    begin
      repeat
        if  (Info.Attr and faDirectory = 0)
        and (pos('_',Info.Name) = 0) then lbTables.Items.Add(copy(Info.Name,0,pos('.',Info.Name)-1));
      until FindNextUTF8(info)<>0;
    end;
  FindCloseUTF8(Info);
  for i := 0 to lbTables.Items.Count-1 do
    lbTables.Selected[i]:=True;
end;

initialization
  {$I udbaserepair.lrs}

end.
