unit utgridview;

{$mode objfpc}{$H+}

interface

{$define USEFORM}

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,ugridview,utask,
  {$ifdef USEFORM}
  GuiTestRunner,Forms,Controls
  {$endif}
  ;

type

  TGridviewtest= class(TTestCase)
  published
    procedure Create;
    procedure AddRow3;
    procedure AddTempRow;
    procedure AddRow1;
    procedure MoveRowTo1;
    procedure AddRow2;
    procedure AddRow5;
    procedure AddRow4;
    procedure CheckSettext;
    procedure Destroy;
  end;

implementation
uses uData;
var
  GV : ugridview.TfGridView;
  aDs: TTaskList;
  {$ifdef USEFORM}
  aFm : TForm;
  {$endif}
procedure TGridviewtest.Create;
begin
  GV := TfGridView.Create(nil);
  GV.SortField:='GPRIORITY';
  GV.DefaultRows:='GLOBALWIDTH:800;GPRIORITY:30;COMPLETED:30;SUMMARY:200;PROJECT:200;STARTDATE:60;DUEDATE:60;USER:100;OWNER:100';
  GV.BaseName := 'TESTTASK';
  aDs := TTaskList.Create(nil,Data);
  aDs.SelectActiveByUser('NOUSER');
  aDs.Open;
  GV.DataSet:=aDS;
  GV.SetRights(True);
  {$ifdef USEFORM}
  Application.CreateForm(TForm,aFm);
  GV.Parent := aFm;
  GV.Align:=alClient;
  GV.Show;
  aFm.Show;
  {$endif}
end;

procedure TGridviewtest.AddRow3; //Add one Row (later Row 3)
begin
  GV.Append;
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.gList.RowCount=2,'RowCount='+IntToStr(GV.gList.RowCount));
  Check(GV.gList.Row=1,'Row='+IntToStr(GV.gList.Row));
end;

procedure TGridviewtest.AddTempRow; //Try to check if double insert does nothing
begin
  GV.Insert;
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.gList.RowCount=2,'RowCount='+IntToStr(GV.gList.RowCount));
  Check(GV.gList.Row=1,'Row='+IntToStr(GV.gList.Row));
end;

procedure TGridviewtest.AddRow1; //Add an Row after Row3, set Summary to see if ident field check works
begin
  GV.Post;
  GV.IdentField:='SUMMARY';
  GV.Append;
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.gList.RowCount=3,'RowCount='+IntToStr(GV.gList.RowCount));
  Check(GV.gList.Row=2,'Row='+IntToStr(GV.gList.Row));
end;

procedure TGridviewtest.MoveRowTo1; //Move new Added Row to Position 1
begin
  GV.Post;
  GV.gList.MoveColRow(False,2,1);
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.dgFake.DataSource.DataSet.FieldByName('GPRIORITY').AsInteger=2,'Sortfield1='+IntToStr(GV.dgFake.DataSource.DataSet.FieldByName('GPRIORITY').AsInteger));
  Checktrue(GV.GotoRowNumber(1),'faild to goto Row 1');
  Check(GV.dgFake.DataSource.DataSet.FieldByName('GPRIORITY').AsInteger=1,'Sortfield1='+IntToStr(GV.dgFake.DataSource.DataSet.FieldByName('GPRIORITY').AsInteger));
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.dgFake.DataSource.DataSet.FieldByName('GPRIORITY').AsInteger=1,'Sortfield2='+IntToStr(GV.dgFake.DataSource.DataSet.FieldByName('GPRIORITY').AsInteger));
end;

procedure TGridviewtest.AddRow2; //Insert Row on Position 2
begin
  GV.gList.Row:=2;
  GV.Insert;
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.gList.RowCount=4,'RowCount='+IntToStr(GV.gList.RowCount));
  Check(GV.gList.Row=2,'Row='+IntToStr(GV.gList.Row));
end;

procedure TGridviewtest.AddRow5;//just append an new Row
begin
  GV.Post;
  GV.Append;
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.gList.RowCount=5,'RowCount='+IntToStr(GV.gList.RowCount));
  Check(GV.gList.Row=4,'Row='+IntToStr(GV.gList.Row));
end;

procedure TGridviewtest.AddRow4; //Insert Row after Position 3
begin
  GV.Post;
  GV.gList.Row:=3;
  GV.InsertAfter;
  {$ifdef USEFORM}
  Application.ProcessMessages;
  {$endif}
  Check(GV.gList.RowCount=6,'RowCount='+IntToStr(GV.gList.RowCount));
  Check(GV.gList.Row=4,'Row='+IntToStr(GV.gList.Row));
end;

procedure TGridviewtest.CheckSettext;
begin

end;

procedure TGridviewtest.Destroy;
begin
  GV.Free;
  {$ifdef USEFORM}
  aFm.Free;
  {$endif}
  aDS.Free;
end;



initialization

  RegisterTest(TGridviewtest);
end.

