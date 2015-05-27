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
Created 05.01.2013
*******************************************************************************}
unit uprojectimport;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uProjects,XMLRead,XMLWrite,DOM,uminiconvencoding,uData,uTask,db,
  Utils,Math,FileUtil,uBaseDBInterface,variants,uBaseDbClasses,uBaseDatasetInterfaces;
//function ImportMPX(aFile : TStream;aProject : TProject) : Boolean; //MS Project ASCII
function ImportMSPDI(aFile : TStream;aProject : TProject) : Boolean; //MS Project XML
function ImportGAN(aFile : TStream;aProject : TProject;ReplaceIDs : Boolean = True;ImportAsSnapshot : Boolean = False) : Boolean; //Gantt Project
function ExportGAN(aFile : TStream;bProject : TProject) : Boolean;

implementation
function ImportMSPDI(aFile: TStream; aProject: TProject): Boolean;
var
  aDoc: TXMLDocument;
begin
  ReadXMLFile(aDoc,aFile);

  aDoc.Free;
end;
function ImportGAN(aFile: TStream; aProject: TProject; ReplaceIDs: Boolean;
  ImportAsSnapshot: Boolean): Boolean;
var
  aDoc: TXMLDocument;
  aProjectNode: TDOMNode;
  aAllocNode: TDOMNode;
  i: Integer;
  aRecNode: TDOMNode;
  a: Integer;
  procedure ImportTasks(aParent : Variant;aTasks : TDOMNode);
  var
    aTask: TDOMNode;
    i: Integer;
    myFormat : TFormatSettings = (
        CurrencyFormat: 1;
        NegCurrFormat: 5;
        ThousandSeparator: ',';
        DecimalSeparator: '.';
        CurrencyDecimals: 2;
        DateSeparator: '-';
        TimeSeparator: ':';
        ListSeparator: ',';
        CurrencyString: '$';
        ShortDateFormat: 'y-m-d';
        LongDateFormat: 'yyyy-mm-dd';
        TimeAMString: 'AM';
        TimePMString: 'PM';
        ShortTimeFormat: 'hh:nn';
        LongTimeFormat: 'hh:nn:ss';
        ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                          'Jul','Aug','Sep','Oct','Nov','Dec');
        LongMonthNames: ('January','February','March','April','May','June',
                         'July','August','September','October','November','December');
        ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
        LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
        TwoDigitYearCenturyWindow: 50;
      );
    bTask: TTask;
  begin
    if assigned(aTasks) and (aTasks.FindNode('task') <> nil) and (aProject.Tasks.Count > 0) then
      begin
        aProject.Tasks.DataSet.Edit;
        aProject.Tasks.FieldByName('HASCHILDS').AsString:='Y';
        if aProject.Tasks.CanEdit then
          aProject.Tasks.DataSet.Post;
      end;
    for i := 0 to aTasks.ChildNodes.Count-1 do
      begin
        if aTasks.ChildNodes[i].NodeName = 'task' then
          begin
            aTask := aTasks.ChildNodes[i];
            with aProject do
              begin
                if ReplaceIDs and (Tasks.DataSet.Locate('ORIGID;SUMMARY',VarArrayOf([aTask.Attributes.GetNamedItem('id').NodeValue,ConvertEncoding(aTask.Attributes.GetNamedItem('name').NodeValue,GuessEncoding(aTask.Attributes.GetNamedItem('name').NodeValue),EncodingUTF8)]),[])) then
                  Tasks.DataSet.Edit
                else
                  Tasks.DataSet.Append;
                Tasks.FieldByName('PARENT').AsVariant:=aParent;
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                Tasks.FieldByName('SUMMARY').AsString:=ConvertEncoding(aTask.Attributes.GetNamedItem('name').NodeValue,GuessEncoding(aTask.Attributes.GetNamedItem('name').NodeValue),EncodingUTF8);
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                Tasks.FieldByName('STARTDATE').AsDateTime:=StrToDateTime(aTask.Attributes.GetNamedItem('start').NodeValue,myFormat);
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                if aTask.Attributes.GetNamedItem('meeting').NodeValue = 'true' then
                  Tasks.FieldByName('CLASS').AsString:='M';
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                if (not Tasks.FieldByName('STARTDATE').IsNull) then
                  Tasks.FieldByName('DUEDATE').AsDateTime:=StrToDateTime(aTask.Attributes.GetNamedItem('start').NodeValue,myFormat)+(StrToInt(aTask.Attributes.GetNamedItem('duration').NodeValue));
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                Tasks.FieldByName('ORIGID').AsString:=aTask.Attributes.GetNamedItem('id').NodeValue;
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                if Tasks.State = dsInsert then
                  if aTask.Attributes.GetNamedItem('complete').NodeValue = '1' then
                    Tasks.FieldByName('COMPLETED').AsString:='Y';
                if not tasks.CanEdit then Tasks.DataSet.Edit;
                if aTask.FindNode('notes') <> nil then
                  begin
                    Tasks.FieldByName('DESC').AsString:=aTask.FindNode('notes').FirstChild.NodeValue;
                  end;
                if Tasks.CanEdit then
                  Tasks.DataSet.Post;
                bTask := TTask.CreateEx(nil,Data,Tasks.Connection);
                bTask.Select(Tasks.Id.AsVariant);
                bTask.Open;
                if bTask.Count > 0 then
                  begin
                    while bTask.Dependencies.Count > 0 do
                      bTask.Dependencies.Delete;
                  end;
                bTask.Free;
              end;
            ImportTasks(aProject.Tasks.Id.AsVariant,aTask);
          end;
      end;
  end;
  procedure ImportTaskDeps(aParent : Variant;aTasks : TDOMNode);
  var
    aTask: TDOMNode;
    i: Integer;
    bTask: TTask;
    aLink: String;
    b: Integer;
  begin
    for i := 0 to aTasks.ChildNodes.Count-1 do
      begin
        if aTasks.ChildNodes[i].NodeName = 'task' then
          begin
            aTask := aTasks.ChildNodes[i];
            with aProject do
              begin
                for b := 0 to aTask.ChildNodes.Count-1 do
                  if aTask.ChildNodes[b].NodeName = 'depend' then
                  begin
                    if Tasks.DataSet.Locate('ORIGID',aTask.Attributes.GetNamedItem('id').NodeValue,[]) then
                      aLink := Data.BuildLink(Tasks.DataSet);
                    if Tasks.DataSet.Locate('ORIGID',aTask.ChildNodes[b].Attributes.GetNamedItem('id').NodeValue,[]) then
                      begin
                        bTask := TTask.CreateEx(nil,Data,Tasks.Connection);
                        bTask.Select(Tasks.Id.AsVariant);
                        bTask.Open;
                        if bTask.Count > 0 then
                          begin
                            bTask.Dependencies.Open;
                            bTask.Dependencies.Add(aLink);
                          end;
                        bTask.Free;
                      end;
                  end;
              end;
            ImportTaskDeps(aProject.Tasks.Id.AsVariant,aTask);
          end;
      end;
  end;
begin
  ReadXMLFile(aDoc,aFile);
  aProjectNode := aDoc.FindNode('project');
  aAllocNode := aProjectNode.FindNode('allocations');
  aRecNode := aProjectNode.FindNode('resources');
  if aProject.FieldByName('NAME').IsNull then
    begin
      if not aProject.CanEdit then aProject.DataSet.Edit;
      aProject.FieldByName('NAME').AsString:=aProjectNode.Attributes.GetNamedItem('name').NodeValue;
    end;
  aProject.Tasks.DataSet.DisableControls;
  try
  //Todo import Calendars
  //Import all Tasks
  ImportTasks(Null,aProjectNode.FindNode('tasks'));
  //Fill in Dependencies
  ImportTaskDeps(Null,aProjectNode.FindNode('tasks'));
  //Import resources
  if Assigned(aAllocNode) and Assigned(aRecNode) then
    for i := 0 to aAllocNode.ChildNodes.Count-1 do
      begin
        if aAllocNode.ChildNodes[i].NodeName = 'allocation' then
          with aProject do
            if Tasks.DataSet.Locate('ORIGID',aAllocNode.ChildNodes[i].Attributes.GetNamedItem('task-id').NodeValue,[]) then
              begin
                for a := 0 to aRecNode.ChildNodes.Count-1 do
                  if aRecNode.ChildNodes[a].Attributes.GetNamedItem('id').NodeValue = aAllocNode.ChildNodes[i].Attributes.GetNamedItem('resource-id').NodeValue then
                    begin
                      if Tasks.FieldByName('USER').IsNull then
                        begin
                          Tasks.DataSet.Edit;
                          Tasks.FieldByName('USER').AsString:=aRecNode.ChildNodes[a].Attributes.GetNamedItem('name').NodeValue;
                          Tasks.DataSet.Post;
                        end;
                      break;
                    end;
              end;
      end;

  finally
    aProject.Tasks.DataSet.EnableControls;
    aAllocNode.Free;
    aProjectNode.Free;
    aDoc.Free;
  end;
end;
function ExportGAN(aFile: TStream; bProject: TProject): Boolean;
var
  aDoc: TXMLDocument;
  aTasksNode: TDOMNode;
  aParentN : Variant;

  function FindParent(aParent : TDOMNode;aID : String) : TDOMNode;
  var
    i: Integer;
  begin
    result := nil;
    if TDOMElement(aParent).GetAttribute('id')=aID then
      Result := aParent
    else
      begin
        for i := 0 to aParent.ChildNodes.Count-1 do
          begin
            Result := FindParent(aParent.ChildNodes[i],aId);
            if Assigned(Result) then break;
          end;
      end;
  end;

  function AddTaskNode(aOParent : TDOMNode;aTask : TTaskList) : TDOMNode;
  var
    aParent: TDOMNode = nil;
    aRec: LargeInt;
    aStart: TDateTime;
    aDue: TDateTime;
    aITask: TTask;
    aDep: TDOMElement;
    aIDep: TDependencies;
  begin
    if aTask.FieldByName('PARENT').AsString<>'' then
      begin
        aParent := FindParent(aTasksNode,aTask.FieldByName('PARENT').AsString);
        if not Assigned(aParent) then
          begin
            aRec := aTask.GetBookmark;
            if aParentN <> aTask.FieldByName('PARENT').AsVariant then
              aParentN := aTask.FieldByName('PARENT').AsVariant
            else aParentN := Null;
            if aTask.DataSet.Locate('SQL_ID',aParentN,[]) then
              aParent := AddTaskNode(aOParent,aTask);
            aTask.GotoBookmark(arec);
          end;
      end;
    if not Assigned(aParent) then aParent := aOParent;
    Result := aDoc.CreateElement('task');
    TDOMElement(result).SetAttribute('id',aTask.Id.AsVariant);
    TDOMElement(result).SetAttribute('name', UniToSys(aTask.FieldByName('SUMMARY').AsString));
    TDOMElement(result).SetAttribute('meeting','false');
    aStart := aTask.FieldByName('STARTDATE').AsDateTime;
    if aStart = 0 then
      aStart := Now();
    aDue := aTask.FieldByName('DUEDATE').AsDateTime;
    if aDue = 0 then
      aDue := aStart+max(aTask.FieldByName('PLANTIME').AsFloat,1);
    if aStart>aDue then aStart := aDue-max(aTask.FieldByName('PLANTIME').AsFloat,1);
    TDOMElement(result).SetAttribute('start',FormatDateTime('YYYY-MM-DD',aStart));
    TDOMElement(result).SetAttribute('duration',IntToStr(trunc(aDue-aStart)));
    aIDep := TDependencies.Create(nil);
    aIDep.SelectByLink(Data.BuildLink(aTask.DataSet));
    aIDep.Open;
    with aIDep.DataSet do
      begin
        First;
        while not EOF do
          begin
            if FieldByName('REF_ID_ID').AsString <> '' then
              begin
                aDep := aDoc.CreateElement('depend');
                TDOMElement(aDep).SetAttribute('id',FieldByName('REF_ID').AsString);
                TDOMElement(aDep).SetAttribute('type','2');
                TDOMElement(aDep).SetAttribute('hardness','Strong');
                TDOMElement(aDep).SetAttribute('difference','0');
                result.AppendChild(aDep);
              end;
            Next;
          end;
      end;
    aIDep.Free;
    aParent.AppendChild(result);
  end;

var
  aProjectNode: TDOMNode;
  aAllocNode: TDOMNode;
  aNode: TDOMElement;
  auNode: TDOMElement;
  aProject: TProject;
begin
  aProject := TProject.Create(nil);
  aProject.Select(bProject.Id.AsVariant);
  with aProject.DataSet as IBaseDBFilter do
    begin
      SortFields := 'GPRIORITY';
      BaseSortFields:='GPRIORITY';
      SortDirection := sdAscending;
    end;
  aProject.Open;
  aDoc := TXMLDocument.Create;
  aProjectNode := aDoc.CreateElement('project');
  aDoc.AppendChild(aProjectNode);
  TDOMElement(aProjectNode).SetAttribute('name',UniToSys(aProject.FieldByName('NAME').AsString));
  TDOMElement(aProjectNode).SetAttribute('version','2.0');
  aTasksNode := aDoc.CreateElement('tasks');
  aProjectNode.AppendChild(aTasksNode);
  aProject.Tasks.Open;
  aProject.Tasks.DataSet.DisableControls;
  with aProject.Tasks.DataSet do
    begin
      First;
      while not EOF do
        begin
          AddTaskNode(aTasksNode,aProject.Tasks);
          Next;
        end;
    end;
  aProject.Tasks.DataSet.EnableControls;
  aNode := aDoc.CreateElement('taskdisplaycolumns');
  aProjectNode.AppendChild(aNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd0');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd1');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd2');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd3');
  TDOMElement(auNode).SetAttribute('order','0');
  TDOMElement(auNode).SetAttribute('width','167');
  TDOMElement(auNode).SetAttribute('visible','true');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd4');
  TDOMElement(auNode).SetAttribute('order','1');
  TDOMElement(auNode).SetAttribute('width','65');
  TDOMElement(auNode).SetAttribute('visible','true');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd5');
  TDOMElement(auNode).SetAttribute('order','2');
  TDOMElement(auNode).SetAttribute('width','64');
  TDOMElement(auNode).SetAttribute('visible','true');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd6');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd7');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd8');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd9');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd10');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);
  auNode := aDoc.CreateElement('displaycolumn');
  TDOMElement(auNode).SetAttribute('property-id','tpd11');
  TDOMElement(auNode).SetAttribute('order','-1');
  TDOMElement(auNode).SetAttribute('width','75');
  TDOMElement(auNode).SetAttribute('visible','false');
  aNode.AppendChild(auNode);

  WriteXMLFile(aDoc,aFile);
  aDoc.Free;
  aProject.Free;
end;

end.

