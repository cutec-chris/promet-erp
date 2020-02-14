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
unit uPerson;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, uBaseDbClasses, uBaseERPDBClasses, uIntfStrConsts,uBaseDatasetInterfaces;
type

  { TPersonList }

  TPersonList = class(TBaseERPList)
  public
    function GetMatchCodeFieldName: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetStatusFieldName : string;override;
    function GetTyp: string; override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function SelectFromLink(aLink : string) : Boolean;override;
    procedure SelectByAccountNo(aAccountNo : string);overload;
    function CombineItems(aRemoteLink: string): Boolean; override;
  end;

  TBaseDbAddress = class(TBaseDBList)
  private
    function GetAddress: TField;
    function GetCity: TField;
    function GetCountry: TField;
    function GetName: TField;
    function GetTitle: TField;
    function GetZip: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Assign(Source: TPersistent); override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    function ToString: ansistring;override;
    procedure OpenItem(AccHistory: Boolean=True); override;
    procedure FromString(aStr : AnsiString);virtual;
    property Title : TField read GetTitle;
    property AdressName : TField read GetName;
    property Address : TField read GetAddress;
    property Country : TField read GetCountry;
    property City : TField read GetCity;
    property Zip : TField read GetZip;
  end;
  TPerson = class;
  TPersonAddress = class(TBaseDBAddress)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName : string;override;
  end;
  TPersonContactData = class(TBaseDBList)
  private
    function Getcomment: TField;
    function GetData: TField;
    function GetTyp: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    function GetNumberFieldName : string;override;
    function GetTextFieldName: string;override;
    property Typ : TField read GetTyp;
    property Data : TField read GetData;
    property Description : TField read Getcomment;
  end;
  TPersonBanking = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function CheckAccount : Boolean;
  end;
  TPersonLinks = class(TLinks)
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
  end;
  TPersonEmployees = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TPerson = class(TPersonList,IBaseHistory)
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FBanking: TPersonBanking;
    FCustomerCont: TPersonContactData;
    FEmployees: TPersonEmployees;
    FHistory: TBaseHistory;
    FImages: TImages;
    FLinks: TPersonLinks;
    FPersonAddress: TPersonAddress;
    FStatus : string;
    FDS : TDataSource;
    FStateChange: TNotifyEvent;
    function GetAccountNo: TField;
    function GetHistory: TBaseHistory;
    function GetInfo: TField;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure Open; override;
    function CreateTable : Boolean;override;
    procedure CascadicPost;override;
    procedure CascadicCancel;override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    function Find(aIdent : string;Unsharp : Boolean = False) : Boolean;override;
    property Address : TPersonAddress read FPersonAddress;
    property ContactData : TPersonContactData read FCustomerCont;
    property History : TBaseHistory read FHistory;
    property Images : TImages read FImages;
    property Banking : TPersonBanking read FBanking;
    property Links : TPersonLinks read FLinks;
    property Employees : TPersonEmployees read FEmployees;
    property AccountNo : TField read GetAccountNo;
    property Info : TField read GetInfo;
    function SelectFromContactData(aCont : string) : Boolean;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
    procedure GenerateThumbnail; override;
  end;

implementation
uses uBaseDBInterface, uBaseSearch, uBaseApplication, uData, Utils,uthumbnails,
  comparewild;

procedure TPersonEmployees.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'EMPLOYEES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,40,True);
            Add('DEPARTMENT',ftString,30,False);
            Add('POSITION',ftString,30,False);
            Add('EMPLOYEE',ftString,20,False);
          end;
    end;
end;
procedure TPersonLinks.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('RREF_ID').AsVariant:=(Parent as TPerson).Id.AsVariant;
end;
procedure TPersonBanking.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CUSTOMERBANKING';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('SORTCODE',ftString,20,False);
            Add('ACCOUNT',ftString,200,False);
            Add('INSTITUTE',ftString,60,false);
          end;
    end;
end;

function TPersonBanking.CheckAccount: Boolean;
  function Modulo97(const aIBAN:string):Integer;
  const
     m36:string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  var
     nCounter, nPruef : Integer;
  begin
     Result := 0;

     for nCounter := 1 to Length(aIBAN) do
     begin
        nPruef := Pos(aIBAN[nCounter], m36) ;

        if (nPruef = 0) then
           raise Exception.CreateFmt('Modulo97PruefZiffer(%s): invalid data', [aIBAN]);

        Dec(nPruef);

        if (nPruef > 9) then
        begin
           Result := Result * 10 + (nPruef div 10);
           nPruef := nPruef mod 10;
        end;

        Result := Result * 10 + nPruef;
        Result := Result mod 97;
     end;
  end;

  function EncodeCountry(const aLand: string): string;
  var
    sLetter: Char;
  begin
    for sLetter in aLand do
      case sLetter of
        'A': Result := Result + '10';
        'B': Result := Result + '11';
        'C': Result := Result + '12';
        'D': Result := Result + '13';
        'E': Result := Result + '14';
        'F': Result := Result + '15';
        'G': Result := Result + '16';
        'H': Result := Result + '17';
        'I': Result := Result + '18';
        'J': Result := Result + '19';
        'K': Result := Result + '20';
        'L': Result := Result + '21';
        'M': Result := Result + '22';
        'N': Result := Result + '23';
        'O': Result := Result + '24';
        'P': Result := Result + '25';
        'Q': Result := Result + '26';
        'R': Result := Result + '27';
        'S': Result := Result + '28';
        'T': Result := Result + '29';
        'U': Result := Result + '30';
        'V': Result := Result + '31';
        'W': Result := Result + '32';
        'X': Result := Result + '33';
        'Y': Result := Result + '34';
        'Z': Result := Result + '35';
      else
        Result := Result + EmptyStr;
      end;
  end;

  function TestIBAN(const aIBAN: string): boolean;
  var
    sBLZ: string;
    sKTO: string;
    sIBAN: string;
    sLand: string;
    sLand2: string;
    sControl: string;
  begin
      sLand := Copy(aIBAN, 1, 2);

      if (sLand <> 'DE') then
      begin
        Result := true;
        Exit;
      end;

      sControl := Copy(aIBAN, 3, 2);
      sBLZ := Copy(aIBAN, 5, 8);
      sKTO := Copy(aIBAN, 13, 10);
      sLand2 := EncodeCountry(sLand);
      sIBAN := sBLZ + sKTO + sLand2 + sControl;

      Result := (Modulo97(sIBAN) = 1);
  end;
begin
  Result := TestIBAN(FieldByName('ACCOUNT').AsString);
end;

function TPersonContactData.Getcomment: TField;
begin
  Result := FieldByName('DESCR');
end;

function TPersonContactData.GetData: TField;
begin
  Result := FieldByName('DATA');
end;

function TPersonContactData.GetTyp: TField;
begin
  Result := FieldByName('TYPE');
end;

procedure TPersonContactData.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CUSTOMERCONT';
      TableCaption := strCustomerCont;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACCOUNTNO',ftString,20,True);
            Add('DESCR',ftString,30,False);
            Add('TYPE',ftString,4,False);
            Add('DATA',ftString,80,False);
            Add('LINK',ftString,400,False);
            Add('ACTIVE',ftString,1,False);
          end;
       if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ACCOUNTNO','ACCOUNTNO',[]);
            Add('TYPE','TYPE',[]);
            Add('DATA','DATA',[]);
          end;
   end;
end;
procedure TPersonContactData.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      if DataSet.FieldDefs.IndexOf('ACCOUNTNO') > -1 then
        FieldByName('ACCOUNTNO').AsString := TPerson(Parent).FieldByName('ACCOUNTNO').AsString;
    end;
end;

function TPersonContactData.GetNumberFieldName: string;
begin
  Result := 'LINK';
end;

function TPersonContactData.GetTextFieldName: string;
begin
  Result := 'DATA';
end;

procedure TPersonAddress.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ADDRESSES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ADDRNO',ftString,15,True);
            Add('DESCR',ftString,30,False);
            Add('ACTIVE',ftString,1,False);
          end;
    end;
end;
procedure TPersonAddress.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      if DataSet.FieldDefs.IndexOf('ACCOUNTNO') > -1 then
        FieldByName('ACCOUNTNO').AsString := TPerson(Parent).FieldByName('ACCOUNTNO').AsString;
      FieldByName('ADDRNO').AsInteger := DataSet.RecordCount+1;
      FieldByName('COUNTRY').AsString := UpperCase(TPerson(Parent).FieldByName('LANGUAGE').AsString);
    end;
end;
function TPersonAddress.GetTextFieldName: string;
begin
  Result := 'NAME';
end;
function TPersonAddress.GetNumberFieldName: string;
begin
  Result := 'ZIP';
end;
function TPersonAddress.GetDescriptionFieldName: string;
begin
  Result:= 'ADDRESS';
end;

function TBaseDbAddress.GetAddress: TField;
begin
  Result := DataSet.FindField('ADDRESS');
end;

function TBaseDbAddress.GetCity: TField;
begin
  Result := DataSet.FindField('CITY');
end;

function TBaseDbAddress.GetCountry: TField;
begin
  Result := DataSet.FindField('COUNTRY');
end;

function TBaseDbAddress.GetName: TField;
begin
  Result := DataSet.FindField('NAME');
end;

function TBaseDbAddress.GetTitle: TField;
begin
  Result := DataSet.FindField('TITLE');
end;

function TBaseDbAddress.GetZip: TField;
begin
  Result := DataSet.FindField('ZIP');
end;

procedure TBaseDbAddress.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableCaption := strAdresses;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,3,True);
            Add('TITLE',ftString,8,False);
            Add('NAME',ftString,200,false);
            Add('CNAME',ftString,40,false);
            Add('ADDITIONAL',ftString,200,False);
            Add('ADDRESS',ftMemo,0,False);
            Add('CITY',ftString,40,False);
            Add('ZIP',ftString,10,False);
            Add('STATE',ftString,50,False);
            Add('COUNTRY',ftString,3,False);
            Add('POBOX',ftInteger,0,False);
          end;
    end;
end;
procedure TBaseDbAddress.Assign(Source: TPersistent);
var
  aAddress: TBaseDbAddress;
  Person: TPerson;
begin
  if Source is TBaseDBAddress then
    begin
      if not TBaseDBAddress(Source).Active then TBaseDBAddress(Source).Open;
      if (DataSet.State <> dsInsert) and (DataSet.State <> dsEdit) then
        DataSet.Edit;
      aAddress := Source as TBaseDbAddress;
      DataSet.FieldByName('TITLE').AsString := aAddress.FieldByName('TITLE').AsString;
      if aAddress.FieldByName('CNAME').AsString<>'' then
        DataSet.FieldByName('NAME').AsString := aAddress.FieldByName('CNAME').AsString+' '+aAddress.FieldByName('NAME').AsString
      else
        DataSet.FieldByName('NAME').AsString := aAddress.FieldByName('NAME').AsString;
      DataSet.FieldByName('ADDITIONAL').AsString := aAddress.FieldByName('ADDITIONAL').AsString;
      DataSet.FieldByName('ADDRESS').AsString := aAddress.FieldByName('ADDRESS').AsString;
      DataSet.FieldByName('CITY').AsString := aAddress.FieldByName('CITY').AsString;
      DataSet.FieldByName('ZIP').AsString := aAddress.FieldByName('ZIP').AsString;
      DataSet.FieldByName('STATE').AsString := aAddress.FieldByName('STATE').AsString;
      DataSet.FieldByName('COUNTRY').AsString := aAddress.FieldByName('COUNTRY').AsString;
    end
  else if Source is TPerson then
    begin
      Self.Assign(Tperson(Source).Address);
      Person := Source as TPerson;
      if (DataSet.State <> dsInsert) and (DataSet.State <> dsEdit) then
        DataSet.Edit;
      DataSet.FieldByName('ACCOUNTNO').AsString := Person.FieldByName('ACCOUNTNO').AsString;
    end
  else
    inherited Assign(Source);
end;

procedure TBaseDbAddress.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      if aDataSet.RecordCount = 0 then
        DataSet.FieldByName('TYPE').AsString:='IAD'
      else
        DataSet.FieldByName('TYPE').AsString:='DAD';
    end;
end;
function TBaseDbAddress.ToString: ansistring;
var
  aAddress : TStringList;
begin
  aAddress := TStringList.Create;
  aAddress.Add(DataSet.FieldbyName('TITLE').AsString);
  if aAddress[aAddress.Count-1] = '' then aAddress.Delete(aAddress.Count-1);
  aAddress.Add(DataSet.FieldbyName('CNAME').AsString+' ');
  if aAddress[aAddress.Count-1] = ' ' then aAddress[aAddress.Count-1] := '';
  aAddress[aAddress.Count-1] := aAddress[aAddress.Count-1]+DataSet.FieldbyName('NAME').AsString;
  if DataSet.FieldbyName('ADDITIONAL').AsString <> '' then
    aAddress.Add(DataSet.FieldbyName('ADDITIONAL').AsString);
  aAddress.Add(DataSet.FieldbyName('ADDRESS').AsString);
  aAddress.Add(DataSet.FieldbyName('COUNTRY').AsString+' ');
  if aAddress[aAddress.Count-1] = ' ' then aAddress[aAddress.Count-1] := '';
  aAddress[aAddress.Count-1] := aAddress[aAddress.Count-1]+DataSet.FieldbyName('ZIP').AsString+' ';
  if aAddress[aAddress.Count-1] = ' ' then aAddress[aAddress.Count-1] := '';
  aAddress[aAddress.Count-1] := aAddress[aAddress.Count-1]+DataSet.FieldbyName('CITY').AsString+' ';
  while (aAddress.Count > 0) and (trim(aAddress[aAddress.Count-1]) = '') do aAddress.Delete(aAddress.Count-1);
  Result := aAddress.Text;
  aAddress.Free;
end;

procedure TBaseDbAddress.OpenItem(AccHistory: Boolean);
begin
  //Do nothing
end;

procedure TBaseDbAddress.FromString(aStr: AnsiString);
var
  Addr: TStringList;
  tmp: String;
  i: Integer;
  tmp1: String;
  AktTitle : string;
  function CountPos(const subtext: string; Text: string): Integer;
  begin
    if (Length(subtext) = 0) or (Length(Text) = 0) or (Pos(subtext, Text) = 0) then
      Result := 0
    else
      Result := (Length(Text) - Length(StringReplace(Text, subtext, '', [rfReplaceAll]))) div
        Length(subtext);
  end;
  function HasTitle(aTitle : string) : Boolean;
  begin
    Result := False;
    if (lowercase(copy(Addr[0],0,length(aTitle)+1)) = aTitle+#10) then
      begin
        AktTitle:=copy(Addr[0],0,Length(aTitle));
        Addr[0]:=copy(Addr[0],pos(#10,Addr[0])+1,length(Addr[0]));
        Result := True;
      end;
    if (lowercase(copy(Addr[0],0,length(aTitle)+1)) = aTitle+#13) then
      begin
        AktTitle:=copy(Addr[0],0,Length(aTitle));
        Addr[0]:=copy(Addr[0],pos(#13,Addr[0])+1,length(Addr[0]));
        Result := True;
      end;
    if (lowercase(copy(Addr[0],0,length(aTitle)+1)) = aTitle+' ') then
      begin
        AktTitle:=copy(Addr[0],0,Length(aTitle));
        Addr[0]:=copy(Addr[0],pos(' ',Addr[0])+1,length(Addr[0]));
        Result := True;
      end;
    if (lowercase(copy(Addr[0],0,length(aTitle)+1)) = aTitle+'.') then
      begin
        AktTitle:=copy(Addr[0],0,Length(aTitle));
        Addr[0]:=copy(Addr[0],pos('.',Addr[0])+1,length(Addr[0]));
        Result := True;
      end;
    if (lowercase(Addr[0]) = aTitle) then
      begin
        AktTitle:=copy(Addr[0],0,Length(aTitle));
        Addr[0]:='';
        Result := True;
      end;
  end;
begin
  Addr := TStringList.Create;
  tmp := StringReplace(aStr,',',lineending,[rfReplaceAll]);
  Addr.Text := tmp;
  if (Addr.Count = 0) then exit;
  //Delete clear lines
  i := 0;
  while i < Addr.Count do
    if Addr[i] = '' then
      Addr.Delete(i)
    else
      inc(i);
  if (Addr.Count = 0) then
    begin
      Addr.Free;
      exit;
    end;
  //Check and Remove for Contact propertys
  i := 0;
  while i < Addr.Count do
    begin
      if (pos('tel ',lowercase(Addr[i])) > 0)
      or (pos('phone ',lowercase(Addr[i])) > 0)
      or (pos('mobile ',lowercase(Addr[i])) > 0)
      or (pos('tel:',lowercase(Addr[i])) > 0)
      or (pos('phone:',lowercase(Addr[i])) > 0)
      or (pos('mobile:',lowercase(Addr[i])) > 0)
      then
        begin
          Addr.Delete(i);
        end
      else if (pos('fax ',lowercase(Addr[i])) > 0)
           or (pos('fax:',lowercase(Addr[i])) > 0) then
        begin
          Addr.Delete(i);
        end
      else if (pos('mail ',lowercase(Addr[i])) > 0)
           or (pos('mail:',lowercase(Addr[i])) > 0)
           then
        begin
          Addr.Delete(i);
        end
      else
        inc(i);
    end;
  //The rest should be the adress
  DataSet.FieldByName('TITLE').Clear;
  if Addr.Count > 0 then
    if HasTitle('firm')
    or HasTitle('firma')
    or HasTitle('herr')
    or HasTitle('frau')
    or HasTitle('mr')
    or HasTitle('dr')
    or HasTitle('prof')
    then
      begin
        DataSet.FieldByName('TITLE').AsString := AktTitle;
        if trim(Addr[0])='' then
          Addr.Delete(0);
      end;
  if Addr.Count > 0 then
    if HasTitle('dr')
    or HasTitle('prof')
    then
      begin
        if trim(Addr[0])='' then
          Addr.Delete(0);
      end;
  if Addr.Count > 0 then
    begin
      DataSet.FieldByName('NAME').AsString := trim(Addr[0]);
      Addr.Delete(0);
    end;
  DataSet.FieldByName('ADDITIONAL').Clear;
  DataSet.FieldByName('CITY').Clear;
  DataSet.FieldByName('ZIP').Clear;
  DataSet.FieldByName('ADDRESS').Clear;
  i := Addr.Count-1;
  if Addr.Count = 0 then exit;
  while i > 0 do
    begin
      tmp := trim(Addr[i]);
      tmp1 := copy(tmp,0,pos(' ',tmp)-1);
      if (length(tmp1)>3) then
        begin
          DataSet.FieldByName('ZIP').AsString := copy(trim(tmp),0,pos(' ',trim(tmp))-1);
          DataSet.FieldByName('CITY').AsString := copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp)));
          Addr.Delete(i);
          break;
        end
      else if (CountPos(' ',tmp) = 2) or (CountPos(' ',tmp) = 3) then
        begin
          DataSet.FieldByName('COUNTRY').AsString := tmp1;
          tmp1 := copy(tmp,pos(' ',tmp)+1,length(tmp));
          DataSet.FieldByName('ZIP').AsString := copy(tmp1,0,pos(' ',tmp1)-1);
          tmp := copy(tmp,pos(DataSet.FieldByName('ZIP').AsString,tmp)+length(DataSet.FieldByName('ZIP').AsString)+1,length(tmp));
          DataSet.FieldByName('CITY').AsString := tmp;
          Addr.Delete(i);
          break;
        end;
      dec(i);
    end;

  DataSet.FieldByName('ADDRESS').AsString := Addr.Text;
  Addr.Free;
end;
procedure TPerson.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if Field.FieldName = 'STATUS' then
    begin
      History.Open;
      History.AddItem(Self.DataSet,Format(strStatusChanged,[FStatus,Field.AsString]),'','',nil,ACICON_STATUSCH);
      FStatus := Field.AsString;
      if Assigned(FStateChange) then
        FStateChange(Self);
    end;
  if (Field.FieldName = 'ACCOUNTNO') then
    begin
      History.AddItem(Self.DataSet,Format(strNumberChanged,[Field.AsString]),'','',DataSet,ACICON_EDITED);
    end;
end;
function TPerson.GetHistory: TBaseHistory;
begin
  Result := History;
end;

function TPerson.GetInfo: TField;
begin
  Result := DataSet.FieldByName('INFO');
end;

function TPerson.GetAccountNo: TField;
begin
  Result := DataSet.FieldByName('ACCOUNTNO');
end;
constructor TPerson.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM,aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=False;
        end;
    end;
  FHistory := TBaseHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FPersonAddress := TPersonAddress.CreateEx(Self,DataModule,aConnection,DataSet);
  FCustomerCont := TPersonContactData.CreateEx(Self,DataModule,aConnection,DataSet);
  FImages := TImages.CreateEx(Self,DataModule,aConnection,DataSet);
  FBanking := TPersonBanking.CreateEx(Self,DataModule,aConnection,DataSet);
  FLinks := TPersonLinks.CreateEx(Self,DataModule,aConnection);
  FEmployees := TPersonEmployees.CreateEx(Self,DataModule,aConnection,DataSet);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;
destructor TPerson.Destroy;
begin
  FreeAndNil(FDS);
  FreeAndNil(FEmployees);
  FreeAndNil(FLinks);
  FreeAndNil(FBanking);
  FreeAndNil(FImages);
  FreeAndNil(FCustomerCont);
  FreeAndNil(FPersonAddress);
  FreeAndNil(FHistory);
  inherited Destroy;
end;

procedure TPerson.Open;
begin
  inherited Open;
  FStatus := Status.AsString;
end;
function TPerson.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FHistory.CreateTable;
  FPersonAddress.CreateTable;
  FCustomerCont.CreateTable;
  FBanking.CreateTable;
  FLinks.CreateTable;
  FEmployees.CreateTable;
end;
procedure TPerson.FillDefaults(aDataSet: TDataSet);
var
  Languages: TLanguages;
begin
  Languages := TLanguages.CreateEx(Self,DataModule,Connection);
  Languages.Open;
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      aDataSet.DisableControls;
      FieldByName('ACCOUNTNO').AsString := Data.Numbers.GetNewNumber('CUSTOMERS');
      FieldByName('TYPE').AsString      := 'C';
      FieldByName('DISCOUNT').AsFloat   := 0;
      FieldByName('TREEENTRY').AsVariant := TREE_ID_CUSTOMER_UNSORTED;
      FieldByName('CRDATE').AsDateTime  := Date;
      FieldByName('CREATEDBY').AsString := Data.Users.IDCode.AsString;
      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
      if Data.Currency.DataSet.Active and Data.Currency.DataSet.Locate('DEFAULTCUR', 'Y', []) then
        FieldByName('CURRENCY').AsString := Data.Currency.FieldByName('SYMBOL').AsString;
      if Languages.DataSet.Active and Languages.DataSet.Locate('DEFAULTLNG', 'Y', []) then
        FieldByName('LANGUAGE').AsString := Languages.FieldByName('ISO6391').AsString;
      aDataSet.EnableControls;
    end;
  Languages.Free;
end;
function TPerson.Find(aIdent: string;Unsharp : Boolean = False): Boolean;
begin
  with DataSet as IBaseDbFilter,BaseApplication as IBaseDbInterface do
    Filter := '('+Data.QuoteField(GetNumberFieldName)+'='+Data.QuoteValue(aIdent)+') OR ('+Data.QuoteField(GetTextFieldName)+'='+Data.QuoteValue(aIdent)+')';
  Open;
  Result := Count > 0;
  if (not Result) and Unsharp then
    begin
      with DataSet as IBaseDbFilter,BaseApplication as IBaseDbInterface do
        Filter := '('+Data.ProcessTerm(Data.QuoteField(GetNumberFieldName)+'='+Data.QuoteValue(aIdent+'*'))+') OR ('+Data.ProcessTerm(Data.QuoteField(GetTextFieldName)+'='+Data.QuoteValue(aIdent+'*'))+')';
      Open;
      Result := Count > 0;
    end;
end;
function TPersonList.SelectFromLink(aLink: string): Boolean;
begin
  Result := False;
  if (not (copy(aLink,0,pos('@',aLink)-1) = 'CUSTOMERS'))
  and (not (copy(aLink,0,pos('@',aLink)-1) = 'CUSTOMERS.ID')) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  if (copy(aLink,0,pos('@',aLink)-1) = 'CUSTOMERS') then
    begin
      with DataSet as IBaseDBFilter do
        Filter := Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink)));
      Result := True;
    end
  else
    begin
      Select(copy(aLink,pos('@',aLink)+1,length(aLink)));
      Result := True;
    end;
  if not Result then
    Result := inherited SelectFromLink(aLink);
end;

function TPerson.SelectFromContactData(aCont: string): Boolean;
var
  CustomerCont: TPersonContactData;
begin
  Result := False;
  try
    CustomerCont := TPersonContactData.Create(nil);
    if Data.IsSQLDb then
      Data.SetFilter(CustomerCont,Data.ProcessTerm('UPPER("DATA")=UPPER('''+aCont+''')',True)+' OR ("DATA" like ''%*%'')')
    else
      Data.SetFilter(CustomerCont,Data.ProcessTerm('"DATA"='''+aCont+''''));
    CustomerCont.First;
    while not CustomerCont.EOF do
      begin
        if comparewild.WildComp(CustomerCont.Data.AsString,aCont) then
          begin
            Self.Filter('"ACCOUNTNO"='+Data.QuoteValue(CustomerCont.DataSet.FieldByName('ACCOUNTNO').AsString));
            Result:=True;
            break;
          end;
        CustomerCont.Next;
      end;
  except
  end;
  CustomerCont.Free;
end;

procedure TPerson.GenerateThumbnail;
var
  aThumbnail: TThumbnails;
begin
  aThumbnail := TThumbnails.CreateEx(nil,DataModule);
  aThumbnail.CreateTable;
  aThumbnail.SelectByRefId(Self.Id.AsVariant);
  aThumbnail.Open;
  if aThumbnail.Count=0 then
    Images.GenerateThumbnail(aThumbnail);
  aThumbnail.Free;
end;

procedure TPerson.CascadicPost;
begin
  FHistory.CascadicPost;
  FPersonAddress.CascadicPost;
  FCustomerCont.CascadicPost;
  FImages.CascadicPost;
  FBanking.CascadicPost;
  FLinks.CascadicPost;
  inherited CascadicPost;
end;
procedure TPerson.CascadicCancel;
begin
  FHistory.CascadicCancel;
  FPersonAddress.CascadicCancel;
  FCustomerCont.CascadicCancel;
  FImages.CascadicCancel;
  FBanking.CascadicCancel;
  FLinks.CascadicCancel;
  inherited CascadicCancel;
end;

function TPersonList.GetMatchCodeFieldName: string;
begin
  Result:='MATCHCODE';
end;
function TPersonList.GetTextFieldName: string;
begin
  Result:='NAME';
end;
function TPersonList.GetNumberFieldName: string;
begin
  Result:='ACCOUNTNO';
end;
function TPersonList.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;

function TPersonList.GetTyp: string;
begin
  Result := 'C';
end;

constructor TPersonList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=True;
        end;
    end;
end;
procedure TPersonList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CUSTOMERS';
      TableCaption := strCustomers;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACCOUNTNO',ftString,20,True);
            Add('MATCHCODE',ftString,200,False);
            Add('STATUS',ftString,4,false);
            Add('NAME',ftString,200,True);
            Add('TREEENTRY',ftLargeInt,0,True);
            Add('DISCOUNT',ftFloat,0,False);
            Add('DISCOUNTGR',ftString,2,False);
            Add('DEFPRICE',ftString,2,False);
            Add('LANGUAGE',ftString,3,False);
            Add('CURRENCY',ftString,5,False);
            Add('EACCOUNT',ftString,20,False);//Remote Accountno
            Add('PAYMENTTAR',ftString,2,False);
            Add('TYPE',ftString,1,True);
            Add('INFO',ftMemo,0,False);
            Add('CATEGORY',ftString,60,False);
            Add('OLD_ID',ftString,200,False);
            Add('CRDATE',ftDate,0,False);
            Add('CHDATE',ftDate,0,False);
            Add('CREATEDBY',ftString,4,True);
            Add('CHANGEDBY',ftString,4,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ACCOUNTNO','ACCOUNTNO',[ixUnique]);
            Add('MATCHCODE','MATCHCODE',[]);
            Add('NAME','NAME',[]);
            Add('STATUS','STATUS',[]);
          end;
      if Data.ShouldCheckTable(TableName) then
        DefineUserFields(aDataSet);
    end;
end;
procedure TPersonList.SelectByAccountNo(aAccountNo: string);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := Data.ProcessTerm(Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(aAccountNo));
        end;
    end;
end;

function TPersonList.CombineItems(aRemoteLink: string): Boolean;
var
  aClass: TBaseDBDatasetClass;
  aObject: TBaseDBDataset;
begin
  Result := True;
  if TBaseDBModule(DataModule).DataSetFromLink(aRemoteLink,aClass) then
    begin
      aObject := aClass.CreateEx(nil,DataModule);
      if not (aObject is TPersonList) then
        begin
          aObject.Free;
          exit;
        end;
      TBaseDbList(aObject).SelectFromLink(aRemoteLink);
      aObject.Open;
      if aObject.Count>0 then
        begin
          with TPerson(aObject).Address do
            begin
              Open;
              while not EOF do
                begin
                  Edit;
                  FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
                  Post;
                  Next;
                end;
            end;
          with TPerson(aObject).ContactData do
            begin
              Open;
              while not EOF do
                begin
                  Edit;
                  FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
                  Post;
                  Next;
                end;
            end;
          with TPerson(aObject).Banking do
            begin
              Open;
              while not EOF do
                begin
                  Edit;
                  FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
                  Post;
                  Next;
                end;
            end;
          with TPerson(aObject).Images do
            begin
              Open;
              while not EOF do
                begin
                  Edit;
                  FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
                  Post;
                  Next;
                end;
            end;
          with TPerson(aObject).History do
            begin
              Open;
              while not EOF do
                begin
                  Edit;
                  FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
                  Post;
                  Next;
                end;
            end;
        end;
      aObject.Free;
    end;
  Result:=Result and inherited CombineItems(aRemoteLink);
end;

initialization
  RegisterdataSetClass('PERSONS',TPerson);
end.
