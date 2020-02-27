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
  Classes, SysUtils, db, uBaseDbClasses, uBaseERPDBClasses, uIntfStrConsts, uBaseDatasetInterfaces2;
type

  { TPersonList }

  TPersonList = class(TBaseERPList)
  private
    FAccountNo,FMatchCode,FName,FDiscountGr,FDefPrice,FLanguage,FCurrency,FEAccount,
    FPaymentTar,FType,FInfo,FCategory,FCreatedBy,FChangedBy,FStatus,FOld_ID : string;
    FTreeEntry : Int64;
    FDiscount : double;
    FCrDate,FCHDate : TDateTime;
  public
    function GetTyp: string; override;
    function SelectFromLink(aLink : string) : Boolean;override;
    //function CombineItems(aRemoteLink: string): Boolean; override;
  published
    property AccountNo: string index 20 read FAccountNo write FAccountNo;
    property MatchCode: string index 200 read FMatchCode write FMatchCode;
    property Status: string index 4 read FStatus write FStatus;
    property Name: string index 200 read FName write FName;
    property TreeEntry: Int64 read FTreeEntry write FTreeEntry;
    property Discount: double read FDiscount write FDiscount;
    property DiscountGr: string index 2 read FDiscountGr write FDiscountGr;
    property DefPrice: string index 2 read FDefPrice write FDefPrice;
    property Language: string index 3 read FLanguage write FLanguage;
    property Currency: string index 5 read FCurrency write FCurrency;
    property EAccount: string index 20 read FEAccount write FEAccount;//Remote Accountno
    property PaymentTar: string index 2 read FPaymentTar write FPaymentTar;
    property Typ: string index 1 read FType write FType;
    property Info: string read FInfo write FInfo;
    property Category: string index 60 read FCategory write FCategory;
    property Old_Id: string index 200 read FOld_Id write FOld_Id;
    property CrDate: TDateTime read FCrDate write FCrDate;
    property ChDate: TDateTime read FChDate write FChDate;
    property CreatedBy: string index 4 read FCreatedBy write FCreatedBy;
    property ChangedBy: string index 4 read FChangedBy write FChangedBy;
  end;

  { TBaseDbAddress }

  TBaseDbAddress = class(TBaseDBList)
  private
    FType,FTitle,FName,FCName,FAdditional,FAddress,FCity,FZip,FState,FCountry : string;
    FPoBox : Integer;
  public
    procedure Assign(Source: TPersistent); override;
    procedure FillDefaults;override;
    function ToString: ansistring;override;
    procedure FromString(aStr : AnsiString);virtual;
    class function MapField(aField: string): string; override;
  published
    property Typ: string index 3 read FType write FType;
    property Title: string index 8 read FTitle write FTitle;
    property Name: string index 200 read FName write FName;
    property CName: string index 40 read FCName write FCName;
    property Additional: string index 200 read FAdditional write FAdditional;
    property Address: string read FAddress write FAddress;
    property City: string index 40 read FCity write FCity;
    property Zip: string index 10 read FZip write FZip;
    property State: string index 50 read FState write FState;
    property Country: string index 3 read FCountry write FCountry;
    property PoBox: Integer read FPoBox write FPoBox;
  end;
  TPerson = class;

  { TPersonAddress }

  TPersonAddress = class(TBaseDBAddress)
  private
    FAddrNo,FDescr : string;
    FActive : Boolean;
  public
    class function GetRealTableName: string; override;
    procedure FillDefaults;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName : string;override;
  published
    property AddrNo: string index 15 read FAddrNo write FAddrNo;
    property Descr: string index 30 read FDescr write FDescr;
    property Active: Boolean read FActive write FActive;
  end;

  { TPersonAddresses }

  TPersonAddresses = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TPersonContactData }

  TPersonContactData = class(TBaseDBList)
  private
    FAccountNo,FDescr,FType,FData,FLink : string;
    Factive : Boolean;
  public
    class function GetRealTableName: string; override;
    procedure FillDefaults;override;
  published
    property AccountNo: string index 20 read FAccountNo write FAccountNo;
    property Descr: string index 30 read FDescr write FDescr;
    property Typ: string index 4 read FType write FType;
    property Data: string index 80 read FData write FData;
    property Link: string index 400 read FLink write FLink;
    property Active: Boolean read FActive write FActive;
  end;

  { TPersonContacts }

  TPersonContacts = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TPersonBanking }

  TPersonBankingAccount = class(TBaseDBDataSet)
  private
    FSortCode,FInstitute,FAccount : string;
  public
    class function GetRealTableName: string; override;
    function CheckAccount : Boolean;
  published
    property SortCode: string index 20 read FSortCode write FSortCode;
    property Account: string index 200 read FAccount write FAccount;
    property Institute: string index 60 read FInstitute write FInstitute;
  end;
  TPersonBanking = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TPersonLinks }

  TPersonLink = class(TLinks)
  public
    procedure FillDefaults;override;
  end;
  TPersonLinks = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TPersonEmployees }

  TPersonEmployee = class(TBaseDbDataSet)
  private
    FName,FDepartment,FPosition,FEmployee : string;
  public
    class function GetRealTableName: string; override;
  published
    property Name: string index 40 read FName write FName;
    property Department: string index 30 read FDepartment write FDepartment;
    property Position: string index 30 read FPosition write FPosition;
    property Employee: string index 20 read FEmployee write FEmployee;
  end;
  TPersonEmployees = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  TPerson = class(TPersonList,IBaseHistory)
  private
    FBanking: TPersonBanking;
    FCustomerCont: TPersonContacts;
    FEmployees: TPersonEmployees;
    FHistory: TBaseHistory;
    FImages: TImages;
    FLinks: TPersonLinks;
    FPersonAddress: TPersonAddresses;
    FDS : TDataSource;
    FStateChange: TNotifyEvent;
    function GetHistory: TBaseHistory;
    procedure SetAccountno(AIndex: Integer; AValue: string);
    procedure SetStatus(AIndex: Integer; AValue: string);
  public
    constructor Create(aOwner : TPersistent);override;
    destructor Destroy;override;
    procedure FillDefaults;override;
    property History : TBaseHistory read FHistory;
    property Images : TImages read FImages;
    property Banking : TPersonBanking read FBanking;
    property Links : TPersonLinks read FLinks;
    property Employees : TPersonEmployees read FEmployees;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
  published
    property Status : string index 4 read FStatus write SetStatus;
    property AccountNo : string index 20 read FAccountNo write SetAccountno;
    property Address : TPersonAddresses read FPersonAddress;
    property ContactData : TPersonContacts read FCustomerCont;
  end;

implementation
uses uData, Utils,
  comparewild;

{ TPersonEmployees }

class function TPersonEmployees.GetObjectTyp: TClass;
begin
  Result := TPersonEmployee;
end;

{ TPersonLinks }

class function TPersonLinks.GetObjectTyp: TClass;
begin
  Result := TPersonLink;
end;

{ TPersonContacts }

class function TPersonContacts.GetObjectTyp: TClass;
begin
  Result := TPersonContactData;
end;

{ TPersonAddresses }

class function TPersonAddresses.GetObjectTyp: TClass;
begin
  Result := TPersonAddress;
end;

{ TPersonEmployees }

class function TPersonEmployee.GetRealTableName: string;
begin
  Result:='EMPLOYEES';
end;

procedure TPersonLink.FillDefaults;
begin
  inherited FillDefaults;
  RRef_ID:=(Parent as TPerson).SQL_ID;
end;

class function TPersonBankingAccount.GetRealTableName: string;
begin
  Result:='CUSTOMERBANKING';
end;

function TPersonBankingAccount.CheckAccount: Boolean;
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

class function TPersonBanking.GetObjectTyp: TClass;
begin
  Result := TPersonBankingAccount;
end;

class function TPersonContactData.GetRealTableName: string;
begin
  Result:='CUSTOMERCONT';
end;

procedure TPersonContactData.FillDefaults;
begin
  AccountNo := (Parent as TPerson).AccountNo;
end;

class function TPersonAddress.GetRealTableName: string;
begin
  Result:='ADDRESSES';
end;

procedure TPersonAddress.FillDefaults;
begin
  inherited FillDefaults;
  //AccountNo := TPerson(Parent).AccountNo;
  //AddrNo := DataSet.RecordCount+1;
  Country := UpperCase(TPerson(Parent).Language);
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

procedure TBaseDbAddress.Assign(Source: TPersistent);
var
  aAddress: TBaseDbAddress;
  Person: TPerson;
begin
{
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
}
    inherited Assign(Source);
end;

procedure TBaseDbAddress.FillDefaults;
begin
  Typ := 'IAD';
{
      if aDataSet.RecordCount = 0 then
        DataSet.FieldByName('TYPE').AsString:='IAD'
      else
        DataSet.FieldByName('TYPE').AsString:='DAD';
}
end;
function TBaseDbAddress.ToString: ansistring;
var
  aAddress : TStringList;
begin
  {
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
  }
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
  {
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
  }
end;

class function TBaseDbAddress.MapField(aField: string): string;
begin
  Result:=inherited MapField(aField);
  if Result = 'Typ' then
    Result := 'TYPE'
  ;
end;

function TPerson.GetHistory: TBaseHistory;
begin
  Result := History;
end;

procedure TPerson.SetAccountno(AIndex: Integer; AValue: string);
begin
  if AValue = FAccountNo then exit;
  FAccountNo := AValue;
  History.AddItem(Self,Format(strNumberChanged,[AValue]),'','',Self,ACICON_EDITED);
end;

procedure TPerson.SetStatus(AIndex: Integer; AValue: string);
begin
  if AValue = FStatus then exit;
  History.AddItem(Self,Format(strStatusChanged,[FStatus,AValue]),'','',nil,ACICON_STATUSCH);
  FStatus := AValue;
  if Assigned(FStateChange) then
    FStateChange(Self);
end;

constructor TPerson.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner);
  FHistory := TBaseHistory.Create(Self);
  FPersonAddress := TPersonAddresses.Create(Self);
  FCustomerCont := TPersonContacts.Create(Self);
  FImages := TImages.Create(Self);
  FBanking := TPersonBanking.Create(Self);
  FLinks := TPersonLinks.Create(Self);
  FEmployees := TPersonEmployees.Create(Self);
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
procedure TPerson.FillDefaults;
var
  Languages: TLanguages;
begin
  {
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
  }
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
      {
      with DataSet as IBaseDBFilter do
        Filter := Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink)));
      Result := True;
      }
    end
  else
    begin
       {
      Select(copy(aLink,pos('@',aLink)+1,length(aLink)));
      Result := True;
      }
    end;
  if not Result then
    Result := inherited SelectFromLink(aLink);
end;

function TPersonList.GetTyp: string;
begin
  Result := 'C';
end;

initialization
  RegisterdataSetClass('PERSONS',TPerson);
end.
