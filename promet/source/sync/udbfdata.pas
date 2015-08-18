unit udbfdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, FileUtil,  Forms, uBaseDBInterface, uminiconvencoding,
  Dialogs, lclproc;

type

  { TaData }

  TaData = class(TDataModule)
    dbase: TDbf;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  aData: TaData;

implementation
{$R *.lfm}
uses uData,uPerson;

resourcestring
  strFailedtoLoadMandants    = 'Mandanten konnten nicht gelanden werden !';
  strMandantnotSelected      = 'kein Mandant gewählt (--mandant) !';
  strLoginFailed             = 'Login fehlgeschlagen';

{ TaData }

procedure TaData.DataModuleCreate(Sender: TObject);
var
  aConnection: TComponent;
  aPerson: TPerson;
  tmp: String;
  Ansi: Boolean;
  aKey: String;

  function ConvertEncoding(Inp : string) : string;
  var
    a : string;
  begin
    a := LConvEncoding.ConvertEncoding(Inp,GuessEncoding(Inp),EncodingUTF8);
    result := a;
  end;
begin
  with Application,Application as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
      if not HasOption('m','mandant') then
        raise Exception.Create(strMandantnotSelected);
      if not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user')) then
        raise Exception.Create(strLoginFailed);
      uData.Data := Data;
    end;
  if Application.HasOption('d','database') then
    begin
      aConnection := Data.GetNewConnection;
      dbase.FilePathFull:=Application.GetOptionValue('d','database');
      //Kontakte
      dbase.TableName:='Contact1.DBF';
      dbase.Open;
      if dbase.Active then
        begin
          while not dbase.EOF do
            begin
              Data.StartTransaction(aConnection);
              try
              aPerson := TPerson.CreateEx(Self,Data,aConnection);
              aKey := trim(dbase.FieldByName('KEY5').Text);
              Data.SetFilter(aPerson,'"ACCOUNTNO"='+Data.QuoteValue(aKey));
              if aPerson.Count = 0 then
                begin
                  aPerson.DataSet.Insert;
                  if (aKey <> '') and (copy(aKey,0,1) = 'K') then
                    aPerson.Number.AsString:=aKey;
                  aPerson.text.AsString:=ConvertEncoding(dbase.FieldByName('CONTACT').AsString);
                  if trim(aPerson.Text.AsString) = '' then
                    aPerson.Text.AsString:=ConvertEncoding(dbase.FieldByName('COMPANY').AsString);
                  if dbase.FieldByName('ADDRESS1').AsString <> '' then
                    begin
                      aPerson.Address.Open;
                      with aPerson.Address.DataSet do
                       begin
                         Insert;
                         tmp := dbase.FieldByName('ADDRESS1').AsString;
                         if trim(dbase.FieldByName('ADDRESS2').AsString) <> '' then
                           tmp := tmp+lineending+dbase.FieldByName('ADDRESS2').AsString;
                         if trim(dbase.FieldByName('ADDRESS3').AsString) <> '' then
                           tmp := tmp+lineending+dbase.FieldByName('ADDRESS3').AsString;
                         FieldByName('ADDRESS').AsString := ConvertEncoding(tmp);
                         FieldByName('TITLE').AsString := ConvertEncoding(dbase.FieldByName('DEAR').AsString);
                         FieldByName('NAME').AsString := ConvertEncoding(dbase.FieldByName('CONTACT').AsString);
                         FieldByName('CITY').AsString := ConvertEncoding(dbase.FieldByName('CITY').AsString);
                         FieldByName('ZIP').AsString := ConvertEncoding(dbase.FieldByName('ZIP').AsString);
                         FieldByName('STATE').AsString := ConvertEncoding(dbase.FieldByName('STATE').AsString);
                         FieldByName('COUNTRY').AsString := ConvertEncoding(dbase.FieldByName('COUNTRY').AsString);
                         Post;
                       end;
                    end;
                  aPerson.FieldByName('INFO').AsString:=ConvertEncoding(dbase.FieldByName('NOTES').AsString);
                  if trim(dbase.FieldByName('PHONE1').AsString) <> '' then
                    begin
                      aPerson.CustomerCont.Open;
                      with aPerson.CustomerCont.DataSet do
                       begin
                         Insert;
                         FieldByName('TYPE').Text:=ConvertEncoding(dbase.FieldByName('EXT1').AsString);
                         if FieldByName('TYPE').Text = 'Büro' then
                           FieldByName('TYPE').AsString:='TELB';
                         if FieldByName('TYPE').Text = 'Büro1' then
                           FieldByName('TYPE').AsString:='TELB';
                         if FieldByName('TYPE').AsString = 'Privat' then
                           FieldByName('TYPE').AsString:='TELP';
                         if FieldByName('TYPE').AsString = 'Fax' then
                           FieldByName('TYPE').AsString:='FAX';
                         if FieldByName('TYPE').AsString = 'Handy' then
                           FieldByName('TYPE').AsString:='CEL';
                         FieldByName('DATA').Text:=ConvertEncoding(dbase.FieldByName('PHONE1').AsString);
                         Post;
                       end;
                    end;
                  if trim(dbase.FieldByName('PHONE2').AsString) <> '' then
                    begin
                      aPerson.CustomerCont.Open;
                      with aPerson.CustomerCont.DataSet do
                       begin
                         Insert;
                         FieldByName('TYPE').AsString:=ConvertEncoding(dbase.FieldByName('EXT2').AsString);
                         if FieldByName('TYPE').AsString = 'Büro' then
                           FieldByName('TYPE').AsString:='TELB';
                         if FieldByName('TYPE').AsString = 'Büro1' then
                           FieldByName('TYPE').AsString:='TELB';
                         if FieldByName('TYPE').AsString = 'Privat' then
                           FieldByName('TYPE').AsString:='TELP';
                         if FieldByName('TYPE').AsString = 'Fax' then
                           FieldByName('TYPE').AsString:='FAX';
                         if FieldByName('TYPE').AsString = 'Handy' then
                           FieldByName('TYPE').AsString:='CEL';
                         FieldByName('DATA').AsString:=ConvertEncoding(dbase.FieldByName('PHONE2').AsString);
                         Post;
                       end;
                    end;
                  if trim(dbase.FieldByName('PHONE3').AsString) <> '' then
                    begin
                      aPerson.CustomerCont.Open;
                      with aPerson.CustomerCont.DataSet do
                       begin
                         Insert;
                         FieldByName('TYPE').AsString:=ConvertEncoding(dbase.FieldByName('EXT3').AsString);
                         if FieldByName('TYPE').AsString = 'Büro' then
                           FieldByName('TYPE').AsString:='TELB';
                         if FieldByName('TYPE').AsString = 'Büro1' then
                           FieldByName('TYPE').AsString:='TELB';
                         if FieldByName('TYPE').AsString = 'Privat' then
                           FieldByName('TYPE').AsString:='TELP';
                         if FieldByName('TYPE').AsString = 'Fax' then
                           FieldByName('TYPE').AsString:='FAX';
                         if FieldByName('TYPE').AsString = 'Handy' then
                           FieldByName('TYPE').AsString:='CEL';
                         FieldByName('DATA').AsString:=ConvertEncoding(dbase.FieldByName('PHONE3').AsString);
                         Post;
                       end;
                    end;
                  aPerson.DataSet.Post;
                  Data.Commit(aConnection);
                end
              else Data.Rollback(aConnection);
              except
                Data.Rollback(aConnection);
              end;
              aPerson.Free;
              dbase.next;
            end;
        end;
      aConnection.Free;
    end;
  Application.Terminate;
end;

initialization

end.

