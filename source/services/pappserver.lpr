program pappserver;
uses SynSQLite3,mORMot;
type
  TUser = class(TSQLRecord)
  published
    property Type : ',ftString,1,False);
    Add('PARENT',ftLargeint,0,False);
    Add('ACCOUNTNO',ftString,20,True);
    Add('NAME',ftString,30,True);
    Add('PASSWORD',ftString,45,False);
    Add('SALT',ftString,105,False);
    Add('IDCODE',ftString,4,False);
    Add('EMPLOYMENT',ftDate,0,False);
    Add('LEAVED',ftDate,0,false);
    Add('CUSTOMERNO',ftString,20,false);
    Add('PERSONNELNO',ftString,20,false);
    Add('DEPARTMENT',ftString,30,false);
    Add('POSITION',ftString,30,false);
    Add('LOGINNAME',ftString,30,false);
    Add('EMAIL',ftString,100,false);
    Add('PAYGROUP',ftLargeint,0,false);
    Add('WORKTIME',ftInteger,0,false); //8 wenn NULL
    Add('WEEKWORKTIME',ftInteger,0,false);//40 wenn NULL
    Add('USEWORKTIME',ftInteger,0,false);
    Add('LOGINACTIVE',ftString,1,false);
    Add('REMOTEACCESS',ftString,1,false);
    Add('LASTLOGIN',ftDateTime,0,false);
    Add('AUTHSOURCE',ftString,10,false);
  end;

begin
end.

