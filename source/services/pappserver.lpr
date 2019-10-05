program pappserver;
uses ubasedbclasses,mORMot,mORMotDB;

var
  Model: TSQLModel;
  DB: TSQLRest;
begin
  Model := TSQLModel.Create([TUser]);
  Connection := TSQLDBZEOSConnectionProperties.
  TSQLDBZEOSConnectionProperties.URI(dFirebird,'',
      FIREBIRD_LIB),'','','',' Firebird',true)
  DB := TSQLRestExternalDBCreate(Model,T)
end.

