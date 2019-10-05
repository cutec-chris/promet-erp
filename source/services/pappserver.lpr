program pappserver;
uses ubasedbclasses,mORMot,mORMotDB;

var
  Model: TSQLModel;
begin
  Model := TSQLModel.Create([TUser]);

end.

