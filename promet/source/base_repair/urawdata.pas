{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uRawdata;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfRawdata }

  TfRawdata = class(TForm)
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fRawdata: TfRawdata;

implementation

{$R *.lfm}

end.
