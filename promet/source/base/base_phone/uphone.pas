unit uPhone;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;
  
function CallPhone(Phonenumber : string) : Boolean;

implementation

{$IFDEF MSWINDOWS}
function tapiRequestMakeCall(DestAddress, AppName, CalledParty, Comment: PChar): Longint; stdcall; external 'TAPI32.DLL';
{$ENDIF}

{$IFDEF MSWINDOWS}
function CallPhone(Phonenumber : string) : Boolean;
var
  aPhoneNumber, AppName: array[0..255] of Char;
begin
  aPhoneNumber := Phonenumber;
  StrPCopy(AppName, Application.Title);
  tapiRequestMakeCall(aPhoneNumber, AppName, '', '');
  Result := True;
end;
{$ELSE}
function CallPhone(Phonenumber : string) : Boolean;
begin
  Result := False;
end;
{$ENDIF}

end.

