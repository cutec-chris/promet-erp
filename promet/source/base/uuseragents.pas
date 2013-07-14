unit uUserAgents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
function OSFromUserAgent(Agent : string) : string;
function AgentFromUserAgent(Agent : string) : string;
function TypeFromUserAgent(Agent : string) : string;
implementation
function OSFromUserAgent(Agent: string): string;
begin
  Result := '';
  if pos('(',Agent) = 0 then exit;
  Agent := Uppercase(Agent);
  if pos('WINDOWS',Agent) > 0 then Result := 'Windows'
  else if pos('IOS',Agent) > 0 then Result := 'iOS'
  else if pos('MAC',Agent) > 0 then Result := 'MacOS'
  else if pos('ANDROID',Agent) > 0 then Result := 'Android'
  else if pos('LINUX',Agent) > 0 then Result := 'Linux'
  ;
end;
function AgentFromUserAgent(Agent: string): string;
begin

end;
function TypeFromUserAgent(Agent: string): string;
begin

end;
end.

