unit libVLC;

{ VideoLAN libvcl.dll (0.8.6b) Interface for Delphi (c)2007 by Paul TOTH
  - Modified by Norbert Mereg     libvcl.dll (0.8.6i)                       }

// http://wiki.videolan.org/ExternalAPI#VLC_Control

interface


const
 LibName = 'libvlc.so.0.0.8';

// Structures
type
  libvlc_exception = record
    Code    : integer;
    Message : pchar;
  end;

 libvlc_instance = pointer;
 libvlc_input    = pointer;

{$IFDEF STATIC}

// Core
function libvlc_new(argc:integer; args:ppchar; var exception:libvlc_exception):libvlc_instance; cdecl external lib;
procedure libvlc_destroy(vlc:libvlc_instance); cdecl external lib;
procedure libvlc_exception_clear(var exception:libvlc_exception); cdecl external lib;

// Playlist
function libvlc_playlist_add(vlc:libvlc_instance; fileName,name:pchar; var exception:libvlc_exception):integer; cdecl external lib;
function libvlc_playlist_add_extended(vlc:libvlc_instance; fileName,name:pchar; optCount:integer; opts:ppchar; var exception:libvlc_exception):integer; cdecl external lib;
procedure libvlc_playlist_clear(vlc:libvlc_instance; var exception:libvlc_exception); cdecl external lib;
function libvlc_playlist_items_count(vlc:libvlc_instance; var exception:libvlc_exception):integer; cdecl external lib;
function libvlc_playlist_isplaying(vlc:libvlc_instance; var exception:libvlc_exception):longbool; cdecl external lib;
procedure libvlc_playlist_play(vlc:libvlc_instance; index,optCount:integer; opts:ppchar; var exception:libvlc_exception); cdecl external lib;
procedure libvlc_playlist_pause(vlc:libvlc_instance; var exception:libvlc_exception); cdecl external lib;
procedure libvlc_playlist_stop(vlc:libvlc_instance; var exception:libvlc_exception); cdecl external lib;
procedure libvlc_playlist_next(vlc:libvlc_instance; var exception:libvlc_exception); cdecl external lib;
procedure libvlc_playlist_prev(vlc:libvlc_instance; var exception:libvlc_exception); cdecl external lib;
function libvlc_playlist_get_input(vlc:libvlc_instance; var exception:libvlc_exception):libvlc_input; cdecl external lib;

// Input
procedure libvlc_input_free(input:libvlc_input); cdecl external lib;
function libvlc_input_get_length(input:libvlc_input; var exception:libvlc_exception):int64; cdecl external lib;
function libvlc_input_get_time(input:libvlc_input; var exception:libvlc_exception):int64; cdecl external lib;
function libvlc_input_get_position(input:libvlc_input; var exception:libvlc_exception):single; cdecl external lib;
procedure libvlc_toggle_fullscreen(input:libvlc_input; var exception:libvlc_exception); cdecl external lib;
procedure libvlc_set_fullscreen(input:libvlc_input; var exception:libvlc_exception); cdecl external lib;
function libvlc_get_fullscreen(input:libvlc_input; var exception:libvlc_exception):longbool; cdecl external lib;

// Video
function libvlc_video_get_width(input:libvlc_input; var exception:libvlc_exception):integer; cdecl external lib;
function libvlc_video_get_height(input:libvlc_input; var exception:libvlc_exception):integer; cdecl external lib;

// Audio
function libvlc_audio_get_mute(vlc:libvlc_instance; var exception:libvlc_exception):longbool; cdecl external lib;
procedure libvlc_audio_set_mute(vlc:libvlc_instance; mute:longbool; var exception:libvlc_exception); cdecl external lib;
function libvlc_audio_get_volume(vlc:libvlc_instance; var exception:libvlc_exception):integer; cdecl external lib;
procedure libvlc_audio_set_volume(vlc:libvlc_instance; volume:integer; var exception:libvlc_exception); cdecl external lib;

//Other
procedure libvlc_video_set_parent(vlc:libvlc_instance; libvlc_drawable_t:integer; var exception:libvlc_exception); cdecl external lib;
//function libvlc_video_get_parent(vlc:libvlc_instance; var exception:libvlc_exception):integer; cdecl external lib;
 {$ELSE}

var

// Core
 libvlc_new:function(argc:integer; args:ppchar; var exception:libvlc_exception):libvlc_instance; cdecl;
 libvlc_destroy:procedure(vlc:libvlc_instance); cdecl;
 libvlc_exception_clear:procedure(var exception:libvlc_exception); cdecl;
// Playlist
 libvlc_playlist_add:function(vlc:libvlc_instance; fileName,name:pchar; var exception:libvlc_exception):integer; cdecl;
 libvlc_playlist_add_extended:function(vlc:libvlc_instance; fileName,name:pchar; optCount:integer; opts:ppchar; var exception:libvlc_exception):integer; cdecl;
 libvlc_playlist_clear:procedure(vlc:libvlc_instance; var exception:libvlc_exception); cdecl;
 libvlc_playlist_items_count:function(vlc:libvlc_instance; var exception:libvlc_exception):integer; cdecl;
 libvlc_playlist_isplaying:function(vlc:libvlc_instance; var exception:libvlc_exception):longbool; cdecl;
 libvlc_playlist_play:procedure(vlc:libvlc_instance; index,optCount:integer; opts:ppchar; var exception:libvlc_exception); cdecl;
 libvlc_playlist_pause:procedure(vlc:libvlc_instance; var exception:libvlc_exception); cdecl;
 libvlc_playlist_stop:procedure(vlc:libvlc_instance; var exception:libvlc_exception); cdecl;
 libvlc_playlist_next:procedure(vlc:libvlc_instance; var exception:libvlc_exception); cdecl;
 libvlc_playlist_prev:procedure(vlc:libvlc_instance; var exception:libvlc_exception); cdecl;
 libvlc_playlist_get_input:function(vlc:libvlc_instance; var exception:libvlc_exception):libvlc_input; cdecl;
 // Input (Vout)
 libvlc_input_free:procedure(input:libvlc_input); cdecl;
 libvlc_input_get_length:function(input:libvlc_input; var exception:libvlc_exception):int64; cdecl;
 libvlc_input_get_time:function(input:libvlc_input; var exception:libvlc_exception):int64; cdecl;
 libvlc_input_get_position:function(input:libvlc_input; var exception:libvlc_exception):single; cdecl;
 libvlc_toggle_fullscreen:procedure(input:libvlc_input; var exception:libvlc_exception); cdecl;
 libvlc_set_fullscreen:procedure(input:libvlc_input; var exception:libvlc_exception); cdecl;
 libvlc_get_fullscreen:function(input:libvlc_input; var exception:libvlc_exception):longbool; cdecl;
 // audio
 libvlc_video_get_width:function(input:libvlc_input; var exception:libvlc_exception):integer; cdecl;
 libvlc_video_get_height:function(input:libvlc_input; var exception:libvlc_exception):integer; cdecl;
 // Audio
 libvlc_audio_get_mute:function(vlc:libvlc_instance; var exception:libvlc_exception):longbool; cdecl;
 libvlc_audio_set_mute:procedure(vlc:libvlc_instance; mute:longbool; var exception:libvlc_exception); cdecl;
 libvlc_audio_get_volume:function(vlc:libvlc_instance; var exception:libvlc_exception):integer; cdecl;
 libvlc_audio_set_volume:procedure(vlc:libvlc_instance; volume:integer; var exception:libvlc_exception); cdecl;

 //Other
 libvlc_video_set_parent:procedure(vlc:libvlc_instance; libvlc_drawable_t:integer; var exception:libvlc_exception); cdecl;
 libvlc_video_get_parent:function(vlc:libvlc_instance; var exception:libvlc_exception):integer; cdecl;


const
  VLD_SUCCESS  =  0;
  VLD_NOLIB    = -1;
  VLD_NOTFOUND = -2;

// load libvlc.dll (get Install path from registry)
function VLD_LoadLibrary:integer;
// return Install path found in registry by VLD_LoadLibrary
function VLD_LibPath:string;
// return libvlc.dll proc adress
function VLD_GetProcAddress(Name:pchar; var addr:pointer):integer;
// return (and clear) last VLD error
function VLD_LastError:integer;
// load everything (dll & procs) and return last VLD error
function VLD_Startup:integer;

{$ENDIF}

implementation

{$IFNDEF STATIC}

uses
 dynlibs, LCLType, LCLProc, LCLIntf;
 
var
  LibVLCHandle: THandle = 0;
  LibPath: string;
  LastError: integer = VLD_SUCCESS;
  VLCLibLoaded: boolean = false;

function GetLibPath: boolean;
//var
// Handle: HKEY;
// RegType: integer;
// DataSize: integer;
begin
{  Result := False;
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'Software\VideoLAN\VLC', 0, KEY_ALL_ACCESS, Handle) = ERROR_SUCCESS) then
  begin
    if RegQueryValueEx(Handle, 'InstallDir', nil, @RegType, nil, @DataSize) = ERROR_SUCCESS then
    begin
      SetLength(LibPath, Datasize);
      RegQueryValueEx(Handle, 'InstallDir', nil, @RegType, PByte(@LibPath[1]), @DataSize);
      LibPath[DataSize] := '\';
      Result := True;
    end;
    RegCloseKey(Handle);
 end;}
// SetLength(LibPath, Length('/usr/lib/'));
 LibPath:='/usr/lib/';
// LibPath:='/usr/lib/';
 result:=True;
end;

function VLD_LibPath: string;
begin
  if LibPath = '' then
    getLibPath;
  Result := LibPath;
end;

function VLD_LoadLibrary:integer;
begin
  if LibVLCHandle = 0 then
  begin
    LibVLCHandle := LoadLibrary(LibName);
    if (LibVLCHandle = 0) and (getLibPath) then
      LibVLCHandle := LoadLibrary(PChar(LibPath + LibName));
  end;

  if LibVLCHandle <> 0 then
    Result := VLD_SUCCESS
  else
  begin
    LastError := VLD_NOLIB;
    Result := LastError;
  end;
end;

function VLD_GetProcAddress(Name: PChar; var Addr: Pointer): Integer;
begin
  if LibVLCHandle = 0 then
  begin
    Result := VLD_LoadLibrary;
    if Result <> VLD_SUCCESS then exit;
  end;

  Addr := GetProcAddress(LibVLCHandle, Name);
  if Addr <> nil then
    Result := VLD_SUCCESS
  else
  begin
    LastError := VLD_NOTFOUND;
    Result := LastError;
  end;
end;

function VLD_LastError: Integer;
begin
 Result := LastError;
 LastError := VLD_SUCCESS;
end;

function VLD_Startup: Integer;
begin
  LastError := VLD_SUCCESS;
  if VLD_LoadLibrary = VLD_SUCCESS then
  begin
    VLD_GetProcAddress('libvlc_new', @libvlc_new);
    VLD_GetProcAddress('libvlc_destroy', @libvlc_destroy);
    VLD_GetProcAddress('libvlc_exception_clear', @libvlc_exception_clear);
    VLD_GetProcAddress('libvlc_playlist_add', @libvlc_playlist_add);
    VLD_GetProcAddress('libvlc_playlist_add_extended', @libvlc_playlist_add_extended);
    VLD_GetProcAddress('libvlc_playlist_clear', @libvlc_playlist_clear);
    VLD_GetProcAddress('libvlc_playlist_items_count', @libvlc_playlist_items_count);
    VLD_GetProcAddress('libvlc_playlist_isplaying', @libvlc_playlist_isplaying);
    VLD_GetProcAddress('libvlc_playlist_play', @libvlc_playlist_play);
    VLD_GetProcAddress('libvlc_playlist_pause', @libvlc_playlist_pause);
    VLD_GetProcAddress('libvlc_playlist_stop', @libvlc_playlist_stop);
    VLD_GetProcAddress('libvlc_playlist_next', @libvlc_playlist_next);
    VLD_GetProcAddress('libvlc_playlist_prev', @libvlc_playlist_prev);
    VLD_GetProcAddress('libvlc_playlist_get_input', @libvlc_playlist_get_input);
    VLD_GetProcAddress('libvlc_input_free', @libvlc_input_free);
    VLD_GetProcAddress('libvlc_input_get_length', @libvlc_input_get_length);
    VLD_GetProcAddress('libvlc_input_get_time', @libvlc_input_get_time);
    VLD_GetProcAddress('libvlc_input_get_position', @libvlc_input_get_position);
    VLD_GetProcAddress('libvlc_toggle_fullscreen', @libvlc_toggle_fullscreen);
    VLD_GetProcAddress('libvlc_set_fullscreen', @libvlc_set_fullscreen);
    VLD_GetProcAddress('libvlc_get_fullscreen', @libvlc_get_fullscreen);
    VLD_GetProcAddress('libvlc_video_get_width', @libvlc_video_get_width);
    VLD_GetProcAddress('libvlc_video_get_height', @libvlc_video_get_height);
    VLD_GetProcAddress('libvlc_audio_get_mute', @libvlc_audio_get_mute);
    VLD_GetProcAddress('libvlc_audio_set_mute', @libvlc_audio_set_mute);
    VLD_GetProcAddress('libvlc_audio_get_volume', @libvlc_audio_get_volume);
    VLD_GetProcAddress('libvlc_audio_set_volume', @libvlc_audio_set_volume);

    VLD_GetProcAddress('libvlc_video_set_parent', @libvlc_video_set_parent);
//    VLD_GetProcAddress('libvlc_video_get_parent', @libvlc_video_get_parent);
    VLCLibLoaded := true;
  end;
  Result := LastError;
end;
{$ENDIF}

end.

