{
   sane - Scanner Access Now Easy.
   Copyright (C) 1997-1999 David Mosberger-Tang and Andreas Beck
   This file is part of the SANE package.

   This file is in the public domain.  You may use and modify it as
   you see fit, as long as this copyright message is included and
   that there is an indication as to what modifications have been
   made (if any).

   SANE is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.

   This file declares SANE application interface.  See the SANE
   standard for a detailed explanation of the interface.

   sane.h converted from C by Malcolm Poole August 2008
   }


{
   For documentation libsane API documentation see
   http://www.sane-project.org/html/doc009.html
   }

unit Sane;
{$ifdef FPC}
  {$mode objfpc}
  {$PACKRECORDS C}
{$endif}

{$ALIGN 4}
{$MINENUMSIZE 4}

interface

uses
  SysUtils;
  
const
  SaneExport = 'libsane';

// SANE_NAME_ constants from saneopt.h : useful for identifying specific options
{$I saneoptsh.inc}
  
const
  SANE_CURRENT_MAJOR = 1;

const
  SANE_FALSE = 0;
  SANE_TRUE  = 1;
  
type
  SANE_Byte = byte;
  SANE_Word = longint;
  SANE_Bool = SANE_Word;
  SANE_Int = SANE_Word;
  SANE_Char = Ansichar;
  SANE_String = PChar;
  SANE_String_Const = PChar;
  SANE_Handle = Pointer;
  SANE_Fixed = SANE_Word;

  PSANE_Byte = ^SANE_Byte;
  PSANE_Int = ^SANE_Int;
  PSANE_Handle = ^SANE_Handle;

  charArray = array of PChar;
  PcharArray = ^charArray;
  // the address of a callback for user authorisation can be passed as the second parameter of sane_init
  // strings addressed by username and password must be #0 terminated and must not exceed 128 characters
  TSANE_Authorization_Callback = procedure( resource: SANE_String_Const; username, password: PChar ); cdecl;


const
  SANE_FIXED_SCALE_SHIFT = 16;
  
function SANE_VERSION_CODE(major,minor,build : SANE_Word) : longword;
function SANE_VERSION_MAJOR(code : longword) : byte;
function SANE_VERSION_MINOR(code : longword) : byte;
function SANE_VERSION_BUILD(code : longword) : word;

function SANE_FIX( v: double): SANE_Word;
function SANE_UNFIX( v: SANE_Word): double;
  
type
  SANE_Status =
    (
    SANE_STATUS_GOOD = 0,	// everything A-OK
    SANE_STATUS_UNSUPPORTED,	// operation is not supported
    SANE_STATUS_CANCELLED,	// operation was cancelled
    SANE_STATUS_DEVICE_BUSY,	// device is busy; try again later
    SANE_STATUS_INVAL,		// data is invalid (includes no dev at open)
    SANE_STATUS_EOF,		// no more data available (end-of-file)
    SANE_STATUS_JAMMED,		// document feeder jammed
    SANE_STATUS_NO_DOCS,	// document feeder out of documents
    SANE_STATUS_COVER_OPEN,	// scanner cover is open
    SANE_STATUS_IO_ERROR,	// error during device I/O
    SANE_STATUS_NO_MEM,		// out of memory
    SANE_STATUS_ACCESS_DENIED	// access to resource has been denied
  );

  SANE_Value_Type = (
    SANE_TYPE_BOOL = 0,
    SANE_TYPE_INT,
    SANE_TYPE_FIXED,             // use functions SANE_FIX and SANE_UNFIX with values of type SANE_TYPE_FIXED
    SANE_TYPE_STRING,
    SANE_TYPE_BUTTON,
    SANE_TYPE_GROUP
  );

  SANE_Unit = (
    SANE_UNIT_NONE = 0,		// the value is unit-less (e.g., # of scans)
    SANE_UNIT_PIXEL,		// value is number of pixels
    SANE_UNIT_BIT,		// value is number of bits
    SANE_UNIT_MM,		// value is millimeters
    SANE_UNIT_DPI,		// value is resolution in dots/inch
    SANE_UNIT_PERCENT,		// value is a percentage
    SANE_UNIT_MICROSECOND	// value is micro seconds
  );

  SANE_Device = record
    name : SANE_String_Const;
    vendor :SANE_String_Const;
    model: SANE_String_Const;
    dev_type: SANE_String_Const;
    end;

  PSANE_Device = ^SANE_Device;
  DeviceArray = array of PSANE_Device;
  PDeviceArray = ^DeviceArray;

const
  SANE_CAP_SOFT_SELECT        = 1 shl 0;
  SANE_CAP_HARD_SELECT        = 1 shl 1;
  SANE_CAP_SOFT_DETECT	      = 1 shl 2;
  SANE_CAP_EMULATED	      = 1 shl 3;
  SANE_CAP_AUTOMATIC	      = 1 shl 4;
  SANE_CAP_INACTIVE           = 1 shl 5;
  SANE_CAP_ADVANCED           = 1 shl 6;
  SANE_CAP_ALWAYS_SETTABLE    = 1 shl 7;
  
function SANE_OPTION_IS_ACTIVE( cap: SANE_Int ): Boolean;
function SANE_OPTION_IS_SETTABLE( cap: SANE_Int ): Boolean;


const
  SANE_INFO_INEXACT	      =	 1 shl 0;
  SANE_INFO_RELOAD_OPTIONS    =  1 shl 1;
  SANE_INFO_RELOAD_PARAMS     =  1 shl 2;


type
  SANE_Constraint_Type =
    (
    SANE_CONSTRAINT_NONE = 0,
    SANE_CONSTRAINT_RANGE,
    SANE_CONSTRAINT_WORD_LIST,
    SANE_CONSTRAINT_STRING_LIST
  );

  pSANE_Range = ^SANE_Range;
  SANE_Range =  record
    min: SANE_Word;		// minimum (element) value
    max: SANE_Word;		// maximum (element) value
    quant: SANE_Word;		// quantization value (0 if none)
    end;

  PSANE_wordlist = ^SANE_wordlist;
  SANE_wordlist = array of SANE_Word;

  SANE_Option_Descriptor = record
      name: SANE_String_Const;	// name of this option (command-line name)
      title: SANE_String_Const;	// title of this option (single-line)
      desc: SANE_String_Const;	// description of this option (multi-line)
      option_type: SANE_Value_Type;	// how are values interpreted?
      option_unit: SANE_Unit;		// what is the (physical) unit?
      size: SANE_Int;
      cap: SANE_Int;		// capabilities
      case constraint_type: SANE_Constraint_Type of
           SANE_CONSTRAINT_NONE :       ({none});
           SANE_CONSTRAINT_RANGE:       (prange :pSANE_Range);         // SANE_Range
           SANE_CONSTRAINT_WORD_LIST:   (pwordlist: PSANE_wordlist);   // array of SANE_Word: first element is list-length
           SANE_CONSTRAINT_STRING_LIST: (pstringlist: PcharArray);     // NULL-terminated array of PChar
      end;

  PSANE_Option_Descriptor = ^SANE_Option_Descriptor;

  SANE_Action =
    (
    SANE_ACTION_GET_VALUE = 0,
    SANE_ACTION_SET_VALUE,
    SANE_ACTION_SET_AUTO
  );

  SANE_Frame =
    (
    SANE_FRAME_GRAY = 0,	// band covering human visual range
    SANE_FRAME_RGB,             // pixel-interleaved red/green/blue bands
    SANE_FRAME_RED,		// red band only
    SANE_FRAME_GREEN,		// green band only
    SANE_FRAME_BLUE		// blue band only
  );

type
  SANE_Parameters = record
    format: SANE_Frame;
    last_frame: SANE_Bool;
    bytes_per_line : SANE_Int;
    pixels_per_line : SANE_Int;
    lines: SANE_Int;
    depth: SANE_Int;
    end;
    
  pSANE_Parameters = ^SANE_Parameters;
  TSANE_Parameters = SANE_Parameters;

type
  TSANE_Status = SANE_Status;
  TSANE_Word = SANE_Word;
  TSANE_UNIT = SANE_Unit;
  TSANE_Constraint_Type = SANE_Constraint_Type;
  TSANE_Range = SANE_Range;
  TSANE_Frame = SANE_Frame;
  TSANE_Handle = SANE_Handle;
  TSANE_Bool = SANE_Bool;
  TSANE_Fixed = SANE_Fixed;
  TSANE_Int = SANE_Int;
  TSANE_String = SANE_String;
  TSANE_String_Const = SANE_String_Const;
  PPSANE_DeviceArray = ^PDeviceArray;

const
  SANE_MAX_USERNAME_LEN	= 128;
  SANE_MAX_PASSWORD_LEN	= 128;

type
  SANE_Auth_Callback = procedure(resource: SANE_String_Const; username, password: PChar) of object;

  function sane_init (version_code: PSANE_Int; authorize: Pointer = nil): SANE_Status; cdecl; external SaneExport;
  procedure sane_exit;  cdecl; external SaneExport;
  function sane_get_devices (var devices: PPSANE_DeviceArray; local_only: SANE_Bool): SANE_Status; cdecl; external SaneExport;
  function sane_open ( devicename: PChar; handle: PSANE_Handle): SANE_Status; cdecl; external SaneExport;
  procedure sane_close (handle: SANE_Handle); cdecl; external SaneExport;
  function sane_get_option_descriptor (handle: SANE_Handle; option: SANE_Int): Pointer cdecl; external SaneExport;
  function sane_control_option (handle: SANE_Handle; option: SANE_Int;
  					action: SANE_Action; value: Pointer;
  					info: pSANE_Int): SANE_Status; cdecl; external SaneExport;
  function sane_get_parameters ( handle: SANE_Handle; params: pSANE_Parameters ): SANE_Status; cdecl; external SaneExport;
  function sane_start (handle: SANE_Handle): SANE_Status; cdecl; external SaneExport;
  function sane_read ( handle: SANE_Handle; data: PChar; max_length: SANE_Int; length: pSANE_Int): SANE_Status; cdecl; external SaneExport;
  procedure sane_cancel ( handle: SANE_Handle ); cdecl; external SaneExport;
  function sane_set_io_mode ( handle: SANE_Handle; non_blocking: SANE_Bool): SANE_Status; cdecl; external SaneExport;
  function sane_get_select_fd ( handle: SANE_Handle; fd: PSANE_Int): SANE_Status; cdecl; external SaneExport;
  function sane_strstatus (status: SANE_Status): Pchar; cdecl; external SaneExport;

implementation

  function SANE_VERSION_CODE(major,minor,build : SANE_Word) : longword;
    begin
{$RANGECHECKS OFF}
       Result := (((major and $ff) shl 24) or ((minor and $ff) shl 16) or ((build and $ffff) shl 0));
{$RANGECHECKS ON}
    end;

  function SANE_VERSION_MAJOR(code : longword) : byte;
    begin
       SANE_VERSION_MAJOR := (code shr 24) and $ff;
    end;

  function SANE_VERSION_MINOR(code : longword) : byte;
    begin
       SANE_VERSION_MINOR := (code shr 16) and $ff;
    end;

  function SANE_VERSION_BUILD(code : longword) : word;
    begin
       SANE_VERSION_BUILD := (code shr 0) and $ffff;
    end;


function SANE_FIX ( v: double ) : SANE_Word;
begin
  Result := Trunc(v * (1 shl SANE_FIXED_SCALE_SHIFT));
end;

function SANE_UNFIX(v : SANE_Fixed) : double;
begin
   SANE_UNFIX := (double(v))/(1 shl SANE_FIXED_SCALE_SHIFT);
end;


function SANE_OPTION_IS_ACTIVE(cap : SANE_Int) : Boolean;
begin
   SANE_OPTION_IS_ACTIVE := ((cap and SANE_CAP_INACTIVE) = 0);
end;

function SANE_OPTION_IS_SETTABLE(cap : SANE_Int) : Boolean;
begin
   SANE_OPTION_IS_SETTABLE := ((cap and SANE_CAP_SOFT_SELECT) <> 0);
end;


end.

