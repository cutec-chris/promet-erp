//******************************************************************************
//
// Borland Kylix Linux API Interface Unit for
// sane (Scanner Access Now Easy) library Version 1
//                                                                              
// This file declares SANE application interface. See the SANE
// standard for a detailed explanation of the interface.
//
//------------------------------------------------------------------------------
// Original Header-file: sane.h
//
// Unit-Version:  1.0
// Translator:    Christoph Federer
//
// This file may be distributed and/or modified under the terms of the GNU      
// General Public License (GPL) version 2 as published by the Free Software     
// Foundation                                                                   
//
//******************************************************************************
unit sanelib;

interface

{$MODE DELPHI}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$ALIGN 4}
{$MINENUMSIZE 4}

const
  SANE_CURRENT_MAJOR = 1;
  SANE_FALSE	= 0;
  SANE_TRUE =	1;

type
  TSANE_Byte = byte;
  PSANE_Byte = ^TSANE_Byte;
  TSANE_Word = LongInt;
  PSANE_Word = ^TSANE_Word;
  TSANE_Bool = TSANE_Word;
  TSANE_Int = TSANE_Word;
  PSANE_Int = ^TSANE_Int;
  TSANE_Char = AnsiChar;
  TSANE_String = ^TSANE_Char;
  TSANE_String_Const = ^TSANE_Char; //typedef const SANE_Char *SANE_String_Const;
  PSANE_String_Const = ^TSANE_String_Const;
  TSANE_Handle = Pointer;
  PSANE_Handle = ^TSANE_Handle;
  TSANE_Fixed = TSANE_Word;

  //inserted for convenience
  PSANE_BoolArray = ^TSANE_BoolArray;
  TSANE_BoolArray = array[0..0] of TSANE_Bool;

  PSANE_WordArray = ^TSANE_WordArray;
  TSANE_WordArray = array[0..0] of TSANE_Word;

  PSANE_FixedArray = ^TSANE_FixedArray;
  TSANE_FixedArray = array[0..0] of TSANE_Fixed;

  PSANE_String_ConstArray = ^TSANE_String_ConstArray;
  TSANE_String_ConstArray=array[0..0] of TSANE_String_Const;

function SANE_VERSION_CODE(major, minor, build:integer):TSANE_Word;

function SANE_VERSION_MAJOR(Code:integer):integer;
function SANE_VERSION_MINOR(Code:integer):integer;
function SANE_VERSION_BUILD(Code:integer):integer;

const
  SANE_FIXED_SCALE_SHIFT = 16;

function SANE_FIX(v:Double):TSANE_Word;
function SANE_UNFIX(v:integer):Double;

type
  TSANE_Status = (
     SANE_STATUS_GOOD = 0,      // everything A-OK
     SANE_STATUS_UNSUPPORTED,   // operation is not supported
     SANE_STATUS_CANCELLED,     // operation was cancelled
     SANE_STATUS_DEVICE_BUSY,   // device is busy; try again later
     SANE_STATUS_INVAL,         // data is invalid (includes no dev at open)
     SANE_STATUS_EOF,           // no more data available (end-of-file)
     SANE_STATUS_JAMMED,        // document feeder jammed
     SANE_STATUS_NO_DOCS,       // document feeder out of documents
     SANE_STATUS_COVER_OPEN,    // scanner cover is open
     SANE_STATUS_IO_ERROR,      // error during device I/O
     SANE_STATUS_NO_MEM,        // out of memory
     SANE_STATUS_ACCESS_DENIED  // access to resource has been denied
  );

  TSANE_Value_Type = (
    SANE_TYPE_BOOL = 0,
    SANE_TYPE_INT,
    SANE_TYPE_FIXED,
    SANE_TYPE_STRING,
    SANE_TYPE_BUTTON,
    SANE_TYPE_GROUP
  );

  TSANE_Unit = (
    SANE_UNIT_NONE = 0,		// the value is unit-less (e.g., # of scans) 
    SANE_UNIT_PIXEL,		  // value is number of pixels
    SANE_UNIT_BIT,		    // value is number of bits
    SANE_UNIT_MM,		      // value is millimeters
    SANE_UNIT_DPI,		    // value is resolution in dots/inch
    SANE_UNIT_PERCENT,		// value is a percentage
    SANE_UNIT_MICROSECOND	// value is micro seconds
  );
  PSANE_Device = ^TSANE_Device;
  TSANE_Device = record
     name: TSANE_String_Const;    // unique device name
     vendor: TSANE_String_Const;  // device vendor string
     model: TSANE_String_Const;	  // device model name
     dev_type: TSANE_String_Const;	  // device type (e.g., "flatbed scanner")
   end;
   PPSANE_DeviceArray=^TPSANE_DeviceArray;
   TPSANE_DeviceArray=array[0..0] of PSANE_Device;

const
  SANE_CAP_SOFT_SELECT = 1 shl 0;
  SANE_CAP_HARD_SELECT = 1 shl 1;
  SANE_CAP_SOFT_DETECT = 1 shl 2;
  SANE_CAP_EMULATED = 1 shl 3;
  SANE_CAP_AUTOMATIC = 1 shl 4;
  SANE_CAP_INACTIVE = 1 shl 5;
  SANE_CAP_ADVANCED = 1 shl 6;
  SANE_CAP_ALWAYS_SETTABLE = 1 shl 7;

function SANE_OPTION_IS_ACTIVE(cap:integer):boolean;
function SANE_OPTION_IS_SETTABLE(cap:integer):boolean;

const
  SANE_INFO_INEXACT = 1 shl 0;
  SANE_INFO_RELOAD_OPTIONS = 1 shl 1;
  SANE_INFO_RELOAD_PARAMS = 1 shl 2;

type
  TSANE_Constraint_Type = (
    SANE_CONSTRAINT_NONE = 0,
    SANE_CONSTRAINT_RANGE,
    SANE_CONSTRAINT_WORD_LIST,
    SANE_CONSTRAINT_STRING_LIST
  );

  PSANE_Range = ^TSANE_Range;
  TSANE_Range = record
    min: TSANE_Word;    // minimum (element) value
    max: TSANE_Word;    // maximum (element) value
    quant: TSANE_Word;  // quantization value (0 if none)
  end;

  TSaneConstraint = record
    case constraint_type : TSANE_Constraint_Type of
      SANE_CONSTRAINT_STRING_LIST :
        (string_list: PSANE_String_ConstArray); // NULL-terminated list
      SANE_CONSTRAINT_WORD_LIST :
        (word_list: PSANE_WordArray);           // first element is list-length
      SANE_CONSTRAINT_RANGE :
        (range: PSANE_Range);
  end;

  PSANE_Option_Descriptor = ^TSANE_Option_Descriptor;
  TSANE_Option_Descriptor = record
     name: TSANE_String_Const;   // name of this option (command-line name)
     title: TSANE_String_Const;  // title of this option (single-line)
     desc: TSANE_String_Const;   // description of this option (multi-line)
     option_type: TSANE_Value_Type;    // how are values interpreted?
     option_unit: TSANE_Unit;          // what is the (physical) unit?
     size: TSANE_Int;
     cap: TSANE_Int;             // capabilities
     constraint: TSaneConstraint;
  end;

  TSANE_Action = (
    SANE_ACTION_GET_VALUE = 0,
    SANE_ACTION_SET_VALUE,
    SANE_ACTION_SET_AUTO
  );

  TSANE_Frame = (
    SANE_FRAME_GRAY,	// band covering human visual range
    SANE_FRAME_RGB,		// pixel-interleaved red/green/blue bands
    SANE_FRAME_RED,		// red band only
    SANE_FRAME_GREEN,	// green band only
    SANE_FRAME_BLUE		// blue band only
  );

  PSANE_Parameters = ^TSANE_Parameters;
  TSANE_Parameters = record
     format: TSANE_Frame;
     last_frame: TSANE_Bool;
     bytes_per_line: TSANE_Int;
     pixels_per_line: TSANE_Int;
     lines: TSANE_Int;
     depth: TSANE_Int;
  end;

  TSANE_Auth_Data = record end; // struct SANE_Auth_Data;

const
  SANE_MAX_USERNAME_LEN = 128;
  SANE_MAX_PASSWORD_LEN = 128;

type
  TSANE_USERNAME_ARRAY = array[0..SANE_MAX_USERNAME_LEN-1] of TSANE_Char;
  TSANE_PASSWORD_ARRAY = array[0..SANE_MAX_PASSWORD_LEN-1] of TSANE_Char;
  TSANE_Auth_Callback = procedure (
        resource: TSANE_String_Const;
        username:  TSANE_USERNAME_ARRAY;
        password:  TSANE_PASSWORD_ARRAY
     ); cdecl;


function sane_init(version_code:PSANE_Int; authorize:TSANE_Auth_Callback=nil):TSANE_Status; cdecl;
procedure sane_exit; cdecl;
function sane_get_devices(var device_list:PPSANE_DeviceArray; local_only:TSANE_Bool=SANE_False):TSANE_Status; cdecl;
function sane_open(devicename: TSANE_String_Const; var handle: TSANE_Handle):TSANE_Status; cdecl;
procedure sane_close(handle:TSANE_Handle); cdecl;
function sane_get_option_descriptor(handle:TSANE_Handle; option:TSANE_Int):PSANE_Option_Descriptor; cdecl;
function sane_control_option(handle:TSANE_Handle; option:TSANE_Int;
           action: TSANE_Action; value:Pointer; info:PSANE_Int=nil):TSANE_Status; cdecl;
function sane_get_parameters(handle:TSANE_Handle; var params:TSANE_Parameters):TSANE_Status; cdecl;
function sane_start(handle:TSANE_Handle):TSANE_Status; cdecl;
function sane_read(handle:TSANE_Handle; data:PSANE_Byte; max_length:TSANE_Int; var length:TSANE_Int):TSANE_Status; cdecl;
procedure sane_cancel(handle:TSANE_Handle); cdecl;
function sane_set_io_mode(handle:TSANE_Handle; non_blocking:TSANE_Bool):TSANE_Status; cdecl;
function sane_get_select_fd(handle:TSANE_Handle; fd:PSANE_Int):TSANE_Status; cdecl;
function sane_strstatus(status:TSANE_Status):TSANE_String_Const; cdecl;

const
  sane_lib='libsane.so.1';

implementation

// macros
function SANE_VERSION_CODE(major, minor, build:integer):TSANE_Word;
begin
  result:=((major and $ff) shl 24) or
     ((minor and $ff) shl 16) or
     (build and $ff);
end;

function SANE_VERSION_MAJOR(Code:integer):integer;
begin
  result:=(Code shr 24) and $ff;
end;

function SANE_VERSION_MINOR(Code:integer):integer;
begin
  result:=(Code shr 16) and $ff;
end;

function SANE_VERSION_BUILD(Code:integer):integer;
begin
  result:=Code and $ffff;
end;

function SANE_FIX(v:Double):TSANE_Word;
begin
  result:=round(v * (1 shl SANE_FIXED_SCALE_SHIFT));
end;

function SANE_UNFIX(v:Integer):Double;
begin
  result:=v / (1 shl SANE_FIXED_SCALE_SHIFT);
end;

function SANE_OPTION_IS_ACTIVE(cap:integer):boolean;
begin
  result:=(cap and SANE_CAP_INACTIVE)=0;
end;

function SANE_OPTION_IS_SETTABLE(cap:integer):boolean;
begin
  result:=(cap and SANE_CAP_SOFT_SELECT)<>0;
end;

// external functions
function sane_init; external sane_lib name 'sane_init';
procedure sane_exit; external sane_lib name 'sane_exit';
function sane_get_devices; external sane_lib name 'sane_get_devices';
function sane_open(devicename: TSANE_String_Const; var handle: TSANE_Handle):TSANE_Status; external sane_lib name 'sane_open';
procedure sane_close; external sane_lib name 'sane_close';
function sane_get_option_descriptor; external sane_lib name 'sane_get_option_descriptor';
function sane_control_option; external sane_lib name 'sane_control_option';
function sane_get_parameters; external sane_lib name 'sane_get_parameters';
function sane_start; external sane_lib name 'sane_start';
function sane_read; external sane_lib name 'sane_read';
procedure sane_cancel; external sane_lib name 'sane_cancel';
function sane_set_io_mode; external sane_lib name 'sane_set_io_mode';
function sane_get_select_fd; external sane_lib name 'sane_get_select_fd';
function sane_strstatus; external sane_lib name 'sane_strstatus';

end.
