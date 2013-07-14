
unit sanelsane;
interface

{
  Automatically converted by H2Pas 1.0.0 from /media/2DF0E8714D527894/Source/prometheus/source/components/scanning/sanelsane.tmp.h
  The following command line parameters were used:
    -e
    -p
    -d
    -D
    -w
    -l
    libsane
    -o
    /media/2DF0E8714D527894/Source/prometheus/source/components/scanning/sanelsane.pas
    /media/2DF0E8714D527894/Source/prometheus/source/components/scanning/sanelsane.tmp.h
}

  const
    External_library='libsane'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  






  Type
  SANE_Action =  Longint;
  PSANE_Action  = ^SANE_Action;
  SANE_Auth_Data = record
        {undefined structure}
      end;
  PSANE_Auth_Data  = ^SANE_Auth_Data;
  SANE_Word = longint;
  SANE_Bool = SANE_Word;
  PSANE_Bool  = ^SANE_Bool;
  SANE_Byte = byte;
  PSANE_Byte  = ^SANE_Byte;
  SANE_Char = char;
  PSANE_Char  = ^SANE_Char;
  SANE_Constraint_Type =  Longint;
  PSANE_Constraint_Type  = ^SANE_Constraint_Type;
  SANE_String_Const = PSANE_Char;
  SANE_Device = record
        name : SANE_String_Const;
        vendor : SANE_String_Const;
        model : SANE_String_Const;
        dev_type : SANE_String_Const;
      end;
  PSANE_Device  = ^SANE_Device;
  PPSANE_Device=^PSANE_Device;
  SANE_Fixed = SANE_Word;
  PSANE_Fixed  = ^SANE_Fixed;
  SANE_Frame =  Longint;
  PSANE_Frame  = ^SANE_Frame;
  SANE_Handle = pointer;
  PSANE_Handle  = ^SANE_Handle;
  SANE_Int = SANE_Word;
  PSANE_Int  = ^SANE_Int;
  SANE_Value_Type =  Longint;
  SANE_Unit =  Longint;
  PSANE_Word  = ^SANE_Word;
  SANE_Range = record
        min : SANE_Word;
        max : SANE_Word;
        quant : SANE_Word;
      end;
  PSANE_Range  = ^SANE_Range;
  PSANE_String_Const  = ^SANE_String_Const;
  SANE_Option_Descriptor = record
        name : SANE_String_Const;
        title : SANE_String_Const;
        desc : SANE_String_Const;
        option_type : SANE_Value_Type;
        option_unit : SANE_Unit;
        size : SANE_Int;
        cap : SANE_Int;
        constraint_type : SANE_Constraint_Type;
        constraint : record
            case longint of
              0 : ( string_list : PSANE_String_Const );
              1 : ( word_list : PSANE_Word );
              2 : ( range : PSANE_Range );
            end;
      end;
  PSANE_Option_Descriptor  = ^SANE_Option_Descriptor;
  SANE_Parameters = record
        format : SANE_Frame;
        last_frame : SANE_Bool;
        bytes_per_line : SANE_Int;
        pixels_per_line : SANE_Int;
        lines : SANE_Int;
        depth : SANE_Int;
      end;
  PSANE_Parameters  = ^SANE_Parameters;
  SANE_Status = (
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

//  SANE_Status =  Longint;
//  PSANE_Status  = ^SANE_Status;
  SANE_String = PSANE_Char;
  PSANE_String  = ^SANE_String;
  PSANE_Unit  = ^SANE_Unit;
  PSANE_Value_Type  = ^SANE_Value_Type;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  { sane - Scanner Access Now Easy.
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
     standard for a detailed explanation of the interface.   }
{$ifndef sane_h}
{$DEFINE H2PAS_FUNCTION_1}
  {
   * SANE types and defines
    }

  const
    SANE_CURRENT_MAJOR = 1;    
    SANE_CURRENT_MINOR = 0;    
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   

  function SANE_VERSION_CODE(major,minor,build : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_MAJOR(code : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_MINOR(code : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_BUILD(code : longint) : longint;  

  const
    SANE_FALSE = 0;    
    SANE_TRUE = 1;    

  const
    SANE_FIXED_SCALE_SHIFT = 16;    
  { was #define dname(params) para_def_expr }
  { argument types are unknown }

  function SANE_FIX(v:Double):SANE_Word;
//  function SANE_FIX(v : longint) : SANE_Word;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_UNFIX(v : longint) : longint;  

  { everything A-OK  }
  { operation is not supported  }
  { operation was cancelled  }
  { device is busy; try again later  }
  { data is invalid (includes no dev at open)  }
  { no more data available (end-of-file)  }
  { document feeder jammed  }
  { document feeder out of documents  }
  { scanner cover is open  }
  { error during device I/O  }
  { out of memory  }
  { access to resource has been denied  }
{
    Const
      SANE_STATUS_GOOD = 0;
      SANE_STATUS_UNSUPPORTED = 1;
      SANE_STATUS_CANCELLED = 2;
      SANE_STATUS_DEVICE_BUSY = 3;
      SANE_STATUS_INVAL = 4;
      SANE_STATUS_EOF = 5;
      SANE_STATUS_JAMMED = 6;
      SANE_STATUS_NO_DOCS = 7;
      SANE_STATUS_COVER_OPEN = 8;
      SANE_STATUS_IO_ERROR = 9;
      SANE_STATUS_NO_MEM = 10;
      SANE_STATUS_ACCESS_DENIED = 11;
}
  { following are for later sane version, older frontends wont support  }
{$if 0}
  { lamp not ready, please retry  }
  const
    SANE_STATUS_WARMING_UP = 12;    
  { scanner mechanism locked for transport  }    SANE_STATUS_HW_LOCKED = 13;    
{$endif}

    Const
      SANE_TYPE_BOOL = 0;
      SANE_TYPE_INT = 1;
      SANE_TYPE_FIXED = 2;
      SANE_TYPE_STRING = 3;
      SANE_TYPE_BUTTON = 4;
      SANE_TYPE_GROUP = 5;

  { the value is unit-less (e.g., # of scans)  }
  { value is number of pixels  }
  { value is number of bits  }
  { value is millimeters  }
  { value is resolution in dots/inch  }
  { value is a percentage  }
  { value is micro seconds  }

    Const
      SANE_UNIT_NONE = 0;
      SANE_UNIT_PIXEL = 1;
      SANE_UNIT_BIT = 2;
      SANE_UNIT_MM = 3;
      SANE_UNIT_DPI = 4;
      SANE_UNIT_PERCENT = 5;
      SANE_UNIT_MICROSECOND = 6;

  { unique device name  }
  { device vendor string  }
  { device model name  }
  { device type (e.g., "flatbed scanner")  }

  const
    SANE_CAP_SOFT_SELECT = 1 shl 0;    
    SANE_CAP_HARD_SELECT = 1 shl 1;    
    SANE_CAP_SOFT_DETECT = 1 shl 2;    
    SANE_CAP_EMULATED = 1 shl 3;    
    SANE_CAP_AUTOMATIC = 1 shl 4;    
    SANE_CAP_INACTIVE = 1 shl 5;    
    SANE_CAP_ADVANCED = 1 shl 6;    
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   

  function SANE_OPTION_IS_ACTIVE(cap : longint) : boolean;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_OPTION_IS_SETTABLE(cap : longint) : boolean;

  const
    SANE_INFO_INEXACT = 1 shl 0;    
    SANE_INFO_RELOAD_OPTIONS = 1 shl 1;    
    SANE_INFO_RELOAD_PARAMS = 1 shl 2;    

    Const
      SANE_CONSTRAINT_NONE = 0;
      SANE_CONSTRAINT_RANGE = 1;
      SANE_CONSTRAINT_WORD_LIST = 2;
      SANE_CONSTRAINT_STRING_LIST = 3;

  { minimum (element) value  }
  { maximum (element) value  }
  { quantization value (0 if none)  }

    Const
      SANE_ACTION_GET_VALUE = 0;
      SANE_ACTION_SET_VALUE = 1;
      SANE_ACTION_SET_AUTO = 2;

  { band covering human visual range  }
  { pixel-interleaved red/green/blue bands  }
  { red band only  }
  { green band only  }
  { blue band only  }

    Const
      SANE_FRAME_GRAY = 0;
      SANE_FRAME_RGB = 1;
      SANE_FRAME_RED = 2;
      SANE_FRAME_GREEN = 3;
      SANE_FRAME_BLUE = 4;

  { push remaining types down to match existing backends  }
  { these are to be exposed in a later version of SANE  }
  { most front-ends will require updates to understand them  }
{$if 0}
  { backend specific textual data  }
  const
    SANE_FRAME_TEXT = $0A;    
  { complete baseline JPEG file  }    SANE_FRAME_JPEG = $0B;    
  { CCITT Group 3 1-D Compressed (MH)  }    SANE_FRAME_G31D = $0C;    
  { CCITT Group 3 2-D Compressed (MR)  }    SANE_FRAME_G32D = $0D;    
  { CCITT Group 4 2-D Compressed (MMR)  }    SANE_FRAME_G42D = $0E;    
  { bare infrared channel  }    SANE_FRAME_IR = $0F;    
  { red+green+blue+infrared  }    SANE_FRAME_RGBI = $10;    
  { gray+infrared  }    SANE_FRAME_GRAYI = $11;    
  { undefined schema  }    SANE_FRAME_XML = $12;    
{$endif}

  const
    SANE_MAX_USERNAME_LEN = 128;    
    SANE_MAX_PASSWORD_LEN = 128;    

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


  type

    SANE_Auth_Callback = procedure (resource:SANE_String_Const; username:PSANE_Char; password:PSANE_Char);cdecl;

  function sane_init(var version_code:SANE_Int; authorize:SANE_Auth_Callback):SANE_Status;cdecl;external External_library name 'sane_init';

  procedure sane_exit;cdecl;external External_library name 'sane_exit';

(* Const before type ignored *)
  function sane_get_devices(var device_list:PPSANE_Device; local_only:SANE_Bool):SANE_Status;cdecl;external External_library name 'sane_get_devices';

  function sane_open(devicename:SANE_String_Const; var handle:SANE_Handle):SANE_Status;cdecl;external External_library name 'sane_open';

  procedure sane_close(handle:SANE_Handle);cdecl;external External_library name 'sane_close';

(* Const before type ignored *)
  function sane_get_option_descriptor(handle:SANE_Handle; option:SANE_Int):PSANE_Option_Descriptor;cdecl;external External_library name 'sane_get_option_descriptor';

  function sane_control_option(handle:SANE_Handle; option:SANE_Int; action:SANE_Action; value:pointer; info:PSANE_Int):SANE_Status;cdecl;external External_library name 'sane_control_option';

  function sane_get_parameters(handle:SANE_Handle; var params:SANE_Parameters):SANE_Status;cdecl;external External_library name 'sane_get_parameters';

  function sane_start(handle:SANE_Handle):SANE_Status;cdecl;external External_library name 'sane_start';

  function sane_read ( handle: SANE_Handle; data: PChar; max_length: SANE_Int; length: pSANE_Int): SANE_Status; cdecl; external External_library name 'sane_read';
//  function sane_read(handle:SANE_Handle; data:PSANE_Byte; max_length:SANE_Int; var length:SANE_Int):SANE_Status;cdecl;external External_library name 'sane_read';

  procedure sane_cancel(handle:SANE_Handle);cdecl;external External_library name 'sane_cancel';

  function sane_set_io_mode(handle:SANE_Handle; non_blocking:SANE_Bool):SANE_Status;cdecl;external External_library name 'sane_set_io_mode';

  function sane_get_select_fd(handle:SANE_Handle; var fd:SANE_Int):SANE_Status;cdecl;external External_library name 'sane_get_select_fd';

  function sane_strstatus(status:SANE_Status):SANE_String_Const;cdecl;external External_library name 'sane_strstatus';

{$endif}
  { sane_h  }

implementation

{$IFDEF H2PAS_FUNCTION_1}
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_CODE(major,minor,build : longint) : longint;
  begin
//    SANE_VERSION_CODE:=(((SANE_Word(major(@($ff)))) shl 24) or ((SANE_Word(minor(@($ff)))) shl 16)) or ((SANE_Word(build(@($ffff)))) shl 0);
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_MAJOR(code : longint) : longint;
  begin
    SANE_VERSION_MAJOR:=((SANE_Word(code)) shr 24) and $ff;
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_MINOR(code : longint) : longint;
  begin
    SANE_VERSION_MINOR:=((SANE_Word(code)) shr 16) and $ff;
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_VERSION_BUILD(code : longint) : longint;
  begin
    SANE_VERSION_BUILD:=((SANE_Word(code)) shr 0) and $ffff;
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  function SANE_FIX(v:Double):SANE_Word;
//  function SANE_FIX(v : longint) : SANE_Word;
  begin
    result:=round(v * (1 shl SANE_FIXED_SCALE_SHIFT));
//    SANE_FIX:=SANE_Word(v*(1 shl SANE_FIXED_SCALE_SHIFT));
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_UNFIX(v : longint) : longint;
  begin
//    SANE_UNFIX:=(double(v))/(1 shl SANE_FIXED_SCALE_SHIFT);
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_OPTION_IS_ACTIVE(cap : longint) : boolean;
  begin
    result:=(cap and SANE_CAP_INACTIVE)=0;
  end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SANE_OPTION_IS_SETTABLE(cap : longint) : boolean;
  begin
    result:=(cap and SANE_CAP_SOFT_SELECT)<>0;
  end;
{$ENDIF H2PAS_FUNCTION_1}


end.
