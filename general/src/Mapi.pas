{ *********************************************************************** }
{                                                                         }
{ Delphi Runtime Library                                                  }
{                                                                         }
{ Copyright (c) 1996-2001 Borland Software Corporation                    }
{                                                                         }
{ *********************************************************************** }

{*******************************************************}
{       Simple MAPI Interface Unit                      }
{*******************************************************}

unit Mapi;

{NOTE: Do not place the $NOPACKAGEUNIT directive in this file}

interface

uses Windows;

{
  Messaging Applications Programming Interface.

  Purpose:

    This file defines the structures and constants used by that
    subset of the Messaging Applications Programming Interface
    which is supported under Windows by Microsoft Mail for PC
    Networks version 3.x.
}

(*$HPPEMIT '#include <mapi.h>'*)

type
  {$EXTERNALSYM FLAGS}
  FLAGS = Cardinal;
  {$EXTERNALSYM LHANDLE}
  LHANDLE = Cardinal;
  PLHANDLE = ^Cardinal;

const
  {$EXTERNALSYM lhSessionNull}
  lhSessionNull = (0);

type
  PMapiFileDesc = ^TMapiFileDesc;
  {$EXTERNALSYM MapiFileDesc}
  MapiFileDesc = packed record
    ulReserved: Cardinal;        { Reserved for future use (must be 0)     }
    flFlags: Cardinal;           { Flags                                   }
    nPosition: Cardinal;         { character in text to be replaced by attachment }
    lpszPathName: LPSTR;         { Full path name of attachment file       }
    lpszFileName: LPSTR;         { Original file name (optional)           }
    lpFileType: Pointer;         { Attachment file type (can be lpMapiFileTagExt) }
  end;
  TMapiFileDesc = MapiFileDesc;

const
  {$EXTERNALSYM MAPI_OLE}
  MAPI_OLE = $00000001;
  {$EXTERNALSYM MAPI_OLE_STATIC}
  MAPI_OLE_STATIC = $00000002;


type
  PMapiFileTagExt = ^TMapiFileTagExt;
  {$EXTERNALSYM MapiFileTagExt}
  MapiFileTagExt = packed record
    ulReserved: Cardinal;       { Reserved, must be zero.                  }
    cbTag: Cardinal;            { Size (in bytes) of                       }
    lpTag: PByte;               { X.400 OID for this attachment type       }
    cbEncoding: Cardinal;       { Size (in bytes) of                       }
    lpEncoding: PByte;          { X.400 OID for this attachment's encoding }
  end;
  TMapiFileTagExt = MapiFileTagExt;


  PMapiRecipDesc = ^TMapiRecipDesc;
  {$EXTERNALSYM MapiRecipDesc}
  MapiRecipDesc = packed record
    ulReserved: Cardinal;       { Reserved for future use                  }
    ulRecipClass: Cardinal;     { Recipient class                          }
                                { MAPI_TO, MAPI_CC, MAPI_BCC, MAPI_ORIG    }
    lpszName: LPSTR;            { Recipient name                           }
    lpszAddress: LPSTR;         { Recipient address (optional)             }
    ulEIDSize: Cardinal;        { Count in bytes of size of pEntryID       }
    lpEntryID: Pointer;         { System-specific recipient reference      }
  end;
  TMapiRecipDesc = MapiRecipDesc;

const
  {$EXTERNALSYM MAPI_ORIG}
  MAPI_ORIG = 0;                { Recipient is message originator          }
  {$EXTERNALSYM MAPI_TO}
  MAPI_TO = 1;                  { Recipient is a primary recipient         }
  {$EXTERNALSYM MAPI_CC}
  MAPI_CC = 2;                  { Recipient is a copy recipient            }
  {$EXTERNALSYM MAPI_BCC}
  MAPI_BCC = 3;                 { Recipient is blind copy recipient        }

type
  PMapiMessage = ^TMapiMessage;
  {$EXTERNALSYM MapiMessage}
  MapiMessage = packed record
    ulReserved: Cardinal;         { Reserved for future use (M.B. 0)       }
    lpszSubject: LPSTR;           { Message Subject                        }
    lpszNoteText: LPSTR;          { Message Text                           }
    lpszMessageType: LPSTR;       { Message Class                          }
    lpszDateReceived: LPSTR;      { in YYYY/MM/DD HH:MM format             }
    lpszConversationID: LPSTR;    { conversation thread ID                 }
    flFlags: FLAGS;               { unread,return receipt                  }
    lpOriginator: PMapiRecipDesc; { Originator descriptor                  }
    nRecipCount: Cardinal;        { Number of recipients                   }
    lpRecips: PMapiRecipDesc;     { Recipient descriptors                  }
    nFileCount: Cardinal;         { # of file attachments                  }
    lpFiles: PMapiFileDesc;       { Attachment descriptors                 }
  end;
  TMapiMessage = MapiMessage;

const
  {$EXTERNALSYM MAPI_UNREAD}
  MAPI_UNREAD = $00000001;
  {$EXTERNALSYM MAPI_RECEIPT_REQUESTED}
  MAPI_RECEIPT_REQUESTED = $00000002;
  {$EXTERNALSYM MAPI_SENT}
  MAPI_SENT = $00000004;


{ Entry points. }

{ flFlags values for Simple MAPI entry points. All documented flags are
  shown for each call. Duplicates are commented out but remain present
  for every call. }

{ MAPILogon() flags. }

  {$EXTERNALSYM MAPI_LOGON_UI}
  MAPI_LOGON_UI = $00000001;                { Display logon UI             }
  {$EXTERNALSYM MAPI_PASSWORD_UI}
  MAPI_PASSWORD_UI = $00020000;             { prompt for password only     }
  {$EXTERNALSYM MAPI_NEW_SESSION}
  MAPI_NEW_SESSION = $00000002;             { Don't use shared session     }
  {$EXTERNALSYM MAPI_FORCE_DOWNLOAD}
  MAPI_FORCE_DOWNLOAD = $00001000;          { Get new mail before return   }
  {$EXTERNALSYM MAPI_ALLOW_OTHERS}
  MAPI_ALLOW_OTHERS = $00000008;            { Make this a shared session   (removed from 4.0 SDK) }
  {$EXTERNALSYM MAPI_EXPLICIT_PROFILE}
  MAPI_EXPLICIT_PROFILE = $00000010;        { Don't use default profile    (removed from 4.0 SDK) }
  {$EXTERNALSYM MAPI_EXTENDED}
  MAPI_EXTENDED = $00000020;                { Extended MAPI Logon          }
  {$EXTERNALSYM MAPI_USE_DEFAULT}
  MAPI_USE_DEFAULT = $00000040;             { Use default profile in logon (removed from 4.0 SDK) }

  {$EXTERNALSYM MAPI_SIMPLE_DEFAULT}
  MAPI_SIMPLE_DEFAULT = MAPI_LOGON_UI or MAPI_FORCE_DOWNLOAD or MAPI_ALLOW_OTHERS; { removed from 4.0 SDK }
  {$EXTERNALSYM MAPI_SIMPLE_EXPLICIT}
  MAPI_SIMPLE_EXPLICIT = MAPI_NEW_SESSION or MAPI_FORCE_DOWNLOAD or MAPI_EXPLICIT_PROFILE; { removed from 4.0 SDK }

{ MAPILogoff() flags.      }

  {$EXTERNALSYM MAPI_LOGOFF_SHARED}
  MAPI_LOGOFF_SHARED = $00000001;           { Close all shared sessions  (removed from 4.0 SDK)  }
  {$EXTERNALSYM MAPI_LOGOFF_UI}
  MAPI_LOGOFF_UI = $00000002;               { It's OK to present UI      (removed from 4.0 SDK)  }

{ MAPISendMail() flags.    }

{ #define MAPI_LOGON_UI        0x00000001     Display logon UI             }
{ #define MAPI_NEW_SESSION     0x00000002     Don't use shared session     }
  {$EXTERNALSYM MAPI_DIALOG}
  MAPI_DIALOG = $00000008;                  { Display a send note UI       }
{ # define MAPI_USE_DEFAULT     0x00000040     Use default profile in logon }

{ MAPIFindNext() flags.    }

  {$EXTERNALSYM MAPI_UNREAD_ONLY}
  MAPI_UNREAD_ONLY = $00000020;             { Only unread messages         }
  {$EXTERNALSYM MAPI_GUARANTEE_FIFO}
  MAPI_GUARANTEE_FIFO = $00000100;          { use date order               }
  {$EXTERNALSYM MAPI_LONG_MSGID}
  MAPI_LONG_MSGID = $00004000;              { allow 512 char returned ID   }

{ MAPIReadMail() flags.    }

  {$EXTERNALSYM MAPI_PEEK}
  MAPI_PEEK = $00000080;                    { Do not mark as read.         }
  {$EXTERNALSYM MAPI_SUPPRESS_ATTACH}
  MAPI_SUPPRESS_ATTACH = $00000800;         { header + body, no files      }
  {$EXTERNALSYM MAPI_ENVELOPE_ONLY}
  MAPI_ENVELOPE_ONLY = $00000040;           { Only header information      }
  {$EXTERNALSYM MAPI_BODY_AS_FILE}
  MAPI_BODY_AS_FILE = $00000200;

{ MAPISaveMail() flags.    }

{ #define MAPI_LOGON_UI        0x00000001     Display logon UI             }
{ #define MAPI_NEW_SESSION     0x00000002     Don't use shared session     }
{ #define MAPI_LONG_MSGID      0x00004000  /* allow 512 char returned ID   }

{ MAPIAddress() flags.     }

{ #define MAPI_LOGON_UI        0x00000001     Display logon UI             }
{ #define MAPI_NEW_SESSION     0x00000002     Don't use shared session     }

{ MAPIDetails() flags.     }

{ #define MAPI_LOGON_UI        0x00000001     Display logon UI             }
{ #define MAPI_NEW_SESSION     0x00000002     Don't use shared session     }
  {$EXTERNALSYM MAPI_AB_NOMODIFY}
  MAPI_AB_NOMODIFY = $00000400;             { Don't allow mods of AB entries }

{ MAPIResolveName() flags. }

{ #define MAPI_LOGON_UI        0x00000001     Display logon UI             }
{ #define MAPI_NEW_SESSION     0x00000002     Don't use shared session     }
{ #define MAPI_DIALOG          0x00000008     Prompt for choices if ambiguous }
{ #define MAPI_AB_NOMODIFY     0x00000400     Don't allow mods of AB entries }

type
  PFNMapiLogon = ^TFNMapiLogOn;
  TFNMapiLogOn = function(ulUIParam: Cardinal; lpszProfileName: LPSTR;
    lpszPassword: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
    lplhSession: PLHANDLE): Cardinal stdcall;

  PFNMapiLogOff = ^TFNMapiLogOff;
  TFNMapiLogOff = function(lhSession: LHANDLE; ulUIParam: Cardinal; flFlags: FLAGS;
    ulReserved: Cardinal): Cardinal stdcall;

  PFNMapiSendMail = ^TFNMapiSendMail;
  TFNMapiSendMail = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    var lpMessage: TMapiMessage; flFlags: FLAGS;
    ulReserved: Cardinal): Cardinal stdcall;

  PFNMapiSendDocuments = ^TFNMapiSendDocuments;
  TFNMapiSendDocuments = function(ulUIParam: Cardinal; lpszDelimChar: LPSTR;
    lpszFilePaths: LPSTR; lpszFileNames: LPSTR;
    ulReserved: Cardinal): Cardinal stdcall;

  PFNMapiFindNext = ^TFNMapiFindNext;
  TFNMapiFindNext = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    lpszMessageType: LPSTR; lpszSeedMessageID: LPSTR; flFlags: FLAGS;
    ulReserved: Cardinal; lpszMessageID: LPSTR): Cardinal stdcall;

  PFNMapiReadMail = ^TFNMapiReadMail;
  TFNMapiReadMail = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
    var lppMessage: PMapiMessage): Cardinal stdcall;

  PFNMapiSaveMail = ^TFNMapiSaveMail;
  TFNMapiSaveMail = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    var lpMessage: TMapiMessage; flFlags: FLAGS; ulReserved: Cardinal;
    lpszMessageID: LPSTR): Cardinal stdcall;

  PFNMapiDeleteMail = ^TFNMapiDeleteMail;
  TFNMapiDeleteMail = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    lpszMessageID: LPSTR; flFlags: FLAGS;
    ulReserved: Cardinal): Cardinal stdcall;

  PFNMapiFreeBuffer = ^TFNMapiFreeBuffer;
  TFNMapiFreeBuffer = function(pv: Pointer): Cardinal stdcall;

  PFNMapiAddress = ^TFNMapiAddress;
  TFNMapiAddress = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    lpszCaption: LPSTR; nEditFields: Cardinal; lpszLabels: LPSTR;
    nRecips: Cardinal; var lpRecips: TMapiRecipDesc; flFlags: FLAGS;
    ulReserved: Cardinal; lpnNewRecips: PULONG;
    var lppNewRecips: PMapiRecipDesc): Cardinal stdcall;

  PFNMapiDetails = ^TFNMapiDetails;
  TFNMapiDetails = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    var lpRecip: TMapiRecipDesc; flFlags: FLAGS;
    ulReserved: Cardinal): Cardinal stdcall;

  PFNMapiResolveName = ^TFNMapiResolveName;
  TFNMapiResolveName = function(lhSession: LHANDLE; ulUIParam: Cardinal;
    lpszName: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
    var lppRecip: PMapiRecipDesc): Cardinal stdcall;

const
  {$EXTERNALSYM SUCCESS_SUCCESS}
  SUCCESS_SUCCESS = 0;
  {$EXTERNALSYM MAPI_USER_ABORT}
  MAPI_USER_ABORT = 1;
  {$EXTERNALSYM MAPI_E_USER_ABORT}
  MAPI_E_USER_ABORT                  = MAPI_USER_ABORT;
  {$EXTERNALSYM MAPI_E_FAILURE}
  MAPI_E_FAILURE = 2;
  {$EXTERNALSYM MAPI_E_LOGON_FAILURE}
  MAPI_E_LOGON_FAILURE = 3;
  {$EXTERNALSYM MAPI_E_LOGIN_FAILURE}
  MAPI_E_LOGIN_FAILURE               = MAPI_E_LOGON_FAILURE;
  {$EXTERNALSYM MAPI_E_DISK_FULL}
  MAPI_E_DISK_FULL = 4;
  {$EXTERNALSYM MAPI_E_INSUFFICIENT_MEMORY}
  MAPI_E_INSUFFICIENT_MEMORY = 5;
  {$EXTERNALSYM MAPI_E_ACCESS_DENIED}
  MAPI_E_ACCESS_DENIED = 6;
  {$EXTERNALSYM MAPI_E_TOO_MANY_SESSIONS}
  MAPI_E_TOO_MANY_SESSIONS = 8;
  {$EXTERNALSYM MAPI_E_TOO_MANY_FILES}
  MAPI_E_TOO_MANY_FILES = 9;
  {$EXTERNALSYM MAPI_E_TOO_MANY_RECIPIENTS}
  MAPI_E_TOO_MANY_RECIPIENTS = 10;
  {$EXTERNALSYM MAPI_E_ATTACHMENT_NOT_FOUND}
  MAPI_E_ATTACHMENT_NOT_FOUND = 11;
  {$EXTERNALSYM MAPI_E_ATTACHMENT_OPEN_FAILURE}
  MAPI_E_ATTACHMENT_OPEN_FAILURE = 12;
  {$EXTERNALSYM MAPI_E_ATTACHMENT_WRITE_FAILURE}
  MAPI_E_ATTACHMENT_WRITE_FAILURE = 13;
  {$EXTERNALSYM MAPI_E_UNKNOWN_RECIPIENT}
  MAPI_E_UNKNOWN_RECIPIENT = 14;
  {$EXTERNALSYM MAPI_E_BAD_RECIPTYPE}
  MAPI_E_BAD_RECIPTYPE = 15;
  {$EXTERNALSYM MAPI_E_NO_MESSAGES}
  MAPI_E_NO_MESSAGES = 16;
  {$EXTERNALSYM MAPI_E_INVALID_MESSAGE}
  MAPI_E_INVALID_MESSAGE = 17;
  {$EXTERNALSYM MAPI_E_TEXT_TOO_LARGE}
  MAPI_E_TEXT_TOO_LARGE = 18;
  {$EXTERNALSYM MAPI_E_INVALID_SESSION}
  MAPI_E_INVALID_SESSION = 19;
  {$EXTERNALSYM MAPI_E_TYPE_NOT_SUPPORTED}
  MAPI_E_TYPE_NOT_SUPPORTED = 20;
  {$EXTERNALSYM MAPI_E_AMBIGUOUS_RECIPIENT}
  MAPI_E_AMBIGUOUS_RECIPIENT = 21;
  {$EXTERNALSYM MAPI_E_AMBIG_RECIP}
  MAPI_E_AMBIG_RECIP                 = MAPI_E_AMBIGUOUS_RECIPIENT;
  {$EXTERNALSYM MAPI_E_MESSAGE_IN_USE}
  MAPI_E_MESSAGE_IN_USE = 22;
  {$EXTERNALSYM MAPI_E_NETWORK_FAILURE}
  MAPI_E_NETWORK_FAILURE = 23;
  {$EXTERNALSYM MAPI_E_INVALID_EDITFIELDS}
  MAPI_E_INVALID_EDITFIELDS = 24;
  {$EXTERNALSYM MAPI_E_INVALID_RECIPS}
  MAPI_E_INVALID_RECIPS = 25;
  {$EXTERNALSYM MAPI_E_NOT_SUPPORTED}
  MAPI_E_NOT_SUPPORTED = 26;


{ Delphi wrapper calls around Simple MAPI }

function MapiLogOn(ulUIParam: Cardinal; lpszProfileName: LPSTR;
  lpszPassword: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
  lplhSession: PLHANDLE): Cardinal;

function MapiLogOff(lhSession: LHANDLE; ulUIParam: Cardinal; flFlags: FLAGS;
  ulReserved: Cardinal): Cardinal;

function MapiSendMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  var lpMessage: TMapiMessage; flFlags: FLAGS; ulReserved: Cardinal): Cardinal;

function MapiSendDocuments(ulUIParam: Cardinal; lpszDelimChar: LPSTR;
  lpszFilePaths: LPSTR; lpszFileNames: LPSTR; ulReserved: Cardinal): Cardinal;

function MapiFindNext(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszMessageType: LPSTR; lpszSeedMessageID: LPSTR; flFlags: FLAGS;
  ulReserved: Cardinal; lpszMessageID: LPSTR): Cardinal;

function MapiReadMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
  var lppMessage: PMapiMessage): Cardinal;

function MapiSaveMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  var lpMessage: TMapiMessage; flFlags: FLAGS; ulReserved: Cardinal;
  lpszMessageID: LPSTR): Cardinal;

function MapiDeleteMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszMessageID: LPSTR; flFlags: FLAGS;
  ulReserved: Cardinal): Cardinal;

function MapiFreeBuffer(pv: Pointer): Cardinal;

function MapiAddress(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszCaption: LPSTR; nEditFields: Cardinal; lpszLabels: LPSTR;
  nRecips: Cardinal; var lpRecips: TMapiRecipDesc; flFlags: FLAGS;
  ulReserved: Cardinal; lpnNewRecips: PULONG;
  var lppNewRecips: PMapiRecipDesc): Cardinal;

function MapiDetails(lhSession: LHANDLE; ulUIParam: Cardinal;
  var lpRecip: TMapiRecipDesc; flFlags: FLAGS;
  ulReserved: Cardinal): Cardinal;

function MapiResolveName(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszName: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
  var lppRecip: PMapiRecipDesc): Cardinal;

var
  MAPIDLL: string = 'MAPI32.DLL';

implementation

var
  MAPIModule: HModule = 0;
  LogOn: TFNMapiLogOn = nil;
  LogOff: TFNMapiLogOff = nil;
  SendMail: TFNMapiSendMail = nil;
  SendDocuments: TFNMapiSendDocuments = nil;
  FindNext: TFNMapiFindNext = nil;
  ReadMail: TFNMapiReadMail = nil;
  SaveMail: TFNMapiSaveMail = nil;
  DeleteMail: TFNMapiDeleteMail = nil;
  FreeBuffer: TFNMapiFreeBuffer = nil;
  Address: TFNMapiAddress = nil;
  Details: TFNMapiDetails = nil;
  ResolveName: TFNMapiResolveName = nil;

var
  MAPIChecked: Boolean = False;

procedure InitMapi;
var
  OSVersionInfo: TOSVersionInfo;
  hkWMS: HKEY;
  MAPIValueSize: Longint;
  MAPIValueBuf: array[0..8] of char;
  rType: Longint;
begin
  if not MAPIChecked then
  begin
    MAPIChecked := True;
    MAPIModule := 0;

    OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
    GetVersionEx(OSVersionInfo);
    if (OSVersionInfo.dwMajorVersion > 3) or
      ((OSVersionInfo.dwMajorVersion = 3) and
       (OSVersionInfo.dwMinorVersion > 51)) then
    begin
      MAPIValueSize := sizeof(MAPIValueBuf);
      if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows Messaging Subsystem',
        0, KEY_READ, hkWMS) <> ERROR_SUCCESS then Exit;
      if RegQueryValueEx(hkWMS, 'MAPI', nil, @rType, @MAPIValueBuf,
        @MAPIValueSize) <> ERROR_SUCCESS then Exit;
      RegCloseKey(hkWMS);
      if not ((MAPIValueBuf[0] = '1') and (MAPIValueBuf[1] = #0)) then Exit;
    end
    else if GetProfileInt('Mail', 'MAPI', 0) = 0 then Exit;

    MAPIModule := LoadLibrary(PChar(MAPIDLL));
  end;
end;

function MapiLogOn(ulUIParam: Cardinal; lpszProfileName: LPSTR;
  lpszPassword: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
  lplhSession: PLHANDLE): Cardinal;
begin
  InitMapi;
  if @LogOn = nil then
    @LogOn := GetProcAddress(MAPIModule, 'MAPILogon');
  if @LogOn <> nil then
    Result := LogOn(ulUIParam, lpszProfileName, lpszPassword, flFlags,
      ulReserved, lplhSession)
  else Result := 1;
end;

function MapiLogOff(lhSession: LHANDLE; ulUIParam: Cardinal; flFlags: FLAGS;
  ulReserved: Cardinal): Cardinal;
begin
  InitMapi;
  if @LogOff = nil then
    @LogOff := GetProcAddress(MAPIModule, 'MAPILogoff');
  if @LogOff <> nil then
    Result := LogOff(lhSession, ulUIParam, flFlags, ulReserved)
  else Result := 1;
end;

function MapiSendMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  var lpMessage: TMapiMessage; flFlags: FLAGS; ulReserved: Cardinal): Cardinal;
begin
  InitMapi;
  if @SendMail = nil then
    @SendMail := GetProcAddress(MAPIModule, 'MAPISendMail');
  if @SendMail <> nil then
    Result := SendMail(lhSession, ulUIParam, lpMessage, flFlags, ulReserved)
  else Result := 1;
end;

function MapiSendDocuments(ulUIParam: Cardinal; lpszDelimChar: LPSTR;
  lpszFilePaths: LPSTR; lpszFileNames: LPSTR;
  ulReserved: Cardinal): Cardinal;
begin
  InitMapi;
  if @SendDocuments = nil then
    @SendDocuments := GetProcAddress(MAPIModule, 'MAPISendDocuments');
  if @SendDocuments <> nil then
    Result := SendDocuments(ulUIParam, lpszDelimChar, lpszFilePaths,
      lpszFileNames, ulReserved)
  else Result := 1;
end;

function MapiFindNext(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszMessageType: LPSTR; lpszSeedMessageID: LPSTR; flFlags: FLAGS;
  ulReserved: Cardinal; lpszMessageID: LPSTR): Cardinal;
begin
  InitMapi;
  if @FindNext = nil then
    @FindNext := GetProcAddress(MAPIModule, 'MAPIFindNext');
  if @FindNext <> nil then
    Result := FindNext(lhSession, ulUIParam, lpszMessageType,
      lpszSeedMessageID, flFlags, ulReserved, lpszMessageID)
  else Result := 1;
end;

function MapiReadMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszMessageID: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
  var lppMessage: PMapiMessage): Cardinal;
begin
  InitMapi;
  if @ReadMail = nil then
    @ReadMail := GetProcAddress(MAPIModule, 'MAPIReadMail');
  if @ReadMail <> nil then
    Result := ReadMail(lhSession, ulUIParam, lpszMessageID, flFlags,
      ulReserved, lppMessage)
  else Result := 1;
end;

function MapiSaveMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  var lpMessage: TMapiMessage; flFlags: FLAGS; ulReserved: Cardinal;
  lpszMessageID: LPSTR): Cardinal;
begin
  InitMapi;
  if @SaveMail = nil then
    @SaveMail := GetProcAddress(MAPIModule, 'MAPISaveMail');
  if @SaveMail <> nil then
    Result := SaveMail(lhSession, ulUIParam, lpMessage, flFlags, ulReserved,
      lpszMessageID)
  else Result := 1;
end;

function MapiDeleteMail(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszMessageID: LPSTR; flFlags: FLAGS;
  ulReserved: Cardinal): Cardinal;
begin
  InitMapi;
  if @DeleteMail = nil then
    @DeleteMail := GetProcAddress(MAPIModule, 'MAPIDeleteMail');
  if @DeleteMail <> nil then
    Result := DeleteMail(lhSession, ulUIParam, lpszMessageID, flFlags,
      ulReserved)
  else Result := 1;
end;

function MapiFreeBuffer(pv: Pointer): Cardinal;
begin
  InitMapi;
  if @FreeBuffer = nil then
    @FreeBuffer := GetProcAddress(MAPIModule, 'MAPIFreeBuffer');
  if @FreeBuffer <> nil then
    Result := FreeBuffer(pv)
  else Result := 1;
end;

function MapiAddress(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszCaption: LPSTR; nEditFields: Cardinal; lpszLabels: LPSTR;
  nRecips: Cardinal; var lpRecips: TMapiRecipDesc; flFlags: FLAGS;
  ulReserved: Cardinal; lpnNewRecips: PULONG;
  var lppNewRecips: PMapiRecipDesc): Cardinal;
begin
  InitMapi;
  if @Address = nil then
    @Address := GetProcAddress(MAPIModule, 'MAPIAddress');
  if @Address <> nil then
    Result := Address(lhSession, ulUIParam, lpszCaption, nEditFields,
      lpszLabels, nRecips, lpRecips, flFlags, ulReserved, lpnNewRecips,
      lppNewRecips)
  else Result := 1;
end;

function MapiDetails(lhSession: LHANDLE; ulUIParam: Cardinal;
  var lpRecip: TMapiRecipDesc; flFlags: FLAGS; ulReserved: Cardinal): Cardinal;
begin
  InitMapi;
  if @Details = nil then
    @Details := GetProcAddress(MAPIModule, 'MAPIDetails');
  if @Details <> nil then
    Result := Details(lhSession, ulUIParam, lpRecip, flFlags, ulReserved)
  else Result := 1;
end;

function MapiResolveName(lhSession: LHANDLE; ulUIParam: Cardinal;
  lpszName: LPSTR; flFlags: FLAGS; ulReserved: Cardinal;
  var lppRecip: PMapiRecipDesc): Cardinal;
begin
  InitMapi;
  if @ResolveName = nil then
    @ResolveName := GetProcAddress(MAPIModule, 'MAPIResolveName');
  if @ResolveName <> nil then
    Result := ResolveName(lhSession, ulUIParam, lpszName, flFlags,
      ulReserved, lppRecip)
  else Result := 1;
end;

initialization
finalization
  if MAPIModule <> 0 then FreeLibrary(MAPIModule);
end.
