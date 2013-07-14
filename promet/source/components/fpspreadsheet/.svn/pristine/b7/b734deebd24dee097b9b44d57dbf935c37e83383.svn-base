{
uvirtuallayer_ole_types.pas

Part of "uvirtuallayer_ole".

Presented as an unit to hide definitions when using OLE virtual
layer.

AUTHORS: José Mejuto Porral
}
unit uvirtuallayer_ole_types;

{$mode objfpc}{$H+}

interface

const
SECT_DIFSECT = $FFFFFFFC;
SECT_FATSECT = $FFFFFFFD;
SECT_ENDOFCHAIN = $FFFFFFFE;
SECT_FREESECT = $FFFFFFFF;
BYTES_PER_FAT_ENTRY=4;
OLE_SIGTATURE: array [0..7] of BYTE =($D0,$CF,$11,$E0,$A1,$B1,$1A,$E1);
WINCOMPOUND_NOSID=$FFFFFFFF;

type

tagFILETIME=packed record
  dwLowDateTime: DWORD;
  dwHighDateTime: DWORD;
end;

FILETIME=tagFILETIME;
TIME_T=tagFILETIME;
SID=DWORD;
SECT=DWORD;
PSECT=^SECT;
DFPROPTYPE=WORD;
FSINDEX=DWORD;

TWCBFStructuredStorageHeader=packed record
  _abSig: array [0..7] of BYTE;     // [000H,08] {0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1} for current version,
                                    // which are also supported by the reference implementation
  _clid: TGUID;                     // [008H,16] class id (set with WriteClassStg, retrieved with GetClassFile/ReadClassStg)
  _uMinorVersion: WORD;             // [018H,02] minor version of the format: 33 is written by reference implementation
  _uDllVersion: WORD;               // [01AH,02] major version of the dll/format: 3 is written by reference implementation
  _uByteOrder: WORD;                // [01CH,02] 0xFFFE: indicates Intel byte-ordering
  _uSectorShift: WORD;              // [01EH,02] size of sectors in power-of-two (typically 9, indicating 512-byte sectors)
  _uMiniSectorShift: WORD;          // [020H,02] size of mini-sectors in power-of-two (typically 6, indicating 64-byte mini-sectors)
  _usReserved: WORD;                // [022H,02] reserved, must be zero
  _ulReserved1: DWORD;              // [024H,04] reserved, must be zero
  _ulReserved2: DWORD;              // [028H,04] reserved, must be zero
  _csectFat: DWORD;                 // [02CH,04] number of SECTs in the FAT chain
  _sectDirStart: DWORD;             // [030H,04] first SECT in the Directory chain
  _signature: DWORD;                // [034H,04] signature used for transactionin: must be zero. The reference implementation
                                    // does not support transactioning
  _ulMiniSectorCutoff: DWORD;       // [038H,04] maximum size for mini-streams: typically 4096 bytes
  _sectMiniFatStart: DWORD;         // [03CH,04] first SECT in the mini-FAT chain
  _csectMiniFat: DWORD;             // [040H,04] number of SECTs in the mini-FAT chain
  _sectDifStart: DWORD;             // [044H,04] first SECT in the DIF chain
  _csectDif: DWORD;                 // [048H,04] number of SECTs in the DIF chain
  _sectFat: array [0..108] of DWORD;// [04CH,436] the SECTs of the first 109 FAT sectors
end;
PWCBFStructuredStorageHeader=^TWCBFStructuredStorageHeader;

type etagSTGTY=(
  STGTY_INVALID    = 0,
  STGTY_STORAGE   = 1,
  STGTY_STREAM    = 2,
  STGTY_LOCKBYTES  = 3,
  STGTY_PROPERTY  = 4,
  STGTY_ROOT    = 5
  );
  
type etagDECOLOR=(
  DE_RED       = 0,
  DE_BLACK      = 1
  );
  
TWCBFStructuredStorageDirectoryEntry=packed record// [offset from start in bytes, length in bytes]
  _ab: array [0..31] of WChar;      // [000H,64] 64 bytes. The Element name in Unicode, padded with zeros t
                                    //  fill this byte array
  _cb: WORD;                        // [040H,02] Length of the Element name in characters, not bytes
  _mse: BYTE;                       // [042H,01] Type of object: value taken from the STGTY enumeration
  _bflags: BYTE;                    // [043H,01] Value taken from DECOLOR enumeration.
  _sidLeftSib: SID;                 // [044H,04] SID of the left-sibling of this entry in the directory tree
  _sidRightSib: SID;                // [048H,04] SID of the right-sibling of this entry in the directory tree
  _sidChild: SID;                   // [04CH,04] SID of the child acting as the root of all the children of this
                                    //  element (if _mse=STGTY_STORAGE)
  _clsId: TGUID;                    // [050H,16] CLSID of this storage (if _mse=STGTY_STORAGE)
  _dwUserFlags: DWORD;              // [060H,04] User flags of this storage (if _mse=STGTY_STORAGE)
  _time: array [0..1] of TIME_T;    // [064H,16] Create/Modify time-stamps (if _mse=STGTY_STORAGE)
  _sectStart: SECT;                 // [074H,04] starting SECT of the stream (if _mse=STGTY_STREAM)
  _ulSize: DWORD;                   // [078H,04] size of stream in bytes (if _mse=STGTY_STREAM)
  _dptPropType: DFPROPTYPE;         // [07CH,02] Reserved for future use. Must be zero.
  _Padding: array [0..1] of BYTE;
end;

implementation

end.

