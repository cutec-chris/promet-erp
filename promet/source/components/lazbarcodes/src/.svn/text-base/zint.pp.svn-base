
unit zint;
interface

{
  Automatically converted by H2Pas 1.0.0 from zint.h
  The following command line parameters were used:
    zint.h
}

{$IFDEF FPC}
//{$PACKRECORDS C}
{$ENDIF}


  {  zint.h - definitions for libzint
  
      libzint - the open source barcode library
      Copyright (C) 2009 Robin Stuart <robin@zint.org.uk>
  
      This program is free software; you can redistribute it and/or modify
      it under the terms of the GNU General Public License as published by
      the Free Software Foundation; either version 3 of the License, or
      (at your option) any later version.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      GNU General Public License for more details.
  
      You should have received a copy of the GNU General Public License along
      with this program; if not, write to the Free Software Foundation, Inc.,
      51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
   }
{$ifndef ZINT_H}
{$define ZINT_H}  
{ C++ extern C conditionnal removed }
  { __cplusplus  }
  { Pointer to next line  }

  type
  PointerTo_zint_render_line=^zint_render_line;
  PointerTo_PointerTo_zint_render_line=^PointerTo_zint_render_line;
    zint_render_line = record
        x : single;
        y : single;
        length : single;
        width : single;
        next : PointerTo_zint_render_line; // ^zint_render_line;
      end;

  { Suggested string width, may be 0 if none recommended  }
  { Pointer to next character  }
  PointerTo_zint_render_string=^zint_render_string;
  PointerTo_PointerTo_zint_render_string=^PointerTo_zint_render_string;
    zint_render_string = record
        x : single;
        y : single;
        fsize : single;
        width : single;
        length : longint;
        text : ^byte;
        next : PointerTo_zint_render_string; // ^zint_render_string;
      end;

  { Pointer to next ring  }
  PointerTo_zint_render_ring=^zint_render_ring;
  PointerTo_PointerTo_zint_render_ring=^PointerTo_zint_render_ring;
    zint_render_ring = record
        x : single;
        y : single;
        radius : single;
        line_width : single;
        next : PointerTo_zint_render_ring; // ^zint_render_ring;
      end;

  { Pointer to next hexagon  }
  PointerTo_zint_render_hexagon=^zint_render_hexagon;
  PointerTo_PointerTo_zint_render_hexagon=^PointerTo_zint_render_hexagon;
    zint_render_hexagon = record
        x : single;
        y : single;
        next : PointerTo_zint_render_hexagon; // ^zint_render_hexagon;
      end;

  { Pointer to first line  }
  { Pointer to first string  }
  { Pointer to first ring  }
  { Pointer to first hexagon  }
  PointerTo_zint_render=^zint_render;
    zint_render = record
        width : single;
        height : single;
        lines : ^zint_render_line;
        strings : ^zint_render_string;
        rings : ^zint_render_ring;
        hexagons : ^zint_render_hexagon;
      end;

  { Largest symbol is 177x177 QR Code  }
    zint_symbol = record
        symbology : longint;
        height : longint;
        whitespace_width : longint;
        border_width : longint;
        output_options : longint;
        fgcolour : array[0..9] of char;
        bgcolour : array[0..9] of char;
        outfile : array[0..255] of char;
        scale : single;
        option_1 : longint;
        option_2 : longint;
        option_3 : longint;
        show_hrt : longint;
        input_mode : longint;
        text : array[0..127] of byte;
        rows : longint;
        width : longint;
        primary : array[0..127] of char;
        encoded_data : array[0..177] of array[0..142] of byte;
        row_height : array[0..177] of longint;
        errtxt : array[0..99] of char;
        bitmap : ^char;
        bitmap_width : longint;
        bitmap_height : longint;
        rendered : ^zint_render;
      end;

  PointerTo_zint_symbol=^zint_symbol;
  { Tbarcode 7 codes  }

  const
    BARCODE_CODE11 = 1;    
    BARCODE_C25MATRIX = 2;    
    BARCODE_C25INTER = 3;    
    BARCODE_C25IATA = 4;    
    BARCODE_C25LOGIC = 6;    
    BARCODE_C25IND = 7;    
    BARCODE_CODE39 = 8;    
    BARCODE_EXCODE39 = 9;    
    BARCODE_EANX = 13;    
    BARCODE_EAN128 = 16;    
    BARCODE_CODABAR = 18;    
    BARCODE_CODE128 = 20;    
    BARCODE_DPLEIT = 21;    
    BARCODE_DPIDENT = 22;    
    BARCODE_CODE16K = 23;    
    BARCODE_CODE49 = 24;    
    BARCODE_CODE93 = 25;    
    BARCODE_FLAT = 28;    
    BARCODE_RSS14 = 29;    
    BARCODE_RSS_LTD = 30;    
    BARCODE_RSS_EXP = 31;    
    BARCODE_TELEPEN = 32;    
    BARCODE_UPCA = 34;    
    BARCODE_UPCE = 37;    
    BARCODE_POSTNET = 40;    
    BARCODE_MSI_PLESSEY = 47;    
    BARCODE_FIM = 49;    
    BARCODE_LOGMARS = 50;    
    BARCODE_PHARMA = 51;    
    BARCODE_PZN = 52;    
    BARCODE_PHARMA_TWO = 53;    
    BARCODE_PDF417 = 55;    
    BARCODE_PDF417TRUNC = 56;    
    BARCODE_MAXICODE = 57;    
    BARCODE_QRCODE = 58;    
    BARCODE_CODE128B = 60;    
    BARCODE_AUSPOST = 63;    
    BARCODE_AUSREPLY = 66;    
    BARCODE_AUSROUTE = 67;    
    BARCODE_AUSREDIRECT = 68;    
    BARCODE_ISBNX = 69;    
    BARCODE_RM4SCC = 70;    
    BARCODE_DATAMATRIX = 71;    
    BARCODE_EAN14 = 72;    
    BARCODE_CODABLOCKF = 74;    
    BARCODE_NVE18 = 75;    
    BARCODE_JAPANPOST = 76;    
    BARCODE_KOREAPOST = 77;    
    BARCODE_RSS14STACK = 79;    
    BARCODE_RSS14STACK_OMNI = 80;    
    BARCODE_RSS_EXPSTACK = 81;    
    BARCODE_PLANET = 82;    
    BARCODE_MICROPDF417 = 84;    
    BARCODE_ONECODE = 85;    
    BARCODE_PLESSEY = 86;    
  { Tbarcode 8 codes  }
    BARCODE_TELEPEN_NUM = 87;    
    BARCODE_ITF14 = 89;    
    BARCODE_KIX = 90;    
    BARCODE_AZTEC = 92;    
    BARCODE_DAFT = 93;    
    BARCODE_MICROQR = 97;    
  { Tbarcode 9 codes  }
    BARCODE_HIBC_128 = 98;    
    BARCODE_HIBC_39 = 99;    
    BARCODE_HIBC_DM = 102;    
    BARCODE_HIBC_QR = 104;    
    BARCODE_HIBC_PDF = 106;    
    BARCODE_HIBC_MICPDF = 108;    
    BARCODE_HIBC_BLOCKF = 110;    
    BARCODE_HIBC_AZTEC = 112;    
  { Zint specific  }
    BARCODE_AZRUNE = 128;    
    BARCODE_CODE32 = 129;    
    BARCODE_EANX_CC = 130;    
    BARCODE_EAN128_CC = 131;    
    BARCODE_RSS14_CC = 132;    
    BARCODE_RSS_LTD_CC = 133;    
    BARCODE_RSS_EXP_CC = 134;    
    BARCODE_UPCA_CC = 135;    
    BARCODE_UPCE_CC = 136;    
    BARCODE_RSS14STACK_CC = 137;    
    BARCODE_RSS14_OMNI_CC = 138;    
    BARCODE_RSS_EXPSTACK_CC = 139;    
    BARCODE_CHANNEL = 140;    
    BARCODE_CODEONE = 141;    
    BARCODE_GRIDMATRIX = 142;    
    BARCODE_NO_ASCII = 1;    
    BARCODE_BIND = 2;    
    BARCODE_BOX = 4;    
    BARCODE_STDOUT = 8;    
    READER_INIT = 16;    
    SMALL_TEXT = 32;    
    DATA_MODE = 0;    
    UNICODE_MODE = 1;    
    GS1_MODE = 2;    
    KANJI_MODE = 3;    
    SJIS_MODE = 4;    
    DM_SQUARE = 100;    
    WARN_INVALID_OPTION = 2;    
    ERROR_TOO_LONG = 5;    
    ERROR_INVALID_DATA = 6;    
    ERROR_INVALID_CHECK = 7;    
    ERROR_INVALID_OPTION = 8;    
    ERROR_ENCODING_PROBLEM = 9;    
    ERROR_FILE_ACCESS = 10;    
    ERROR_MEMORY = 11;
{$ENDIF}

implementation


end.
