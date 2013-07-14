{**************************************************************************************************}
{                                                                                                  }
{  Y library (Yl)                                                                                  }
{                                                                                                  }
{  Directives to support different compilers                                                       }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: Yl.inc.                                                                   }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// Ermitteln, ob Borland-Compiler
{$IFNDEF FPC ~}
  // hier weitere Ausschlußkriterien für einen Borland-Compiler eintragen
  {$DEFINE BORLAND}
{$ENDIF ~FPC}

// Wir nehmen hier an, daß der Compiler alle Eigenschaften unterstützt
// Unterstützung von Sprach-Eigenschaften
{$DEFINE SUPPORTS_CONSTPARAMS}       // Compiler supports const parameters
{$DEFINE SUPPORTS_VARARGS}           // Compiler supports varargs
{$DEFINE SUPPORTS_OUTPARAMS}         // Compiler supports out parameters
{$DEFINE SUPPORTS_THREADVAR}         // Compiler supports threadvar declarations
{$DEFINE SUPPORTS_INTERFACE}         // Compiler supports interfaces
{$DEFINE SUPPORTS_DISPINTERFACE}     // Compiler supports dispinterfaces
{$DEFINE SUPPORTS_EXTSYM}            // Compiler supports the $EXTERNALSYM directive
{$DEFINE SUPPORTS_NODEFINE}          // Compiler supports the $NODEFINE directive
{$DEFINE SUPPORTS_OVERLOAD}          // Compiler supports overloading
{$DEFINE SUPPORTS_IMPLEMENTS}        // Compiler supports implements

// Unterstützung von Typen
{$DEFINE SUPPORTS_LONGWORD}          // Compiler supports the LongWord type (unsigned 32 bit)
{$DEFINE SUPPORTS_INT64}             // Compiler supports the Int64 type
{$DEFINE SUPPORTS_UINT64}            // Compiler supports the UInt64 type
{$DEFINE SUPPORTS_WIDECHAR}          // Compiler supports the WideChar type
{$DEFINE SUPPORTS_WIDESTRING}        // Compiler supports the WideString type
{$DEFINE SUPPORTS_SINGLE}            // Compiler supports the Single type
{$DEFINE SUPPORTS_DOUBLE}            // Compiler supports the Double type
{$DEFINE SUPPORTS_EXTENDED}          // Compiler supports the Extended type
{$DEFINE SUPPORTS_CURRENCY}          // Compiler supports the Currency type
{$DEFINE SUPPORTS_VARIANT}           // Compiler supports variant
{$DEFINE SUPPORTS_CUSTOMVARIANTS}    // Compiler supports custom variants
{$DEFINE SUPPORTS_DYNAMICARRAYS}     // Compiler supports dynamic arrays
{$DEFINE SUPPORTS_DEFAULTPARAMS}     // Compiler supports default parameters
{$DEFINE SUPPORTS_PREDECLARED_PTRS}  // Compiler supports predeclared pointer types
                                     //   PByte, PWord, PLongWord, PCardinal,
                                     //   PShortInt, PSmallInt, PLongInt, PInt64, PInteger,
                                     //   PSingle, PDouble, PExtended, PCurrency, PComp,
                                     //   PBoolean, PPointer
{$DEFINE SUPPORTS_PTRINT}            // Compiler supports PtrInt and PtrUInt

// Unterstützung von Direktiven
{$DEFINE SUPPORTS_DEPRECATED}        // Compiler supports the deprecated directive
{$DEFINE SUPPORTS_PLATFORM}          // Compiler supports the platform directive
{$DEFINE SUPPORTS_LIBRARY}           // Compiler supports the library directive
{$DEFINE SUPPORTS_LOCAL}             // Compiler supports the local directive
{$DEFINE SUPPORTS_ENUMVALUE}         // Compiler supports values for enums
{$DEFINE SUPPORTS_UNSAFE_WARNINGS}   // Compiler supports unsafe warnings
{$DEFINE SUPPORTS_WEAKPACKAGEUNIT}   // Compiler supports the WEAKPACKAGEUNIT directive (don't valid for trial)

// Wir nehmen hier an, daß die Laufzeitbibliotheken die Funktionalität unterstützen
// Existenz bestimmter Units oder Funktionen
{$DEFINE RTL_SUPPORTS_CREATERES}     // The exception class has CreateRes, CreateResFmt methods
{$DEFINE RTL_UNIT_RTLCONSTS}         // The unit RTLConsts exists
{$DEFINE RTL_UNIT_TYPES}             // The unit Types exists
{$DEFINE RTL_UNIT_VARIANTS}          // The unit Variants exists
{$DEFINE RTL_UNIT_STRUTILS}          // The unit StrUtils exists
{$DEFINE RTL_XPLATFORM}              // The RTL supports crossplatform function names (e.g. RaiseLastOSError)


// die Unit Libc existiert nur, wenn für Linux entwickelt wird
{ TODO : existiert sie bei FPC auch für Unix? }
{$IFDEF LINUX}
  {$DEFINE RTL_UNIT_LIBC}            // The unit Libc exists on Linux
{$ENDIF LINUX}

// *******************************************************************************************

// Eigenschaften von Borland Compilern festlegen
{$IFDEF BORLAND}
  {$I YlExclNonB.inc}        // exclude non Borland extensions
  {$DEFINE ENDIAN_LITTLE}    // Borland läuft auf x86 Architektur
  {$DEFINE CPUI386}          // Borland läuft auf 32 Bit Intel x86 Architektur
  {$IFDEF LINUX}
    {$DEFINE UNIX}
    {$I YlKylix.inc}         // exclude pre-Kylix extensions
  {$ELSE LINUX~}
    // Delphi 2005, RTL 17.0
    {$IFDEF VER170}
    {$ENDIF VER170}

    // Delphi 8, RTL 16.0
    {$IFDEF VER160}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
    {$ENDIF VER160}

    // Delphi 7, RTL 15.0
    {$IFDEF VER150}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
    {$ENDIF VER150}

    // Delphi 6, BCB 6, RTL 14.0
    {$IFDEF VER140}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
    {$ENDIF VER140}

    // Delphi 5, BCB 5, RTL 13.0
    {$IFDEF VER130}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
    {$ENDIF VER130}

    // BCB 4, RTL 12.5
    {$IFDEF VER125}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
    {$ENDIF VER125}

    // Delphi 4, RTL 12.0
    {$IFDEF VER120}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
      {$I YlExclB125.inc}    // exclude BCB 4 extensions
    {$ENDIF VER120}

    // BCB 3, RTL 11.0
    {$IFDEF VER110}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
      {$I YlExclB125.inc}    // exclude BCB 4 extensions
      {$I YlExclB120.inc}    // exclude Delphi 4 extensions
    {$ENDIF VER110}

    // Delphi 3, RTL 10.0
    {$IFDEF VER100}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
      {$I YlExclB125.inc}    // exclude BCB 4 extensions
      {$I YlExclB120.inc}    // exclude Delphi 4 extensions
      {$I YlExclB110.inc}    // exclude BCB 3 extensions
    {$ENDIF VER100}

    // BCB 1, RTL 9.3
    {$IFDEF VER93}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
      {$I YlExclB125.inc}    // exclude BCB 4 extensions
      {$I YlExclB120.inc}    // exclude Delphi 4 extensions
      {$I YlExclB110.inc}    // exclude BCB 3 extensions
      {$I YlExclB100.inc}    // exclude Delphi 3 extensions
    {$ENDIF VER93}

    // Delphi 2, RTL 9.0
    {$IFDEF VER90}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
      {$I YlExclB125.inc}    // exclude BCB 4 extensions
      {$I YlExclB120.inc}    // exclude Delphi 4 extensions
      {$I YlExclB110.inc}    // exclude BCB 3 extensions
      {$I YlExclB100.inc}    // exclude Delphi 3 extensions
      {$I YlExclB093.inc}    // exclude BCB 1 extensions
    {$ENDIF VER90}

    // Delphi 1, RTL 8.0
    {$IFDEF VER80}
      {$I YlExclB170.inc}    // exclude Delphi 2005 extensions
      {$I YlExclB160.inc}    // exclude Delphi 8 extensions
      {$I YlExclB150.inc}    // exclude Delphi 7 extensions
      {$I YlExclB140.inc}    // exclude Delphi/BCB 6 extensions
      {$I YlExclB130.inc}    // exclude Delphi/BCB 5 extensions
      {$I YlExclB125.inc}    // exclude BCB 4 extensions
      {$I YlExclB120.inc}    // exclude Delphi 4 extensions
      {$I YlExclB110.inc}    // exclude BCB 3 extensions
      {$I YlExclB100.inc}    // exclude Delphi 3 extensions
      {$I YlExclB093.inc}    // exclude BCB 1 extensions
      {$I YlExclB090.inc}    // exclude Delphi 2 extensions
    {$ENDIF VER80}
  {$ENDIF ~LINUX}
{$ENDIF BORLAND}

// *******************************************************************************************

// FPC initalisieren und Eigenschaften festlegen
{$IFDEF FPC}
  {$MODE DELPHI}

  {$IFDEF VER1_0}
     Your compiler version is 20th century technology.
     Use FPC 1.1 or later to compile this.
  {$ELSE VER1_0~}
    {$I YlExclNonFPC.inc}    // exclude non FPC extensions
  {$ENDIF ~VER1_0}
{$ENDIF FPC}

// *******************************************************************************************

//  History:
//  2005-07-30, Peter J. Haas
//   - add SUPPORTS_PTRINT
//
//  2005-02-09, Peter J. Haas
//   - add RTL_SUPPORTS_CREATERES

