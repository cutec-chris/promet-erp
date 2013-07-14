{**************************************************************************************************}
{                                                                                                  }
{  Y library (Yl)                                                                                  }
{                                                                                                  }
{  Directives to support different compilers                                                       }
{                                                                                                  }
{  Exclusion extensions, which are not exists in FPC                                               }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YlExclNonFPC.inc.                                                         }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

{$IFNDEF FPC_HAS_TYPE_SINGLE}
  {$UNDEF SUPPORTS_SINGLE}        // FPC supports the Single type dependent of CPU
{$ENDIF}
{$IFNDEF FPC_HAS_TYPE_DOUBLE}
  {$UNDEF SUPPORTS_DOUBLE}        // FPC supports the Double type dependent of CPU
{$ENDIF}
{$IFNDEF FPC_HAS_TYPE_EXTENDED}
  {$UNDEF SUPPORTS_EXTENDED}      // FPC supports the Extended type dependent of CPU
{$ENDIF}
{ TODO : Prüfen }
{$UNDEF SUPPORTS_UINT64}          // FPC don't supports the UInt64 type 
{$UNDEF SUPPORTS_DISPINTERFACE}
{$UNDEF SUPPORTS_EXTSYM}          // FPC ignore the $EXTERNALSYM directive
{$UNDEF SUPPORTS_NODEFINE}        // FPC ignore the $NODEFINE directive
{$UNDEF SUPPORTS_IMPLEMENTS}
{$UNDEF SUPPORTS_DEPRECATED}      // FPC ignore the deprecated directive
{$UNDEF SUPPORTS_PLATFORM}        // FPC ignore the platform directive
{$UNDEF SUPPORTS_LIBRARY}         // FPC ignore the library directive
{$UNDEF SUPPORTS_LOCAL}
{$UNDEF SUPPORTS_UNSAFE_WARNINGS}
{$UNDEF SUPPORTS_WEAKPACKAGEUNIT}

{$UNDEF RTL_UNIT_RTLCONSTS}
