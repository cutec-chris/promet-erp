{**************************************************************************************************}
{                                                                                                  }
{  Y library (Yl)                                                                                  }
{                                                                                                  }
{  Directives to support different compilers                                                       }
{                                                                                                  }
{  Exclusion Delphi/BCB 6 extensions, which are not exists in former Delphi/BCB versions           }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YlExclB140.inc.                                                           }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

{$UNDEF SUPPORTS_PREDECLARED_PTRS}  // Compiler supports predeclared pointer types (PByte, ...)

{$UNDEF SUPPORTS_DEPRECATED}        // Compiler supports the deprecated directive
{$UNDEF SUPPORTS_PLATFORM}          // Compiler supports the platform directive
{$UNDEF SUPPORTS_LIBRARY}           // Compiler supports the library directive
{$UNDEF SUPPORTS_LOCAL}             // Compiler supports the local directive
{$UNDEF SUPPORTS_CUSTOMVARIANTS}    // Compiler supports custom variants
{$UNDEF SUPPORTS_VARARGS}           // Compiler supports varargs
{$UNDEF SUPPORTS_ENUMVALUE}         // Compiler supports values for enums
{$UNDEF SUPPORTS_WEAKPACKAGEUNIT}   // Compiler supports the WEAKPACKAGEUNIT directive (don't valid for trial)

{$UNDEF RTL_UNIT_RTLCONSTS}         // The unit RTLConsts exists
{$UNDEF RTL_UNIT_TYPES}             // The unit Types exists
{$UNDEF RTL_UNIT_VARIANTS}          // The unit Variants exists
{$UNDEF RTL_UNIT_STRUTILS}          // The unit StrUtils exists
{$UNDEF RTL_XPLATFORM}              // The RTL supports crossplatform function names (e.g. RaiseLastOSError)

