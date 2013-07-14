{**************************************************************************************************}
{                                                                                                  }
{  Y library (Yl)                                                                                  }
{                                                                                                  }
{  Directives to support different compilers                                                       }
{                                                                                                  }
{  Exclusion Delphi 4 extensions, which are not exists in former Delphi versions                   }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YlExclB120.inc.                                                           }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

{$UNDEF SUPPORTS_LONGWORD}        // Compiler supports the LongWord type (unsigned 32 bit)
{$UNDEF SUPPORTS_INT64}           // Compiler supports the Int64 type
{$UNDEF SUPPORTS_DYNAMICARRAYS}   // Compiler supports dynamic arrays
{$UNDEF SUPPORTS_DEFAULTPARAMS}   // Compiler supports default parameters
{$UNDEF SUPPORTS_OVERLOAD}        // Compiler supports overloading
{$UNDEF SUPPORTS_IMPLEMENTS}      // Compiler supports implements
{$UNDEF RTL_SUPPORTS_CREATERES}   // The exception class has CreateRes, CreateResFmt methods
