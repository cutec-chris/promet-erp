{**************************************************************************************************}
{                                                                                                  }
{  Y library (Yl)                                                                                  }
{                                                                                                  }
{  Directives to support the Kylix compiler                                                        }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YlKylix.inc.                                                              }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

{$IFDEF CONDITIONALEXPRESSIONS}
  // Kylix 1, RTL 14.0
  {$IF not Declared(CompilerVersion)}
    {$I YlExclB150.inc}    // exclude Delphi 7 extensions
  // Kylix 2, RTL 14.1 .. 14.4 (14.2)
  {$ELSEIF Declared(CompilerVersion) and (CompilerVersion >= 14.1) and (CompilerVersion < 14.5)}
    {$I YlExclB150.inc}    // exclude Delphi 7 extensions
  // Kylix 3, RTL 14.5
  {$ELSEIF Declared(CompilerVersion) and (CompilerVersion = 14.5)}
    {$I YlExclB150.inc}    // exclude Delphi 7 extensions
  // > Kylix 3, > RTL 14.5
  {$ELSEIF Declared(CompilerVersion) and (CompilerVersion > 14.5)}
    Add new Kylix version
  {$IFEND}
{$ENDIF}

