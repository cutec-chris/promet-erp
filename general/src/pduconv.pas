{**********************************************
PDU Conversion Unit v1.0
(c)2004 Mpu Gondrong (wawan @ gatra . com)
Release to Public Domain, feel free to use and modify

Some of this code based on PduConv:
http://freshmeat.net/projects/pduconv/

See brief description of PDU:
http://www.dreamfabric.com/sms/
***********************************************}

unit PduConv;

interface

uses usms;

type
  TSmsPdu = record
    Center,  				      // sms center number
    Phone,   				      // mobile station number
    Text: string;  		              // sms content
    MTI: TSmsMTI;   	                      // message type indicator
    Ref,                                      // reference number
    Stat,                                     // status report
    PID,                                      // protocol identifier
    DCS: Byte;                                // data coding scheme
    SCTS: TDateTime;                          // service center time stamp
  end;

function DecodePdu(ScToMs: Boolean; const Txt: string): TSMS;
function EncodePdu(SMS : TSMS): string;

implementation

uses
	SysUtils;

{ Define Non-Printable Characters as a question mark }
const
  NPC7 = 63;
  NPC8 = Ord('?');

(***************************************************************************
		This lookup table converts from ISO-8859-1 8-bit ASCII to the
		7 bit "default alphabet" as defined in ETSI GSM 03.38

		ISO-characters that don't have any correspondning character in the
		7-bit alphabet is replaced with the NPC7-character.  If there's
		a close match between the ISO-char and a 7-bit character (for example
		the letter i with a circumflex and the plain i-character) a substitution
		is done. These "close-matches" are marked in the lookup table by
		having its value negated.

		There are some character (for example the curly brace "{") that must
		be converted into a 2 byte 7-bit sequence.  These characters are
		marked in the table by having 256 added to its value.
***************************************************************************)

lookup_ascii8to7: array [0..255] of SmallInt = (
  NPC7,       {     0      null [NUL]                              }
  NPC7,       {     1      start of heading [SOH]                  }
  NPC7,       {     2      start of text [STX]                     }
  NPC7,       {     3      end of text [ETX]                       }
  NPC7,       {     4      end of transmission [EOT]               }
  NPC7,       {     5      enquiry [ENQ]                           }
  NPC7,       {     6      acknowledge [ACK]                       }
  NPC7,       {     7      bell [BEL]                              }
  NPC7,       {     8      backspace [BS]                          }
  NPC7,       {     9      horizontal tab [HT]                     }
  10,         {    10      line feed [LF]                          }
  NPC7,       {    11      vertical tab [VT]                       }
  10+256,     {    12      form feed [FF]                          }
  13,         {    13      carriage return [CR]                    }
  NPC7,       {    14      shift out [SO]                          }
  NPC7,       {    15      shift in [SI]                           }
  NPC7,       {    16      data link escape [DLE]                  }
  NPC7,       {    17      device control 1 [DC1]                  }
  NPC7,       {    18      device control 2 [DC2]                  }
  NPC7,       {    19      device control 3 [DC3]                  }
  NPC7,       {    20      device control 4 [DC4]                  }
  NPC7,       {    21      negative acknowledge [NAK]              }
  NPC7,       {    22      synchronous idle [SYN]                  }
  NPC7,       {    23      end of trans. block [ETB]               }
  NPC7,       {    24      cancel [CAN]                            }
  NPC7,       {    25      end of medium [EM]                      }
  NPC7,       {    26      substitute [SUB]                        }
  NPC7,       {    27      escape [ESC]                            }
  NPC7,       {    28      file separator [FS]                     }
  NPC7,       {    29      group separator [GS]                    }
  NPC7,       {    30      record separator [RS]                   }
  NPC7,       {    31      unit separator [US]                     }
  32,         {    32      space                                   }
  33,         {    33    ! exclamation mark                        }
  34,         {    34    " double quotation mark                   }
  35,         {    35    # number sign                             }
  2,          {    36    $ dollar sign                             }
  37,         {    37    % percent sign                            }
  38,         {    38    & ampersand                               }
  39,         {    39    ' apostrophe                              }
  40,         {    40    ( left parenthesis                        }
  41,         {    41    ) right parenthesis                       }
  42,         {    42    * asterisk                                }
  43,         {    43    + plus sign                               }
  44,         {    44    , comma                                   }
  45,         {    45    - hyphen                                  }
  46,         {    46    . period                                  }
  47,         {    47    / slash,                                  }
  48,         {    48    0 digit 0                                 }
  49,         {    49    1 digit 1                                 }
  50,         {    50    2 digit 2                                 }
  51,         {    51    3 digit 3                                 }
  52,         {    52    4 digit 4                                 }
  53,         {    53    5 digit 5                                 }
  54,         {    54    6 digit 6                                 }
  55,         {    55    7 digit 7                                 }
  56,         {    56    8 digit 8                                 }
  57,         {    57    9 digit 9                                 }
  58,         {    58    : colon                                   }
  59,         {    59    ; semicolon                               }
  60,         {    60    < less-than sign                          }
  61,         {    61    = equal sign                              }
  62,         {    62    > greater-than sign                       }
  63,         {    63    ? question mark                           }
  0,          {    64    @ commercial at sign                      }
  65,         {    65    A uppercase A                             }
  66,         {    66    B uppercase B                             }
  67,         {    67    C uppercase C                             }
  68,         {    68    D uppercase D                             }
  69,         {    69    E uppercase E                             }
  70,         {    70    F uppercase F                             }
  71,         {    71    G uppercase G                             }
  72,         {    72    H uppercase H                             }
  73,         {    73    I uppercase I                             }
  74,         {    74    J uppercase J                             }
  75,         {    75    K uppercase K                             }
  76,         {    76    L uppercase L                             }
  77,         {    77    M uppercase M                             }
  78,         {    78    N uppercase N                             }
  79,         {    79    O uppercase O                             }
  80,         {    80    P uppercase P                             }
  81,         {    81    Q uppercase Q                             }
  82,         {    82    R uppercase R                             }
  83,         {    83    S uppercase S                             }
  84,         {    84    T uppercase T                             }
  85,         {    85    U uppercase U                             }
  86,         {    86    V uppercase V                             }
  87,         {    87    W uppercase W                             }
  88,         {    88    X uppercase X                             }
  89,         {    89    Y uppercase Y                             }
  90,         {    90    Z uppercase Z                             }
  60+256,     {    91    [ left square bracket                     }
  47+256,     {    92    \ backslash                               }
  62+256,     {    93    ] right square bracket                    }
  20+256,     {    94    ^ circumflex accent                       }
  17,         {    95    _ underscore                              }
  -39,        {    96    ` back apostrophe                         }
  97,         {    97    a lowercase a                             }
  98,         {    98    b lowercase b                             }
  99,         {    99    c lowercase c                             }
  100,        {   100    d lowercase d                             }
  101,        {   101    e lowercase e                             }
  102,        {   102    f lowercase f                             }
  103,        {   103    g lowercase g                             }
  104,        {   104    h lowercase h                             }
  105,        {   105    i lowercase i                             }
  106,        {   106    j lowercase j                             }
  107,        {   107    k lowercase k                             }
  108,        {   108    l lowercase l                             }
  109,        {   109    m lowercase m                             }
  110,        {   110    n lowercase n                             }
  111,        {   111    o lowercase o                             }
  112,        {   112    p lowercase p                             }
  113,        {   113    q lowercase q                             }
  114,        {   114    r lowercase r                             }
  115,        {   115    s lowercase s                             }
  116,        {   116    t lowercase t                             }
  117,        {   117    u lowercase u                             }
  118,        {   118    v lowercase v                             }
  119,        {   119    w lowercase w                             }
  120,        {   120    x lowercase x                             }
  121,        {   121    y lowercase y                             }
  122,        {   122    z lowercase z                             }
  40+256,     (*  123    { left brace                            *)
  64+256,     {   124    | vertical bar                            }
  41+256,     (*  125    } right brace                           *)
  61+256,     {   126    ~ tilde accent                            }
  NPC7,       {   127      delete [DEL]                            }
  NPC7,       {   128                                              }
  NPC7,       {   129                                              }
  -39,        {   130      low left rising single quote            }
  -102,       {   131      lowercase italic f                      }
  -34,        {   132      low left rising double quote            }
  NPC7,       {   133      low horizontal ellipsis                 }
  NPC7,       {   134      dagger mark                             }
  NPC7,       {   135      double dagger mark                      }
  NPC7,       {   136      letter modifying circumflex             }
  NPC7,       {   137      per thousand (mille) sign               }
  -83,        {   138      uppercase S caron or hacek              }
  -39,        {   139      left single angle quote mark            }
  -214,       {   140      uppercase OE ligature                   }
  NPC7,       {   141                                              }
  NPC7,       {   142                                              }
  NPC7,       {   143                                              }
  NPC7,       {   144                                              }
  -39,        {   145      left single quotation mark              }
  -39,        {   146      right single quote mark                 }
  -34,        {   147      left double quotation mark              }
  -34,        {   148      right double quote mark                 }
  -42,        {   149      round filled bullet                     }
  -45,        {   150      en dash                                 }
  -45,        {   151      em dash                                 }
  -39,        {   152      small spacing tilde accent              }
  NPC7,       {   153      trademark sign                          }
  -115,       {   154      lowercase s caron or hacek              }
  -39,        {   155      right single angle quote mark           }
  -111,       {   156      lowercase oe ligature                   }
  NPC7,       {   157                                              }
  NPC7,       {   158                                              }
  -89,        {   159      uppercase Y dieresis or umlaut          }
  -32,        {   160      non-breaking space                      }
  64,         {   161    ¡ inverted exclamation mark               }
  -99,        {   162    ¢ cent sign                               }
  1,          {   163    £ pound sterling sign                     }
  36,         {   164    ¤ general currency sign                   }
  3,          {   165    ¥ yen sign                                }
  -33,        {   166    ¦ broken vertical bar                     }
  95,         {   167    § section sign                            }
  -34,        {   168    ¨ spacing dieresis or umlaut              }
  NPC7,       {   169    © copyright sign                          }
  NPC7,       {   170    ª feminine ordinal indicator              }
  -60,        {   171    « left (double) angle quote               }
  NPC7,       {   172    ¬ logical not sign                        }
  -45,        {   173    ­ soft hyphen                             }
  NPC7,       {   174    ® registered trademark sign               }
  NPC7,       {   175    ¯ spacing macron (long) accent            }
  NPC7,       {   176    ° degree sign                             }
  NPC7,       {   177    ± plus-or-minus sign                      }
  -50,        {   178    ² superscript 2                           }
  -51,        {   179    ³ superscript 3                           }
  -39,        {   180    ´ spacing acute accent                    }
  -117,       {   181    µ micro sign                              }
  NPC7,       {   182    ¶ paragraph sign, pilcrow sign            }
  NPC7,       {   183    · middle dot, centered dot                }
  NPC7,       {   184    ¸ spacing cedilla                         }
  -49,        {   185    ¹ superscript 1                           }
  NPC7,       {   186    º masculine ordinal indicator             }
  -62,        {   187    » right (double) angle quote (guillemet)  }
  NPC7,       {   188    ¼ fraction 1/4                            }
  NPC7,       {   189    ½ fraction 1/2                            }
  NPC7,       {   190    ¾ fraction 3/4                            }
  96,         {   191    ¿ inverted question mark                  }
  -65,        {   192    À uppercase A grave                       }
  -65,        {   193    Á uppercase A acute                       }
  -65,        {   194    Â uppercase A circumflex                  }
  -65,        {   195    Ã uppercase A tilde                       }
  91,         {   196    Ä uppercase A dieresis or umlaut          }
  14,         {   197    Å uppercase A ring                        }
  28,         {   198    Æ uppercase AE ligature                   }
  9,          {   199    Ç uppercase C cedilla                     }
  -31,        {   200    È uppercase E grave                       }
  31,         {   201    É uppercase E acute                       }
  -31,        {   202    Ê uppercase E circumflex                  }
  -31,        {   203    Ë uppercase E dieresis or umlaut          }
  -73,        {   204    Ì uppercase I grave                       }
  -73,        {   205    Í uppercase I acute                       }
  -73,        {   206    Î uppercase I circumflex                  }
  -73,        {   207    Ï uppercase I dieresis or umlaut          }
  -68,        {   208    Ð uppercase ETH                           }
  93,         {   209    Ñ uppercase N tilde                       }
  -79,        {   210    Ò uppercase O grave                       }
  -79,        {   211    Ó uppercase O acute                       }
  -79,        {   212    Ô uppercase O circumflex                  }
  -79,        {   213    Õ uppercase O tilde                       }
  92,         {   214    Ö uppercase O dieresis or umlaut          }
  -42,        {   215    × multiplication sign                     }
  11,         {   216    Ø uppercase O slash                       }
  -85,        {   217    Ù uppercase U grave                       }
  -85,        {   218    Ú uppercase U acute                       }
  -85,        {   219    Û uppercase U circumflex                  }
  94,         {   220    Ü uppercase U dieresis or umlaut          }
  -89,        {   221    Ý uppercase Y acute                       }
  NPC7,       {   222    Þ uppercase THORN                         }
  30,         {   223    ß lowercase sharp s, sz ligature          }
  127,        {   224    à lowercase a grave                       }
  -97,        {   225    á lowercase a acute                       }
  -97,        {   226    â lowercase a circumflex                  }
  -97,        {   227    ã lowercase a tilde                       }
  123,        {   228    ä lowercase a dieresis or umlaut          }
  15,         {   229    å lowercase a ring                        }
  29,         {   230    æ lowercase ae ligature                   }
  -9,         {   231    ç lowercase c cedilla                     }
  4,          {   232    è lowercase e grave                       }
  5,          {   233    é lowercase e acute                       }
  -101,       {   234    ê lowercase e circumflex                  }
  -101,       {   235    ë lowercase e dieresis or umlaut          }
  7,          {   236    ì lowercase i grave                       }
  7,          {   237    í lowercase i acute                       }
  -105,       {   238    î lowercase i circumflex                  }
  -105,       {   239    ï lowercase i dieresis or umlaut          }
  NPC7,       {   240    ð lowercase eth                           }
  125,        {   241    ñ lowercase n tilde                       }
  8,          {   242    ò lowercase o grave                       }
  -111,       {   243    ó lowercase o acute                       }
  -111,       {   244    ô lowercase o circumflex                  }
  -111,       {   245    õ lowercase o tilde                       }
  124,        {   246    ö lowercase o dieresis or umlaut          }
  -47,        {   247    ÷ division sign                           }
  12,         {   248    ø lowercase o slash                       }
  6,          {   249    ù lowercase u grave                       }
  -117,       {   250    ú lowercase u acute                       }
  -117,       {   251    û lowercase u circumflex                  }
  126,        {   252    ü lowercase u dieresis or umlaut          }
  -121,       {   253    ý lowercase y acute                       }
  NPC7,       {   254    þ lowercase thorn                         }
  -121        {   255    ÿ lowercase y dieresis or umlaut          }
);

{***************************************************************************
	 This lookup table converts from the 7 bit "default alphabet" as
		defined in ETSI GSM 03.38 to a standard ISO-8859-1 8-bit ASCII.

		Some characters in the 7-bit alphabet does not exist in the ISO
		character set, they are replaced by the NPC8-character.

		If the character is decimal 27 (ESC) the following character have
		a special meaning and must be handled separately.
***************************************************************************}

lookup_ascii7to8: array[0..127] of Byte = (
  64,         {  0      @  COMMERCIAL AT                           }
  163,        {  1      £  POUND SIGN                              }
  36,         {  2      $  DOLLAR SIGN                             }
  165,        {  3      ¥  YEN SIGN                                }
  232,        {  4      è  LATIN SMALL LETTER E WITH GRAVE         }
  233,        {  5      é  LATIN SMALL LETTER E WITH ACUTE         }
  249,        {  6      ù  LATIN SMALL LETTER U WITH GRAVE         }
  236,        {  7      ì  LATIN SMALL LETTER I WITH GRAVE         }
  242,        {  8      ò  LATIN SMALL LETTER O WITH GRAVE         }
  199,        {  9      Ç  LATIN CAPITAL LETTER C WITH CEDILLA     }
  10,         {  10        LINE FEED                               }
  216,        {  11     Ø  LATIN CAPITAL LETTER O WITH STROKE      }
  248,        {  12     ø  LATIN SMALL LETTER O WITH STROKE        }
  13,         {  13        CARRIAGE RETURN                         }
  197,        {  14     Å  LATIN CAPITAL LETTER A WITH RING ABOVE  }
  229,        {  15     å  LATIN SMALL LETTER A WITH RING ABOVE    }
  NPC8,       {  16        GREEK CAPITAL LETTER DELTA              }
  95,         {  17     _  LOW LINE                                }
  NPC8,       {  18        GREEK CAPITAL LETTER PHI                }
  NPC8,       {  19        GREEK CAPITAL LETTER GAMMA              }
  NPC8,       {  20        GREEK CAPITAL LETTER LAMBDA             }
  NPC8,       {  21        GREEK CAPITAL LETTER OMEGA              }
  NPC8,       {  22        GREEK CAPITAL LETTER PI                 }
  NPC8,       {  23        GREEK CAPITAL LETTER PSI                }
  NPC8,       {  24        GREEK CAPITAL LETTER SIGMA              }
  NPC8,       {  25        GREEK CAPITAL LETTER THETA              }
  NPC8,       {  26        GREEK CAPITAL LETTER XI                 }
  27,         {  27        ESCAPE TO EXTENSION TABLE               }
  198,        {  28     Æ  LATIN CAPITAL LETTER AE                 }
  230,        {  29     æ  LATIN SMALL LETTER AE                   }
  223,        {  30     ß  LATIN SMALL LETTER SHARP S (German)     }
  201,        {  31     É  LATIN CAPITAL LETTER E WITH ACUTE       }
  32,         {  32        SPACE                                   }
  33,         {  33     !  EXCLAMATION MARK                        }
  34,         {  34     "  QUOTATION MARK                          }
  35,         {  35     #  NUMBER SIGN                             }
  164,        {  36     ¤  CURRENCY SIGN                           }
  37,         {  37     %  PERCENT SIGN                            }
  38,         {  38     &  AMPERSAND                               }
  39,         {  39     '  APOSTROPHE                              }
  40,         {  40     (  LEFT PARENTHESIS                        }
  41,         {  41     )  RIGHT PARENTHESIS                       }
  42,         {  42     *  ASTERISK                                }
  43,         {  43     +  PLUS SIGN                               }
  44,         {  44     ,  COMMA                                   }
  45,         {  45     -  HYPHEN-MINUS                            }
  46,         {  46     .  FULL STOP                               }
  47,         {  47     /  SOLIDUS (SLASH)                         }
  48,         {  48     0  DIGIT ZERO                              }
  49,         {  49     1  DIGIT ONE                               }
  50,         {  50     2  DIGIT TWO                               }
  51,         {  51     3  DIGIT THREE                             }
  52,         {  52     4  DIGIT FOUR                              }
  53,         {  53     5  DIGIT FIVE                              }
  54,         {  54     6  DIGIT SIX                               }
  55,         {  55     7  DIGIT SEVEN                             }
  56,         {  56     8  DIGIT EIGHT                             }
  57,         {  57     9  DIGIT NINE                              }
  58,         {  58     :  COLON                                   }
  59,         {  59     ;  SEMICOLON                               }
  60,         {  60     <  LESS-THAN SIGN                          }
  61,         {  61     =  EQUALS SIGN                             }
  62,         {  62     >  GREATER-THAN SIGN                       }
  63,         {  63     ?  QUESTION MARK                           }
  161,        {  64     ¡  INVERTED EXCLAMATION MARK               }
  65,         {  65     A  LATIN CAPITAL LETTER A                  }
  66,         {  66     B  LATIN CAPITAL LETTER B                  }
  67,         {  67     C  LATIN CAPITAL LETTER C                  }
  68,         {  68     D  LATIN CAPITAL LETTER D                  }
  69,         {  69     E  LATIN CAPITAL LETTER E                  }
  70,         {  70     F  LATIN CAPITAL LETTER F                  }
  71,         {  71     G  LATIN CAPITAL LETTER G                  }
  72,         {  72     H  LATIN CAPITAL LETTER H                  }
  73,         {  73     I  LATIN CAPITAL LETTER I                  }
  74,         {  74     J  LATIN CAPITAL LETTER J                  }
  75,         {  75     K  LATIN CAPITAL LETTER K                  }
  76,         {  76     L  LATIN CAPITAL LETTER L                  }
  77,         {  77     M  LATIN CAPITAL LETTER M                  }
  78,         {  78     N  LATIN CAPITAL LETTER N                  }
  79,         {  79     O  LATIN CAPITAL LETTER O                  }
  80,         {  80     P  LATIN CAPITAL LETTER P                  }
  81,         {  81     Q  LATIN CAPITAL LETTER Q                  }
  82,         {  82     R  LATIN CAPITAL LETTER R                  }
  83,         {  83     S  LATIN CAPITAL LETTER S                  }
  84,         {  84     T  LATIN CAPITAL LETTER T                  }
  85,         {  85     U  LATIN CAPITAL LETTER U                  }
  86,         {  86     V  LATIN CAPITAL LETTER V                  }
  87,         {  87     W  LATIN CAPITAL LETTER W                  }
  88,         {  88     X  LATIN CAPITAL LETTER X                  }
  89,         {  89     Y  LATIN CAPITAL LETTER Y                  }
  90,         {  90     Z  LATIN CAPITAL LETTER Z                  }
  196,        {  91     Ä  LATIN CAPITAL LETTER A WITH DIAERESIS   }
  214,        {  92     Ö  LATIN CAPITAL LETTER O WITH DIAERESIS   }
  209,        {  93     Ñ  LATIN CAPITAL LETTER N WITH TILDE       }
  220,        {  94     Ü  LATIN CAPITAL LETTER U WITH DIAERESIS   }
  167,        {  95     §  SECTION SIGN                            }
  191,        {  96     ¿  INVERTED QUESTION MARK                  }
  97,         {  97     a  LATIN SMALL LETTER A                    }
  98,         {  98     b  LATIN SMALL LETTER B                    }
  99,         {  99     c  LATIN SMALL LETTER C                    }
  100,        {  100    d  LATIN SMALL LETTER D                    }
  101,        {  101    e  LATIN SMALL LETTER E                    }
  102,        {  102    f  LATIN SMALL LETTER F                    }
  103,        {  103    g  LATIN SMALL LETTER G                    }
  104,        {  104    h  LATIN SMALL LETTER H                    }
  105,        {  105    i  LATIN SMALL LETTER I                    }
  106,        {  106    j  LATIN SMALL LETTER J                    }
  107,        {  107    k  LATIN SMALL LETTER K                    }
  108,        {  108    l  LATIN SMALL LETTER L                    }
  109,        {  109    m  LATIN SMALL LETTER M                    }
  110,        {  110    n  LATIN SMALL LETTER N                    }
  111,        {  111    o  LATIN SMALL LETTER O                    }
  112,        {  112    p  LATIN SMALL LETTER P                    }
  113,        {  113    q  LATIN SMALL LETTER Q                    }
  114,        {  114    r  LATIN SMALL LETTER R                    }
  115,        {  115    s  LATIN SMALL LETTER S                    }
  116,        {  116    t  LATIN SMALL LETTER T                    }
  117,        {  117    u  LATIN SMALL LETTER U                    }
  118,        {  118    v  LATIN SMALL LETTER V                    }
  119,        {  119    w  LATIN SMALL LETTER W                    }
  120,        {  120    x  LATIN SMALL LETTER X                    }
  121,        {  121    y  LATIN SMALL LETTER Y                    }
  122,        {  122    z  LATIN SMALL LETTER Z                    }
  228,        {  123    ä  LATIN SMALL LETTER A WITH DIAERESIS     }
  246,        {  124    ö  LATIN SMALL LETTER O WITH DIAERESIS     }
  241,        {  125    ñ  LATIN SMALL LETTER N WITH TILDE         }
  252,        {  126    ü  LATIN SMALL LETTER U WITH DIAERESIS     }
  224         {  127    à  LATIN SMALL LETTER A WITH GRAVE         }
);

(*  The double bytes below must be handled separately after the
		table lookup.
		12             27 10      FORM FEED
		94             27 20   ^  CIRCUMFLEX ACCENT
		123            27 40   {  LEFT CURLY BRACKET
		125            27 41   }  RIGHT CURLY BRACKET
		92             27 47   \  REVERSE SOLIDUS (BACKSLASH)
		91             27 60   [  LEFT SQUARE BRACKET
		126            27 61   ~  TILDE
		93             27 62   ]  RIGHT SQUARE BRACKET
		124            27 64   |  VERTICAL BAR
*)

function HexToInt(W: PWord): Byte;
var
  B, C: Byte;
begin
  B := Lo(W^) - Ord('0');
  if B > 9 then Dec(B, 7);
  C := Hi(W^) - Ord('0');
  if C > 9 then Dec(C, 7);
  Result := B shl 4 + C;
end;

function IntToHex(B: Byte): Word;
var
  C: Byte;
begin
  C := (B and $F);
  B := B shr 4;
  if C > 9 then Inc(C, 7);
  if B > 9 then Inc(B, 7);
  Result := (C + Ord('0')) shl 8 or (B + Ord('0'));
end;

function PduToDec(P: PWord; Len: Cardinal): string;
var
  W: PWord;
begin
  SetLength(Result, Len);
  W := @Result[1];
  while Len > 0 do
    begin
      W^ := Swap(P^);
      Inc(P);
      Inc(W);
      Dec(Len, 2);
    end;
  if (PChar(W)-1)^ = 'F' then
    SetLength(Result, Length(Result)-1);
end;

function DecToPdu(P: PWord; Len: Cardinal): string;
var
  W: PByte;
begin
  // decimal semi octet --
  if Odd(Len) then
    Inc(Len);
  SetLength(Result, Len div 2);
  W := @Result[1];
  while Len > 0 do
    begin
      W^ := (Hi(P^) shr 4 + Ord('0')) or (Lo(P^) + Ord('0'));
      Inc(P);
      Inc(W);
      Dec(Len, 2);
    end;
end;

function PduToBin(P: PWord; Len: Cardinal): string;
var
  W: PByte;
  I: Integer;
begin
  // make hex_pdu to bin_pdu --
  SetLength(Result, Len div 2);
  W := @Result[1];
  for I := 1 to Len div 2 do
    begin
      W^ := HexToInt(P);
      Inc(P); Inc(W);
    end;
end;

{
 * Use a lookup table to convert from the 7-bit default alphabet
 * used by SMS to an ISO-8859-1 ASCII string.
 *  a7bit   An array of the 7-bit 'string' to convert
 }
function Convert_7bit_To_Ascii(const a7bit: string): string;
var
  R: Integer;
  W: PChar;
  C: Char;
begin
  SetLength(Result, Length(a7bit) * 2);
  R := 0; W := @Result[1];
  while R < Length(a7bit) do
    begin
      Inc(R);
      C := Chr(lookup_ascii7to8[Ord(a7bit[R])]);
      if C = #27 then
        begin
	  Inc(R);
	  case Ord(a7bit[R]) of
	  10: C := #12;
	  20: C := '^';
          40: C := '{';
          41: C := '}';
          47: C := '\';
          60: C := '[';
          61: C := '~';
          62: C := ']';
          64: C := '|';
          else C := Chr(NPC8);
	  end;
	end;
      W^ := C;
      Inc(W);
    end;
  SetLength(Result, W - @Result[1]);
end;

{
 * Use a lookup table to convert from an ISO-8859-1 string to 
 * the 7-bit default alphabet used by SMS.
 *  ascii   The string to convert
 }
function convert_ascii_to_7bit(const Ascii: string): string;
var
  I: Integer;
  W: PChar;
  M: SmallInt;
begin
  SetLength(Result, Length(Ascii) * 2);
  W := @Result[1];
  for I := 1 to Length(Ascii) do
    begin
      M := Abs(lookup_ascii8to7[Ord(ascii[I])]);
      if M > 256 then
        begin
          W^ := #27;
	  Inc(W);
	  Dec(M, 256);
	end;
      W^ := Chr(M);
      Inc(W);
    end;
  SetLength(Result, W - @Result[1]);
end;

{
 *  Convert a PDU-coded string to ISO-8859-1 ASCII
 *  *pdu      The pdu-array to convert to cleartext
 }
function PduToAscii(W: PWord; Len: Cardinal): string;
var
  R: Integer;
  C: PChar;
  S: string;
begin
  SetLength(Result, Len);
  S := PduToBin(W, Len);
  C := @Result[1];
  for R := 0 to Length(S)-1 do
    begin
      if R mod 7 = 0 then
  	C^ := Chr(Ord(S[R+1]) and $7F)
      else if R mod 7 = 6 then
        begin
	  C^ := Chr(((Ord(S[R+1]) shl 6) or (Ord(S[R]) shr 2)) and $7F);
	  Inc(C);
          C^ := Chr(Ord(S[R+1]) shr 1 and $7F);
	end
      else
	C^ := Chr(((Ord(S[R+1]) shl (R mod 7)) or (Ord(S[R]) shr (7+1 - (R mod 7)))) and $7F);
      Inc(C);
    end;
  SetLength(Result, C - @Result[1]);
  Result := Convert_7bit_To_Ascii(Result);
end;

{
 *  Convert an ISO-8859-1 ASCII string to an array of PDU-coded bytes
 *  *ascii  The ISO-cleartext to convert
 }
function AsciiToPdu(const Ascii: string; P: PWord): Integer;
var
  I: Cardinal;
  R: PChar;
  S: string;
begin
  S := Convert_Ascii_To_7bit(Ascii);
  R := @S[1];
  Result := Length(S);
  for I := 0 to Result-1 do
    begin
      P^ := IntToHex((Ord(R^) shr (I mod 7) and $7F) or	(Ord((R+1)^) shl (7-(I mod 7)) and $FF));
      if (I mod 7) = 6 then
        Inc(R);
      Inc(R);
      Inc(P);
    end;
  Result := (Result - Result div 8) * 2;
end;

function PduToDateTime(W: PWord): TDateTime;
type
  TScts = packed record
    Y,
    M,
    D,
    H,
    N,
    S: Byte;
    T: ShortInt;
  end;
  PScts = ^TScts;
var
  S: string;
  Cen: Word;
  I: Integer;
  B: PByte;
begin
  S := PduToBin(W, 14);
  B := @S[1];
  for I := 1 to 7 do
    begin
      B^ := (B^ and $0F) * 10 + B^ shr 4;
      Inc(B);
    end;
  with PScts(@S[1])^ do
    begin
      if Y > 95 then Cen := 1900 else Cen := 2000;
	Result := EncodeDate(Cen + Y, M, D) + EncodeTime(H, N, S, 0) - (T / 96) + (7 / 24);  // GMT+7
    end;
end;

{ Decode PDU text to TSmsPdu structure
	ScToMs: True if PDU from SMS Center, False if from Mobile Station
	Txt: PDU text
	Result: TSMSPdu record
}
function DecodePdu(ScToMs: Boolean; const Txt: string): TSMS;
type
  TPDUAddr = packed record
    Len,
    Toa,
    Str: Word;
  end;
  PPDUAddr = ^TPDUAddr;
var
  I,
  J: Integer;
begin
  I := 1;
  with PPDUAddr(@Txt[I])^ do
    begin
      J := HexToInt(@Len)* 2;
{      if J > 0 then
        Result.Center := PduToDec(@Str, J-2);}
      Inc(I, J + 2);
    end;
  // type of PDU (from first octet) --
//  Result.Fio := TSmsFios(HexToInt(@Txt[I]));
  Inc(I, 2);
  case Byte(HexToInt(@Txt[I])) and 3 of
  0:if ScToMs then Result.MessageTyp := mtiDeliver
    else
      begin
        Result.MessageTyp := mtiDeliverRep;
	Result.Reference := HexToInt(@Txt[I]);
	Exit;
      end;
  1:if ScToMs then
      begin
        Result.MessageTyp := mtiSubmitRep;
        Result.Reference := HexToInt(@Txt[I]);
        Exit;
      end
    else
      begin
	Result.MessageTyp := mtiSubmit;
	Exit;
      end;
  2:if ScToMs then
      begin
	Result.MessageTyp := mtiStatusRep;
	Result.Reference := HexToInt(@Txt[I]);
	Inc(I, 2);
      end
    else
      begin
	Result.MessageTyp := mtiCommand;
	Exit;
      end;
  end;
  // only sms_deliver & status_report is processed --
  with PPDUAddr(@Txt[I])^ do
    begin
      J := HexToInt(@Len);
      if Odd(J) then Inc(J);
        Result.Reciver := TPhoneNumber.Create(PduToDec(@Str, J));
	Inc(I, J + 4);
    end;
  if Result.MessageTyp = mtiDeliver then
    begin
      Result.ProtocolID := HexToInt(@Txt[I]);
      Result.DataCodingSheme := HexToInt(@Txt[I+2]);
      Inc(I, 4);
    end;
  Result.TimeStamp := PduToDateTime(@Txt[I]);
  Inc(I, 14);
  if Result.MessageTyp = mtiDeliver then
    begin
      J := HexToInt(@Txt[I]);
      Result.Text := PduToAscii(@Txt[I+2], Length(Txt) - I);
      if Length(Result.Text) > J then
        SetLength(Result.Text, J);
    end
  else
    begin
      Result.Text := '';
//      Result.Stat := HexToInt(@Txt[I+14]);  // 0..2 = success
    end;
end;

{ Encode ascii text to PDU text (to submit via mobile station)
	Str: Plain text to encode
	Phone: Mobile station number
	Result: PDU text
}
function EncodePdu(SMS : TSMS): string;
type
  TPduSubmit = packed record
    Smsc_Len,
    Fo,
    Ref,
    Rcv_Len,
    Rcv_Toa: Word;
  end;
  PPduSubmit = ^TPduSubmit;
var
  W: PWord;
  Phone : string;
//  B: TSmsFios;
  I: Integer;
begin
  SetLength(Result, Length(SMS.Text)*2 + Length(SMS.Reciver.BuildModemCompatibleNumber)*2 + 9*2);
  Phone := SMS.Reciver.BuildModemCompatibleNumber;
//  B := [fioSR, fioVP1, fioMM_RD, fioMT2];
  with PPduSubmit(@Result[1])^ do
    begin
      Smsc_Len := $3030; // 00: without service center
//      Fo := IntToHex(Byte(B));
      Ref := $3030; 		 // 00: ref from phone
      Rcv_Len := IntToHex(Length(Phone));
      Rcv_Toa := $3139;  // 91: internat. format
    end;
  W := @Result[SizeOf(TPduSubmit)+1];
  for I := 1 to Length(Phone) shr 1 do
    begin
      W^ := Swap(PWord(@Phone[(I-1)*2+1])^);
      Inc(W);
    end;
  if Odd(Length(Phone)) then
    begin
      W^ := Swap($4600 + Ord(Phone[Length(Phone)]));
      Inc(W);
    end;
  W^ := $3030; // PID: 00
  Inc(W);
  W^ := $3030; // DCS: 00
  Inc(W);
  W^ := $4141; // VP: AA (4 days)
  Inc(W);
  W^ := IntToHex(Length(SMS.Text));
  Inc(W);
  SetLength(Result, PChar(W) - @Result[1] + AsciiToPdu(SMS.Text, W));
end;

end.
