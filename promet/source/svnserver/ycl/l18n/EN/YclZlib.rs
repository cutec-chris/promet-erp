{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclZlib - zlib, gzip stream classes - resource strings                                          }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclZlib.rs.                                                               }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2002-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of  Peter J. Haas, located at  }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

  // zlib
  RsZlibNeedDict     = 'need dictionary';
  RsZlibStreamEnd    = 'stream end';
  RsZlibOK           = '';
  RsZlibErrNo        = 'file error';
  RsZlibStreamError  = 'stream error';
  RsZlibDataError    = 'data error';
  RsZlibMemError     = 'insufficient memory';
  RsZlibBufError     = 'buffer error';
  RsZlibVersionError = 'incompatible version';
  RsZlibUnknownError = 'unknown zlib error';
  RsZlibNoSetSize    = 'TZLibStream can''t perform set size';
  RsZlibNoSeek       = 'TZLibStream can''t perform seek';
  RsZlibNoWrite      = 'TZLibReader can''t write';
  RsZlibNoRead       = 'TZLibWriter can''t read';

  // gzip
  RsGzipNoSetSize    = 'gzip stream can''t perform set size';
  RsGzipNoSeek       = 'gzip stream can''t perform seek';
  RsGzipNoWrite      = 'gzip reader can''t write';
  RsGzipNoRead       = 'gzip writer can''t read';

  RsGzipNoGZipStream          = 'no gzip stream';
  RsGzipNoDeflate             = 'no deflate compression';
  RsGzipMultipartNotSupported = 'multipart gzip files are not supported';
  RsGzipEncryptedNotSupported = 'encrypted gzip files are not supported';
  RsGzipUnknownFlags          = 'unknown flags';
  RsGzipCRCError              = 'checksum error';
  RsGzipSizeError             = 'uncompressed size error';

