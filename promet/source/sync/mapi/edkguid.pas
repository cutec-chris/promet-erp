unit edkguid;

interface

   
  const
  IID_IExchangeManageStore : TGUID  = '{559D10B0-A772-11CD-9BC8-00AA002FC45A}';
  strIID_IExchangeManageStore       = '{559d10b0-a772-11cd-9bc8-00AA002fc45a}';

  IID_IExchangeManageStore2 : TGUID = '{b6dca470-0ff3-11d0-a409-00c04fd7bd87}';
  strIID_IExchangeManageStore2      = '{b6dca470-0ff3-11d0-a409-00c04fd7bd87}';

  IID_IExchangeManageStore3   : TGUID = '{166d9bc2-db75-44a9-8a93-9f3ffc994d76}' ;
  strIID_IExchangeManageStore3        = '{166d9bc2-db75-44a9-8a93-9f3ffc994d76}' ;

  IID_IExchangeManageStore4   : TGUID = '{2590FF87-C431-4f9c-B1A8-CD69D760CD10}';	// {2590FF87-C431-4f9c-B1A8-CD69D760CD10}
  strIID_IExchangeManageStore4        = '{2590FF87-C431-4f9c-B1A8-CD69D760CD10}';	// {2590FF87-C431-4f9c-B1A8-CD69D760CD10}


  IID_IExchangeModifyTable : TGUID  = '{2d734cb0-53fd-101b-b19d-08002b3056e3}';
  strIID_IExchangeModifyTable       = '{2d734cb0-53fd-101b-b19d-08002b3056e3}';

  IID_IExchangeRuleAction  : TGUID  = '{74bba840-c93a-11ce-9581-00aa005742f7}';
  strIID_IExchangeRuleAction        = '{74bba840-c93a-11ce-9581-00aa005742f7}';

  IID_IExchangeExportChanges:TGUID  = '{a3ea9cc0-d1b2-11cd-80fc-00aa004bba0b}';
  strIID_IExchangeExportChanges     = '{a3ea9cc0-d1b2-11cd-80fc-00aa004bba0b}';

  IID_IExchangeExportChanges2 : TGUID = '{387cebe0-f53f-11cf-a48f-00c04fd65595}';
  strIID_IExchangeExportChanges2      = '{387cebe0-f53f-11cf-a48f-00c04fd65595}';

  IID_IExchangeExportChanges3 : TGUID = '{702e7f86-50a6-11d1-abd6-00a0c905660a}';
  strIID_IExchangeExportChanges3      = '{702e7f86-50a6-11d1-abd6-00a0c905660a}';

  IID_IExchangeImportHierarchyChanges : TGUID = '{85a66cf0-d0e0-11cd-80fc-00aa004bba0b}';
  strIID_IExchangeImportHierarchyChanges      = '{85a66cf0-d0e0-11cd-80fc-00aa004bba0b}';


  IID_IExchangeImportContentsChanges : TGUID = '{f75abfa0-d0e0-11cd-80fc-00aa004bba0b}';
  strIID_IExchangeImportContentsChanges = '{f75abfa0-d0e0-11cd-80fc-00aa004bba0b}';

  IID_IExchangeImportContentsChanges2  : TGUID = '{7dfdd720-f53f-11cf-a48f-00c04fd65595}';
  strIID_IExchangeImportContentsChanges2       = '{7dfdd720-f53f-11cf-a48f-00c04fd65595}';

  IID_IExchangeChangeAdvisor           : TGUID = '{1e300720-a839-11cf-bde0-00004c7531e3}';
  strIID_IExchangeChangeAdvisor                = '{1e300720-a839-11cf-bde0-00004c7531e3}';

  IID_IExchangeMessageConversion       : TGUID = '{3532b360-d114-11cf-a83b-00c04fd65597}';
  strIID_IExchangeMessageConversion            = '{3532b360-d114-11cf-a83b-00c04fd65597}';

  IID_IExchangeFavorites               : TGUID = '{cf4f3bc0-ec66-11ce-b31c-00aa00574cc6}';
  strIID_IExchangeFavorites                    = '{cf4f3bc0-ec66-11ce-b31c-00aa00574cc6}';

  IID_IExchangeNntpNewsfeed            : TGUID = '{380f41c0-3cdc-11d0-9792-00c04fd6551d}';
  strIID_IExchangeNntpNewsfeed                 = '{380f41c0-3cdc-11d0-9792-00c04fd6551d}';

(*
	The following interface GUID is the start of a range of IID's
	Each IID specifies a different CPID.  The IID corresponding to
	cpid 932 would be defined by 
		DEFINE_GUID(IID_IExchangeMessageCpid0, 932, 0x1ae9, 0x11cf,	0x84, 0xe0, 0x00, 0xaa, 0x00, 0x6b, 04f, 0xae);
*)

//DEFINE_GUID(IID_IExchangeMessageCpid0, 0, 0x1ae9,0x11cf, 0x84, 0xe0, 0x00, 0xaa, 0x00, 0x6b, 0x4f, 0xae);


implementation

end.
