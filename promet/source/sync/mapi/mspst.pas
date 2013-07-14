unit mspst;

interface

uses MAPIdefs;

(*
 *  M S P S T . H
 *  
 *  This file lists internal properties of the Microsoft Personal
 *  Information Store
 *  
 *  Copyright 1986-1996 Microsoft Corporation. All Rights Reserved.
 *)

//#ifndef _MSPST_H_
//#define _MSPST_H_


(*  The following is a list of properties that may be passed in
    as the properties in the array of SPropValue structure on the
    MsgServiceConfigure function.
    
    Creating a PST profile section through CreateMsgService.
    The creation of the actual PST file is a two step process.  First the
    client should call CreateMsgService to setup the profile section and then
    ConfigureMsgService to create the PST file.
    The CreateMsgService call will setup the PR_DISPLAY_NAME property in the
    profile section to be used on the PST when it is created.

    Configuring an PST file through ConfigureMsgService.
    The configuration of an PST can take two forms, either configuring an
    existing PST or creating a new PST.  The Microsoft Personal Information
    Store provider will try to find the necessary properties by first looking
    in the array of SPropValue structures provided by the client and then in the
    profile section, except for PR_PST_PW_SZ_OLD for which it will only look
    in the array of properties.

    The Microsoft PST provider will try to open the file specified by the
    PR_PST_PATH property, using the password given in the PR_PST_PW_SZ_OLD
    property.  If it finds a file and it recognizes it as a PST
    file, it will start the configuration routine.  Otherwise it will start the
    creation routine.

    The configuration routine will look for the PR_DISPLAY_NAME_A and
    PR_COMMENT_A properties and set them in the message store object.  Then it
    will look for the PR_PST_REMEMBER_PW property to decide if it should
    remember the password in the profile. (If not found then it will defaut to
    the current status of the profile password.)  Then if it is supposed to
    use UI, it will display the configuration property sheet to the user.  After
    all has succeeded, it will update the profile.

    The creation routine will follow one of two paths to get the PR_PST_PATH
    property.  If it is supposed to use UI it will always display the file open
    dialog to confirm the path passed in or allow the user to change it.  If
    the user chooses an existing file and it recognizes it as an PST it will
    drop back to the configuration routine.  If the user chooses an existing
    file and it is not recognized as an PST file, the user will be given the
    option of choosing another file or  creating a new PST in its place, in
    which case is will continue with the create routine.  If the user chooses
    a new file it will continue with the create routine.  If the routine is not
    allowed to use UI, then the routine will create a file at the given path
    even if another file exists there.

    Once it decides to continue with the creation process it will get the
    PR_DISPLAY_NAME, PR_COMMENT, PR_PST_ENCRYPTION, and PR_PST_SZ_PW_NEW
    properties.  If it is supposed to use UI, it will use these to initialize
    the creation dialog and get any changes the user want.  Then it will create
    a new file and update the profile.
    
    PR_DISPLAY_NAME_A   display name for the PST service
    PR_COMMENT_A        comment to the place on the PST store object
    PR_PST_PATH         location the store to create or configure
    PR_PST_REMEMBER_PW  whether or not the remember the password in the profile
    PR_PST_ENCRYPTION   encryption level at which to create the file
    PR_PST_PW_SZ_OLD    password of the PST being configured
    PR_PST_PW_SZ_NEW    password to use for future access to the PST
*)

const

PST_EXTERN_PROPID_BASE =         $6700;
PR_PST_PATH = (($6700) shl 16) or PT_STRING8;
PR_PST_REMEMBER_PW = (($6701) shl 16) or PT_BOOLEAN;
PR_PST_ENCRYPTION = (($6702) shl 16) or PT_LONG;
PR_PST_PW_SZ_NEW = (($6704) shl 16) or PT_STRING8; 
PR_PST_PW_SZ_OLD = (($6703) shl 16) or PT_STRING8;
    
PSTF_NO_ENCRYPTION              = $80000000;
PSTF_COMPRESSABLE_ENCRYPTION    = $40000000;
PSTF_BEST_ENCRYPTION            = $20000000;

(*
 *  PR_MDB_PROVIDER is the GUID that represent the Microsoft Personal
 *  Information Store.  This guid is available as a property in the stores
 *  table and on the message store and status objects.
 *)
MSPST_UID_PROVIDER	: array[0..15] of byte = (   $4e, $49, $54, $41,
                                $f9, $bf, $b8, $01,
                                $00, $aa, $00, $37,
                                $d9, $6e, $00, $00 );

//#endif  /* _MSPST_H_ */

//MS-PST.pdf
PidTagReplVersionhistory = $0e340102;
PidTagReplFlags = $0e380003;
PidTagPstLrnorestrictions = $6633000b;
PidTagLatestPstEnsure = $66fa0003;
PidTagPstPassword = $67ff0003;


implementation

end.
