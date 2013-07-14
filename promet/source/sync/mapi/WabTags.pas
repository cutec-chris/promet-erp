unit WabTags;

interface

const ENCODING_PREFERENCE                     = ( $00020000);

const ENCODING_TEXT                           = ( $00000000);
const ENCODING_MIME                           = ( $00040000);

(*  Specifies how the body of the message is encoded.
    00 - Body encoded as text
        01 - body encoded as HTML (only valid if message in MIME)
        10 - (actualy 1X) Text and HTML as multipart alternative (only valid if message in MIME)
 *)
const BODY_ENCODING_MASK                      = ( $00180000);
const BODY_ENCODING_TEXT                      = ( $00000000); //* for completeness */
const BODY_ENCODING_HTML                      = ( $00080000);
const BODY_ENCODING_TEXT_AND_HTML             = ( $00100000);

(*  Specifies how to handle Mac attachments
    00 - BinHex
        01 - UUENCODED (not valid if message in MIME - will be ignored, BinHex used instead)
        10 - Apple Single (only valid if message in MIME)
        11 - Apple Double (only valid if message in MIME)
 *)
const MAC_ATTACH_ENCODING_MASK                = ( $00600000);
const MAC_ATTACH_ENCODING_BINHEX              = ( $00000000);
const MAC_ATTACH_ENCODING_UUENCODE            = ( $00200000);
const MAC_ATTACH_ENCODING_APPLESINGLE         = ( $00400000);
const MAC_ATTACH_ENCODING_APPLEDOUBLE         = ( $00600000);

implementation

end.
