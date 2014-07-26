{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pdocuments;

interface

uses
  uDocProperties, uDocumentAcquire, uDocumentAction, uDocumentAddOptions, 
  uDocumentCheckin, uDocumentFrame, uDocumentOptions, uMimeTypeEdit, 
  uPreviewFrame, uSelectTemplate, ueditor, uSynEditFiler, uzugferd, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pdocuments', @Register);
end.
