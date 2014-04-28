unit updfexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PReport, PRJpegImage, PRAnnotation, FileUtil, Forms,
  Controls, Graphics, Dialogs,PdfDoc;

type

  { Tfpdfexport }

  Tfpdfexport = class(TForm)
    Pdf: TPReport;
    Image: TPRJpegImage;
    PRLayoutPanel1: TPRLayoutPanel;
    Page: TPRPage;
    procedure PagePrintPage(Sender: TObject; ACanvas: TPRCanvas);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fpdfexport: Tfpdfexport;

implementation

{$R *.lfm}

{ Tfpdfexport }

procedure Tfpdfexport.PagePrintPage(Sender: TObject; ACanvas: TPRCanvas);
var
  Dest: TPRDestination;
begin
  // create a new destination for the current page.
  Dest := Pdf.CreateDestination;

  // setting the properties for the destination object.
  with Dest do
  begin
    DestinationType := dtXYZ;
    Left := 0;
    Top := 0;
    Zoom := 1;
  end;

  // set the destination object as the open-action.
  Pdf.OpenAction := Dest;
end;

end.

