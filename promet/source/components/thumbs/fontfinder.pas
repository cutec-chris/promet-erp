unit fontfinder;

//2008 Theo

interface

uses Classes, SysUtils;

const
  Font_Sans = 'sans-serif';
  Font_Serif = 'serif';
  Font_Monospace = 'monospace';
  Font_Sans_ExportList = 'Helvetica,Arial,sans-serif';
  Font_Serif_ExportList = 'Times New Roman,Times,serif';
  Font_Mono_ExportList = 'Courier New,Courier,monospace';

type

  { TFontFinder }

  TFontFinder = class
  private
    fScreenFontsLo: TStringList;
    fScreenFonts: TStringList;
    fFontSubst: TStringList;
    fSans: string;
    fSerif: string;
    fMono: string;
    fDefaultName: string;
  protected
    function FindAFontSubstr(Inp: string): string;
    function FindAFontIdent(Inp: string): string;
    function FindAFontSubstit(Inp: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function FindAFont(Inp: string): string;
    function FindAFontFromList(Inp: TStrings): string;
    function FindAFontFromDelimitedString(Inp: string; Delim: char = ','): string;
    property FontSubst: TStringList read fFontSubst write fFontSubst;
    property Sans: string read fSans write fSans;
    property Serif: string read fSerif write fSerif;
    property Mono: string read fMono write fMono;
    property Defaultname: string read fDefaultName write fDefaultName;
  published

  end;

procedure EnumFonts(List: TStrings);

implementation

uses LCLType, LCLIntf, Forms;

function EnumFontsNoDups(var LogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx; FontType: longint; Data: LParam): longint; stdcall;
var
  L: TStringList;
  S: string;
begin
  L := TStringList(ptrint(Data));
  S := LogFont.elfLogFont.lfFaceName;
  if L.IndexOf(S) < 0 then
    L.Add(S);
  Result := 1;
end;

procedure EnumFonts(List: TStrings);
var
  DC: HDC;
  lf: TagLogFontA;
  i, p: integer;
  L: TStringList;
  haveac: boolean;
begin
  haveac := False;
  lf.lfCharSet := FCS_ISO_10646_1;
  lf.lfFaceName := '';
  lf.lfPitchAndFamily := DEFAULT_PITCH;
  L := TStringList.Create;

  DC := GetDC(0);
  try
    EnumFontFamiliesEX(DC, @lf, @EnumFontsNoDups, ptrint(L), 0);
    if L.Count > 0 then
    begin
      for i := 0 to L.Count - 1 do
      begin
        if not haveac and (Pos('courier [adobe]', L[i]) > 0) then
        begin
          L[i] := 'adobe courier';
          haveac := True;
        end;
        P := Pos('[', L[i]);
        if P > 0 then
          L[i] := Trim(Copy(L[i], 1, p - 1));
      end;
      L.Sort;
      List.Assign(L);
    end
    else
      List.Assign(Screen.Fonts);
  finally
    ReleaseDC(0, DC);
    L.Free;
  end;
end;


{ TFontFinder }

constructor TFontFinder.Create;
begin
  fScreenFontsLo := TStringList.Create;
  fScreenFonts := TStringList.Create;
  EnumFonts(fScreenFonts);
  fScreenFontsLo.Text := Lowercase(fScreenFonts.Text);
  fFontSubst := TStringList.Create;

  {$IFDEF mswindows}
  fSans := 'Arial';
  fSerif := 'Times New Roman';
  fMono := 'Courier New';
  fFontSubst.add('helvetica=' + fSans);
  fFontSubst.add('times=' + fSerif);
  fFontSubst.add('courier=' + fMono);
  {$ELSE}
  fSans := 'helvetica';
  fSerif := 'times';
  fMono := 'courier';
  fFontSubst.add('arial=' + fSans);
  fFontSubst.add('times new roman=' + fSerif);
  fFontSubst.add('courier new=' + fMono);
  {$ENDIF}

  fFontSubst.add(Font_Sans + '=' + fSans);
  fFontSubst.add(Font_Serif + '=' + fSerif);
  fFontSubst.add(Font_Monospace + '=' + fMono);

  fDefaultName := fSerif;
end;

destructor TFontFinder.Destroy;
begin
  fScreenFonts.Free;
  fScreenFontsLo.Free;
  fFontSubst.Free;
  inherited;
end;

function TFontFinder.FindAFont(Inp: string): string;
begin
  Result := FindAFontIdent(Inp);
  if Result <> '' then
    exit;
  Result := FindAFontSubstit(Inp);
  if Result <> '' then
    exit;
  Result := FindAFontSubstr(Inp);
  if Result <> '' then
    exit;
  Result := fDefaultName;

end;

procedure StrictSetDelimitedText(AText: string; ADelim: char; AList: TStringList);
var
  i: integer;
  Buf: string;
begin
  AList.Clear;
  Buf:='';
  for i := 1 to Length(AText) do
    if AText[i] = ADelim then
    begin
      AList.Add(Trim(Buf));
      Buf := '';
    end
    else
      Buf := Buf + AText[i];
  if Trim(Buf) <> '' then
    AList.Add(Trim(Buf));
end;


function TFontFinder.FindAFontFromDelimitedString(Inp: string; Delim: char): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  StrictSetDelimitedText(Inp, Delim, SL);
  Result := FindAFontFromList(SL);
  SL.Free;
end;

function TFontFinder.FindAFontFromList(Inp: TStrings): string;
var
  i: integer;
begin
  for i := 0 to Inp.Count - 1 do
  begin
    Result := FindAFontIdent((Inp[i]));
    if Result <> '' then
      Exit;
  end;

  for i := 0 to Inp.Count - 1 do
  begin
    Result := FindAFontSubstit((Inp[i]));
    if Result <> '' then
      Exit;
  end;

  for i := 0 to Inp.Count - 1 do
  begin
    Result := FindAFontSubstr((Inp[i]));
    if Result <> '' then
      Exit;
  end;
  Result := fDefaultName;
end;

function TFontFinder.FindAFontIdent(Inp: string): string;
var
  i: integer;
begin
  Result := '';
  inp := Lowercase(Trim(inp));
  i := fScreenFontsLo.IndexOf(Inp);
  if i > -1 then
    Result := fScreenFonts[i];
end;

function TFontFinder.FindAFontSubstit(Inp: string): string;
begin
  Result := '';
  inp := Lowercase(Trim(inp));
  Result := fFontSubst.Values[inp];
end;

function TFontFinder.FindAFontSubstr(Inp: string): string;
var
  i: integer;
begin
  Result := '';
  inp := Lowercase(Trim(inp));
  if pos(' ', inp) > 0 then
    inp := Copy(inp, 1, pos(' ', inp) - 1);
  for i := 0 to fScreenFontsLo.Count - 1 do
    if Pos(inp, fScreenFontsLo[i]) > 0 then
    begin
      Result := fScreenFonts[i];
      exit;
    end;
end;

end.

