
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998 Alex Shovkoplyas              }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

{$ifdef CLX_USED}
error 
{$endif}

unit ElCBFmts;

interface

uses
  Windows, 
{$ifdef VCL_6_USED}
Types,
{$endif}
SysUtils;

function GetFormatName(AFormat : integer) : string;
function GetFormatIndex(FormatName : string) : integer;
function HasFormat(Handle : THandle; Index : integer) : boolean;

implementation

function HasFormat(Handle : THandle; Index : integer) : boolean;
var
  Fmt : integer;
begin
  result := false;
  if not OpenClipboard(Handle) then Exit;
  try
    Fmt := EnumClipboardFormats(0);
    while Fmt <> 0 do
    begin
      if Fmt = Index then
      begin
        result := true;
        break;
      end;
      Fmt := EnumClipboardFormats(Fmt);
    end;
  finally
    CloseClipboard;
  end;
end;

function GetFormatName(AFormat : integer) : string;
const
  FormatNames : array[1..16] of string =
    ('TEXT', 'BITMAP', 'METAFILEPICT', 'SYLK', 'DIF', 'TIFF',
    'OEMTEXT', 'DIB', 'PALETTE', 'PENDATA', 'RIFF', 'WAVE',
    'UNICODETEXT', 'ENHMETAFILE', 'HDROP', 'LOCALE');
begin
  if (AFormat >= CF_TEXT) and (AFormat <= CF_LOCALE) then
    Result := FormatNames[AFormat]
  else
  begin
    SetLength(Result, 128);
    SetLength(Result, GetClipboardFormatName(AFormat, PChar(Result), 128));
  end;
end;

function GetFormatIndex(FormatName : string) : integer;
const
  FormatNames = ':TEXT:BITMAP:METAFILEPICT:SYLK:DIF:TIFF:OEMTEXT:DIB:PALETTE:' +
    'PENDATA:RIFF:WAVE:UNICODETEXT:ENHMETAFILE:HDROP:LOCALE:';
var
  S : string;
begin
  s := UpperCase(FormatName);
  case Pos(':' + S + ':', FormatNames) of
    1 : Result := CF_TEXT;
    6 : Result := CF_BITMAP;
    13 : Result := CF_METAFILEPICT;
    26 : Result := CF_SYLK;
    31 : Result := CF_DIF;
    35 : Result := CF_TIFF;
    40 : Result := CF_OEMTEXT;
    48 : Result := CF_DIB;
    52 : Result := CF_PALETTE;
    60 : Result := CF_PENDATA;
    68 : Result := CF_RIFF;
    73 : Result := CF_WAVE;
    78 : Result := CF_UNICODETEXT;
    90 : Result := CF_ENHMETAFILE;
    102 : Result := CF_HDROP;
    108 : Result := CF_LOCALE;
  else
    Result := RegisterClipboardFormat(PChar(S));
  end;
end;

end.
