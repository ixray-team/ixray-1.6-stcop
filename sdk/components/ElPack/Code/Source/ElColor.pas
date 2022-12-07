{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Portion copyright (c) 2001, Alexander Hramov     }
{                                                    }
{====================================================}

unit ElColor; { EldoS Color Managment }

interface

uses 
  Windows,
  Graphics,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElTools;

function ColorToRGB(const Color: TColor): longint;
function BrightColor(const Color: TColor; const Percent: byte):TColor;
function DarkColor(const Color: TColor; const Percent: byte): TColor;
function ColorToGray(const Color: TColor): TColor;
function ConvertColorToHTML(Color : TColor) : integer;

implementation

function ColorToRGB(const Color: TColor): longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;

function BrightColor(const Color: TColor; const Percent: byte):TColor;
var
  R, G, B: byte;
  FColor: TColorRef;
  InOnePercent: single;
begin
  FColor := ColorToRGB(Color);

  R := GetRValue(FColor);
  G := GetGValue(FColor);
  B := GetBValue(FColor);

  InOnePercent := Percent / 100;

  Inc(R, Round((255 - R) * InOnePercent));
  Inc(G, Round((255 - G) * InOnePercent));
  Inc(B, Round((255 - B) * InOnePercent));

  Result := RGB(R, G, B);
end;

function DarkColor(const Color: TColor; const Percent: byte): TColor;
var
  R, G, B: integer;
  FColor: TColorRef;
  InOnePercent: single;
begin
  FColor := ColorToRGB(Color);
  R := GetRValue(FColor);
  G := GetGValue(FColor);
  B := GetBValue(FColor);

  InOnePercent := Percent / 100;

  Dec(R, Round((255 - R) * InOnePercent));
  Dec(G, Round((255 - G) * InOnePercent));
  Dec(B, Round((255 - B) * InOnePercent));

  Result := RGB(R, G, B);
end;

function ColorToGray(const Color: TColor): TColor;
var
  R, G, B, M: integer;
  FColor: TColorRef;
begin
  FColor := ColorToRGB(Color);
  R := GetRValue(FColor);
  G := GetGValue(FColor);
  B := GetBValue(FColor);

  M := Round(R * 0.30 + G * 0.59 + B * 0.11);

  Result := RGB(M, M, M);
end;


function ConvertColorToHTML(Color : TColor) : integer;
begin
  result := SwapInt32(Color shl 8);
end;

end.
