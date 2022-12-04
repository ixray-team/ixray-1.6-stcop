{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 2001-2002, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

01/30/2002 (c) Akzhan Abdulin

  Image index property editor now compatible with Borland Delphi 6.

  Images with big dimensions now drawn correctly.

07/19/2001 (c) Akzhan Abdulin

  Image index property editor now can automatically detect
  target component's 'Images' property.

07/18/2001 (c) Akzhan Abdulin

  Selected image now drawn in object inspector view  

07/15/2001 (c) Akzhan Abdulin

  Initiated (tested under Delphi 5 only)

*)

unit ElImageIndexProp;

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

interface

{$IFDEF D_5_UP}

uses
  Classes, Forms, Controls, SysUtils, Graphics, Windows,
  ImgList,
{$ifdef VCL_6_USED}
  DesignEditors, DesignWindows, DsnConst, DesignIntf, VCLEditors
{$else}
  DsgnIntf
{$endif}
  ;

type
  TElImageIndexProperty = class(TIntegerProperty{$IFDEF D_6_UP}, ICustomPropertyListDrawing, ICustomPropertyDrawing{$ENDIF})
  private
  protected
    function GetImages: TCustomImageList; virtual;
    function GetImagesPropertyName: String; virtual;
    property Images: TCustomImageList read GetImages;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    { ICustomPropertyDrawing }
{$ifdef D_6_UP}
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
{$endif}
    procedure PropDrawValue(ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF D_6_UP}override;{$ENDIF}
    { ICustomPropertyListDrawing }
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); {$IFNDEF D_6_UP}override;{$ENDIF}
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); {$IFNDEF D_6_UP}override;{$ENDIF}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF D_6_UP}override;{$ENDIF}
  end;

{$ENDIF}

implementation

{$IFDEF D_5_UP}

uses
  TypInfo;

{ TElImageIndexProperty }

function TElImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

function TElImageIndexProperty.GetImages: TCustomImageList;
var
  C: TPersistent;
begin
  C := GetComponent(0);
  Result := TCustomImageList(GetObjectProp(C, GetImagesPropertyName(), TCustomImageList));
end;

function TElImageIndexProperty.GetImagesPropertyName: String;
begin
  Result := 'Images';
end;

procedure TElImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc('-1');
  if Images = nil then Exit;
  for I := 0 to Images.Count - 1 do
    Proc(IntToStr(I));
end;

procedure TElImageIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ATextRect: TRect;
  Bmp: Graphics.TBitmap;
begin
  ATextRect := ARect;
  if Images <> nil then
  begin
    Inc(ATextRect.Left, 1 + Images.Width + 1);
    with ARect do
    begin
      ACanvas.FillRect(Rect(Left, Top, ATextRect.Left, Bottom));
    end;
    Bmp := Graphics.TBitmap.Create;
    try
      Images.GetBitmap(StrToInt(Value), Bmp);
      if Images.Height > ARect.Bottom - ARect.Top - 2 then Bmp.Height := ARect.Bottom - ARect.Top - 2;
      ACanvas.Draw(ARect.Left + 1, ARect.Top + 1, Bmp);
    finally
      Bmp.Free;
    end;
  end;
  with ATextRect, ACanvas do
    ACanvas.TextRect(ATextRect, Left + 1, (Top + Bottom - TextHeight(Value)) div 2, Value);
end;

procedure TElImageIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
{$ifdef D_6_UP}
  AHeight := ACanvas.TextHeight('Wg');
{$else}
  inherited;
{$endif}
  if (Images <> nil) and (AHeight < 1 + Images.Height + 1) then
    AHeight := 1 + Images.Height + 1;
end;

procedure TElImageIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
{$ifdef D_6_UP}
var
  I: Char;
  WD: Integer;
{$endif}
begin
{$ifdef D_6_UP}
  AWidth := 0;
  for I := '0' to '9' do
  begin
    WD := ACanvas.TextWidth(I);
    if WD > AWidth then
    begin
      AWidth := WD;
    end;
  end;
  AWidth := ACanvas.TextWidth('-') + (7 * AWidth);
{$else}
  inherited;
{$endif}
  if Images <> nil then Inc(AWidth, 1 + Images.Width + 1);
end;

{$ifdef D_6_UP}
procedure TElImageIndexProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;
{$endif}

procedure TElImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
{$ifdef D_6_UP}
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
{$else}
    inherited PropDrawValue(ACanvas, ARect, ASelected);
{$endif}
end;

{$ENDIF}

end.
