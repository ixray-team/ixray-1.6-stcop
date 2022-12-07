
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{   RLE compression (c) Copyright 1999,              }
{   Dipl. Ing. Mike Lischke                          }
{   public@lischke-online.de                         }
{   Used with permission from Mike Lischke           }
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

{.$DEFINE USE_JPEG}

unit ElImgLst;

{ ImageList with WriteData and ReadData fix }

interface

uses
  Classes,
  SysUtils,
  {$ifdef CLX_USED}
  QComCtrls,
  QImgList,
  QGraphics,
  QControls,
  {$else}
  Controls,
  Forms,
  Windows,
{$ifdef VCL_6_USED}
Types,
{$endif}
  Consts, 
  CommCtrl,
{$IFDEF VCL_4_USED}
  ImgList,
{$ENDIF}
  Graphics,
  {$endif}
  ElTools
{$IFDEF USE_JPEG}
  , JPEG
{$ENDIF}
  ;

type
  TElImageList = class(TImageList)
  private
    (*
    procedure CreateHandle;
    *)
  public
    { Private declarations }
    procedure ReadImg(Stream : TStream);
    procedure WriteImg(Stream : TStream);
    function Equal(IL : TElImageList) : Boolean;
    constructor Create(AOwner : TComponent); override;
  protected
    procedure GetFullImages(Image, Mask: TBitmap);
    { Protected declarations }
    procedure ReadLeft(Reader : TReader);
    procedure ReadTop(Reader : TReader);
    procedure WriteLeft(Writer : TWriter);
    procedure WriteTop(Writer : TWriter);
    procedure DefineProperties(Filer : TFiler); override;
{$IFDEF USE_JPEG}
  private
    FUseCompression : Boolean;
    FCompressionQuality : TJPEGQualityRange;
  published
    property UseCompression : Boolean read FUseCompression write FUseCompression; { Published }
    property CompressionQuality : TJPEGQualityRange read FCompressionQuality write FCompressionQuality; { Published }
{$ENDIF}
  end; { TElImageList }

function EncodeRLE(const Source, Target: PByte; Count, BPP: Integer): Integer;
function DecodeRLE(const Source, Target: Pointer; Count, ColorDepth: Cardinal): Integer;

implementation

const HandleOffset = 3;

function DecodeRLE(const Source, Target: Pointer; Count, ColorDepth: Cardinal): Integer;

// Decodes RLE compressed data from Source into Target. Count determines size of target buffer and ColorDepth
// the size of one data entry.
// Result is the amount of bytes decoded.

var 
  I: Integer;
  SourcePtr,
  TargetPtr: PByte;
  RunLength: Cardinal;
  Counter: Cardinal;

begin
  Result := 0;
  Counter := 0;
  TargetPtr := Target;
  SourcePtr := Source;
  // unrolled decoder loop to speed up process
  case ColorDepth of
    8:
      while Counter < Count do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(TargetPtr);
          end;
          Inc(SourcePtr);
          Inc(Result, 2);
        end
        else
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(Result, RunLength + 1)
        end;
        Inc(Counter, RunLength);
      end;
    15,
    16:
      while Counter < Count do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 2);
          Inc(Result, 3);
        end
        else
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(Result, RunLength * 2 + 1);
        end;
        Inc(Counter, 2 * RunLength);
     end;
    24:
      while Counter < Count do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr, 2);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 3);
          Inc(Result, 4);
        end
        else
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(Result, RunLength * 3 + 1);
        end;
        Inc(Counter, 3 * RunLength);
      end;
    32:
      while Counter < Count do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr, 3);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 4);
          Inc(Result, 5);
        end
        else
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(Result,RunLength * 4 + 1);
        end;
        Inc(Counter, 4 * RunLength);
      end;
    else
    begin
    end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetPixel(P: PByte; BPP: Byte): Cardinal;

// Retrieves a pixel value from a buffer. The actual size and order of the bytes is not important
// since we are only using the value for comparisons with other pixels.

begin
  try
    Result := P^;
  except
    on E: Exception do
    begin
      result := 0;
    end;
  end;
  Inc(P);
  Dec(BPP);
  while BPP > 0 do
  begin
    Result := Result shl 8;
    Result := Result or P^;
    Inc(P);
    Dec(BPP);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CountDiffPixels(P: PByte; BPP: Byte; Count: Integer): Integer;

// counts pixels in buffer until two identical adjacent ones found

var
  N: Integer;
  Pixel,
  NextPixel: Cardinal;

begin
  N := 0;
  NextPixel := 0; // shut up compiler
  if Count = 1 then Result := Count
               else
  begin
    Pixel := GetPixel(P, BPP);
    while Count > 1 do
    begin
      Inc(P, BPP);
      NextPixel := GetPixel(P, BPP);
      if NextPixel = Pixel then Break;
      Pixel := NextPixel;
      Inc(N);
      Dec(Count);
    end;
    if NextPixel = Pixel then Result := N
                         else Result := N + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CountSamePixels(P: PByte; BPP: Byte; Count: Integer): Integer;

var
  Pixel,
  NextPixel: Cardinal;

begin
  Result := 1;
  Pixel := GetPixel(P, BPP);
  Dec(Count);
  while Count > 0 do
  begin
    Inc(P, BPP);
    NextPixel := GetPixel(P, BPP);
    if NextPixel <> Pixel then Break;
    Inc(Result);
    Dec(Count);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function EncodeRLE(const Source, Target: PByte; Count, BPP: Integer): Integer;

// Encodes "Count" bytes pointed to by Source into the buffer supplied with Target and returns the
// number of bytes stored in Target. BPP denotes bytes per pixel color depth.
// Note: The target buffer must provide enough space to hold the compressed data. Using a size of
//       twice the size of the input buffer is sufficent.

var
  DiffCount, // pixel count until two identical
  SameCount: Integer; // number of identical adjacent pixels
  SourcePtr,
  TargetPtr: PByte;

begin
  Result := 0;
  SourcePtr := Source;
  TargetPtr := Target;
  while Count > 0 do
  begin
    DiffCount := CountDiffPixels(SourcePtr, BPP, Count);
    SameCount := CountSamePixels(SourcePtr, BPP, Count);
    if DiffCount > 128 then DiffCount := 128;
    if SameCount > 128  then SameCount := 128;

    if DiffCount > 0 then
    begin
      // create a raw packet
      TargetPtr^ := DiffCount - 1; Inc(TargetPtr);
      Dec(Count, DiffCount * BPP);
      Inc(Result, (DiffCount * BPP) + 1);
      while DiffCount > 0 do
      begin
        TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr);
        if BPP > 1 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
        if BPP > 2 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
        if BPP > 3 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
        Dec(DiffCount);
      end;
    end;

    if SameCount > 1 then
    begin
      // create a RLE packet
      TargetPtr^ := (SameCount - 1) or $80; Inc(TargetPtr);
      Dec(Count, SameCount * BPP);
      Inc(Result, BPP + 1);
      Inc(SourcePtr, (SameCount - 1) * BPP);
      TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr);
      if BPP > 1 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
      if BPP > 2 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
      if BPP > 3 then begin TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr); end;
    end;
  end;
end;

procedure TElImageList.ReadLeft(Reader : TReader);
var
  i : LongInt;
begin
  i := DesignInfo;
  LongRec(i).Lo := Reader.ReadInteger;
  DesignInfo := i;
end;

procedure TElImageList.ReadTop(Reader : TReader);
var
  i : LongInt;
begin
  i := DesignInfo;
  LongRec(i).Hi := Reader.ReadInteger;
  DesignInfo := i;
end;

procedure TElImageList.WriteLeft(Writer : TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TElImageList.WriteTop(Writer : TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

procedure TElImageList.DefineProperties(Filer : TFiler); { protected }
var
  Ancestor : TComponent;
  Info : Longint;

  function DoWrite : Boolean;
  begin
    {if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TElImageList) or
        not Equal(TElImageList(Filer.Ancestor))
    else}
      Result := Count > 0;
  end;

begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then Info := Ancestor.DesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft,
    LongRec(DesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop,
    LongRec(DesignInfo).Hi <> LongRec(Info).Hi);
  Filer.DefineBinaryProperty('Bitmap', ReadImg, WriteImg, DoWrite);
end; { DefineProperties }

procedure TElImageList.ReadImg(Stream : TStream); { private }
var
  Tmp, Msk : TBitmap;
  I, LCount : integer;
  Version : integer;
{$IFDEF USE_JPEG}
  IsJPEG : boolean;
  Image : TJPEGImage;
{$ENDIF}

  procedure LoadFromRLE;
  var ds,
      BPP : Integer;
      P   : PChar;
      MemStream : TMemoryStream;
      TmpBmp1,
      MskBmp1  : TBitmap;
      i, j,
      di, dj,
      x, y, t   : integer;

  begin
    Stream.ReadBuffer(LCount, SizeOf(LCount));
    MemStream := TMemoryStream.Create;
    if Version = -3 then
    begin
      try
        for I := 0 to LCount - 1 do
        begin
          MemStream.Clear;
          // read encoded size
          Stream.ReadBuffer(j, sizeof(j));
          GetMem(P, j);
          try
            // read encoded sequence
            Stream.ReadBuffer(P^, j);
            // read decompressed size
            Stream.ReadBuffer(ds, sizeof(ds));
            MemStream.SetSize(ds);
            // read BPP value
            Stream.ReadBuffer(BPP, sizeof(BPP));
            j := DecodeRLE(P, MemStream.Memory, MemStream.Size, BPP);
          finally
            FreeMem(P);
          end;
          Tmp.LoadFromStream(MemStream);

          MemStream.Clear;
          // read encoded size
          Stream.ReadBuffer(j, sizeof(j));
          GetMem(P, j);
          // read encoded sequence
          Stream.ReadBuffer(P^, j);
          // read decompressed size
          Stream.ReadBuffer(ds, sizeof(ds));

          MemStream.SetSize(ds);
          // read BPP value
          Stream.ReadBuffer(BPP, sizeof(BPP));
          j := DecodeRLE(P, MemStream.Memory, MemStream.Size, BPP);
          FreeMem(P);
  
          Msk.LoadFromStream(MemStream);
          Add(Tmp, Msk);
        end;
      finally
        MemStream.Free;
      end;
    end else
    begin
      {Tmp.TransparentMode := tmFixed;
      Tmp.TransparentColor := clNone;
      Tmp.Transparent := false;
      }try
        MemStream.Clear;
        // read encoded size
        Stream.ReadBuffer(j, sizeof(j));
        GetMem(P, j);
        try
          // read encoded sequence
          Stream.ReadBuffer(P^, j);
          // read decompressed size
          Stream.ReadBuffer(ds, sizeof(ds));
          MemStream.SetSize(ds);
          // read BPP value
          Stream.ReadBuffer(BPP, sizeof(BPP));
          j := DecodeRLE(P, MemStream.Memory, MemStream.Size, BPP);
        finally
          FreeMem(P);
        end;
        Tmp.LoadFromStream(MemStream);

        MemStream.Clear;
        // read encoded size
        Stream.ReadBuffer(j, sizeof(j));
        GetMem(P, j);
        // read encoded sequence
        Stream.ReadBuffer(P^, j);
        // read decompressed size
        Stream.ReadBuffer(ds, sizeof(ds));

        MemStream.SetSize(ds);
        // read BPP value
        Stream.ReadBuffer(BPP, sizeof(BPP));
        j := DecodeRLE(P, MemStream.Memory, MemStream.Size, BPP);
        FreeMem(P);
        Msk.LoadFromStream(MemStream);

        if Tmp.Height = Height then
        begin
          ImageList_Add(Handle, Tmp.Handle, Msk.Handle);
          while Count > LCount do 
            Delete(Count -1);
        end
        else
        begin
          TmpBmp1 := TBitmap.Create;
          MskBmp1 := TBitmap.Create;

          TmpBmp1.Width := Width;
          TmpBmp1.Height := Height;
          MskBmp1.Width := Width;
          MskBmp1.Height := Height;
        
          try
            di := Tmp.Height div Height;
            dj := Tmp.Width div Width;
            t  := 0;
            for i := 0 to di  - 1 do    { Iterate }
            begin
              for j := 0 to dj  - 1 do    { Iterate }
              begin
                x := j * Width;
                y := i * Height;
                TmpBmp1.Canvas.CopyRect(Rect(0, 0, Width, Height), Tmp.Canvas, Rect(x, y, x + Width, y + Height));
                MskBmp1.Canvas.CopyRect(Rect(0, 0, Width, Height), Msk.Canvas, Rect(x, y, x + Width, y + Height));
                Add(TmpBmp1, MskBmp1);
                inc(t);
                if t >= LCount then break;
              end;    { for }
              if t >= LCount then break;
            end;    { for }
          finally
            TmpBmp1.Free;
            MskBmp1.Free;
          end;
        end;
      finally
        MemStream.Free;
      end;
    end;
  end;

begin
  Clear;
  Stream.ReadBuffer(LCount, SizeOf(integer));
  if LCount < 0 then
    Version := LCount
  else
    Version := 0;
  Tmp := TBitmap.Create;
  Msk := TBitmap.Create;

  if Version <=-3 then
  begin
    LoadFromRLE;
    Tmp.Free;
    Msk.Free;
    exit;
  end;

{$IFDEF USE_JPEG}
  if (Version <= -2) then
  begin
    Stream.ReadBuffer(IsJPEG, SizeOf(IsJPEG));
    Image := TJPEGImage.Create;
    Image.Performance := jpBestQuality;
  end
  else
    IsJPEG := false;
{$ENDIF}
  if LCount <= -1 then Stream.ReadBuffer(LCount, SizeOf(LCount));
  try
    for I := 0 to LCount - 1 do
    begin
{$IFDEF USE_JPEG}
      if not IsJPEG then
{$ENDIF}
      begin
        Tmp.LoadFromStream(Stream);
        if Version <= -1 then
        begin
          Msk.LoadFromStream(Stream);
          Add(Tmp, Msk);
        end
        else
        begin
{$IFNDEF VER90}
          Msk.Handle := Tmp.MaskHandle;
{$ENDIF}
          Add(Tmp, Msk);
        end;
      end
{$IFDEF USE_JPEG}
      else
      begin
        Image.LoadFromStream(Stream);
        Tmp.Assign(Image);
        Image.LoadFromStream(Stream);
        Msk.Assign(Image);
        Add(Tmp, Msk);
      end;
{$ENDIF}
    end;
  finally
    Tmp.Free;
    Msk.Free;
{$IFDEF USE_JPEG}
    if (Version <= -2) then Image.Free;
{$ENDIF}
  end;
  if (Count <> LCount) then
    raise EReadError.Create('Failed to load ElImageList');
end; { ReadData }
{$HINTS OFF}
procedure TElImageList.WriteImg(Stream : TStream); { private }
var
  Tmp, Msk  : TBitmap;
  I, SCount : integer;
  MemStream : TMemoryStream;
  P         : PChar;
  L, X, Y   : integer;
  Info      : TImageInfo;
const
  BPPSize : array[TPixelFormat] of integer = (0, 1, 1, 1, 2, 2, 3, 4, 1);
  BPPs    : array[TPixelFormat] of integer = (0, 1, 4, 8, 15, 16, 24, 32, 1);

begin
  SCount := -4;
  try
    Stream.WriteBuffer(SCount, SizeOf(integer));
    Tmp := TBitmap.Create;
    Msk := TBitmap.Create;

    SCount := Count;
    Stream.WriteBuffer(SCount, SizeOf(integer));
    MemStream := TMemoryStream.Create;
    try
      GetFullImages(Tmp, Msk);

      MemStream.Clear;
      Tmp.SaveToStream(MemStream);
      GetMem(P, MemStream.Size * 4);
      if Tmp.PixelFormat = pfDevice then
      begin
        BPPS[Tmp.PixelFormat] := GetDeviceCaps(Tmp.Canvas.Handle, BITSPIXEL);
        BPPSize[Tmp.PixelFormat] := BPPs[Tmp.PixelFormat] div 8;
        {case BPPs[Tmp.PixelFormat] of
          1: Tmp.PixelFormat := pf1bit;
          4: Tmp.PixelFormat := pf4bit;
          8: Tmp.PixelFormat := pf8bit;
          15: Tmp.PixelFormat := pf15bit;
          16: Tmp.PixelFormat := pf16bit;
          24: Tmp.PixelFormat := pf24bit;
          32: Tmp.PixelFormat := pf32bit;
        end;}
      end;
      L := EncodeRLE(MemStream.Memory, PByte(P), MemStream.Size, BPPSize[Tmp.PixelFormat]);
      // write compressed sequence
      Stream.WriteBuffer(L, sizeof(L));
      // write compressed sequence
      Stream.WriteBuffer(P^, L);
      // write complete image size
      L := MemStream.Size;
      Stream.WriteBuffer(L, sizeof(L));
      // write BPP value
      L := BPPs[Tmp.PixelFormat];
      Stream.WriteBuffer(L, sizeof(L));
      FreeMem(P);

      MemStream.Clear;
      if Msk.PixelFormat = pfDevice then
      begin
        BPPS[Msk.PixelFormat] := GetDeviceCaps(Msk.Canvas.Handle, BITSPIXEL);
        BPPSize[Msk.PixelFormat] := BPPs[Msk.PixelFormat] div 8;
        {case BPPs[Msk.PixelFormat] of
          1: Msk.PixelFormat := pf1bit;
          4: Msk.PixelFormat := pf4bit;
          8: Msk.PixelFormat := pf8bit;
          15: Msk.PixelFormat := pf15bit;
          16: Msk.PixelFormat := pf16bit;
          24: Msk.PixelFormat := pf24bit;
          32: Msk.PixelFormat := pf32bit;
        end;}
      end;

      Msk.SaveToStream(MemStream);
      GetMem(P, MemStream.Size * 4);

      L := EncodeRLE(MemStream.Memory, PByte(P),
                     MemStream.Size, BPPSize[Msk.PixelFormat]);
      Stream.WriteBuffer(L, sizeof(L));
      Stream.WriteBuffer(P^, L);
      L := MemStream.Size;
      Stream.WriteBuffer(L, sizeof(L));
      L := BPPs[Msk.PixelFormat];
      Stream.WriteBuffer(L, sizeof(L));
      FreeMem(P);
    finally
      MemStream.Free;
      Tmp.Free;
      Msk.Free;
    end;
  except
    on E: Exception do MessageBox(0, PChar('Failed to write ElImageList: ' + E.Message), nil, 0);
  end;
end; { WriteData }
{$HINTS ON}
function TElImageList.Equal(IL : TElImageList) : Boolean;
var
  ThisImage, OtherImage : TMemoryStream;

  function EqualStreams(S1, S2 : TMemoryStream) : Boolean;
  begin
    Result := (S1.Size = S2.Size) and CompareMem(S1.Memory, S2.Memory, S1.Size);
  end;

begin
  result := false;
  exit;
  if (IL = nil) or (Count <> IL.Count) then
  begin
    Result := False;
    Exit;
  end;
  if (Count = 0) and (IL.Count = 0) then
  begin
    Result := True;
    Exit;
  end;
  ThisImage := TMemoryStream.Create;
  try
    WriteImg(ThisImage);
    OtherImage := TMemoryStream.Create;
    try
      IL.WriteImg(OtherImage);
      Result := EqualStreams(ThisImage, OtherImage);
    finally
      OtherImage.Free;
    end;
  finally
    ThisImage.Free;
  end;
end;

procedure TElImageList.GetFullImages(Image, Mask: TBitmap);
var
  I: integer;
  Tmp, Msk: TBitmap;
begin
  Image.Height := Height; Image.Width := Width * Count;
  Mask.Height := Height; Mask.Width := Width * Count;
  Tmp := TBitmap.Create;
  Tmp.Width := Width; Tmp.Height := Height;
  Msk := TBitmap.Create;
  Msk.Width := Width; Msk.Height := Height;
  try
    for I := 0 to Count - 1 do
    begin
      GetImages(I, Tmp, Msk);
      BitBlt(Image.Canvas.Handle, I * Width, 0, Width,
        Height, Tmp.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(Mask.Canvas.Handle, I * Width, 0, Width,
        Height, Msk.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    Msk.Free;
    Tmp.Free;
  end;
  Mask.Monochrome := True;
end;

(*
procedure TElImageList.CreateHandle;
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);
var
  FHandle : PInteger;
begin
  FHandle := PInteger(Self);
  Inc(FHandle, HandleOffset);
  if IsWinXPUp then
    FHandle^ := Integer(ImageList_Create(Width, Height, ILC_COLOR32 or Mask[Masked],
      AllocBy, AllocBy))
  else
    FHandle^ := Integer(ImageList_Create(Width, Height, ILC_COLORDDB or Mask[Masked],
      AllocBy, AllocBy));
  if FHandle^ = 0 then raise EInvalidOperation.Create(SInvalidImageList);
  if BkColor <> clNone then
    BkColor := BkColor;
end;
*)

constructor TElImageList.Create(AOwner : TComponent);
(*
var P      : Pointer;
    P1     : PInteger;
    OldVal : DWORD;
    M      : TMethod;
    D      : procedure of object;
    i : Pointer;
*)
begin
  inherited;
  (*
  P := Pointer(@TElImageList.HandleNeeded);
  D := CreateHandle;
  M := TMethod(D);
  if VirtualProtect(P, 16, PAGE_READWRITE, @OldVal) then
  begin
    asm
      mov  eax, P
      mov  byte ptr [eax], $E9
      push ebx
      mov ebx, M.Code
      mov  dword ptr [eax + 1], ebx
      pop ebx
    end;
    VirtualProtect(P, 16, OldVal, @OldVal);
  end;
  *)
end;

end.

