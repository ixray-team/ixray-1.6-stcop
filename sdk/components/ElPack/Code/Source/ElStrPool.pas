{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
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

unit ElStrPool;

interface

uses Classes,
     {$ifdef ELPACK_UNICODE}
     ElUnicodeStrings;
     {$else}
     ElStrArray;
     {$endif}

type

{$ifdef ELPACK_UNICODE}
     TElFStringArray = TElWideStringArray;
{$else}
     TElFStringArray = TElStringArray;
{$endif}

     TElStringPool = class(TComponent)
     private
       FItems : TElFStringArray;
       procedure SetItems(newValue : TElFStringArray);
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;
       procedure Assign(Source : TPersistent); override;
     published
       property Items : TElFStringArray read FItems write SetItems;
     end;

implementation

constructor TElStringPool.Create(AOwner : TComponent);
begin
  inherited;
  FItems := TElFStringArray.Create;
end;

destructor TElStringPool.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TElStringPool.Assign(Source : TPersistent);
begin
  if Source is TStrings then
    FItems.Assign(Source)
  else
    inherited;
end;

procedure TElStringPool.SetItems(newValue : TElFStringArray);
begin
  FItems.Assign(newValue);
end;

end.

