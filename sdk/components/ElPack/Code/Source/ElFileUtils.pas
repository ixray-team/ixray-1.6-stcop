{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Parts:                                           }
{   copyright (c) 2001 Akzhan Abdulin                }
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

(*
  Version History:

08/28/2001

  Initiated (c) Akzhan Abdulin

*)

unit ElFileUtils;

interface

{$IFDEF WIN32}
uses
  Windows;

{$ifndef BROKEN_UNICODE}
function GetWideCurrentDir: WideString;
function SetWideCurrentDir(const Dir: WideString): Boolean;
function CreateWideDir(const Dir: WideString): Boolean;
function RemoveWideDir(const Dir: WideString): Boolean;

function GetWideModuleName(Module: HMODULE): WideString;
{$endif}
{$ENDIF}

implementation

uses
  ElStrUtils;

{$IFDEF WIN32}
{$ifndef BROKEN_UNICODE}
function GetWideCurrentDir: WideString;
var
  Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  SetWideString(Result, Buffer, GetCurrentDirectoryW(MAX_PATH, Buffer));
end;

function SetWideCurrentDir(const Dir: WideString): Boolean;
begin
  Result := SetCurrentDirectoryW(PWideChar(Dir));
end;

function CreateWideDir(const Dir: WideString): Boolean;
begin
  Result := CreateDirectoryW(PWideChar(Dir), nil);
end;

function RemoveWideDir(const Dir: WideString): Boolean;
begin
  Result := RemoveDirectoryW(PWideChar(Dir));
end;

function GetWideModuleName(Module: HMODULE): WideString;
var
  ModName: array[0..MAX_PATH] of WideChar;
begin
  SetWideString(Result, ModName, Windows.GetModuleFileNameW(Module, ModName, MAX_PATH));
end;
{$endif}
{$ENDIF}

end.
