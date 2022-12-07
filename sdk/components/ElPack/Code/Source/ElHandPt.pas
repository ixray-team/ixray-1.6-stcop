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

unit ElHandPt;

interface

{$ifndef CLX_USED}
uses Windows, Controls, Forms;
{$else}
uses QControls;
{$endif}

{$ifdef BORLAND_URL_CURSOR}
const crURLCursor = crHandPoint;
{$else}
{$ifndef CLX_USED}
{$r ElHandPt.res}
{$endif}
const crURLCursor = 1458;
{$endif}

implementation

{$ifndef BORLAND_URL_CURSOR}
initialization
{$ifndef CLX_USED}
  Screen.Cursors[crURLCursor] := LoadCursor(HInstance, 'URLCURSOR');
{$endif}
{$endif}

end.
