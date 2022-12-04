(*

These conditional defines turn on or off some rarely used features of
different elpack controls. Turning those features off reduces size of
generatede executable.

To turn the feature off place "." before "$" in the directive

To turn on debugging in ElPack define a compiler variable called DEBUG and
rebuild ElPack.

C++BUILDER 4 note:

Despite the BROKEN_UNICODE define ElPack supports unicode with C++Builder 4.
Please contact EldoS for details about how to enable unicode support.

*)

// comment this to make ElTree and some other controls ANSI
// uncommen tthis to keep ElTree and other controls unicode
{$DEFINE ELPACK_UNICODE}

// comment this if you need to keep your existing code
// or you don't want to migrate to ElPack 3.0 completely
// NOTE: when you comment this, you will need to modify package source files
// and remove all references to inplace editor units
{$DEFINE VER3_EDITORS}

// comment this if you don't use inplace editors at all
// saves about 300K of space (but only if you don't use VER3_EDITORS)
{$DEFINE ELTREE_USE_INPLACE_EDITORS}

// comment this if you don't use styles
{$DEFINE ELTREE_USE_STYLES}

// comment this to remove drag'n'drop
{$DEFINE ELTREE_USE_OLE_DRAGDROP}

// comment this to prevent use of extended inplace editors
// (currency editor, boolean editor, date time picker etc.)
{$DEFINE ELTREE_USE_EXT_EDITORS}

// comment this to remove Storage and linkage with ElIniFile
{$DEFINE SUPPORT_STORAGE}

// comment this if you don't use SoundMap
{$DEFINE USE_SOUND_MAP}

// comment this if you don't use HTML formating
{$DEFINE HAS_HTML_RENDER}

// comment this to disable popup calendar in ElDateTimePicker
{$DEFINE DATETIMEPICKER_SUPPORT_CALENDAR}

// uncomment this to replace ElPack calendar in popup calendar (in ElCalendarDlg)
// and ElDateTimePicker popup calendar
{.$DEFINE CALENDAR_USE_WINDOWS_CALENDAR}

// comment this if you compile the module, that uses ElIniFile, but does not
// use VCL
{$DEFINE ELINI_USE_GRAPHICS}

// uncomment this to exclude manifest from linkage (in case you use other controls that include manifest)
{.$define ELPACK_EXCLUDE_MANIFEST}

(*

The following overrides will help you turn the features off for particular projects

*)

{$IFDEF NO_ELTREE_USE_INPLACE_EDITORS}
{$UNDEF ELTREE_USE_INPLACE_EDITORS}
{$DEFINE NO_ELTREE_USE_EXT_EDITORS}
{$ENDIF}

{$IFDEF NO_ELINI_USE_GRAPHICS}
{$UNDEF ELINI_USE_GRAPHICS}
{$ENDIF}

{$IFDEF NO_ELTREE_USE_STYLES}
{$UNDEF ELTREE_USE_STYLES}
{$ENDIF}

{$IFDEF NO_ELTREE_USE_OLE_DRAGDROP}
{$UNDEF ELTREE_USE_OLE_DRAGDROP}
{$ENDIF}

{$IFDEF NO_ELTREE_USE_EXT_EDITORS}
{$UNDEF ELTREE_USE_EXT_EDITORS}
{$ENDIF}

{$IFDEF NO_SUPPORT_STORAGE}
{$UNDEF SUPPORT_STORAGE}
{$ENDIF}

{$IFDEF NO_USE_SOUND_MAP}
{$UNDEF USE_SOUND_MAP}
{$ENDIF}

(*

Different internal directives. Do not modify them.

*)

{$DEFINE ELPACK_COMPLETE}
{$DEFINE ELPACK_PRO}
{.$DEFINE EL_DEMO}
{.$DEFINE LITE}


{$IFNDEF ELPACK_COMPLETE}
  {$UNDEF SUPPORT_STORAGE}
  {$UNDEF ELTREE_USE_EXT_EDITORS}
  {$UNDEF USE_SOUND_MAP}
  {$UNDEF DATETIMEPICKER_SUPPORT_CALENDAR}
  {$UNDEF ELINI_USE_GRAPHICS}
{$ENDIF}

{$ifndef DATETIMEPICKER_SUPPORT_CALENDAR}
{$undef CALENDAR_USE_WINDOWS_CALENDAR}
{$endif}

{$IFDEF LITE}
  {$UNDEF HAS_HTML_RENDER}
{$ENDIF}

{$DEFINE MANUAL_BUTTONS}

{$R-}

{$IFDEF VER90}
  {$DEFINE D_2}
  {$DEFINE D_2_UP}
  {$DEFINE VCL_2_USED}
  {$UNDEF ELPACK_UNICODE}
  {$DEFINE BROKEN_UNICODE}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE D_3}
  {$DEFINE D_3_UP}
  {$DEFINE VCL_3_USED}
  {$UNDEF ELPACK_UNICODE}
  {$DEFINE BROKEN_UNICODE}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE B_3_UP}
  {$DEFINE B_3}
  {$DEFINE VCL_3_USED}
  {$DEFINE BUILDER_USED}
  {$UNDEF ELPACK_UNICODE}
  {$DEFINE BROKEN_UNICODE}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE D_3_UP}
  {$DEFINE D_4_UP}
  {$DEFINE VCL_4_USED}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE B_3_UP}
  {$DEFINE B_4_UP}
  {$DEFINE B_4}
  {$DEFINE VCL_4_USED}
  {$DEFINE BUILDER_USED}
  {$UNDEF ELPACK_UNICODE}
  {$DEFINE BROKEN_UNICODE}
{$ENDIF}

{$IFDEF VER130}
	{$IFDEF BCB}
		{$DEFINE B_3_UP}
		{$DEFINE B_4_UP}
		{$DEFINE B_5_UP}
		{$DEFINE B_5}
		{$DEFINE VCL_4_USED}
		{$DEFINE VCL_5_USED}
		{$DEFINE BUILDER_USED}
    {$ELSE}
		{$DEFINE D_3_UP}
		{$DEFINE D_4_UP}
		{$DEFINE D_5_UP}
		{$DEFINE VCL_4_USED}
		{$DEFINE VCL_5_USED}
		{.DEFINE USEADO}
    {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
	{$IFDEF BCB}
		{$DEFINE B_3_UP}
		{$DEFINE B_4_UP}
		{$DEFINE B_5_UP}
		{$DEFINE B_6_UP}
		{$DEFINE B_6}
		{$DEFINE VCL_4_USED}
		{$DEFINE VCL_5_USED}
		{$DEFINE VCL_6_USED}
		{$DEFINE BUILDER_USED}
    {$ELSE}
		{$DEFINE D_3_UP}
		{$DEFINE D_4_UP}
		{$DEFINE D_5_UP}
		{$DEFINE D_6_UP}
		{$DEFINE D_6}
		{$DEFINE VCL_4_USED}
		{$DEFINE VCL_5_USED}
		{$DEFINE VCL_6_USED}
		{.DEFINE USEADO}
    {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
	{$IFNDEF BCB}
		{$DEFINE D_3_UP}
		{$DEFINE D_4_UP}
		{$DEFINE D_5_UP}
		{$DEFINE D_6_UP}
		{$DEFINE D_7_UP}
		{$DEFINE D_7}
		{$DEFINE VCL_4_USED}
		{$DEFINE VCL_5_USED}
		{$DEFINE VCL_6_USED}
		{$DEFINE VCL_7_USED}
		{.DEFINE USEADO}
    {$ENDIF}
{$ENDIF}

{$ifdef LINUX}
{$define KYLIX_USED}
{$define ELPACK_UNICODE}
{$define CLX_USED}
{$undef ELTREE_USE_OLE_DRAGDROP}
{$else}
{$define MSWINDOWS}
{$endif}

{$ifdef CLX_USED}
{$define ELPACK_UNICODE}
{$undef ELTREE_USE_OLE_DRAGDROP}
{$undef DATETIMEPICKER_SUPPORT_CALENDAR}
{$endif}

{$IFDEF BUILDER_USED}
	{$ObjExportAll On}
{$ENDIF}


{$ifdef DEBUG}
{$J+,R-,I-,Q-,O-,D+,L+,Y+}
{$else}
{$J+,R-,I-,Q-,O-,D-,L-,Y-}
{$endif}
