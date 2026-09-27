#include "stdafx.h"
#include "EditorContext.h"
#include "ui_main.h"
#include "device.h"
#include "Library.h"
#include "ImageManager.h"
#include "render.h"
#include "UI_ToolsCustom.h"
#include "EditorPreferences.h"
#include "ELog.h"
#include "SoundManager.h"
#include "../Engine/XrGameMaterialLibraryEditors.h"

ECORE_API EditorContext EContext;

ECORE_API void BindEditorContext()
{
	EContext.Lib = &Lib;
	EContext.ImageLib = &ImageLib;
	EContext.Render = &RImplementation;
	EContext.Tools = Tools;
	EContext.Prefs = EPrefs;
	EContext.Log = &ELog;
	EContext.SndLib = SndLib;
	EContext.Materials = GameMaterialLibraryEditors;
}
