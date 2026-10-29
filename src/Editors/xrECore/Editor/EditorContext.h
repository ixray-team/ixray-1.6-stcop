#pragma once

class TUI;
class CEditorRenderDevice;
class ELibrary;
class CImageManager;
class CRender;
class CToolCustom;
class CCustomPreferences;
class CLog;
class CSoundManager;
class XrGameMaterialLibraryEditors;
class CXVisor;

struct ECORE_API EditorContext
{
	TUI* UI = nullptr;
	ELibrary* Lib = nullptr;
	CImageManager* ImageLib = nullptr;
	CRender* Render = nullptr;
	CToolCustom* Tools = nullptr;
	CCustomPreferences* Prefs = nullptr;
	CLog* Log = nullptr;
	CSoundManager* SndLib = nullptr;
	XrGameMaterialLibraryEditors* Materials = nullptr;
	// The script and config analyzer, owned by the Level Editor
	CXVisor* Visor = nullptr;
};

extern ECORE_API EditorContext EContext;
ECORE_API void BindEditorContext();

void BindEditorRenderHost();
void DestroyEditorRenderHostObjects();
