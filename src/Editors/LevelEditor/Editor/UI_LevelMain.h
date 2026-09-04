#pragma once

enum ELECommand
{
	COMMAND_EXTFIRST_EXT = COMMAND_MAIN_LAST-1,

	COMMAND_CHANGE_TARGET,
	COMMAND_ENABLE_TARGET,
	COMMAND_READONLY_TARGET,
	COMMAND_SHOW_TARGET,

    COMMAND_MULTI_RENAME_OBJECTS = 64,

	COMMAND_REFRESH_SOUND_ENVS,
    COMMAND_REFRESH_SOUND_ENV_GEOMETRY,
    
	COMMAND_CLEAN_LIBRARY,
    COMMAND_LIBRARY_EDITOR,
    COMMAND_LANIM_EDITOR,
	COMMAND_CLEAR_DEBUG_DRAW,
    COMMAND_IMPORT_COMPILER_ERROR,
    COMMAND_IMPORT_AICOMPILER_ERROR,
    COMMAND_EXPORT_COMPILER_ERROR,
	COMMAND_VALIDATE_SCENE,
    COMMAND_RELOAD_OBJECTS,

	COMMAND_CUT,
	COMMAND_COPY,
	COMMAND_PASTE,
    COMMAND_DUPLICATE,
	COMMAND_LOAD_SELECTION,
	COMMAND_SAVE_SELECTION,
    COMMAND_LOAD_LEVEL_PART, 
    COMMAND_UNLOAD_LEVEL_PART,   
    
    COMMAND_CLEAR_SCENE_SUMMARY,
    COMMAND_COLLECT_SCENE_SUMMARY,
	COMMAND_SHOW_SCENE_SUMMARY,			
	COMMAND_EXPORT_SCENE_SUMMARY,
    COMMAND_SCENE_HIGHLIGHT_TEXTURE,

	COMMAND_OPTIONS,
	COMMAND_BUILD,

	COMMAND_MAKE_GAME,
	COMMAND_MAKE_PUDDLES,
	COMMAND_MAKE_PLANARS,
    COMMAND_MAKE_DETAILS,
	COMMAND_MAKE_HOM,
    COMMAND_MAKE_SOM,
    COMMAND_MAKE_AIMAP,
    COMMAND_MAKE_AIMAP_LEGACY,

	COMMAND_INVERT_SELECTION_ALL = 100,
	COMMAND_SELECT_ALL,
	COMMAND_DESELECT_ALL,
	COMMAND_DELETE_SELECTION,
	COMMAND_HIDE_UNSEL,
	COMMAND_HIDE_SEL,
	COMMAND_HIDE_ALL,
    COMMAND_LOCK_ALL,
    COMMAND_LOCK_SEL,
    COMMAND_LOCK_UNSEL,

    COMMAND_SET_SNAP_OBJECTS,
    COMMAND_ADD_SEL_SNAP_OBJECTS,
	COMMAND_DEL_SEL_SNAP_OBJECTS,
    COMMAND_CLEAR_SNAP_OBJECTS,
	COMMAND_SELECT_SNAP_OBJECTS,

    COMMAND_LOAD_FIRSTRECENT = 116,

    COMMAND_SHOWCONTEXTMENU,
    COMMAND_SHOW_CLIP_EDITOR,

    COMMAND_CREATE_SHAPE_BOX,
    COMMAND_CREATE_SHAPE_SPHERE,

    COMMAND_LE_END
};

class CLevelMain :
	public TUI
{
	typedef TUI inherited;

	virtual void RealUpdateScene();
	virtual void RealQuit();

public:
	C3DCursor* m_Cursor;
	xr_task_group LoaderEvent;

public:
	CLevelMain();
	virtual ~CLevelMain();

	virtual char* GetCaption() override;
	virtual void ResetStatus() override;
	virtual void SetStatus(const char* s, bool bOutLog = true) override;
	virtual void ProgressDraw() override;

	virtual const char* EditorName() override { return "level"; }
	virtual const char* EditorDesc() override { return "Level Editor"; }

	void ShowContextMenu(int cls);
	bool PickGround(Fvector& hitpoint, const Fvector& start, const Fvector& direction, int bSnap = 1, Fvector* hitnormal = 0);
	bool SelectionFrustum(CFrustum& frustum);

	virtual bool ApplyShortCut(u32 Key, TShiftState Shift) override;

	// commands
	virtual void RegisterCommands() override;

	virtual void SaveSettings(nlohmann::json&) override;
	virtual void LoadSettings(nlohmann::json&) override;
	virtual Ivector2 GetRenderMousePosition() const override;
	virtual void OnDrawUI() override;

	virtual bool IsPlayInEditor();

private:
};

extern CLevelMain*	LUI;