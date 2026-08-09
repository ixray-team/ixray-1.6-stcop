#pragma once

enum
{
	COMMAND_EXTFIRST_EXT = COMMAND_MAIN_LAST-1,

    COMMAND_MAKE_PREVIEW,
    COMMAND_IMPORT,
    COMMAND_EXPORT_OGF,
    COMMAND_EXPORT_OMF,
    COMMAND_EXPORT_DM,
    COMMAND_EXPORT_OBJ,
    COMMAND_EXPORT_CPP,
    COMMAND_BATCH_CONVERT,
    COMMAND_PREVIEW_OBJ_PREF,
    COMMAND_SELECT_PREVIEW_OBJ,
	COMMAND_SHOW_CLIPMAKER,
    COMMAND_OPTIMIZE_MOTIONS,
    COMMAND_MAKE_THUMBNAIL,
    COMMAND_CHANGE_TARGET,

    COMMAND_LOAD_FIRSTRECENT,
};
//------------------------------------------------------------------------------

class CActorMain :
	public TUI
{
	using inherited = TUI ;

public:
	CActorMain();
	virtual ~CActorMain();

	virtual char* GetCaption();

	virtual void ResetStatus();
	virtual void SetStatus(const char* s, bool bOutLog);

	virtual const char* EditorName() { return "actor"; }
	virtual const char* EditorDesc() { return "Actor Editor"; }

	// commands
	virtual void RegisterCommands();

protected:
	virtual void OnDrawUI();

	virtual Ivector2 GetRenderMousePosition() const;
};

extern CActorMain*	AUI;

class CAEPreferences :
    public CCustomPreferences
{
    typedef CCustomPreferences inherited;

public:
    size_t PrefConfigVer = 0;

public:
					CAEPreferences():bAlwaysShowKeyBar12(false),bAlwaysShowKeyBar34(false){}
    bool							bAlwaysShowKeyBar12;
    bool							bAlwaysShowKeyBar34;

    virtual void 	Load			();
    virtual void 	Save			();
    virtual void	FillProp        (PropItemVec& items);

};