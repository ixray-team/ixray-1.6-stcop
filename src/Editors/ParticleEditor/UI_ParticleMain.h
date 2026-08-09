#pragma once

enum
{
	COMMAND_EXTFIRST_EXT = COMMAND_MAIN_LAST - 1,

	COMMAND_VALIDATE,

	COMMAND_SELECT_PREVIEW_OBJ,
	COMMAND_EDIT_PREVIEW_PROPS,

	COMMAND_PLAY_CURRENT,
	COMMAND_STOP_CURRENT,
	COMMAND_JUMP_TO_ITEM,
	COMMAND_SAVE_XR,
	COMMAND_LOAD_XR,
	COMMAND_COMPACT_PARTICLES,
	COMMAND_CREATE_GROUP_FROM_SELECTED
};
//------------------------------------------------------------------------------

class CParticleMain :
	public TUI
{
	using inherited = TUI;

public:
	CParticleMain();
	virtual ~CParticleMain();

	virtual char* GetCaption();

	virtual const char* EditorName() { return "particle"; }
	virtual const char* EditorDesc() { return "Particle Editor"; }

	// commands
	virtual void RegisterCommands();
	virtual void OnDrawUI();
};
extern CParticleMain* PUI;