#pragma once
#include "game_cl_mp.h"
#include "VoiceChat.h"

class CUIGameFMP;

class game_cl_freemp final : public game_cl_mp
{
private:
	using inherited = game_cl_mp;
	CUIGameFMP* m_game_ui;
	CVoiceChat* m_pVoiceChat = nullptr;

public:
	game_cl_freemp();
	virtual	~game_cl_freemp();


	virtual CUIGameCustom* createGameUI();
	virtual void SetGameUI(CUIGameCustom*);

	virtual	void net_import_state(NET_Packet& P);
	virtual	void net_import_update(NET_Packet& P);

	virtual void shedule_Update(u32 dt);
	virtual void TranslateGameMessage(u32 msg, NET_Packet& P);

	virtual	bool OnKeyboardPress(int key);
	virtual void OnConnected();

	virtual bool OnKeyboardRelease(int key) override;

	virtual const char* GetGameScore(string32& score_dest);
	virtual void OnRender() override;
	virtual void OnVoiceMessage(NET_Packet* P) override;

	virtual game_cl_mp* cast_game_cl_mp() override { return this; }
	virtual game_cl_freemp* cast_game_cl_freemp() override { return this; }
};

bool IsGameTypeSingleCompatible();