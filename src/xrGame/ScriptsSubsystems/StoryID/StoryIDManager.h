#pragma once
#include "script_game_object.h"

class CScriptStoryIDManager
{
	xr_hash_map<ALife::_OBJECT_ID, shared_str> m_containers_by_id;
	xr_string_map<shared_str, ALife::_OBJECT_ID> m_containers_by_script_story_id;
	mutable xrSRWLock m_containers_lock;

	CScriptStoryIDManager() = default;
	
public:

	CScriptStoryIDManager(const CScriptStoryIDManager&) = delete;
	CScriptStoryIDManager& operator=(const CScriptStoryIDManager&) = delete;
	CScriptStoryIDManager(CScriptStoryIDManager&&) = delete;
	CScriptStoryIDManager& operator=(CScriptStoryIDManager&&) = delete;

	void Register(ALife::_OBJECT_ID obj_id, shared_str script_story_id);
	void Unregister(ALife::_OBJECT_ID obj_id);
	void Unregister(const char* script_story_id);
	ALife::_OBJECT_ID GetID(const char* script_story_id) const;
	const char* GetID(ALife::_OBJECT_ID obj_id) const;
	
	static CScriptStoryIDManager& GetInstance();
	static void VerifiedRegisterObject(CSE_Abstract* se_obj);
	static void script_register(lua_State *L);
};


