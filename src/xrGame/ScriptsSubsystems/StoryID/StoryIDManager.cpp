#include "StdAfx.h"
#include "ScriptsSubsystems/StoryID/StoryIDManager.h"

#include "alife_object_registry.h"
#include "alife_simulator.h"

CScriptStoryIDManager& CScriptStoryIDManager::GetInstance()
{
    static CScriptStoryIDManager instance;
    return instance;
}

void CScriptStoryIDManager::VerifiedRegisterObject(CSE_Abstract* se_obj)
{
    auto& self = CScriptStoryIDManager::GetInstance();
    {
        if (auto Casted = smart_cast<CSE_ALifeDynamicObject*>(se_obj); Casted && Casted->m_script_story_ID.size())
        {
            self.Register(se_obj->ID, Casted->m_script_story_ID);
            return;
        }
    }
    auto& ini = se_obj->spawn_ini();
    if (ini.section_exist("story_object"))
    {
        LPCSTR key;
        LPCSTR value;
        if (!ini.r_line("story_object", 0, &key, &value) || !key)
        {
            R_ASSERT3(false, "There is no 'story_id' field in [story_object] section :object", se_obj->name());
        }
        if (value)
            self.Register(se_obj->ID, value);
        return;
    }
    auto story_id = READ_IF_EXISTS(pSettings, r_string, se_obj->name(), "story_id", nullptr);
    if (story_id)
    {
        self.Register(se_obj->ID, story_id);
    }
}

namespace ScriptStoryIDManager
{

    ALife::_OBJECT_ID get(CScriptStoryIDManager& manager, LPCSTR story_id)
    {
       return manager.GetID(story_id);
    }

    LPCSTR get_story_id(CScriptStoryIDManager& manager, ALife::_OBJECT_ID id)
    {
       return manager.GetID(id);
    }

    void Register(CScriptStoryIDManager& manager, ALife::_OBJECT_ID obj_id, LPCSTR story_id, bool registered)
    {
        manager.Register(obj_id, story_id);
    }
}

void CScriptStoryIDManager::script_register(lua_State* L)
{
    using namespace luabind;
    
    module(L, "story_objects")[
        class_<CScriptStoryIDManager>("CScriptStoryIDManager")
            .def("register",	&ScriptStoryIDManager::Register)
            .def("unregister_by_id", (void(CScriptStoryIDManager::*)(ALife::_OBJECT_ID))&CScriptStoryIDManager::Unregister)
            .def("unregister_by_story_id", (void(CScriptStoryIDManager::*)(LPCSTR))&CScriptStoryIDManager::Unregister)
            .def("get", &ScriptStoryIDManager::get)
            .def("get_story_id", &ScriptStoryIDManager::get_story_id),
            def("get_story_objects_registry", &CScriptStoryIDManager::GetInstance),
            def("check_spawn_ini_for_story_id", &CScriptStoryIDManager::VerifiedRegisterObject)
            ];
}

void CScriptStoryIDManager::Register(ALife::_OBJECT_ID obj_id, shared_str script_story_id)
{
    xrSRWLockGuard guard(m_containers_lock);
    auto ByIDIt = m_containers_by_id.find(obj_id);
    auto ByScriptStoryIDIt = m_containers_by_script_story_id.find(script_story_id);
    if (ByScriptStoryIDIt != m_containers_by_script_story_id.end() && ByScriptStoryIDIt->second != obj_id)
    {
        xr_string message = "You are trying to spawn two or more objects with the same story_id:[";
        message.append(script_story_id.c_str());
        message.append("] --> [");
        auto ExistName = ai().alife().objects().object(ByScriptStoryIDIt->second)->name();
        message.append(ExistName);
        message.append("] try to add:[");
        auto NewName = ai().alife().objects().object(obj_id)->name();
        message.append(NewName);
        message.append("]");
        R_ASSERT2(ByScriptStoryIDIt == m_containers_by_script_story_id.end(), message.c_str());
    }
    if (ByIDIt != m_containers_by_id.end()){
        VERIFY(ByScriptStoryIDIt != m_containers_by_script_story_id.end());
        if(ByScriptStoryIDIt->first != script_story_id){
            xr_string message = "Object [";
            message.append(script_story_id.c_str());
            message.append("] is already in story_objects_registry with story_id[");
            message.append(ByIDIt->second.c_str());
            R_ASSERT2(ByScriptStoryIDIt != m_containers_by_script_story_id.end(), message.c_str());
        }else
        {
            return;
        }
    }
    m_containers_by_id[obj_id] = script_story_id;
    m_containers_by_script_story_id[script_story_id] = obj_id;
}

void CScriptStoryIDManager::Unregister(ALife::_OBJECT_ID obj_id)
{
    xrSRWLockGuard guard(m_containers_lock);
    if (m_containers_by_id.contains(obj_id)){
        auto elem = *m_containers_by_id.find(obj_id);
        m_containers_by_id.erase(elem.first);
        m_containers_by_script_story_id.erase(elem.second);
    }
}

void CScriptStoryIDManager::Unregister(LPCSTR script_story_id)
{
    xrSRWLockGuard guard(m_containers_lock);
    if (m_containers_by_script_story_id.contains(script_story_id)){
        auto elem = *m_containers_by_script_story_id.find(script_story_id);
        m_containers_by_id.erase(elem.second);
        m_containers_by_script_story_id.erase(elem.first);
    }
}

ALife::_OBJECT_ID CScriptStoryIDManager::GetID(LPCSTR script_story_id) const
{
    xrSRWLockGuard guard(m_containers_lock, true);
    auto it = m_containers_by_script_story_id.find(script_story_id);
    return it != m_containers_by_script_story_id.end() ? it->second : ALife::_OBJECT_ID(-1);
}

LPCSTR CScriptStoryIDManager::GetID(ALife::_OBJECT_ID obj_id) const
{
    xrSRWLockGuard guard(m_containers_lock, true);
    auto it = m_containers_by_id.find(obj_id);
    return it != m_containers_by_id.end() ? it->second.c_str() : nullptr;
}
