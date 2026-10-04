#include "stdafx.h"

CCustomObject* EScene::FindObjectByName( const char* name, ObjClassID classfilter )
{
	if(!name)
	return NULL;
	
	CCustomObject* object = 0;
	if (classfilter==OBJCLASS_DUMMY)
	{
		SceneToolsMapPairIt _I = m_SceneTools.begin();
		SceneToolsMapPairIt _E = m_SceneTools.end();
		for (; _I!=_E; ++_I)
		{
			ESceneCustomOTool* mt = smart_cast<ESceneCustomOTool*>(_I->second);

			if (mt&&(0!=(object=mt->FindObjectByName(name))))
				return object;
		}
	}else{
		ESceneCustomOTool* mt = GetOTool(classfilter); VERIFY(mt);
		if (mt&&(0!=(object=mt->FindObjectByName(name)))) return object;
	}
	return object;
}

CCustomObject* EScene::FindObjectByName(const char* name, CCustomObject* pass_object)
{
	CCustomObject* object = 0;
	SceneToolsMapPairIt _I = m_SceneTools.begin();
	SceneToolsMapPairIt _E = m_SceneTools.end();
	for (; _I != _E; _I++)
	{
		ESceneCustomOTool* mt = smart_cast<ESceneCustomOTool*>(_I->second);
		if (mt && (0 != (object = mt->FindObjectByName(name, pass_object))))
		{
			return object;
		}
	}

	return 0;
}

bool EScene::FindDuplicateName()
{
	xr_hash_set<shared_str> nameSet;

	for (const auto& [key, tool] : m_SceneTools)
	{
		auto* customTool = smart_cast<ESceneCustomOTool*>(tool);
		if (!customTool)
			continue;

		for (CCustomObject* object : customTool->GetObjects())
		{
			const shared_str& name = object->GetName();
			auto [iterator, inserted] = nameSet.insert(name);

			if (!inserted)
			{
				ELog.DlgMsg(mtError, "Duplicate object name already exists: '%s'", *name);
				return true;
			}
		}
	}

	return false;
}

static bool ParseNameIndex(const char* Str, u32& Index)
{
	if (!Str[0] || (Str[0] == '0' && Str[1]))
	{
		return false;
	}

	u64 Value = 0;
	for (; *Str; ++Str)
	{
		if (*Str < '0' || *Str > '9')
		{
			return false;
		}

		Value = Value * 10 + u64(*Str - '0');
		if (Value >= u64(type_max(int)))
		{
			return false;
		}
	}

	Index = u32(Value);
	return true;
}

void EScene::GenObjectName(ObjClassID ClsID, char* Buffer, const char* Pref)
{
	const bool HasPrefix = Pref && Pref[0];

	xr_string Base;
	if (HasPrefix)
	{
		Base = Pref;
	}
	else
	{
		ESceneCustomOTool* ObjTool = GetOTool(ClsID);
		VERIFY(ObjTool);
		Base = ObjTool->ClassName();
	}

	const char* BaseStr = Base.c_str();
	const size_t BaseLen = Base.size();

	xr_vector<u32> UsedSlots;
	for (const auto& [Key, Tool] : m_SceneTools)
	{
		auto* CustomTool = smart_cast<ESceneCustomOTool*>(Tool);
		if (!CustomTool)
		{
			continue;
		}

		for (CCustomObject* Object : CustomTool->GetObjects())
		{
			const char* Name = Object->GetName();

			if (!Name || _strnicmp(Name, BaseStr, BaseLen) != 0)
			{
				continue;
			}

			const char* Tail = Name + BaseLen;
			if (Tail[0] == 0)
			{
				if (HasPrefix)
				{
					UsedSlots.push_back(0);
				}
				continue;
			}

			u32 Index = 0;
			if (Tail[0] == '_' && ParseNameIndex(Tail + 1, Index))
			{
				UsedSlots.push_back(HasPrefix ? Index + 1 : Index);
			}
		}
	}

	xr_vector<bool> Busy(UsedSlots.size() + 1, false);
	for (u32 Slot : UsedSlots)
	{
		if (Slot < Busy.size())
		{
			Busy[Slot] = true;
		}
	}

	u32 FreeSlot = 0;
	while (Busy[FreeSlot])
	{
		++FreeSlot;
	}

	xr_string Result = Base;
	if (!HasPrefix)
	{
		Result += "_" + xr_string::ToString(int(FreeSlot));
	}
	else if (FreeSlot != 0)
	{
		Result += "_" + xr_string::ToString(int(FreeSlot - 1));
	}

	xr_strcpy(Buffer, 256, Result.c_str());
}