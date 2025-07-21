////////////////////////////////////////////////////////////////////////////
//	Module 		: memory_space.h
//	Created 	: 25.12.2003
//  Modified 	: 25.12.2003
//	Author		: Dmitriy Iassenev
//	Description : Memory space
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrSound/ai_sounds.h"
#include "xrserver_space.h"
#include "alife_space.h"
#include "../xrScripts/script_export_space.h"

#define USE_STALKER_VISION_FOR_MONSTERS

class CEntityAlive;
class CGameObject;

namespace MemorySpace 
{
	struct CNotYetVisibleObject
	{
		const CGameObject* m_object;
		float m_value;
		u32   m_update_time;
		u32   m_prev_time;
	};

	struct SObjectParams 
	{
		u32     m_level_vertex_id;
		Fvector m_position;
	};

	struct CObjectParams :
		public SObjectParams 
	{
		IC void fill(const CGameObject* game_object);
	};

	struct SMemoryObject
	{
		u32  m_level_time;
		u32  m_last_level_time;
		bool m_enabled;

		SMemoryObject() :
			m_level_time(0),
			m_last_level_time(0),
			m_enabled(true)
		{
		}

		IC void fill()
		{
			m_enabled = true;
		}
	};

	struct CMemoryObject :
		public SMemoryObject
	{
		const CGameObject* m_object;
		CObjectParams m_object_params;
		CObjectParams m_self_params;
		Flags64 m_squad_mask;

		IC			CMemoryObject();
		    bool	operator==(u16 id) const; 
	 static u16		object_id(const CObject* object);
		IC	void	fill(const CGameObject* game_object, const CGameObject* self, const u64& mask);
	};

	struct CVisibleObject : 
		CMemoryObject 
	{
		using inherited = CMemoryObject;
		Flags64 m_visible;

	public:
		IC CVisibleObject()
		{
			m_visible.zero();
		}

		IC bool visible(const u64& mask) const
		{
			return (!!m_visible.test(mask));
		}

		IC void visible(const u64& mask, bool value)
		{
			m_visible.set(mask, value ? TRUE : FALSE);
		}

		IC void fill(const CGameObject* game_object, const CGameObject* self, const u64& mask, const u64& visibility_mask)
		{
			inherited::fill(game_object, self, mask);
			m_visible.set(visibility_mask, TRUE);
		}
	};

	struct CHitObject : 
		public CMemoryObject
	{
		Fvector m_direction;
		u16     m_bone_index;
		float   m_amount;
	};

	struct CSoundObject : 
		public CMemoryObject
	{
		ESoundTypes m_sound_type;
		float       m_power;

		IC void fill(const CGameObject* game_object, const CGameObject* self, const ESoundTypes sound_type, const float sound_power, const u64& mask)
		{
			CMemoryObject::fill(game_object, self, mask);
			m_sound_type = sound_type;
			m_power = sound_power;
		}

		IC int	sound_type() const
		{
			return (int(m_sound_type));
		}
	};

	struct CMemoryInfo : 
		public CVisibleObject 
	{
		bool m_visual_info;
		bool m_sound_info;
		bool m_hit_info;

		CMemoryInfo()
		{
			m_visual_info = false;
			m_sound_info = false;
			m_hit_info = false;
		}
		DECLARE_SCRIPT_REGISTER_FUNCTION
	};

	struct SLevelTimePredicate 
	{
		bool operator()(const CMemoryObject& object1, const CMemoryObject& object2) const
		{
			return (object1.m_level_time < object2.m_level_time);
		}
	};
};

using namespace MemorySpace;