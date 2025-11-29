////////////////////////////////////////////////////////////////////////////
//	Module 		: memory_space.h
//	Created 	: 25.12.2003
//  Modified 	: 25.12.2003
//	Author		: Dmitriy Iassenev
//	Description : Memory space
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrSound/ai_sounds.h"
#include "xrServer_Space.h"
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
		virtual ~SObjectParams() = default;
		u32     m_level_vertex_id;
		Fvector m_position;

		virtual void Serialize(ISaveObject& Object) {
			Object << m_level_vertex_id << m_position;
#ifdef USE_ORIENTATION
			BEGIN_CHUNK(Object,"SObjectParams::Orientation")
			{
				Object << m_orientation;
			}
#endif
		}
	};

	struct CObjectParams :
		public SObjectParams 
	{
		IC void fill(const CGameObject* game_object);
	};

	struct SMemoryObject
	{
		virtual ~SMemoryObject() = default;
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

		virtual void Serialize(ISaveObject& Object) {
#ifdef USE_GAME_TIME
			BEGIN_CHUNK(Object,"SMemoryObject::game_time")
			{
				Object << m_game_time;
			}
#endif
#ifdef USE_LEVEL_TIME
			BEGIN_CHUNK(Object,"SMemoryObject::level_time")
			{
				Object << m_level_time;
			}
#endif
#ifdef USE_LAST_GAME_TIME
			BEGIN_CHUNK(Object,"SMemoryObject::last_game_time")
			{
				Object << m_last_game_time;
			}
#endif
#ifdef USE_LAST_LEVEL_TIME
			BEGIN_CHUNK(Object,"SMemoryObject::last_level_time")
			{
				Object << m_last_level_time;
			}
#endif
#ifdef USE_FIRST_GAME_TIME
			BEGIN_CHUNK(Object,"SMemoryObject::first_game_time")
			{
				Object << m_first_game_time;
			}
#endif
#ifdef USE_FIRST_LEVEL_TIME
			BEGIN_CHUNK(Object,"SMemoryObject::first_level_time")
			{
				Object << m_first_level_time;
			}
#endif
#ifdef USE_UPDATE_COUNT
			BEGIN_CHUNK(Object,"SMemoryObject::update_count")
			{
				Object << m_update_count;
			}
#endif
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
		    bool	operator==(ALife::_OBJECT_ID id) const; 
	 static ALife::_OBJECT_ID		object_id(const CObject* object);
		IC	void	fill(const CGameObject* game_object, const CGameObject* self, const u64& mask);

		virtual void Serialize(ISaveObject& Object) override {
			SMemoryObject::Serialize(Object);
			m_object_params.Serialize(Object);
			m_self_params.Serialize(Object);
		}
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
			m_visible.set(mask, value ? true : false);
		}

		IC void fill(const CGameObject* game_object, const CGameObject* self, const u64& mask, const u64& visibility_mask)
		{
			inherited::fill(game_object, self, mask);
			m_visible.set(visibility_mask, true);
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