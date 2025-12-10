////////////////////////////////////////////////////////////////////////////
//	Module 		: xrServer_Objects.cpp
//	Created 	: 19.09.2002
//  Modified 	: 04.06.2003
//	Author		: Oles Shyshkovtsov, Alexander Maksimchuk, Victor Reutskiy and Dmitriy Iassenev
//	Description : Server objects
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "xrServer_Objects.h"
#include <magic_enum/magic_enum.hpp>
#include "game_base_space.h"
#include "xrServer_Objects_ALife.h"

////////////////////////////////////////////////////////////////////////////
// CSE_Shape
////////////////////////////////////////////////////////////////////////////
CSE_Shape::CSE_Shape						()
{
}

CSE_Shape::~CSE_Shape						()
{
}

void CSE_Shape::cform_read					(NET_Packet	&tNetPacket)
{
	shapes.clear				();
	u8							count;
	tNetPacket.r_u8				(count);
	
	while (count) {
		shape_def				S;
		tNetPacket.r_u8			(S.type);
		switch (S.type) {
			case 0 :
				{
				if (tNetPacket.inistream)
				{
					tNetPacket.r_vec3(S.data.sphere.P);
					tNetPacket.r_float(S.data.sphere.R);
				}
				else
					tNetPacket.r	(&S.data.sphere,sizeof(S.data.sphere));	

				}break;
			case 1 :	
				tNetPacket.r_matrix(S.data.box);
				break;
		}
		shapes.push_back		(S);
		--count;
	}
}

void CSE_Shape::cform_write					(NET_Packet	&tNetPacket)
{
	tNetPacket.w_u8				(u8(shapes.size()));
	for (u32 i=0; i<shapes.size(); ++i) 
	{
		shape_def				&S = shapes[i];
		tNetPacket.w_u8			(S.type);
		switch (S.type) 
		{
			case 0:
				{
				if (tNetPacket.inistream)
				{
					tNetPacket.w_vec3(S.data.sphere.P);
					tNetPacket.w_float(S.data.sphere.R);
				}
				else
					tNetPacket.w	(&S.data.sphere,sizeof(S.data.sphere));
				}break;
			case 1:	
				tNetPacket.w_matrix	(S.data.box);
				break;
		}
	}
}

void CSE_Shape::cform_serialize		(ISaveObject& Object)
{
	BEGIN_CHUNK(Object, "CSE_Shape::cform_read")
	{
		Object << shapes;
	}
}

void CSE_Shape::assign_shapes	(CShapeData::shape_def* _shapes, u32 _cnt)
{
	shapes.resize	(_cnt);
	for (u32 k=0; k<_cnt; k++)
		shapes[k]	= _shapes[k];
}

////////////////////////////////////////////////////////////////////////////
// CSE_Spectator
////////////////////////////////////////////////////////////////////////////
CSE_Spectator::CSE_Spectator				(const char* caSection) : CSE_Abstract(caSection)
{
}

CSE_Spectator::~CSE_Spectator				()
{
}

u8	 CSE_Spectator::g_team					()
{
	return 0;
}

void CSE_Spectator::STATE_Read				(NET_Packet	&tNetPacket, u16 size)
{
}

void CSE_Spectator::STATE_Write				(NET_Packet	&tNetPacket)
{
}

void CSE_Spectator::UPDATE_Read				(NET_Packet	&tNetPacket)
{
}

void CSE_Spectator::UPDATE_Write			(NET_Packet	&tNetPacket)
{
}

void CSE_Spectator::STATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_Spectator::STATE")
	{}
}

void CSE_Spectator::UPDATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_Spectator::UPDATE")
	{}
}

#if !defined(XRGAME_EXPORTS)
void CSE_Spectator::FillProps				(const char* pref, PropItemVec& items)
{
  	inherited::FillProps			(pref,items);
}
#endif // #ifndef XRGAME_EXPORTS

////////////////////////////////////////////////////////////////////////////
// CSE_Temporary
////////////////////////////////////////////////////////////////////////////
CSE_Temporary::CSE_Temporary				(const char* caSection) : CSE_Abstract(caSection)
{
	m_tNodeID					= u32(-1);
}

CSE_Temporary::~CSE_Temporary				()
{
}

void CSE_Temporary::STATE_Read				(NET_Packet	&tNetPacket, u16 size)
{
	tNetPacket.r_u32			(m_tNodeID);
};

void CSE_Temporary::STATE_Write				(NET_Packet	&tNetPacket)
{
	tNetPacket.w_u32			(m_tNodeID);
};

void CSE_Temporary::UPDATE_Read				(NET_Packet	&tNetPacket)
{
};

void CSE_Temporary::UPDATE_Write			(NET_Packet	&tNetPacket)
{
};

void CSE_Temporary::STATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_Temporary::STATE")
	{
		Object << m_tNodeID;
	}
}

void CSE_Temporary::UPDATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_Temporary::UPDATE")
	{}
}

#ifndef XRGAME_EXPORTS
void CSE_Temporary::FillProps				(const char* pref, PropItemVec& values)
{
};
#endif // #ifndef XRGAME_EXPORTS

////////////////////////////////////////////////////////////////////////////
// CSE_Conditional
////////////////////////////////////////////////////////////////////////////
CSE_Conditional::CSE_Conditional				(LPCSTR caSection) : CSE_Abstract(caSection)
{
	m_section_meet_cond = pSettings->r_string(caSection, "meet_cond");
	m_section_not_meet_cond = pSettings->r_string(caSection, "not_meet_cond");
	std::optional<Conditions> temp = magic_enum::enum_cast<Conditions>(pSettings->r_string(caSection, "condition"));
	if (I_ASSERT_M(temp.has_value(), "Conditional spawn section has invalid condition", caSection))
	{
		m_condition = temp.value();
	} else
	{
		m_condition = Conditions::Invalid;
	}
}

CSE_Conditional::~CSE_Conditional				()
{
}

void CSE_Conditional::STATE_Read				(NET_Packet	&tNetPacket, u16 size)
{
}

void CSE_Conditional::STATE_Write				(NET_Packet	&tNetPacket)
{
}

void CSE_Conditional::UPDATE_Read				(NET_Packet	&tNetPacket)
{
}

void CSE_Conditional::UPDATE_Write			(NET_Packet	&tNetPacket)
{
}

void CSE_Conditional::STATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_Conditional::STATE")
	{}
}

void CSE_Conditional::UPDATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_Conditional::UPDATE")
	{}
}

#ifndef XRGAME_EXPORTS
void CSE_Conditional::FillProps				(LPCSTR pref, PropItemVec& values)
{
};
#endif // #ifndef XRGAME_EXPORTS



////////////////////////////////////////////////////////////////////////////
// CSE_PHSkeleton
////////////////////////////////////////////////////////////////////////////
CSE_PHSkeleton::CSE_PHSkeleton(const char* caSection)
{
	source_id					= ALife::INVALID_OBJECT_ID;
	_flags.zero					();
}

CSE_PHSkeleton::~CSE_PHSkeleton()
{

}

void CSE_PHSkeleton::STATE_Read		(NET_Packet	&tNetPacket, u16 size)
{
	CSE_Visual				*visual = smart_cast<CSE_Visual*>(this);
	R_ASSERT				(visual);
	tNetPacket.r_stringZ	(visual->startup_animation);
	tNetPacket.r_u8			(_flags.flags);
	
	auto self = dynamic_cast<CSE_ALifeDynamicObjectVisual*>(this); // I need to access netpacket version here
	R_ASSERT(self);
	
	if(self->m_wVersion < 130)
	{
		u16 ID16;
		tNetPacket.r_u16(ID16);
		source_id = ID16 == 0xffff ? ALife::INVALID_OBJECT_ID : ID16;
	} else
	{
		u32 ID32;
		tNetPacket.r_u32(ID32);
		source_id = ID32;
	}
	if (_flags.test(flSavedData)) {
		data_load(tNetPacket);
	}
}

void CSE_PHSkeleton::STATE_Write		(NET_Packet	&tNetPacket)
{
	CSE_Visual				*visual = smart_cast<CSE_Visual*>(this);
	R_ASSERT				(visual);
	tNetPacket.w_stringZ	(visual->startup_animation);
	tNetPacket.w_u8			(_flags.flags);
	tNetPacket << source_id;
	////////////////////////saving///////////////////////////////////////
	if(_flags.test(flSavedData))
	{
		data_save(tNetPacket);
	}
}

void CSE_PHSkeleton::data_load(NET_Packet &tNetPacket)
{
	saved_bones.net_Load(tNetPacket);
	_flags.set(flSavedData,true);
}

void CSE_PHSkeleton::data_save(NET_Packet &tNetPacket)
{
	saved_bones.net_Save(tNetPacket);
//	this comment is added by Dima (correct me if this is wrong)
//  if we call 2 times in a row StateWrite then we get different results
//	WHY???
//	_flags.set(flSavedData,false);
}

void CSE_PHSkeleton::load(NET_Packet &tNetPacket)
{
	_flags.assign(tNetPacket.r_u8());
	data_load(tNetPacket);
	source_id = ALife::INVALID_OBJECT_ID;
}
void CSE_PHSkeleton::UPDATE_Write(NET_Packet &tNetPacket)
{

}

void CSE_PHSkeleton::UPDATE_Read(NET_Packet &tNetPacket)
{

}

void CSE_PHSkeleton::STATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_PHSkeleton::STATE")
	{
		CSE_Visual* visual = smart_cast<CSE_Visual*>(this);
		R_ASSERT(visual);
		Object << visual->startup_animation << _flags.flags << source_id;
	}
}

void CSE_PHSkeleton::UPDATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_PHSkeleton::UPDATE")
	{}
}

void CSE_PHSkeleton::data_serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_PHSkeleton::data")
	{
		saved_bones.net_Serialize(Object);
	}
}

#ifndef XRGAME_EXPORTS
void CSE_PHSkeleton::FillProps				(const char* pref, PropItemVec& values)
{
}
#endif // #ifndef XRGAME_EXPORTS

CSE_AbstractVisual::CSE_AbstractVisual(const char* section):inherited1(section),inherited2(section)
{
}

CSE_AbstractVisual::~CSE_AbstractVisual()
{
}

void CSE_AbstractVisual::STATE_Read		(NET_Packet	&tNetPacket, u16 size)
{
	visual_read						(tNetPacket,m_wVersion);
	tNetPacket.r_stringZ			(startup_animation);
}

void CSE_AbstractVisual::STATE_Write	(NET_Packet	&tNetPacket)
{
	visual_write				(tNetPacket);
	tNetPacket.w_stringZ		(startup_animation);
}

#if !defined(XRGAME_EXPORTS)
void CSE_AbstractVisual::FillProps		(const char* pref, PropItemVec& values)
{
	inherited1::FillProps			(pref,values);
	inherited2::FillProps			(pref,values);
}
#endif // #ifndef XRGAME_EXPORTS

void CSE_AbstractVisual::UPDATE_Read	(NET_Packet	&tNetPacket)
{
}

void CSE_AbstractVisual::UPDATE_Write	(NET_Packet	&tNetPacket)
{
}

void CSE_AbstractVisual::STATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_AbstractVisual::STATE")
	{
		visual_serialize(Object);
		Object << startup_animation;
	}
}

void CSE_AbstractVisual::UPDATE_Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CSE_AbstractVisual::UPDATE")
	{}
}

const char*	CSE_AbstractVisual::getStartupAnimation		()
{
	return *startup_animation;
}

CSE_Visual* CSE_AbstractVisual::visual					()
{
	return this;
}
