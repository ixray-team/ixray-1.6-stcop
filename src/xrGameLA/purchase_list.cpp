////////////////////////////////////////////////////////////////////////////
//	Module 		: purchase_list.cpp
//	Created 	: 12.01.2006
//  Modified 	: 12.01.2006
//	Author		: Dmitriy Iassenev
//	Description : purchase list class
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "purchase_list.h"
#include "inventoryowner.h"
#include "gameobject.h"
#include "ai_object_location.h"
#include "level.h"

static float min_deficit_factor = .3f;

void CPurchaseList::process	(CInifile &ini_file, LPCSTR section, CInventoryOwner &owner)
{
	const CGameObject& game_object = smart_cast<const CGameObject&>(owner);
	owner.sell_useless_items();
	m_deficits.clear();

	for (auto I = cnt.begin(); I != cnt.end(); ++I) {
		int					count = _GetItemCount(*(*I).second);
		THROW3(count <= 2, "Invalid parameters for item", *(*I).first);
		if (count > 0)
		{
			string256			temp0, temp1;
			process(
				game_object,
				(*I).first,
				atoi(_GetItem(*(*I).second, 0, temp0)),
				(float)atof(_GetItem(*(*I).second, 1, temp1))
			);
		}
	}
}

void CPurchaseList::process	(const CGameObject &owner, const shared_str &name, const u32 &count, const float &probability)
{
	if (!count || fis_zero(probability,EPS_S)) return;

	const Fvector			&position = owner.Position();
	const u32				&level_vertex_id = owner.ai_location().level_vertex_id();
	const ALife::_OBJECT_ID	&id = owner.ID();
	CRandom					random((u32)(CPU::QPC() & u32(-1)));
	u32 i, j = 0;
	for (i=0, j=0; i<count; ++i) {
		if (random.randF() > probability)
			continue;

		++j;
		Level().spawn_item		(*name,position,level_vertex_id,id,false);
	}

	DEFICITS::const_iterator	I = m_deficits.find(name);
	VERIFY3						(I == m_deficits.end(),"Duplicate section in the purchase list",*name);
	m_deficits.insert			(
		std::make_pair(
			name,
			(float)count*probability
			/
			_max((float)j,min_deficit_factor)
		)
	);
}
