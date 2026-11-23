////////////////////////////////////////////////////////////////////////////
//	Module 		: purchase_list.cpp
//	Created 	: 12.01.2006
//  Modified 	: 12.01.2006
//	Author		: Dmitriy Iassenev
//	Description : purchase list class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "purchase_list.h"
#include "InventoryOwner.h"
#include "GameObject.h"
#include "ai_object_location.h"
#include "Level.h"
#include "Inventory.h"
#include "InventoryBox.h"
#include "ai_space.h"
#include "alife_simulator.h"

extern CSE_Abstract* CALifeSimulator__spawn_item_build(CALifeSimulator* self_, const char* section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent, const char* build);

static float min_deficit_factor = .3f;

struct SPurchaseItem
{
	u32 count = 0;
	float prob = 1.0f;
	shared_str build;
};

void CPurchaseList::process(CInifile& ini_file, const char* section, CInventoryOwner& owner)
{
	owner.sell_useless_items();

	m_deficits.clear();

	const CGameObject& game_object = *owner.cast_game_object();

	xr_map<shared_str, SPurchaseItem> FinalSupplises = {};
	
	auto ProcessSingleTradeItemSettingFunc = [&](shared_str loc_section, u32 count, float prob, shared_str build)
	{
		FinalSupplises[loc_section] = {count, prob, build};
	};

	auto ProcessMultipleTradeItemsSettingsFunc = [&](this auto self, shared_str loc_section, u32 count, float prob)
	{
		if(!I_ASSERT(pSettings->section_exist(loc_section)))
		{
			return;
		}

		for(auto& Item : pSettings->r_section(loc_section).Data)
		{
			if (!pSettings->section_exist(Item.first))
			{
				if(Item.first.c_str()[0] == '$')
				{
					LPCSTR section_name = Item.first.c_str()+1;
					self(section_name, count, prob);
					continue;
				}
				Msg("! Section [%s] (parsing trade list [%s]) doesn't exist!", Item.first.c_str(), loc_section.c_str());
				continue;
			}
			ProcessSingleTradeItemSettingFunc(Item.first, count, prob, nullptr);
		}
	};
	
	CInifile::Sect& S = ini_file.r_section(section);
	
	for (const auto& sect : S.Data)
	{
		if (!sect.second.size())
		{
			Msg("! PurchaseList : cannot handle lines in section without values! Section [%s], file [%s]", sect.first.c_str(), ini_file.fname());
			continue;
		}

		string256 temp0 = {}, temp1 = {};

		u32 count = atoi(_GetItem(*sect.second, 0, temp0));
		float prob = _GetItemCount(*sect.second) >= 2 ? atof(_GetItem(*sect.second, 1, temp1)) : 1.0f;

		shared_str build = _GetItemCount(*sect.second) >= 3 ? _GetItem(*sect.second, 2, temp1) : nullptr;

		if (!pSettings->section_exist(sect.first))
		{
			if(sect.first.c_str()[0] == '$')
			{
				LPCSTR section_name = sect.first.c_str()+1;
				ProcessMultipleTradeItemsSettingsFunc(section_name, count, prob);
				continue;
			}
			Msg("! Section [%s] doesn't exist! File [%s]", sect.first.c_str(), ini_file.fname());
			continue;
		}

		ProcessSingleTradeItemSettingFunc(sect.first, count, prob, build);
	}

	auto& Inventory = owner.inventory();
	for (auto& elem : FinalSupplises)
	{
		if (Inventory.GetTraderExternalStorageMode())
		{
			auto Storage = Inventory.FindSuitableStorage(elem.first);
			if (!I_ASSERT(Storage))
			{
				continue;
			}
			process(*Storage, elem.first, elem.second.count, elem.second.prob, elem.second.build);
		}
		else
		{
			process(game_object, elem.first, elem.second.count, elem.second.prob, elem.second.build);
		}
	}
}

void CPurchaseList::process(const CGameObject& owner, const shared_str& name, const u32& count, const float& probability, const shared_str& build)
{
	VERIFY3(count, "Invalid count for section in the purchase list", *name);
	VERIFY3(!fis_zero(probability, EPS_S), "Invalid probability for section in the purchase list", *name);

	const Fvector& position = owner.Position();
	const u32& level_vertex_id = owner.ai_location().level_vertex_id();
	const GameGraph::_GRAPH_ID game_vertex_id = owner.ai_location().game_vertex_id();
	const ALife::_OBJECT_ID& id = owner.ID();
	CRandom random((u32)(CPU::QPC() & u32(-1)));
	u32 i = 0, j = 0;
	for (; i < count; ++i)
	{
		if (random.randF() > probability)
		{
			continue;
		}

		++j;
		CALifeSimulator__spawn_item_build(ai().get_alife(), *name, position, level_vertex_id, game_vertex_id, id, *build);
	}

	DEFICITS::const_iterator I = m_deficits.find(name);
	VERIFY3(I == m_deficits.end(), "Duplicate section in the purchase list", *name);
	m_deficits.insert(
		std::make_pair(
			name,
			(float)count * probability /
				std::max((float)j, min_deficit_factor)
		)
	);
}
