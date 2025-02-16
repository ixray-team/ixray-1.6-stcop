#include "stdafx.h"
#include "character_supplies.h"

static int ItemIndexOf(LPCSTR haystack, LPCSTR needle)
{
	int j = 0;
	string128 item;
	_GetItem(haystack, j, item);
	while (xr_strlen(item) > 0)
	{
		if (xr_strcmp(item, needle) == 0)
		{
			return j;
		}
		_GetItem(haystack, ++j, item);
	}
	return -1;
}

static bool IsBool(LPCSTR B)
{
	return (xr_strcmp(B,"1")==0 || xr_strcmp(B,"true")==0 || xr_strcmp(B,"on")==0 || xr_strcmp(B,"yes")==0);
}

static void LoadAmmoDefn(CUIXml* pXML, XML_NODE* itemNode, SSpecificCharacterSupplies::Item::AmmoDef& defn, LPCSTR attrLeft, LPCSTR attrSect, LPCSTR attrIdx, LPCSTR attrSpawn, LPCSTR sect, LPCSTR opt)
{
	defn.rounds = pXML->ReadAttribInt(itemNode, attrLeft, -1);
	defn.section = pXML->ReadAttrib(itemNode, attrSect, "");
	defn.idx = pXML->ReadAttribInt(itemNode, attrIdx, 0);
	LPCSTR ammoClass = READ_IF_EXISTS(pSettings, r_string, sect, opt, "");
	if (ammoClass && ammoClass[0])
	{
		if (defn.section.length() > 1 && defn.idx == 0)
		{
			// detect ammo idx by ammo section
			defn.idx = ItemIndexOf(ammoClass, defn.section.c_str());
			if (defn.idx == -1)
			{
				defn.idx = 0;
			}
		}
		else
		{
			// detect ammo section by ammo idx
			string128 buf;
			defn.section = _GetItem(ammoClass, defn.idx, buf);
			if (defn.section.empty())
			{
				defn.section = _GetItem(ammoClass, 0, buf);
			}
		}
	}
	defn.spawnCount = pXML->ReadAttribInt(itemNode, attrSpawn, 0);
}

void SSpecificCharacterSupplies::LoadSupplies(CUIXml* pXML, XML_NODE* supplies, ITEMS& items, TEMPLATES& templates)
{
	int itemsNum = pXML->GetNodesNum(supplies, "item");
	for (int i = 0; i < itemsNum; i++)
	{
		XML_NODE* itemNode = pXML->NavigateToNode(supplies, "item", i);
		Item item;
		item.section = pXML->ReadAttrib(itemNode, "s", "");
		if (item.section.size() == 0) item.section = pXML->Read(itemNode, "");
		if (item.section.size() == 0) continue;

		item.count = pXML->ReadAttribInt(itemNode, "qty", 1);
		item.probability = pXML->ReadAttribFlt(itemNode, "prob", 1.f);
		item.condition = pXML->ReadAttribFlt(itemNode, "cond", 1.f);
		item.hasScope = IsBool(pXML->ReadAttrib(itemNode, "scope", ""));
		item.hasLauncher = IsBool(pXML->ReadAttrib(itemNode, "launcher", ""));
		item.hasSilencer = IsBool(pXML->ReadAttrib(itemNode, "silencer", ""));
		LoadAmmoDefn(pXML, itemNode, item.ammo,    "ammo_left", "ammo_s", "ammo_idx", "w_ammo", item.section.c_str(), "ammo_class");
		LoadAmmoDefn(pXML, itemNode, item.grenade, "gren_left", "gren_s", "gren_idx", "w_gren", item.section.c_str(), "grenade_class");
		// int upgrNum = pXML->GetNodesNum(itemNode, "upgrade");
		LPCSTR upgrades = pXML->ReadAttrib(itemNode, "upgrades", "");
		int upgrNum = _GetItemCount(upgrades);
		for (int j = 0; j < upgrNum; j++)
		{
			// shared_str upgr = pXML->ReadAttrib(itemNode, "upgrade", j, "s", "");
			string256 upgr;
			_GetItem(upgrades, j, upgr);
			if (xr_strlen(upgr) > 0)
			{
				item.upgrades.push_back(upgr);
			}
		}
		items.push_back(item);
	}

	itemsNum = pXML->GetNodesNum(supplies, "include");
	for (int i = 0; i < itemsNum; i++)
	{
		XML_NODE* itemNode = pXML->NavigateToNode(supplies, "include", i);
		TemplateRef tmplRef;
		tmplRef.name = pXML->ReadAttrib(itemNode, "id", "");
		if (tmplRef.name.size() == 0)
		{
			tmplRef.name = pXML->Read(itemNode, "");
		}
		if (tmplRef.name.size() > 0)
		{
			tmplRef.count = pXML->ReadAttribInt(itemNode, "qty", 1);
			tmplRef.probability = pXML->ReadAttribFlt(itemNode, "prob", 1.f);
			templates.push_back(tmplRef);
		}
	}
}

void SSpecificCharacterSupplies::LoadSupplies(CUIXml* pXML, XML_NODE* supplies)
{
	LoadSupplies(pXML, supplies, m_items, m_templates);

	int selectNum = pXML->GetNodesNum(supplies, "random_select");
	for (int i = 0; i < selectNum; i++)
	{
		XML_NODE* selectNode = pXML->NavigateToNode(supplies, "random_select", i);
		RandomSelect selectDef;
		selectDef.totalWeight = 0.f;
		selectDef.count = pXML->ReadAttribInt(selectNode, "qty", 1);
		selectDef.probability = pXML->ReadAttribFlt(selectNode, "prob", 1.f);

		int optionNum = pXML->GetNodesNum(selectNode, "opt");
		for (int j = 0; j < optionNum; j++)
		{
			XML_NODE* optionNode = pXML->NavigateToNode(selectNode, "opt", j);
			RandomSelect::Option optionDef;
			optionDef.weight = pXML->ReadAttribFlt(optionNode, "w", 1.f);
			LoadSupplies(pXML, optionNode, optionDef.items, optionDef.templates);

			selectDef.options.push_back(optionDef);
			selectDef.totalWeight += optionDef.weight;
		}
		m_selects.push_back(selectDef);
	}
}

void CSpecificCharacterSupplyTemplate::InitXmlIdToIndex()
{
	if (!id_to_index::tag_name)
		id_to_index::tag_name = "template";
	if (!id_to_index::file_str)
		id_to_index::file_str = pSettings->r_string("profiles", "supplies_template_files");
}

void CSpecificCharacterSupplyTemplate::Load(shared_str id)
{
	R_ASSERT(id.size());
	m_OwnId = id;
	inherited_shared::load_shared(m_OwnId, NULL);
}

void CSpecificCharacterSupplyTemplate::load_shared(LPCSTR)
{
	const ITEM_DATA& item_data = *id_to_index::GetById(m_OwnId);
	CUIXml*		pXML = item_data._xml;
	pXML->SetLocalRoot(pXML->GetRoot());
	XML_NODE* item_node = pXML->NavigateToNode(id_to_index::tag_name, item_data.pos_in_file);
	get_sd()->m_supplies.LoadSupplies(pXML, item_node);
}

const SSpecificCharacterSupplies& CSpecificCharacterSupplyTemplate::Supplies() const
{
	return get_sd()->m_supplies;
}
