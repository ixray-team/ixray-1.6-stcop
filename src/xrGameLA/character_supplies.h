#pragma once

#include "shared_data.h"
#include "xml_str_id_loader.h"

//////////////////////////////////////////////////////////////////////////
// Supply definition for specific characters
//////////////////////////////////////////////////////////////////////////
struct SSpecificCharacterSupplies
{
	// Item spawn definition
	struct Item
	{
		struct AmmoDef
		{
			// number of rounds in weapon magazine, -1 for default amount
			int rounds;
			// section of ammo in weapon magazine
			xr_string section;
			// index of ammo section in ammo_class or grenade_class lists
			int idx;
			// number of ammo packs to spawn additionally (of type specified by "section" or "idx")
			int spawnCount;
		};

		// item section to spawn
		shared_str section;
		// number of items to spawn
		int count;
		// probability to spawn for every item in count
		float probability;
		// item condition when spawned
		float condition;
		// if weapon has launcher attached
		bool hasLauncher;
		// if weapon has scope attached
		bool hasScope;
		// if weapon has silencer attached
		bool hasSilencer;
		// a list if pre-installed upgrades for weapon/outfit
		xr_vector<shared_str> upgrades;

		AmmoDef ammo, grenade;
	};

	// Template reference to spawn pre-defined template of items
	struct TemplateRef
	{
		// template name
		shared_str name;
		// how many times to spawn
		int count;
		// probability to spawn
		float probability;
	};
	typedef xr_vector<Item>		ITEMS;
	typedef xr_vector<TemplateRef>	TEMPLATES;

	// Allows to randomly spawn only one set of items from several options
	struct RandomSelect
	{
		struct Option
		{
			// the weight of this case
			float weight;
			ITEMS items;
			TEMPLATES templates;
		};
		// A sum of all case weights, for later processing
		float totalWeight;
		xr_vector<Option> options;
		// How many times to spawn
		int count;
		// Probability to spawn anything at all
		float probability;
	};
	typedef xr_vector<RandomSelect>	SELECTS;

	ITEMS m_items;
	TEMPLATES m_templates;
	SELECTS m_selects;

	void LoadSupplies(CUIXml* pXML, XML_NODE* supplies);
	void LoadSupplies(CUIXml* pXML, XML_NODE* supplies, ITEMS& items, TEMPLATES& templates);
};


struct SSpecificCharacterSupplyTemplateData : CSharedResource
{
	SSpecificCharacterSupplies m_supplies;
};


// Supply template for specific characters
class CSpecificCharacterSupplyTemplate : public CSharedClass<SSpecificCharacterSupplyTemplateData, shared_str, false>,
										 public CXML_IdToIndex<CSpecificCharacterSupplyTemplate>
{
	typedef CSharedClass	<SSpecificCharacterSupplyTemplateData, shared_str, false>	inherited_shared;
	typedef CXML_IdToIndex	<CSpecificCharacterSupplyTemplate>							id_to_index;
	typedef xr_vector<SSpecificCharacterSupplies::Item>									SUPPLY_ITEMS;
	typedef xr_vector<SSpecificCharacterSupplies::Item>									SUPPLY_TEMPLATES;
	friend id_to_index;

	shared_str		m_OwnId;

protected:
	//загрузка из XML файла
	virtual void				load_shared(LPCSTR);
	static void					InitXmlIdToIndex();

public:
	// load this object with data
	virtual void				Load(shared_str	id);
	const SSpecificCharacterSupplies& Supplies() const;
};

