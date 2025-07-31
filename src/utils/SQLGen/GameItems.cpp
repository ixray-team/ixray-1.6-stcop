#include "api.h"
#include "../../xrServerEntities/clsid_game.h"

#include <iostream>

static CLASS_ID Classes[] =
{
	CLSID_OBJECT_AMMO,
	CLSID_AF_MERCURY_BALL,
	CLSID_AF_GRAVI,
	CLSID_AF_BLACKDROPS,
	CLSID_AF_NEEDLES,
	CLSID_AF_BAST,
	CLSID_AF_BLACK_GRAVI,
	CLSID_AF_DUMMY,
	CLSID_AF_ZUDA,
	CLSID_AF_THORN,
	CLSID_AF_FADED_BALL,
	CLSID_AF_ELECTRIC_BALL,
	CLSID_AF_RUSTY_HAIR,
	CLSID_AF_GALANTINE,
	CLSID_AF_CTA,
	CLSID_ARTEFACT,
	CLSID_OBJECT_W_M134,
	CLSID_OBJECT_W_FN2000,
	CLSID_OBJECT_W_AK74,
	CLSID_OBJECT_W_LR300,
	CLSID_OBJECT_W_HPSA,
	CLSID_OBJECT_W_PM,
	CLSID_OBJECT_W_FORT,
	CLSID_OBJECT_W_BINOCULAR,
	CLSID_OBJECT_W_SHOTGUN,
	CLSID_OBJECT_W_ASHOTGUN,
	CLSID_OBJECT_W_MAGAZINED,
	CLSID_OBJECT_W_MAGAZWGL,
	CLSID_OBJECT_W_SVD,
	CLSID_OBJECT_W_SVU,
	CLSID_OBJECT_W_RPG7,
	CLSID_OBJECT_W_VAL,
	CLSID_OBJECT_W_VINTOREZ,
	CLSID_OBJECT_W_WALTHER,
	CLSID_OBJECT_W_USP45,
	CLSID_OBJECT_W_GROZA,
	CLSID_OBJECT_W_KNIFE,
	CLSID_OBJECT_W_BM16,
	CLSID_OBJECT_W_RG6,
	CLSID_OBJECT_W_STATMGUN,
	CLSID_OBJECT_AMMO,
	CLSID_OBJECT_A_VOG25,
	CLSID_OBJECT_A_OG7B,
	CLSID_OBJECT_A_M209,
	CLSID_OBJECT_W_SCOPE,
	CLSID_OBJECT_W_SILENCER,
	CLSID_OBJECT_W_GLAUNCHER,
	CLSID_PH_SKELETON_OBJECT,
	CLSID_OBJECT_PHYSIC       ,
	CLSID_PHYSICS_DESTROYABLE ,
	CLSID_INVENTORY_BOX       ,
	CLSID_DETECTOR_SIMPLE     ,
	CLSID_DETECTOR_ADVANCED   ,
	CLSID_DETECTOR_ELITE      ,
	CLSID_DETECTOR_SCIENTIFIC ,
	CLSID_DEVICE_PDA          ,
	CLSID_DEVICE_TORCH        ,
	CLSID_DEVICE_COMPASS      ,
	CLSID_DEVICE_AF_MERGER    ,
	CLSID_DEVICE_FLARE        ,
	CLSID_IITEM_BOLT          ,
	CLSID_IITEM_MEDKIT        ,
	CLSID_IITEM_BANDAGE       ,
	CLSID_IITEM_FOOD          ,
	CLSID_IITEM_BOTTLE        ,
	CLSID_IITEM_ANTIRAD       ,
	CLSID_IITEM_EXPLOSIVE     ,
	CLSID_IITEM_DOCUMENT      ,
	CLSID_IITEM_ATTACH        ,
	CLSID_GRENADE_F1          ,
	CLSID_OBJECT_G_RPG7       ,
	CLSID_GRENADE_RGD5        ,
	CLSID_OBJECT_G_FAKE       ,
	CLSID_OBJECT_PLAYERS_BAG  ,
	CLSID_EQUIPMENT_SIMPLE    ,
	CLSID_EQUIPMENT_SCIENTIFIC,
	CLSID_EQUIPMENT_STALKER,
	CLSID_EQUIPMENT_MILITARY,
	CLSID_EQUIPMENT_EXO,
	CLSID_EQUIPMENT_HELMET,
};

xr_vector<shared_str> ParseGameItems(CInifile* File)
{
	xr_vector<CLASS_ID> ScriptClasses;

	ScriptClasses.push_back(TEXT2CLSID("WP_BINOC"));
	ScriptClasses.push_back(TEXT2CLSID("WP_KNIFE"));
	ScriptClasses.push_back(TEXT2CLSID("WP_BM16"));
	ScriptClasses.push_back(TEXT2CLSID("WP_GROZA"));
	ScriptClasses.push_back(TEXT2CLSID("WP_SVD"));
	ScriptClasses.push_back(TEXT2CLSID("WP_AK74"));
	ScriptClasses.push_back(TEXT2CLSID("WP_LR300"));
	ScriptClasses.push_back(TEXT2CLSID("WP_HPSA"));
	ScriptClasses.push_back(TEXT2CLSID("WP_PM"));
	ScriptClasses.push_back(TEXT2CLSID("WP_RG6"));
	ScriptClasses.push_back(TEXT2CLSID("WP_RPG7"));
	ScriptClasses.push_back(TEXT2CLSID("WP_SHOTG"));
	ScriptClasses.push_back(TEXT2CLSID("WP_ASHTG"));
	ScriptClasses.push_back(TEXT2CLSID("WP_SVU"));
	ScriptClasses.push_back(TEXT2CLSID("WP_USP45"));
	ScriptClasses.push_back(TEXT2CLSID("WP_VAL"));
	ScriptClasses.push_back(TEXT2CLSID("WP_VINT"));
	ScriptClasses.push_back(TEXT2CLSID("WP_WALTH"));
	ScriptClasses.push_back(TEXT2CLSID("WP_MAGAZ"));
	ScriptClasses.push_back(TEXT2CLSID("W_STMGUN"));

	ScriptClasses.push_back(TEXT2CLSID("AMMO_S"));
	ScriptClasses.push_back(TEXT2CLSID("S_VOG25"));
	ScriptClasses.push_back(TEXT2CLSID("S_OG7B"));
	ScriptClasses.push_back(TEXT2CLSID("S_M209"));
	ScriptClasses.push_back(TEXT2CLSID("G_F1_S"));
	ScriptClasses.push_back(TEXT2CLSID("G_RGD5_S"));

	ScriptClasses.push_back(TEXT2CLSID("WP_SCOPE"));
	ScriptClasses.push_back(TEXT2CLSID("WP_SILEN"));
	ScriptClasses.push_back(TEXT2CLSID("WP_GLAUN"));

	ScriptClasses.push_back(TEXT2CLSID("TORCH_S"));
	ScriptClasses.push_back(TEXT2CLSID("DET_SCIE"));
	ScriptClasses.push_back(TEXT2CLSID("DET_ELIT"));
	ScriptClasses.push_back(TEXT2CLSID("DET_ADVA"));
	ScriptClasses.push_back(TEXT2CLSID("DET_SIMP"));
	ScriptClasses.push_back(TEXT2CLSID("S_PDA"));
	ScriptClasses.push_back(TEXT2CLSID("D_PDA"));
	ScriptClasses.push_back(TEXT2CLSID("II_ATTCH"));
	ScriptClasses.push_back(TEXT2CLSID("S_MEDKI"));
	ScriptClasses.push_back(TEXT2CLSID("S_BANDG"));
	ScriptClasses.push_back(TEXT2CLSID("S_ANTIR"));
	ScriptClasses.push_back(TEXT2CLSID("S_FOOD"));
	ScriptClasses.push_back(TEXT2CLSID("S_BOTTL"));
	ScriptClasses.push_back(TEXT2CLSID("SCRPTART"));
	ScriptClasses.push_back(TEXT2CLSID("ARTEFACT"));
	ScriptClasses.push_back(TEXT2CLSID("E_STLK"));
	ScriptClasses.push_back(TEXT2CLSID("E_HLMET"));

	xr_vector<shared_str> Trash;

	for (auto Sect : File->sections())
	{
		if (!Sect->line_exist("class"))
			continue;

		CLASS_ID ClassID = File->r_clsid(Sect->Name, "class");

		auto Iter = std::find(std::begin(Classes), std::end(Classes), ClassID);
		auto IterScript = std::find(std::begin(ScriptClasses), std::end(ScriptClasses), ClassID);
		if (Iter != std::end(Classes))
		{
			Trash.emplace_back(Sect->Name);
		}
		else if (IterScript != std::end(ScriptClasses))
		{
			Trash.emplace_back(Sect->Name);
		}
		else
		{
			Msg("Not found class from section: %s", *Sect->Name);
		}
	}

	return std::move(Trash);
}