////////////////////////////////////////////////////////////////////////////
//	Module 		: xrServer_Object_Base.h
//	Created 	: 19.09.2002
//  Modified 	: 16.07.2004
//	Author		: Oles Shyshkovtsov, Alexander Maksimchuk, Victor Reutskiy and Dmitriy Iassenev
//	Description : Server base object
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "xrServer_Objects_Abstract.h"
#include "object_interfaces.h"
#include "script_value_container.h"
#include "alife_space.h"
#include "../xrCore/client_id.h"

enum class SpawnFileChunks : u8
{
	// Original
	Header = 0,
	SpawnGraphOld = 1,
	LevelPoints = 2,
	PatrolPathStorage = 3,
	GameGraph = 4,
	// New serialization system
	SpawnGraphNew = 5
};

class NET_Packet;
class xrClientData;
class CSE_ALifeGroupAbstract;
class CSE_ALifeSchedulable;
class CSE_ALifeInventoryItem;
class CSE_ALifeTraderAbstract;
class CSE_ALifeObject;
class CSE_ALifeDynamicObject;
class CSE_ALifeItemAmmo;
class CSE_ALifeItemWeapon;
class CSE_ALifeItemDetector;
class CSE_ALifeMonsterAbstract;
class CSE_ALifeHumanAbstract;
class CSE_ALifeAnomalousZone;
class CSE_ALifeTrader;
class CSE_ALifeCreatureAbstract;
class CSE_ALifeSmartZone;
class CSE_ALifeOnlineOfflineGroup;
class CSE_ALifeItemPDA;

#pragma warning(push)
#pragma warning(disable:4005)

SERVER_ENTITY_DECLARE_BEGIN(CPureServerObject,IPureServerObject)
	virtual							~CPureServerObject() = default;
	virtual void					load(IReader	&tFileStream);
	virtual void					save(IWriter	&tMemoryStream);
	virtual void					load(NET_Packet	&tNetPacket);
	virtual void					save(NET_Packet	&tNetPacket);
};

SERVER_ENTITY_DECLARE_BEGIN3(CSE_Abstract,ISE_Abstract,CPureServerObject,CScriptValueContainer)
public:
	enum ESpawnFlags {
		flSpawnEnabled				= u32(1 << 0),
		flSpawnOnSurgeOnly			= u32(1 << 1),
		flSpawnSingleItemOnly		= u32(1 << 2),
		flSpawnIfDestroyedOnly		= u32(1 << 3),
		flSpawnInfiniteCount		= u32(1 << 4),
		flSpawnDestroyOnSpawn		= u32(1 << 5),
	};

private:
	LPSTR							s_name_replace;

public:
	bool							net_Ready;
	bool							net_Processed;	// Internal flag for connectivity-graph
	
	u16								m_wVersion;
	u16								m_script_version;
	u16								RespawnTime;

	ALife::_OBJECT_ID				ID;				// internal ID
	ALife::_OBJECT_ID				ID_Parent;		// internal ParentID, ALife::INVALID_OBJECT_ID means no parent
	ALife::_OBJECT_ID				ID_Phantom;		// internal PhantomID, ALife::INVALID_OBJECT_ID means no phantom
	xrClientData*					owner;

	// spawn data
	shared_str						s_name;
//.	u8								s_gameid;
	GameTypeChooser					m_gameType;
	u8								s_RP;
	Flags16							s_flags;		// state flags
	xr_vector<ALife::_OBJECT_ID>	children;

	// update data
	Fvector							o_Position;
	Fvector							o_Angle;
	CLASS_ID						m_tClassID;
	int								m_script_clsid;
	shared_str						m_ini_string;
	CInifile						*m_ini_file;

	// for ALife control
	bool							m_bALifeControl;
	ALife::_SPAWN_ID				m_tSpawnID;

	// ALife spawn params
	// obsolete, just because we hope to uncomment all this stuff
	Flags32							m_spawn_flags;

	//client object custom data serialization
	xr_vector<u8>					client_data;
	CSaveChunk* client_data_new = nullptr;
	virtual void					load					(NET_Packet	&tNetPacket);

	//////////////////////////////////////////////////////////////////////////
	
									CSE_Abstract			(const char* caSection);
	virtual							~CSE_Abstract			();
	virtual void					OnEvent					(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender ){};
#if !defined(XRGAME_EXPORTS)
	virtual void					FillProps				(const char* pref, PropItemVec &items);
	virtual void					FillProp				(const char* pref, PropItemVec &items);
#if !defined(AI_COMPILER)
	virtual void 			on_render				(CDUInterface* du, ISE_AbstractLEOwner* owner_, bool bSelected, const Fmatrix& parent,int priority, bool strictB2F){} 
	virtual	visual_data*	visual_collection		() const { return 0; }
	virtual	u32				visual_collection_size	() const { return 0; }
	virtual	void			set_additional_info		(void* info) {};
#endif
#endif
	virtual bool					Net_Relevant			(){return false;}; // !!!! WARNING!!!
	//
	virtual void			Spawn_Write				(NET_Packet &tNetPacket, bool bLocal);
	virtual bool			Spawn_Read				(NET_Packet &tNetPacket);
	virtual bool Spawn_Serialize(ISaveObject& Object, bool bLocal = true, bool Copying = false);
	virtual const char*			name					() const override;
	virtual const char*			name_replace			() const override;
	virtual void			set_name				(const char* s) override
	{
		s_name		= s;
	};
	virtual void			set_name_replace		(const char* s) override {xr_free(s_name_replace); s_name_replace = xr_strdup(s);};
	virtual Fvector&		position				();
	virtual Fvector&		angle					();
	virtual Flags16&		flags					();
	virtual CSE_Visual* 	visual					();
	virtual ISE_Shape*  	shape					();
	virtual CSE_Motion* 	motion					();
	virtual bool			validate				();
	//

	IC		const Fvector			&Position				() const					{return o_Position;};
	// we need this to prevent virtual inheritance :-(
	virtual CSE_Abstract			*base					();
	virtual const CSE_Abstract		*base					() const;
	virtual CSE_Abstract			*init					();
	virtual bool					match_configuration		() const {return true;}
	// end of the virtual inheritance dependant code
	IC		int						script_clsid			() const					{VERIFY(m_script_clsid >= 0); return (m_script_clsid);}
			CInifile				&spawn_ini				();

// for smart cast
	virtual CSE_ALifeGroupAbstract		*cast_group_abstract		() {return nullptr;}
	virtual CSE_ALifeSchedulable		*cast_schedulable			() {return nullptr;}
	virtual CSE_ALifeInventoryItem		*cast_inventory_item		() {return nullptr;}
	virtual CSE_ALifeTraderAbstract		*cast_trader_abstract		() {return nullptr;}

	virtual CSE_ALifeObject				*cast_alife_object			() {return nullptr;}
	virtual CSE_ALifeDynamicObject		*cast_alife_dynamic_object	() {return nullptr;}
	virtual CSE_ALifeItemAmmo			*cast_item_ammo				() {return nullptr;}
	virtual CSE_ALifeItemWeapon			*cast_item_weapon			() {return nullptr;}
	virtual CSE_ALifeItemDetector		*cast_item_detector			() {return nullptr;}
	virtual CSE_ALifeMonsterAbstract	*cast_monster_abstract		() {return nullptr;}
	virtual CSE_ALifeHumanAbstract		*cast_human_abstract		() {return nullptr;}
	virtual CSE_ALifeAnomalousZone		*cast_anomalous_zone		() {return nullptr;}
	virtual CSE_ALifeTrader				*cast_trader				() {return nullptr;}

	virtual CSE_ALifeCreatureAbstract	*cast_creature_abstract		() {return nullptr;}
	virtual CSE_ALifeSmartZone			*cast_smart_zone			() {return nullptr;}
	virtual CSE_ALifeOnlineOfflineGroup	*cast_online_offline_group	() {return nullptr;}
	virtual CSE_ALifeItemPDA			*cast_item_pda				() {return nullptr;}

	// For new sync system
	virtual void SyncRead(NET_Packet& Packet)  {};
	virtual void SyncWrite(NET_Packet& Packet) {};
};

#pragma warning(pop)
