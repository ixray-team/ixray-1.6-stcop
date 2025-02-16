////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_item.h
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Victor Reutsky, Yuri Dobronravin
//	Description : Inventory item
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "inventory_space.h"
#include "hit_immunity.h"
#include "attachable_item.h"
#include "xrserver_objects_alife.h"
#include "xrserver_objects_alife_items.h"
#include "PhysicsShellHolder.h"

enum EHandDependence{
	hdNone	= 0,
	hd1Hand	= 1,
	hd2Hand	= 2
};

class CSE_Abstract;
class CGameObject;
class CFoodItem;
class CMissile;
class CHudItem;
class CBattery;
class CWeaponAmmo;
class CWeapon;
//class CPhysicsShellHolder;
class NET_Packet;
class CEatableItem;
struct SPHNetState;
struct net_update_IItem;
struct net_updateData;
class CInventoryOwner;

struct SHit;

class CInventoryItem : 
	public CAttachableItem,
	public CHitImmunity
#ifdef DEBUG
	, public pureRender
#endif
{
private:
	typedef CAttachableItem inherited;
protected:
	enum EIIFlags{				FdropManual			=(1<<0),
								FCanTake			=(1<<1),
								FCanTrade			=(1<<2),
								Fbelt				=(1<<3),
								Fruck				=(1<<4),
								FRuckDefault		=(1<<5),
								FUsingCondition		=(1<<6),
								FAllowSprint		=(1<<7),
								Fuseful_for_NPC		=(1<<8),
								FInInterpolation	=(1<<9),
								FInInterpolate		=(1<<10),
								FIsQuestItem		=(1<<11),
								FdeleteManual		=(1<<12),
	};

	Flags16						m_flags;
public:
								CInventoryItem		();
	virtual						~CInventoryItem		();

public:
	virtual void				Load				(LPCSTR section);

	virtual LPCSTR				Name				();
	virtual LPCSTR				NameShort			();
//.	virtual LPCSTR				NameComplex			();
	shared_str					ItemDescription		() { return m_Description; }
	virtual void				GetBriefInfo		(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count) {};
	virtual LPCSTR				GetCurrentFireModeStr		()	{return " ";};
	
	virtual void				OnEvent				(NET_Packet& P, u16 type);
	
	virtual bool				Useful				() const;									// !!! Переопределить. (см. в Inventory.cpp)
	virtual bool				Attach				(PIItem pIItem, bool b_send_event) {return false;}
	virtual bool				Detach				(PIItem pIItem) {return false;}
	//при детаче спаунится новая вещь при заданно названии секции
	virtual bool				Detach				(const char* item_section_name, bool b_spawn_item);
	virtual bool				CanAttach			(PIItem pIItem) {return false;}
	virtual bool				CanDetach			(LPCSTR item_section_name) {return false;}

	virtual EHandDependence		HandDependence		()	const	{return hd1Hand;};
	virtual bool				IsSingleHanded		()	const	{return true;};	
	virtual bool				Action				(u16 cmd, u32 flags) {return false;}	// true если известная команда, иначе false

	virtual void				OnH_B_Chield		();
	virtual void				OnH_A_Chield		();
    virtual void				OnH_B_Independent	(bool just_before_destroy);
	virtual void				OnH_A_Independent	();

			u16					object_id			() const						{return object().ID();}

	virtual void				render_item_ui		()								{}; //when in slot & query return TRUE
	virtual bool				render_item_ui_query()								{return false;}; //when in slot

	virtual void				UpdateCL			();

	virtual	void				Hit					(SHit* pHDS);

			BOOL				GetDropManual		() const	{ return m_flags.test(FdropManual);}
			void				SetDropManual		(BOOL val)	{ m_flags.set(FdropManual, val);}
			BOOL				GetDeleteManual		() const	{ return m_flags.test(FdeleteManual);}
			void				SetDeleteManual		(BOOL val)	{ m_flags.set(FdeleteManual, val); m_flags.set(FdropManual, val);}

			BOOL				IsInvalid			() const;

			BOOL				IsQuestItem			()	const	{return m_flags.test(FIsQuestItem);}			
	virtual u32					Cost				() const	{ return m_cost; }
	virtual float				Weight				() const	{ return m_weight;}		

public:
	CInventory*					m_pCurrentInventory;

	shared_str					m_section_id;
	shared_str					m_name;
	shared_str					m_nameShort;
	shared_str					m_nameComplex;

	EItemPlace					m_eItemPlace;

	virtual void				OnMoveToSlot		() {};
	virtual void				OnMoveToBelt		() {};
	virtual void				OnMoveToRuck		() {};
					
			Irect				GetInvGridRect		() const;
			int					GetGridWidth		() const ;
			Irect				GetUpgrIconRect		() const;
			LPCSTR				GetUpgrIconName		() const;
			int					GetGridHeight		() const ;
			const shared_str&	GetIconName			() const		{return m_icon_name;};
			int					GetXPos				() const ;
			int					GetYPos				() const ;
	//---------------------------------------------------------------------
			float				GetKillMsgXPos		() const ;
			float				GetKillMsgYPos		() const ;
			float				GetKillMsgWidth		() const ;
			float				GetKillMsgHeight	() const ;
	//---------------------------------------------------------------------
			float				GetCondition		() const					{return condition_;}
	virtual	float				GetConditionToShow	() const					{return GetCondition();}
			void				ChangeCondition		(float fDeltaCondition);
			void				SetCondition		(float fCondition);
			void				ResetCondition		();

	virtual TSlotId				BaseSlot			()  const					{return m_baseSlot;}
			TSlotId				CurrSlot			()  const					{return m_currentSlot;}
			EItemPlace			CurrPlace			()  const					{return m_eItemPlace;}
			
			void				SetCurrPlace		(EItemPlace place);
			void				SetCurrSlot			(TSlotId slot);

			bool				Belt				()							{return !!m_flags.test(Fbelt);}
			void				Belt				(bool on_belt)				{m_flags.set(Fbelt,on_belt);}
			bool				Ruck				()							{return !!m_flags.test(Fruck);}
			void				Ruck				(bool on_ruck)				{m_flags.set(Fruck,on_ruck);}
			bool				RuckDefault			()							{return !!m_flags.test(FRuckDefault);}
			
	virtual bool				CanTake				() const					{return !!m_flags.test(FCanTake);}
			bool				CanTrade			() const;
	virtual bool 				IsNecessaryItem	    (CInventoryItem* item);
	virtual bool				IsNecessaryItem	    (const shared_str& item_sect){return false;};
			bool				IsSingleHand()	const		{ return m_bSingleHand; };

			shared_str			m_SectNightVision;//tatarinrafa: добавил считывание строки ПНВ
			u32					m_attach_to_owner;
protected:
	
	TSlotId						m_currentSlot;
	TSlotId						m_baseSlot;
	u32							m_cost;
	float						m_weight;
	float						condition_;
	shared_str					m_Description;

	ALife::_TIME_ID				m_dwItemRemoveTime;
	ALife::_TIME_ID				m_dwItemIndependencyTime;

	float						m_fControlInertionFactor;
	shared_str					m_icon_name;
	bool						m_bPickUpVisible;
	bool						m_bSingleHand;
	bool						m_bUniqueMarked;
	bool						m_bTradeIgnoreCondition;
	float						m_fConditionCostKoef;
	float						m_fConditionCostCurve;

public:
	virtual void				make_Interpolation	();
	virtual void				PH_B_CrPr			(); // actions & operations before physic correction-prediction steps
	virtual void				PH_I_CrPr			(); // actions & operations after correction before prediction steps
#ifdef DEBUG
	virtual void				PH_Ch_CrPr			(); // 
#endif
	virtual void				PH_A_CrPr			(); // actions & operations after phisic correction-prediction steps

	// Happens when object gets online. Import saved data from server here. Do oject initialization based on data imported from server
	virtual BOOL				net_Spawn(CSE_Abstract* DC);
	virtual void				net_Destroy();

	// import variables data from server (not called in single player)
	virtual void				net_Import				(NET_Packet& P);
	// export variables data to server (refer to CSE_ALifeItemWeapon::UPDATE_Write)
	virtual void				net_Export				(NET_Packet& P);

	//data saving if online. Saveed stuff only needed for online presence
	virtual void				save(NET_Packet &output_packet);
	//data loading if online
	virtual void				load(IReader &input_packet);


	virtual BOOL				net_SaveRelevant()							{ return TRUE; }

	virtual void				activate_physic_shell		();
	virtual u16					bone_count_to_synchronize	() const;

	virtual bool				NeedToDestroyObject			() const;
	virtual ALife::_TIME_ID		TimePassedAfterIndependant	() const;

	virtual	bool				IsSprintAllowed				() const		{return !!m_flags.test(FAllowSprint);} ;
	virtual	bool				IsPickUpVisible				() const		{return m_bPickUpVisible;} ;
	virtual	bool				IsUniqueMarked				() const		{return m_bUniqueMarked;} ;
			bool				TradeIgnoreCondition		() const		{return m_bTradeIgnoreCondition;}
			float				ConditionCostKoef			() const		{return m_fConditionCostKoef;}
			float				ConditionCostCurve			() const		{return m_fConditionCostCurve;}

	virtual	float				GetControlInertionFactor(	) const			{return m_fControlInertionFactor;};

protected:
	virtual void				UpdateXForm	();
			
protected:
	net_updateData*				m_net_updateData;
	net_updateData*				NetSync						();
	void						CalculateInterpolationParams();

public:
	virtual void				reload					(LPCSTR section);
	virtual void				reinit					();
	virtual bool				can_kill				() const;
	virtual CInventoryItem*		can_kill				(CInventory *inventory) const;
	virtual const CInventoryItem*can_kill				(const xr_vector<const CGameObject*> &items) const;
	virtual CInventoryItem*		can_make_killing		(const CInventory *inventory) const;
	virtual bool				ready_to_kill			() const;
	IC		bool				useful_for_NPC			() const;
#ifdef DEBUG
	virtual void				OnRender					();
#endif

public:
	virtual DLL_Pure*			_construct					();
	IC	CPhysicsShellHolder&	object						() const{ VERIFY		(m_object); return		(*m_object);}
	virtual void				on_activate_physic_shell	() { R_ASSERT2(0, "failed call of virtual function!"); }
	
protected:
	float						m_holder_range_modifier;
	float						m_holder_fov_modifier;

public:
	virtual	void				modify_holder_params		(float &range, float &fov) const;

protected:
	IC	CInventoryOwner&		inventory_owner				() const;

private:
	CPhysicsShellHolder*		m_object;

public:
	virtual CInventoryItem		*cast_inventory_item		()	{return this;}
	virtual CAttachableItem		*cast_attachable_item		()	{return this;}
	virtual CPhysicsShellHolder	*cast_physics_shell_holder	()	{return 0;}
	virtual CEatableItem		*cast_eatable_item			()	{return 0;}
	virtual CWeapon				*cast_weapon				()	{return 0;}
	virtual CFoodItem			*cast_food_item				()	{return 0;}
	virtual CMissile			*cast_missile				()	{return 0;}
	virtual CHudItem			*cast_hud_item				()	{return 0;}
	virtual CWeaponAmmo			*cast_weapon_ammo			()	{return 0;}
	virtual CGameObject			*cast_game_object			()  {return 0;}

	////////// upgrades //////////////////////////////////////////////////
public:
	typedef xr_vector<shared_str>	Upgrades_type;

protected:
	Upgrades_type	m_upgrades;

public:
	IC bool	has_any_upgrades			() { return (m_upgrades.size() != 0); }
	bool	has_upgrade					( const shared_str& upgrade_id );
	bool	has_upgrade_group			( const shared_str& upgrade_group_id );
	void	add_upgrade					( const shared_str& upgrade_id, bool loading );
	bool	get_upgrades_str			( string2048& res ) const;

	bool	equal_upgrades				( Upgrades_type const& other_upgrades ) const;

	bool	verify_upgrade				( LPCSTR section );
	bool	install_upgrade				( LPCSTR section );
	void	pre_install_upgrade			();

#ifdef DEBUG	
	void	log_upgrades				();
#endif // DEBUG

	IC Upgrades_type const& upgrades	() const;
	virtual void	Interpolate			();
	float	interpolate_states			(net_update_IItem const & first, net_update_IItem const & last, SPHNetState & current);

protected:
	virtual	void	net_Spawn_install_upgrades	( Upgrades_type saved_upgrades );
	virtual bool	install_upgrade_impl		( LPCSTR section, bool test );

	template <typename T>
	IC		bool	process_if_exists			( LPCSTR section, LPCSTR name, T (CInifile::*method)(LPCSTR, LPCSTR)const, T& value, bool test );
	template <>
			bool	process_if_exists			( LPCSTR section, LPCSTR name, float (CInifile::*method)(LPCSTR, LPCSTR)const, float& value, bool test );
			bool	process_if_exists			( LPCSTR section, LPCSTR name, float& value, bool test, float(*getter)(float), float(*multiply)(float,float) );

	template <typename TRet, typename TVal>
	IC		bool	process_if_exists_set		( LPCSTR section, LPCSTR name, TRet (CInifile::*method)(LPCSTR, LPCSTR)const, TVal& value, bool test );
	bool								m_just_after_spawn;
	bool								m_activated;
};

#include "inventory_item_inline.h"