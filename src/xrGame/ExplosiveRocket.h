//////////////////////////////////////////////////////////////////////
// ExplosiveRocket.h:	ракета, которой стреляет RocketLauncher 
//						взрывается при столкновении
//////////////////////////////////////////////////////////////////////

#pragma once

#include "CustomRocket.h"
#include "Explosive.h"
#include "inventory_item.h"

class CExplosiveRocket :
	public CCustomRocket,
	public CInventoryItem,
	public CExplosive
{
private:
	using inherited = CCustomRocket;
	friend CRocketLauncher;
public:
	CExplosiveRocket() = default;
	virtual ~CExplosiveRocket() = default;
	virtual DLL_Pure* _construct();

public:
	virtual CExplosive* cast_explosive() override { return this; }
	virtual CExplosiveRocket* cast_explosive_rocket() override { return this; }
	virtual CInventoryItem* cast_inventory_item() override { return this; }
	virtual CAttachableItem* cast_attachable_item() override { return this; }
	virtual CGameObject* cast_game_object() override { return this; }
	virtual IDamageSource* cast_IDamageSource() override { return CExplosive::cast_IDamageSource(); }
	virtual CPhysicItem* cast_physics_item() override { return this; }
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual CCustomRocket* cast_custom_rocket() override { return this; }

	virtual void on_activate_physic_shell() override;

public:

	virtual void Load(const char* section) override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual	void net_Relcase(CObject* O) override;
	virtual void OnH_A_Independent() override;
	virtual void OnH_B_Independent(bool just_before_destroy) override;
	virtual void UpdateCL() override;

	virtual void Contact(const Fvector& pos, const Fvector& normal) override;

	virtual void OnEvent(NET_Packet& P, u16 type) override;

	virtual	void Hit(SHit* pHDS) override
	{
		inherited::Hit(pHDS);
	};

public:
	virtual bool UsedAI_Locations() override { return inherited::UsedAI_Locations(); }
	virtual void net_Import(NET_Packet& P) override { inherited::net_Import(P); }
	virtual void net_Export(NET_Packet& P) override { inherited::net_Export(P); }

	virtual void save(NET_Packet& output_packet) override { inherited::save(output_packet); }
	virtual void load(IReader& input_packet) override { inherited::load(input_packet); }
	virtual void Serialize(ISaveObject& Object) override { inherited::Serialize(Object); }
	virtual bool net_SaveRelevant() override { return inherited::net_SaveRelevant(); }

	virtual void OnH_A_Chield() override { inherited::OnH_A_Chield(); }
	virtual void OnH_B_Chield() override { inherited::OnH_B_Chield(); }
	virtual void renderable_Render() override { inherited::renderable_Render(); }
	virtual void make_Interpolation() override;
	virtual void PH_B_CrPr() override; // actions & operations before physic correction-prediction steps
	virtual void PH_I_CrPr() override; // actions & operations after correction before prediction steps
#ifdef DEBUG
	virtual void PH_Ch_CrPr() override;
#endif
#ifdef DEBUG_DRAW
	virtual void OnRender() override;
#endif
	virtual void PH_A_CrPr() override; // actions & operations after phisic correction-prediction steps
	virtual void reinit() override;
	virtual void reload(const char* section) override;
	virtual void activate_physic_shell() override;
	virtual void setup_physic_shell() override;
	virtual void create_physic_shell() override;

public:
	virtual bool Useful() const override;

protected:
	virtual bool use_parent_ai_locations() const override
	{
		return CAttachableItem::use_parent_ai_locations();
	}
};