#pragma once

class CInventoryItem;
class CWeapon;
class IKinematics;
class IRenderVisual;

struct THudCartridges final
{
private:
	RStringVec m_bone_names{};
	shared_str m_cartridge_visual;
	shared_str m_shell_visual;
	xr_hash_map<u8, shared_str> m_cartridge_type_visuals{};
	xr_hash_map<u8, shared_str> m_shell_type_visuals{};

	xr_vector<u16> m_bones{};

	struct SSlot
	{
		Fmatrix Transform = Fidentity;
		xr_hash_map<shared_str, IRenderVisual*> Visuals{};
	};
	xr_vector<xr_vector<SSlot>> m_slots{};

	struct SState
	{
		u32 Count = 0;
		u8 Type = 0;
	} m_states[2]{};

	const shared_str& Visual(u8 Type, bool Shell) const;
	static void Apply(CInventoryItem& Item, u32 Count, u8 Type, bool Mirror);

public:
	void BeginComponent(IECSOwner* O);
	void EndComponent();
	void Render(const Fmatrix& XForm, IKinematics* Model, u32 Mirror = 0);
	void Update(u32 Count, u8 Type, bool Mirror = false);
	static void Update(CWeapon* Weapon, u32 Count, u8 Type, bool Mirror = false);

private:
	ECS_COMPONENT(THudCartridges)
	ECS_END
};
