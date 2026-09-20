#pragma once

#include "GameObject.h"
#include "PhysicsShellHolder.h"

enum EattachmentType : u8
{
	eTypeNone,
	eTypeScope,
	eTypeMuzzle,
	eTypeMount,
	eTypeGLauncher,
	eTypeMagazine,
	eTypeCustom,
	eTypeDecore,
};

enum EattachmentState
{
	eAStateNone = (1 << 0),
	eAStateVisible = (1 << 1),
	eAStatePermanent = (1 << 3),
	eAStateMCombined = (1 << 4),
	eAStateFullyLoaded = (1 << 5),
	eAStateBroken = (1 << 6),
};

class IKinematics;
class CInventoryItem;
struct item_attachment final
{
	struct placement
	{
		Fvector position = Fvector{0, 0, 0};
		Fvector direction = Fvector{0, 0, 0};
		Fvector scale = Fvector{1, 1, 1};
		int parent_bone_id = 0;
		IKinematics* m_model = nullptr;
	};
	shared_str mod_sect_name;
	CInventoryItem* m_parent = nullptr;	
	placement place, hud_place;
	EattachmentType attachment_type = EattachmentType::eTypeNone;
	Flags32 state = {0u};
};

struct item_attachments_manager
{
	item_attachments_manager(CInventoryItem* item)
		: m_parent(item) { m_attachments.clear(); };
	xr_hash_map<shared_str, item_attachment> m_attachments;
	CInventoryItem* m_parent = nullptr;

	void load_attachments(IKinematics* parent_model);
	void load_attachment(shared_str sect_name, IKinematics* parent_model);
	void switch_attachment(shared_str sect_name);
	void enable_attachment(shared_str sect_name);
	void disable_attachment(shared_str sect_name);

	void anim_play_attachment(const shared_str& item_anm_name, float speed, bool bMixIn);

	ICF const item_attachment* get_attachment(shared_str sect_name, EattachmentType type = eTypeNone) const
	{
		auto it = m_attachments.find(sect_name);
		if (it != m_attachments.end())
		{
			const item_attachment& attachment = it->second;
			if (type == eTypeNone || attachment.attachment_type == type)
				return &it->second;
		}

		return nullptr;
	};
	ICF const item_attachment* get_attachment(EattachmentType type = eTypeNone) const
	{
		for (auto& pair : m_attachments)
		{
			const item_attachment& attachment = pair.second;
			if (attachment.attachment_type == type)
			{
				return &pair.second;
			}
		}

		return nullptr;
	};

	void unload_attachments();
	void render_attachments(Fmatrix& xform, IKinematics* parent_model, bool hud_mode);
};