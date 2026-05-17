#pragma once
#include "../../xrUI/Widgets/UIStatic.h"
#include "HudSound.h"

class CObject;

enum
{
	flVisObjNotValid = (1 << 0),
	flTargetLocked = (1 << 1),
};

struct SBinocVisibleObj
{
	SBinocVisibleObj() = default;
	CObject* m_object;
	CUIStatic m_lt;
	CUIStatic m_lb;
	CUIStatic m_rt;
	CUIStatic m_rb;
	Frect cur_rect;

	float m_upd_speed;
	Flags8 m_flags;
	void create_default(u32 color);
	void Draw();
	void Update();
	bool operator<(const SBinocVisibleObj& other) const { return m_flags.test(flVisObjNotValid) < other.m_flags.test(flVisObjNotValid); } //move non-actual to tail
};

struct TBinocularsVision final
{
	using VIS_OBJECTS = xr_vector<SBinocVisibleObj*>;
	using VIS_OBJECTS_IT = VIS_OBJECTS::iterator;
	VIS_OBJECTS	m_active_objects {};

	Fcolor m_frame_color;
	float m_rotating_speed = 0.0f;
	HUD_SOUND_COLLECTION m_sounds;
	shared_str m_section;

public:
	void EndComponent();

	void Load(const shared_str& section);
	void Update();
	void Draw();
	void remove_links(CObject* object);

private:
	ECS_COMPONENT(TBinocularsVision)
		ECS_STRING(m_section.c_str(), "Vision Section");
	ECS_END
};