#include "StdAfx.h"
#include "CActorAuraPostEffectsBalancer.h"
#include "Actor.h"
#include "Level.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_object_registry.h"
#include "PostprocessAnimator.h"
#include "ActorEffector.h"
#include "pp_effector_custom.h"

namespace
{
	float clamp01(float v)
	{
		if (v < 0.f)	return 0.f;
		if (v > 1.f)	return 1.f;
		return v;
	}

	float distance_fade_impl(float dist, float max_distance)
	{
		if (max_distance <= 0.f)
			return 1.f;

		if (dist <= max_distance * 0.8f)
			return 1.f;

		return clamp01((max_distance - dist) / (max_distance * 0.2f));
	}

	void load_pp_state(const char* section, SPPInfo& out)
	{
		out.duality.h			= pSettings->r_float(section, "duality_h");
		out.duality.v			= pSettings->r_float(section, "duality_v");
		out.gray				= pSettings->r_float(section, "gray");
		out.blur				= pSettings->r_float(section, "blur");
		out.noise.intensity		= pSettings->r_float(section, "noise_intensity");
		out.noise.grain			= pSettings->r_float(section, "noise_grain");
		out.noise.fps			= pSettings->r_float(section, "noise_fps");
		sscanf(pSettings->r_string(section, "color_base"), "%f,%f,%f", &out.color_base.r, &out.color_base.g, &out.color_base.b);
		sscanf(pSettings->r_string(section, "color_gray"), "%f,%f,%f", &out.color_gray.r, &out.color_gray.g, &out.color_gray.b);
		sscanf(pSettings->r_string(section, "color_add"), "%f,%f,%f", &out.color_add.r, &out.color_add.g, &out.color_add.b);
	}

	class CAuraPPEffectorSpp : public CPPEffectorCustom
	{
		typedef CPPEffectorCustom inherited;

		const CActorAuraPostEffectsBalancer::SRunningInstance*	m_inst;

	public:
		CAuraPPEffectorSpp(const SPPInfo& ppi, const CActorAuraPostEffectsBalancer::SRunningInstance* inst)
			: inherited(ppi), m_inst(inst)
		{
		}

		virtual bool	update	() override
		{
			m_factor = m_inst ? m_inst->get_factor() : 0.f;
			return true;
		}
	};
}

CActorAuraPostEffectsBalancer::RecordMap	CActorAuraPostEffectsBalancer::m_records;
CActorAuraPostEffectsBalancer::RunningMap	CActorAuraPostEffectsBalancer::m_running;

xr_map<EAuraPostEffectType, u32> CActorAuraPostEffectsBalancer::smax_playing =
{
	{EAuraPostEffectType::Fire,		2},
	{EAuraPostEffectType::Radiation,	2},
	{EAuraPostEffectType::Psi,		2},
	{EAuraPostEffectType::Chemical,	2},
	{EAuraPostEffectType::Gravity,		2},
};

bool CActorAuraPostEffectsBalancer::SAuraEffectKey::operator<(const SAuraEffectKey& other) const
{
	if (type != other.type)				return type < other.type;
	if (object_id != other.object_id)	return object_id < other.object_id;
	return pp_section < other.pp_section;
}

bool CActorAuraPostEffectsBalancer::SAuraEffectKey::operator==(const SAuraEffectKey& other) const
{
	return type == other.type && object_id == other.object_id && pp_section == other.pp_section;
}

bool CActorAuraPostEffectsBalancer::SAuraRunningKey::operator<(const SAuraRunningKey& other) const
{
	if (type != other.type)	return type < other.type;
	return pp_section < other.pp_section;
}

bool CActorAuraPostEffectsBalancer::SAuraRunningKey::operator==(const SAuraRunningKey& other) const
{
	return type == other.type && pp_section == other.pp_section;
}

float CActorAuraPostEffectsBalancer::SRunningInstance::get_factor() const
{
	static constexpr float s_fade_speed = 2.5f;

	const float target = dying ? 0.f : CActorAuraPostEffectsBalancer::calc_live_factor(*this);
	applied_intensity += (target - applied_intensity) * clamp01(Device.fTimeDelta * s_fade_speed);
	return applied_intensity;
}

float CActorAuraPostEffectsBalancer::calc_live_factor(const SRunningInstance& inst)
{
	float sum = 0.f;
	for (const SAuraEffectKey& key : inst.contributors)
	{
		const auto it = m_records.find(key);
		if (it == m_records.end())
			continue;

		const SAuraEffectRecord& rec = it->second;
		sum += rec.intensity * distance_fade(rec, source_distance(key, rec));
	}
	return clamp01(sum);
}

float CActorAuraPostEffectsBalancer::distance_fade(const SAuraEffectRecord& rec, float dist)
{
	return distance_fade_impl(dist, rec.max_distance);
}

float CActorAuraPostEffectsBalancer::source_distance(const SAuraEffectKey& key, const SAuraEffectRecord& rec)
{
	if (key.object_id == cInvalidAuraObjectID || !rec.game_object_ptr)
		return 0.f;

	CActor* actor = Actor();
	if (!actor)
		return 0.f;

	return rec.game_object_ptr->Position().distance_to(actor->Position());
}

void CActorAuraPostEffectsBalancer::RegisterEffect(EAuraPostEffectType type, float intensity, u32 object_id,
												   float max_processing_distance, const shared_str& pp_section)
{
	if (!pp_section.size())
		return;

	const SAuraEffectKey key{type, object_id, pp_section};

	const auto it = m_records.find(key);
	if (it == m_records.end())
	{
		SAuraEffectRecord rec;

		rec.game_object_ptr	= nullptr;
		if (object_id != cInvalidAuraObjectID)
		{
			CObject* finded_object		= Level().Objects.net_Find((ALife::_OBJECT_ID)object_id);
			rec.game_object_ptr			= finded_object != nullptr ? finded_object->cast_game_object() : nullptr;
		}

		rec.intensity		= intensity;
		rec.max_distance	= max_processing_distance;
		m_records.emplace(key, rec);
	}
	else
	{
		it->second.intensity		= intensity;
		it->second.max_distance		= max_processing_distance;
	}
}

void CActorAuraPostEffectsBalancer::UnregisterEffect(EAuraPostEffectType type, u32 object_id, const shared_str& pp_section)
{
	std::erase_if(m_records, [&](const RecordMap::value_type& elem)
	{
		const SAuraEffectKey& key = elem.first;
		return key.type == type
			&& key.object_id == object_id
			&& (!pp_section.size() || key.pp_section == pp_section);
	});
}

void CActorAuraPostEffectsBalancer::update()
{
	CActor* actor = Actor();
	if (!actor || !actor->g_Alive())
	{
		clear_all();
		return;
	}

	std::erase_if(m_records, [](const RecordMap::value_type& elem)
	{
		const SAuraEffectKey& key = elem.first;
		const SAuraEffectRecord& rec = elem.second;

		if (key.object_id == cInvalidAuraObjectID)
			return false;

		if (ai().get_alife() && !ai().alife().objects().object((ALife::_OBJECT_ID)key.object_id, true))
			return true;

		CObject* finded_object = Level().Objects.net_Find((ALife::_OBJECT_ID)key.object_id);
		CGameObject* pGameObject = finded_object != nullptr ? finded_object->cast_game_object() : nullptr;
		if (!pGameObject || pGameObject != rec.game_object_ptr)
			return true;

		return pGameObject->Position().distance_to(Actor()->Position()) > rec.max_distance;
	});

	for (auto it = m_running.begin(); it != m_running.end();)
	{
		if (actor->Cameras().GetPPEffector((EEffectorPPType)it->second.pp_slot_id) != it->second.effector)
			it = m_running.erase(it);
		else
			++it;
	}

	for (const auto& [type, limit] : smax_playing)
	{
		struct SCandidate
		{
			const SAuraEffectKey*		key;
			const SAuraEffectRecord*	rec;
			float						dist;
		};

		xr_vector<SCandidate> candidates;
		for (const auto& [key, rec] : m_records)
		{
			if (key.type != type || rec.intensity <= 0.f)
				continue;

			candidates.push_back({&key, &rec, source_distance(key, rec)});
		}

		std::sort(candidates.begin(), candidates.end(), [](const SCandidate& a, const SCandidate& b)
		{
			if (a.rec->intensity != b.rec->intensity)	return a.rec->intensity > b.rec->intensity;
			if (a.dist != b.dist)						return a.dist < b.dist;
			return *a.key < *b.key;
		});

		if (candidates.size() > limit)
			candidates.resize(limit);

		struct SWanted
		{
			xr_vector<SAuraEffectKey>	contributors;
			float	sum					= 0.f;
			u32		anchor_object_id	= cInvalidAuraObjectID;
			float	anchor_dist			= flt_max;
		};
		xr_map<shared_str, SWanted> wanted;

		for (const SCandidate& c : candidates)
		{
			SWanted& w = wanted[c.key->pp_section];
			w.contributors.push_back(*c.key);
			w.sum += c.rec->intensity;

			if (c.dist < w.anchor_dist)
			{
				w.anchor_dist		= c.dist;
				w.anchor_object_id	= c.key->object_id;
			}
		}

		for (const auto& [pp_section, w] : wanted)
		{
			const SAuraRunningKey rkey{type, pp_section};
			const float merged = clamp01(w.sum);

			const auto it = m_running.find(rkey);
			if (it == m_running.end())
			{
				SRunningInstance proto;
				proto.pp_slot_id		= 0;
				proto.effector			= nullptr;
				proto.target_intensity	= merged;
				proto.applied_intensity	= 0.f;
				proto.dying				= false;
				proto.anchor_object_id	= w.anchor_object_id;
				proto.contributors		= w.contributors;
				spawn_instance(rkey, proto);
			}
			else
			{
				it->second.dying				= false;
				it->second.target_intensity		= merged;
				it->second.anchor_object_id		= w.anchor_object_id;
				it->second.contributors			= w.contributors;
			}
		}

		for (auto it = m_running.begin(); it != m_running.end();)
		{
			if (it->first.type != type || wanted.find(it->first.pp_section) != wanted.end())
			{
				++it;
				continue;
			}

			it->second.dying = true;
			if (it->second.applied_intensity <= 0.001f)
				it = remove_instance(it);
			else
				++it;
		}
	}
}

void CActorAuraPostEffectsBalancer::spawn_instance(const SAuraRunningKey& key, const SRunningInstance& proto)
{
	CActor* actor = Actor();
	if (!actor)
		return;

	const auto ins = m_running.emplace(key, proto);
	SRunningInstance& inst = ins.first->second;

	const char* section = *key.pp_section;
	if (!section || !section[0])
	{
		m_running.erase(ins.first);
		return;
	}

	inst.pp_slot_id = (u32)actor->Cameras().RequestPPEffectorId();

	if (pSettings->line_exist(section, "pp_eff_name") || pSettings->line_exist(section, "ppe_file"))
	{
		const char* filename_key = pSettings->line_exist(section, "pp_eff_name") ? "pp_eff_name" : "ppe_file";
		CPostprocessAnimatorLerp* pp	= new CPostprocessAnimatorLerp();
		pp->SetType						((EEffectorPPType)inst.pp_slot_id);
		pp->SetCyclic					(READ_IF_EXISTS(pSettings, r_bool, section, "pp_eff_cyclic", true));
		pp->bOverlap					= READ_IF_EXISTS(pSettings, r_bool, section, "pp_eff_overlap", true);
		pp->SetFactorFunc				(GET_KOEFF_FUNC(&inst, &SRunningInstance::get_factor));
		pp->Load						(pSettings->r_string(section, filename_key));
		inst.effector = actor->Cameras().AddPPEffector(pp);
	}
	else if (pSettings->line_exist(section, "duality_h"))
	{
		SPPInfo ppi;
		load_pp_state(section, ppi);

		CAuraPPEffectorSpp* pp = new CAuraPPEffectorSpp(ppi, &inst);
		pp->SetType((EEffectorPPType)inst.pp_slot_id);
		inst.effector = actor->Cameras().AddPPEffector(pp);
	}
	else
	{
		m_running.erase(ins.first);
	}
}

CActorAuraPostEffectsBalancer::RunningMap::iterator CActorAuraPostEffectsBalancer::remove_instance(RunningMap::iterator it)
{
	if (Actor())
		Actor()->Cameras().RemovePPEffector((EEffectorPPType)it->second.pp_slot_id);

	return m_running.erase(it);
}

void CActorAuraPostEffectsBalancer::clear_all()
{
	if (Actor())
	{
		for (const auto& [key, inst] : m_running)
			Actor()->Cameras().RemovePPEffector((EEffectorPPType)inst.pp_slot_id);
	}

	m_running.clear();
	m_records.clear();
}
