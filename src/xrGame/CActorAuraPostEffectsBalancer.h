#pragma once

class CGameObject;
class CEffectorPP;
class CActor;

enum class EAuraPostEffectType : u8
{
	Fire,
	Radiation,
	Psi,
	Chemical,
	Gravity,
};

inline constexpr u32 cInvalidAuraObjectID = 0xFFFFFFFFu;

class CActorAuraPostEffectsBalancer
{
public:
	struct SAuraEffectKey
	{
		EAuraPostEffectType type;
		u32 object_id;
		shared_str pp_section;

		bool operator<(const SAuraEffectKey& other) const;
		bool operator==(const SAuraEffectKey& other) const;
	};

	struct SRunningInstance
	{
		const CActorAuraPostEffectsBalancer* owner = nullptr;
		u32 pp_slot_id = 0;
		CEffectorPP* effector = nullptr;
		float target_intensity = 0.f;
		mutable float applied_intensity = 0.f;
		bool dying = false;
		u32 anchor_object_id = cInvalidAuraObjectID;
		xr_vector<SAuraEffectKey> contributors;

		float get_factor() const;
	};

	void RegisterEffect(EAuraPostEffectType type, float intensity, u32 object_id, float max_processing_distance, const shared_str& pp_section);
	void UnregisterEffect(EAuraPostEffectType type, u32 object_id, const shared_str& pp_section = shared_str());
	void update(CActor& actor);
	void clear_all();

private:
	struct SAuraEffectRecord
	{
		CGameObject* game_object_ptr;
		float intensity;
		float max_distance;
	};

	struct SAuraRunningKey
	{
		EAuraPostEffectType type;
		shared_str pp_section;

		bool operator<(const SAuraRunningKey& other) const;
		bool operator==(const SAuraRunningKey& other) const;
	};

	using RecordMap = xr_map<SAuraEffectKey, SAuraEffectRecord>;
	using RunningMap = xr_map<SAuraRunningKey, SRunningInstance>;

	RecordMap m_records;
	RunningMap m_running;
	CActor* m_actor = nullptr;

	float calc_live_factor(const SRunningInstance& inst) const;
	float distance_fade(const SAuraEffectRecord& rec, float dist) const;
	float source_distance(const SAuraEffectKey& key, const SAuraEffectRecord& rec) const;
	void spawn_instance(const SAuraRunningKey& key, const SRunningInstance& proto);
	RunningMap::iterator remove_instance(RunningMap::iterator it);
};
