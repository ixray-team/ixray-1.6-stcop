#pragma once

class CGameObject;
class CEffectorPP;

enum class EAuraPostEffectType : u8
{
	Fire,
	Radiation,
	Psi,
	Chemical,
};

inline constexpr u32 cInvalidAuraObjectID = 0xFFFFFFFFu;

class CActorAuraPostEffectsBalancer
{
public:
	struct SAuraEffectKey
	{
		EAuraPostEffectType	type;
		u32					object_id;
		shared_str			pp_section;

		bool	operator<	(const SAuraEffectKey& other) const;
		bool	operator==	(const SAuraEffectKey& other) const;
	};

	struct SRunningInstance
	{
		u32							pp_slot_id;
		CEffectorPP*				effector;
		float						target_intensity;
		mutable float				applied_intensity;
		bool						dying;
		u32							anchor_object_id;
		xr_vector<SAuraEffectKey>	contributors;

		float	get_factor			() const;
	};

public:
	static void	RegisterEffect		(EAuraPostEffectType type, float intensity, u32 object_id,
									 float max_processing_distance, const shared_str& pp_section);

	static void	UnregisterEffect	(EAuraPostEffectType type, u32 object_id, const shared_str& pp_section = shared_str());

	static void	update				();

	static void	clear_all			();

private:
	struct SAuraEffectRecord
	{
		CGameObject*	game_object_ptr;
		float			intensity;
		float			max_distance;
	};

	struct SAuraRunningKey
	{
		EAuraPostEffectType	type;
		shared_str			pp_section;

		bool	operator<	(const SAuraRunningKey& other) const;
		bool	operator==	(const SAuraRunningKey& other) const;
	};

	typedef xr_map<SAuraEffectKey, SAuraEffectRecord>	RecordMap;
	typedef xr_map<SAuraRunningKey, SRunningInstance>	RunningMap;

	static RecordMap	m_records;
	static RunningMap	m_running;

	static xr_map<EAuraPostEffectType, u32>	smax_playing;

private:
	static float	calc_live_factor	(const SRunningInstance& inst);
	static float	distance_fade		(const SAuraEffectRecord& rec, float dist);
	static float	source_distance		(const SAuraEffectKey& key, const SAuraEffectRecord& rec);
	static void		spawn_instance		(const SAuraRunningKey& key, const SRunningInstance& proto);
	static RunningMap::iterator	remove_instance	(RunningMap::iterator it);
};
