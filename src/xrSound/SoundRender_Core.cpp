#include "stdafx.h"

#include "../xrEngine/xrLevel.h"
#include "../xrCore/Collision/cl_intersect.h"
#include "SoundRender_Core.h"
#include "New/SoundMixer.h"
#include "New/SoundBackend.h"
#include "New/SoundMixerInternal.h"
#include "../xrEngine/IRenderable.h"
#include "Recorder/SoundVoiceChat.h"
#include "ai_sounds.h"

using namespace XRay::Sound;

int		psSoundTargets = 256;
Flags32	psSoundFlags = { ss_Hardware };
float	psSoundOcclusionScale = 0.5f;
float	psSoundDoppler = 1.0f;
float	psSoundCull = 0.01f;
float	psSoundRolloff = 0.75f;
u32		psSoundModel = 0;
float	psSoundVEffects = 1.0f;
float	psSoundVFactor = 1.0f;
float	psSoundVShooting = 1.0f;
float	psSoundCompression = 0.5f;

float	psSoundVMusic = 1.0f;
int		psSoundCacheSizeMB = 256;

// Voice Chat
float	psSoundVRecorder = 1.f;
int		psSoundRecorderMode = 1;
int		psSoundRecorderDenoise = 1;
float	psSoundVPlayers = 1.f;

float   psTimeFactor = 1.0f;

CSoundRender_Core* SoundRender = nullptr;
CSound_manager_interface* Sound = nullptr;

thread_local CDB::COLLIDER geom_DB;

void CSoundRender_Core::debug_draw()
{

}

void CSoundRender_Core::update(const Fmatrix& m_V, const Fvector& P, const Fvector& D, const Fvector& N)
{
	if (!bReady)
	{
		return;
	}

	// Events
	listenerPos = P;
	XRay::Sound::Mixer::Update((void*)Handler, psTimeFactor, master_volume, psSoundVEffects, psSoundVMusic, psSoundVEffects * psSoundVShooting, psSoundCompression, m_V, P, D, N);
#ifdef XR_MP_BUILD
	pSoundVoiceChat->Update(P, D, N);
#endif
}

static u32 g_saved_event_count = 0;
void CSoundRender_Core::update_events()
{
	PROF_EVENT("Sound: Update Events");
	g_saved_event_count = (u32)s_events.size();
	for (u32 it = 0; it < s_events.size(); it++)
	{
		event& E = s_events[it];
		Handler(E.first, E.second);
	}
	s_events.resize(0);
}

void CSoundRender_Core::statistic(CSound_stats* dest, CSound_stats_ext* ext)
{
}

void CSoundRender_Core::time_factor(float time_factor)
{
	psTimeFactor = time_factor;
}

float CSoundRender_Core::get_occlusion(Fvector& P, float R, Fvector* occ)
{
	float occ_value = 1.f;

	// Calculate RAY params
	Fvector base = listener_position();
	Fvector	pos, dir;
	float	range;
	pos.random_dir();
	pos.mul(R);
	pos.add(P);
	dir.sub(pos, base);
	range = dir.magnitude();
	dir.div(range);

	if (0 != geom_MODEL) {
		bool bNeedFullTest = true;
		// 1. Check cached polygon
		float _u, _v, _range;
		if (CDB::TestRayTri(base, dir, occ, _u, _v, _range, true))
			if (_range > 0 && _range < range) { occ_value = psSoundOcclusionScale; bNeedFullTest = false; }
		// 2. Polygon doesn't picked up - real database query
		if (bNeedFullTest)
		{
			geom_DB.ray_options(CDB::OPT_ONLYNEAREST);
			geom_DB.ray_query(geom_MODEL, base, dir, range);
			if (0 != geom_DB.r_count())
			{
				// cache polygon
				const CDB::RESULT* R_ = geom_DB.r_begin();
				const CDB::TRI& T = geom_MODEL->get_tris()[R_->id];
				const xr_vector<Fvector>& V = geom_MODEL->get_verts();
				occ[0].set(V[T.verts[0]]);
				occ[1].set(V[T.verts[1]]);
				occ[2].set(V[T.verts[2]]);
				occ_value = OcclusionMaterialCallback(R_->material);
			}
		}
	}
	if (0 != geom_SOM)
	{
		geom_DB.ray_options(CDB::OPT_CULL);
		geom_DB.ray_query(geom_SOM, base, dir, range);
		u32 r_cnt = geom_DB.r_count();
		CDB::RESULT* _B = geom_DB.r_begin();

		if (0 != r_cnt) {
			for (u32 k = 0; k < r_cnt; k++) {
				CDB::RESULT* R_ = _B + k;
				occ_value *= *(float*)&R_->dummy;
			}
		}
	}
	return occ_value;
}

float CSoundRender_Core::get_occlusion_to(const Fvector& hear_pt, const Fvector& snd_pt, float dispersion)
{
	float occ_value = 1.f;

	if (0 != geom_SOM) {
		// Calculate RAY params
		Fvector	pos, dir;
		pos.random_dir();
		pos.mul(dispersion);
		pos.add(snd_pt);
		dir.sub(pos, hear_pt);
		float range = dir.magnitude();
		dir.div(range);

		geom_DB.ray_options(CDB::OPT_CULL);
		geom_DB.ray_query(geom_SOM, hear_pt, dir, range);
		u32 r_cnt = geom_DB.r_count();
		CDB::RESULT* _B = geom_DB.r_begin();

		if (0 != r_cnt) {
			for (u32 k = 0; k < r_cnt; k++) {
				CDB::RESULT* R = _B + k;
				occ_value *= *(float*)&R->dummy;
			}
		}
	}
	return occ_value;
}

CSoundRender_Core::CSoundRender_Core()
{
	bPresent = false;
	bUserEnvironment = false;
	geom_MODEL = nullptr;
	geom_ENV = nullptr;
	geom_SOM = nullptr;
	s_environment = nullptr;
	Handler = nullptr;
	e_current.set_identity();
	e_target.set_identity();
	bListenerMoved = false;
	bReady = false;
	m_iPauseCounter = 1;
}

CSoundRender_Core::~CSoundRender_Core()
{
	xr_delete(geom_ENV);
	xr_delete(geom_SOM);
	xr_delete(pSoundVoiceChat);
}

void CSoundRender_Core::_initialize(int stage)
{
	if (stage == 0) 
	{
		GenerateDevicesToken();
		XRay::Sound::Mixer::Initialize();
	}

	env_load();
	bPresent = true;
	bReady = true;

#ifdef XR_MP_BUILD
	pSoundVoiceChat = new SoundVoiceChat();
#endif
}

void CSoundRender_Core::_clear()
{
	XRay::Sound::Mixer::Shutdown();

	bReady = false;
	env_unload();
	xr_delete(pSoundVoiceChat);
}

void CSoundRender_Core::stop_emitters()
{
	Mixer::StopAll();
}

int CSoundRender_Core::pause_emitters(bool val)
{
	m_iPauseCounter += val ? +1 : -1;
	VERIFY(m_iPauseCounter >= 0);

	if (val) {
		Mixer::PauseAll();
	} else {
		Mixer::ResumeAll();
	}

	return m_iPauseCounter;
}

void CSoundRender_Core::env_load()
{
	// Load environment
	string_path	fn;
	if (FS.exist(fn, _game_data_, SNDENV_FILENAME)) {
		s_environment = new SoundEnvironment_LIB();
		s_environment->Load(fn);
	}

	// Load geometry
	// Assosiate geometry
}

void CSoundRender_Core::env_unload()
{
	// Unload 
	if (s_environment)
		s_environment->Unload();
	xr_delete(s_environment);

	// Unload geometry
}

void CSoundRender_Core::_restart()
{
	env_apply();
}

CDB::COLLIDER* CSoundRender_Core::get_geometry_db()
{
	return &geom_DB;
}

CDB::MODEL* CSoundRender_Core::get_geometry_env()
{
	return geom_ENV;
}

CDB::MODEL*CSoundRender_Core::get_geometry_som()
{
	return geom_SOM;
}

CDB::MODEL* CSoundRender_Core::get_geometry_occ()
{
	return geom_MODEL;
}

void CSoundRender_Core::set_handler(sound_event* E)
{
	Handler = E;
}

void CSoundRender_Core::set_geometry_occ(CDB::MODEL* M)
{
	geom_MODEL = M;
}

void CSoundRender_Core::set_geometry_som(IReader* I)
{
#ifdef _EDITOR
	ETOOLS::destroy_model(geom_SOM);
#else
	xr_delete(geom_SOM);
#endif
	if (0 == I)		return;

	// check version
	R_ASSERT(I->find_chunk(0));
	u32 version = I->r_u32();
	VERIFY2(version == 0, "Invalid SOM version");
	// load geometry	
	IReader* geom = I->open_chunk(1);
	VERIFY2(geom, "Corrupted SOM file");
	// Load tris and merge them
	struct SOM_poly {
		Fvector3	v1;
		Fvector3	v2;
		Fvector3	v3;
		u32			b2sided;
		float		occ;
	};
	// Create AABB-tree
	static CDB::Collector CL; CL.clear();
	while (!geom->eof()) {
		SOM_poly				P;
		geom->r(&P, sizeof(P));
		CL.add_face_packed_D(P.v1, P.v2, P.v3, *(u32*)&P.occ, 0.01f);
		if (P.b2sided)
			CL.add_face_packed_D(P.v3, P.v2, P.v1, *(u32*)&P.occ, 0.01f);
	}

	geom_SOM = new CDB::MODEL();
	geom_SOM->build(CL.getV(), CL.getVS(), CL.getT(), CL.getTS());
	geom_SOM->wait_loading();

	geom->close();
}

void CSoundRender_Core::set_geometry_env(IReader* I)
{
	xr_delete(geom_ENV);

	if (I == nullptr || s_environment == nullptr)
	{
		return;
	}

	// Associate names
	xr_vector<u16> ids;
	IReader* names = I->open_chunk(0);
	while (!names->eof())
	{
		string256 n;
		names->r_stringZ(n, sizeof(n));

		int id = s_environment->GetID(n);
		R_ASSERT(id >= 0);

		ids.push_back(u16(id));
	}
	names->close();

	// Load geometry
	IReader* GeomChunk = I->open_chunk(1);
	u8* _data = (u8*)xr_malloc(GeomChunk->length());

	Memory.mem_copy(_data, GeomChunk->pointer(), GeomChunk->length());
	IReader* Geom = new IReader(_data, GeomChunk->length(), 0);

	hdrCFORM H;
	Geom->r(&H, sizeof(hdrCFORM));
	Fvector* verts = (Fvector*)Geom->pointer();
	CDB::TRI* tris = (CDB::TRI*)(verts + H.vertcount);

	Mixer::ResetZones();

	for (u32 idx_offset = 0; idx_offset < H.facecount; idx_offset += 12)
	{
		sound_zone_params params = {};
		params.min = Fvector(1000000, 1000000, 1000000);
		params.max = Fvector(-1000000, -1000000, -1000000);

		u16 base_id_front = (u16)(((tris + idx_offset)->dummy & 0x0000ffff) >> 0);
		u16 base_id_back = (u16)(((tris + idx_offset)->dummy & 0xffff0000) >> 16);
		u32 id = base_id_back;

		for (size_t i = 0; i < 12; i++)
		{
			CDB::TRI* T = tris + idx_offset + i;

			u16 id_front = (u16)((T->dummy & 0x0000ffff) >> 0); //	front face
			u16 id_back = (u16)((T->dummy & 0xffff0000) >> 16); //	back face
			R_ASSERT(id_front == base_id_front);
			R_ASSERT(id_back == base_id_back);

			// T->dummy = u32(ids[id_back] << 16) | u32(ids[id_front]); // old gsc kal
			T->dummy = Mixer::GetZones().size();

			params.min.min(verts[T->verts[0]]);
			params.max.max(verts[T->verts[0]]);
			params.min.min(verts[T->verts[1]]);
			params.max.max(verts[T->verts[1]]);
			params.min.min(verts[T->verts[2]]);
			params.max.max(verts[T->verts[2]]);
		}

		const CSoundRender_Environment* LocalEnv = s_environment->Get(ids[id]);
		R_ASSERT(LocalEnv);

		params.version = LocalEnv->version;
		params.name = LocalEnv->name;
		params.environment = LocalEnv->Environment;
		params.settings.room = LocalEnv->Room;
		params.settings.room_rolloff_factor = LocalEnv->RoomRolloffFactor;
		params.settings.decay_time = LocalEnv->DecayTime;
		params.settings.decay_hf_ratio = LocalEnv->DecayHFRatio;
		params.settings.reflections = LocalEnv->Reflections;
		params.settings.reflections_delay = LocalEnv->ReflectionsDelay;
		params.settings.reverb = LocalEnv->Reverb;
		params.settings.reverb_delay = LocalEnv->ReverbDelay;
		params.settings.environment_size = LocalEnv->EnvironmentSize;
		params.settings.environment_diffusion = LocalEnv->EnvironmentDiffusion;
		params.settings.air_absorption_hf = LocalEnv->AirAbsorptionHF;

		params.center = params.max;
		params.center.add(params.min);
		params.center.div(2);
		params.size = params.max;
		params.size.sub(params.min);
		params.size.div(2);

		Mixer::AddZone(params);
	}

	geom_ENV = new CDB::MODEL();
	geom_ENV->build(verts, H.vertcount, tris, H.facecount);
	geom_ENV->wait_loading();

	GeomChunk->close();
	Geom->close();
	xr_free(_data);
}

void CSoundRender_Core::set_master_volume(float f)
{
	master_volume = f;
}

void CSoundRender_Core::create(ref_sound& S, const char* fName, esound_type sound_type, int game_type)
{
	if (!bPresent)
	{
		return;
	}

	S._p = new ref_sound_data(fName, sound_type, game_type);
}

void CSoundRender_Core::attach_tail(ref_sound& S, const char* fName)
{
	if (!bPresent)
	{
		return;
	}
	string_path fn;
	xr_strcpy(fn, fName);

	if (strext(fn))
	{
		*strext(fn) = 0;
	}

	if (S._p->fn_attached[0].size() && S._p->fn_attached[1].size())
	{
		return;
	}

	u32 idx = S._p->fn_attached[0].size() ? 1 : 0;
	S._p->fn_attached[idx] = fn;
}

void CSoundRender_Core::clone(ref_sound& S, const ref_sound& from, esound_type sound_type, int	game_type)
{
	if (!bPresent)		return;

	S._p = new ref_sound_data();
	S._p->fTimeTotal = from._p->fTimeTotal;
	S._p->fn_attached[0] = from._p->fn_attached[0];
	S._p->fn_attached[1] = from._p->fn_attached[1];
	S._p->g_type = game_type;// (game_type == sg_SourceType) ? S._p->handle->game_type() : game_type;
	S._p->s_type = sound_type;
}

u32 CSoundRender_Core::GetMixedFlags(u32 flags, ref_sound& S)
{
	u32 MixedFlags = 0;

	if (flags & sm_Looped)
	{
		MixedFlags |= (u32)Mixer::Flags::Looped;
	}

	if (flags & sm_NoFeedback)
	{
		MixedFlags |= (u32)Mixer::Flags::NoFeedback;
	}

	if (S._sound_type() == st_Shooting)
	{
		MixedFlags |= (u32)Mixer::Flags::Shooting;
	}

	if ((flags & sm_Intro))
	{
		if (S._sound_type() == st_Music)
		{
			MixedFlags |= (u32)Mixer::Flags::Music;
		}

		MixedFlags |= (u32)Mixer::Flags::Intro;
	}
	else if ((flags & sm_2D) == 0)
	{
		MixedFlags |= (u32)Mixer::Flags::Spatial;
	}

	if (S._g_type() == ESoundTypes::SOUND_TYPE_WORLD_AMBIENT)
	{
		MixedFlags |= (u32)Mixer::Flags::NoOCC;
	}

	return MixedFlags;
}

void CSoundRender_Core::play(ref_sound& S, CObject* O, u32 flags, float delay)
{
	if (!bPresent || !S.handle()) return;
	S._p->g_object = O;

	u32 mixer_flags = GetMixedFlags(flags, S);

	if (!S.slot())
	{
		S._p->slot = Mixer::Create();
	}

	Mixer::Play(S.slot(), mixer_flags, &S, delay); 
	if (O) {
		Mixer::UpdateParameter(S.slot(), Mixer::ParameterId::Position, ((IRenderable*)O)->renderable.xform.c);
	}
}

void CSoundRender_Core::play_no_feedback(ref_sound& S, CObject* O, u32 flags, float delay, Fvector* pos, float* vol, float* freq, Fvector2* range)
{
	if (!bPresent || !S.handle())
		return;

	Fvector range_vec;
	Fvector* range_ptr = nullptr;
	if (range) {
		range_vec = Fvector(range->x, range->y, range->y);
		range_ptr = &range_vec;
	}

	u32 mixer_flags = (u32)Mixer::Flags::NoFeedback | GetMixedFlags(flags, S);
	Mixer::PlayNoFeedback(mixer_flags, &S, O, delay, freq, vol, range_ptr, pos);
}

void CSoundRender_Core::play_at_pos(ref_sound& S, CObject* O, const Fvector& pos, u32 flags, float delay)
{
	if (!bPresent || !S.handle())
		return;

	S._p->g_object = O;

	if (!S.slot()) {
		S._p->slot = Mixer::Create();
	}

	u32 mixer_flags = (u32)Mixer::Flags::NoPosUpdate | GetMixedFlags(flags, S);
	Mixer::Play(S.slot(), mixer_flags, &S, delay);
	S._p->fTimeTotal = Mixer::GetDuration(S.slot());
	S._p->g_type = (S._p->g_type == sg_SourceType) ? S._p->g_type : XRay::Sound::Mixer::GetGameType(S.slot());
	Mixer::UpdateParameter(S.slot(), Mixer::ParameterId::Position, pos);

}

void CSoundRender_Core::destroy(ref_sound& S)
{
	Mixer::Destroy(S.slot());
	if (S._p) S._p->slot = 0;
	S._p = 0;
}

void CSoundRender_Core::_create_data(ref_sound_data& S, const char* fName, esound_type sound_type, int game_type)
{
	string_path fn;
	xr_strcpy(fn, fName);

	if (strext(fn))
		*strext(fn) = 0;

	S.s_type = sound_type;
	S.g_type = game_type;
	S.slot = 0;
	S.g_object = 0;
	S.g_userdata = 0;
	S.fn_attached[0] = fn;
}

void CSoundRender_Core::_destroy_data(ref_sound_data& S)
{
	if (!S.dont_destroy_slot)
	{
		Mixer::Destroy(S.slot);
	}

	S.slot = 0;
	S.fn_attached[0] = nullptr;
	S.fn_attached[1] = nullptr;
}

void CSoundRender_Core::GenerateDevicesToken()
{
	int DevicesCount = 0;
	SDL_AudioDeviceID* Devices = SDL_GetAudioPlaybackDevices(&DevicesCount);

	snd_devices_token = new xr_token[DevicesCount + 2];

	snd_devices_token[0].id = 0;
	snd_devices_token[0].name = "default device";

	SoundDevices[snd_devices_token[0].name] = SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK;

	for (int Iter = 0; Iter < DevicesCount; Iter++)
	{
		xr_string DeviceName = Platform::UTF8_to_CP1251(SDL_GetAudioDeviceName(Devices[Iter]));
		snd_devices_token[Iter + 1].id = Iter + 1;
		snd_devices_token[Iter + 1].name = xr_strdup(DeviceName.c_str());

		xr_strlwr(DeviceName);
		SoundDevices[DeviceName] = Devices[Iter];
	}

	snd_devices_token[DevicesCount + 1].id = -1;
	snd_devices_token[DevicesCount + 1].name = nullptr;

	SDL_free(Devices);
}

void CSoundRender_Core::SwitchAuidoDevice(const xr_string& Name)
{
	if (!SoundDevices.contains(Name))
		return;

	SDL_AudioDeviceID NewDevice = SoundDevices[Name];
	XRay::Sound::Backend::ChangeDevice(NewDevice);
}

void CSoundRender_Core::env_apply()
{
	bListenerMoved = true;
}

ISoundVoiceChat* CSoundRender_Core::GetSoundVoiceChat()
{
	return pSoundVoiceChat;
}

const Fvector& CSoundRender_Core::listener_position()
{
	return listenerPos;
}

void CSoundRender_Core::objects_relcase(CObject** objects, int count)
{
	Mixer::DereferenceObjects(objects, count);
}

void CSoundRender_Core::set_user_env(CSound_environment* E)
{
	if ((0 == E) && !bUserEnvironment) return;

	if (E)
	{
		s_user_environment = *((CSoundRender_Environment*)E);
		sound_zone_params params = {};
		params.min = Fvector(1000000, 1000000, 1000000);
		params.max = Fvector(-1000000, -1000000, -1000000);

		params.version = s_user_environment.version;
		params.name = s_user_environment.name;
		params.environment = s_user_environment.Environment;
		params.settings.room = s_user_environment.Room;
		params.settings.room_rolloff_factor = s_user_environment.RoomRolloffFactor;
		params.settings.decay_time = s_user_environment.DecayTime;
		params.settings.decay_hf_ratio = s_user_environment.DecayHFRatio;
		params.settings.reflections = s_user_environment.Reflections;
		params.settings.reflections_delay = s_user_environment.ReflectionsDelay;
		params.settings.reverb = s_user_environment.Reverb;
		params.settings.reverb_delay = s_user_environment.ReverbDelay;
		params.settings.environment_size = s_user_environment.EnvironmentSize;
		params.settings.environment_diffusion = s_user_environment.EnvironmentDiffusion;
		params.settings.air_absorption_hf = s_user_environment.AirAbsorptionHF;

		params.center = params.max;
		params.center.add(params.min);
		params.center.div(2);
		params.size = params.max;
		params.size.sub(params.min);
		params.size.div(2);

		Mixer::AddEditorZone(params);
		bUserEnvironment = true;
	} else {
		bUserEnvironment = false;
	}

	env_apply();
}

void CSoundRender_Core::refresh_env_library()
{
	env_unload();
	env_load();
	env_apply();
}

void CSoundRender_Core::refresh_sources()
{
	Mixer::StopAll();

	// TODO: 
	/*
	for (u32 sit = 0; sit < s_sources.size(); sit++) {
		CSoundRender_Source* s = s_sources[sit];
		s->unload();
		s->load(*s->fname);
	}
	*/
}