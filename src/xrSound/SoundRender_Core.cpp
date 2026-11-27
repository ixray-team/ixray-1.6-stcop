#include "stdafx.h"
#include "Collision/xr_area.h"

#include "../xrEngine/xrLevel.h"
#include "../xrCore/Collision/cl_intersect.h"
#include "SoundRender_Core.h"
#include "New/SoundMixer.h"
#include "New/SoundBackend.h"
#include "New/SoundMixerInternal.h"
#include "New/SoundSource.h"
#include "../xrEngine/IRenderable.h"
#include "Recorder/SoundVoiceChat.h"
#include "ai_sounds.h"

using namespace XRay::Sound;

int psSoundTargets = 1024 - 32;
Flags32 psSoundFlags = {0};
float psSoundOcclusionScale = 0.5f;
float psSoundDoppler = 0.5f;
float psSoundCull = 0.01f;
float psSoundRolloff = 0.75f;
u32 psSoundModel = 0;
float psSoundVFactor = 1.0f;
float psSoundCarFactor = 1.0f;
float psSoundShootingReverb = 0.1f;
float psSoundCompression = 0.5f;

int psSoundCacheSizeMB = 256;

// Voice Chat
float psSoundVRecorder = 1.f;
int psSoundRecorderMode = 1;
int psSoundRecorderDenoise = 1;
float psSoundVPlayers = 1.f;

float psTimeFactor = 1.0f;

CSoundRender_Core* SoundRender = nullptr;
CSound_manager_interface* Sound = nullptr;

thread_local CDB::COLLIDER geom_DB;

void CSoundRender_Core::debug_draw()
{

}

void CSoundRender_Core::update(const Fvector& P, const Fvector& D, const Fvector& N)
{
	if (!bReady)
	{
		return;
	}

	// Events
	listenerPos = P;
	// master_volume mutes the game when the window loses focus
	XRay::Sound::Mixer::Update((void*)Handler, psTimeFactor, master_volume, psSoundCompression, P, D, N);
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
	xrSRWLockGuard GeometryGuard(GeometryLock, true);
	float occ_value = 1.f;

	// Calculate RAY params
	Fvector base = listener_position();
	Fvector pos, dir;
	float range;
	pos.random_dir();
	pos.mul(R);
	pos.add(P);
	dir.sub(pos, base);
	range = dir.magnitude();
	dir.div(range);

	if (geom_MODEL)
	{
		bool bNeedFullTest = true;
		// 1. Check cached polygon
		float _u, _v, _range;
		if (!geom_MODEL->IsStreamingEnabled() && CDB::TestRayTri(base, dir, occ, _u, _v, _range, true))
		{
			if (_range > 0 && _range < range)
			{
				occ_value = psSoundOcclusionScale;
				bNeedFullTest = false;
			}
		}
		// 2. Polygon doesn't picked up - real database query
		if (bNeedFullTest)
		{
			geom_DB.ray_options(CDB::OPT_ONLYNEAREST);
			const auto Models = geom_MODEL->GetStaticModels();
			float NearestRange = range;
			for (const auto& Model : Models)
			{
				geom_DB.ray_query(*Model, base, dir, NearestRange);
				if (geom_DB.r_count())
				{
					const auto& Result = geom_DB.r_any();
					const auto& Tri = Result.model->get_tris()[Result.tris_id];
					const auto& Verts = Result.model->get_verts();
					for (u32 Index = 0; Index < 3; ++Index)
					{
						Result.ModelWorldTransform.transform_tiny(occ[Index], Verts[Tri.verts[Index]]);
					}
					NearestRange = Result.range;
					occ_value = OcclusionMaterialCallback(Tri.material);
				}
			}
		}
	}

	if (geom_SOM)
	{
		geom_DB.ray_options(CDB::OPT_CULL);
		geom_DB.ray_query(*geom_SOM, base, dir, range);
		auto& R = geom_DB.r_vec();
		if (!R.empty())
		{
			for (auto& elem : R)
			{
				occ_value *= *(float*)&elem.model->get_tris()[elem.tris_id].dummy;
			}
		}
	}
	return occ_value;
}

float CSoundRender_Core::get_occlusion_to(const Fvector& hear_pt, const Fvector& snd_pt, float dispersion)
{
	xrSRWLockGuard GeometryGuard(GeometryLock, true);
	float occ_value = 1.f;

	if (geom_SOM) {
		// Calculate RAY params
		Fvector	pos, dir;
		pos.random_dir();
		pos.mul(dispersion);
		pos.add(snd_pt);
		dir.sub(pos, hear_pt);
		float range = dir.magnitude();
		dir.div(range);

		geom_DB.ray_options(CDB::OPT_CULL);
		geom_DB.ray_query(*geom_SOM, hear_pt, dir, range);
		auto& R = geom_DB.r_vec();
		if (!R.empty())
		{
			for (auto& elem : R)
			{
				occ_value *= *(float*)&elem.model->get_tris()[elem.tris_id].dummy;
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

xrSRWLock& CSoundRender_Core::GetGeometryLock()
{
	return GeometryLock;
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

CObjectSpace* CSoundRender_Core::get_geometry_occ()
{
	return geom_MODEL;
}

void CSoundRender_Core::set_handler(sound_event* E)
{
	Handler = E;
}

void CSoundRender_Core::set_geometry_occ(CObjectSpace* M)
{
	swap_geometry_occ(M);
}

CDB::MODEL* CSoundRender_Core::swap_geometry_env(CDB::MODEL* model)
{
	xrSRWLockGuard RenderGuard(XRay::Sound::Mixer::GetRenderMutex());
	xrSRWLockGuard GeometryGuard(GeometryLock);
	CDB::MODEL* previous = geom_ENV;
	geom_ENV = model;
	return previous;
}

CDB::MODEL* CSoundRender_Core::swap_geometry_som(CDB::MODEL* model)
{
	xrSRWLockGuard RenderGuard(XRay::Sound::Mixer::GetRenderMutex());
	xrSRWLockGuard GeometryGuard(GeometryLock);
	CDB::MODEL* previous = geom_SOM;
	geom_SOM = model;
	return previous;
}

CObjectSpace* CSoundRender_Core::swap_geometry_occ(CObjectSpace* model)
{
	xrSRWLockGuard RenderGuard(XRay::Sound::Mixer::GetRenderMutex());
	xrSRWLockGuard GeometryGuard(GeometryLock);
	CObjectSpace* previous = geom_MODEL;
	geom_MODEL = model;
	return previous;
}

CDB::MODEL* CSoundRender_Core::create_geometry_som(IReader* reader)
{
	if (reader == nullptr || !reader->find_chunk(0) || reader->elapsed() < sizeof(u32) || reader->r_u32() != 0)
	{
		Msg("! Sound geometry SOM has an unsupported or truncated header; disabling it");
		return nullptr;
	}
	xr_unique_ptr<IReader> geom(reader->open_chunk(1));
	if (geom == nullptr)
		return nullptr;
	struct SOM_poly
	{
		Fvector3 v1;
		Fvector3 v2;
		Fvector3 v3;
		u32 b2sided;
		float occ;
	};
	if (geom->length() % sizeof(SOM_poly) != 0)
	{
		Msg("! Sound geometry SOM contains a truncated polygon; disabling it");
		return nullptr;
	}
	CDB::Collector collector;
	collector.clear();
	while (!geom->eof())
	{
		SOM_poly point;
		geom->r(&point, sizeof(point));
		if (!_valid(point.v1) || !_valid(point.v2) || !_valid(point.v3) || !_valid(point.occ))
		{
			Msg("! Sound geometry SOM contains an invalid polygon; disabling it");
			return nullptr;
		}
		collector.add_face_packed_D(point.v1, point.v2, point.v3, *(u32*)&point.occ, 0.01f);
		if (point.b2sided)
			collector.add_face_packed_D(point.v3, point.v2, point.v1, *(u32*)&point.occ, 0.01f);
	}
	if (collector.getVS() < 4 || collector.getTS() < 2)
	{
		Msg("! Sound geometry SOM has too little collision data; disabling it");
		return nullptr;
	}
	CDB::MODEL* model = new CDB::MODEL();
	model->build(collector.getV(), collector.getVS(), collector.getT(), collector.getTS());
	model->wait_loading();
	return model;
}

void CSoundRender_Core::set_geometry_som(IReader* I)
{
	CDB::MODEL* previous = swap_geometry_som(create_geometry_som(I));
#ifdef _EDITOR
	ETOOLS::destroy_model(previous);
#else
	xr_delete(previous);
#endif
}

CDB::MODEL* CSoundRender_Core::create_geometry_env(IReader* reader, SoundZoneVector& zones)
{
	zones.clear();
	if (reader == nullptr || s_environment == nullptr)
		return nullptr;

	xr_vector<u16> ids;
	xr_unique_ptr<IReader> names(reader->open_chunk(0));
	if (names == nullptr)
		return nullptr;
	while (!names->eof())
	{
		string256 name;
		names->r_stringZ(name, sizeof(name));
		const int id = s_environment->GetID(name);
		if (id < 0)
		{
			Msg("! Sound environment '%s' is unavailable; disabling level sound geometry", name);
			return nullptr;
		}
		ids.push_back(u16(id));
	}

	xr_unique_ptr<IReader> geometryChunk(reader->open_chunk(1));
	if (geometryChunk == nullptr || geometryChunk->length() < sizeof(hdrCFORM))
		return nullptr;
	const u8* bytes = static_cast<const u8*>(geometryChunk->pointer());
	xr_vector<u8> data(bytes, bytes + geometryChunk->length());
	IReader geometry(data.data(), data.size(), 0);
	hdrCFORM header;
	geometry.r(&header, sizeof(header));
	if (header.vertcount == 0 || header.facecount == 0 || header.facecount % 12 != 0 ||
		size_t(header.vertcount) * sizeof(Fvector) + size_t(header.facecount) * sizeof(CDB::TRI_Vanilla) > geometry.elapsed())
	{
		Msg("! Sound environment geometry has an invalid or truncated layout; disabling it");
		return nullptr;
	}
	xr_vector<Fvector> vertices(header.vertcount);
	geometry.r(vertices.data(), vertices.size() * sizeof(Fvector));
	xr_vector<CDB::TRI> triangles(header.facecount);
	for (CDB::TRI& triangle : triangles)
	{
		CDB::TRI_Vanilla packed;
		geometry.r(&packed, sizeof(packed));
		std::copy(std::begin(packed.verts), std::end(packed.verts), triangle.verts);
		triangle.dummy = packed.dummy;
		triangle.StreamedSectorID = 0;
	}

	for (u32 offset = 0; offset < header.facecount; offset += 12)
	{
		sound_zone_params params = {};
		params.min = Fvector(1000000, 1000000, 1000000);
		params.max = Fvector(-1000000, -1000000, -1000000);
		const u16 baseIdFront = u16(triangles[offset].dummy & 0x0000ffff);
		const u16 baseIdBack = u16((triangles[offset].dummy & 0xffff0000) >> 16);
		if (baseIdBack >= ids.size())
		{
			Msg("! Sound environment zone index is invalid; disabling it");
			return nullptr;
		}
		for (size_t i = 0; i < 12; ++i)
		{
			CDB::TRI* triangle = &triangles[offset + i];
			const u16 idFront = u16(triangle->dummy & 0x0000ffff);
			const u16 idBack = u16((triangle->dummy & 0xffff0000) >> 16);
			if (idFront != baseIdFront || idBack != baseIdBack)
			{
				Msg("! Sound environment zone has inconsistent triangles; disabling it");
				return nullptr;
			}
			for (u32 vertex : triangle->verts)
			{
				if (vertex >= header.vertcount || !_valid(vertices[vertex]))
				{
					Msg("! Sound environment zone has an invalid vertex; disabling it");
					return nullptr;
				}
				params.min.min(vertices[vertex]);
				params.max.max(vertices[vertex]);
			}
			triangle->dummy = zones.size();
		}
		const CSoundRender_Environment* localEnvironment = s_environment->Get(ids[baseIdBack]);
		if (localEnvironment == nullptr)
			return nullptr;
		params.version = localEnvironment->version;
		params.name = localEnvironment->name;
		params.environment = localEnvironment->Environment;
		params.settings.room = localEnvironment->Room;
		params.settings.room_hf = localEnvironment->RoomHF;
		params.settings.room_rolloff_factor = localEnvironment->RoomRolloffFactor;
		params.settings.decay_time = localEnvironment->DecayTime;
		params.settings.decay_hf_ratio = localEnvironment->DecayHFRatio;
		params.settings.reflections = localEnvironment->Reflections;
		params.settings.reflections_delay = localEnvironment->ReflectionsDelay;
		params.settings.reverb = localEnvironment->Reverb;
		params.settings.reverb_delay = localEnvironment->ReverbDelay;
		params.settings.environment_size = localEnvironment->EnvironmentSize;
		params.settings.environment_diffusion = localEnvironment->EnvironmentDiffusion;
		params.settings.air_absorption_hf = localEnvironment->AirAbsorptionHF;
		params.center = params.max;
		params.center.add(params.min);
		params.center.div(2);
		params.size = params.max;
		params.size.sub(params.min);
		params.size.div(2);
		XRay::Sound::Mixer::PrepareZone(params);
		zones.push_back(params);
	}

	CDB::MODEL* model = new CDB::MODEL();
	model->build(vertices.data(), header.vertcount, triangles.data(), header.facecount);
	model->wait_loading();
	return model;
}

void CSoundRender_Core::set_geometry_env(IReader* I)
{
	Snd_PurgeIdleSources();
	SoundZoneVector zones;
	CDB::MODEL* model = create_geometry_env(I, zones);
	if (model != nullptr)
		Mixer::SwapZones(zones);
	xr_delete(swap_geometry_env(model));
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

static u32 Snd_GetTypeBus(const ref_sound& S)
{
	esound_type Type = S._p != nullptr ? S._p->s_type : st_Effect;
	return Type == st_Music ? Mixer::FindBus("music") : Type == st_Shooting ? Mixer::FindBus("shooting") : 0;
}

static u32 Snd_GetRouteBus(const ref_sound& S)
{
	return (S._p != nullptr && S._p->route_bus.size()) ? Mixer::FindBus(S._p->route_bus.c_str()) : 0;
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
	S._p->ignore_bus_shaders = from._p->ignore_bus_shaders;
	S._p->route_bus = from._p->route_bus;
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

	if (flags & sm_NoCarFactor)
	{
		MixedFlags |= (u32)Mixer::Flags::NoCarFactor;
	}

	if (flags & sm_NoOCC)
	{
		MixedFlags |= (u32)Mixer::Flags::NoOCC;
	}

	if ((flags & sm_Intro))
	{
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

	if (S.ignores_bus_shaders())
	{
		MixedFlags |= (u32)Mixer::Flags::NoBusShaders;
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

	Mixer::Play(S.slot(), mixer_flags, &S, delay, Snd_GetTypeBus(S), Snd_GetRouteBus(S));
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
	Mixer::PlayNoFeedback(mixer_flags, &S, O, delay, freq, vol, range_ptr, pos, Snd_GetTypeBus(S), Snd_GetRouteBus(S));
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
	Mixer::Play(S.slot(), mixer_flags, &S, delay, Snd_GetTypeBus(S), Snd_GetRouteBus(S));
	S._p->fTimeTotal = Mixer::GetDuration(S.slot());
	S._p->g_type = (S._p->g_type == sg_SourceType) ? XRay::Sound::Mixer::GetGameType(S.slot()) : S._p->g_type;
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

void CSoundRender_Core::SwitchAudioDevice(const xr_string& Name)
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
		params.settings.room_hf = s_user_environment.RoomHF;
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
	Mixer::ReloadSources();
}

bool CSoundRender_Core::object_in_audiozone(const Fvector& P)
{
	xrSRWLockGuard GeometryGuard(GeometryLock, true);
	if (geom_ENV)
	{
		Fvector dir = {0.0f, -1.0f, 0.0f};
		geom_DB.ray_options(CDB::OPT_ONLYNEAREST);
		geom_DB.ray_query(*geom_ENV, P, dir, 1000.0f);
		if (geom_DB.r_count())
		{
			auto& r = geom_DB.r_any();
			auto& T = r.model->get_tris()[r.tris_id];
			FvectorVec& V = geom_ENV->get_verts();
			Fvector tri_norm = zero_vel;
			tri_norm.mknormal(V[T.verts[0]], V[T.verts[1]], V[T.verts[2]]);
			float dot = dir.dotproduct(tri_norm);
			if (dot >= 0.0f)
			{
				return true;
			}
		}
	}
	return false;
}
