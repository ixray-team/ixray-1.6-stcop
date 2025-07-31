#include "stdafx.h"
#pragma hdrstop

#include "Environment.h"
#include "xr_efflensflare.h"
#include "thunderbolt.h"
#include "Rain.h"

#include "IGame_Level.h"
#include "../xrServerEntities/object_broker.h"
#include "../xrServerEntities/LevelGameDef.h"
#include "../xrServerEntities/ShapeData.h"

void CEnvModifier::load	(IReader* fs, u32 version)
{
	use_flags.one					();
	fs->r_fvector3	(position);
	radius			= fs->r_float	();
	power			= fs->r_float	();
	far_plane		= fs->r_float	();
	fs->r_fvector3	(fog_color);
	fog_density		= fs->r_float	();
	fs->r_fvector3	(ambient);
	fs->r_fvector3	(sky_color);
	fs->r_fvector3	(hemi_color);

	if(version>=0x0016)
	{
		use_flags.assign(fs->r_u16());
	}

	if(version>=0x0018)
	{
		shape_type = fs->r_u8();
		Fvector R, S;
		fs->r_fvector3(R);
		fs->r_fvector3(S);

		obb.m_translate.set(position);
		obb.m_rotate.set(Fmatrix().setXYZ(R));
		obb.m_halfsize.set(S);
	}
	else
		shape_type = CShapeData::cfSphere;

}

float	CEnvModifier::sum	(CEnvModifier& M, Fvector3& view)
{
	float	_dist_sq;

	switch(M.shape_type)
	{
	case CShapeData::cfSphere:
	{
		_dist_sq	=	view.distance_to_sqr(M.position);
	}
	break;
	case CShapeData::cfBox:
	{
		Fmatrix xform; M.obb.xform_get(xform);
		Fvector n, p;
		// make 6 planes from OBB
		Fplane pl[6];

		pl[0].build_unit_normal(p.set( M.obb.m_halfsize.x, 0, 0), n.set( 1, 0, 0) ).transform(xform); // +X
		pl[1].build_unit_normal(p.set(-M.obb.m_halfsize.x, 0, 0), n.set(-1, 0, 0) ).transform(xform); // -X
		pl[2].build_unit_normal(p.set( 0, M.obb.m_halfsize.y, 0), n.set( 0, 1, 0) ).transform(xform); // +Y
		pl[3].build_unit_normal(p.set( 0,-M.obb.m_halfsize.y, 0), n.set( 0,-1, 0) ).transform(xform); // -Y
		pl[4].build_unit_normal(p.set( 0, 0, M.obb.m_halfsize.z), n.set( 0, 0, 1) ).transform(xform); // +Z
		pl[5].build_unit_normal(p.set( 0, 0,-M.obb.m_halfsize.z), n.set( 0, 0,-1) ).transform(xform); // -Z

		p.set(view);

		for(int i = 0; i < 6; i++)
			if(pl[i].classify(p) > 0.0f)
				pl[i].project(p, p);

		_dist_sq	=	view.distance_to_sqr(p);
	}
	break;
	default:
		NODEFAULT;
	}

	if (_dist_sq>=(M.radius*M.radius))
		return			0;

	float	_att		=	1-_sqrt(_dist_sq)/M.radius;	//[0..1];
	float	_power		=	M.power*_att;
	

	if(M.use_flags.test(eViewDist))
	{
		far_plane			+=	M.far_plane*_power;
		use_flags.set		(eViewDist, TRUE);
	}
	if(M.use_flags.test(eFogColor))
	{
		fog_color.mad		(M.fog_color,_power);
		use_flags.set		(eFogColor, TRUE);
	}
	if(M.use_flags.test(eFogDensity))
	{
		fog_density			+=	M.fog_density*_power;
		use_flags.set		(eFogDensity, TRUE);
	}

	if(M.use_flags.test(eAmbientColor))
	{
		ambient.mad			(M.ambient,_power);
		use_flags.set		(eAmbientColor, TRUE);
	}

	if(M.use_flags.test(eSkyColor))
	{
		sky_color.mad		(M.sky_color,_power);
		use_flags.set		(eSkyColor, TRUE);
	}

	if(M.use_flags.test(eHemiColor))
	{
		hemi_color.mad		(M.hemi_color,_power);
		use_flags.set		(eHemiColor, TRUE);
	}
	
	return				_power;
}

//-----------------------------------------------------------------------------
// Environment ambient
//-----------------------------------------------------------------------------
void CEnvAmbient::SSndChannel::load(CInifile& config, LPCSTR sect, pcstr sectionToReadFrom)
{
	m_load_section = sectionToReadFrom ? sectionToReadFrom : sect;

    if (config.read_if_exists(m_sound_dist, m_load_section, "sound_dist"))
    {
        if (m_sound_dist.x > m_sound_dist.y)
            std::swap(m_sound_dist.x, m_sound_dist.y);
        config.read_if_exists(m_sound_dist.x, m_load_section, "min_distance");
        config.read_if_exists(m_sound_dist.y, m_load_section, "max_distance");
    }
    else
    {
        m_sound_dist.x = config.r_float(m_load_section, "min_distance");
        m_sound_dist.y = config.r_float(m_load_section, "max_distance");
        R_ASSERT2(m_sound_dist.y > m_sound_dist.x, sect);
    }

    Ivector2 staticPeriod;
    Ivector4 period;
    if (config.try_read_if_exists(period, m_load_section, "sound_period")) // Pre Clear Sky
    {
        config.read_if_exists(period.x, m_load_section, "period0");
        config.read_if_exists(period.y, m_load_section, "period1");
        config.read_if_exists(period.z, m_load_section, "period2");
        config.read_if_exists(period.w, m_load_section, "period3");
        m_sound_period.set(period.mul(1000.f));
    }
    else if (config.read_if_exists(staticPeriod, m_load_section, "sound_period")) // SOC
    {
        period = { staticPeriod.x, staticPeriod.y, staticPeriod.x, staticPeriod.y };
        config.read_if_exists(period.x, m_load_section, "period0");
        config.read_if_exists(period.y, m_load_section, "period1");
        config.read_if_exists(period.z, m_load_section, "period2");
        config.read_if_exists(period.w, m_load_section, "period3");
        m_sound_period.set(period.mul(1000.f));
    }
    else // COP
    {
        m_sound_period.x = config.r_s32(m_load_section, "period0");
        m_sound_period.y = config.r_s32(m_load_section, "period1");
        m_sound_period.z = config.r_s32(m_load_section, "period2");
        m_sound_period.w = config.r_s32(m_load_section, "period3");
    }

	R_ASSERT				(m_sound_period.x <= m_sound_period.y && m_sound_period.z <= m_sound_period.w);

    pcstr snds = config.r_string(m_load_section, "sounds");
    const int cnt = _GetItemCount(snds);
    string_path tmp;
    R_ASSERT3(cnt, "sounds empty", m_load_section.c_str());

	m_sounds.resize			(cnt);

	for (size_t k=0; k<cnt; ++k)
	{
		_GetItem			(snds,k,tmp);
		m_sounds[k].create	(tmp,st_Effect,sg_SourceType);
	}
}

CEnvAmbient::SEffect* CEnvAmbient::create_effect	(CInifile& config, LPCSTR id)
{
	SEffect*					result = new SEffect();
	result->life_time			= iFloor(config.r_float(id,"life_time")*1000.f);
	result->particles			= config.r_string	(id,"particles");		
	VERIFY						(result->particles.size());
	result->offset				= config.r_fvector3	(id,"offset");
	result->wind_gust_factor	= config.r_float(id,"wind_gust_factor");
	
	if (config.line_exist(id,"sound"))
		result->sound.create	(config.r_string(id,"sound"),st_Effect,sg_SourceType);

	if (config.line_exist(id,"wind_blast_strength")) {
		result->wind_blast_strength		= config.r_float(id,"wind_blast_strength");
		result->wind_blast_direction.setHP	(deg2rad(config.r_float(id,"wind_blast_longitude")), 0.f);
		result->wind_blast_in_time		= config.r_float(id,"wind_blast_in_time");
		result->wind_blast_out_time		= config.r_float(id,"wind_blast_out_time");
		return							(result);
	}

	result->wind_blast_strength			= 0.f;
	result->wind_blast_direction.set	(0.f, 0.f, 1.f);
	result->wind_blast_in_time			= 0.f;
	result->wind_blast_out_time			= 0.f;

	return								(result);
}

CEnvAmbient::SSndChannel* CEnvAmbient::create_sound_channel	(CInifile& config, LPCSTR id, pcstr sectionToReadFrom)
{
	SSndChannel*			result = new SSndChannel();
	result->load			(config, id, sectionToReadFrom);
	return					(result);
}

CEnvAmbient::~CEnvAmbient						()
{
	destroy					();
}

void CEnvAmbient::destroy ()
{
	delete_data				(m_effects);
	delete_data				(m_sound_channels);
}

void CEnvAmbient::load( 
		CInifile& ambients_config,
		CInifile& sound_channels_config,
		CInifile& effects_config,
		const shared_str& sect
	)
{
	m_ambients_config_filename = ambients_config.fname();
	m_load_section		= sect;
	string_path			tmp;
	
	// sounds
    pcstr channels = nullptr;
	bool overrideReadingSection = false;
	if (ambients_config.line_exist(sect, "sounds"))
	{
		channels = ambients_config.r_string(sect, "sounds");
		overrideReadingSection = true;
	}
    if (ambients_config.line_exist(sect, "snd_channels"))
    {
        channels = ambients_config.r_string(sect, "snd_channels");
		overrideReadingSection = false;
	}
    if (ambients_config.line_exist(sect, "sound_channels")) // higher priority
    {
        channels = ambients_config.r_string(sect, "sound_channels");
		overrideReadingSection = false;
	}

	// SOC has no separate sound channels in env ambient section
	// Env ambient IS the sound channel in SOC
	size_t cnt = overrideReadingSection ? 1 : _GetItemCount(channels);
	m_sound_channels.resize	(cnt);

	for (size_t i=0; i<cnt; ++i)
        m_sound_channels[i] = create_sound_channel(sound_channels_config, _GetItem(channels, i, tmp),
            overrideReadingSection ? m_load_section.c_str() : nullptr);

	// effects
    Fvector2 period;
    if (ambients_config.read_if_exists(period, sect, "effect_period"))
    {
        ambients_config.read_if_exists(period.x, sect, "min_effect_period");
        ambients_config.read_if_exists(period.y, sect, "max_effect_period");
    }
    else
    {
        period.x = ambients_config.r_float(sect, "min_effect_period");
        period.y = ambients_config.r_float(sect, "max_effect_period");
    }
    m_effect_period.set(iFloor(period.x * 1000.f), iFloor(period.y * 1000.f));

	if (ambients_config.line_exist(sect, "effects"))
	{
		LPCSTR effs = ambients_config.r_string(sect, "effects");
		cnt = _GetItemCount(effs);
		//	R_ASSERT3				(cnt,"effects empty", sect.c_str());

		m_effects.resize(cnt);
		for (size_t k = 0; k < cnt; ++k)
			m_effects[k] = create_effect(effects_config, _GetItem(effs, k, tmp));
	}

	R_ASSERT					(!m_sound_channels.empty() || !m_effects.empty());
}

//-----------------------------------------------------------------------------
// Environment descriptor
//-----------------------------------------------------------------------------
CEnvDescriptor::CEnvDescriptor	(shared_str const& identifier) :
	m_identifier		(identifier)
{
	old_style			= false;

	exec_time			= 0.0f;
	exec_time_loaded	= 0.0f;
	
	clouds_color.set	(1,1,1,1);
	sky_color.set		(1,1,1);
	sky_rotation		= 0.0f;

	far_plane			= 400.0f;;

	fog_color.set		(1,1,1);
	fog_density			= 0.0f;
	fog_distance		= 400.0f;

	rain_type			= "default";

	rain_density		= 0.0f;
	rain_color.set		(0,0,0);

	rain_angle			= 0.0f;
	rain_length			= 5.0f;
	rain_width			= 0.3f;
	rain_speed_min		= 40.0f;
	rain_speed_max		= 80.0f;
	rain_angle_rotation = 0.f;

	bolt_period			= 0.0f;
	bolt_duration		= 0.0f;

    wind_velocity		= 0.0f;
    wind_direction		= 0.0f;
    
	ambient.set			(0,0,0);
	hemi_color.set		(1,1,1,1);
	sun_color.set		(1,1,1);
	sun_dir.set			(0,-1,0);

	m_fSunShaftsIntensity = 0;
	m_fWaterIntensity = 1;

    lens_flare_id		= "";
	tb_id				= "";
    
	env_ambient			= nullptr;

	trees_amplitude = 0.005f;
	trees_speed = 1.00f;
	trees_rotation = 10.0f;
	trees_wave.set(.1f, .01f, .11f);
}

#define	C_CHECK(C)	if (C.x<0 || C.x>2 || C.y<0 || C.y>2 || C.z<0 || C.z>2)	{ Msg("! Invalid '%s' in env-section '%s'",#C,identifier);}
void CEnvDescriptor::load	(CEnvironment& environment, CInifile& config, pcstr section)
{
	pcstr identifier = m_identifier.c_str();
	bool oldStyle = false;
	if (section)
	{
		identifier = section;
		oldStyle = true;
	}

	Ivector3 tm				={0,0,0};
    const int result = sscanf(m_identifier.c_str(), "%d:%d:%d", &tm.x, &tm.y, &tm.z);
    //R_ASSERT3(result == 3 && (tm.x >= 0) && (tm.x < 24) && (tm.y >= 0) && (tm.y < 60) && (tm.z >= 0) && (tm.z < 60),
    //    "Incorrect weather time", m_identifier.c_str());
	exec_time				= tm.x*3600.f+tm.y*60.f+tm.z;
	exec_time_loaded		= exec_time;
	string_path				st,st_env;
	xr_strcpy				(st,config.r_string	(identifier,"sky_texture"));
	xr_strconcat(st_env,st,"#small"		);
	sky_texture_name		= st;
	sky_texture_env_name	= st_env;
	clouds_texture_name		= config.r_string	(identifier,"clouds_texture");
	LPCSTR	cldclr			= config.r_string	(identifier,"clouds_color");
	float	multiplier		= 0, save=0;
	sscanf					(cldclr,"%f,%f,%f,%f,%f",&clouds_color.x,&clouds_color.y,&clouds_color.z,&clouds_color.w,&multiplier);
	save=clouds_color.w;	clouds_color.mul		(.5f*multiplier);		
	clouds_color.w			= save; 
	
	sky_color				= config.r_fvector3	(identifier,"sky_color");
	
	if (config.line_exist(identifier,"sky_rotation"))	sky_rotation	= deg2rad(config.r_float(identifier,"sky_rotation"));
	else											sky_rotation	= 0;
	far_plane				= config.r_float	(identifier,"far_plane");
	fog_color				= config.r_fvector3	(identifier,"fog_color");
	fog_density				= config.r_float	(identifier,"fog_density");
	fog_distance			= config.r_float	(identifier,"fog_distance");

	rain_type				= 
		config.line_exist(identifier, "rain_type") ? config.r_string(identifier, "rain_type") : "default";

	rain_density			= config.r_float	(identifier,"rain_density");		clamp(rain_density,0.f,1.f);
	rain_color				= config.r_fvector3	(identifier,"rain_color");

	rain_angle = config.line_exist(identifier, "rain_angle") ? config.r_float(identifier, "rain_angle") : 0.0f;
	clampr(-30.0f, 30.0f, rain_angle);

	rain_length = config.line_exist(identifier, "rain_length") ? config.r_float(identifier, "rain_length") : 5.0f;
	rain_width = config.line_exist(identifier, "rain_width") ? config.r_float(identifier, "rain_width") : 0.3f;

	rain_speed_min = config.line_exist(identifier, "rain_speed_min") ? config.r_float(identifier, "rain_speed_min") : 40.0f;
	rain_speed_max = config.line_exist(identifier, "rain_speed_max") ? config.r_float(identifier, "rain_speed_max") : 80.0f;

	rain_angle_rotation = deg2rad(config.line_exist(identifier, "rain_angle_rotation") ? config.r_float(identifier, "rain_angle_rotation") : 0.0f);
	clampr(0.0f, 360.f, rain_angle_rotation);

	wind_velocity			= config.r_float	(identifier,"wind_velocity");
	wind_direction			= deg2rad(config.r_float(identifier,"wind_direction"));

	if (config.line_exist(identifier, "hemisphere_color"))
	{
		hemi_color = config.r_fvector4(identifier, "hemisphere_color");
	}
	else
	{
		hemi_color = config.r_fvector4(identifier, "hemi_color");
	}
	sun_color				= config.r_fvector3	(identifier,"sun_color");

	if (oldStyle)
	{
		ambient = pSettings->r_fvector3(identifier, "ambient");

		if (config.line_exist(identifier, "env_ambient"))
			env_ambient = environment.AppendEnvAmb(config.r_string(identifier, "env_ambient"), pSettings);
	}
	else
	{
		ambient = config.r_fvector3(identifier, "ambient_color");
		if (config.line_exist(identifier, "ambient"))
			env_ambient = environment.AppendEnvAmb(config.r_string(identifier, "ambient"));
	}

	if (config.line_exist(identifier, "tree_amplitude_intensity"))
		trees_amplitude = config.r_float(identifier, "tree_amplitude_intensity");

	
	float sun_altitude = 0.f;
	float sun_longitude = 0.f;
	if (config.line_exist(identifier, "sun_altitude"))
	{
		sun_altitude = config.r_float(identifier, "sun_altitude");
		sun_longitude = config.r_float(identifier, "sun_longitude");
		sun_dir.setHP(
			deg2rad(sun_altitude),
			deg2rad(sun_longitude)
		);
		R_ASSERT(_valid(sun_dir));
	}
	else
		sun_dir.setHP			(
			deg2rad(config.r_fvector2(identifier,"sun_dir").y),
			deg2rad(config.r_fvector2(identifier,"sun_dir").x)
		);

	if (sun_dir.y >= 0)
	{
		// Автофикс: если y >= 0, делаем его немного отрицательным
		sun_dir.y = -fabs(sun_dir.y) - FLT_EPSILON;
		sun_dir.normalize();
	}

	VERIFY2(sun_dir.y < 0,
		make_string<const char*>("Invalid sun direction settings while loading\n"
			"Config: %s\n"
			"Time: %s\n"
			"sun_altitude = %.6f (deg), sun_longitude = %.6f (deg)\n"
			"Computed sun_dir = (%.6f, %.6f, %.6f)",
			config.fname(),
			identifier,  // Текущее время, зависит от движка
			sun_altitude, sun_longitude,
			sun_dir.x, sun_dir.y, sun_dir.z));
	
    if (oldStyle)
    {
        lens_flare_id = environment.eff_LensFlare->AppendDef(environment, pSettings,
            config.r_string(section, "flares"));
        tb_id = environment.eff_Thunderbolt->AppendDef(environment, pSettings,
            pSettings, config.r_string(section, "thunderbolt"));
    }
    else
    {
        lens_flare_id = environment.eff_LensFlare->AppendDef(
            environment, environment.m_suns_config, config.r_string(identifier, "sun"));
        tb_id = environment.eff_Thunderbolt->AppendDef(environment, environment.m_thunderbolt_collections_config,
            environment.m_thunderbolts_config, config.r_string(identifier, "thunderbolt_collection"));
    }

	if (tb_id.size())
	{
		if (config.read_if_exists(bolt_period, identifier, "bolt_period"))
			bolt_period = config.r_float(identifier, "bolt_period");
		else
			bolt_period = config.r_float(identifier, "thunderbolt_period");

		if (config.read_if_exists(bolt_duration, identifier, "bolt_duration"))
			bolt_duration = config.r_float(identifier, "bolt_duration");
		else
			bolt_duration = config.r_float(identifier, "thunderbolt_duration");
	}

	if (config.line_exist(identifier,"sun_shafts_intensity"))
		m_fSunShaftsIntensity = config.r_float(identifier,"sun_shafts_intensity");

	if (config.line_exist(identifier,"water_intensity"))
		m_fWaterIntensity = config.r_float(identifier,"water_intensity");

	C_CHECK					(clouds_color);
	C_CHECK					(sky_color	);
	C_CHECK					(fog_color	);
	C_CHECK					(rain_color	);
	C_CHECK					(ambient	);
	C_CHECK					(hemi_color	);
	C_CHECK					(sun_color	);
}

void CEnvDescriptor::on_device_create	()
{
	m_pDescriptor->OnDeviceCreate(*this);
	/*
	if (sky_texture_name.size())	
		sky_texture.create		(sky_texture_name.c_str());

	if (sky_texture_env_name.size())
		sky_texture_env.create	(sky_texture_env_name.c_str());

	if (clouds_texture_name.size())	
		clouds_texture.create	(clouds_texture_name.c_str());
		*/
}

void CEnvDescriptor::on_device_destroy	()
{
	m_pDescriptor->OnDeviceDestroy();
	/*
	sky_texture.destroy		();
	sky_texture_env.destroy	();
	clouds_texture.destroy	();
	*/
}

//-----------------------------------------------------------------------------
// Environment Mixer
//-----------------------------------------------------------------------------
CEnvDescriptorMixer::CEnvDescriptorMixer(shared_str const& identifier) :
	CEnvDescriptor	(identifier)
{
}

void CEnvDescriptorMixer::destroy()
{
	m_pDescriptorMixer->Destroy();
	/*
	sky_r_textures.clear		();
	sky_r_textures_env.clear	();
	clouds_r_textures.clear		();
	*/

	//	Reuse existing code
	on_device_destroy();
/*
	sky_texture.destroy			();
	sky_texture_env.destroy		();
	clouds_texture.destroy		();
	*/
}

void CEnvDescriptorMixer::clear	()
{
	m_pDescriptorMixer->Clear();
	/*
	std::pair<u32,ref_texture>	zero = std::make_pair(u32(0),ref_texture(0));
	sky_r_textures.clear		();
	sky_r_textures.push_back	(zero);
	sky_r_textures.push_back	(zero);
	sky_r_textures.push_back	(zero);

	sky_r_textures_env.clear	();
	sky_r_textures_env.push_back(zero);
	sky_r_textures_env.push_back(zero);
	sky_r_textures_env.push_back(zero);

	clouds_r_textures.clear		();
	clouds_r_textures.push_back	(zero);
	clouds_r_textures.push_back	(zero);
	clouds_r_textures.push_back	(zero);
	*/
}

constexpr float NO_RAIN_TO_RAINY_TIME_OFFSET = 0.4f;

void CEnvDescriptorMixer::lerp	(CEnvironment* Env, CEnvDescriptor& A, CEnvDescriptor& B, float f, CEnvModifier& Mdf, float modifier_power)
{
	float	modif_power		=	1.f/(modifier_power+1);	// the environment itself
	float	fi				=	1-f;

	// XXX: it would be nice to lerp this too.
	old_style = A.old_style;

	m_pDescriptorMixer->lerp(&*A.m_pDescriptor, &*B.m_pDescriptor);
	/*
	sky_r_textures.clear		();
	sky_r_textures.push_back	(std::make_pair(0,A.sky_texture));
	sky_r_textures.push_back	(std::make_pair(1,B.sky_texture));

	sky_r_textures_env.clear	();

	sky_r_textures_env.push_back(std::make_pair(0,A.sky_texture_env));
	sky_r_textures_env.push_back(std::make_pair(1,B.sky_texture_env));

	clouds_r_textures.clear		();
	clouds_r_textures.push_back	(std::make_pair(0,A.clouds_texture));
	clouds_r_textures.push_back	(std::make_pair(1,B.clouds_texture));
	*/

	weight					=	f;

	// rain specific time weght, for making smoother rain and thunder
	float rain_f = f;
	{
		if (A.rain_density <= 0.01f) // if the previous desc did not have the rain at all - make a
			// delayed rain begining
			if (f < NO_RAIN_TO_RAINY_TIME_OFFSET) // dont start rain and rain effects, untill we are
				// close to the half of the cycle time
				rain_f = 0.f;
			else // if we are close to the half of the cycle time - start lerping rain
				rain_f = (f - NO_RAIN_TO_RAINY_TIME_OFFSET) /
				(1.f -
					NO_RAIN_TO_RAINY_TIME_OFFSET); // get the 0.0 - 1.0 value from offseted
		// weight for the rest of the time, when we
		// need the rain and rain effects enabled

		else if (B.rain_density <= 0.01f) // if next weather is not rainy at all - then DO rain only
			// untill half of the cycle time is past
			if (f <
				1.f - NO_RAIN_TO_RAINY_TIME_OFFSET) // dont start rain and rain effects, untill we
				// are at least close to the half of the cycle
				rain_f =
				f /
				(1.f -
					NO_RAIN_TO_RAINY_TIME_OFFSET); // get the 0.0 - 1.0 value from offseted weight
			else // if we passed half of the time - stop rain and effects
				rain_f = 1.f;
	}

	float rain_fi = 1.f - rain_f;

	R_ASSERT2(rain_f >= 0.f && rain_f <= 1.f, make_string<const char*>("rain_f = %f", rain_f));

	clouds_color.lerp		(A.clouds_color,B.clouds_color,f);

	sky_rotation			=	(fi*A.sky_rotation + f*B.sky_rotation);

//.	far_plane				=	(fi*A.far_plane + f*B.far_plane + Mdf.far_plane)*psVisDistance*modif_power;
	if(Mdf.use_flags.test(eViewDist))
		far_plane				=	(fi*A.far_plane + f*B.far_plane + Mdf.far_plane)*psVisDistance*modif_power;
	else
		far_plane				=	(fi*A.far_plane + f*B.far_plane)*psVisDistance;
	
//.	fog_color.lerp			(A.fog_color,B.fog_color,f).add(Mdf.fog_color).mul(modif_power);
	fog_color.lerp			(A.fog_color,B.fog_color,f);
	if(Mdf.use_flags.test(eFogColor))
		fog_color.add(Mdf.fog_color).mul(modif_power);

//.	fog_density				=	(fi*A.fog_density + f*B.fog_density + Mdf.fog_density)*modif_power;
	fog_density				=	(fi*A.fog_density + f*B.fog_density);
	if(Mdf.use_flags.test(eFogDensity))
	{
		fog_density			+= Mdf.fog_density;
		fog_density			*= modif_power;
	}

	fog_distance			=	(fi*A.fog_distance + f*B.fog_distance) * psVisDistance;
	fog_near				=	(1.0f - fog_density)*0.85f * fog_distance;
	fog_far					=	0.99f * fog_distance;
	
	rain_density = rain_fi * A.rain_density + rain_f * B.rain_density;
	rain_color.lerp(A.rain_color, B.rain_color, rain_f);

	rain_angle = fi * A.rain_angle + f * B.rain_angle;
	rain_length = fi * A.rain_length + f * B.rain_length;
	rain_width = fi * A.rain_width + f * B.rain_width;

	rain_speed_min = fi * A.rain_speed_min + f * B.rain_speed_min;
	rain_speed_max = fi * A.rain_speed_max + f * B.rain_speed_max;

	rain_angle_rotation = fi * A.rain_angle_rotation + f * B.rain_angle_rotation;

	bolt_period = rain_fi * A.bolt_period + rain_f * B.bolt_period;
	bolt_duration = rain_fi * A.bolt_duration + rain_f * B.bolt_duration;

	// wind
	wind_velocity			=	fi*A.wind_velocity + f*B.wind_velocity;
	wind_direction			=	fi*A.wind_direction + f*B.wind_direction;

	m_fSunShaftsIntensity	=	fi*A.m_fSunShaftsIntensity + f*B.m_fSunShaftsIntensity;
	m_fWaterIntensity		=	fi*A.m_fWaterIntensity + f*B.m_fWaterIntensity;

	trees_amplitude = fi * A.trees_amplitude + f * B.trees_amplitude;

	// colors
//.	sky_color.lerp			(A.sky_color,B.sky_color,f).add(Mdf.sky_color).mul(modif_power);
	sky_color.lerp			(A.sky_color,B.sky_color,f);
	if(Mdf.use_flags.test(eSkyColor))
		sky_color.add(Mdf.sky_color).mul(modif_power);

//.	ambient.lerp			(A.ambient,B.ambient,f).add(Mdf.ambient).mul(modif_power);
	ambient.lerp			(A.ambient,B.ambient,f);
	if(Mdf.use_flags.test(eAmbientColor))
		ambient.add(Mdf.ambient).mul(modif_power);

	hemi_color.lerp			(A.hemi_color,B.hemi_color,f);

	if(Mdf.use_flags.test(eHemiColor))
	{
		hemi_color.x			+= Mdf.hemi_color.x;
		hemi_color.y			+= Mdf.hemi_color.y; 
		hemi_color.z			+= Mdf.hemi_color.z;
		hemi_color.x			*= modif_power;
		hemi_color.y			*= modif_power;
		hemi_color.z			*= modif_power;
	}

	sun_color.lerp			(A.sun_color,B.sun_color,f);

	if (rain_density > 0.f) {
		Env->wetness_factor += (rain_density * 4.0) / 10000.f;
	} else {
		Env->wetness_factor -= 0.0001f * 2.0;
	}

	clamp(Env->wetness_factor, 0.f, 1.f);

	R_ASSERT				( _valid(A.sun_dir) );
	R_ASSERT				( _valid(B.sun_dir) );
	sun_dir.lerp			(A.sun_dir,B.sun_dir,f).normalize();
	R_ASSERT				( _valid(sun_dir) );

	VERIFY2					(sun_dir.y<0,"Invalid sun direction settings while lerp");}

//-----------------------------------------------------------------------------
// Environment IO
//-----------------------------------------------------------------------------
CEnvAmbient* CEnvironment::AppendEnvAmb(const shared_str& sect, CInifile * pIni /*= nullptr*/)
{
    for (const auto& ambient : Ambients)
        if (ambient->name().equal(sect))
            return ambient;

    CEnvAmbient* ambient = Ambients.emplace_back(new CEnvAmbient());
    ambient->load(pIni ? *pIni : *m_ambients_config,
        pIni ? *pIni : *m_sound_channels_config,
        pIni ? *pIni : *m_effects_config, sect);
    return ambient;
}

void	CEnvironment::mods_load			()
{
	Modifiers.clear();
	string_path							path;
	if (FS.exist(path,"$level$","level.env_mod"))	
	{
		IReader*	fs	= FS.r_open		(path);
		u32			id	= 0;
		u32 ver		= 0x0015;
		u32 sz;

		while( 0!=(sz=fs->find_chunk(id)) )	
		{
			if(id==0 && sz==sizeof(u32))
			{
				ver				= fs->r_u32();
			}else
			{
				CEnvModifier		E;
				E.load				(fs, ver);
				Modifiers.push_back	(E);
			}
			id					++;
		}
		FS.r_close	(fs);
	}

	load_level_specific_ambients ();
}

void	CEnvironment::mods_unload		()
{
	Modifiers.clear();
}

CEnvModifier*	CEnvironment::new_modifier	()
{
	CEnvModifier em;
    em.shape_type = CShapeData::cfSphere;
	Modifiers.push_back(em);
    return &Modifiers.back();
}

void	CEnvironment::remove_modifier	(CEnvModifier *ptr)
{
	xr_list<CEnvModifier>::iterator it = Modifiers.begin(), end = Modifiers.end();
	for(; it != end; it++)
		if(&*it == ptr) {
			Modifiers.erase(it);
		return;
		}
}

void    CEnvironment::load_level_specific_ambients ()
{
	const shared_str level_name = g_pGameLevel->name();

	string_path path;
	xr_strconcat(path, "environment\\ambients\\", level_name.c_str(), ".ltx");

	string_path full_path;
	CInifile* level_ambients = new CInifile(
			FS.update_path(full_path, "$game_config$", path),
			TRUE,
			TRUE,
			FALSE);

	if (level_ambients->section_count() == 0)
	{
		xr_delete(level_ambients);
		return;
	}

    for (const auto& ambient : Ambients)
    {
        shared_str section_name = ambient->name();

        CInifile * sounds = m_sound_channels_config;
        CInifile * effects = m_effects_config;

        // choose a source ini file
        CInifile * source = nullptr;
        if (level_ambients && level_ambients->section_exist(section_name))
            source = level_ambients;
        else if (m_ambients_config && m_ambients_config->section_exist(section_name))
            source = m_ambients_config;
        else
        {
            source = pSettings;
            sounds = pSettings;
            effects = pSettings;
        }

        // check and reload if needed
        if (xr_strcmp(ambient->get_ambients_config_filename().c_str(), source->fname()))
        {
            ambient->destroy();
            ambient->load(*source, *sounds, *effects, section_name);
        }
    }

	xr_delete(level_ambients);
}

CEnvDescriptor* CEnvironment::create_descriptor	(shared_str const& identifier, CInifile* config, pcstr section)
{
	CEnvDescriptor*	result = new CEnvDescriptor(identifier);
	if (config)
		result->load(*this, *config, section);
	return			result;
}

void CEnvironment::load_weathers		()
{
    if (!WeatherCycles.empty())
        return;

    FS_FileSet weathers;
    FS.file_list(weathers, "$game_weathers$", FS_ListFiles, "*.ltx");

    // CoP style weather config
    xr_string id;
    for (const auto& file : weathers)
    {
        pcstr fileName = file.name.c_str();
        const size_t length = xr_strlen(fileName);
        id.assign(fileName, length - 4);
        EnvVec& env = WeatherCycles[id.c_str()];

        string_path file_path;
        FS.update_path(file_path, "$game_weathers$", fileName);
        CInifile* config = CInifile::Create(file_path);

        auto& sections = config->sections();
        env.reserve(sections.size());

        for (const auto& section : sections)
            env.emplace_back(create_descriptor(section->Name, config));

        CInifile::Destroy(config);
    }

    // SoC style weather config
    int weatherCount = 0;
    if (pSettings->section_exist("weathers"))
    {
        weatherCount = pSettings->line_count("weathers");
        Log("~ SoC style weather config detected");
    }

    for (int weatherIdx = 0; weatherIdx < weatherCount; ++weatherIdx)
    {
        pcstr weatherName, weatherSection;
        if (pSettings->r_line("weathers", weatherIdx, &weatherName, &weatherSection))
        {
            const int envCount = pSettings->line_count(weatherSection);

            EnvVec& env = WeatherCycles[weatherName];
            env.reserve(envCount);
            
            pcstr executionTime, envSection;
            for (int envIdx = 0; envIdx < envCount; ++envIdx)
            {
                if (pSettings->r_line(weatherSection, envIdx, &executionTime, &envSection))
                    env.emplace_back(create_descriptor(executionTime, pSettings, envSection));
            }
        }
    }

    R_ASSERT2(!WeatherCycles.empty(), "Empty weathers.");

    // sorting weather envs
    for (auto& cycle : WeatherCycles)
    {
        R_ASSERT3(cycle.second.size() > 1, "Environment in weather must >=2", cycle.first.c_str());
        std::sort(cycle.second.begin(), cycle.second.end(), sort_env_etl_pred);
    }

    SetWeather((*WeatherCycles.begin()).first.c_str());
}

void CEnvironment::load_weather_effects()
{
    if (!WeatherFXs.empty())
        return;

    FS_FileSet weathersEffects;
    FS.file_list(weathersEffects, "$game_weather_effects$", FS_ListFiles, "*.ltx");

    xr_string id;
    for (const auto& file : weathersEffects)
    {
        pcstr fileName = file.name.c_str();
        const size_t length = xr_strlen(fileName);
        id.assign(fileName, length - 4);
        EnvVec& env = WeatherFXs[id.c_str()];

        string_path file_name;
        FS.update_path(file_name, "$game_weather_effects$", fileName);
        CInifile* config = CInifile::Create(file_name);

        auto& sections = config->sections();
        env.reserve(sections.size() + 2);

        env.emplace_back(create_descriptor("00:00:00", nullptr));

        for (const auto& section : sections)
            env.emplace_back(create_descriptor(section->Name, config));

        CInifile::Destroy(config);

        env.emplace_back(create_descriptor("24:00:00", nullptr));
        env.back()->exec_time_loaded = DAY_LENGTH;
    }

    // SoC style weather effects config
    u32 weatherEffectsCount = 0;
    if (pSettings->section_exist("weather_effects"))
    {
        weatherEffectsCount = pSettings->line_count("weather_effects");
        Log("~ SoC style weather effects config detected");
    }

    for (u32 weatherIdx = 0; weatherIdx < weatherEffectsCount; ++weatherIdx)
    {
        pcstr weatherName, weatherSection, envSection;
        if (pSettings->r_line("weather_effects", weatherIdx, &weatherName, &weatherSection))
        {
            EnvVec& env = WeatherFXs[weatherName];
            env.emplace_back(create_descriptor("00:00:00", nullptr));

            const u32 envCount = pSettings->line_count(weatherSection);
            pcstr executionTime;
            for (u32 envIdx = 0; envIdx < envCount; ++envIdx)
            {
                if (pSettings->r_line(weatherSection, envIdx, &executionTime, &envSection))
                {
                    env.emplace_back(create_descriptor(executionTime, pSettings, envSection));
                    env.back()->old_style = true;
                }
            }

            env.emplace_back(create_descriptor("24:00:00", nullptr));
            env.back()->exec_time_loaded = DAY_LENGTH;
        }
    }
    // sorting weather envs
    for (auto& fx : WeatherFXs)
    {
        R_ASSERT3(fx.second.size() > 1, "Environment in weather must >=2", fx.first.c_str());
        std::sort(fx.second.begin(), fx.second.end(), sort_env_etl_pred);
    }
}

void CEnvironment::load		()
{
	if (!CurrentEnv)
		create_mixer		();

	m_pRender->OnLoad();
	//tonemap					= Device.Resources->_CreateTexture("$user$tonemap");	//. hack
	if (!eff_Rain)    		eff_Rain 		= new CEffect_Rain();
	if (!eff_LensFlare)		eff_LensFlare 	= new CLensFlare();
	if (!eff_Thunderbolt)	eff_Thunderbolt	= new CEffect_Thunderbolt();
	
	load_weathers			();
	load_weather_effects	();
}

void CEnvironment::unload	()
{
	EnvsMapIt _I,_E;
	// clear weathers
	_I		= WeatherCycles.begin();
	_E		= WeatherCycles.end();
	for (; _I!=_E; _I++){
		for (EnvIt it=_I->second.begin(); it!=_I->second.end(); it++)
			xr_delete	(*it);
	}

	WeatherCycles.clear		();
	// clear weather effect
	_I		= WeatherFXs.begin();
	_E		= WeatherFXs.end();
	for (; _I!=_E; _I++){
		for (EnvIt it=_I->second.begin(); it!=_I->second.end(); it++)
			xr_delete	(*it);
	}
	WeatherFXs.clear		();
	// clear ambient
	for (EnvAmbVecIt it=Ambients.begin(); it!=Ambients.end(); it++)
		xr_delete		(*it);
	Ambients.clear		();
	// misc
	xr_delete			(eff_Rain);
	xr_delete			(eff_LensFlare);
	xr_delete			(eff_Thunderbolt);
	CurrentWeather		= 0;
	CurrentWeatherName	= 0;
	CurrentEnv->clear	();
	Invalidate			();

	m_pRender->OnUnload	();
//	tonemap				= 0;
}
