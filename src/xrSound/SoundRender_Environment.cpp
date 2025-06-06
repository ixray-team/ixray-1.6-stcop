#include "stdafx.h"


#include "SoundRender.h"
#include "SoundRender_Environment.h"

CSoundRender_Environment::CSoundRender_Environment()
{
	version			= sdef_env_version;
	set_default		();
}

CSoundRender_Environment::~CSoundRender_Environment()
{
}

void CSoundRender_Environment::set_default	()
{
    //Room = 0.f; //reverbs->flGain;
    //RoomHF = reverbs->flGainHF;
    //RoomLF = reverbs->flGainLF;
    //Density = reverbs->flDensity;
    //RoomRolloffFactor = reverbs->flRoomRolloffFactor;
    //DecayTime = reverbs->flDecayTime;
    //DecayHFRatio = reverbs->flDecayHFRatio;
    //DecayLFRatio = reverbs->flDecayLFRatio;
    //Reflections = reverbs->flReflectionsGain;
    //ReflectionsDelay = reverbs->flReflectionsDelay;
    //ReflectionsPan[0] = *reverbs->flReflectionsPan;
    //Reverb = reverbs->flLateReverbGain;
    //ReverbDelay = reverbs->flLateReverbDelay;
    //EnvironmentDiffusion = reverbs->flDiffusion;
    //AirAbsorptionHF = reverbs->flAirAbsorptionGainHF;
    //DecayHFLimit = reverbs->iDecayHFLimit;
    //EchoTime = reverbs->flEchoTime;
    //EchoDepth = reverbs->flEchoDepth;
    //ReverbPan[0] = *reverbs->flLateReverbPan;
    //ModulationTime = reverbs->flModulationTime;
    //ModulationDepth = reverbs->flModulationDepth;
    //HFReference = reverbs->flHFReference;
    //LFReference = reverbs->flLFReference;
}

void CSoundRender_Environment::set_identity	()
{
	set_default				();
}

void CSoundRender_Environment::lerp			(CSoundRender_Environment& A, CSoundRender_Environment& B, float f)
{
	float	fi				= 1.f-f;

    Room                    = fi*A.Room                	+ f*B.Room;                
    RoomHF                  = fi*A.RoomHF              	+ f*B.RoomHF;              
    RoomRolloffFactor       = fi*A.RoomRolloffFactor   	+ f*B.RoomRolloffFactor;
    DecayTime               = fi*A.DecayTime           	+ f*B.DecayTime;           
    DecayHFRatio            = fi*A.DecayHFRatio        	+ f*B.DecayHFRatio;        
    Reflections             = fi*A.Reflections         	+ f*B.Reflections;         
    ReflectionsDelay        = fi*A.ReflectionsDelay    	+ f*B.ReflectionsDelay;    
    Reverb                  = fi*A.Reverb              	+ f*B.Reverb;              
    ReverbDelay             = fi*A.ReverbDelay         	+ f*B.ReverbDelay;         
    EnvironmentSize         = fi*A.EnvironmentSize     	+ f*B.EnvironmentSize;     
    EnvironmentDiffusion    = fi*A.EnvironmentDiffusion	+ f*B.EnvironmentDiffusion;
    AirAbsorptionHF         = fi*A.AirAbsorptionHF     	+ f*B.AirAbsorptionHF;     
    DecayLFRatio = fi * A.DecayLFRatio + f * B.DecayLFRatio;
    ModulationTime = fi * A.ModulationTime + f * B.ModulationTime;
    ModulationDepth = fi * A.ModulationDepth + f * B.ModulationDepth;
    Density = fi * A.Density + f * B.Density;
    HFReference = fi * A.HFReference + f * B.HFReference;
    LFReference = fi * A.LFReference + f * B.LFReference;
    EchoTime = fi * A.EchoTime + f * B.EchoTime;
    EchoDepth = fi * A.EchoDepth + f * B.EchoDepth;
}

bool CSoundRender_Environment::load(IReader* fs)
{
    version = fs->r_u32();

    fs->r_stringZ(name);

    auto mB_to_gain = [](float mb) 
    { 
        return powf(10.0f, mb / 2000.0f); 
    };

    if (version <= SNDENV_VER_COP)
    {
        Room = mB_to_gain(fs->r_float());
        RoomHF = mB_to_gain(fs->r_float());
    }
    else
    {
        Room = fs->r_float();
        RoomHF = fs->r_float();
    }

    RoomRolloffFactor = fs->r_float();
    DecayTime = fs->r_float();
    DecayHFRatio = fs->r_float();
    Reflections = fs->r_float();
    ReflectionsDelay = fs->r_float();

    if (version <= SNDENV_VER_COP)
    {
        Reverb = mB_to_gain(fs->r_float());
    }
    else
    {
        Reverb = fs->r_float();
    }

    ReverbDelay = fs->r_float();
    EnvironmentSize = fs->r_float();
    EnvironmentDiffusion = fs->r_float();

    if (version <= SNDENV_VER_COP)
    {
        AirAbsorptionHF = mB_to_gain(fs->r_float());
    }
    else
    {
        AirAbsorptionHF = fs->r_float();
    }

    Environment = fs->r_u32();

    if (version == SNDENV_VER_IXR)
    {
        RoomLF = fs->r_float();
        ReflectionsPan[0] = fs->r_float();
        ReverbPan[0] = fs->r_float();

        DecayHFLimit = fs->r_u32();
        EchoTime = fs->r_float();
        EchoDepth = fs->r_float();
        ReverbDelay = fs->r_float();
        DecayLFRatio = fs->r_float();
        ModulationTime = fs->r_float();
        ModulationDepth = fs->r_float();
        HFReference = fs->r_float();
        LFReference = fs->r_float();
        Density = fs->r_float();
    }

    return true;
}

void CSoundRender_Environment::save	(IWriter* fs)
{
	fs->w_u32 	                    (sdef_env_version);
	fs->w_stringZ                   (name);

    fs->w_float	                    (Room                );
    fs->w_float	                    (RoomHF              );
    fs->w_float	                    (RoomRolloffFactor   );
    fs->w_float	                    (DecayTime           );
    fs->w_float	                    (DecayHFRatio        );
    fs->w_float	                    (Reflections         );
    fs->w_float	                    (ReflectionsDelay    );
    fs->w_float	                    (Reverb              );
    fs->w_float	                    (ReverbDelay         );
    fs->w_float	                    (EnvironmentSize     );
    fs->w_float	                    (EnvironmentDiffusion);
    fs->w_float	                    (AirAbsorptionHF     );
	fs->w_u32						(Environment		 );
}

//////////////////////////////////////////////////////////////////////////
void	SoundEnvironment_LIB::Load	(const char* name)
{
	R_ASSERT			(library.empty());
	IReader* F			= FS.r_open(name);
	IReader* C;
	library.reserve		(256);
	for (u32 chunk=0; 0!=(C=F->open_chunk(chunk)); chunk++)
	{
		CSoundRender_Environment*	E	= new CSoundRender_Environment	();
		if (E->load(C))	library.push_back(E);
        C->close		();
	}
	FS.r_close			(F);
}
bool	SoundEnvironment_LIB::Save	(const char* name)
{
	IWriter* F			= FS.w_open(name);
    if (F){
        for (u32 chunk=0; chunk<library.size(); chunk++)
        {
            F->open_chunk		(chunk);
            library[chunk]->save(F);
            F->close_chunk		();
        }
        FS.w_close		(F);
        return 			true;
    }   
    return 				false;
}
void	SoundEnvironment_LIB::Unload	()
{
	for (u32 chunk=0; chunk<library.size(); chunk++)
		xr_delete(library[chunk]);
	library.clear		();
}
int		SoundEnvironment_LIB::GetID		(const char* name)
{
	for (SE_IT it=library.begin(); it!=library.end(); it++)
		if (0==_stricmp(name,*(*it)->name)) return int(it-library.begin());
	return -1;
}
CSoundRender_Environment*	SoundEnvironment_LIB::Get		(const char* name)
{
	for (SE_IT it=library.begin(); it!=library.end(); it++)
		if (0==_stricmp(name,*(*it)->name)) return *it;
    return nullptr;
}
CSoundRender_Environment*	SoundEnvironment_LIB::Get		(int id)
{
	return library[id];
}
CSoundRender_Environment*	SoundEnvironment_LIB::Append	(CSoundRender_Environment* parent)
{
	library.push_back	(parent?new CSoundRender_Environment(*parent):new CSoundRender_Environment());
	return library.back	();
}
void						SoundEnvironment_LIB::Remove	(const char* name)
{
	for (SE_IT it=library.begin(); it!=library.end(); it++)
		if (0==_stricmp(name,*(*it)->name))
		{
			xr_delete		(*it);
			library.erase	(it);
			break;
		}
}
void						SoundEnvironment_LIB::Remove	(int id)
{
	xr_delete		(library[id]);
	library.erase	(library.begin()+id);
}
SoundEnvironment_LIB::SE_VEC& SoundEnvironment_LIB::Library	()	
{ 
	return library;
}
