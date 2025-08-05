//---------------------------------------------------------------------------

#include "stdafx.h"



#include "ui_shadermain.h"
#include "../../xrSound/stdafx.h"
#include "../../xrSound/SoundRender_Source.h"
#include "../xrECore/Editor/D3DUtils.h"

#include "SHSoundEnvTools.h"
//------------------------------------------------------------------------------

#define EAX_ENVIRONMENT_GENERIC 0
#define EAX_ENVIRONMENT_PADDEDCELL 1
#define EAX_ENVIRONMENT_ROOM 2
#define EAX_ENVIRONMENT_BATHROOM 3
#define EAX_ENVIRONMENT_LIVINGROOM 4
#define EAX_ENVIRONMENT_STONEROOM 5
#define EAX_ENVIRONMENT_AUDITORIUM 6
#define EAX_ENVIRONMENT_CONCERTHALL 7
#define EAX_ENVIRONMENT_CAVE 8
#define EAX_ENVIRONMENT_ARENA 9
#define EAX_ENVIRONMENT_HANGAR 10
#define EAX_ENVIRONMENT_CARPETEDHALLWAY 11
#define EAX_ENVIRONMENT_HALLWAY 12
#define EAX_ENVIRONMENT_STONECORRIDOR 13
#define EAX_ENVIRONMENT_ALLEY 14
#define EAX_ENVIRONMENT_FOREST 15
#define EAX_ENVIRONMENT_CITY 16
#define EAX_ENVIRONMENT_MOUNTAINS 17
#define EAX_ENVIRONMENT_QUARRY 18
#define EAX_ENVIRONMENT_PLAIN 19
#define EAX_ENVIRONMENT_PARKINGLOT 20
#define EAX_ENVIRONMENT_SEWERPIPE 21
#define EAX_ENVIRONMENT_UNDERWATER 22
#define EAX_ENVIRONMENT_DRUGGED 23
#define EAX_ENVIRONMENT_DIZZY 24
#define EAX_ENVIRONMENT_PSYCHOTIC 25
#define EAX_ENVIRONMENT_COUNT 26

xr_token eax_environment[] =
{
	{"Alley",               EAX_ENVIRONMENT_ALLEY			},                          
	{"Arena",               EAX_ENVIRONMENT_ARENA			},                          
	{"Auditorium",          EAX_ENVIRONMENT_AUDITORIUM		},                               
	{"Bathroom",            EAX_ENVIRONMENT_BATHROOM		},                             
	{"Carpeted Hallway",    EAX_ENVIRONMENT_CARPETEDHALLWAY	},                                     
	{"Cave",                EAX_ENVIRONMENT_CAVE			},                         
	{"City",                EAX_ENVIRONMENT_CITY			},                         
	{"Concert Hall",        EAX_ENVIRONMENT_CONCERTHALL		},                                 
	{"Dizzy",               EAX_ENVIRONMENT_DIZZY			},                          
	{"Drugged",             EAX_ENVIRONMENT_DRUGGED			},                            
	{"Forest",              EAX_ENVIRONMENT_FOREST			},                           
	{"Generic",			   	EAX_ENVIRONMENT_GENERIC			},
	{"Hallway",             EAX_ENVIRONMENT_HALLWAY			},                            
	{"Hangar",              EAX_ENVIRONMENT_HANGAR			},                           
	{"Livingroom",          EAX_ENVIRONMENT_LIVINGROOM		},                               
	{"Mountains",           EAX_ENVIRONMENT_MOUNTAINS		},                              
	{"Padded Cell",		    EAX_ENVIRONMENT_PADDEDCELL		},
	{"Parkinglot",          EAX_ENVIRONMENT_PARKINGLOT		},                               
	{"Plain",               EAX_ENVIRONMENT_PLAIN			},                          
	{"Psychotic",           EAX_ENVIRONMENT_PSYCHOTIC		},
	{"Quarry",              EAX_ENVIRONMENT_QUARRY			},                           
	{"Room",                EAX_ENVIRONMENT_ROOM			},
	{"Sewer Pipe",          EAX_ENVIRONMENT_SEWERPIPE		},                               
	{"Stone Corridor",      EAX_ENVIRONMENT_STONECORRIDOR	},                                   
	{"Stone Room",          EAX_ENVIRONMENT_STONEROOM		},                               
	{"Under Water",         EAX_ENVIRONMENT_UNDERWATER		},                                
	{0,						0								}
};

struct EAXID2EFXPRESET
{
	u32 ID;
	EFXEAXREVERBPROPERTIES Preset;
};

EAXID2EFXPRESET EFXPresetTable[] =
{
	{ EAX_ENVIRONMENT_ALLEY,			EFX_REVERB_PRESET_ALLEY },
	{ EAX_ENVIRONMENT_ARENA,			EFX_REVERB_PRESET_ARENA },
	{ EAX_ENVIRONMENT_AUDITORIUM,		EFX_REVERB_PRESET_AUDITORIUM },
	{ EAX_ENVIRONMENT_BATHROOM,			EFX_REVERB_PRESET_BATHROOM },
	{ EAX_ENVIRONMENT_CARPETEDHALLWAY,  EFX_REVERB_PRESET_CARPETEDHALLWAY },
	{ EAX_ENVIRONMENT_CAVE,				EFX_REVERB_PRESET_CAVE },
	{ EAX_ENVIRONMENT_CITY,				EFX_REVERB_PRESET_CITY },
	{ EAX_ENVIRONMENT_CONCERTHALL,		EFX_REVERB_PRESET_CONCERTHALL },
	{ EAX_ENVIRONMENT_DIZZY,			EFX_REVERB_PRESET_DIZZY },
	{ EAX_ENVIRONMENT_DRUGGED,			EFX_REVERB_PRESET_DRUGGED },
	{ EAX_ENVIRONMENT_FOREST,			EFX_REVERB_PRESET_FOREST },
	{ EAX_ENVIRONMENT_GENERIC,			EFX_REVERB_PRESET_GENERIC },
	{ EAX_ENVIRONMENT_HALLWAY,			EFX_REVERB_PRESET_HALLWAY },
	{ EAX_ENVIRONMENT_HANGAR,			EFX_REVERB_PRESET_HANGAR },
	{ EAX_ENVIRONMENT_LIVINGROOM,		EFX_REVERB_PRESET_LIVINGROOM },
	{ EAX_ENVIRONMENT_MOUNTAINS,		EFX_REVERB_PRESET_MOUNTAINS },
	{ EAX_ENVIRONMENT_PADDEDCELL,		EFX_REVERB_PRESET_PADDEDCELL },
	{ EAX_ENVIRONMENT_PARKINGLOT,		EFX_REVERB_PRESET_PARKINGLOT },
	{ EAX_ENVIRONMENT_PLAIN,			EFX_REVERB_PRESET_PLAIN },
	{ EAX_ENVIRONMENT_PSYCHOTIC,		EFX_REVERB_PRESET_PSYCHOTIC },
	{ EAX_ENVIRONMENT_QUARRY,			EFX_REVERB_PRESET_QUARRY },
	{ EAX_ENVIRONMENT_ROOM,				EFX_REVERB_PRESET_ROOM },
	{ EAX_ENVIRONMENT_SEWERPIPE,		EFX_REVERB_PRESET_SEWERPIPE },
	{ EAX_ENVIRONMENT_STONECORRIDOR,	EFX_REVERB_PRESET_STONECORRIDOR },
	{ EAX_ENVIRONMENT_STONEROOM,		EFX_REVERB_PRESET_STONEROOM },
	{ EAX_ENVIRONMENT_UNDERWATER,		EFX_REVERB_PRESET_UNDERWATER },
};

//------------------------------------------------------------------------------
CSHSoundEnvTools::CSHSoundEnvTools(const ISHInit& init)
	: ISHTools(init)
{
	m_Env = 0;
	m_SoundName = "alexmx\\beep";
	OnChangeWAV(0);
}

CSHSoundEnvTools::~CSHSoundEnvTools()
{
}
//---------------------------------------------------------------------------

void CSHSoundEnvTools::OnChangeWAV	(PropValue* prop)
{

	BOOL bPlay 		= !!m_PreviewSnd._feedback();
	m_PreviewSnd.destroy();
	if (m_SoundName.size()){
		m_PreviewSnd.create				(*m_SoundName,st_Effect,sg_Undefined);
		CSoundRender_Source* src= (CSoundRender_Source*)m_PreviewSnd._handle();
		m_Params.min_distance	= src->m_fMinDist;
		m_Params.max_distance	= src->m_fMaxDist;
	}
	if (bPlay) 		m_PreviewSnd.play	(0,sm_Looped);
	
}

void CSHSoundEnvTools::OnControlClick(ButtonValue* V, bool& bModif, bool& bSafe)
{

	switch (V->btn_num){
	case 0: m_PreviewSnd.play	(0,sm_Looped);	break;
	case 1: m_PreviewSnd.stop	();				break;
	}

	bModif = false;
}

void CSHSoundEnvTools::OnActivate()
{
	if (!psSoundFlags.is(ss_Hardware)){
		Log("#!HARDWARE or FX flags are not set. Preview is disabled.");
	}else{
		m_PreviewSnd.play			(0,sm_Looped);
		PropItemVec items;

		PropValue* V;
		V							= PHelper().CreateChoose	(items,"Source\\WAVE name",	&m_SoundName,	smSoundSource);
		V->OnChangeEvent.bind		(this,&CSHSoundEnvTools::OnChangeWAV);
		ButtonValue* B				= PHelper().CreateButton	(items,"Source\\Controls", "Play,Stop",0);
		B->OnBtnClickEvent.bind		(this,&CSHSoundEnvTools::OnControlClick);
		
		Ext.m_PreviewProps->AssignItems(items);
	   // Ext.m_PreviewProps->ShowProperties();
	}
	// fill items
	FillItemList		();
   // Ext.m_Items->SetOnModifiedEvent		(fastdelegate::bind<TOnModifiedEvent>(this,&CSHSoundEnvTools::Modified));
	Ext.m_Items->SetOnItemCloneEvent    (TOnItemClone(this,  &CSHSoundEnvTools::OnCloneItem));
	Ext.m_Items->SetOnItemCreaetEvent(TOnItemCreate(this, &CSHSoundEnvTools::OnCreateItem));

	Ext.m_Items->SetOnItemRenameEvent	(TOnItemRename(this,&CSHSoundEnvTools::OnRenameItem));
	Ext.m_Items->SetOnItemRemoveEvent	(TOnItemRemove(this,&CSHSoundEnvTools::OnRemoveItem));
	inherited::OnActivate		();
}
//---------------------------------------------------------------------------

void CSHSoundEnvTools::OnDeactivate()
{
	m_PreviewSnd.stop			();
	inherited::OnDeactivate		();
}
//---------------------------------------------------------------------------

void CSHSoundEnvTools::OnFrame()
{
	inherited::OnFrame();
}
//---------------------------------------------------------------------------

#define SOUND_SEL0_COLOR 	0x00A0A0F0
#define SOUND_SEL1_COLOR 	0x00FFFFFF

void CSHSoundEnvTools::OnRender()
{
	if (m_PreviewSnd._handle()){	
		RCache.set_xform_world	(Fidentity);
		EDevice->SetShader	(EDevice->m_WireShader);
		u32 clr0			= SOUND_SEL0_COLOR;
		u32 clr1			= SOUND_SEL1_COLOR;
		DU_impl.DrawLineSphere	(Fvector().set(0,0,0), m_Params.max_distance, clr1, true);
		DU_impl.DrawLineSphere	(Fvector().set(0,0,0), m_Params.min_distance, clr0, false);
	}
}
//---------------------------------------------------------------------------

bool CSHSoundEnvTools::OnCreate()
{
	Load							();
	return true;
}

void CSHSoundEnvTools::OnDestroy()
{
	m_Library.Unload	();
	m_bModified 		= FALSE;
}
//---------------------------------------------------------------------------

void CSHSoundEnvTools::ApplyChanges(bool bForced)
{
	UseEnvironment		();
}

void CSHSoundEnvTools::Reload()
{
	ResetCurrentItem	();
	Load				();
	FillItemList		();
}

void CSHSoundEnvTools::FillItemList()
{
	// store folders
	// fill items
	ListItemsVec items;
	SoundEnvironment_LIB::SE_VEC& lst = m_Library.Library();
	for (SoundEnvironment_LIB::SE_IT it=lst.begin(); it!=lst.end(); it++)
		LHelper().CreateItem(items,*(*it)->name,0);
	// assign items
	Ext.m_Items->AssignItems(items,0,false);
}

void CSHSoundEnvTools::Load()
{
	string_path 		fn;
	FS.update_path		(fn,_game_data_,SNDENV_FILENAME);

	m_bLockUpdate		= TRUE;

	if (FS.exist(fn))
	{
		m_Library.Unload();
		m_Library.Load	(fn);
	}else{
		ELog.DlgMsg(mtInformation,"Can't find file '%s'",fn);
	}

	m_bLockUpdate		= FALSE;
}

bool CSHSoundEnvTools::Save()
{
	ApplyChanges();
	m_bLockUpdate				= TRUE;

	// save
	string_path 				fn;
	FS.update_path				(fn,_game_data_,SNDENV_FILENAME);

	// save new file
	EFS.MarkFile				(fn,false);
	bool bRes					= m_Library.Save(fn);
	m_bLockUpdate				= FALSE;

	if (bRes) 					m_bModified	= FALSE;
	return bRes;
}


CSoundRender_Environment* CSHSoundEnvTools::FindItem(LPCSTR name)
{
	if (name && name[0]){
		return m_Library.Get(name);
	}else return 0;
}

void CSHSoundEnvTools::AppendItem(LPCSTR folder_name, LPCSTR parent_name)
{
	CSoundRender_Environment* parent= FindItem(parent_name);
	m_LastSelection = folder_name;
	CSoundRender_Environment* S 	= m_Library.Append(parent);
	if (!parent)		S->set_default();
	S->name				= m_LastSelection.c_str();
	ExecCommand			(COMMAND_UPDATE_LIST);
	ExecCommand			(COMMAND_UPDATE_PROPERTIES);
	Modified			();
}

void CSHSoundEnvTools::OnRenameItem(LPCSTR old_full_name, LPCSTR new_full_name, EItemType type)
{
	if (type==TYPE_OBJECT){
		ApplyChanges	();
		CSoundRender_Environment* S = m_Library.Get(old_full_name); R_ASSERT(S);
		S->name			= new_full_name;
		ExecCommand		(COMMAND_UPDATE_PROPERTIES);
		ExecCommand		(COMMAND_UPDATE_LIST);
	}
}

void CSHSoundEnvTools::OnRemoveItem(LPCSTR name, EItemType type)
{
	if (type==TYPE_OBJECT){
		R_ASSERT		(name && name[0]);
		if (m_Env && m_Env->name == name)
		{
			m_Env = 0;
			Tools->UpdateProperties(true);
		}
		m_Library.Remove(name);
	}
}

void CSHSoundEnvTools::SetCurrentItem(LPCSTR name, bool bView)
{
	if (m_bLockUpdate) return;
	CSoundRender_Environment* S = FindItem(name);
	if (m_Env!=S){
		m_Env 			= S;
		if (m_Env) 		m_EnvSrc = *m_Env;
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		if (bView) ViewSetCurrentItem(name);
	}
	UseEnvironment	();
}

void CSHSoundEnvTools::ResetCurrentItem()
{
	m_Env=0;
	UseEnvironment	();
}

void  CSHSoundEnvTools::OnRevResetClick(ButtonValue* V, bool& bModif, bool& bSafe)
{
	switch (V->btn_num){
	case 0: m_Env->set_identity();	break;
	case 1: OnEnvChange(V);    		break;
	}
	Modified();
}

void  CSHSoundEnvTools::OnEnvSizeChange(PropValue* sender)
{
	CSoundRender_Environment 	test_env=*m_Env;
	test_env.EnvironmentSize	= m_EnvSrc.EnvironmentSize;
	test_env.DecayTime			= m_EnvSrc.DecayTime;
	test_env.Reflections	 	= m_EnvSrc.Reflections;
	test_env.ReflectionsDelay	= m_EnvSrc.ReflectionsDelay;
	test_env.Reverb				= m_EnvSrc.Reverb;
	test_env.ReverbDelay 		= m_EnvSrc.ReverbDelay;
	CSound_environment* E		= m_Env;
	Sound->set_environment_size	(&test_env,&E);
	ExecCommand					(COMMAND_UPDATE_PROPERTIES);
}

void CSHSoundEnvTools::OnEnvChange(PropValue* sender)
{
	EAXID2EFXPRESET* Preset = nullptr;

	for (const xr_token& Token : eax_environment)
	{
		if (m_Env->Environment == Token.id)
		{
			Preset = &EFXPresetTable[Token.id];
		}
	}

	m_Env->Room = Preset->Preset.flGain;
	m_Env->RoomHF = Preset->Preset.flGainHF;
	m_Env->RoomLF = Preset->Preset.flGainLF;
	m_Env->RoomRolloffFactor = Preset->Preset.flRoomRolloffFactor;

	m_Env->DecayTime = Preset->Preset.flDecayTime;
	m_Env->DecayHFRatio = Preset->Preset.flDecayHFRatio;
	m_Env->DecayLFRatio = Preset->Preset.flDecayLFRatio;
	m_Env->DecayHFLimit = Preset->Preset.iDecayHFLimit;

	m_Env->Reflections = Preset->Preset.flReflectionsGain;
	m_Env->ReflectionsDelay = Preset->Preset.flReflectionsDelay;
	m_Env->ReflectionsPan[0] = Preset->Preset.flReflectionsPan[0];
	m_Env->ReflectionsPan[1] = Preset->Preset.flReflectionsPan[1];
	m_Env->ReflectionsPan[2] = Preset->Preset.flReflectionsPan[2];

	m_Env->EchoTime = Preset->Preset.flEchoTime;
	m_Env->EchoDepth = Preset->Preset.flEchoDepth;

	m_Env->Reverb = Preset->Preset.flLateReverbGain;
	m_Env->ReverbDelay = Preset->Preset.flLateReverbDelay;
	m_Env->ReverbPan[0] = Preset->Preset.flLateReverbPan[0];
	m_Env->ReverbPan[1] = Preset->Preset.flLateReverbPan[1];
	m_Env->ReverbPan[2] = Preset->Preset.flLateReverbPan[2];

	m_Env->EnvironmentDiffusion = Preset->Preset.flDiffusion;

	m_Env->AirAbsorptionHF = Preset->Preset.flAirAbsorptionGainHF;

	m_Env->ModulationTime = Preset->Preset.flModulationTime;
	m_Env->ModulationDepth = Preset->Preset.flModulationDepth;

	m_Env->Density = Preset->Preset.flDensity;

	m_Env->HFReference = Preset->Preset.flHFReference;
	m_Env->LFReference = Preset->Preset.flLFReference;

	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void CSHSoundEnvTools::RealUpdateList()
{
	FillItemList			();
}

#define EAXLISTENER_MINENVIRONMENTSIZE            1.0f
#define EAXLISTENER_MAXENVIRONMENTSIZE            100.0f
#define EAXLISTENER_MINENVIRONMENTDIFFUSION       0.0f
#define EAXLISTENER_MAXENVIRONMENTDIFFUSION       1.0f
#define EAXLISTENER_MINROOM                       (-10000)
#define EAXLISTENER_MAXROOM                       0
#define EAXLISTENER_MINROOMHF                     (-10000)
#define EAXLISTENER_MAXROOMHF                     0
#define EAXLISTENER_MINROOMROLLOFFFACTOR          0.0f
#define EAXLISTENER_MAXROOMROLLOFFFACTOR          10.0f
#define EAXLISTENER_MINAIRABSORPTIONHF            (-100.0f)
#define EAXLISTENER_MAXAIRABSORPTIONHF            0.0f
#define EAXLISTENER_MINREFLECTIONS                (-10000)
#define EAXLISTENER_MAXREFLECTIONS                1000
#define EAXLISTENER_MINREFLECTIONSDELAY           0.0f
#define EAXLISTENER_MAXREFLECTIONSDELAY           0.3f
#define EAXLISTENER_MINREVERB                     (-10000)
#define EAXLISTENER_MAXREVERB                     2000
#define EAXLISTENER_MINREVERBDELAY                0.0f
#define EAXLISTENER_MAXREVERBDELAY                0.1f
#define EAXLISTENER_MINDECAYTIME                  0.1f
#define EAXLISTENER_MAXDECAYTIME                  20.0f
#define EAXLISTENER_MINDECAYHFRATIO               0.1f
#define EAXLISTENER_MAXDECAYHFRATIO               2.0f

void CSHSoundEnvTools::RealUpdateProperties()
{
	PropItemVec items;
	if (m_Env){
		// fill environment
		CSoundRender_Environment& S	= *m_Env;
		ButtonValue* B			= 0;
		B=PHelper().CreateButton(items, "Environment\\Set",	"Identity,Reset", 	ButtonValue::flFirstOnly);
		B->OnBtnClickEvent.bind	(this,&CSHSoundEnvTools::OnRevResetClick);
		PropValue* V=0;
		V=PHelper().CreateToken32(items,"Environment\\Preset",					&S.Environment	       ,eax_environment);
		V->OnChangeEvent.bind	(this,&CSHSoundEnvTools::OnEnvChange);
		V=PHelper().CreateFloat	(items, "Environment\\Size",					&S.EnvironmentSize     ,EAXLISTENER_MINENVIRONMENTSIZE, 	EAXLISTENER_MAXENVIRONMENTSIZE			,0.01f,	3);
		V->OnChangeEvent.bind	(this,&CSHSoundEnvTools::OnEnvSizeChange);
		PHelper().CreateFloat	(items, "Environment\\Diffusion",				&S.EnvironmentDiffusion,EAXLISTENER_MINENVIRONMENTDIFFUSION,EAXLISTENER_MAXENVIRONMENTDIFFUSION		,0.01f,	3);
		PHelper().CreateFloat	(items, "Room\\Room",							&S.Room                ,(float)EAXLISTENER_MINROOM, 	  	(float)EAXLISTENER_MAXROOM				,1.f,	0);
		PHelper().CreateFloat	(items, "Room\\RoomHF",							&S.RoomHF              ,(float)EAXLISTENER_MINROOMHF, 	  	(float)EAXLISTENER_MAXROOMHF			,1.f,	0);
		PHelper().CreateFloat	(items, "Distance Effects\\RoomRolloffFactor",	&S.RoomRolloffFactor   ,EAXLISTENER_MINROOMROLLOFFFACTOR, 	EAXLISTENER_MAXROOMROLLOFFFACTOR		,0.01f,	3);
		PHelper().CreateFloat	(items, "Distance Effects\\AirAbsorptionHF",  	&S.AirAbsorptionHF     ,EAXLISTENER_MINAIRABSORPTIONHF, 	EAXLISTENER_MAXAIRABSORPTIONHF			,0.01f,	3);
		PHelper().CreateFloat	(items, "Reflections\\Reflections",				&S.Reflections         ,(float)EAXLISTENER_MINREFLECTIONS,	(float)EAXLISTENER_MAXREFLECTIONS		,1.f,	0);
		PHelper().CreateFloat	(items, "Reflections\\ReflectionsDelay",		&S.ReflectionsDelay    ,EAXLISTENER_MINREFLECTIONSDELAY, 	EAXLISTENER_MAXREFLECTIONSDELAY			,0.01f,	3);
		PHelper().CreateFloat	(items, "Reverb\\Reverb",						&S.Reverb              ,(float)EAXLISTENER_MINREVERB, 	  	(float)EAXLISTENER_MAXREVERB			,1.f,	0);
		PHelper().CreateFloat	(items, "Reverb\\ReverbDelay",					&S.ReverbDelay         ,EAXLISTENER_MINREVERBDELAY, 		EAXLISTENER_MAXREVERBDELAY				,0.01f,	3);
		PHelper().CreateFloat	(items, "Decay\\DecayTime",						&S.DecayTime           ,EAXLISTENER_MINDECAYTIME, 			EAXLISTENER_MAXDECAYTIME				,0.01f,	3);
		PHelper().CreateFloat	(items, "Decay\\DecayHFRatio",					&S.DecayHFRatio        ,EAXLISTENER_MINDECAYHFRATIO, 		EAXLISTENER_MAXDECAYHFRATIO				,0.01f,	3);
	}

	Ext.m_ItemProps->ClearProperties();
	Ext.m_ItemProps->AssignItems		(items);
	Ext.m_ItemProps->SetModifiedEvent	(TOnModifiedEvent(this,&CSHSoundEnvTools::Modified));
}
//---------------------------------------------------------------------------
