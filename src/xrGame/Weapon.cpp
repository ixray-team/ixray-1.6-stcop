#include "stdafx.h"
#include "Weapon.h"
#include "entity_alive.h"
#include "inventory_item_impl.h"
#include "inventory.h"
#include "xrserver_objects_alife_items.h"
#include "actor.h"
#include "actoreffector.h"
#include "level.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_cl_base.h"
#include "../Include/xrRender/Kinematics.h"
#include "ai_object_location.h"
#include "../xrphysics/mathutils.h"
#include "object_broker.h"
#include "player_hud.h"
#include "gamepersistent.h"
#include "effectorFall.h"
#include "debug_renderer.h"
#include "clsid_game.h"
#include "weaponBinocularsVision.h"
#include "ui/UIWindow.h"
#include "ui/UIXmlInit.h"
#include "Torch.h"
#include "script_game_object.h"
#include "WeaponMagazinedWGrenade.h"
#include "WeaponBinoculars.h"
#include "../xrEngine/gamemtllib.h"
#include "level_bullet_manager.h"

#define WEAPON_REMOVE_TIME		60000
#define ROTATION_TIME			0.25f

BOOL	b_toggle_weapon_aim		= FALSE;
extern CUIXml*	pWpnScopeXml;

ENGINE_API extern float psHUD_FOV_def;

CWeapon::CWeapon()
{
	SetState				(eHidden);
	SetNextState			(eHidden); 
	m_sub_state				= eSubstateReloadBegin;
	m_bTriStateReload		= false;
	SetDefaults				();

	m_Offset.identity		();
	m_StrapOffset.identity	();
	m_StrapOffset_alt.identity();

	m_iAmmoCurrentTotal		= 0;
	m_BriefInfo_CalcFrame	= 0;

	iAmmoElapsed			= -1;
	iMagazineSize			= -1;
	m_ammoType				= 0;

	eHandDependence			= hdNone;

	m_zoom_params.m_fCurrentZoomFactor			= g_fov;
	m_zoom_params.m_fZoomRotationFactor			= 0.f;
	m_zoom_params.m_pVision						= nullptr;
	m_zoom_params.m_pNight_vision				= nullptr;

	m_pCurrentAmmo			= nullptr;

	m_pFlameParticles2		= nullptr;
	m_sFlameParticles2		= nullptr;


	m_fCurrentCartirdgeDisp = 1.f;

	m_strap_bone0			= 0;
	m_strap_bone1			= 0;
	m_strap_bone0_id = -1;
	m_strap_bone1_id = -1;
	m_StrapOffset.identity	();
	m_StrapOffset_alt.identity();
	m_strapped_mode			= false;
	m_can_be_strapped = false;
	m_strapped_mode_rifle = false;
	m_can_be_strapped_rifle = false;
	m_ef_main_weapon_type	= u32(-1);
	m_ef_weapon_type		= u32(-1);
	m_UIScope				= nullptr;
	m_set_next_ammoType_on_reload = undefined_ammo_type;
	m_crosshair_inertion	= 0.f;
	m_activation_speed_is_overriden	=	false;
	m_cur_scope				= 0;
	m_bRememberActorNVisnStatus = false;
	bReloadKeyPressed		= false;
	bAmmotypeKeyPressed		= false;
	bUnjamKeyPressed		= false;
	bNextModeKeyPressed		= false;
	bPrevModeKeyPressed		= false;
	m_HudFovZoom = 0.0f;

	hud_silencer = nullptr;
	hud_scope = nullptr;
	hud_gl = nullptr;

	bIsNeedCallDet = false;

	lock_time = 0.f;
	lock_time_param = 0;
	lock_time_callback = nullptr;
	_last_update_time = Device.dwTimeGlobal;
	curr_anim = "";
	ammo_cnt_to_reload = -1;
	IsReloaded = false;
	_last_shot_ammotype = 0;
	IsAimStarted = false;
	_wanim_force_assign = false;
}

CWeapon::~CWeapon		()
{
	xr_delete				(m_UIScope);
	delete_data				(m_scopes);
}

void CWeapon::Hit					(SHit* pHDS)
{
	inherited::Hit(pHDS);
}



void CWeapon::UpdateXForm	()
{
	if (!H_Parent())
		return;

	// Get access to entity and its visual
	CEntityAlive*			E = smart_cast<CEntityAlive*>(H_Parent());
	
	if (!E) {
		if (!IsGameTypeSingle()) {
			UpdatePosition(H_Parent()->XFORM());
			UpdatePosition_alt(H_Parent()->XFORM());
		}
		return;
	}

	const CInventoryOwner	*parent = smart_cast<const CInventoryOwner*>(E);
	if (!parent || (parent && parent->use_simplified_visual()))
		return;

	if (!m_can_be_strapped_rifle) {
		if (parent->attached(this))
			return;
	}

	IKinematics*			V = smart_cast<IKinematics*>	(E->Visual());
	VERIFY					(V);

	// Get matrices
	int						boneL = -1, boneR = -1, boneR2 = -1;

	// this ugly case is possible in case of a CustomMonster, not a Stalker, nor an Actor
	if ((m_strap_bone0_id == -1 || m_strap_bone1_id == -1) && m_can_be_strapped_rifle) {
		m_strap_bone0_id = V->LL_BoneID(m_strap_bone0);
		m_strap_bone1_id = V->LL_BoneID(m_strap_bone1);
	}

	if (parent->inventory().GetActiveSlot() != CurrSlot() && m_can_be_strapped_rifle /* &&
		parent->inventory().InSlot(this)*/) { // TODO: What is this condition needed for?
		boneR = m_strap_bone0_id;
		boneR2 = m_strap_bone1_id;
		boneL = boneR;

		if (!m_strapped_mode_rifle)
			m_strapped_mode_rifle = true;
	}
	else {
		E->g_WeaponBones(boneL, boneR, boneR2);

		if (m_strapped_mode_rifle)
			m_strapped_mode_rifle = false;
	}

	if (boneR == -1)		return;

	if ((HandDependence() == hd1Hand) || (GetState() == eReload) || (!E->g_Alive()))
		boneL				= boneR2;

	Fmatrix mL, mR;
	if (smart_cast<CActor*>(H_Parent())) {
		V->Bone_GetAnimPos(mL, boneL, u8(-1), false);
		V->Bone_GetAnimPos(mR, boneR, u8(-1), false);
	}
	else {
		V->CalculateBones();
		mL = V->LL_GetTransform(boneL);
		mR = V->LL_GetTransform(boneR);
	}

	// Calculate
	Fmatrix					mRes;
	Fvector					R,D,N;
	D.sub					(mL.c,mR.c);	

	if(fis_zero(D.magnitude())) {
		mRes.set			(E->XFORM());
		mRes.c.set			(mR.c);
	}
	else {		
		D.normalize			();

		R.crossproduct		(mR.j,D);
		R.normalize			();

		N.crossproduct		(D,R);			
		N.normalize			();

		mRes.set			(R,N,D,mR.c);
		mRes.mulA_43		(E->XFORM());
	}

	if (CurrSlot() == INV_SLOT_2)
		UpdatePosition_alt(mRes);
	else
		UpdatePosition(mRes);
}

void CWeapon::UpdateFireDependencies_internal()
{
	if (Device.dwFrame!=dwFP_Frame) 
	{
		dwFP_Frame			= Device.dwFrame;

		UpdateXForm			();

		if ( GetHUDmode() )
		{
			HudItemData()->setup_firedeps		(m_current_firedeps);
			VERIFY(_valid(m_current_firedeps.m_FireParticlesXForm));
		} else 
		{
			// 3rd person or no parent
			Fmatrix& parent			= XFORM();
			Fvector& fp				= vLoadedFirePoint;
			Fvector& fp2			= vLoadedFirePoint2;
			Fvector& sp				= vLoadedShellPoint;

			parent.transform_tiny	(m_current_firedeps.vLastFP,fp);
			parent.transform_tiny	(m_current_firedeps.vLastFP2,fp2);
			parent.transform_tiny	(m_current_firedeps.vLastSP,sp);
			
			m_current_firedeps.vLastFD.set	(0.f,0.f,1.f);
			parent.transform_dir	(m_current_firedeps.vLastFD);

			m_current_firedeps.m_FireParticlesXForm.set(parent);
			VERIFY(_valid(m_current_firedeps.m_FireParticlesXForm));
		}
	}
}

void CWeapon::ForceUpdateFireParticles()
{
	if ( !GetHUDmode() )
	{//update particlesXFORM real bullet direction

		if (!H_Parent())		return;

		Fvector					p, d; 
		smart_cast<CEntity*>(H_Parent())->g_fireParams	(this, p,d);

		Fmatrix						_pxf;
		_pxf.k						= d;
		_pxf.i.crossproduct			(Fvector().set(0.0f,1.0f,0.0f),	_pxf.k);
		_pxf.j.crossproduct			(_pxf.k,		_pxf.i);
		_pxf.c						= XFORM().c;
		
		m_current_firedeps.m_FireParticlesXForm.set	(_pxf);
	}
}

void CWeapon::Load		(LPCSTR section)
{
	inherited::Load					(section);
	CShootingObject::Load			(section);

	m_base_inertion = m_current_inertion;

	m_zoom_inertion.PitchOffsetR = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_aim_pitch_offset_r", 0.0f);
	m_zoom_inertion.PitchOffsetD = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_aim_pitch_offset_d", 0.0f);
	m_zoom_inertion.PitchOffsetN = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_aim_pitch_offset_n", 0.0f);

	m_zoom_inertion.OriginOffset = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_aim_origin_offset", ORIGIN_OFFSET * 0.5f);
	m_zoom_inertion.TendtoSpeed = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_aim_tendto_speed", TENDTO_SPEED);
	
	if(pSettings->line_exist(section, "flame_particles_2"))
		m_sFlameParticles2 = pSettings->r_string(section, "flame_particles_2");

	// load ammo classes
	m_ammoTypes.clear	(); 
	LPCSTR				S = pSettings->r_string(section,"ammo_class");
	if (S && S[0]) 
	{
		string128		_ammoItem;
		int				count		= _GetItemCount	(S);
		for (int it=0; it<count; ++it)	
		{
			_GetItem				(S,it,_ammoItem);
			m_ammoTypes.push_back	(_ammoItem);
		}
	}

	iAmmoElapsed		= pSettings->r_s32		(section,"ammo_elapsed"		);
	iMagazineSize		= pSettings->r_s32		(section,"ammo_mag_size"	);
	
	u8 rm = READ_IF_EXISTS( pSettings, r_u8, section, "cam_return", 1 );
	cam_recoil.ReturnMode = (rm == 1);
	
	rm = READ_IF_EXISTS( pSettings, r_u8, section, "cam_return_stop", 0 );
	cam_recoil.StopReturn = (rm == 1);

	float temp_f = 0.0f;
	temp_f					= pSettings->r_float( section,"cam_relax_speed" );
	cam_recoil.RelaxSpeed	= _abs( deg2rad( temp_f ) );
	VERIFY( !fis_zero(cam_recoil.RelaxSpeed) );
	if ( fis_zero(cam_recoil.RelaxSpeed) )
	{
		cam_recoil.RelaxSpeed = EPS_L;
	}

	cam_recoil.RelaxSpeed_AI = cam_recoil.RelaxSpeed;
	if ( pSettings->line_exist( section, "cam_relax_speed_ai" ) )
	{
		temp_f						= pSettings->r_float( section, "cam_relax_speed_ai" );
		cam_recoil.RelaxSpeed_AI	= _abs( deg2rad( temp_f ) );
		VERIFY( !fis_zero(cam_recoil.RelaxSpeed_AI) );
		if ( fis_zero(cam_recoil.RelaxSpeed_AI) )
		{
			cam_recoil.RelaxSpeed_AI = EPS_L;
		}
	}
	temp_f						= pSettings->r_float( section, "cam_max_angle" );
	cam_recoil.MaxAngleVert		= _abs( deg2rad( temp_f ) );
	VERIFY( !fis_zero(cam_recoil.MaxAngleVert) );
	if ( fis_zero(cam_recoil.MaxAngleVert) )
	{
		cam_recoil.MaxAngleVert = EPS;
	}
	
	temp_f						= pSettings->r_float( section, "cam_max_angle_horz" );
	cam_recoil.MaxAngleHorz		= _abs( deg2rad( temp_f ) );
	VERIFY( !fis_zero(cam_recoil.MaxAngleHorz) );
	if ( fis_zero(cam_recoil.MaxAngleHorz) )
	{
		cam_recoil.MaxAngleHorz = EPS;
	}
	
	temp_f						= pSettings->r_float( section, "cam_step_angle_horz" );
	cam_recoil.StepAngleHorz	= deg2rad( temp_f );
	
	cam_recoil.DispersionFrac	= _abs( READ_IF_EXISTS( pSettings, r_float, section, "cam_dispersion_frac", 0.7f ) );

	
	//zoom_cam_recoil.Clone( cam_recoil ); ==== ������ !!!!!!!!!!
	zoom_cam_recoil.RelaxSpeed		= cam_recoil.RelaxSpeed;
	zoom_cam_recoil.RelaxSpeed_AI	= cam_recoil.RelaxSpeed_AI;
	zoom_cam_recoil.DispersionFrac	= cam_recoil.DispersionFrac;
	zoom_cam_recoil.MaxAngleVert	= cam_recoil.MaxAngleVert;
	zoom_cam_recoil.MaxAngleHorz	= cam_recoil.MaxAngleHorz;
	zoom_cam_recoil.StepAngleHorz	= cam_recoil.StepAngleHorz;

	zoom_cam_recoil.ReturnMode		= cam_recoil.ReturnMode;
	zoom_cam_recoil.StopReturn		= cam_recoil.StopReturn;

	
	if ( pSettings->line_exist( section, "zoom_cam_relax_speed" ) )
	{
		zoom_cam_recoil.RelaxSpeed		= _abs( deg2rad( pSettings->r_float( section, "zoom_cam_relax_speed" ) ) );
		VERIFY( !fis_zero(zoom_cam_recoil.RelaxSpeed) );
		if ( fis_zero(zoom_cam_recoil.RelaxSpeed) )
		{
			zoom_cam_recoil.RelaxSpeed = EPS_L;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_relax_speed_ai" ) )
	{
		zoom_cam_recoil.RelaxSpeed_AI	= _abs( deg2rad( pSettings->r_float( section,"zoom_cam_relax_speed_ai" ) ) ); 
		VERIFY( !fis_zero(zoom_cam_recoil.RelaxSpeed_AI) );
		if ( fis_zero(zoom_cam_recoil.RelaxSpeed_AI) )
		{
			zoom_cam_recoil.RelaxSpeed_AI = EPS_L;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_max_angle" ) )
	{
		zoom_cam_recoil.MaxAngleVert	= _abs( deg2rad( pSettings->r_float( section, "zoom_cam_max_angle" ) ) );
		VERIFY( !fis_zero(zoom_cam_recoil.MaxAngleVert) );
		if ( fis_zero(zoom_cam_recoil.MaxAngleVert) )
		{
			zoom_cam_recoil.MaxAngleVert = EPS;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_max_angle_horz" ) )
	{
		zoom_cam_recoil.MaxAngleHorz	= _abs( deg2rad( pSettings->r_float( section, "zoom_cam_max_angle_horz" ) ) ); 
		VERIFY( !fis_zero(zoom_cam_recoil.MaxAngleHorz) );
		if ( fis_zero(zoom_cam_recoil.MaxAngleHorz) )
		{
			zoom_cam_recoil.MaxAngleHorz = EPS;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_step_angle_horz" ) )	{
		zoom_cam_recoil.StepAngleHorz	= deg2rad( pSettings->r_float( section, "zoom_cam_step_angle_horz" ) ); 
	}
	if ( pSettings->line_exist( section, "zoom_cam_dispersion_frac" ) )	{
		zoom_cam_recoil.DispersionFrac	= _abs( pSettings->r_float( section, "zoom_cam_dispersion_frac" ) );
	}

	m_pdm.m_fPDM_disp_base			= pSettings->r_float( section, "PDM_disp_base"			);
	m_pdm.m_fPDM_disp_vel_factor	= pSettings->r_float( section, "PDM_disp_vel_factor"	);
	m_pdm.m_fPDM_disp_accel_factor	= pSettings->r_float( section, "PDM_disp_accel_factor"	);
	m_pdm.m_fPDM_disp_crouch		= pSettings->r_float( section, "PDM_disp_crouch"		);
	m_pdm.m_fPDM_disp_crouch_no_acc	= pSettings->r_float( section, "PDM_disp_crouch_no_acc" );
	m_crosshair_inertion			= READ_IF_EXISTS(pSettings, r_float, section, "crosshair_inertion",	5.91f);
	m_HudFovZoom = READ_IF_EXISTS(pSettings, r_float, hud_sect, "hud_fov_zoom", 0.0f);

	m_first_bullet_controller.load	(section);
	fireDispersionConditionFactor = pSettings->r_float(section,"fire_dispersion_condition_factor");

// modified by Peacemaker [17.10.08]
//	misfireProbability			  = pSettings->r_float(section,"misfire_probability"); 
//	misfireConditionK			  = READ_IF_EXISTS(pSettings, r_float, section, "misfire_condition_k",	1.0f);
	misfireStartCondition			= pSettings->r_float(section, "misfire_start_condition");
	misfireEndCondition				= READ_IF_EXISTS(pSettings, r_float, section, "misfire_end_condition", 0.f);
	misfireStartProbability			= READ_IF_EXISTS(pSettings, r_float, section, "misfire_start_prob", 0.f);
	misfireEndProbability			= pSettings->r_float(section, "misfire_end_prob");
	conditionDecreasePerShot		= pSettings->r_float(section,"condition_shot_dec"); 
	conditionDecreasePerQueueShot	= READ_IF_EXISTS(pSettings, r_float, section, "condition_queue_shot_dec", conditionDecreasePerShot); 




	vLoadedFirePoint	= pSettings->r_fvector3		(section,"fire_point"		);
	
	if(pSettings->line_exist(section,"fire_point2")) 
		vLoadedFirePoint2= pSettings->r_fvector3	(section,"fire_point2");
	else 
		vLoadedFirePoint2= vLoadedFirePoint;

	// hands
	eHandDependence		= EHandDependence(pSettings->r_s32(section,"hand_dependence"));
	m_bIsSingleHanded	= true;
	if (pSettings->line_exist(section, "single_handed"))
		m_bIsSingleHanded	= !!pSettings->r_bool(section, "single_handed");
	// 
	m_fMinRadius		= pSettings->r_float		(section,"min_radius");
	m_fMaxRadius		= pSettings->r_float		(section,"max_radius");


	// информация о возможных апгрейдах и их визуализации в инвентаре
	m_eScopeStatus			 = (ALife::EWeaponAddonStatus)pSettings->r_s32(section,"scope_status");
	m_eSilencerStatus		 = (ALife::EWeaponAddonStatus)pSettings->r_s32(section,"silencer_status");
	m_eGrenadeLauncherStatus = (ALife::EWeaponAddonStatus)pSettings->r_s32(section,"grenade_launcher_status");

	m_zoom_params.m_bZoomEnabled		= !!pSettings->r_bool(section,"zoom_enabled");
	m_zoom_params.m_fZoomRotateTime		= pSettings->r_float(section,"zoom_rotate_time");

	if ( m_eScopeStatus == ALife::eAddonAttachable )
	{
		if(pSettings->line_exist(section, "scopes_sect"))		
		{
			LPCSTR str = pSettings->r_string(section, "scopes_sect");
			for(int i = 0, count = _GetItemCount(str); i < count; ++i )	
			{
				string128						scope_section;
				_GetItem						(str, i, scope_section);
				m_scopes.push_back				(scope_section);
			}
		}
		else
		{
			m_scopes.push_back(section);
		}
	}
	else if( m_eScopeStatus == ALife::eAddonPermanent )
	{
		shared_str scope_tex_name			= pSettings->r_string(cNameSect(), "scope_texture");
		m_zoom_params.m_fScopeZoomFactor	= pSettings->r_float( cNameSect(), "scope_zoom_factor");
		if ( !g_dedicated_server )
		{
			m_UIScope				= new CUIWindow();
			if(!pWpnScopeXml)
			{
				pWpnScopeXml			= new CUIXml();
				pWpnScopeXml->Load		(CONFIG_PATH, UI_PATH, "scopes.xml");
			}
			CUIXmlInit::InitWindow	(*pWpnScopeXml, scope_tex_name.c_str(), 0, m_UIScope);
		}
	}
    
	if ( m_eSilencerStatus == ALife::eAddonAttachable )
	{
		m_sSilencerName = pSettings->r_string(section,"silencer_name");

		int UseHQ = EngineExternal()[EEngineExternalUI::HQIcons];
		m_iSilencerX = pSettings->r_s32(section, "silencer_x") * (1 + UseHQ);
		m_iSilencerY = pSettings->r_s32(section, "silencer_y") * (1 + UseHQ);
	}
    
	if ( m_eGrenadeLauncherStatus == ALife::eAddonAttachable )
	{
		m_sGrenadeLauncherName = pSettings->r_string(section,"grenade_launcher_name");

		int UseHQ = EngineExternal()[EEngineExternalUI::HQIcons];
		m_iGrenadeLauncherX = pSettings->r_s32(section, "grenade_launcher_x") * (1 + UseHQ);
		m_iGrenadeLauncherY = pSettings->r_s32(section, "grenade_launcher_y") * (1 + UseHQ);
	}

	InitAddons();
	if(pSettings->line_exist(section,"weapon_remove_time"))
		m_dwWeaponRemoveTime = pSettings->r_u32(section,"weapon_remove_time");
	else
		m_dwWeaponRemoveTime = WEAPON_REMOVE_TIME;

	if(pSettings->line_exist(section,"auto_spawn_ammo"))
		m_bAutoSpawnAmmo = pSettings->r_bool(section,"auto_spawn_ammo");
	else
		m_bAutoSpawnAmmo = TRUE;



	m_zoom_params.m_bHideCrosshairInZoom		= true;

	if(pSettings->line_exist(hud_sect, "zoom_hide_crosshair"))
		m_zoom_params.m_bHideCrosshairInZoom = !!pSettings->r_bool(hud_sect, "zoom_hide_crosshair");	

	Fvector			def_dof;
	def_dof.set		(-1,-1,-1);
//	m_zoom_params.m_ZoomDof		= READ_IF_EXISTS(pSettings, r_fvector3, section, "zoom_dof", Fvector().set(-1,-1,-1));
//	m_zoom_params.m_bZoomDofEnabled	= !def_dof.similar(m_zoom_params.m_ZoomDof);

	m_zoom_params.m_ReloadDof	= READ_IF_EXISTS(pSettings, r_fvector4, section, "reload_dof", Fvector4().set(-1,-1,-1,-1));


	m_bHasTracers			= !!READ_IF_EXISTS(pSettings, r_bool, section, "tracers", true);
	m_u8TracerColorID		= READ_IF_EXISTS(pSettings, r_u8, section, "tracers_color_ID", u8(-1));

	string256						temp;
	for (int i=egdNovice; i<egdCount; ++i) 
	{
		xr_strconcat(temp,"hit_probability_",get_token_name(difficulty_type_token,i));
		m_hit_probability[i]		= READ_IF_EXISTS(pSettings,r_float,section,temp,1.f);
	}

	
	m_zoom_params.m_bUseDynamicZoom				= READ_IF_EXISTS(pSettings,r_bool,section,"scope_dynamic_zoom",FALSE);
	m_zoom_params.m_sUseZoomPostprocess			= 0;
	m_zoom_params.m_sUseBinocularVision			= 0;

	m_bUseSilHud = READ_IF_EXISTS(pSettings, r_bool, section, "hud_when_silencer_is_attached", false);
	m_bUseScopeHud = READ_IF_EXISTS(pSettings, r_bool, section, "hud_when_scope_is_attached", false);
	m_bUseGLHud = READ_IF_EXISTS(pSettings, r_bool, section, "hud_when_gl_is_attached", false);

	if (m_bUseSilHud)
		hud_silencer = pSettings->r_string(section, "hud_silencer");

	if (m_bUseScopeHud)
		hud_scope = pSettings->r_string(section, "hud_scope");

	if (m_bUseGLHud)
		hud_gl = pSettings->r_string(section, "hud_gl");

	m_bUseChangeFireModeAnim = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "use_firemode_change_anim", false);
	m_bRestGL_and_Sil = READ_IF_EXISTS(pSettings, r_bool, section, "restricted_gl_and_sil", false);

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	m_bJamNotShot = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "no_jam_fire", !isGuns);
	m_bAmmoInChamber = READ_IF_EXISTS(pSettings, r_bool, section, "ammo_in_chamber", false);

	m_bMixAfterIdle = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "mix_shoot_after_idle", false);
	m_bMixAfterReload = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "mix_shoot_after_reload", false);
	m_bMixAfterQueue = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "mix_shoot_after_shoot_in_queue", false);

	// Added by Axel, to enable optional condition use on any item
	m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", true));
}

void CWeapon::LoadFireParams		(LPCSTR section)
{
	cam_recoil.Dispersion = deg2rad( pSettings->r_float( section,"cam_dispersion" ) ); 
	cam_recoil.DispersionInc = 0.0f;

	if ( pSettings->line_exist( section, "cam_dispersion_inc" ) )	{
		cam_recoil.DispersionInc = deg2rad( pSettings->r_float( section, "cam_dispersion_inc" ) ); 
	}
	
	zoom_cam_recoil.Dispersion		= cam_recoil.Dispersion;
	zoom_cam_recoil.DispersionInc	= cam_recoil.DispersionInc;

	if ( pSettings->line_exist( section, "zoom_cam_dispersion" ) )	{
		zoom_cam_recoil.Dispersion		= deg2rad( pSettings->r_float( section, "zoom_cam_dispersion" ) ); 
	}
	if ( pSettings->line_exist( section, "zoom_cam_dispersion_inc" ) )	{
		zoom_cam_recoil.DispersionInc	= deg2rad( pSettings->r_float( section, "zoom_cam_dispersion_inc" ) ); 
	}

	CShootingObject::LoadFireParams(section);
};



BOOL CWeapon::net_Spawn		(CSE_Abstract* DC)
{
	BOOL bResult					= inherited::net_Spawn(DC);
	CSE_Abstract					*e	= (CSE_Abstract*)(DC);
	CSE_ALifeItemWeapon			    *E	= smart_cast<CSE_ALifeItemWeapon*>(e);

	//iAmmoCurrent					= E->a_current;
	iAmmoElapsed					= E->a_elapsed;
	m_flagsAddOnState				= E->m_addon_flags.get();
	m_ammoType						= E->ammo_type;
	SetState						(E->wpn_state);
	SetNextState					(E->wpn_state);
	bMisfire						= E->misfire;
	if (E->rt_zoom_factor == 0.f)
		m_fRTZoomFactor					= m_zoom_params.m_fScopeZoomFactor;
	else
		m_fRTZoomFactor					= E->rt_zoom_factor;

	if (E->cur_scope < m_scopes.size() && m_scopes.size()>1)
		m_cur_scope = E->cur_scope;

	m_DefaultCartridge.Load(m_ammoTypes[m_ammoType].c_str(), m_ammoType);
	if(iAmmoElapsed) 
	{
		m_fCurrentCartirdgeDisp = m_DefaultCartridge.param_s.kDisp;
		for(int i = 0; i < iAmmoElapsed; ++i) 
			m_magazine.push_back(m_DefaultCartridge);
	}

	xr_vector<u8> ammo_ids = E->m_AmmoIDs;

	for (u32 i = 0; i < ammo_ids.size(); i++)
	{
		u8 LocalAmmoType = ammo_ids[i];
		if (i >= m_magazine.size())
			continue;

		CCartridge& l_cartridge = *(m_magazine.begin() + i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType)
			continue;

		l_cartridge.Load(*m_ammoTypes[LocalAmmoType], LocalAmmoType);
	}

	UpdateAddonsVisibility();
	InitAddons();

	m_dwWeaponIndependencyTime = 0;

	VERIFY((u32)iAmmoElapsed == m_magazine.size());
	m_bAmmoWasSpawned		= false;

	_wanim_force_assign = true;

	return bResult;
}

void CWeapon::net_Destroy	()
{
	inherited::net_Destroy	();

	//удалить объекты партиклов
	StopFlameParticles	();
	StopFlameParticles2	();
	StopLight			();
	Light_Destroy		();

	while (m_magazine.size())
		m_magazine.pop_back();
}

BOOL CWeapon::IsUpdating()
{	
	bool bIsActiveItem = m_pInventory && m_pInventory->ActiveItem()==this;
	return bIsActiveItem || bWorking;// || IsPending() || getVisible();
}

void CWeapon::net_Export(NET_Packet& P)
{
	inherited::net_Export	(P);

	P.w_float_q8			(GetCondition(),0.0f,1.0f);


	u8 need_upd				= IsUpdating() ? 1 : 0;
	P.w_u8					(need_upd);
	P.w_u16					(u16(iAmmoElapsed));
	P.w_u8					(m_flagsAddOnState);
	P.w_u8					(m_ammoType);
	P.w_u8					((u8)GetState());
	P.w_u8					((u8)IsZoomed());
	P.w_u8					((u8)bMisfire);
	P.w_float				(m_fRTZoomFactor);
	P.w_u8					((u8)m_cur_scope);

	P.w_u8(u8(m_magazine.size()));
	for (u32 i = 0; i < m_magazine.size(); i++)
	{
		CCartridge& l_cartridge = *(m_magazine.begin() + i);
		P.w_u8(l_cartridge.m_LocalAmmoType);
	}
}

void CWeapon::net_Import(NET_Packet& P)
{
	inherited::net_Import (P);
	
	float _cond;
	P.r_float_q8			(_cond,0.0f,1.0f);
	SetCondition			(_cond);

	u8 flags				= 0;
	P.r_u8					(flags);

	u16 ammo_elapsed = 0;
	P.r_u16					(ammo_elapsed);

	u8						NewAddonState;
	P.r_u8					(NewAddonState);

	m_flagsAddOnState		= NewAddonState;
	UpdateAddonsVisibility	();

	u8 ammoType, wstate;
	P.r_u8					(ammoType);
	P.r_u8					(wstate);

	u8 Zoom;
	P.r_u8					(Zoom);

	u8 Misfire;
	P.r_u8					(Misfire);
	bMisfire				= Misfire;

	float RTZoom;
	P.r_float				(RTZoom);
	m_fRTZoomFactor			= RTZoom;

	u8 scope;
	P.r_u8					(scope);
	m_cur_scope				= scope;

	u8 AmmoCount = P.r_u8();
	for (u32 i = 0; i < AmmoCount; i++)
	{
		u8 LocalAmmoType = P.r_u8();
		if (i >= m_magazine.size())
			continue;

		CCartridge& l_cartridge = *(m_magazine.begin() + i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType)
			continue;

		l_cartridge.Load(m_ammoTypes[LocalAmmoType].c_str(), LocalAmmoType);
	}

	if (H_Parent() && H_Parent()->Remote())
	{
		if (Zoom) OnZoomIn();
		else OnZoomOut();
	};
	switch (wstate)
	{	
	case eFire:
	case eFire2:
	case eSwitch:
	case eReload:
		{
		}break;	
	default:
		{
			if (ammoType >= m_ammoTypes.size())
				Msg("!! Weapon [%d], State - [%d]", ID(), wstate);
			else
			{
				m_ammoType = ammoType;
				SetAmmoElapsed((ammo_elapsed));
			}
		}break;
	}
	
	VERIFY((u32)iAmmoElapsed == m_magazine.size());
}

void CWeapon::save(NET_Packet &output_packet)
{
	inherited::save	(output_packet);
	save_data		(iAmmoElapsed,					output_packet);
	save_data		(m_cur_scope, 					output_packet);
	save_data		(m_flagsAddOnState, 			output_packet);
	save_data		(m_ammoType,					output_packet);
	save_data		(m_zoom_params.m_bIsZoomModeNow,output_packet);
	save_data		(m_bRememberActorNVisnStatus,	output_packet);
}

void CWeapon::load(IReader &input_packet)
{
	inherited::load	(input_packet);
	load_data		(iAmmoElapsed,					input_packet);
	load_data		(m_cur_scope,					input_packet);
	load_data		(m_flagsAddOnState,				input_packet);
	UpdateAddonsVisibility			();
	load_data		(m_ammoType,					input_packet);
	load_data		(m_zoom_params.m_bIsZoomModeNow,input_packet);

	if (m_zoom_params.m_bIsZoomModeNow)	
			OnZoomIn();
		else			
			OnZoomOut();

	load_data		(m_bRememberActorNVisnStatus,	input_packet);
}


void CWeapon::OnEvent(NET_Packet& P, u16 type) 
{
	switch (type)
	{
	case GE_ADDON_CHANGE:
		{
			P.r_u8					(m_flagsAddOnState);
			InitAddons();
			UpdateAddonsVisibility();
		}break;

	case GE_WPN_STATE_CHANGE:
		{
			u8				state;
			P.r_u8			(state);
			P.r_u8			(m_sub_state);		
//			u8 NewAmmoType = 
				P.r_u8();
			u8 AmmoElapsed = P.r_u8();
			u8 NextAmmo = P.r_u8();
			if (NextAmmo == undefined_ammo_type)
				m_set_next_ammoType_on_reload = undefined_ammo_type;
			else
				m_set_next_ammoType_on_reload = NextAmmo;

			if (OnClient()) SetAmmoElapsed(int(AmmoElapsed));			
			OnStateSwitch	(u32(state));
		}
		break;
	default:
		{
			inherited::OnEvent(P,type);
		}break;
	}
};

void CWeapon::shedule_Update	(u32 dT)
{
	// Queue shrink
//	u32	dwTimeCL		= Level().timeServer()-NET_Latency;
//	while ((NET.size()>2) && (NET[1].dwTimeStamp<dwTimeCL)) NET.pop_front();	

	// Inherited
	inherited::shedule_Update	(dT);
}

void CWeapon::OnH_B_Independent	(bool just_before_destroy)
{
	RemoveShotEffector			();

	inherited::OnH_B_Independent(just_before_destroy);

	FireEnd						();
	SetPending					(FALSE);
	SwitchState					(eHidden);

	m_strapped_mode				= false;
	m_strapped_mode_rifle = false;
	m_zoom_params.m_bIsZoomModeNow	= false;
	UpdateXForm					();

}

void CWeapon::OnH_A_Independent	()
{
	m_dwWeaponIndependencyTime = Level().timeServer();
	inherited::OnH_A_Independent();
	Light_Destroy				();
	UpdateAddonsVisibility		();
	Engine.Sheduler.Unregister(this);
};

void CWeapon::OnH_A_Chield		()
{
	inherited::OnH_A_Chield		();
	UpdateAddonsVisibility		();
	shedule.t_min = shedule.t_max = 1;
	Engine.Sheduler.Register(this, TRUE);
};

void CWeapon::OnActiveItem ()
{
	//. from Activate
	UpdateAddonsVisibility();
	m_BriefInfo_CalcFrame = 0;

//. Show
	SwitchState					(eShowing);
//-

	bReloadKeyPressed = false;
	bAmmotypeKeyPressed = false;
	bStopReloadSignal = false;

	inherited::OnActiveItem		();
	//если мы занружаемся и оружие было в руках
//.	SetState					(eIdle);
//.	SetNextState				(eIdle);
}

void CWeapon::OnHiddenItem ()
{
	m_BriefInfo_CalcFrame = 0;

	if(IsGameTypeSingle())
		SwitchState(eHiding);
	else
		SwitchState(eHidden);

	OnZoomOut();
	inherited::OnHiddenItem		();

	m_set_next_ammoType_on_reload = undefined_ammo_type;
}

bool CWeapon::SendDeactivateItem()
{
	if (GetState() == eFire)
		return false;

	if (ParentIsActor() && (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcSprint || IsZoomed() || (GetState() != eIdle && GetState() != eShowing)))
	{
		if (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcSprint)
			Actor()->SetMovementState(eWishful, mcSprint, false);
		return false;
	}

	return inherited::SendDeactivateItem();
}

void CWeapon::SendHiddenItem()
{
	if (!CHudItem::object().getDestroy() && m_pInventory)
	{
		// !!! Just single entry for given state !!!
		NET_Packet		P;
		CHudItem::object().u_EventGen		(P,GE_WPN_STATE_CHANGE,CHudItem::object().ID());
		P.w_u8			(u8(eHiding));
		P.w_u8			(u8(m_sub_state));
		P.w_u8			(m_ammoType);
		P.w_u8			(u8(iAmmoElapsed & 0xff));
		P.w_u8			(m_set_next_ammoType_on_reload);
		CHudItem::object().u_EventSend		(P, net_flags(TRUE, TRUE, FALSE, TRUE));
		SetPending		(TRUE);
	}
}


void CWeapon::OnH_B_Chield		()
{
	m_dwWeaponIndependencyTime = 0;
	inherited::OnH_B_Chield		();

	OnZoomOut					();
	m_set_next_ammoType_on_reload = undefined_ammo_type;
}

extern u32 hud_adj_mode;

void CWeapon::UpdateCL		()
{
	inherited::UpdateCL		();
	UpdateHUDAddonsVisibility();
	//подсветка от выстрела
	UpdateLight				();

	//нарисовать партиклы
	UpdateFlameParticles	();
	UpdateFlameParticles2	();

	if(!IsGameTypeSingle())
		make_Interpolation		();

	if (GetDetector() && (GetDetector()->GetState() == CCustomDetector::eIdle || !GetDetector()->NeedActivation()))
	{
		if (bUnjamKeyPressed)
		{
			bUnjamKeyPressed = false;
			Action(kWPN_RELOAD, CMD_START);
		}
		else if (bAmmotypeKeyPressed)
		{
			bAmmotypeKeyPressed = false;
			Action(kWPN_NEXT, CMD_START);
		}
		else if (bReloadKeyPressed)
		{
			bReloadKeyPressed = false;
			Action(kWPN_RELOAD, CMD_START);
		}
		else if (bNextModeKeyPressed)
		{
			bNextModeKeyPressed = false;
			Action(kWPN_FIREMODE_NEXT, CMD_START);
		}
		else if (bPrevModeKeyPressed)
		{
			bPrevModeKeyPressed = false;
			Action(kWPN_FIREMODE_PREV, CMD_START);
		}
	}

	if (AllowBore())
	{
		if (GetNextState() == GetState() && IsGameTypeSingle() && H_Parent() == Level().CurrentEntity())
		{
			CActor* pActor	= smart_cast<CActor*>(H_Parent());
			if(pActor && !pActor->AnyMove() && this==pActor->inventory().ActiveItem())
			{
				if (hud_adj_mode == 0 && GetState() == eIdle && (Device.dwTimeGlobal - m_dw_curr_substate_time > 20000) && !IsZoomed() && g_player_hud->attached_item(1) == nullptr)
				{
					SwitchState(eBore);
					ResetSubStateTime();
				}
			}
		}
	}

	if(m_zoom_params.m_pNight_vision && !need_renderable())
	{
		if(!m_zoom_params.m_pNight_vision->IsActive())
		{
			CActor *pA = smart_cast<CActor *>(H_Parent());
			R_ASSERT(pA);
			CTorch* pTorch = smart_cast<CTorch*>( pA->inventory().ItemFromSlot(TORCH_SLOT) );
			if ( pTorch && pTorch->GetNightVisionStatus() )
			{
				m_bRememberActorNVisnStatus = pTorch->GetNightVisionStatus();
				pTorch->SwitchNightVision(false, false);
			}
			m_zoom_params.m_pNight_vision->Start(m_zoom_params.m_sUseZoomPostprocess, pA, false);
		}

	}
	else if(m_bRememberActorNVisnStatus)
	{
		m_bRememberActorNVisnStatus = false;
		EnableActorNVisnAfterZoom();
	}

	if (!!GetHUDmode()) {
		m_current_inertion.lerp(m_base_inertion, m_zoom_inertion, m_zoom_params.m_fZoomRotationFactor);
	}

	if(m_zoom_params.m_pVision)
		m_zoom_params.m_pVision->Update();

	ModUpdate();
}

void CWeapon::SetWeaponModelBoneStatus(std::string bone, BOOL show)
{
	if (HudItemData())
		HudItemData()->set_bone_visible(bone.c_str(), show, TRUE);

	IKinematics* pWeaponVisual = Visual()->dcast_PKinematics();
	if (pWeaponVisual && pWeaponVisual->LL_BoneID(bone.c_str()) != BI_NONE)
		pWeaponVisual->LL_SetBoneVisible(pWeaponVisual->LL_BoneID(bone.c_str()), show, FALSE);
}

void CWeapon::SetWeaponMultipleBonesStatus(std::string section, std::string line, BOOL show)
{
	if (!pSettings->section_exist(section.c_str()))
		return;

	if (!!pSettings->line_exist(section.c_str(), line.c_str()))
	{
		LPCSTR	S = pSettings->r_string(section.c_str(), line.c_str());
		if (S && S[0])
		{
			string128 _Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, _Item);
				SetWeaponModelBoneStatus(_Item, show);
			}
		}
	}
}

void CWeapon::ModUpdate()
{
	u32 delta = Device.GetTimeDeltaSafe(_last_update_time);

	if (!IsZoomed() && IsAimStarted)
		IsAimStarted = false;

	ProcessAmmo();
	ProcessAmmoGL();

	ProcessUpgrade();
	ProcessScope();

	if (!H_Parent() || H_Parent() && smart_cast<CEntityAlive*>(H_Parent()))
		ReassignWorldAnims();

	if (!H_Parent())
	{
		lock_time_callback = nullptr;
		lock_time = 0.f;
	}

	if (lock_time > delta)
		lock_time -= delta;
	else
	{
		lock_time = 0.f;
		curr_anim = "";

		if (lock_time_callback != nullptr)
		{
			lock_time_callback(this, lock_time_param);
			lock_time_callback = nullptr;
			lock_time_param = 0;
		}
	}

	_last_update_time = Device.dwTimeGlobal;
}

void CWeapon::ProcessScope()
{
	s32 cur_index;
	if (IsScopeAttached() && get_ScopeStatus() == 2)
		cur_index = m_cur_scope;
	else
		cur_index = -1;

	for (u32 i = 0; i < m_scopes.size(); ++i)
	{
		shared_str tmp = GetScopeSection(i);
		bool status = (i == cur_index);

		if (pSettings->line_exist(tmp, "bones"))
			SetWeaponMultipleBonesStatus(tmp.c_str(), "bones", status);

		if (pSettings->line_exist(tmp, "hide_bones"))
			SetWeaponMultipleBonesStatus(tmp.c_str(), "hide_bones", !status);
	}

	if (cur_index >= 0)
	{
		shared_str tmp = GetScopeSection(cur_index);
		if (pSettings->line_exist(tmp, "overriding_hide_bones"))
			SetWeaponMultipleBonesStatus(tmp.c_str(), "overriding_hide_bones", false);

		if (pSettings->line_exist(tmp, "overriding_show_bones"))
			SetWeaponMultipleBonesStatus(tmp.c_str(), "overriding_show_bones", true);
	}
	else
	{
		SetWeaponMultipleBonesStatus(cNameSect().c_str(), "no_scope_overriding_hide_bones", FALSE);
		SetWeaponMultipleBonesStatus(cNameSect().c_str(), "no_scope_overriding_show_bones", TRUE);
	}
}

void CWeapon::ProcessUpgrade()
{
	if (pSettings->line_exist(cNameSect(), "def_hide_bones"))
		SetWeaponMultipleBonesStatus(cNameSect().c_str(), "def_hide_bones", FALSE);

	if (pSettings->line_exist(cNameSect(), "def_show_bones"))
		SetWeaponMultipleBonesStatus(cNameSect().c_str(), "def_show_bones", TRUE);

	if (!!pSettings->line_exist(cNameSect().c_str(), "upgrades"))
	{
		LPCSTR	S = pSettings->r_string(cNameSect().c_str(), "upgrades");
		if (S && S[0])
		{
			string128 _Item;
			int	count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, _Item);
				HideOneUpgradeLevel(_Item);
			}
		}
	}

	for (u32 i = 0; i < m_upgrades.size(); i++)
	{
		LPCSTR section = pSettings->r_string(m_upgrades.at(i).c_str(), "section");
		if (pSettings->line_exist(section, "hide_bones"))
			SetWeaponMultipleBonesStatus(section, "hide_bones", FALSE);

		if (pSettings->line_exist(section, "show_bones"))
			SetWeaponMultipleBonesStatus(section, "show_bones", TRUE);

		if (pSettings->line_exist(section, "hide_bones_override"))
			SetWeaponMultipleBonesStatus(section, "hide_bones_override", false);

		if (IsSilencerAttached())
		{
			if (pSettings->line_exist(section, "hide_bones_override_when_silencer_attached"))
				SetWeaponMultipleBonesStatus(section, "hide_bones_override_when_silencer_attached", false);
		}

		if (IsScopeAttached())
		{
			if (pSettings->line_exist(section, "hide_bones_override_when_scope_attached"))
				SetWeaponMultipleBonesStatus(section, "hide_bones_override_when_scope_attached", false);
		}

		if (get_GrenadeLauncherStatus() == 1 || get_GrenadeLauncherStatus() == 2 && IsGrenadeLauncherAttached())
		{
			if (pSettings->line_exist(section, "hide_bones_override_when_gl_attached"))
				SetWeaponMultipleBonesStatus(section, "hide_bones_override_when_gl_attached", false);
		}

	}

	if (get_GrenadeLauncherStatus() == 1 || get_GrenadeLauncherStatus() == 2 && IsGrenadeLauncherAttached())
	{
		if (pSettings->line_exist(cNameSect().c_str(), "def_hide_bones_override_when_gl_attached"))
			SetWeaponMultipleBonesStatus(cNameSect().c_str(), "def_hide_bones_override_when_gl_attached", false);
	}
}

void CWeapon::HideOneUpgradeLevel(std::string section)
{
	if (!!pSettings->line_exist(section.c_str(), "elements"))
	{
		LPCSTR	S = pSettings->r_string(section.c_str(), "elements");
		if (S && S[0])
		{
			string128 _Item;
			int	count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, _Item);
				if (!!pSettings->line_exist(_Item, "effects"))
				{
					LPCSTR St = pSettings->r_string(_Item, "effects");
					if (St && St[0])
					{
						string128 _tmp = "";
						int	cnt = _GetItemCount(St);
						for (int itr = 0; itr < cnt; ++itr)
						{
							_GetItem(St, itr, _tmp);
							HideOneUpgradeLevel(_tmp);
						}
					}
				}

				LPCSTR up_sect = pSettings->r_string(_Item, "section");
				SetWeaponMultipleBonesStatus(up_sect, "show_bones", FALSE);
			}
		}
	}
}

void CWeapon::EnableActorNVisnAfterZoom()
{
	CActor *pA = smart_cast<CActor *>(H_Parent());
	if(IsGameTypeSingle() && !pA)
		pA = g_actor;

	if(pA)
	{
		CTorch* pTorch = smart_cast<CTorch*>( pA->inventory().ItemFromSlot(TORCH_SLOT) );
		if ( pTorch )
		{
			pTorch->SwitchNightVision(true, false);
			pTorch->GetNightVision()->PlaySounds(CNightVisionEffector::eIdleSound);
		}
	}
}

u32 CWeapon::PlayHUDMotion(xr_string M, BOOL bMixIn, u32 state, bool lock_shooting, bool need_suffix, TAnimationEffector fun, int param)
{
	if (need_suffix)
		M = NeedAddSuffix(M);

	u32 result = CHudItem::PlayHUDMotion(M, bMixIn, this, state, need_suffix);

	MakeLockByConfigParam("lock_time_" + M, lock_shooting, fun, param);

	if (lock_time > 0.f)
		curr_anim = M;

	return result;
}

void CWeapon::MakeLockByConfigParam(xr_string key, bool lock_shooting, TAnimationEffector fun, int param)
{
	if (pSettings->line_exist(hud_sect, key.c_str()))
	{
		float time = pSettings->r_float(hud_sect, key.c_str());
		lock_time = floor(time * 1000.f);

		if (lock_shooting)
			fShotTimeCounter = floor(time * 1000.f);

		if (fun != nullptr)
		{
			lock_time_callback = fun;
			lock_time_param = param;
		}
	}
}

xr_string CWeapon::AddSuffixName(xr_string M, xr_string suffix, xr_string test_suffix)
{
	xr_string new_name = M + suffix;
	xr_string test_name = new_name + test_suffix;

	if (HudAnimationExist(new_name.c_str()))
		return new_name;
	else if (HudAnimationExist(test_name.c_str()))
		return test_name;

	return M;
}

xr_string CWeapon::GetActualCurrentAnim() const
{
	if (curr_anim != "")
		return curr_anim;
	else if (m_current_motion != nullptr)
		return m_current_motion.c_str();

	return "";
}

u8 CWeapon::GetAmmoTypeIndex(bool second)
{
	if (second)
	{
		CWeaponMagazinedWGrenade* wpn_mag_gl = smart_cast<CWeaponMagazinedWGrenade*>(this);
		if (wpn_mag_gl)
			return wpn_mag_gl->m_ammoType2;
	}

	return m_ammoType;
}

u8 CWeapon::GetAmmoTypeToReload()
{
	u8 result = m_set_next_ammoType_on_reload;
	if (result == undefined_ammo_type)
		result = GetAmmoTypeIndex();

	return result;
}

u8 CWeapon::GetOrdinalAmmoType()
{
	if (READ_IF_EXISTS(pSettings, r_bool, hud_sect, "ammo_params_use_previous_shot_type", false))
		return _last_shot_ammotype;
	else if (READ_IF_EXISTS(pSettings, r_bool, hud_sect, "ammo_params_use_last_cartridge_type", false) && m_magazine.size() > 0)
		return GetCartridgeType(GetCartridgeFromMagVector(m_magazine.size() - 1));
	else
		return GetAmmoTypeIndex(IsGrenadeMode());
}

u8 CWeapon::GetGlAmmotype()
{
	return GetAmmoTypeIndex(!IsGrenadeMode());
}

u8 CWeapon::GetCartridgeType(CCartridge* c)
{
	return c->m_LocalAmmoType;
}

CCartridge* CWeapon::GetCartridgeFromMagVector(u32 index)
{
	if (index >= m_magazine.size())
		return nullptr;

	if (IsGrenadeLauncherAttached() && IsGrenadeMode())
	{
		CWeaponMagazinedWGrenade* wpn_gl = static_cast<CWeaponMagazinedWGrenade*>(this);
		return &(wpn_gl->m_magazine2[index]);
	}
	else
		return &(m_magazine[index]);
}

CCartridge* CWeapon::GetGrenadeCartridgeFromGLVector(u32 index)
{
	if (!IsGrenadeLauncherAttached() || index >= GetAmmoInGLCount())
		return nullptr;

	CWeaponMagazinedWGrenade* wpn_gl = smart_cast<CWeaponMagazinedWGrenade*>(this);

	if (!wpn_gl)
		return nullptr;

	if (IsGrenadeMode())
		return &(wpn_gl->m_magazine[index]);
	else
		return &(wpn_gl->m_magazine2[index]);
}

u32 CWeapon::GetAmmoInGLCount()
{
	u32 result = 0;

	if (get_GrenadeLauncherStatus() == 0 || (get_GrenadeLauncherStatus() == 2) && !IsGrenadeLauncherAttached())
		return result;

	CWeaponMagazinedWGrenade* wpngl = smart_cast<CWeaponMagazinedWGrenade*>(this);

	if (wpngl)
	{
		if (IsGrenadeMode())
			result = m_magazine.size();
		else
			result = wpngl->m_magazine2.size();
	}

	return result;
}

int CWeapon::GetMagCapacity()
{
	int result = 0;
	int ammotype = -1;

	if (IsGrenadeLauncherAttached() && IsGrenadeMode())
	{
		CWeaponMagazinedWGrenade* w_gl = static_cast<CWeaponMagazinedWGrenade*>(this);
		result = w_gl->iMagazineSize2;
	}
	else
	{
		result = iMagazineSize;
		ammotype = GetAmmoTypeToReload();
	}

	if (ammotype >= 0)
	{
		xr_string param = "ammo_mag_size_for_type_" + xr_string().ToString(ammotype);
		result = READ_IF_EXISTS(pSettings, r_u32, cNameSect(), param.c_str(), result);
		result = FindIntValueInUpgradesDef(param.c_str(), result);
	}

	return result;
}

void CWeapon::ProcessAmmo(bool forced)
{
    if (READ_IF_EXISTS(pSettings, r_bool, hud_sect, "use_advanced_ammo_bones", false))
	{
        ProcessAmmoAdv(forced);
        return;
    }

    if (!READ_IF_EXISTS(pSettings, r_bool, hud_sect, "use_ammo_bones", false))
        return;

    xr_string prefix = pSettings->r_string(hud_sect, "ammo_bones_prefix");
    xr_string prefix_hide = READ_IF_EXISTS(pSettings, r_string, hud_sect, "ammo_hide_bones_prefix", "");
    xr_string prefix_var = READ_IF_EXISTS(pSettings, r_string, hud_sect, "ammo_var_bones_prefix", "");
    int start_index = READ_IF_EXISTS(pSettings, r_u32, hud_sect, "start_ammo_bone_index", 0);
    int limitator = READ_IF_EXISTS(pSettings, r_u32, hud_sect, "end_ammo_bone_index", 0);

    int finish_index = start_index + m_magazine.size() - 1;

    if (IsMisfire() && pSettings->line_exist(hud_sect, "additional_ammo_bone_when_jammed") && pSettings->r_bool(hud_sect, "additional_ammo_bone_when_jammed"))
        finish_index += 1;

    if (pSettings->line_exist(hud_sect, "ammo_divisor_up"))
        finish_index = ceil(finish_index / READ_IF_EXISTS(pSettings, r_u32, hud_sect, "ammo_divisor_up", 1));
	else if (pSettings->line_exist(hud_sect, "ammo_divisor_down"))
        finish_index = floor(finish_index / READ_IF_EXISTS(pSettings, r_u32, hud_sect, "ammo_divisor_down", 1));

    if (finish_index > limitator)
        finish_index = limitator;

    for (int i = start_index; i <= finish_index; i++)
	{
        SetWeaponModelBoneStatus((prefix + xr_string().ToString(i)).c_str(), TRUE);
        if (prefix_hide.length() > 0)
            SetWeaponModelBoneStatus((prefix_hide + xr_string().ToString(i)).c_str(), FALSE);
    }

    for (int i = finish_index + 1; i <= limitator; i++)
	{
        SetWeaponModelBoneStatus((prefix + xr_string().ToString(i)).c_str(), FALSE);
        if (prefix_hide.length() > 0)
            SetWeaponModelBoneStatus((prefix_hide + xr_string().ToString(i)).c_str(), TRUE);
    }

    if (prefix_var.length() > 0)
	{
        for (int i = start_index - 1; i <= limitator; i++)
		{
            SetWeaponModelBoneStatus((prefix_var + xr_string().ToString(i)).c_str(), i == finish_index);
        }
    }
}

void CWeapon::ProcessAmmoAdv(bool forced)
{
    if (!IsGrenadeMode() && (GetState() == eFire) && !forced && READ_IF_EXISTS(pSettings, r_bool, hud_sect, "ammo_params_toggle_shooting", false))
        return;

    int cnt = m_magazine.size();
    int ammotype = 0;
    shared_str bones_sect = nullptr;

    if (IsGrenadeMode())
        ammotype = GetOrdinalAmmoType();
	else if (GetState() == eUnjam)
	{
		if (IsMisfire())
			ammotype = GetOrdinalAmmoType();
	}
	else if (GetState() == eReload)
	{
		if (IsTriStateReload())
		{
            if (strncmp(GetActualCurrentAnim().c_str(), "anm_open", strlen("anm_open")) == 0)
			{
                xr_string param = "ejection_delay_" + GetActualCurrentAnim();
				u32 ejection_delay = READ_IF_EXISTS(pSettings, r_u32, hud_sect, param.c_str(), 0);
                if (ejection_delay > 0 && Device.GetTimeDeltaSafe(m_dwMotionStartTm, m_dwMotionCurrTm) < ejection_delay)
                    return;
            }

            ammotype = GetAmmoTypeToReload();
        }
		else
		{
            ammotype = GetAmmoTypeIndex(IsGrenadeMode());

            if (READ_IF_EXISTS(pSettings, r_bool, hud_sect, "minus_ammo_in_usual_reloads", false) && cnt >= 1 && strncmp(GetActualCurrentAnim().c_str(), "_empty", strlen("_empty")) != 0)
                cnt -= 1;
        }
    }
	else
	{
        ammotype = GetOrdinalAmmoType();

        if (READ_IF_EXISTS(pSettings, r_bool, hud_sect, "minus_ammo_in_bore", false) && cnt >= 1 && strncmp(GetActualCurrentAnim().c_str(), "anm_bore", strlen("anm_bore")) == 0)
            cnt -= 1;
    }

	xr_string sect_w_ammotype = "ammo_params_section_" + xr_string().ToString(ammotype);

    if (pSettings->line_exist(hud_sect, sect_w_ammotype.c_str()))
        bones_sect = pSettings->r_string(hud_sect, sect_w_ammotype.c_str());
	else if (pSettings->line_exist(hud_sect, "ammo_params_section"))
        bones_sect = pSettings->r_string(hud_sect, "ammo_params_section");

    if (bones_sect != nullptr)
	{
        if (IsMisfire() && pSettings->line_exist(bones_sect, "additional_ammo_bone_when_jammed") && pSettings->r_bool(bones_sect, "additional_ammo_bone_when_jammed"))
            cnt += 1;

        SetWeaponMultipleBonesStatus(bones_sect.c_str(), "all_bones", FALSE);

        xr_string configuration = "configuration_" + xr_string().ToString(cnt);
        SetWeaponMultipleBonesStatus(bones_sect.c_str(), configuration.c_str(), TRUE);
    }
}

void CWeapon::ProcessAmmoGL(bool forced)
{
	if (get_GrenadeLauncherStatus() == 0)
		return;

	int cnt = GetAmmoInGLCount();
	int ammotype = 0;
	if (IsGrenadeMode() && (GetState() == eReload))
	{
		if (cnt == 0)
		{
			ammotype = GetAmmoTypeToReload();
			cnt = 1;
		}
		else
			ammotype = GetGlAmmotype();
	}
	else
		ammotype = GetGlAmmotype();

	xr_string sect_w_ammotype = "gl_ammo_params_section_" + xr_string().ToString(ammotype);

	shared_str bones_sect = nullptr;
	if (pSettings->line_exist(hud_sect, sect_w_ammotype.c_str()))
		bones_sect = pSettings->r_string(hud_sect, sect_w_ammotype.c_str());
	else if (pSettings->line_exist(hud_sect, "gl_ammo_params_section"))
		bones_sect = pSettings->r_string(hud_sect, "gl_ammo_params_section");

	if (bones_sect != nullptr)
	{
		SetWeaponMultipleBonesStatus(bones_sect.c_str(), "all_bones", false);

		xr_string configuration = "configuration_" + xr_string().ToString(cnt);
		SetWeaponMultipleBonesStatus(bones_sect.c_str(), configuration.c_str(), true);
	}
}

bool CWeapon::IsCollimatorInstalled()
{
	if (!IsScopeAttached() || get_ScopeStatus() != 2)
		return false;

	shared_str scope = nullptr;
	scope = GetCurrentScopeSection();
	scope = pSettings->r_string(scope, "scope_name");

	return READ_IF_EXISTS(pSettings, r_bool, scope, "collimator", false);
}

bool CWeapon::IsHudModelForceUnhide()
{
	return IsCollimatorInstalled() /* || IsLensedScopeInstalled(wpn) && IsLensEnabled() || IsAlterZoomMode()*/;
}

bool CWeapon::IsUIForceUnhiding()
{
	bool result = IsHudModelForceUnhide();

	if (result)
	{
		/*if (buf.IsAlterZoomMode())
			result = true;
		else */if (get_ScopeStatus() == 1)
			result = !READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "zoom_hide_ui", false);
		else if (get_ScopeStatus() == 2 && IsScopeAttached())
			result = !READ_IF_EXISTS(pSettings, r_bool, pSettings->r_string(GetCurrentScopeSection(), "scope_name"), "zoom_hide_ui", false);
	}

	return result;
}

bool CWeapon::IsUIForceHiding()
{
	CWeaponBinoculars* bino = smart_cast<CWeaponBinoculars*>(this);

	if (bino && IsZoomed())
		return READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "zoom_hide_ui", true);
	else if (get_ScopeStatus() == 1 && IsZoomed())
		return READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "zoom_hide_ui", false);
	else if (get_ScopeStatus() == 2 && IsScopeAttached() && IsZoomed())
		return READ_IF_EXISTS(pSettings, r_bool, pSettings->r_string(GetCurrentScopeSection(), "scope_name"), "zoom_hide_ui", false);
	else
		return false;
}

shared_str CWeapon::FindStrValueInUpgradesDef(shared_str key, shared_str def)
{
	for (int i = 0; i < m_upgrades.size(); ++i)
	{
		shared_str str = m_upgrades[i];
		str = pSettings->r_string(str, "section");
		if (pSettings->line_exist(str, key))
		{
			shared_str result = pSettings->r_string(str.c_str(), key.c_str());
			if (strcmp(result.c_str(), def.c_str()) != 0)
				return result;
		}
	}
	return def;
}

float CWeapon::ModifyFloatUpgradedValue(shared_str key, float def)
{
	float result = def;
	for (int i = 0; i < m_upgrades.size(); ++i)
	{
		shared_str str = m_upgrades[i];
		str = pSettings->r_string(str, "section");
		if (pSettings->line_exist(str, key))
			result += READ_IF_EXISTS(pSettings, r_float, str.c_str(), key.c_str(), 0.0f);
	}
	return result;
}

int CWeapon::FindIntValueInUpgradesDef(shared_str key, int def)
{
	int result = def;
	for (int i = 0; i < m_upgrades.size(); ++i)
	{
		shared_str str = m_upgrades[i];
		str = pSettings->r_string(str, "section");
		if (pSettings->line_exist(str, key))
			result = READ_IF_EXISTS(pSettings, r_u32, str.c_str(), key.c_str(), def);
	}
	return result;
}

bool CWeapon::FindBoolValueInUpgradesDef(shared_str key, bool def, bool scan_after_nodefault)
{
	bool result = def;
	for (int i = 0; i < m_upgrades.size(); ++i)
	{
		shared_str str = m_upgrades[i];
		str = pSettings->r_string(str, "section");
		if (pSettings->line_exist(str, key))
		{
			result = pSettings->r_bool(str.c_str(), key.c_str());
			if (!scan_after_nodefault && result != def)
				return result;
		}
	}
	return result;
}

bool CWeapon::IsActionProcessing()
{
	return H_Parent() && (lock_time > 0.f /* || ParentIsActor() && IsActorSuicideNow() || IsActorPlanningSuicide()*/);
}

void CWeapon::MakeWeaponKick(Fvector3& pos, Fvector3& dir)
{
	shared_str sect = cNameSect();
	shared_str material = nullptr;
	CCartridge c;

	material = READ_IF_EXISTS(pSettings, r_string, sect, "kick_material", "objects\\knife");

	material = FindStrValueInUpgradesDef("kick_material", material);

	c.param_s.buckShot = 1;
	c.param_s.impair = 1.0f;
	c.param_s.kDisp = 1.0f;
	c.param_s.kHit = 1.0f;
	c.param_s.kImpulse = 1.0f;
	c.param_s.kAP = ModifyFloatUpgradedValue("kick_ap", READ_IF_EXISTS(pSettings, r_float, sect, "kick_ap", EPS_L));
	c.param_s.fWallmarkSize = ModifyFloatUpgradedValue("kick_wallmark_size", READ_IF_EXISTS(pSettings, r_float, sect, "kick_wallmark_size", 0.05f));
	c.bullet_material_idx = GMLib.GetMaterialIdx(material.c_str());
	c.param_s.u8ColorID = 0;
	c.m_LocalAmmoType = 0;
	c.param_s.kAirRes = 1.0f;
	c.m_InvShortName = nullptr;

	int cnt = FindIntValueInUpgradesDef("kick_hit_count", READ_IF_EXISTS(pSettings, r_u32, sect, "kick_hit_count", 1));
	float hp = ModifyFloatUpgradedValue("kick_hit_power", READ_IF_EXISTS(pSettings, r_float, sect, "kick_hit_power", 0.0f));
	float imp = ModifyFloatUpgradedValue("kick_hit_impulse", READ_IF_EXISTS(pSettings, r_float, sect, "kick_hit_impulse", 0.0f));
	int htype = FindIntValueInUpgradesDef("kick_hit_type", READ_IF_EXISTS(pSettings, r_u32, sect, "kick_hit_type", ALife::EHitType::eHitTypeWound));
	float hdist = ModifyFloatUpgradedValue("kick_distance", READ_IF_EXISTS(pSettings, r_float, sect, "kick_distance", 0.0f));

	float disp_hor = ModifyFloatUpgradedValue("kick_disp_hor", READ_IF_EXISTS(pSettings, r_float, sect, "kick_disp_hor", 0.0f));
	float disp_ver = ModifyFloatUpgradedValue("kick_disp_ver", READ_IF_EXISTS(pSettings, r_float, sect, "kick_disp_ver", 0.0f));

	Level().BulletManager().AddBullet(pos, dir, 10000.f, 0.f, 0.f, 0.f, ID(), ALife::EHitType(htype), hdist, c, 1.0f, true, false);

	c.bullet_material_idx = GMLib.GetMaterialIdx("objects\\clothes");
	c.param_s.fWallmarkSize = 0.0001f;

	Fvector3 tmpdir, right, up;
	for (int i = 0; i < cnt; ++i)
	{
		tmpdir = dir;
		tmpdir.generate_orthonormal_basis_normalized(tmpdir, up, right);

		up.mul(disp_ver);
		right.mul(disp_hor);

		tmpdir.sub(up);
		tmpdir.sub(right);

		up.mul(2.f * i / static_cast<float>(cnt));
		right.mul(2.f * i / static_cast<float>(cnt));

		tmpdir.add(up);
		tmpdir.add(right);

		Level().BulletManager().AddBullet(pos, tmpdir, 10000.f, hp, imp, 0.f, ID(), ALife::EHitType(htype), hdist, c, 1.0f, true, false);
	}
}

void CWeapon::ReassignWorldAnims()
{
	if (!_wanim_force_assign)
		return;

	if (!READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "use_world_anims", false))
		return;

	xr_string anm;
	switch (GetState())
	{
	case eIdle:
		anm = "wanm_idle";
		break;
	case eShowing:
		anm = "wanm_draw";
		break;
	case eHiding:
		anm = "wanm_holster";
		break;
	case eFire:
		anm = "wanm_shoot";
		if (m_magazine.size() <= 0 && pSettings->line_exist(cNameSect(), (anm + "_last").c_str()))
			anm += "_last";

		break;
	case eReload:
		anm = "wanm_reload";
		break;
	default:
		anm = "wanm_idle";
		break;
	}

	xr_string result = READ_IF_EXISTS(pSettings, r_string, cNameSect(), anm.c_str(), "");

	if (result.length() == 0)
		anm = "wanm_idle";

	if (IsMisfire() && pSettings->line_exist(cNameSect(), (anm + "_jammed").c_str()))
		anm += "_jammed";
	else if (m_magazine.size() <= 0 && GetState() != eFire && pSettings->line_exist(cNameSect(), (anm + "_empty").c_str()))
		anm += "_empty";
	/*else if (IsFirstShotAnimationNeeded() && IsJustAfterReload() && GetState() != eFire)
		AddSuffixIfStringExist(cNameSect(), "_first", anm);*/

	if (pSettings->line_exist(cNameSect(), (anm + GetFiremodeSuffix()).c_str()))
		anm += GetFiremodeSuffix();

	if (IsGrenadeMode() && pSettings->line_exist(cNameSect(), (anm + "_g").c_str()))
		anm += "_g";
	else if (IsGrenadeLauncherAttached() && pSettings->line_exist(cNameSect(), (anm + "_w_gl").c_str()))
		anm += "_w_gl";

	IKinematics* pWeaponVisual = smart_cast<IKinematics*>(Visual());

	if (pWeaponVisual && pWeaponVisual->dcast_PKinematicsAnimated())
	{
		MotionID m = pWeaponVisual->dcast_PKinematicsAnimated()->ID_Cycle(pSettings->r_string(cNameSect(), anm.c_str()));
		pWeaponVisual->dcast_PKinematicsAnimated()->PlayCycle(m, true);
		_wanim_force_assign = false;
	}
}

bool  CWeapon::need_renderable()
{
	return !(IsZoomed() && ZoomTexture() && !IsRotatingToZoom() && !IsHudModelForceUnhide());
}

void CWeapon::renderable_Render		()
{
	UpdateXForm				();

	//нарисовать подсветку

	RenderLight				();	

	//если мы в режиме снайперки, то сам HUD рисовать не надо
	if(IsZoomed() && !IsRotatingToZoom() && ZoomTexture() && !IsHudModelForceUnhide())
		RenderHud		(FALSE);
	else
		RenderHud		(TRUE);

	inherited::renderable_Render	();
}

void CWeapon::signal_HideComplete()
{
	if(H_Parent()) 
		setVisible			(FALSE);
	SetPending				(FALSE);
}

void CWeapon::SetDefaults()
{
	SetPending			(FALSE);

	m_flags.set			(FUsingCondition, TRUE);
	bMisfire			= false;
	m_flagsAddOnState	= 0;
	m_zoom_params.m_bIsZoomModeNow	= false;
}

void CWeapon::UpdatePosition(const Fmatrix& trans)
{
	Position().set		(trans.c);
	if (m_strapped_mode || m_strapped_mode_rifle)
		XFORM().mul(trans, m_StrapOffset);
	else
		XFORM().mul(trans, m_Offset);

	VERIFY				(!fis_zero(DET(renderable.xform)));
}

void CWeapon::UpdatePosition_alt(const Fmatrix& trans) {
	Position().set(trans.c);
	if (m_strapped_mode || m_strapped_mode_rifle)
		XFORM().mul(trans, m_StrapOffset_alt);
	else
		XFORM().mul(trans, m_Offset);

	VERIFY(!fis_zero(DET(renderable.xform)));
}

bool CWeapon::Action_PrepareEarlyShotInReload()
{
	bool result = false;

	if (!IsReloaded && !IsTriStateReload())
	{
		DoReload();
		IsReloaded = true;
	}

	bReloadKeyPressed = false;
	bAmmotypeKeyPressed = false;

	if (iAmmoElapsed <= 0)
		return result;

	lock_time = 0.f;
	SetPending(FALSE);

	result = true;
	return result;
}

bool CWeapon::OnActWhileReload_CanActNow() const
{
	bool result = false;

	if (GetState() == eReload || GetState() == eReload && m_sub_state == eSubstateReloadEnd)
	{
		xr_string param = "early_reload_end_delta_" + GetActualCurrentAnim();
		u32 delay = static_cast<u32>(READ_IF_EXISTS(pSettings, r_float, HudSection(), param.c_str(), 0.f) * 1000.f);
		result = Device.GetTimeDeltaSafe(m_dwMotionCurrTm, m_dwMotionEndTm) < delay;
	}

	return result;
}

bool CWeapon::CanAimNow()
{
	if (IsGrenadeLauncherAttached() && IsGrenadeMode())
	{
		shared_str sect = "";
		if (IsScopeAttached())
			sect = GetCurrentScopeSection();
		else
			sect = HudSection();

		if (READ_IF_EXISTS(pSettings, r_bool, sect, "prohibit_aim_for_grenade_mode", false))
			return false;
	}

	return true;
}

bool CWeapon::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;

	
	switch(cmd) 
	{
		case kWPN_FIRE:
			{
				if (IsTriStateReload() && GetState() == eReload && (m_sub_state == eSubstateReloadInProcess || m_sub_state == eSubstateReloadBegin) && flags & CMD_START)
				{
					bStopReloadSignal = true;
					return true;
				}

				if (OnActWhileReload_CanActNow())
				{
					if (!Action_PrepareEarlyShotInReload())
						return false;
				}
				else if (IsPending())
					return false;

				if (flags&CMD_START) 
					FireStart();
				else 
					FireEnd();

				return true;
			} 
		case kQUICK_KICK:
		{
			if (!(flags & CMD_START))
				return false;

			if (FindBoolValueInUpgradesDef("disable_kick_anim", READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "disable_kick_anim", false), true))
				return false;

			if (IsScopeAttached() && FindBoolValueInUpgradesDef("disable_kick_anim_when_scope_attached", READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "disable_kick_anim_when_scope_attached", false)))
				return false;

			if (IsSilencerAttached() && FindBoolValueInUpgradesDef("disable_kick_anim_when_sil_attached", READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "disable_kick_anim_when_sil_attached", false), true))
				return false;

			if (IsGrenadeMode() && FindBoolValueInUpgradesDef("disable_kick_anim_when_gl_enabled", READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "disable_kick_anim_when_gl_enabled", false), true))
				return false;

			if (IsGrenadeLauncherAttached() && FindBoolValueInUpgradesDef("disable_kick_anim_when_gl_attached", READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "disable_kick_anim_when_gl_attached", false), true))
				return false;

			if (IsZoomed())
				return false;

			if (GetState() == eIdle || GetState() == eKick && !lock_time)
			{
				SwitchState(eKick);
				return true;
			}

			return false;
		}
		case kWPN_NEXT: 
			{
				return SwitchAmmoType(flags);
			} 

		case kWPN_ZOOM:
			if (IsZoomEnabled())
			{
				if (b_toggle_weapon_aim)
				{
					if (flags & CMD_START)
					{
						if (!IsZoomed() && CanAimNow())
						{
							if (!IsPending())
							{
								if (GetState() != eIdle)
									SwitchState(eIdle);

								OnZoomIn();
							}
						}
						else
							OnZoomOut();
					}
				}
				else
				{
					if (flags & CMD_START)
					{
						if (!IsZoomed() && !IsPending() && CanAimNow())
						{
							if (GetState() != eIdle)
								SwitchState(eIdle);

							OnZoomIn();
						}
					}
					else 
						if (IsZoomed())
							OnZoomOut();
				}
				return true;
			}
			else 
				return false;

		case kWPN_ZOOM_INC:
		case kWPN_ZOOM_DEC:
			if (IsZoomEnabled() && IsZoomed() && (flags & CMD_START))
			{
				if(cmd==kWPN_ZOOM_INC)  ZoomInc();
				else					ZoomDec();
				return true;
			}else
				return false;
	}
	return false;
}

bool CWeapon::SwitchAmmoType(u32 flags) 
{
	if (OnClient())
		return false;

	if (IsPending())
		return false;

	if (IsTriStateReload() && iAmmoElapsed == iMagazineSize)
		return false;

	if (GetState() != eIdle)
		return false;

	if (IsZoomed())
		return false;

	if (!(flags & CMD_START))
		return false;

	if (!bUnjamKeyPressed && !bReloadKeyPressed && !bAmmotypeKeyPressed)
	{
		if (IsMisfire())
			bUnjamKeyPressed = true;
		else
			bAmmotypeKeyPressed = true;
	}
	else
		return false;

	if (GetDetector() && GetDetector()->GetState() != CCustomDetector::eIdle)
		return false;

	if (IsMisfire())
	{
		SwitchState(eUnjam);
		return false;
	}

	u8 l_newType = m_ammoType;
	bool b1, b2;
	do 
	{
		l_newType = u8((u32(l_newType+1)) % m_ammoTypes.size());
		b1 = (l_newType != m_ammoType);
		b2 = unlimited_ammo() ? false : (!m_pInventory->GetAny(m_ammoTypes[l_newType].c_str()));						
	} while(b1 && b2);

	if (l_newType != m_ammoType)
	{
		m_set_next_ammoType_on_reload = l_newType;
		if (OnServer())
			return TryReload();
	}
	else
		bAmmotypeKeyPressed = false;

	return false;
}

void CWeapon::SpawnAmmo(u32 boxCurr, LPCSTR ammoSect, u32 ParentID) 
{
	if(!m_ammoTypes.size())			return;
	if (OnClient())					return;
	m_bAmmoWasSpawned				= true;
	
	int l_type						= 0;
	l_type							%= m_ammoTypes.size();

	if(!ammoSect) ammoSect			= m_ammoTypes[l_type].c_str(); 
	
	++l_type; 
	l_type							%= m_ammoTypes.size();

	CSE_Abstract *D					= F_entity_Create(ammoSect);

	{	
		CSE_ALifeItemAmmo *l_pA		= smart_cast<CSE_ALifeItemAmmo*>(D);
		R_ASSERT					(l_pA);
		l_pA->m_boxSize				= (u16)pSettings->r_s32(ammoSect, "box_size");
		D->s_name					= ammoSect;
		D->set_name_replace			("");
//.		D->s_gameid					= u8(GameID());
		D->s_RP						= 0xff;
		D->ID						= 0xffff;
		if (ParentID == 0xffffffff)	
			D->ID_Parent			= (u16)H_Parent()->ID();
		else
			D->ID_Parent			= (u16)ParentID;

		D->ID_Phantom				= 0xffff;
		D->s_flags.assign			(M_SPAWN_OBJECT_LOCAL);
		D->RespawnTime				= 0;
		l_pA->m_tNodeID				= g_dedicated_server ? u32(-1) : ai_location().level_vertex_id();

		if(boxCurr == 0xffffffff) 	
			boxCurr					= l_pA->m_boxSize;

		while(boxCurr) 
		{
			l_pA->a_elapsed			= (u16)(boxCurr > l_pA->m_boxSize ? l_pA->m_boxSize : boxCurr);
			NET_Packet				P;
			D->Spawn_Write			(P, TRUE);
			Level().Send			(P,net_flags(TRUE));

			if(boxCurr > l_pA->m_boxSize) 
				boxCurr				-= l_pA->m_boxSize;
			else 
				boxCurr				= 0;
		}
	}
	F_entity_Destroy				(D);
}

int CWeapon::GetSuitableAmmoTotal( bool use_item_to_spawn ) const
{
	int ae_count = iAmmoElapsed;
	if ( !m_pInventory )
	{
		return ae_count;
	}

	//чтоб не делать лишних пересчетов
	if ( m_pInventory->ModifyFrame() <= m_BriefInfo_CalcFrame )
	{
		return ae_count + m_iAmmoCurrentTotal;
	}
	m_BriefInfo_CalcFrame = Device.dwFrame;

	m_iAmmoCurrentTotal = 0;
	for ( u8 i = 0; i < u8(m_ammoTypes.size()); ++i ) 
	{
		m_iAmmoCurrentTotal += GetAmmoCount_forType( m_ammoTypes[i] );

		if ( !use_item_to_spawn )
		{
			continue;
		}
		if ( !inventory_owner().item_to_spawn() )
		{
			continue;
		}
		m_iAmmoCurrentTotal += inventory_owner().ammo_in_box_to_spawn();
	}
	return ae_count + m_iAmmoCurrentTotal;
}

int CWeapon::GetAmmoCount( u8 ammo_type ) const
{
	VERIFY( m_pInventory );
	R_ASSERT( ammo_type < m_ammoTypes.size() );

	return GetAmmoCount_forType( m_ammoTypes[ammo_type] );
}

int CWeapon::GetAmmoCount_forType( shared_str const& ammo_type ) const
{
	int res = 0;

	TIItemContainer::iterator itb = m_pInventory->m_belt.begin();
	TIItemContainer::iterator ite = m_pInventory->m_belt.end();
	for ( ; itb != ite; ++itb ) 
	{
		CWeaponAmmo*	pAmmo = smart_cast<CWeaponAmmo*>( *itb );
		if ( pAmmo && (pAmmo->cNameSect() == ammo_type) )
		{
			res += pAmmo->m_boxCurr;
		}
	}

	itb = m_pInventory->m_ruck.begin();
	ite = m_pInventory->m_ruck.end();
	for ( ; itb != ite; ++itb ) 
	{
		CWeaponAmmo*	pAmmo = smart_cast<CWeaponAmmo*>( *itb );
		if ( pAmmo && (pAmmo->cNameSect() == ammo_type) )
		{
			res += pAmmo->m_boxCurr;
		}
	}
	return res;
}

float CWeapon::GetConditionMisfireProbability() const
{
// modified by Peacemaker [17.10.08]
//	if(GetCondition() > 0.95f) 
//		return 0.0f;
	if(GetCondition() > misfireStartCondition) 
		return 0.0f;
	if(GetCondition() < misfireEndCondition) 
		return misfireEndProbability;
//	float mis = misfireProbability+powf(1.f-GetCondition(), 3.f)*misfireConditionK;
	float mis = misfireStartProbability + (
		(misfireStartCondition - GetCondition()) *				// condition goes from 1.f to 0.f
		(misfireEndProbability - misfireStartProbability) /		// probability goes from 0.f to 1.f
		((misfireStartCondition == misfireEndCondition) ?		// !!!say "No" to devision by zero
			misfireStartCondition : 
			(misfireStartCondition - misfireEndCondition))
										  );
	clamp(mis,0.0f,0.99f);
	return mis;
}

BOOL CWeapon::CheckForMisfire	()
{
	if (OnClient()) return FALSE;

	float rnd = ::Random.randF(0.f,1.f);
	float mp = GetConditionMisfireProbability();
	if (rnd < mp)
	{
		FireEnd();
		bMisfire = true;

		bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
		if (!isGuns)
			SwitchState(eMisfire);		
		
		return TRUE;
	}

	return FALSE;
}

BOOL CWeapon::IsMisfire() const
{	
	return bMisfire;
}

bool CWeapon::IsGrenadeLauncherAttached() const
{
	return (ALife::eAddonAttachable == m_eGrenadeLauncherStatus &&
			0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher)) || 
			ALife::eAddonPermanent == m_eGrenadeLauncherStatus;
}

bool CWeapon::IsScopeAttached() const
{
	return (ALife::eAddonAttachable == m_eScopeStatus &&
			0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonScope)) || 
			ALife::eAddonPermanent == m_eScopeStatus;

}

bool CWeapon::IsSilencerAttached() const
{
	return (ALife::eAddonAttachable == m_eSilencerStatus &&
			0 != (m_flagsAddOnState&CSE_ALifeItemWeapon::eWeaponAddonSilencer)) || 
			ALife::eAddonPermanent == m_eSilencerStatus;
}

bool CWeapon::GrenadeLauncherAttachable()
{
	return (ALife::eAddonAttachable == m_eGrenadeLauncherStatus);
}
bool CWeapon::ScopeAttachable()
{
	return (ALife::eAddonAttachable == m_eScopeStatus);
}
bool CWeapon::SilencerAttachable()
{
	return (ALife::eAddonAttachable == m_eSilencerStatus);
}

shared_str wpn_scope				= "wpn_scope";
shared_str wpn_silencer				= "wpn_silencer";
shared_str wpn_grenade_launcher		= "wpn_launcher";




void CWeapon::UpdateHUDAddonsVisibility()
{//actor only
	if(!GetHUDmode())
		return;

	if(ScopeAttachable())
	{
		HudItemData()->set_bone_visible(wpn_scope, IsScopeAttached() );
	}

	if(m_eScopeStatus==ALife::eAddonDisabled )
	{
		HudItemData()->set_bone_visible(wpn_scope, FALSE, TRUE );
	}else
		if(m_eScopeStatus==ALife::eAddonPermanent)
			HudItemData()->set_bone_visible(wpn_scope, TRUE, TRUE );

	if(SilencerAttachable())
	{
		HudItemData()->set_bone_visible(wpn_silencer, IsSilencerAttached());
	}
	if(m_eSilencerStatus==ALife::eAddonDisabled )
	{
		HudItemData()->set_bone_visible(wpn_silencer, FALSE, TRUE);
	}
	else
		if(m_eSilencerStatus==ALife::eAddonPermanent)
			HudItemData()->set_bone_visible(wpn_silencer, TRUE, TRUE);

	if(GrenadeLauncherAttachable())
	{
		HudItemData()->set_bone_visible(wpn_grenade_launcher, IsGrenadeLauncherAttached());
	}
	if(m_eGrenadeLauncherStatus==ALife::eAddonDisabled )
	{
		HudItemData()->set_bone_visible(wpn_grenade_launcher, FALSE, TRUE);
	}else
		if(m_eGrenadeLauncherStatus==ALife::eAddonPermanent)
			HudItemData()->set_bone_visible(wpn_grenade_launcher, TRUE, TRUE);
}

void CWeapon::UpdateAddonsVisibility()
{
	IKinematics* pWeaponVisual = smart_cast<IKinematics*>(Visual()); R_ASSERT(pWeaponVisual);

	u16  bone_id;
	UpdateHUDAddonsVisibility								();	

	pWeaponVisual->CalculateBones_Invalidate				();

	bone_id = pWeaponVisual->LL_BoneID					(wpn_scope);
	if(ScopeAttachable())
	{
		if(IsScopeAttached())
		{
			if(!pWeaponVisual->LL_GetBoneVisible		(bone_id))
			pWeaponVisual->LL_SetBoneVisible				(bone_id,TRUE,TRUE);
		}else{
			if(pWeaponVisual->LL_GetBoneVisible				(bone_id))
				pWeaponVisual->LL_SetBoneVisible			(bone_id,FALSE,TRUE);
		}
	}
	if(m_eScopeStatus==ALife::eAddonDisabled && bone_id!=BI_NONE && 
		pWeaponVisual->LL_GetBoneVisible(bone_id) )
	{
		pWeaponVisual->LL_SetBoneVisible					(bone_id,FALSE,TRUE);
//		Log("scope", pWeaponVisual->LL_GetBoneVisible		(bone_id));
	}
	bone_id = pWeaponVisual->LL_BoneID						(wpn_silencer);
	if(SilencerAttachable())
	{
		if(IsSilencerAttached()){
			if(!pWeaponVisual->LL_GetBoneVisible		(bone_id))
				pWeaponVisual->LL_SetBoneVisible			(bone_id,TRUE,TRUE);
		}else{
			if( pWeaponVisual->LL_GetBoneVisible			(bone_id))
				pWeaponVisual->LL_SetBoneVisible			(bone_id,FALSE,TRUE);
		}
	}
	if(m_eSilencerStatus==ALife::eAddonDisabled && bone_id!=BI_NONE && 
		pWeaponVisual->LL_GetBoneVisible(bone_id) )
	{
		pWeaponVisual->LL_SetBoneVisible					(bone_id,FALSE,TRUE);
//		Log("silencer", pWeaponVisual->LL_GetBoneVisible	(bone_id));
	}

	bone_id = pWeaponVisual->LL_BoneID						(wpn_grenade_launcher);
	if(GrenadeLauncherAttachable())
	{
		if(IsGrenadeLauncherAttached())
		{
			if(!pWeaponVisual->LL_GetBoneVisible		(bone_id))
				pWeaponVisual->LL_SetBoneVisible			(bone_id,TRUE,TRUE);
		}else{
			if(pWeaponVisual->LL_GetBoneVisible				(bone_id))
				pWeaponVisual->LL_SetBoneVisible			(bone_id,FALSE,TRUE);
		}
	}
	if(m_eGrenadeLauncherStatus==ALife::eAddonDisabled && bone_id!=BI_NONE && 
		pWeaponVisual->LL_GetBoneVisible(bone_id) )
	{
		pWeaponVisual->LL_SetBoneVisible					(bone_id,FALSE,TRUE);
//		Log("gl", pWeaponVisual->LL_GetBoneVisible			(bone_id));
	}

	pWeaponVisual->CalculateBones_Invalidate				();
	pWeaponVisual->CalculateBones							(TRUE);
}


void CWeapon::InitAddons()
{
}

float CWeapon::CurrentZoomFactor()
{
	return IsScopeAttached() ? m_zoom_params.m_fScopeZoomFactor : m_zoom_params.m_fIronSightZoomFactor;
};

void GetZoomData(const float scope_factor, float& delta, float& min_zoom_factor);

float LastZoomFactor = 0.f;

void CWeapon::OnZoomIn()
{
	m_zoom_params.m_bIsZoomModeNow		= true;
	if (m_zoom_params.m_bUseDynamicZoom && IsScopeAttached())
	{
		if (LastZoomFactor)
			m_fRTZoomFactor = LastZoomFactor;
		else
			m_fRTZoomFactor = CurrentZoomFactor();
		float delta, min_zoom_factor;
		GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);
		clamp(m_fRTZoomFactor, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
		SetZoomFactor(m_fRTZoomFactor);
	}
	else if (CurrentZoomFactor() != 0)
		m_zoom_params.m_fCurrentZoomFactor = CurrentZoomFactor();

	if (GetHUDmode())
		GamePersistent().SetPickableEffectorDOF(true);

	if (m_zoom_params.m_sUseBinocularVision.size() && IsScopeAttached() && nullptr == m_zoom_params.m_pVision)
		m_zoom_params.m_pVision = new CBinocularsVision(m_zoom_params.m_sUseBinocularVision);

	if(m_zoom_params.m_sUseZoomPostprocess.size() && IsScopeAttached()) 
	{
		CActor* actor = smart_cast<CActor*>(H_Parent());

		if (actor && nullptr == m_zoom_params.m_pNight_vision)
			m_zoom_params.m_pNight_vision = new CNightVisionEffector(m_zoom_params.m_sUseZoomPostprocess);
	}
}

void CWeapon::OnZoomOut()
{
	m_zoom_params.m_bIsZoomModeNow		= false;
	m_fRTZoomFactor = GetZoomFactor();//store current
	m_zoom_params.m_fCurrentZoomFactor	= g_fov;

// 	GamePersistent().RestoreEffectorDOF	();

	if(GetHUDmode())
		GamePersistent().SetPickableEffectorDOF(false);

	ResetSubStateTime					();

	xr_delete							(m_zoom_params.m_pVision);
	if(m_zoom_params.m_pNight_vision)
	{
		m_zoom_params.m_pNight_vision->Stop(100000.0f, false);
		xr_delete(m_zoom_params.m_pNight_vision);
	}
}

CUIWindow* CWeapon::ZoomTexture()
{
	if (UseScopeTexture())
		return m_UIScope;
	else
		return nullptr;
}

void CWeapon::SwitchState(u32 S)
{
	if (OnClient()) return;

#ifndef MASTER_GOLD
	if ( bDebug )
	{
		Msg("---Server is going to send GE_WPN_STATE_CHANGE to [%d], weapon_section[%s], parent[%s]",
			S, cNameSect().c_str(), H_Parent() ? H_Parent()->cName().c_str() : "nullptr Parent");
	}
#endif // #ifndef MASTER_GOLD

	SetNextState		( S );
	if (CHudItem::object().Local() && !CHudItem::object().getDestroy() && m_pInventory && OnServer())	
	{
		// !!! Just single entry for given state !!!
		NET_Packet		P;
		CHudItem::object().u_EventGen		(P,GE_WPN_STATE_CHANGE,CHudItem::object().ID());
		P.w_u8			(u8(S));
		P.w_u8			(u8(m_sub_state));
		P.w_u8			(m_ammoType);
		P.w_u8			(u8(iAmmoElapsed & 0xff));
		P.w_u8			(m_set_next_ammoType_on_reload);
		CHudItem::object().u_EventSend		(P, net_flags(TRUE, TRUE, FALSE, TRUE));
	}
}

void CWeapon::OnMagazineEmpty	()
{
	VERIFY((u32)iAmmoElapsed == m_magazine.size());

	if (ParentIsActor())
	{
		int	AC = GetSuitableAmmoTotal();
		Actor()->callback(GameObject::eOnWeaponMagazineEmpty)(lua_game_object(), AC);
	}
}


void CWeapon::reinit			()
{
	CShootingObject::reinit		();
	CHudItemObject::reinit			();
}

void CWeapon::reload(LPCSTR section) {
	CShootingObject::reload(section);
	CHudItemObject::reload(section);

	m_can_be_strapped = true;
	m_can_be_strapped_rifle = (/*BaseSlot() == INV_SLOT_2 ||*/ BaseSlot() == INV_SLOT_3);
	m_strapped_mode = false;
	m_strapped_mode_rifle = false;

	if (m_eScopeStatus == ALife::eAddonAttachable) {
		m_addon_holder_range_modifier = READ_IF_EXISTS(
			pSettings, r_float, GetScopeName(), "holder_range_modifier", m_holder_range_modifier);
		m_addon_holder_fov_modifier = READ_IF_EXISTS(pSettings, r_float, GetScopeName(),
			"holder_fov_modifier", m_holder_fov_modifier);
	}
	else {
		m_addon_holder_range_modifier = m_holder_range_modifier;
		m_addon_holder_fov_modifier = m_holder_fov_modifier;
	}

	{
		Fvector pos, ypr;
		pos = pSettings->r_fvector3(section, "position");
		ypr = pSettings->r_fvector3(section, "orientation");
		ypr.mul(PI / 180.f);

		m_Offset.setHPB(ypr.x, ypr.y, ypr.z);
		m_Offset.translate_over(pos);
	}

	if (BaseSlot() == INV_SLOT_3) {
		// Strap bones:
		if (pSettings->line_exist(section, "strap_bone0"))
			m_strap_bone0 = pSettings->r_string(section, "strap_bone0");
		else {
			m_strap_bone0 = "bip01_spine2";
		}
		if (pSettings->line_exist(section, "strap_bone1"))
			m_strap_bone1 = pSettings->r_string(section, "strap_bone1");
		else {
			m_strap_bone1 = "bip01_spine1";
		}

		// Right shoulder strap coordinates:
		m_StrapOffset = m_Offset;
		Fvector pos, ypr;
		if (pSettings->line_exist(section, "strap_position") &&
			pSettings->line_exist(section, "strap_orientation")) {
			pos = pSettings->r_fvector3(section, "strap_position");
			ypr = pSettings->r_fvector3(section, "strap_orientation");
		}
		else {
			pos = Fvector().set(-0.34f, -0.20f, 0.15f);
			ypr = Fvector().set(-0.0f, 0.0f, 84.0f);
		}
		ypr.mul(PI / 180.f);
		m_StrapOffset.setHPB(ypr.x, ypr.y, ypr.z);
		m_StrapOffset.translate_over(pos);

		// Left shoulder strap coordinates:
		m_StrapOffset_alt = m_Offset;
		Fvector pos_alt, ypr_alt;
		if (pSettings->line_exist(section, "strap_position_alt") &&
			pSettings->line_exist(section, "strap_orientation_alt")) {
			pos_alt = pSettings->r_fvector3(section, "strap_position_alt");
			ypr_alt = pSettings->r_fvector3(section, "strap_orientation_alt");
		}
		else {
			pos_alt = Fvector().set(-0.34f, 0.20f, 0.15f);
			ypr_alt = Fvector().set(0.0f, 0.0f, 94.0f);
		}
		ypr_alt.mul(PI / 180.f);
		m_StrapOffset_alt.setHPB(ypr_alt.x, ypr_alt.y, ypr_alt.z);
		m_StrapOffset_alt.translate_over(pos_alt);
	}
	else {
		m_can_be_strapped = false;
		m_can_be_strapped_rifle = false;
	}

	m_ef_main_weapon_type =
		READ_IF_EXISTS(pSettings, r_u32, section, "ef_main_weapon_type", u32(-1));
	m_ef_weapon_type =
		READ_IF_EXISTS(pSettings, r_u32, section, "ef_weapon_type", u32(-1));
}

void CWeapon::create_physic_shell()
{
	CPhysicsShellHolder::create_physic_shell();
}

bool CWeapon::ActivationSpeedOverriden (Fvector& dest, bool clear_override)
{
	if ( m_activation_speed_is_overriden )
	{
		if ( clear_override )
		{
			m_activation_speed_is_overriden	=	false;
		}

		dest						=	m_overriden_activation_speed;
		return							true;
	}
	
	return								false;
}

void CWeapon::SetActivationSpeedOverride	(Fvector const& speed)
{
	m_overriden_activation_speed	=	speed;
	m_activation_speed_is_overriden	=	true;
}

void CWeapon::activate_physic_shell()
{
	UpdateXForm();
	CPhysicsShellHolder::activate_physic_shell();
}

void CWeapon::setup_physic_shell()
{
	CPhysicsShellHolder::setup_physic_shell();
}

int		g_iWeaponRemove = 1;

bool CWeapon::NeedToDestroyObject()	const
{
	if (IsGameTypeSingle()) return false;
	if (Remote()) return false;
	if (H_Parent()) return false;
	if (g_iWeaponRemove == -1) return false;
	if (g_iWeaponRemove == 0) return true;
	if (TimePassedAfterIndependant() > m_dwWeaponRemoveTime)
		return true;

	return false;
}

ALife::_TIME_ID	 CWeapon::TimePassedAfterIndependant()	const
{
	if(!H_Parent() && m_dwWeaponIndependencyTime != 0)
		return Level().timeServer() - m_dwWeaponIndependencyTime;
	else
		return 0;
}

bool CWeapon::can_kill	() const
{
	if (GetSuitableAmmoTotal(true) || m_ammoTypes.empty())
		return				(true);

	return					(false);
}

CInventoryItem *CWeapon::can_kill	(CInventory *inventory) const
{
	if (GetAmmoElapsed() || m_ammoTypes.empty())
		return				(const_cast<CWeapon*>(this));

	TIItemContainer::iterator I = inventory->m_all.begin();
	TIItemContainer::iterator E = inventory->m_all.end();
	for ( ; I != E; ++I) {
		CInventoryItem	*inventory_item = smart_cast<CInventoryItem*>(*I);
		if (!inventory_item)
			continue;
		
		xr_vector<shared_str>::const_iterator	i = std::find(m_ammoTypes.begin(),m_ammoTypes.end(),inventory_item->object().cNameSect());
		if (i != m_ammoTypes.end())
			return			(inventory_item);
	}

	return					(0);
}

const CInventoryItem *CWeapon::can_kill	(const xr_vector<const CGameObject*> &items) const
{
	if (m_ammoTypes.empty())
		return				(this);

	xr_vector<const CGameObject*>::const_iterator I = items.begin();
	xr_vector<const CGameObject*>::const_iterator E = items.end();
	for ( ; I != E; ++I) {
		const CInventoryItem	*inventory_item = smart_cast<const CInventoryItem*>(*I);
		if (!inventory_item)
			continue;

		xr_vector<shared_str>::const_iterator	i = std::find(m_ammoTypes.begin(),m_ammoTypes.end(),inventory_item->object().cNameSect());
		if (i != m_ammoTypes.end())
			return			(inventory_item);
	}

	return					(0);
}

bool CWeapon::ready_to_kill() const
{
	return !IsMisfire() && ((GetState() == eIdle) || (GetState() == eFire) || (GetState() == eFire2)) && GetAmmoElapsed();
}

u8 CWeapon::GetCurrentHudOffsetIdx() const
{
	auto pActor = smart_cast<const CActor*>(H_Parent());
	if (!pActor)
		return 0;

	bool b_aiming =
		((IsZoomed() && /*m_zoom_params.*/ m_zoom_params.m_fZoomRotationFactor <= 1.f) ||
			(!IsZoomed() && /*m_zoom_params.*/ m_zoom_params.m_fZoomRotationFactor > 0.f));

	if (!b_aiming)
		return 0;
	// else if (IsGrenadeMode())
	//	return 2;
	else
		return 1;
}

void CWeapon::SelectCurrentOffset(Fvector& pos, Fvector& rot)
{
	u8 idx = GetCurrentHudOffsetIdx();

	s32 cur_index;
	if (IsScopeAttached() && get_ScopeStatus() == 2)
		cur_index = m_cur_scope;
	else
		cur_index = -1;

	string32 offset_name;
	xr_sprintf(offset_name, "%s_hud_offset_pos%s", idx == 0 ? "hands" : idx == 1 ? "aim" : idx == 2 ? "gl" : "", UI().is_widescreen() ? "_16x9" : "");

	string32 rot_name;
	xr_sprintf(rot_name, "%s_hud_offset_rot%s", idx == 0 ? "hands" : idx == 1 ? "aim" : idx == 2 ? "gl" : "", UI().is_widescreen() ? "_16x9" : "");

	if (cur_index >= 0)
	{
		if (pSettings->line_exist(GetCurrentScopeSection(), offset_name) && pSettings->line_exist(GetCurrentScopeSection(), rot_name))
		{
			pos = pSettings->r_fvector3(GetCurrentScopeSection(), offset_name);
			rot = pSettings->r_fvector3(GetCurrentScopeSection(), rot_name);
			return;
		}
	}

	pos = HudItemData()->m_measures.m_hands_offset[0][idx];
	rot = HudItemData()->m_measures.m_hands_offset[1][idx];
}

void CWeapon::UpdateHudAdditonal(Fmatrix& trans)
{
	auto pActor = smart_cast<const CActor*>(H_Parent());
	if (!pActor)
		return;

	u8 idx = GetCurrentHudOffsetIdx();

	attachable_hud_item* hi = HudItemData();
	R_ASSERT(hi);

	if(		(IsZoomed() && m_zoom_params.m_fZoomRotationFactor<=1.f) ||
			(!IsZoomed() && m_zoom_params.m_fZoomRotationFactor>0.f))
	{
		Fvector						curr_offs, curr_rot;
		SelectCurrentOffset(curr_offs, curr_rot);
		curr_offs.mul				(m_zoom_params.m_fZoomRotationFactor);
		curr_rot.mul				(m_zoom_params.m_fZoomRotationFactor);
		Fmatrix						hud_rotation;
		hud_rotation.identity		();
		hud_rotation.rotateX		(curr_rot.x);

		Fmatrix						hud_rotation_y;
		hud_rotation_y.identity		();
		hud_rotation_y.rotateY		(curr_rot.y);
		hud_rotation.mulA_43		(hud_rotation_y);

		hud_rotation_y.identity		();
		hud_rotation_y.rotateZ		(curr_rot.z);
		hud_rotation.mulA_43		(hud_rotation_y);

		hud_rotation.translate_over	(curr_offs);
		trans.mulB_43				(hud_rotation);

		if(pActor->IsZoomAimingMode())
			m_zoom_params.m_fZoomRotationFactor += Device.fTimeDelta/m_zoom_params.m_fZoomRotateTime;
		else
			m_zoom_params.m_fZoomRotationFactor -= Device.fTimeDelta/m_zoom_params.m_fZoomRotateTime;

		clamp(m_zoom_params.m_fZoomRotationFactor, 0.f, 1.f);
	}

	//============= Подготавливаем общие переменные =============//
	clamp(idx, u8(0), u8(1));
	bool bForAim = (idx == 1);

	static float fAvgTimeDelta = Device.fTimeDelta;
	fAvgTimeDelta = _inertion(fAvgTimeDelta, Device.fTimeDelta, 0.8f);

	float fYMag = pActor->fFPCamYawMagnitude;
	float fPMag = pActor->fFPCamPitchMagnitude;

	//============= Боковой стрейф с оружием =============//
	// Рассчитываем фактор боковой ходьбы
	float fStrafeMaxTime = hi->m_measures.m_strafe_offset[2][idx].y;
	// Макс. время в секундах, за которое мы наклонимся из центрального положения
	if (fStrafeMaxTime <= EPS)
		fStrafeMaxTime = 0.01f;

	float fStepPerUpd = fAvgTimeDelta / fStrafeMaxTime; // Величина изменение фактора поворота

	// Добавляем боковой наклон от движения камеры
	float fCamReturnSpeedMod = 1.5f;
	// Восколько ускоряем нормализацию наклона, полученного от движения камеры (только от бедра)

	// Высчитываем минимальную скорость поворота камеры для начала инерции
	float fStrafeMinAngle = _lerp(
		hi->m_measures.m_strafe_offset[3][0].y,
		hi->m_measures.m_strafe_offset[3][1].y,
		m_zoom_params.m_fZoomRotationFactor);

	// Высчитываем мксимальный наклон от поворота камеры
	float fCamLimitBlend = _lerp(
		hi->m_measures.m_strafe_offset[3][0].x,
		hi->m_measures.m_strafe_offset[3][1].x,
		m_zoom_params.m_fZoomRotationFactor);

	// Считаем стрейф от поворота камеры
	if (abs(fYMag) > (m_fLR_CameraFactor == 0.0f ? fStrafeMinAngle : 0.0f))
	{
		//--> Камера крутится по оси Y
		m_fLR_CameraFactor -= (fYMag * fAvgTimeDelta * 0.75f);
		clamp(m_fLR_CameraFactor, -fCamLimitBlend, fCamLimitBlend);
	}
	else
	{
		//--> Камера не поворачивается - убираем наклон
		if (m_fLR_CameraFactor < 0.0f)
		{
			m_fLR_CameraFactor += fStepPerUpd * (bForAim ? 1.0f : fCamReturnSpeedMod);
			clamp(m_fLR_CameraFactor, -fCamLimitBlend, 0.0f);
		}
		else
		{
			m_fLR_CameraFactor -= fStepPerUpd * (bForAim ? 1.0f : fCamReturnSpeedMod);
			clamp(m_fLR_CameraFactor, 0.0f, fCamLimitBlend);
		}
	}

	// Добавляем боковой наклон от ходьбы вбок
	float fChangeDirSpeedMod = 3;
	// Восколько быстро меняем направление направление наклона, если оно в другую сторону от текущего
	u32 iMovingState = pActor->GetMovementState(eReal);
	if ((iMovingState & ACTOR_DEFS::EMoveCommand::mcLStrafe) != 0)
	{
		// Движемся влево
		float fVal = (m_fLR_MovingFactor > 0.f ? fStepPerUpd * fChangeDirSpeedMod : fStepPerUpd);
		m_fLR_MovingFactor -= fVal;
	}
	else if ((iMovingState & ACTOR_DEFS::EMoveCommand::mcRStrafe) != 0)
	{
		// Движемся вправо
		float fVal = (m_fLR_MovingFactor < 0.f ? fStepPerUpd * fChangeDirSpeedMod : fStepPerUpd);
		m_fLR_MovingFactor += fVal;
	}
	else
	{
		// Двигаемся в любом другом направлении - плавно убираем наклон
		if (m_fLR_MovingFactor < 0.0f)
		{
			m_fLR_MovingFactor += fStepPerUpd;
			clamp(m_fLR_MovingFactor, -1.0f, 0.0f);
		}
		else
		{
			m_fLR_MovingFactor -= fStepPerUpd;
			clamp(m_fLR_MovingFactor, 0.0f, 1.0f);
		}
	}
	clamp(m_fLR_MovingFactor, -1.0f, 1.0f); // Фактор боковой ходьбы не должен превышать эти лимиты

	// Вычисляем и нормализируем итоговый фактор наклона
	float fLR_Factor = m_fLR_MovingFactor;
	fLR_Factor += m_fLR_CameraFactor;

	clamp(fLR_Factor, -1.0f, 1.0f); // Фактор боковой ходьбы не должен превышать эти лимиты

	// Производим наклон ствола для нормального режима и аима
	for (int _idx = 0; _idx <= 1; _idx++) //<-- Для плавного перехода
	{
		bool bEnabled = (hi->m_measures.m_strafe_offset[2][_idx].x != 0.0f);
		if (!bEnabled)
			continue;

		Fvector curr_offs, curr_rot;

		// Смещение позиции худа в стрейфе
		curr_offs = hi->m_measures.m_strafe_offset[0][_idx]; // pos
		curr_offs.mul(fLR_Factor); // Умножаем на фактор стрейфа

		// Поворот худа в стрейфе
		curr_rot = hi->m_measures.m_strafe_offset[1][_idx]; // rot
		curr_rot.mul(-PI / 180.f); // Преобразуем углы в радианы
		curr_rot.mul(fLR_Factor); // Умножаем на фактор стрейфа

		// Мягкий переход между бедром \ прицелом
		if (_idx == 0)
		{
			// От бедра
			curr_offs.mul(1.f - m_zoom_params.m_fZoomRotationFactor);
			curr_rot.mul(1.f - m_zoom_params.m_fZoomRotationFactor);
		}
		else
		{
			// Во время аима
			curr_offs.mul(m_zoom_params.m_fZoomRotationFactor);
			curr_rot.mul(m_zoom_params.m_fZoomRotationFactor);
		}

		Fmatrix hud_rotation;
		Fmatrix hud_rotation_y;

		hud_rotation.identity();
		hud_rotation.rotateX(curr_rot.x);

		hud_rotation_y.identity();
		hud_rotation_y.rotateY(curr_rot.y);
		hud_rotation.mulA_43(hud_rotation_y);

		hud_rotation_y.identity();
		hud_rotation_y.rotateZ(curr_rot.z);
		hud_rotation.mulA_43(hud_rotation_y);

		hud_rotation.translate_over(curr_offs);
		trans.mulB_43(hud_rotation);
	}

	//============= Инерция оружия =============//
	// Параметры инерции
	float fInertiaSpeedMod = _lerp(
		hi->m_measures.m_inertion_params.m_tendto_speed,
		hi->m_measures.m_inertion_params.m_tendto_speed_aim,
		m_zoom_params.m_fZoomRotationFactor);

	float fInertiaReturnSpeedMod = _lerp(
		hi->m_measures.m_inertion_params.m_tendto_ret_speed,
		hi->m_measures.m_inertion_params.m_tendto_ret_speed_aim,
		m_zoom_params.m_fZoomRotationFactor);

	float fInertiaMinAngle = _lerp(
		hi->m_measures.m_inertion_params.m_min_angle,
		hi->m_measures.m_inertion_params.m_min_angle_aim,
		m_zoom_params.m_fZoomRotationFactor);

	Fvector4 vIOffsets; // x = L, y = R, z = U, w = D
	vIOffsets.x = _lerp(
		hi->m_measures.m_inertion_params.m_offset_LRUD.x,
		hi->m_measures.m_inertion_params.m_offset_LRUD_aim.x,
		m_zoom_params.m_fZoomRotationFactor);
	vIOffsets.y = _lerp(
		hi->m_measures.m_inertion_params.m_offset_LRUD.y,
		hi->m_measures.m_inertion_params.m_offset_LRUD_aim.y,
		m_zoom_params.m_fZoomRotationFactor);
	vIOffsets.z = _lerp(
		hi->m_measures.m_inertion_params.m_offset_LRUD.z,
		hi->m_measures.m_inertion_params.m_offset_LRUD_aim.z,
		m_zoom_params.m_fZoomRotationFactor);
	vIOffsets.w = _lerp(
		hi->m_measures.m_inertion_params.m_offset_LRUD.w,
		hi->m_measures.m_inertion_params.m_offset_LRUD_aim.w,
		m_zoom_params.m_fZoomRotationFactor);

	// Высчитываем инерцию из поворотов камеры
	bool bIsInertionPresent = m_fLR_InertiaFactor != 0.0f || m_fUD_InertiaFactor != 0.0f;
	if (abs(fYMag) > fInertiaMinAngle || bIsInertionPresent)
	{
		float fSpeed = fInertiaSpeedMod;
		if (fYMag > 0.0f && m_fLR_InertiaFactor > 0.0f ||
			fYMag < 0.0f && m_fLR_InertiaFactor < 0.0f)
		{
			fSpeed *= 2.f; //--> Ускоряем инерцию при движении в противоположную сторону
		}

		m_fLR_InertiaFactor -= (fYMag * fAvgTimeDelta * fSpeed); // Горизонталь (м.б. > |1.0|)
	}

	if (abs(fPMag) > fInertiaMinAngle || bIsInertionPresent)
	{
		float fSpeed = fInertiaSpeedMod;
		if (fPMag > 0.0f && m_fUD_InertiaFactor > 0.0f ||
			fPMag < 0.0f && m_fUD_InertiaFactor < 0.0f)
		{
			fSpeed *= 2.f; //--> Ускоряем инерцию при движении в противоположную сторону
		}

		m_fUD_InertiaFactor -= (fPMag * fAvgTimeDelta * fSpeed); // Вертикаль (м.б. > |1.0|)
	}

	clamp(m_fLR_InertiaFactor, -1.0f, 1.0f);
	clamp(m_fUD_InertiaFactor, -1.0f, 1.0f);

	// Плавное затухание инерции (основное, но без линейной никогда не опустит инерцию до полного 0.0f)
	m_fLR_InertiaFactor *= clampr(1.f - fAvgTimeDelta * fInertiaReturnSpeedMod, 0.0f, 1.0f);
	m_fUD_InertiaFactor *= clampr(1.f - fAvgTimeDelta * fInertiaReturnSpeedMod, 0.0f, 1.0f);

	// Минимальное линейное затухание инерции при покое (горизонталь)
	if (fYMag == 0.0f)
	{
		float fRetSpeedMod = (fYMag == 0.0f ? 1.0f : 0.75f) * (fInertiaReturnSpeedMod * 0.075f);
		if (m_fLR_InertiaFactor < 0.0f)
		{
			m_fLR_InertiaFactor += fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fLR_InertiaFactor, -1.0f, 0.0f);
		}
		else
		{
			m_fLR_InertiaFactor -= fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fLR_InertiaFactor, 0.0f, 1.0f);
		}
	}

	// Минимальное линейное затухание инерции при покое (вертикаль)
	if (fPMag == 0.0f)
	{
		float fRetSpeedMod = (fPMag == 0.0f ? 1.0f : 0.75f) * (fInertiaReturnSpeedMod * 0.075f);
		if (m_fUD_InertiaFactor < 0.0f)
		{
			m_fUD_InertiaFactor += fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fUD_InertiaFactor, -1.0f, 0.0f);
		}
		else
		{
			m_fUD_InertiaFactor -= fAvgTimeDelta * fRetSpeedMod;
			clamp(m_fUD_InertiaFactor, 0.0f, 1.0f);
		}
	}

	// Применяем инерцию к худу
	float fLR_lim = (m_fLR_InertiaFactor < 0.0f ? vIOffsets.x : vIOffsets.y);
	float fUD_lim = (m_fUD_InertiaFactor < 0.0f ? vIOffsets.z : vIOffsets.w);

	Fvector curr_offs;
	curr_offs = { fLR_lim * -1.f * m_fLR_InertiaFactor, fUD_lim * m_fUD_InertiaFactor, 0.0f };

	Fmatrix hud_rotation;
	hud_rotation.identity();
	hud_rotation.translate_over(curr_offs);
	trans.mulB_43(hud_rotation);
}

void CWeapon::SetAmmoElapsed(int ammo_count)
{
	iAmmoElapsed				= ammo_count;

	u32 uAmmo					= u32(iAmmoElapsed);

	if (uAmmo != m_magazine.size())
	{
		if (uAmmo > m_magazine.size())
		{
			CCartridge			l_cartridge; 
			l_cartridge.Load	(m_ammoTypes[m_ammoType].c_str(), m_ammoType);
			while (uAmmo > m_magazine.size())
				m_magazine.push_back(l_cartridge);
		}
		else
		{
			while (uAmmo < m_magazine.size())
				m_magazine.pop_back();
		};
	};
}

u32	CWeapon::ef_main_weapon_type	() const
{
	VERIFY	(m_ef_main_weapon_type != u32(-1));
	return	(m_ef_main_weapon_type);
}

u32	CWeapon::ef_weapon_type	() const
{
	VERIFY	(m_ef_weapon_type != u32(-1));
	return	(m_ef_weapon_type);
}

bool CWeapon::IsNecessaryItem	    (const shared_str& item_sect)
{
	return (std::find(m_ammoTypes.begin(), m_ammoTypes.end(), item_sect) != m_ammoTypes.end() );
}

void CWeapon::modify_holder_params		(float &range, float &fov) const
{
	if (!IsScopeAttached()) {
		inherited::modify_holder_params	(range,fov);
		return;
	}
	range	*= m_addon_holder_range_modifier;
	fov		*= m_addon_holder_fov_modifier;
}

bool CWeapon::render_item_ui_query()
{
	bool b_is_active_item = (m_pInventory->ActiveItem()==this);
	bool res = b_is_active_item && IsZoomed() && ZoomHideCrosshair() && ZoomTexture() && !IsRotatingToZoom();
	return res;
}

void CWeapon::render_item_ui()
{
	if(m_zoom_params.m_pVision)
		m_zoom_params.m_pVision->Draw();

	ZoomTexture()->Update	();
	ZoomTexture()->Draw		();
}

bool CWeapon::unlimited_ammo() 
{ 
	if (IsGameTypeSingle())
	{
		if(m_pInventory)
		{
			return inventory_owner().unlimited_ammo() && m_DefaultCartridge.m_flags.test(CCartridge::cfCanBeUnlimited);
		}else
			return false;
	}

	return ((GameID() == eGameIDDeathmatch) && 
			m_DefaultCartridge.m_flags.test(CCartridge::cfCanBeUnlimited)); 
			
};

float CWeapon::GetMagazineWeight(const decltype(CWeapon::m_magazine)& mag) const {
	float res = 0;
	const char* last_type = nullptr;
	float last_ammo_weight = 0;
	for (auto& c : mag) {
		// Usually ammos in mag have same type, use this fact to improve performance
		if (last_type != c.m_ammoSect.c_str()) {
			last_type = c.m_ammoSect.c_str();
			last_ammo_weight = c.Weight();
		}
		res += last_ammo_weight;
	}
	return res;
}

float CWeapon::Weight() const
{
	float res = CInventoryItemObject::Weight();
	if(IsGrenadeLauncherAttached()&&GetGrenadeLauncherName().size()){
		res += pSettings->r_float(GetGrenadeLauncherName(),"inv_weight");
	}
	if(IsScopeAttached()&&m_scopes.size()){
		res += pSettings->r_float(GetScopeName(),"inv_weight");
	}
	if(IsSilencerAttached()&&GetSilencerName().size()){
		res += pSettings->r_float(GetSilencerName(),"inv_weight");
	}
	
	res += GetMagazineWeight(m_magazine);

	return res;
}

extern bool hud_adj_crosshair;
bool CWeapon::show_crosshair()
{
	return (!IsPending() || IsPending() && GetState() == eKick) && ((!IsZoomed() || !ZoomHideCrosshair()) || hud_adj_mode != 0 && hud_adj_crosshair);
}

bool CWeapon::show_indicators()
{
	return !(IsZoomed() && ZoomTexture() && IsUIForceHiding() && !IsUIForceUnhiding());
}

float CWeapon::GetConditionToShow	() const
{
	return	(GetCondition());//powf(GetCondition(),4.0f));
}

BOOL CWeapon::ParentMayHaveAimBullet	()
{
	CObject* O=H_Parent();
	CEntityAlive* EA=smart_cast<CEntityAlive*>(O);
	return EA->cast_actor()!=0;
}

BOOL CWeapon::ParentIsActor	()
{
	CObject* O			= H_Parent();
	if (!O)
		return FALSE;

	CEntityAlive* EA	= smart_cast<CEntityAlive*>(O);
	if (!EA)
		return FALSE;

	return EA->cast_actor()!=0;
}

void CWeapon::debug_draw_firedeps()
{
#ifdef DEBUG
	if(hud_adj_mode==5||hud_adj_mode==6||hud_adj_mode==7)
	{
		CDebugRenderer			&render = Level().debug_renderer();

		if (hud_adj_mode == 5)
			render.draw_aabb(get_LastFP(), 0.005f, 0.005f, 0.005f, color_xrgb(255, 0, 0));

		if (hud_adj_mode == 6)
			render.draw_aabb(get_LastFP2(), 0.005f, 0.005f, 0.005f, color_xrgb(0, 0, 255));

		if (hud_adj_mode == 7)
			render.draw_aabb(get_LastSP(), 0.005f, 0.005f, 0.005f, color_xrgb(0, 255, 0));
	}
#endif // DEBUG
}

const float &CWeapon::hit_probability	() const
{
	VERIFY					((g_SingleGameDifficulty >= egdNovice) && (g_SingleGameDifficulty <= egdMaster)); 
	return					(m_hit_probability[egdNovice]);
}

BOOL EnableDof = true;

void CWeapon::OnStateSwitch	(u32 S)
{
	inherited::OnStateSwitch(S);
	m_BriefInfo_CalcFrame = 0;

	switch (S)
	{
        case eShowingDet:
			PlayHUDMotion("anm_prepare_detector", TRUE, GetState());
            SetPending(true);
        break;
        case eShowingEndDet:
			PlayHUDMotion("anm_draw_detector", FALSE, GetState());
            SetPending(true);
        break;
        break;
        case eHideDet:
			PlayHUDMotion("anm_finish_detector", TRUE, GetState());
            SetPending(true);
        break;
	}

	switch (S)
	{
	case eShowing:
	case eIdle:
	case eHiding:
	case eFire:
	case eReload:
	case eHidden:
	case eSwitch:
	case eSwitchMode:
		_wanim_force_assign = true;
		ReassignWorldAnims();
		break;
	}

	if(EnableDof && GetState()==eReload)
	{
		if(H_Parent()==Level().CurrentEntity() && !fsimilar(m_zoom_params.m_ReloadDof.w,-1.0f))
		{
			CActor* current_actor	= smart_cast<CActor*>(H_Parent());
			if (current_actor)
				current_actor->Cameras().AddCamEffector(new CEffectorDOF(m_zoom_params.m_ReloadDof));
		}
	}
}

void CWeapon::OnAnimationEnd(u32 state) 
{
	inherited::OnAnimationEnd(state);

	switch (state)
	{
		case eShowingDet:
		{
			if (GetDetector(true))
			{
				GetDetector(true)->SwitchState(CCustomDetector::eShowing);
				GetDetector(true)->TurnDetectorInternal(true);
				SwitchState(eShowingEndDet);
			}
		}break;
		case eShowingEndDet:
		case eHideDet:
			SwitchState(eIdle);
		break;
	}
}

bool CWeapon::NeedBlockSprint() const
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (isGuns)
		return GetState() != eIdle && GetState() != eSprintStart && GetState() != eHidden;
	else
		return GetState() == eFire || GetState() == eFire2 || EngineExternal()[EEngineExternalGame::EnableBlockSprintInReload] && GetState() == eReload;
}

u8 CWeapon::GetCurrentHudOffsetIdx()
{
	CActor* pActor	= smart_cast<CActor*>(H_Parent());
	if(!pActor)		return 0;
	
	bool b_aiming		= 	((IsZoomed() && m_zoom_params.m_fZoomRotationFactor<=1.f) ||
							(!IsZoomed() && m_zoom_params.m_fZoomRotationFactor>0.f));

	if(!b_aiming)
		return		0;
	else
		return		1;
}

void CWeapon::render_hud_mode()
{
	RenderLight();
}

bool CWeapon::MovingAnimAllowedNow()
{
	return !IsZoomed();
}

bool CWeapon::IsHudModeNow()
{
	return (HudItemData()!=nullptr);
}

void CWeapon::ZoomInc()
{
	if(!IsScopeAttached())					return;
	if(!m_zoom_params.m_bUseDynamicZoom)	return;
	float delta,min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);

	float f					= GetZoomFactor()-delta;
	clamp					(f,m_zoom_params.m_fScopeZoomFactor,min_zoom_factor);
	SetZoomFactor			( f );
	LastZoomFactor = f;
}

void CWeapon::ZoomDec()
{
	if(!IsScopeAttached())					return;
	if(!m_zoom_params.m_bUseDynamicZoom)	return;
	float delta,min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor,delta,min_zoom_factor);

	float f					= GetZoomFactor()+delta;
	clamp					(f,m_zoom_params.m_fScopeZoomFactor,min_zoom_factor);
	SetZoomFactor			( f );
	LastZoomFactor = f;
}

u32 CWeapon::Cost() const
{
	u32 res = CInventoryItem::Cost();
	if(IsGrenadeLauncherAttached()&&GetGrenadeLauncherName().size()){
		res += pSettings->r_u32(GetGrenadeLauncherName(),"cost");
	}
	if(IsScopeAttached()&&m_scopes.size()){
		res += pSettings->r_u32(GetScopeName(),"cost");
	}
	if(IsSilencerAttached()&&GetSilencerName().size()){
		res += pSettings->r_u32(GetSilencerName(),"cost");
	}
	
	if(iAmmoElapsed)
	{
		float w		= pSettings->r_float(m_ammoTypes[m_ammoType].c_str(),"cost");
		float bs	= pSettings->r_float(m_ammoTypes[m_ammoType].c_str(),"box_size");

		res			+= iFloor(w*(iAmmoElapsed/bs));
	}

	return res;
}

float CWeapon::GetHudFov() {
	auto base = inherited::GetHudFov();
	auto zoom = m_HudFovZoom ? m_HudFovZoom : (psHUD_FOV_def * Device.fFOV / g_fov);
	base += (zoom - base) * m_zoom_params.m_fZoomRotationFactor;
	return base;
}

CCustomDetector* CWeapon::GetDetector(bool in_slot)
{
	if (in_slot)
	{
		if (m_pInventory)
		{
			CCustomDetector* in_slot = smart_cast<CCustomDetector*>(m_pInventory->ItemFromSlot(DETECTOR_SLOT));
			if (in_slot)
				return in_slot;
		}
	}
	else
	{
		if (HudItemData())
		{
			attachable_hud_item* i1 = g_player_hud->attached_item(1);
			if (i1)
				return smart_cast<CCustomDetector*>(i1->m_parent_hud_item);
		}
	}

	return nullptr;
}