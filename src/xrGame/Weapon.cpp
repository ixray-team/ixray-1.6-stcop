#include "StdAfx.h"
#include "Weapon.h"
#include "entity_alive.h"
#include "inventory_item_impl.h"
#include "Inventory.h"
#include "xrServer_Objects_ALife_Items.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "Level.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_cl_base.h"
#include "../Include/xrRender/Kinematics.h"
#include "ai_object_location.h"
#include "../xrPhysics/MathUtils.h"
#include "object_broker.h"
#include "player_hud.h"
#include "GamePersistent.h"
#include "EffectorFall.h"
#include "debug_renderer.h"
#include "clsid_game.h"
#include "WeaponBinocularsVision.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/UIXmlInit.h"
#include "Torch.h"
#include "CustomDetector.h"
#include "script_game_object.h"
#include <WeaponBinoculars.h>
#include "../xrScripts/script_callback_ex.h"

#include <algorithm>

#define WEAPON_REMOVE_TIME		60000
#define ROTATION_TIME			0.25f

BOOL	b_toggle_weapon_aim		= FALSE;
extern CUIXml*	pWpnScopeXml;

ENGINE_API extern float psHUD_FOV_def;
ENGINE_API extern bool g_3d_scopes;

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

	iAmmoChamberElapsed = 0;
	iChamberSize = 1;

	m_ammoType				= 0;
	m_ChamberAmmoType		= 0;

	eHandDependence			= hdNone;

	m_zoom_params.m_fCurrentZoomFactor			= g_fov;
	m_zoom_params.m_fZoomRotationFactor			= 0.f;
	m_zoom_params.m_pVision						= nullptr;
	m_zoom_params.m_pNight_vision				= nullptr;

	m_pCurrentAmmo			= nullptr;

	m_pFlameParticles2		= nullptr;
	m_sFlameParticles2		= nullptr;

	m_bIAmWeaponRPG7 = false;
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
	m_cur_scope				= 0;
	_last_update_time = Device.dwTimeGlobal;
	useLegacyMisfire = false;

	for (auto& it : m_ammo_bones_mag)
	{
		xr_delete(it);
	}
	m_ammo_bones_mag.clear();

	for (auto& it : m_ammo_bones_gl)
	{
		xr_delete(it);
	}
	m_ammo_bones_gl.clear();

	for (auto& it : m_shell_bones)
	{
		xr_delete(it);
	}
	m_shell_bones.clear();
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
	CGameObject* go = H_Parent()->cast_game_object();
	if (go == nullptr)
	{
		return;
	}

	if (go->cast_trader())
	{
		return;
	}

	if (!go->cast_entity_alive()) {
		if (!IsGameTypeSingle()) {
			UpdatePosition(H_Parent()->XFORM());
			UpdatePosition_alt(H_Parent()->XFORM());
		}
		return;
	} 

	const CInventoryOwner* parent = go->cast_inventory_owner(); //smart_cast<const CInventoryOwner*>(go);
	if (!parent || (parent && parent->use_simplified_visual()))
		return;

	if (!m_can_be_strapped_rifle) {
		if (parent->attached(this))
			return;
	}

	IKinematics*			V = PKinematics(go->Visual());
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
		go->cast_entity_alive()->g_WeaponBones(boneL, boneR, boneR2);

		if (m_strapped_mode_rifle)
			m_strapped_mode_rifle = false;
	}

	if (boneR == -1)		return;

	if ((HandDependence() == hd1Hand) || (GetState() == eReload) || (!go->cast_entity_alive()->g_Alive()))
		boneL				= boneR2;

	Fmatrix mL, mR;
	if (go->cast_actor()) {
		V->Bone_GetAnimPos(mL, boneL, u8(-1), false);
		V->Bone_GetAnimPos(mR, boneR, u8(-1), false);
	}
	else {
		// V->CalculateBones();
		mL = V->LL_GetTransform(boneL);
		mR = V->LL_GetTransform(boneR);
	}

	// Calculate
	Fmatrix					mRes;
	Fvector					R,D,N;
	D.sub					(mL.c,mR.c);	

	if(fis_zero(D.magnitude())) {
		mRes.set			(go->XFORM());
		mRes.c.set			(mR.c);
	}
	else {		
		D.normalize			();

		R.crossproduct		(mR.j,D);
		R.normalize			();

		N.crossproduct		(D,R);			
		N.normalize			();

		mRes.set			(R,N,D,mR.c);
		mRes.mulA_43		(go->XFORM());
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

			if(H_Parent() && H_Parent()->cast_actor() && render_item_ui_query())
			{
				Level().Cameras().camera_Matrix(parent);
				parent.j.invert();
				parent.i.invert();
			}

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
		if (!H_Parent()->cast_entity())		return;
		Fvector					p, d; 
		H_Parent()->cast_entity()->g_fireParams	(this, p,d);

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
	VERIFY2(!fis_zero(cam_recoil.RelaxSpeed), make_string<const char*>("Section [%s], line cam_relax_speed = %f", section, temp_f));
	if ( fis_zero(cam_recoil.RelaxSpeed) )
	{
		cam_recoil.RelaxSpeed = EPS_L;
	}

	cam_recoil.RelaxSpeed_AI = cam_recoil.RelaxSpeed;
	if ( pSettings->line_exist( section, "cam_relax_speed_ai" ) )
	{
		temp_f						= pSettings->r_float( section, "cam_relax_speed_ai" );
		cam_recoil.RelaxSpeed_AI	= _abs( deg2rad( temp_f ) );
		VERIFY2(!fis_zero(cam_recoil.RelaxSpeed_AI), make_string<const char*>("Section [%s], line cam_relax_speed_ai = %f", section, temp_f));
		if ( fis_zero(cam_recoil.RelaxSpeed_AI) )
		{
			cam_recoil.RelaxSpeed_AI = EPS_L;
		}
	}
	temp_f						= pSettings->r_float( section, "cam_max_angle" );
	cam_recoil.MaxAngleVert		= _abs( deg2rad( temp_f ) );
	VERIFY2(!fis_zero(cam_recoil.MaxAngleVert), make_string<const char*>("Section [%s], line cam_max_angle = %f", section, temp_f));

	if ( fis_zero(cam_recoil.MaxAngleVert) )
	{
		cam_recoil.MaxAngleVert = EPS;
	}
	
	temp_f						= pSettings->r_float( section, "cam_max_angle_horz" );
	cam_recoil.MaxAngleHorz		= _abs( deg2rad( temp_f ) );
	VERIFY2(!fis_zero(cam_recoil.MaxAngleHorz), make_string<const char*>("Section [%s], line cam_max_angle_horz = %f", section, temp_f));

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
		VERIFY2(!fis_zero(zoom_cam_recoil.RelaxSpeed), make_string<const char*>("Section [%s], line zoom_cam_relax_speed = %f", section, zoom_cam_recoil.RelaxSpeed));
		if ( fis_zero(zoom_cam_recoil.RelaxSpeed) )
		{
			zoom_cam_recoil.RelaxSpeed = EPS_L;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_relax_speed_ai" ) )
	{
		zoom_cam_recoil.RelaxSpeed_AI	= _abs( deg2rad( pSettings->r_float( section,"zoom_cam_relax_speed_ai" ) ) );
		VERIFY2(!fis_zero(zoom_cam_recoil.RelaxSpeed_AI), make_string<const char*>("Section [%s], line zoom_cam_relax_speed_ai = %f", section, zoom_cam_recoil.RelaxSpeed_AI));
		if ( fis_zero(zoom_cam_recoil.RelaxSpeed_AI) )
		{
			zoom_cam_recoil.RelaxSpeed_AI = EPS_L;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_max_angle" ) )
	{
		zoom_cam_recoil.MaxAngleVert	= _abs( deg2rad( pSettings->r_float( section, "zoom_cam_max_angle" ) ) );
		VERIFY2(!fis_zero(zoom_cam_recoil.MaxAngleVert), make_string<const char*>("Section [%s], line zoom_cam_max_angle = %f", section, zoom_cam_recoil.MaxAngleVert));

		if ( fis_zero(zoom_cam_recoil.MaxAngleVert) )
		{
			zoom_cam_recoil.MaxAngleVert = EPS;
		}
	}
	if ( pSettings->line_exist( section, "zoom_cam_max_angle_horz" ) )
	{
		zoom_cam_recoil.MaxAngleHorz	= _abs( deg2rad( pSettings->r_float( section, "zoom_cam_max_angle_horz" ) ) ); 
		VERIFY2(!fis_zero(zoom_cam_recoil.MaxAngleHorz), make_string<const char*>("Section [%s], line zoom_cam_max_angle_horz = %f", section, zoom_cam_recoil.MaxAngleHorz));

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

	m_first_bullet_controller.load	(section);
	fireDispersionConditionFactor = pSettings->r_float(section,"fire_dispersion_condition_factor");

    if (pSettings->line_exist(section, "misfire_start_condition") ||
        pSettings->line_exist(section, "misfire_end_condition") ||
        pSettings->line_exist(section, "misfire_start_prob") ||
        pSettings->line_exist(section, "misfire_end_prob"))
    {
        misfireStartCondition   = pSettings->r_float(section, "misfire_start_condition");
        misfireEndCondition     = pSettings->r_float(section, "misfire_end_condition");
        misfireStartProbability = pSettings->r_float(section, "misfire_start_prob");
        misfireEndProbability   = pSettings->r_float(section, "misfire_end_prob");
    }
    else
    {
		useLegacyMisfire = true;

        misfireProbability      = pSettings->r_float(section, "misfire_probability");
        misfireConditionK       = READ_IF_EXISTS(pSettings, r_float, section, "misfire_condition_k", 1.0f);

        // For UI indicators to work correctly
        misfireStartCondition   = 0.95f;
        misfireEndCondition     = 0.0f;
        misfireStartProbability = misfireProbability;
        misfireEndProbability   = (misfireProbability + misfireConditionK) * 0.25f;
    }
	conditionDecreasePerShot = pSettings->r_float(section, "condition_shot_dec");
	conditionDecreasePerQueueShot = READ_IF_EXISTS(pSettings, r_float, section, "condition_queue_shot_dec", conditionDecreasePerShot);


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

	bUseAltScope = !!bLoadAltScopesParams(section);

	if (!bUseAltScope)
		LoadOriginalScopesParams(section);
    
	if ( m_eSilencerStatus == ALife::eAddonAttachable )
	{
		m_sSilencerName = pSettings->r_string(section,"silencer_name");

		m_iSilencerX = pSettings->r_s32(section, "silencer_x") * (1 + isHQIcons);
		m_iSilencerY = pSettings->r_s32(section, "silencer_y") * (1 + isHQIcons);
	}
    
	if ( m_eGrenadeLauncherStatus == ALife::eAddonAttachable )
	{
		m_sGrenadeLauncherName = pSettings->r_string(section,"grenade_launcher_name");

		m_iGrenadeLauncherX = pSettings->r_s32(section, "grenade_launcher_x") * (1 + isHQIcons);
		m_iGrenadeLauncherY = pSettings->r_s32(section, "grenade_launcher_y") * (1 + isHQIcons);
	}

	UpdateAltScope();
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
	{
		hud_silencer = pSettings->r_string(section, "hud_silencer");
	}

	if (m_bUseScopeHud)
	{
		hud_scope = pSettings->r_string(section, "hud_scope");
	}

	if (m_bUseGLHud)
	{
		hud_gl = pSettings->r_string(section, "hud_gl");
	}

	m_fHudFovZoomFactor = READ_IF_EXISTS(pSettings, r_float, hud_sect, "hud_fov_zoom_factor", m_fHudFovFactor);
	m_fHudFovGLZoomFactor = READ_IF_EXISTS(pSettings, r_float, hud_sect, "hud_fov_gl_zoom_factor", m_fHudFovFactor);
	m_HudFovZoom = READ_IF_EXISTS(pSettings, r_float, hud_sect, "hud_fov_zoom", 0.0f);

	// Added by Axel, to enable optional condition use on any item
	m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", true));

	m_bHideColimSightInAlter = READ_IF_EXISTS(pSettings, r_bool, section, "hide_collimator_sights_in_alter_zoom", true);

	m_bDisableFireModeAim = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "disable_firemode_aim", false);

	auto LoadVector = [&](RStringVec& vec, const char* sect)
		{
			if (pSettings->line_exist(section, sect))
			{
				LPCSTR S = pSettings->r_string(section, sect);
				if (S && S[0])
				{
					string128 Item = "";
					int count = _GetItemCount(S);
					for (int it = 0; it < count; ++it)
					{
						_GetItem(S, it, Item);
						vec.push_back(Item);
					}
				}
			}
		};

	LoadVector(m_bDefHideBones, "def_hide_bones");
	LoadVector(m_bDefShowBones, "def_show_bones");
	LoadVector(m_bDefHideBonesGLAttached, "def_hide_bones_override_when_gl_attached");
	LoadVector(m_bScopeShowBones, "no_scope_overriding_show_bones");
	LoadVector(m_bScopeHideBones, "no_scope_overriding_hide_bones");
	LoadVector(m_sCollimatorSightsBones, "collimator_sights_bones");

	Fvector tmp_vector = { -1.0f, -1.0f, 0.0f };
	tmp_vector = READ_IF_EXISTS(pSettings, r_fvector3, section, "collimator_breaking_params", tmp_vector);
	CollimatorBreakingParams.start_condition = tmp_vector.x;
	CollimatorBreakingParams.end_condition = tmp_vector.y;
	CollimatorBreakingParams.start_probability = tmp_vector.z;

	tmp_vector = READ_IF_EXISTS(pSettings, r_fvector3, section, "torch_breaking_params", tmp_vector);
	TorchBreakingParams.start_condition = tmp_vector.x;
	TorchBreakingParams.end_condition = tmp_vector.y;
	TorchBreakingParams.start_probability = tmp_vector.z;

	m_fCollimatorLevelsProblem = READ_IF_EXISTS(pSettings, r_float, section, "collimator_problems_level", 0.0f);

	m_bAmmoInChamber = READ_IF_EXISTS(pSettings, r_bool, section, "ammo_in_chamber", false);
	m_bHideColimSightInAlter = READ_IF_EXISTS(pSettings, r_bool, section, "hide_collimator_sights_in_alter_zoom", true);
	
	m_bRestGlSil = READ_IF_EXISTS(pSettings, r_bool, section, "restricted_gl_and_sil", false);
	
	m_bAddCartridgeInOpen = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "add_cartridge_in_open", false);

	m_bBlockUpdateAmmoBonesShooting = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "ammo_params_toggle_shooting", false);
	m_bUseLastAmmoType = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "ammo_params_use_last_cartridge_type", false);
	m_bUseChamberInUpdateBones = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "ammo_params_use_chamber", false);

	m_bBlockReload = READ_IF_EXISTS(pSettings, r_bool, section, "block_reload", false);

	m_fMisfireAfterProblemsLevel = READ_IF_EXISTS(pSettings, r_float, section, "misfire_after_problems_level", 10.0f);

	m_bNoJamFirstShot = READ_IF_EXISTS(pSettings, r_bool, section, "no_jam_in_first_shot", false);
	m_bActorCanShoot = READ_IF_EXISTS(pSettings, r_bool, section, "actor_can_shoot", true);

	m_bUseLightMis = READ_IF_EXISTS(pSettings, r_bool, section, "use_light_misfire", false);
	m_bDisableLightMisDet = READ_IF_EXISTS(pSettings, r_bool, HudSection(), "disable_light_misfires_with_detector", false);

	const static bool isImproveMis = EngineExternal()[EEngineExternalGame::EnableImproveWeaponMisfire];

	m_bJamNotShot = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "no_jam_fire", !isImproveMis);

	light_misfire.startcond = READ_IF_EXISTS(pSettings, r_float, section, "light_misfire_start_condition", 1.0f);
	light_misfire.endcond = READ_IF_EXISTS(pSettings, r_float, section, "light_misfire_end_condition", 0.0f);
	light_misfire.startprob = READ_IF_EXISTS(pSettings, r_float, section, "light_misfire_start_probability", 1.0f);
	light_misfire.endprob = READ_IF_EXISTS(pSettings, r_float, section, "light_misfire_end_probability", 0.0f);

	m_lens_zoom_params.factor_min = READ_IF_EXISTS(pSettings, r_float, section, "min_lens_factor", 1.0f);
	m_lens_zoom_params.factor_max = READ_IF_EXISTS(pSettings, r_float, section, "max_lens_factor", 1.0f);
	m_lens_zoom_params.speed = READ_IF_EXISTS(pSettings, r_float, section, "lens_speed", 0.0f);
	m_lens_zoom_params.gyro_period = READ_IF_EXISTS(pSettings, r_float, section, "lens_gyro_sound_period", 0.0f);
	m_lens_zoom_params.lens_factor_levels_count = READ_IF_EXISTS(pSettings, r_float, section, "lens_factor_levels_count", 5.0f);
	m_lens_zoom_params.delta = 1.0f / m_lens_zoom_params.lens_factor_levels_count;
	m_lens_zoom_params.need_lens_frame = READ_IF_EXISTS(pSettings, r_bool, section, "need_lens_frame", false);
	m_lens_zoom_params.force_zoom_sound = READ_IF_EXISTS(pSettings, r_bool, section, "force_zoom_sound", false);

	m_lens_zoom_params.target_position = 1.0f;
	m_lens_zoom_params.last_gyro_snd_time = Device.dwTimeGlobal;

	if (SoundExist(section, "snd_scope_brightness_plus"))
	{
		m_sounds.LoadSound(section, "snd_scope_brightness_plus", "sndScopeBrightnessPlus", false, SOUND_TYPE_ITEM_TAKING);
	}

	if (SoundExist(section, "snd_scope_brightness_minus"))
	{
		m_sounds.LoadSound(section, "snd_scope_brightness_minus", "sndScopeBrightnessMinus", false, SOUND_TYPE_ITEM_TAKING);
	}

	if (SoundExist(section, "snd_scope_zoom_plus"))
	{
		m_sounds.LoadSound(section, "snd_scope_zoom_plus", "sndScopeZoomPlus", false, SOUND_TYPE_ITEM_TAKING);
	}

	if (SoundExist(section, "snd_scope_zoom_minus"))
	{
		m_sounds.LoadSound(section, "snd_scope_zoom_minus", "sndScopeZoomMinus", false, SOUND_TYPE_ITEM_TAKING);
	}

	if (SoundExist(section, "snd_scope_zoom_gyro"))
	{
		m_sounds.LoadSound(section, "snd_scope_zoom_gyro", "sndScopeZoomGyro", false, SOUND_TYPE_ITEM_TAKING);
	}

	shared_str scope_sect = section;
	if (IsScopeAttached() && get_ScopeStatus() == 2)
	{
		scope_sect = GetScopeName();
	}

	LoadNightBrightnessParamsFromSection(scope_sect);

	m_mags_capacity.clear();
	for (int i = 0; i < m_ammoTypes.size(); i++)
	{
		static shared_str capacity_value;
		capacity_value.printf("ammo_mag_size_for_type_%d", i);
		if (pSettings->line_exist(section, *capacity_value))
		{
			m_mags_capacity.emplace_back(static_cast<u8>(i), pSettings->r_u32(section, *capacity_value));
		}
	}

	if (pSettings->line_exist(hud_sect, "shell_params_section"))
	{
		SAmmoBonesParams* bone_params = new SAmmoBonesParams(undefined_ammo_type);
		bone_params->Load(pSettings->r_string(hud_sect, "shell_params_section"), iMagazineSize + 1);
		m_shell_bones.push_back(bone_params);
	}
	else for (int i = 0; i < m_ammoTypes.size(); i++)
	{
		static shared_str params_section;
		params_section.printf("shell_params_section_%d", i);
		if (pSettings->line_exist(hud_sect, *params_section))
		{
			SAmmoBonesParams* bone_params = new SAmmoBonesParams(i);
			bone_params->Load(pSettings->r_string(hud_sect, *params_section), iMagazineSize + 1);
			m_shell_bones.push_back(bone_params);
		}
	}

	if (pSettings->line_exist(hud_sect, "ammo_params_section") && pSettings->section_exist(pSettings->r_string(hud_sect, "ammo_params_section")))
	{
		SAmmoBonesParams* bone_params = new SAmmoBonesParams(undefined_ammo_type);
		bone_params->Load(pSettings->r_string(hud_sect, "ammo_params_section"), iMagazineSize + 1);
		m_ammo_bones_mag.push_back(bone_params);
	}
	else for (int i = 0; i < m_ammoTypes.size(); i++)
	{
		static shared_str params_section;
		params_section.printf("ammo_params_section_%d", i);
		if (pSettings->line_exist(hud_sect, *params_section))
		{
			SAmmoBonesParams* bone_params = new SAmmoBonesParams(i);
			bone_params->Load(pSettings->r_string(hud_sect, *params_section), iMagazineSize + 1);
			m_ammo_bones_mag.push_back(bone_params);
		}
	}
}

void CWeapon::SAmmoBonesParams::Load(const shared_str& section, u32 size)
{
	if (!AllBones.empty())
	{
		AllBones.clear();
	}
	if (pSettings->line_exist(section, "all_bones"))
	{
		LPCSTR S = pSettings->r_string(section, "all_bones");
		if (S && S[0])
		{
			string128 Item = {};
			u32 count = _GetItemCount(S);
			for (u32 it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				AllBones.push_back(Item);
			}
		}
	}

	static shared_str configuration;

	if (ConfigurationMap.size() > 0)
	{
		for (auto& node : ConfigurationMap)
		{
			if (!node.second.second.empty())
			{
				node.second.second.clear();
			}
		}
	}

	ConfigurationMap.clear();

	for (u32 i = 0; i < size; ++i)
	{
		auto& ConfigNode = ConfigurationMap[i];
		configuration.printf("configuration_%d", i);
		ConfigNode.first = configuration;
		if (pSettings->line_exist(section, configuration))
		{
			LPCSTR S = pSettings->r_string(section, *configuration);
			if (S && S[0])
			{
				string128 Item = {};
				u32 count = _GetItemCount(S);
				for (u32 it = 0; it < count; ++it)
				{
					ConfigNode.second.push_back(_GetItem(S, it, Item));
				}
			}
		}
	}
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

	iAmmoChamberElapsed = E->a_chamber_elapsed;
	m_ChamberAmmoType = E->chamber_ammo_type;

	if (m_bAmmoInChamber)
	{
		m_DefaultCartridgeInChamber.Load(*getAmmoTypes()[m_ChamberAmmoType], u8(m_ChamberAmmoType));
		if (iAmmoChamberElapsed)
		{
			m_fCurrentCartirdgeDisp = m_DefaultCartridgeInChamber.param_s.kDisp;
			for (int i = 0; i < iAmmoChamberElapsed; ++i)
				m_chamber.push_back(m_DefaultCartridgeInChamber);
		}
	}
	else
	{
		m_chamber.clear();
		iAmmoChamberElapsed = 0;
		iChamberSize = 0;
	}

	GiveAmmoFromMagToChamber();

	UpdateAltScope();
	UpdateAddonsVisibility();
	UpdateHUDAddonsVisibility();
	ProcessScope();
	InitAddons();

	m_dwWeaponIndependencyTime = 0;

	VERIFY((u32)iAmmoElapsed == m_magazine.size());
	m_bAmmoWasSpawned		= false;


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

	m_magazine.clear();
	m_chamber.clear();
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
	P.w_u8					((u8)m_ChamberAmmoType);
	P.w_u16					((u16)iAmmoChamberElapsed);
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

	UpdateAddonsVisibility();
	UpdateHUDAddonsVisibility();
	ProcessScope();

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

	u8 chamber_type;
	P.r_u8(chamber_type);
	m_ChamberAmmoType = chamber_type;

	u16 chamber_ammo_elapsed = 0;
	P.r_u16(chamber_ammo_elapsed);

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
				if (m_bAmmoInChamber)
				{
					SetChamberAmmoElapsed(chamber_ammo_elapsed);
					GiveAmmoFromMagToChamber();
				}
			}
		}break;
	}
	
	VERIFY((u32)iAmmoElapsed == m_magazine.size());
}

void CWeapon::save(NET_Packet &output_packet)
{
	inherited::save	(output_packet);
	save_data		(iAmmoElapsed,					output_packet);
	save_data		(iAmmoChamberElapsed,			output_packet);
	save_data		(m_cur_scope, 					output_packet);
	save_data		(m_flagsAddOnState, 			output_packet);
	save_data		(m_ammoType,					output_packet);
	save_data		(m_ChamberAmmoType,				output_packet);
	save_data		(m_zoom_params.m_bIsZoomModeNow,output_packet);
	save_data		(m_bTacticalTorchStatus,		output_packet);
	save_data		(m_bJustAfterReload,			output_packet);
	save_data		(m_LastShotAmmoType,			output_packet);
	save_data(m_lens_zoom_params.target_position,	output_packet);
	save_data(m_lens_night_brightness.cur_step,		output_packet);
}

void CWeapon::load(IReader &input_packet)
{
	inherited::load	(input_packet);
	load_data		(iAmmoElapsed,					input_packet);
	load_data		(iAmmoChamberElapsed,			input_packet);
	load_data		(m_cur_scope,					input_packet);
	load_data		(m_flagsAddOnState,				input_packet);
	load_data		(m_ammoType,					input_packet);
	load_data		(m_ChamberAmmoType,				input_packet);
	load_data		(m_zoom_params.m_bIsZoomModeNow,input_packet);
	load_data		(m_bTacticalTorchStatus,		input_packet);
	load_data		(m_bJustAfterReload,			input_packet);
	load_data		(m_LastShotAmmoType,			input_packet);
	load_data(m_lens_zoom_params.target_position,	input_packet);
	load_data(m_lens_night_brightness.lens_night_brightness_saved_step,	input_packet);

	if (m_zoom_params.m_bIsZoomModeNow)	
	{
		OnZoomIn();
	}
	else			
	{
		OnZoomOut();
	}

	UpdateTorch();
	UpdateAddonsVisibility();
	UpdateHUDAddonsVisibility();
	ProcessScope();
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
			UpdateHUDAddonsVisibility();
			ProcessScope();
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
	PROF_EVENT("CWeapon::shedule_Update")
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
	ProcessScope();
	Engine.Sheduler.Unregister(this);
	//Engine.Sheduler.Register(this);
};

void CWeapon::OnH_A_Chield		()
{
	inherited::OnH_A_Chield		();
	UpdateAddonsVisibility		();
	shedule.t_min = shedule.t_max = 1;
	//Engine.Sheduler.Unregister(this);
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
	m_bBlockEmptyClick = false;
	bWorking = false;
}

bool CWeapon::SendDeactivateItem(bool Force)
{
	if (GetState() == eFire)
	{
		if (!Force)
		{
			return false;
		}

		FireEnd();
	}

	return inherited::SendDeactivateItem(Force);
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
	m_bBlockEmptyClick = false;
	bWorking = false;
	SetState(eHidden);
}

extern u32 hud_adj_mode;

void set_pp_effector_factor2(int id, float f);

void CWeapon::UpdateCL		()
{
	u32 delta = Device.GetTimeDeltaSafe(_last_update_time);

	bool need_update_hud = false;
	bool isHudItemData = HudItemData() != nullptr;

	if (isHudItemData && bUseAltScope) {
		need_update_hud = true;
	}
	
	if (isHudItemData && !bUpdateHUDBonesVisibility)
	{
		bUpdateHUDBonesVisibility = true;
		need_update_hud = true;
	}
	else if (!isHudItemData)
	{
		bUpdateHUDBonesVisibility = false;
	}

	if (need_update_hud)
	{
		ForceUpdateHUD();
	}

	UpdateCollimatorSight();
	UpdateTorch();

	inherited::UpdateCL		();

	//подсветка от выстрела
	UpdateLight				();

	//нарисовать партиклы
	UpdateFlameParticles	();
	UpdateFlameParticles2	();

	if(!IsGameTypeSingle())
		make_Interpolation		();

	if (!IsZoomed() && m_bIsAimStarted)
		m_bIsAimStarted = false;

	if (ParentIsActor())
	{
		if (GetNightVision() && !need_renderable())
		{
			if (!GetNightVision()->IsActive())
			{
				GetNightVision()->SwitchNightVision(true);
			}
			else
			{
				float val = GetNightPPEFactor();

				if (val >= 0.0f)
				{
					set_pp_effector_factor2(effWeaponNightVision, val);
				}
			}
		}
	}

	if (AllowBore())
	{
		if (GetNextState() == GetState() && IsGameTypeSingle() && H_Parent() == Level().CurrentEntity())
		{
			CActor* pActor	= H_Parent() ? H_Parent()->cast_actor() : NULL;
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

	if (!!GetHUDmode()) {
		m_current_inertion.lerp(m_base_inertion, m_zoom_inertion, m_zoom_params.m_fZoomRotationFactor);
	}
	else
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : NULL;
		if ((IsZoomed() && m_zoom_params.m_fZoomRotationFactor <= 1.f) ||
			(!IsZoomed() && m_zoom_params.m_fZoomRotationFactor > 0.f))
		{
			if(pActor && pActor->IsZoomAimingMode())
				m_zoom_params.m_fZoomRotationFactor += Device.fTimeDelta/m_zoom_params.m_fZoomRotateTime;
			else
				m_zoom_params.m_fZoomRotationFactor -= Device.fTimeDelta/m_zoom_params.m_fZoomRotateTime;

			clamp(m_zoom_params.m_fZoomRotationFactor, 0.f, 1.f);
		}
	}

	if (m_zoom_params.m_pVision)
	{
		m_zoom_params.m_pVision->Update();
	}

	UpdateLensFactor(delta);

	_last_update_time = Device.dwTimeGlobal;
}

void CWeapon::ForceUpdateHUD()
{
	UpdateScopePosition();
	UpdateHUDAddonsVisibility();
	ProcessScope();
	u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, type_to_update);
	UpdateShellBones(iAmmoElapsed, m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType());
}

void CWeapon::SwitchTorch(bool status, bool forced)
{
	if (!m_HudLight.GetTorchInstalled())
	{
		return;
	}

	if (!forced && status == m_bTacticalTorchStatus)
	{
		return;
	}

	m_bTacticalTorchStatus = status;

	m_HudLight.SwitchTorchlight(status);
}

void CWeapon::UpdateTorch()
{
	if (!m_HudLight.GetTorchInstalled())
	{
		return;
	}

	if (H_Parent() != nullptr && !ParentIsActor())
	{
		SwitchTorch(false);
	}

	SwitchTorch(m_bTacticalTorchStatus, true);

	bool is_broken = false;
	float current_condition = GetCondition();

	if (current_condition < TorchBreakingParams.end_condition)
	{
		is_broken = true;
	}
	else if (current_condition < TorchBreakingParams.start_condition)
	{
		is_broken = (::Random.randF(0.0f, 1.0f) < TorchBreakingParams.start_probability +
			(TorchBreakingParams.start_condition - current_condition) *
			(1.0f - TorchBreakingParams.start_probability) /
			(TorchBreakingParams.start_condition - TorchBreakingParams.end_condition));
	}

	if (is_broken)
	{
		m_HudLight.SwitchTorchlight(false);
	}

	if (m_HudLight.GetTorchInstalled())
	{
		if (attachable_hud_item* item = HudItemData())
		{
			for (const shared_str& bone : m_HudLight.ConeBones)
			{
				item->set_bone_visible(bone, m_HudLight.GetTorchActive(), TRUE);
			}
		}
	}
}

void CWeapon::LoadUpgradeBonesToHide(const char* section, const char* line)
{
	if (!pSettings->section_exist(section))
		return;

	if (!!pSettings->line_exist(section, line))
	{
		LPCSTR	S = pSettings->r_string(section, line);
		if (S && S[0])
		{
			string128 _Item = "";
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, _Item);
				m_bShowBonesUpgToHide.push_back(_Item);
			}
		}
	}
}

void CWeapon::ProcessScope()
{
	s32 cur_index = -1;

	if (IsScopeAttached() && get_ScopeStatus() == 2)
		cur_index = m_cur_scope;

	for (u32 i = 0; i < m_scopes.size(); ++i)
	{
		shared_str tmp = GetScopeSection(i);
		bool status = (i == cur_index);

		if (pSettings->line_exist(tmp, "bones"))
			SetMultipleBonesStatus(tmp.c_str(), "bones", status);

		if (pSettings->line_exist(tmp, "hide_bones"))
			SetMultipleBonesStatus(tmp.c_str(), "hide_bones", !status);
	}

	if (cur_index >= 0)
	{
		shared_str tmp = GetScopeSection(cur_index);
		if (pSettings->line_exist(tmp, "overriding_hide_bones"))
			SetMultipleBonesStatus(tmp.c_str(), "overriding_hide_bones", false);

		if (pSettings->line_exist(tmp, "overriding_show_bones"))
			SetMultipleBonesStatus(tmp.c_str(), "overriding_show_bones", true);
	}
	else
	{
		IKinematics* pWeaponVisual = Visual()->dcast_PKinematics();
		R_ASSERT(pWeaponVisual);

		pWeaponVisual->CalculateBones_Invalidate();

		auto ChangeBoneVisible = [&](const shared_str& bone, bool status)
			{
				u16 bone_id = pWeaponVisual->LL_BoneID(bone);

				if (bone_id != BI_NONE)
					pWeaponVisual->LL_SetBoneVisible(bone_id, status, TRUE);
			};

		for (auto& bone : m_bScopeHideBones)
		{
			ChangeBoneVisible(bone, false);
		}

		for (auto& bone : m_bScopeShowBones)
		{
			ChangeBoneVisible(bone, true);
		}

		if (HudItemData() == nullptr)
			return;

		for (auto& bone : m_bScopeHideBones)
		{
			HudItemData()->set_bone_visible(bone, false, TRUE);
		}

		for (auto& bone : m_bScopeShowBones)
		{
			HudItemData()->set_bone_visible(bone, true, TRUE);
		}
	}
}

void CWeapon::HideOneUpgradeLevel(const char* section)
{
	if (!!pSettings->line_exist(section, "elements"))
	{
		LPCSTR	S = pSettings->r_string(section, "elements");
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
				LoadUpgradeBonesToHide(up_sect, "show_bones");
			}
		}
	}
}

bool CWeapon::need_renderable()
{
	return !(IsZoomed() && ZoomTexture() && !IsRotatingToZoom() && !IsHudModelForceUnhide());
}

void CWeapon::renderable_Render		()
{
	UpdateXForm				();

	//нарисовать подсветку

	RenderLight				();	

	//если мы в режиме снайперки, то сам HUD рисовать не надо
	if(IsZoomed() && !IsRotatingToZoom() && ZoomTexture())
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

bool CWeapon::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;

	
	switch(cmd) 
	{
		case kWPN_FIRE:
			{
				if (IsTriStateReload() && GetState() == eReload && (m_sub_state == eSubstateReloadInProcess || m_bAddCartridgeInOpen && m_sub_state == eSubstateReloadBegin) && flags & CMD_START)
				{
					bStopReloadSignal = true;
					return true;
				}
				if(IsPending())		
					return false;

				if (flags&CMD_START) 
					FireStart();
				else 
					FireEnd();

				return true;
			} 
		case kWPN_NEXT: 
			{
				return SwitchAmmoType(flags);
			} 
		case kWPN_ZOOM:
		{
			if (IsZoomEnabled())
			{
				if (b_toggle_weapon_aim)
				{
					if (flags & CMD_START)
					{
						if (!IsZoomed())
						{
							if (!IsPending())
							{
								if (!CanAimNow())
								{
									CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;

									if (pActor && !b_toggle_weapon_aim && pActor->GetMovementState(eReal) & mcSprint)
									{
										pActor->SetMovementState(eWishful, mcSprint, false);
									}

									return false;
								}

								if (GetState() != eIdle)
								{
									SwitchState(eIdle);
								}

								OnZoomIn();
							}
						}
						else
						{
							if (!CanLeaveAimNow())
							{
								if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
								{
									pActor->SetActorKeyRepeatFlag(kfUNZOOM, true);
								}
								return false;
							}

							OnZoomOut();
						}
					}
				}
				else
				{
					if (flags & CMD_START)
					{
						if (!CanAimNow())
						{
							CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;

							if (pActor && !b_toggle_weapon_aim && pActor->GetMovementState(eReal) & mcSprint)
							{
								pActor->SetMovementState(eWishful, mcSprint, false);
							}

							return false;
						}

						if (!IsZoomed() && !IsPending())
						{
							if (GetState() != eIdle)
							{
								SwitchState(eIdle);
							}

							OnZoomIn();
						}
					}
					else if (IsZoomed())
					{
						if (!CanLeaveAimNow())
						{
							if (CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr)
							{
								pActor->SetActorKeyRepeatFlag(kfUNZOOM, true);
							}
							return false;
						}

						OnZoomOut();
					}
				}
				return true;
			}
			else
			{
				return false;
			}
		}break;
		case kBRIGHTNESS_PLUS:
		case kBRIGHTNESS_MINUS:
		{
			if ((flags & CMD_START) && IsZoomEnabled() && IsZoomed() && GetState() == eIdle && !IsActionProcessing())
			{
				ChangeNightBrightness(cmd == kBRIGHTNESS_MINUS  ? -1 : 1);
				return true;
			}
		}break;
		case kWPN_ZOOM_INC:
		case kWPN_ZOOM_DEC:
		{
			if (IsZoomEnabled() && IsZoomed() && (flags & CMD_START))
			{

				if (cmd == kWPN_ZOOM_INC)
				{
					ZoomInc();
				}
				else
				{
					ZoomDec();
				}
				return true;
			}
		}break;
	}

	return false;
}

bool CWeapon::SwitchAmmoType(u32 flags)
{
	if (OnClient() || !(flags & CMD_START))
	{
		return false;
	}

	if (m_bBlockReload && GetState() != eIdle)
	{
		return false;
	}

	if (IsMisfire() && !IsGrenadeMode())
	{
		return false;
	}

	if (IsTriStateReload() && iAmmoElapsed == iMagazineSize)
	{
		return false;
	}

	const static bool isDelayedWeaponActions = EngineExternal()[EEngineExternalGame::EnableDelayedWeaponActions];

	if (!isDelayedWeaponActions && IsPending())
	{
		return false;
	}

	u8 l_newType = m_ammoType;
	bool b1, b2;
	do
	{
		l_newType = u8((u32(l_newType + 1)) % m_ammoTypes.size());
		b1 = (l_newType != m_ammoType);
		b2 = unlimited_ammo() ? false : (!m_pInventory->GetAny(m_ammoTypes[l_newType].c_str()));
	} while (b1 && b2);

	if (l_newType != m_ammoType)
	{
		m_set_next_ammoType_on_reload = l_newType;
		if (OnServer() && SetKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags::kfNEXTAMMO))
		{
			Reload();
		}
	}

	return true;
}

void CWeapon::Set_PDM_Base(float value)
{
	m_pdm.m_fPDM_disp_base = value;
}

void CWeapon::Set_PDM_Vel_F(float value)
{
	m_pdm.m_fPDM_disp_vel_factor = value;
}

void CWeapon::Set_PDM_Accel_F(float value)
{
	m_pdm.m_fPDM_disp_accel_factor = value;
}

void CWeapon::Set_PDM_Crouch(float value)
{
	m_pdm.m_fPDM_disp_crouch = value;
}

void CWeapon::Set_PDM_Crouch_NA(float value)
{
	m_pdm.m_fPDM_disp_crouch_no_acc = value;
}

void CWeapon::setCrosshairInertion(float value)
{
	m_crosshair_inertion = value;
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

void CWeapon::SetAmmoMagSize(int size)
{
	iMagazineSize = size;
}

int CWeapon::GetSuitableAmmoTotal( bool use_item_to_spawn ) const
{
	int ae_count = iAmmoElapsed + iAmmoChamberElapsed;
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

	for (PIItem item : m_pInventory->m_belt)
	{
		CWeaponAmmo* pAmmo = item->cast_weapon_ammo();
		if (pAmmo && pAmmo->cNameSect() == ammo_type)
		{
			res += pAmmo->m_boxCurr;
		}
	}

	for (PIItem item : m_pInventory->m_ruck)
	{
		CWeaponAmmo* pAmmo = item->cast_weapon_ammo();
		if (pAmmo && pAmmo->cNameSect() == ammo_type)
		{
			res += pAmmo->m_boxCurr;
		}
	}
	return res;
}

float CWeapon::GetConditionMisfireProbability() const
{
	// modified by Peacemaker [17.10.08]
	float mis;
	if (useLegacyMisfire)
	{
		if (GetCondition() > 0.95f)
			return 0.0f;
		mis = misfireProbability + powf(1.f - GetCondition(), 3.f) * misfireConditionK;
	}
	else {
		if (GetCondition() > misfireStartCondition)
			return 0.0f;
		if (GetCondition() < misfireEndCondition)
			return misfireEndProbability;
		mis = misfireStartProbability + (
			(misfireStartCondition - GetCondition()) *				// condition goes from 1.f to 0.f
			(misfireEndProbability - misfireStartProbability) /		// probability goes from 0.f to 1.f
			((misfireStartCondition == misfireEndCondition) ?		// !!!say "No" to devision by zero
				misfireStartCondition :
				(misfireStartCondition - misfireEndCondition))
			);
	}
	clamp(mis, 0.0f, 0.99f);
	return mis;
}

bool CWeapon::IsJamProhibited()
{
	// [bug] в классе РГ-6 выстрел ракеты происходит до заклина оружия, что может мешать.
	//if (smart_cast<CWeaponRG6*>(this) != nullptr && m_bJamNotShot)
	//{
	//	if (rg6_misfire_assign_allowed)
	//		return true;
	//}

	if (IsGrenadeMode())
	{
		return false;
	}

	// Запрет клина в первом выстреле после перезарядки
	if (m_bJustAfterReload && m_bNoJamFirstShot)
	{
		return true;
	}

	return false;
}

bool CWeapon::OnWeaponJam()
{
	CActor* pActor = H_Parent()->cast_actor();

	//_wanim_force_assign = true;

	//if (pActor->IsActorSuicideNow())
	//{
	//	return false;
	//}

	if (m_bUseLightMis && !(pActor->GetDetector() != nullptr && m_bDisableLightMisDet))
	{
		float curcond = GetCondition();
		float startcond = light_misfire.startcond;
		float endcond = light_misfire.endcond;
		float startprob = light_misfire.startprob;
		float endprob = light_misfire.endprob;

		float curprob = 0.0f;

		if (curcond < endcond)
		{
			curprob = endprob;
		}
		else if (curcond > startcond)
		{
			curprob = 0.0f;
		}
		else
		{
			curprob = endprob + curcond * (startprob - endprob) / (startcond - endcond);
		}

		if (::Random.randF(0.0f, 1.0f) < curprob)
		{
			//ApplyLensRecoil(GetMisfireRecoil());
			SetState(eLightMis);
			SetNextState(eLightMis);
			SwitchState(eLightMis);
			return true;
		}
	}

	if (m_bJamNotShot)
	{
		SwitchState(eMisfire);
		return true;
	}

	SetMisfireStatus(true);
	return false;
}

bool CWeapon::CheckForMisfire_validate_NoMisfire()
{
	if (CActor* pActor = H_Parent()->cast_actor())
	{
		float problems_lvl = m_fMisfireAfterProblemsLevel;

		if (problems_lvl > 0.0f && pActor->CurrentElectronicsProblemsCnt() >= problems_lvl)
		{
			return OnWeaponJam();
		}

		if (!m_bActorCanShoot)
		{
			SwitchState(eMisfire);
			return true;
		}
	}

	return false;
}

bool CWeapon::CheckForMisfire()
{
	if (OnClient())
	{
		return false;
	}

	if (!ParentIsActor())
	{
		return false;
	}

	float rnd = ::Random.randF(0.f, 1.f);
	float mp = GetConditionMisfireProbability();

	const static bool isImproveMis = EngineExternal()[EEngineExternalGame::EnableImproveWeaponMisfire];

	if (rnd < mp)
	{
		FireEnd();

		if (!isImproveMis)
		{
			SetMisfireStatus(true);
			SwitchState(eMisfire);
			return true;
		}

		if (IsJamProhibited())
		{
			SetMisfireStatus(false);
			return false;
		}
		else
		{
			return OnWeaponJam();
		}
	}
	else if (isImproveMis)
	{
		return CheckForMisfire_validate_NoMisfire();
	}

	return false;
}


bool CWeapon::IsMisfire() const
{	
	return bMisfire;
}

void CWeapon::Reload()
{
	OnZoomOut();
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

void CWeapon::UpdateScopePosition()
{
	auto HID = HudItemData();

	if (HID != nullptr && ScopeAttachable())
	{
		shared_str hands_section = HID->m_measures.m_hands_positions.sSection;
		shared_str scope_section = GetCurrentScopeSection();
		shared_str hud_section = HudSection();

		bool is_16x9 = UI().is_widescreen();

		if (IsScopeAttached())
		{
			if (hands_section != scope_section)
			{
				HID->m_measures.m_hands_positions.Load(scope_section, is_16x9);
			}
		}
		else if (hands_section != hud_section)
		{
			HID->m_measures.m_hands_positions.Load(hud_section, is_16x9);
		}
	}
}

static const char* wpn_scope = "wpn_scope";
static const char* wpn_silencer = "wpn_silencer";
static const char* wpn_grenade_launcher = "wpn_launcher";

void CWeapon::UpdateHUDAddonsVisibility()
{
	if (!GetHUDmode())
		return;

	bool test = !!(get_ScopeStatus() == 2 && IsScopeAttached() || get_ScopeStatus() == 1);

	HudItemData()->set_bone_visible(wpn_scope, test, TRUE);

	test = !!(get_SilencerStatus() == 2 && IsSilencerAttached() || get_SilencerStatus() == 1);

	HudItemData()->set_bone_visible(wpn_silencer, test, TRUE);

	test = !!(get_GrenadeLauncherStatus() == 2 && IsGrenadeLauncherAttached() || get_GrenadeLauncherStatus() == 1);

	HudItemData()->set_bone_visible(wpn_grenade_launcher, test, TRUE);

	for (auto& bone : m_bDefHideBones)
	{
		HudItemData()->set_bone_visible(bone, false, TRUE);
	}

	for (auto& bone : m_bDefShowBones)
	{
		HudItemData()->set_bone_visible(bone, true, TRUE);
	}

	if (m_bShowBonesUpgToHide.empty())
	{
		if (!!pSettings->line_exist(m_section_id.c_str(), "upgrades"))
		{
			LPCSTR S = pSettings->r_string(m_section_id.c_str(), "upgrades");
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
	}

	for (auto& bone : m_bShowBonesUpgToHide)
	{
		HudItemData()->set_bone_visible(bone, false, TRUE);
	}

	for (auto& bone : m_bShowBonesUpgToShow)
	{
		HudItemData()->set_bone_visible(bone, true, TRUE);
	}

	for (auto& bone : m_bHideBonesUpgrade)
	{
		HudItemData()->set_bone_visible(bone, false, TRUE);
	}

	for (u32 i = 0; i < m_upgrades.size(); i++)
	{
		LPCSTR section = pSettings->r_string(m_upgrades.at(i).c_str(), "section");

		if (pSettings->line_exist(section, "show_bones"))
			SetMultipleBonesStatus(section, "show_bones", TRUE);
	}

	for (auto& bone : m_bHideBonesOverride)
	{
		HudItemData()->set_bone_visible(bone, false, TRUE);
	}

	if (IsSilencerAttached())
	{
		for (auto& bone : m_bHideBonesSilAttached)
		{
			HudItemData()->set_bone_visible(bone, false, TRUE);
		}
	}

	if (IsScopeAttached())
	{
		for (auto& bone : m_bHideBonesScopeAttached)
		{
			HudItemData()->set_bone_visible(bone, false, TRUE);
		}
	}

	if (IsGrenadeLauncherAttached())
	{
		for (auto& bone : m_bHideBonesGLAttached)
		{
			HudItemData()->set_bone_visible(bone, false, TRUE);
		}
	}

	if (IsGrenadeLauncherAttached())
	{
		for (auto& bone : m_bDefHideBonesGLAttached)
		{
			HudItemData()->set_bone_visible(bone, false, TRUE);
		}
	}
}

void CWeapon::UpdateAddonsVisibility()
{
	IKinematics* pWeaponVisual = Visual()->dcast_PKinematics();
	R_ASSERT(pWeaponVisual);

	pWeaponVisual->CalculateBones_Invalidate();

	auto ChangeBoneVisible = [&](const shared_str& bone, bool status, bool child = true)
	{
			u16 bone_id = pWeaponVisual->LL_BoneID(bone);

			if (bone_id != BI_NONE)
				pWeaponVisual->LL_SetBoneVisible(bone_id, status, child);
	};

	bool test = !!(get_ScopeStatus() == 2 && IsScopeAttached() || get_ScopeStatus() == 1);
	ChangeBoneVisible(wpn_scope, test);

	test = !!(get_SilencerStatus() == 2 && IsSilencerAttached() || get_SilencerStatus() == 1);
	ChangeBoneVisible(wpn_silencer, test);

	test = !!(get_GrenadeLauncherStatus() == 2 && IsGrenadeLauncherAttached() || get_GrenadeLauncherStatus() == 1);
	ChangeBoneVisible(wpn_grenade_launcher, test);

	for (auto& bone : m_bDefHideBones)
	{
		ChangeBoneVisible(bone, false, false);
	}

	for (auto& bone : m_bDefShowBones)
	{
		ChangeBoneVisible(bone, true, false);
	}

	if (m_bShowBonesUpgToHide.empty())
	{
		if (!!pSettings->line_exist(m_section_id.c_str(), "upgrades"))
		{
			LPCSTR S = pSettings->r_string(m_section_id.c_str(), "upgrades");
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
	}

	for (auto& bone : m_bShowBonesUpgToHide)
	{
		ChangeBoneVisible(bone, false, false);
	}

	for (auto& bone : m_bShowBonesUpgToShow)
	{
		ChangeBoneVisible(bone, true, false);
	}

	for (auto& bone : m_bHideBonesUpgrade)
	{
		ChangeBoneVisible(bone, false, false);
	}

	for (u32 i = 0; i < m_upgrades.size(); i++)
	{
		LPCSTR section = pSettings->r_string(m_upgrades.at(i).c_str(), "section");

		if (pSettings->line_exist(section, "show_bones"))
			SetMultipleBonesStatus(section, "show_bones", TRUE);

	}

	for (auto& bone : m_bHideBonesOverride)
	{
		ChangeBoneVisible(bone, false, false);
	}

	if (IsSilencerAttached())
	{
		for (auto& bone : m_bHideBonesSilAttached)
		{
			ChangeBoneVisible(bone, false, false);
		}
	}

	if (IsScopeAttached())
	{
		for (auto& bone : m_bHideBonesScopeAttached)
		{
			ChangeBoneVisible(bone, false, false);
		}
	}

	if (IsGrenadeLauncherAttached())
	{
		for (auto& bone : m_bHideBonesGLAttached)
		{
			ChangeBoneVisible(bone, false, false);
		}
	}

	if (IsGrenadeLauncherAttached())
	{
		for (auto& bone : m_bDefHideBonesGLAttached)
		{
			ChangeBoneVisible(bone, false, false);
		}
	}

	if (m_HudLight.GetTorchInstalled())
	{
		for (const shared_str& bone : m_HudLight.ConeBones)
		{
			ChangeBoneVisible(bone, m_HudLight.GetTorchActive());
		}
	}

	pWeaponVisual->CalculateBones_Invalidate();
	pWeaponVisual->CalculateBones(TRUE);
}

void CWeapon::InitAddons()
{
	if (ScopeAttachable())
	{
		shared_str section = m_section_id;

		if (IsScopeAttached())
		{
			m_fHudFovZoomFactor = READ_IF_EXISTS(pSettings, r_float, GetScopeName(), "hud_fov_zoom_factor", m_fHudFovZoomFactor);
		}
		else
		{
			m_fHudFovZoomFactor = READ_IF_EXISTS(pSettings, r_float, section, "hud_fov_zoom_factor", m_fHudFovFactor);
		}

		if (IsScopeAttached())
		{
			m_fHudFovGLZoomFactor = READ_IF_EXISTS(pSettings, r_float, GetScopeName(), "hud_fov_gl_zoom_factor", m_fHudFovGLZoomFactor);
		}
		else
		{
			m_fHudFovGLZoomFactor = READ_IF_EXISTS(pSettings, r_float, section, "hud_fov_gl_zoom_factor", m_fHudFovFactor);
		}
	}
}

bool CWeapon::CanAimNow()
{
	CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;

	if (pActor == nullptr)
	{
		return true;
	}

	const static bool isDelayedWeaponActions = EngineExternal()[EEngineExternalGame::EnableDelayedWeaponActions];

	if (!isDelayedWeaponActions)
	{
		return true;
	}

	bool result = true;

	if (pActor && pActor->GetDetector() != nullptr)
	{
		result = !!(pActor->GetDetector()->GetState() == CCustomDetector::eIdle);
	}

	if (m_eAnimationsFlags.test(EAnimationsFlags::af_sprint_in_out) && (pActor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal) & ACTOR_DEFS::EMoveCommand::mcSprint || GetState() == eSprintStart || GetState() == eSprintEnd || m_bSwitchSprint))
	{
		result = false;
	}

	if (result)
	{
		if (IsGrenadeLauncherAttached() && IsGrenadeMode())
		{
			shared_str sect = HudSection();

			if (IsScopeAttached())
			{
				sect = GetCurrentScopeSection();
			}

			if (READ_IF_EXISTS(pSettings, r_bool, sect, "prohibit_aim_for_grenade_mode", false))
			{
				result = false;
			}
		}
	}

	return result;
}

bool CWeapon::CanLeaveAimNow()
{
	CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;

	if (pActor == nullptr)
	{
		return true;
	}

	const static bool isDelayedWeaponActions = EngineExternal()[EEngineExternalGame::EnableDelayedWeaponActions];

	if (!isDelayedWeaponActions)
	{
		return true;
	}

	//if (pActor->IsActorSuicideNow() || pActor->IsActorPlanningSuicide() || pActor->IsControllerPreparing())
	//{
	//	return true;
	//}

	if (IsActionProcessing() || m_eAnimationsFlags.test(EAnimationsFlags::af_aim_in_out) && GetState() != eIdle)
	{
		return false;
	}

	return true;
}

float CWeapon::CurrentZoomFactor()
{
	return IsScopeAttached() ? m_zoom_params.m_fScopeZoomFactor : m_zoom_params.m_fIronSightZoomFactor;
};

void GetZoomData(const float scope_factor, float& delta, float& min_zoom_factor);

float LastZoomFactor = 0.f;

void CWeapon::OnZoomIn()
{
	m_bSwitchSprint = false;
	m_zoom_params.m_bIsZoomModeNow		= true;
	if (m_zoom_params.m_bUseDynamicZoom && IsScopeAttached())
	{
		if (LastZoomFactor)
		{
			m_fRTZoomFactor = LastZoomFactor;
		}
		else
		{
			m_fRTZoomFactor = CurrentZoomFactor();
		}

		float delta, min_zoom_factor;
		GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);
		clamp(m_fRTZoomFactor, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
		SetZoomFactor(m_fRTZoomFactor);
	}
	else if (CurrentZoomFactor() != 0)
	{
		m_zoom_params.m_fCurrentZoomFactor = CurrentZoomFactor();
	}

	UpdateZoomCrosshairUI();

	GamePersistent().SetPickableEffectorDOF(true);

	if (m_zoom_params.m_sUseBinocularVision.size() && IsScopeAttached() && nullptr == m_zoom_params.m_pVision)
	{
		m_zoom_params.m_pVision = new CBinocularsVision(m_zoom_params.m_sUseBinocularVision);
	}

	if (m_zoom_params.m_sUseZoomPostprocess.size() && IsScopeAttached()) 
	{
		CActor* actor = H_Parent() ? H_Parent()->cast_actor() : nullptr;

		if (actor && !GetNightVision())
		{
			m_zoom_params.m_pNight_vision = new CWeaponNightVision(m_zoom_params.m_sUseZoomPostprocess, actor);
		}
	}
}

void CWeapon::OnZoomOut()
{
	m_zoom_params.m_bIsZoomModeNow		= false;
	m_fRTZoomFactor = GetZoomFactor();//store current
	m_zoom_params.m_fCurrentZoomFactor = g_fov;

	GamePersistent().SetPickableEffectorDOF(false);

	ResetSubStateTime					();

	xr_delete(m_zoom_params.m_pVision);

	if (GetNightVision())
	{
		GetNightVision()->SwitchNightVision(false);
		xr_delete(m_zoom_params.m_pNight_vision);
	}
}

CUIWindow* CWeapon::ZoomTexture()
{
	return UseScopeTexture() ? m_UIScope : nullptr;
}

bool CWeapon::UseScopeTexture()
{
	return !g_3d_scopes;
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

	bUseAltScope = !!bReloadSectionScope(section);

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

bool CWeapon::can_kill() const
{
	if (GetSuitableAmmoTotal(true) || m_ammoTypes.empty())
	{
		return true;
	}

	return false;
}

CInventoryItem* CWeapon::can_kill(CInventory* inventory) const
{
	if ((GetAmmoChamberElapsed() + GetAmmoElapsed()) > 0 || m_ammoTypes.empty())
	{
		return const_cast<CWeapon*>(this);
	}

	for (const PIItem inventory_item : inventory->m_all)
	{
		xr_vector<shared_str>::const_iterator i = std::find(m_ammoTypes.begin(), m_ammoTypes.end(), inventory_item->object().cNameSect());
		if (i != m_ammoTypes.end())
		{
			return inventory_item;
		}
	}

	return 0;
}

const CInventoryItem* CWeapon::can_kill(const xr_vector<const CGameObject*>& items) const
{
	if (m_ammoTypes.empty())
	{
		return this;
	}

	for (const CGameObject* game_obj : items)
	{
		const CInventoryItem* inventory_item = smart_cast<const CInventoryItem*>(game_obj);

		if (inventory_item == nullptr)
		{
			continue;
		}

		xr_vector<shared_str>::const_iterator i = std::find(m_ammoTypes.begin(), m_ammoTypes.end(), inventory_item->object().cNameSect());
		if (i != m_ammoTypes.end())
		{
			return inventory_item;
		}
	}

	return 0;
}

bool CWeapon::ready_to_kill() const
{
	return (!IsMisfire() && ((GetState() == eIdle) || (GetState() == eFire) || (GetState() == eFire2)) && (GetAmmoElapsed() + GetAmmoChamberElapsed()) > 0);
}

u8 CWeapon::GetCurrentHudOffsetIdx() const
{
	const CActor* pActor = smart_cast<const CActor*>(H_Parent());
	if (!pActor)
	{
		return 0;
	}

	bool b_aiming = ((IsZoomed() && m_zoom_params.m_fZoomRotationFactor <= 1.f) || (!IsZoomed() && m_zoom_params.m_fZoomRotationFactor > 0.f));

	if (!b_aiming)
	{
		return 0;
	}
	else if (IsGrenadeMode())
	{
		return 2;
	}
	else
	{
		return 1;
	}
}

void CWeapon::UpdateHudAdditonal(Fmatrix& trans)
{
	CActor* pActor = H_Parent() != nullptr ? H_Parent()->cast_actor() : nullptr;
	if (pActor == nullptr)
	{
		return;
	}

	u8 idx = GetCurrentHudOffsetIdx();

	attachable_hud_item* hi = HudItemData();
	if (hi == nullptr)
	{
		return;
	}

	Fvector curr_offs, curr_rot;
	curr_offs = hi->m_measures.m_hands_positions.hands_offsets[0][idx];//pos,aim
	curr_rot = hi->m_measures.m_hands_positions.hands_offsets[1][idx];//rot,aim
	curr_offs.mul(m_zoom_params.m_fZoomRotationFactor);
	curr_rot.mul(m_zoom_params.m_fZoomRotationFactor);

	Fmatrix	hud_rotation;
	hud_rotation.identity();
	hud_rotation.rotateX(curr_rot.x);

	Fmatrix	hud_rotation_y;
	hud_rotation_y.identity();
	hud_rotation_y.rotateY(curr_rot.y);
	hud_rotation.mulA_43(hud_rotation_y);

	hud_rotation_y.identity();
	hud_rotation_y.rotateZ(curr_rot.z);
	hud_rotation.mulA_43(hud_rotation_y);

	hud_rotation.translate_over(curr_offs);
	trans.mulB_43(hud_rotation);

	if (pActor->IsZoomAimingMode())
	{
		m_zoom_params.m_fZoomRotationFactor += Device.fTimeDelta / m_zoom_params.m_fZoomRotateTime;
	}
	else
	{
		m_zoom_params.m_fZoomRotationFactor -= Device.fTimeDelta / m_zoom_params.m_fZoomRotateTime;
	}

	clamp(m_zoom_params.m_fZoomRotationFactor, 0.f, 1.f);
}

void CWeapon::SetAmmoElapsed(int ammo_count)
{
	iAmmoElapsed = ammo_count;

	u32 uAmmo = u32(iAmmoElapsed);

	if (uAmmo != m_magazine.size())
	{
		if (uAmmo > m_magazine.size())
		{
			CCartridge l_cartridge;
			l_cartridge.Load(m_ammoTypes[m_ammoType].c_str(), m_ammoType);

			while (uAmmo > m_magazine.size())
			{
				m_magazine.push_back(l_cartridge);
			}
		}
		else
		{
			while (uAmmo < m_magazine.size())
			{
				m_magazine.pop_back();
			}
		}
	}
}

void CWeapon::SetChamberAmmoElapsed(int ammo_count)
{
	iAmmoChamberElapsed = ammo_count;

	u32 uAmmo = u32(iAmmoChamberElapsed);

	if (uAmmo != m_chamber.size())
	{
		if (uAmmo > m_chamber.size())
		{
			CCartridge l_cartridge;
			l_cartridge.Load(m_ammoTypes[m_ChamberAmmoType].c_str(), m_ChamberAmmoType);

			while (uAmmo > m_chamber.size())
			{
				m_chamber.push_back(l_cartridge);
			}
		}
		else
		{
			while (uAmmo < m_chamber.size())
			{
				m_chamber.pop_back();
			}
		}
	}
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
	bool b_is_active_item = (m_pInventory && m_pInventory->ActiveItem()==this);
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

	if (EngineExternal().CallOfPripyatMode())
	{
		return ((GameID() == eGameIDDeathmatch) &&
			m_DefaultCartridge.m_flags.test(CCartridge::cfCanBeUnlimited));
	}
	else
	{
		return ((GameID() != eGameIDArtefactHunt) &&
			(GameID() != eGameIDCaptureTheArtefact) &&
			m_DefaultCartridge.m_flags.test(CCartridge::cfCanBeUnlimited));
	}
			
}
bool CWeapon::infinite_fire()
{
	if (IsGameTypeSingle())
	{
		if (m_pInventory)
		{
			return inventory_owner().infinite_fire();
		}
	}

	return false;
}
;

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

	if (iAmmoChamberElapsed)
	{
		float w = pSettings->r_float(*getAmmoTypes()[m_ChamberAmmoType], "inv_weight");
		float bs = pSettings->r_float(*getAmmoTypes()[m_ChamberAmmoType], "box_size");

		res += w * (iAmmoChamberElapsed / bs);
	}

	return res;
}

extern bool hud_adj_crosshair;
bool CWeapon::show_crosshair()
{
	return (!IsPending() || GetState() == eEmptyClick || GetState() == eSprintStart || GetState() == eSprintEnd) && ((!IsZoomed() || !ZoomHideCrosshair()) || hud_adj_crosshair);
}

bool CWeapon::show_indicators()
{
	if (!IsGrenadeMode() && IsZoomed())
	{
		if (bUseAltScope && bScopeIsHasTexture && IsScopeAttached() && (ZoomTexture() != nullptr || g_3d_scopes))
		{
			return false;
		}

		if (IsUIForceHiding() && !IsUIForceUnhiding())
		{
			if (g_3d_scopes)
			{
				if (IsLensedScopeInstalled())
				{
					return false;
				}
			}

			if (ZoomTexture() != nullptr)
			{
				return false;
			}
		}
	}

	return true;

	//return !(IsZoomed() && ZoomTexture() && IsUIForceHiding() && !IsUIForceUnhiding());
}

float CWeapon::GetConditionToShow	() const
{
	return	(GetCondition());//powf(GetCondition(),4.0f));
}

BOOL CWeapon::ParentMayHaveAimBullet	()
{
	return H_Parent() && H_Parent()->cast_actor();
}

BOOL CWeapon::ParentIsActor	()
{
	return H_Parent() && H_Parent()->cast_actor();
}

void CWeapon::debug_draw_firedeps()
{
#ifdef DEBUG_DRAW
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

	if (S == eBore)
	{
		u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();
		UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, type_to_update);
	}

	if(EnableDof && GetState()==eReload)
	{
		if(H_Parent()==Level().CurrentEntity() && !fsimilar(m_zoom_params.m_ReloadDof.w,-1.0f))
		{
			CActor* current_actor	= H_Parent() ? H_Parent()->cast_actor() : NULL;
			if (current_actor)
				current_actor->Cameras().AddCamEffector(new CEffectorDOF(m_zoom_params.m_ReloadDof));
		}
	}
}

void CWeapon::OnAnimationEnd(u32 state) 
{
	inherited::OnAnimationEnd(state);
}

void CWeapon::SetSilencerX(int value)
{
	m_iSilencerX = value;
}

void CWeapon::SetSilencerY(int value)
{
	m_iSilencerY = value;
}

bool CWeapon::NeedBlockSprint() const
{
	u32 state = GetState();
	const static bool isDelayedWeaponActions = EngineExternal()[EEngineExternalGame::EnableDelayedWeaponActions];

	if (isDelayedWeaponActions)
	{
		return state != eIdle && state != eSprintStart || m_bIsAimAnimationPlaying;
	}

	const static bool isBlockSprintInReload = EngineExternal()[EEngineExternalGame::EnableBlockSprintInReload];

	return state == eFire || state == eFire2 || state == eSprintEnd || isBlockSprintInReload && state == eReload || m_bIsAimAnimationPlaying;
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
	float dt = m_lens_zoom_params.delta;
	float oldpos = m_lens_zoom_params.target_position;
	bool force_zoom_sound = false;

	m_lens_zoom_params.target_position += dt;

	SetLensParams(m_lens_zoom_params);

	if ((m_lens_zoom_params.target_position != oldpos) && (m_lens_zoom_params.force_zoom_sound || (m_lens_zoom_params.factor_min != m_lens_zoom_params.factor_max)) && (abs(oldpos - m_lens_zoom_params.target_position) > 0.0001f))
	{
		PlaySound("sndScopeZoomPlus", get_LastFP());
	}

	if (!IsScopeAttached())
	{
		return;
	}

	if (!m_zoom_params.m_bUseDynamicZoom)
	{
		return;
	}

	float delta, min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);

	float f = GetZoomFactor() - delta;
	clamp(f, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
	SetZoomFactor(f);

	LastZoomFactor = f;
}

void CWeapon::ZoomDec()
{
	float dt = m_lens_zoom_params.delta;
	float oldpos = m_lens_zoom_params.target_position;
	bool force_zoom_sound = false;

	m_lens_zoom_params.target_position -= dt;

	SetLensParams(m_lens_zoom_params);

	if ((m_lens_zoom_params.target_position != oldpos) && (m_lens_zoom_params.force_zoom_sound || (m_lens_zoom_params.factor_min != m_lens_zoom_params.factor_max)) && (abs(oldpos - m_lens_zoom_params.target_position) > 0.0001f))
	{
		PlaySound("sndScopeZoomMinus", get_LastFP());
	}


	if (!IsScopeAttached())
	{
		return;
	}

	if (!m_zoom_params.m_bUseDynamicZoom)
	{
		return;
	}

	float delta, min_zoom_factor;
	GetZoomData(m_zoom_params.m_fScopeZoomFactor, delta, min_zoom_factor);

	float f = GetZoomFactor() + delta;
	clamp(f, m_zoom_params.m_fScopeZoomFactor, min_zoom_factor);
	SetZoomFactor(f);
	
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

	if (iAmmoChamberElapsed)
	{
		float w = pSettings->r_float(getAmmoTypes()[m_ChamberAmmoType].c_str(), "cost");
		float bs = pSettings->r_float(getAmmoTypes()[m_ChamberAmmoType].c_str(), "box_size");

		res += iFloor(w * (iAmmoChamberElapsed / bs));
	}

	return res;
}

float CWeapon::GetHudFov()
{
	float get = inherited::GetHudFov() / m_fHudFovFactor;
	float zoom = m_HudFovZoom ? m_HudFovZoom : (get * Device.fFOV / g_fov);
	get += (zoom - get) * m_zoom_params.m_fZoomRotationFactor;

	float hud_fov = m_fHudFovFactor;
	float zoom_fov = IsGrenadeMode() ? m_fHudFovGLZoomFactor : m_fHudFovZoomFactor;

	if (((IsZoomed() && m_zoom_params.m_fZoomRotationFactor <= 1.f) || (!IsZoomed() && m_zoom_params.m_fZoomRotationFactor > 0.f)))
	{
		hud_fov = hud_fov - (hud_fov - zoom_fov) * m_zoom_params.m_fZoomRotationFactor;
	}

	return get * hud_fov;
}

const CameraRecoil& CWeapon::getCameraRecoil(void) const
{
	return cam_recoil;
}

const CameraRecoil& CWeapon::getCameraZoomRecoil(void) const
{
	return zoom_cam_recoil;
}

bool CWeapon::IsUIForceHiding()
{
	CWeaponBinoculars* bino = cast_weapon_binoculars();

	if (bino && IsZoomed())
	{
		return READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "zoom_hide_ui", true);
	}
	else if (get_ScopeStatus() == 1 && IsZoomed())
	{
		return READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "zoom_hide_ui", true);
	}
	else if (get_ScopeStatus() == 2 && IsScopeAttached() && IsZoomed())
	{
		return READ_IF_EXISTS(pSettings, r_bool, GetScopeName(), "zoom_hide_ui", true);
	}

	return false;
}

bool CWeapon::IsCollimatorInstalled() const
{
	if (!IsScopeAttached() || get_ScopeStatus() != 2)
	{
		return false;
	}

	return READ_IF_EXISTS(pSettings, r_bool, GetScopeName(), "collimator", false);
}

bool CWeapon::IsHudModelForceUnhide() const
{
	return IsCollimatorInstalled() || IsLensedScopeInstalled() && g_3d_scopes /*|| IsAlterZoomMode() */;
}

bool CWeapon::IsUIForceUnhiding() const
{
	bool result = IsHudModelForceUnhide();

	if (result)
	{
		/*if (buf.IsAlterZoomMode())
			result = true;
		else */if (get_ScopeStatus() == 1)
		{
			result = !READ_IF_EXISTS(pSettings, r_bool, cNameSect(), "zoom_hide_ui", false);
		}
		else if (get_ScopeStatus() == 2 && IsScopeAttached())
		{
			result = !READ_IF_EXISTS(pSettings, r_bool, GetScopeName(), "zoom_hide_ui", false);
		}
	}

	return result;
}

int CWeapon::GetScopeX()
{
	const static int useHQ = EngineExternal()[EEngineExternalUI::HQIcons];

	if (bUseAltScope)
	{
		if (m_eScopeStatus != ALife::eAddonPermanent && IsScopeAttached())
		{
			return pSettings->r_s32(GetNameWithAttachmentScope(), "scope_x") * (1 + useHQ);
		}
		else
		{
			return 0;
		}
	}

	return pSettings->r_s32(m_scopes[m_cur_scope], "scope_x") * (1 + useHQ);
}

int CWeapon::GetScopeY()
{
	const static int useHQ = EngineExternal()[EEngineExternalUI::HQIcons];
	if (bUseAltScope)
	{
		if (m_eScopeStatus != ALife::eAddonPermanent && IsScopeAttached())
		{
			return pSettings->r_s32(GetNameWithAttachmentScope(), "scope_y") * (1 + useHQ);
		}
		else
		{
			return 0;
		}
	}

	return pSettings->r_s32(m_scopes[m_cur_scope], "scope_y") * (1 + useHQ);
}


const shared_str CWeapon::GetScopeName() const
{
	if (bUseAltScope)
	{
		return m_scopes[m_cur_scope];
	}
	else
	{
		return pSettings->r_string(m_scopes[m_cur_scope], "scope_name");
	}
}

void CWeapon::UpdateAltScope()
{
	if (m_eScopeStatus != ALife::eAddonAttachable || !bUseAltScope)
		return;

	shared_str sectionNeedLoad;

	sectionNeedLoad = IsScopeAttached() ? GetNameWithAttachmentScope() : m_section_id;

	if (!pSettings->section_exist(sectionNeedLoad))
		return;

	shared_str vis = pSettings->r_string(sectionNeedLoad, "visual");

	if (vis != cNameVisual())
	{
		cNameVisual_set(vis);
	}

	shared_str new_hud = pSettings->r_string(sectionNeedLoad, "hud");
	if (new_hud != hud_sect)
	{
		hud_sect = new_hud;
	}

	hud_sect_cache = hud_sect;
}

shared_str CWeapon::GetNameWithAttachmentScope()
{
	string64 str;
	if (pSettings->line_exist(m_section_id.c_str(), "parent_section"))
	{
		shared_str parent = pSettings->r_string(m_section_id.c_str(), "parent_section");
		xr_sprintf(str, "%s_%s", parent.c_str(), GetScopeName().c_str());
	}
	else
	{
		xr_sprintf(str, "%s_%s", m_section_id.c_str(), GetScopeName().c_str());
	}
	return (shared_str)str;
}

bool CWeapon::bReloadSectionScope(LPCSTR section)
{
	if (!pSettings->line_exist(section, "scopes"))
		return false;

	if (pSettings->r_string(section, "scopes") == NULL)
		return false;

	if (xr_strcmp(pSettings->r_string(section, "scopes"), "none") == 0)
		return false;

	return true;
}

bool CWeapon::bLoadAltScopesParams(LPCSTR section)
{
	if (!pSettings->line_exist(section, "scopes"))
		return false;

	if (pSettings->r_string(section, "scopes") == NULL)
		return false;

	if (xr_strcmp(pSettings->r_string(section, "scopes"), "none") == 0)
		return false;

	if (m_eScopeStatus == ALife::eAddonAttachable)
	{
		LPCSTR str = pSettings->r_string(section, "scopes");
		for (int i = 0, count = _GetItemCount(str); i < count; ++i)
		{
			string128 scope_section;
			_GetItem(str, i, scope_section);
			m_scopes.push_back(scope_section);
		}
	}
	else if (m_eScopeStatus == ALife::eAddonPermanent)
	{
		LoadCurrentScopeParams(section);
	}

	return true;
}

void CWeapon::LoadOriginalScopesParams(LPCSTR section)
{

	if (m_eScopeStatus == ALife::eAddonAttachable)
	{
		if (pSettings->line_exist(section, "scopes_sect"))
		{
			LPCSTR str = pSettings->r_string(section, "scopes_sect");
			for (int i = 0, count = _GetItemCount(str); i < count; ++i)
			{
				string128						scope_section;
				_GetItem(str, i, scope_section);
				m_scopes.push_back(scope_section);
			}
		}
		else
		{
			m_scopes.push_back(section);
		}
	}
	else if (m_eScopeStatus == ALife::eAddonPermanent)
	{
		LoadCurrentScopeParams(section);
	}
}

void createWpnScopeXML()
{
	if (!pWpnScopeXml)
	{
		pWpnScopeXml = new CUIXml();
		pWpnScopeXml->Load(CONFIG_PATH, UI_PATH, "scopes.xml");
	}
}

void CWeapon::LoadCurrentScopeParams(LPCSTR section)
{
	shared_str scope_tex_name = "none";
	bScopeIsHasTexture = false;
	if (pSettings->line_exist(section, "scope_texture"))
	{
		scope_tex_name = pSettings->r_string(section, "scope_texture");
		if (xr_strcmp(scope_tex_name, "none") != 0)
			bScopeIsHasTexture = true;
	}

	m_zoom_params.m_fScopeZoomFactor = pSettings->r_float(section, "scope_zoom_factor");

	if (bScopeIsHasTexture)
	{
		m_zoom_params.m_sUseZoomPostprocess = READ_IF_EXISTS(pSettings, r_string, section, "scope_nightvision", 0);
		m_zoom_params.m_bUseDynamicZoom = READ_IF_EXISTS(pSettings, r_bool, section, "scope_dynamic_zoom", FALSE);

		m_zoom_params.m_sUseBinocularVision = READ_IF_EXISTS(pSettings, r_string, section, "scope_alive_detector", 0);
	}

	if (m_UIScope)
	{
		xr_delete(m_UIScope);
	}

	if (!g_dedicated_server)
	{
		if (bScopeIsHasTexture)
		{
			m_UIScope = new CUIWindow();
			createWpnScopeXML();
			CUIXmlInit::InitWindow(*pWpnScopeXml, scope_tex_name.c_str(), 0, m_UIScope);
		}
	}
}

void CWeapon::GiveAmmoFromMagToChamber()
{
	if (!m_bAmmoInChamber)
		return;

	if (IsGrenadeMode())
		return;

	if (m_magazine.empty())
		return;

	if (!m_chamber.empty())
		return;

	CCartridge FirstBulletInMag;
	while (iAmmoChamberElapsed < iChamberSize)
	{
		FirstBulletInMag = m_magazine.back();
		m_ChamberAmmoType = m_ammoType;
		m_DefaultCartridgeInChamber = FirstBulletInMag;
		m_magazine.pop_back();
		--iAmmoElapsed;

		m_chamber.push_back(FirstBulletInMag);
		++iAmmoChamberElapsed;
	}
}

void CWeapon::DeleteAmmoInChamber()
{
	if (!m_bAmmoInChamber)
		return;

	if (m_chamber.empty())
		return;

	--iAmmoChamberElapsed;
	m_chamber.pop_back();
}

void CWeapon::UnloadChamber(bool spawn_ammo)
{
	xr_map<LPCSTR, u16> l_ammo;

	while (!m_chamber.empty())
	{
		CCartridge& l_cartridge = m_chamber.back();
		xr_map<LPCSTR, u16>::iterator l_it;
		for (l_it = l_ammo.begin(); l_ammo.end() != l_it; ++l_it)
		{
			if (!xr_strcmp(*l_cartridge.m_ammoSect, l_it->first))
			{
				++(l_it->second);
				break;
			}
		}

		if (l_it == l_ammo.end()) l_ammo[*l_cartridge.m_ammoSect] = 1;
		m_chamber.pop_back();
		--iAmmoChamberElapsed;
	}

	//VERIFY((u32)iAmmoInChamberElapsed == m_chamber.size());

	if (ParentIsActor())
	{
		int	AC = GetSuitableAmmoTotal();
		Actor()->callback(GameObject::eOnWeaponMagazineEmpty)(lua_game_object(), AC);
	}

	if (!spawn_ammo)
		return;

	xr_map<LPCSTR, u16>::iterator l_it;
	for (l_it = l_ammo.begin(); l_ammo.end() != l_it; ++l_it)
	{
		if (m_pInventory)
		{
			PIItem get_any = m_pInventory->GetAny(l_it->first);
			CWeaponAmmo* l_pA = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;
			if (l_pA)
			{
				u16 l_free = l_pA->m_boxSize - l_pA->m_boxCurr;
				l_pA->m_boxCurr = l_pA->m_boxCurr + (l_free < l_it->second ? l_free : l_it->second);
				l_it->second = l_it->second - (l_free < l_it->second ? l_free : l_it->second);
			}
		}
		if (l_it->second && !unlimited_ammo()) SpawnAmmo(l_it->second, l_it->first);
	}

	if (GetState() == eIdle)
	{
		SwitchState(eIdle);
	}

	if (!IsGrenadeMode() && m_bUseChamberInUpdateBones && m_bAmmoInChamber)
	{
		UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, m_ammoType);
	}
}

bool CWeapon::GetScopeBack()
{
	if (bUseAltScope && m_eScopeStatus != ALife::eAddonPermanent && IsScopeAttached())
		return !!READ_IF_EXISTS(pSettings, r_bool, GetNameWithAttachmentScope(), "scope_back", false);

	return !!READ_IF_EXISTS(pSettings, r_bool, GetCurrentScopeSection(), "scope_back", false);
}


void CWeapon::UpdateCollimatorSight()
{
	if (!ParentIsActor())
		return;

	if (HudItemData() == nullptr)
		return;

	if (m_sCollimatorSightsBones.empty())
		return;

	conditional_breaking_params bp = CollimatorBreakingParams;
	float current_problems_cnt = Actor()->CurrentElectronicsProblemsCnt();

	if (/*GetAimFactor() > 0.0f && (IsLastZoomAlter() || GetAlterZoomDirectSwitchMixupFactor() > EPS) && m_bHideColimSightInAlter) || */ GetCondition() < bp.end_condition)
	{
		for (auto& bone : m_sCollimatorSightsBones)
		{
			HudItemData()->set_bone_visible(bone, false, TRUE);
		}
	}
	else if (GetCondition() < bp.start_condition || current_problems_cnt > 0.0f)
	{
		float probability = 0.0f;
		float probability2 = 0.0f;

		if (bp.start_condition == bp.end_condition)
			probability = bp.end_condition;
		else
			probability = bp.start_probability + (bp.start_condition - GetCondition()) * (1.0f - bp.start_probability) / (bp.start_condition - bp.end_condition);

		const int collimProblemsCnt = m_fCollimatorLevelsProblem;
		if (current_problems_cnt > 0 && collimProblemsCnt > 0.0f)
		{
			if (current_problems_cnt >= collimProblemsCnt)
				probability = 1.0f;
			else
			{
				probability2 = current_problems_cnt / collimProblemsCnt;
				probability = std::max(probability2, probability);
			}
		}

		for (auto& bone : m_sCollimatorSightsBones)
		{
			HudItemData()->set_bone_visible(bone, !(::Random.randF(0.0f, 1.0f) < probability), TRUE);
		}
	}
	else for (auto& bone : m_sCollimatorSightsBones)
	{
		HudItemData()->set_bone_visible(bone, true, TRUE);
	}
}

u32 CWeapon::FakeReload()
{
	if (unlimited_ammo())
	{
		return GetMagCapacity();
	}

	u32 in_box = GetAmmoCount(GetTargetAmmoType(IsGrenadeMode()));
	return clampr(in_box, (u32)0, (u32)iMagazineSize);
}

void CWeapon::OnMotionMark(u32 state, const motion_marks& mark)
{
	inherited::OnMotionMark(state, mark);

	if (state == eEmptyClick && mark.name == "Left")
	{
		m_bBlockEmptyClick = false;
	}

	if (state == eReload && mark.name == "Left")
	{
		u32 current_configuration = FakeReload();
		bool for_grenade = IsGrenadeMode();
		UpdateAmmoBones(for_grenade ? m_ammo_bones_gl : m_ammo_bones_mag, current_configuration, GetTargetAmmoType(for_grenade));
	}
}

void CWeapon::UpdateAmmoBones(xr_vector<SAmmoBonesParams*>& lVector, u32 idx, u8 type)
{
	if (lVector.empty())
	{
		return;
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, BOOL status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, FALSE);
			}
		}
	};

	if (m_bUseChamberInUpdateBones)
	{
		idx += iAmmoChamberElapsed;
	}

	attachable_hud_item* HID = HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = Visual() != nullptr ? PKinematics(Visual()) : nullptr;

	for (const auto& bone_param : lVector)
	{
		for (const auto& bone_name : bone_param->AllBones)
		{
			SetVisible(hud_kin, bone_name, FALSE);
			SetVisible(world_kin, bone_name, FALSE);
		}
	}

	for (const auto& bone_param : lVector)
	{
		if (bone_param->AmmoType == type || bone_param->AmmoType == undefined_ammo_type)
		{
			auto& Node = bone_param->ConfigurationMap[idx];
			for (const auto& configuration_bone : Node.second)
			{
				SetVisible(hud_kin, configuration_bone, TRUE);
				SetVisible(world_kin, configuration_bone, TRUE);
			}
		}
	}

	if (world_kin != nullptr)
	{
		world_kin->CalculateBones_Invalidate();
		world_kin->CalculateBones(TRUE);
	}

	if (hud_kin != nullptr)
	{
		hud_kin->CalculateBones_Invalidate();
		hud_kin->CalculateBones(TRUE);
	}
}

void CWeapon::UpdateShellBones(u32 idx, u8 type)
{
	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, BOOL status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, FALSE);
			}
		}
	};

	attachable_hud_item* HID = HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = Visual() != nullptr ? PKinematics(Visual()) : nullptr;

	for (const auto& bone_param : m_shell_bones)
	{
		for (const auto& bone_name : bone_param->AllBones)
		{
			SetVisible(hud_kin, bone_name, FALSE);
			SetVisible(world_kin, bone_name, FALSE);
		}
	}

	for (const auto& bone_param : m_shell_bones)
	{
		if (bone_param->AmmoType == type || bone_param->AmmoType == undefined_ammo_type)
		{
			auto& Node = bone_param->ConfigurationMap[idx];
			if (Node.second.empty())
			{
				for (const auto& bone_name : bone_param->AllBones)
				{
					SetVisible(hud_kin, bone_name, TRUE);
					SetVisible(world_kin, bone_name, TRUE);
				}
			}
			else
			{
				for (const auto& configuration_bone : Node.second)
				{
					SetVisible(hud_kin, configuration_bone, TRUE);
					SetVisible(world_kin, configuration_bone, TRUE);
				}
			}
		}
	}

	if (world_kin != nullptr)
	{
		world_kin->CalculateBones_Invalidate();
		world_kin->CalculateBones(TRUE);
	}

	if (hud_kin != nullptr)
	{
		hud_kin->CalculateBones_Invalidate();
		hud_kin->CalculateBones(TRUE);
	}
}

bool CWeapon::ScopeFit(CScope* pIItem) const
{
	for (const shared_str& scope : m_scopes)
	{
		if (bUseAltScope)
		{
			if (scope == pIItem->cNameSect())
			{
				return true;
			}
		}
		else if (pSettings->r_string(scope, "scope_name") == pIItem->cNameSect())
		{
			return true;
		}
	}

	return false;
}

int CWeapon::GetMagCapacity()
{
	int size = iMagazineSize;

	if (!IsGrenadeMode())
	{
		for (auto& ammo_pair : m_mags_capacity)
		{
			if (ammo_pair.first == GetTargetAmmoType())
			{
				size = ammo_pair.second;
			}
		}
	}

	return size;
}

bool CWeapon::IsActionProcessing() const
{
	const static bool is_suicide = false;
	return is_suicide;
}

float CWeapon::GetLensFOV() const
{
	float factor = m_lens_zoom_params.factor_min + (m_lens_zoom_params.factor_max - m_lens_zoom_params.factor_min) * m_lens_zoom_params.real_position;

	float fov = (g_fov / 2.0f) * PI / 180.0f;
	float result = 2.0f * atan(tan(fov) / factor) * 180.0f / PI;

	return result;
}

void CWeapon::LoadNightBrightnessParamsFromSection(shared_str sect)
{
	stepped_params last = m_lens_night_brightness;

	m_lens_night_brightness.max_value = READ_IF_EXISTS(pSettings, r_float, sect, "max_night_brightness", 1.0f) / 3.0f;
	m_lens_night_brightness.min_value = READ_IF_EXISTS(pSettings, r_float, sect, "min_night_brightness", 1.0f) / 3.0f;
	m_lens_night_brightness.steps = READ_IF_EXISTS(pSettings, r_u32, sect, "steps_brightness", 0);
	m_lens_night_brightness.jitter = READ_IF_EXISTS(pSettings, r_float, sect, "jitter_brightness", 1.0f);
	m_lens_night_brightness.min_factor = READ_IF_EXISTS(pSettings, r_float, sect, "scope_nightvision_min_factor", 0.0f);

	bool b_r2 = !!psDeviceFlags.test(rsR2);
	b_r2 |= !!psDeviceFlags.test(rsR4);

	if (!b_r2 && m_lens_night_brightness.max_value > 1.0f)
	{
		m_lens_night_brightness.max_value = 1.0f;
	}

	if (abs(m_lens_night_brightness.max_value - last.max_value) > EPS || fabs(m_lens_night_brightness.min_value - last.min_value) > EPS || m_lens_night_brightness.steps != last.steps)
	{
		if (m_lens_night_brightness.lens_night_brightness_saved_step >= 0)
		{
			m_lens_night_brightness.cur_step = m_lens_night_brightness.lens_night_brightness_saved_step;
			m_lens_night_brightness.lens_night_brightness_saved_step = -1;
		}
		else
		{
			m_lens_night_brightness.cur_step = READ_IF_EXISTS(pSettings, r_u32, sect, "default_brightness_step", m_lens_night_brightness.steps);
		}

		SetNightBrightness(m_lens_night_brightness.cur_step, false);
	}
}

void CWeapon::ChangeNightBrightness(int steps)
{
	if (m_lens_night_brightness.steps == 0)
	{
		m_lens_night_brightness.cur_value = m_lens_night_brightness.min_value;
		return;
	}

	SetNightBrightness(m_lens_night_brightness.cur_step + steps, true);
}

void CWeapon::SetNightBrightness(int steps, bool use_sound)
{
	int last_steps = m_lens_night_brightness.cur_step;

	m_lens_night_brightness.cur_step = steps;
	if (m_lens_night_brightness.cur_step <= 0)
	{
		m_lens_night_brightness.cur_step = 0;
		m_lens_night_brightness.cur_value = m_lens_night_brightness.min_value;
	}
	else if (m_lens_night_brightness.cur_step >= m_lens_night_brightness.steps)
	{
		m_lens_night_brightness.cur_step = m_lens_night_brightness.steps;
		m_lens_night_brightness.cur_value = m_lens_night_brightness.max_value;
	}
	else
	{
		float delta = (m_lens_night_brightness.max_value - m_lens_night_brightness.min_value) / m_lens_night_brightness.steps;
		m_lens_night_brightness.cur_value = m_lens_night_brightness.min_value + delta * m_lens_night_brightness.cur_step;
	}

	if (use_sound)
	{
		if (last_steps > m_lens_night_brightness.cur_step)
		{
			PlaySound("sndScopeBrightnessMinus", get_LastFP());
		}
		else if (last_steps < m_lens_night_brightness.cur_step)
		{
			PlaySound("sndScopeBrightnessPlus", get_LastFP());
		}
	}

	if (last_steps != m_lens_night_brightness.cur_step)
	{
		UpdateZoomCrosshairUI();
	}
}

void CWeapon::UpdateZoomCrosshairUI()
{
	if (m_UIScope != nullptr)
	{
		if (m_UIScope->WindowName() == "switchable_zoom_wnd")
		{
			for (int i = 0; i <= m_lens_night_brightness.steps; ++i)
			{
				static shared_str child_name;
				child_name.printf("auto_static_%d", i);
				if (CUIWindow* child = m_UIScope->FindChild(child_name))
				{
					child->Show(i == m_lens_night_brightness.cur_step);
				}
			}
		}
	}
}

void CWeapon::SetLensParams(lens_zoom_params& params)
{
	if (params.factor_max < params.factor_min)
	{
		float t = params.factor_min;
		params.factor_min = params.factor_max;
		params.factor_max = t;
	}

	if (params.target_position < 0.0f)
	{
		params.target_position = 0.0f;
	}
	else if (params.target_position > 1.0f)
	{
		params.target_position = 1.0f;
	}

	if (params.real_position < 0.0f)
	{
		params.real_position = 0.0f;
	}
	else if (params.real_position > 1.0f)
	{
		params.real_position = 1.0f;
	}

	m_lens_zoom_params = params;
}

void CWeapon::UpdateLensFactor(u32 timedelta)
{
	lens_zoom_params lens_params_tmp = m_lens_zoom_params;
	lens_zoom_params lens_params_final = lens_params_tmp;

	float dt_needed = lens_params_tmp.target_position - lens_params_tmp.real_position;

	if (lens_params_tmp.speed < EPS)
	{
		lens_params_final.real_position = lens_params_tmp.target_position;
		SetLensParams(lens_params_final);
	}
	else if (abs(dt_needed) > EPS)
	{
		if (lens_params_tmp.gyro_period > EPS)
		{
			float zoom_remains = abs(dt_needed) / lens_params_tmp.speed;
			float snd_remains = lens_params_tmp.gyro_period - Device.GetTimeDeltaSafe(lens_params_tmp.last_gyro_snd_time) / 1000.0f;

			if (snd_remains > zoom_remains && snd_remains > 0.0f)
			{
				lens_params_tmp.speed = abs(dt_needed) / snd_remains;
			}
		}

		float dt = timedelta * lens_params_tmp.speed / 1000.0f;

		if (dt < abs(dt_needed))
		{
			if (lens_params_tmp.gyro_period > EPS)
			{
				if (Device.GetTimeDeltaSafe(lens_params_tmp.last_gyro_snd_time) / 1000.0f > lens_params_tmp.gyro_period)
				{
					PlaySound("sndScopeZoomGyro", get_LastFP());
					lens_params_final.last_gyro_snd_time = Device.dwTimeGlobal;
				}
			}
			lens_params_final.real_position += copysign(dt, dt_needed);
		}
		else
		{
			lens_params_final.real_position = lens_params_tmp.target_position;
		}

		SetLensParams(lens_params_final);
	}
}

float CWeapon::GetNightPPEFactor()
{
	float val = -1.0f;
	float min_factor = m_lens_night_brightness.min_factor;

	if (min_factor < 0.0f)
	{
		min_factor = 0.0f;
	}

	if (min_factor > 1.0f)
	{
		min_factor = 1.0f;
	}

	float brightness = (m_lens_night_brightness.steps > 0) ? m_lens_night_brightness.cur_step / static_cast<float>(m_lens_night_brightness.steps) : 1.0f;

	val = min_factor + (1.0f - min_factor) * brightness;

	return val;
}