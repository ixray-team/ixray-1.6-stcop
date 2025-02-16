#include "stdafx.h"
#include "actor.h"
#include "weapon.h"
#include "mercuryball.h"
#include "inventory.h"
#include "hudmanager.h"
#include "character_info.h"
#include "../xrEngine/xr_level_controller.h"
#include "UsableScriptObject.h"
#include "customzone.h"
#include "../GameMtlLib.h"
#include "ui/UIMainIngameWnd.h"
#include "UIGameCustom.h"
#include "Grenade.h"
#include "clsid_game.h"
#include "game_cl_base.h"
#include "Level.h"
#include "uifontdefines.h"

//B77B40
#define PICKUP_INFO_COLOR_FAREST 0xFF686054//895555//DDDDDD
#define PICKUP_INFO_COLOR_MIDLE 0xFF776E61
#define PICKUP_INFO_COLOR_NEAREST 0xFF897F70
//AAAAAA

void CActor::feel_touch_new				(CObject* O)
{
}

void CActor::feel_touch_delete	(CObject* O)
{
	CPhysicsShellHolder* sh=smart_cast<CPhysicsShellHolder*>(O);
	if(sh&&sh->character_physics_support()) m_feel_touch_characters--;
}

BOOL CActor::feel_touch_contact		(CObject *O)
{
	CInventoryItem	*item = smart_cast<CInventoryItem*>(O);
	CInventoryOwner	*inventory_owner = smart_cast<CInventoryOwner*>(O);

	if (item && item->Useful() && !item->object().H_Parent()) 
		return TRUE;

	if(inventory_owner && inventory_owner != smart_cast<CInventoryOwner*>(this))
	{
		CPhysicsShellHolder* sh=smart_cast<CPhysicsShellHolder*>(O);
		if(sh&&sh->character_physics_support()) m_feel_touch_characters++;
		return TRUE;
	}

	return		(FALSE);
}

BOOL CActor::feel_touch_on_contact	(CObject *O)
{
	CCustomZone	*custom_zone = smart_cast<CCustomZone*>(O);
	if (!custom_zone)
		return	(TRUE);

	Fsphere		sphere;
	sphere.P	= Position();
	sphere.R	= EPS_L;
	if (custom_zone->inside(sphere))
		return	(TRUE);

	return		(FALSE);
}

void CActor::PickupModeOn()
{
	m_bPickupMode = true;
}

void CActor::PickupModeOff()
{
	m_bPickupMode = false;
}

ICF static BOOL info_trace_callback(collide::rq_result& result, LPVOID params)
{
	BOOL& bOverlaped	= *(BOOL*)params;
	if(result.O){
		if (Level().CurrentEntity()!=result.O){	
			bOverlaped		= TRUE; //tatarinrafa: раскоментил, чтобы нельзя было подбирать и видеть предметы через двери и тд. Надо тестить.| Uncomented so that actor cant pick up through dynamic objects. Needs tests
			return			TRUE;//FALSE;
		}else{

			return			TRUE;
		}
	}
	else{
		//получить треугольник и узнать его материал
		CDB::TRI* T = Level().ObjectSpace.GetStaticTris() + result.element;
		if (GMLib.GetMaterialByIdx(T->material)->Flags.is(SGameMtl::flPassable) && !GMLib.GetMaterialByIdx(T->material)->Flags.is(SGameMtl::flDynamic)){
			return TRUE;
		}
	}	
	bOverlaped			= TRUE;
	return				FALSE;
}

BOOL CActor::CanPickItem(const CFrustum& frustum, const Fvector& from, CObject* item)
{
	BOOL	bOverlaped		= FALSE;
	Fvector dir,to; 
	item->Center			(to);
	float range				= dir.sub(to,from).magnitude();
	//CInventoryItem* invitem				= smart_cast<CInventoryItem*>(item);
	//if (invitem->object().CLS_ID == CLSID_ARTEFACT){
		//Msg("%f   %s   %s    %s    %s", range, invitem->object().cName(), invitem->object().cNameSect_str(), invitem->object().cNameSect().c_str(), invitem->object().cNameSect());
	//}
	if (range>0.25f){
		if (frustum.testSphere_dirty(to,item->Radius())){
			dir.div						(range);
			collide::ray_defs			RD(from, dir, range, CDB::OPT_CULL, collide::rqtBoth);
			VERIFY						(!fis_zero(RD.dir.square_magnitude()));
			RQR.r_clear					();
			Level().ObjectSpace.RayQuery(RQR,RD, info_trace_callback, &bOverlaped, NULL, item);
		}
	}
	return !bOverlaped;
}

#include "ai\monsters\ai_monster_utils.h"
void CActor::PickupModeUpdate()
{
	if(!m_bPickupMode) return;
	if (GameID() != GAME_SINGLE) return;

	//подбирание объекта
	if(inventory().m_pTarget && inventory().m_pTarget->Useful() &&
		m_pUsableObject && m_pUsableObject->nonscript_usable() &&
		!Level().m_feel_deny.is_object_denied(smart_cast<CGameObject*>(inventory().m_pTarget)) )
	{
		NET_Packet P;
		u_EventGen(P,GE_OWNERSHIP_TAKE, ID());
		P.w_u16(inventory().m_pTarget->object().ID());
		u_EventSend(P);
	}

	if (eacFirstEye != cam_active)
		feel_touch_update	(Position(), m_fPickupInfoRadius);
	else {
		feel_touch_update	((Fvector&)get_bone_position(this, "bip01_spine"), m_fPickupInfoRadius);
	}
	
	CFrustum frustum;
	frustum.CreateFromMatrix(Device.mFullTransform,FRUSTUM_P_LRTB|FRUSTUM_P_FAR);
	//. slow (ray-query test)
	for(xr_vector<CObject*>::iterator it = feel_touch.begin(); it != feel_touch.end(); it++)
		if (CanPickItem(frustum,Device.vCameraPosition,*it)) PickupInfoDraw(*it);
}

#include "../CameraBase.h"
BOOL	g_b_COD_PickUpMode = TRUE;
void	CActor::PickupModeUpdate_COD	()
{
	if (Level().CurrentViewEntity() != this || !g_b_COD_PickUpMode) return;
		
	if (!g_Alive() || eacFreeLook == cam_active) 
	{
		CurrentGameUI()->UIMainIngameWnd->SetPickUpItem(NULL);
		return;
	};
	
	CFrustum frustum;
	frustum.CreateFromMatrix(Device.mFullTransform,FRUSTUM_P_LRTB|FRUSTUM_P_FAR);

	//---------------------------------------------------------------------------
	ISpatialResult.clear	();
	g_SpatialSpace->q_frustum(ISpatialResult, 0, STYPE_COLLIDEABLE, frustum);
	//---------------------------------------------------------------------------

	float maxlen = 1000.0f;
	CInventoryItem* pNearestItem = NULL;
	for (u32 o_it=0; o_it<ISpatialResult.size(); o_it++)
	{
		ISpatial*		spatial	= ISpatialResult[o_it];
		CInventoryItem*	pIItem	= smart_cast<CInventoryItem*> (spatial->dcast_CObject        ());
		if (0 == pIItem) continue;
		if (pIItem->object().H_Parent() != NULL) continue;
		if (!pIItem->CanTake()) continue;
		if (pIItem->object().CLS_ID == CLSID_OBJECT_G_RPG7 || pIItem->object().CLS_ID == CLSID_OBJECT_G_FAKE)
			continue;

		CGrenade*	pGrenade	= smart_cast<CGrenade*> (spatial->dcast_CObject        ());
		if (pGrenade && !pGrenade->Useful()) continue;

		CMissile*	pMissile	= smart_cast<CMissile*> (spatial->dcast_CObject        ());
		if (pMissile && !pMissile->Useful()) continue;
		
		Fvector A, B, tmp; 
		pIItem->object().Center			(A);
		if (A.distance_to_sqr(Position())>4) continue;

		tmp.sub(A, cam_Active()->vPosition);
		B.mad(cam_Active()->vPosition, cam_Active()->vDirection, tmp.dotproduct(cam_Active()->vDirection));
		float len = B.distance_to_sqr(A);
		if (len > 1) continue;

		if (maxlen>len && !pIItem->object().getDestroy())
		{
			maxlen = len;
			pNearestItem = pIItem;
		};
	}

	if(pNearestItem)
	{
		CFrustum					frustum;
		frustum.CreateFromMatrix	(Device.mFullTransform,FRUSTUM_P_LRTB|FRUSTUM_P_FAR);
		if (!CanPickItem(frustum,Device.vCameraPosition,&pNearestItem->object()))
			pNearestItem = NULL;
	}

	if (pNearestItem && pNearestItem->cast_game_object())
	{
		if (Level().m_feel_deny.is_object_denied(pNearestItem->cast_game_object()))
				pNearestItem = NULL;
	}

	if (CurrentGameUI() && CurrentGameUI()->UIMainIngameWnd)
		CurrentGameUI()->UIMainIngameWnd->SetPickUpItem(pNearestItem);

	if (pNearestItem && m_bPickupMode)
	{
		//подбирание объекта
		Game().SendPickUpEvent(ID(), pNearestItem->object().ID());
		
		PickupModeOff();
	}
};

void CActor::PickupInfoDraw(CObject* object)
{
	//tatarinrafa: Немного изменил то, как рабоатет подсвечивание/Added some changes to how highlighting works
	CInventoryItem* item = smart_cast<CInventoryItem*>(object);
	if(!item || !item->IsPickUpVisible())		return;

	LPCSTR draw_str 	= item->NameShort();
	Fmatrix			res;
	res.mul			(Device.mFullTransform,object->XFORM());
	Fvector4		v_res;
	Fvector			shift;

	Fvector dir, to;
	object->Center(to);
	float range = dir.sub(to, Device.vCameraPosition).magnitude();

	shift.set(0,0,0);

	res.transform(v_res,shift);

	if (v_res.z < 0 || v_res.w < 0)	return;
	if (v_res.x < -1.f || v_res.x > 1.f || v_res.y<-1.f || v_res.y>1.f) return;

	float x = (1.f + v_res.x)/2.f * (Device.TargetWidth);
	float y = (1.f - v_res.y)/2.f * (Device.TargetHeight);

	float convertedX = (x  / Device.TargetWidth);
	float convertedY = (y  / Device.TargetHeight);
	convertedX = 1280 * convertedX;
	convertedY = 720 * convertedY;
	//Msg("x = %f, y = %f, convertedX = %f, convertedY = %f", x, y, convertedX, convertedY);

	if (draw_str && convertedX > 380 && convertedX < 900 && convertedY > 200 && convertedY <660)
	{
		if (range < 2.f){
			UI().Font().GetFont(GRAFFITI22_FONT_NAME)->SetAligment(CGameFont::alCenter);
			UI().Font().GetFont(GRAFFITI22_FONT_NAME)->SetColor(PICKUP_INFO_COLOR_NEAREST);
			UI().Font().GetFont(GRAFFITI22_FONT_NAME)->Out(x, y, draw_str);
		}
		else if(range < 4.f){
			UI().Font().GetFont(LETTERICA18_FONT_NAME)->SetAligment(CGameFont::alCenter);
			UI().Font().GetFont(LETTERICA18_FONT_NAME)->SetColor(PICKUP_INFO_COLOR_MIDLE);
			UI().Font().GetFont(LETTERICA18_FONT_NAME)->Out(x, y, draw_str);
		}else
		{
			UI().Font().GetFont(LETTERICA16_FONT_NAME)->SetAligment(CGameFont::alCenter);
			UI().Font().GetFont(LETTERICA16_FONT_NAME)->SetColor(PICKUP_INFO_COLOR_FAREST);
			UI().Font().GetFont(LETTERICA16_FONT_NAME)->Out(x, y, draw_str);
		}


	}
}

void CActor::feel_sound_new(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& Position, float power)
{
	if(who == this)
		m_snd_noise = _max(m_snd_noise,power);
}
