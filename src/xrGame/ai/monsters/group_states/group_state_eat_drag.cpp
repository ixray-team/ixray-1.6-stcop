#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "../monster_cover_manager.h"

#include "../monster_home.h"
#include "ai_object_location.h"
#include "PHMovementControl.h"
#include "CharacterPhysicsSupport.h"
#include "../basemonster/base_monster.h"

#include "group_state_eat_drag.h"

#include "cover_point.h"

#include "../dog/dog.h"

#include "../../../../xrPhysics/IPHCapture.h"
#include "../../../CaptureBoneCallback.h"
#include "../../../../include/xrrender/Kinematics.h"

CStateGroupDrag::CStateGroupDrag(CBaseMonster* object) : inherited(object)
{
	m_pDog = smart_cast<CDogBase*>(object);

	m_cover_position = {};
	m_cover_vertex_id = 0;

	m_failed = false;
	m_corpse_start_position = {};
}

CStateGroupDrag::~CStateGroupDrag()
{
}

void CStateGroupDrag::initialize()
{
	inherited::initialize();
	IKinematics* K = object->EatedCorpse->Visual()->dcast_PKinematics();
	VERIFY(K);
	CInifile* ini = K->LL_UserData();
	VERIFY(ini);

	if (!ini->section_exist("capture_used_bones") || !ini->line_exist("capture_used_bones", "bones"))
	{
		m_failed = true;
		return;
	}
	LPCSTR bones = ini->r_string("capture_used_bones", "bones");

	int				bone_number = _GetItemCount(bones);
	u16* vbones = (u16*)_alloca(bone_number * sizeof(u16));
	u16* I = vbones;
	u16* E = vbones + bone_number;
	for (; I != E; ++I) {
		string32	sbone;
		_GetItem(bones, int(I - vbones), sbone);
		*I = K->LL_BoneID(sbone);
		VERIFY(*I != BI_NONE);
	}
	struct callback : public CPHCaptureBoneCallback {
		IKinematics* m_K;
		u16 const* use_bones;
		int			m_bone_number;
		callback(IKinematics* K, u16 const* ub, int const& bone_number) :
			m_K(K),
			use_bones(ub),
			m_bone_number(bone_number)
		{
		}

		bool	operator()	(u16 bone) {
			u16 bi = bone;
			for (; m_K->LL_GetBoneRoot() != bi;)
			{
				struct cmp_pred
				{
					cmp_pred(u16 i) : m_id(i) {}
					u16 m_id;
					bool operator () (u16 id)
					{
						return id == m_id;
					}
				} cmp(bi);

				u16	const* use_bones_end = use_bones + m_bone_number;
				if (std::find_if(use_bones, use_bones_end, cmp) != use_bones_end)
					return	(true);

				bi = m_K->LL_GetData(bi).GetParentID();
			}
			return			(false);
		}
	} cb(K, vbones, bone_number);

	m_pDog->character_physics_support()->movement()->PHCaptureObject(const_cast<CEntityAlive*>(object->EatedCorpse), &cb);

	m_failed = false;

	IPHCapture* capture = m_pDog->character_physics_support()->movement()->PHCapture();
	if (capture && !capture->Failed()) {
		m_cover_vertex_id = m_pDog->Home->get_place_in_min_home();
		if (m_cover_vertex_id != u32(-1)) {
			m_cover_position = ai().level_graph().vertex_position(m_cover_vertex_id);
		}
		else m_cover_position = object->Position();
		if (m_cover_vertex_id == u32(-1) || object->Position().distance_to(m_cover_position) < 2.f || !m_pDog->Home->at_min_home(m_cover_position)) {
			const CCoverPoint* point = m_pDog->CoverMan->find_cover(m_pDog->Home->get_home_point(), 1, m_pDog->Home->get_min_radius());
			if (point)
			{
				m_cover_vertex_id = point->level_vertex_id();
				if (m_cover_vertex_id != u32(-1))
				{
					m_cover_position = ai().level_graph().vertex_position(m_cover_vertex_id);
				}
			}
		}
	}
	else m_failed = true;
	m_corpse_start_position = object->EatedCorpse->Position();
	object->path().prepare_builder();
}


void CStateGroupDrag::execute()
{
	if (m_failed) return;

	// Установить параметры движения
	object->set_action(ACT_DRAG);
	m_pDog->anim().SetSpecParams(ASP_MOVE_BKWD);

	if (m_cover_vertex_id != u32(-1)) {
		object->path().set_target_point(m_cover_position, m_cover_vertex_id);
	}
	else {
		object->path().set_retreat_from_point(object->EatedCorpse->Position());
	}

	object->path().set_generic_parameters();
	m_pDog->anim().accel_activate(eAT_Calm);

}


void CStateGroupDrag::finalize()
{
	inherited::finalize();

	// бросить труп
	if (m_pDog->character_physics_support()->movement()->PHCapture())
		m_pDog->character_physics_support()->movement()->PHReleaseObject();
}


void CStateGroupDrag::critical_finalize()
{
	inherited::critical_finalize();

	// бросить труп
	if (m_pDog->character_physics_support()->movement()->PHCapture())
		m_pDog->character_physics_support()->movement()->PHReleaseObject();
}


bool CStateGroupDrag::check_completion()
{
	if (m_failed) {
		return true;
	}

	if (!m_pDog->character_physics_support()->movement()->PHCapture()) {
		return true;
	}

	if (m_cover_vertex_id != u32(-1)) {		// valid vertex so wait path end
		if (object->Position().distance_to(m_cover_position) < 2.f)
			return true;
	}
	else {								// invalid vertex so check distanced that passed
		if (m_corpse_start_position.distance_to(object->Position()) > m_pDog->Home->get_min_radius())
			return true;
	}

	return false;
}