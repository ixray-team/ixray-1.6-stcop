////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_anomalous_zone.cpp
//	Created 	: 27.10.2005
//  Modified 	: 10.05.2017 tatarinrafa
//	Author		: Dmitriy Iassenev
//	Description : ALife anomalous zone class
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"
#include "alife_spawn_registry.h"
#include "alife_graph_registry.h"

#pragma warning(push)
#pragma warning(disable:4995)
#include <malloc.h>
#pragma warning(pop)

u32 newgameartscount = 0;

CSE_ALifeItemWeapon	*CSE_ALifeAnomalousZone::tpfGetBestWeapon(ALife::EHitType &tHitType, float &fHitPower)
{
	m_tpCurrentBestWeapon		= 0;
	m_tTimeID					= ai().alife().time_manager().game_time();
	fHitPower					= m_maxPower;
	tHitType					= m_tHitType;
	return						(m_tpCurrentBestWeapon);
}

ALife::EMeetActionType CSE_ALifeAnomalousZone::tfGetActionType(CSE_ALifeSchedulable *tpALifeSchedulable, int iGroupIndex, bool bMutualDetection)
{
	return						(ALife::eMeetActionTypeAttack);
}

bool CSE_ALifeAnomalousZone::bfActive()
{
	return						(fis_zero(m_maxPower,EPS_L) || !interactive());
}

CSE_ALifeDynamicObject *CSE_ALifeAnomalousZone::tpfGetBestDetector()
{
	VERIFY2						(false,"This function shouldn't be called");
	NODEFAULT;
#ifdef DEBUG
	return						(0);
#endif
}
#pragma todo("need to update that function for ability to create many shapes with different sizes")
void CSE_ALifeAnomalousZone::add_shape_size		(float zone_radius)
{
	CShapeData::shape_def _shapes;
	_shapes.data.sphere.P.set	(0.0f,0.0f,0.0f);
	_shapes.data.sphere.R		= zone_radius;
	_shapes.type				= CShapeData::cfSphere;
	assign_shapes		(&_shapes, u32(1));
}

void CSE_ALifeAnomalousZone::spawn_artefacts				()
{
//---- Можно ли спавнить из-за аи сетки?
	VERIFY2					(!m_bOnline,"Cannot spawn artefacts in online!");

	if (this->m_flags.is(flUsedAI_Locations) == FALSE) {
	#ifdef _DEBUG
		Msg("No AI-MAP for object [%d] in game.spawn", this);
	#endif
		return;
	}
//---- Инит
	BOOL m_bSpawnArtefact				= !!READ_IF_EXISTS(pSettings, r_bool, name(),	"art_onstart_spawn", FALSE);
	if (!m_bSpawnArtefact) return;
	float m_fArtefactSpawnProbability	= READ_IF_EXISTS(pSettings, r_float, name(),	"art_onstart_spawn_prob", 0.0);
	u8 m_iArtSpawnCicles				= READ_IF_EXISTS(pSettings, r_u8, name(),		"art_onstart_spawn_cicles", 2);
//---- Инит списка артифактов
	struct ARTEFACT_SPAWN
	{
		shared_str	section;
		float		probability;
	};
	DEFINE_VECTOR(ARTEFACT_SPAWN, ARTEFACT_SPAWN_VECTOR, ARTEFACT_SPAWN_IT);
	ARTEFACT_SPAWN_VECTOR	m_ArtefactSpawn;

	LPCSTR l_caParameters				= READ_IF_EXISTS(pSettings, r_string, name(),	"artefacts", "");
	u16 m_wItemCount = (u16)_GetItemCount(l_caParameters);
R_ASSERT2(!(m_wItemCount & 1), "Invalid number of parameters in string 'artefacts' in the 'system.ltx'!");
	m_wItemCount >>= 1;

	m_ArtefactSpawn.clear();
	string512 l_caBuffer;
	float total_probability = 0.f;
	m_ArtefactSpawn.resize(m_wItemCount);
	for (u16 i = 0; i<m_wItemCount; ++i)
	{
		ARTEFACT_SPAWN& artefact_spawn = m_ArtefactSpawn[i];
		artefact_spawn.section = _GetItem(l_caParameters, i << 1, l_caBuffer);
		artefact_spawn.probability = (float)atof(_GetItem(l_caParameters, (i << 1) | 1, l_caBuffer));
		total_probability += artefact_spawn.probability;
	}
	if (total_probability == 0.f) total_probability = 1.0;
R_ASSERT3(!fis_zero(total_probability), "The probability of artefact spawn is zero!", name());

	for (i = 0; i<m_ArtefactSpawn.size(); ++i)	//нормализировать вероятности
	{
		m_ArtefactSpawn[i].probability = m_ArtefactSpawn[i].probability / total_probability;
	}
	if (m_ArtefactSpawn.empty()) return;
//---- Случайное число
	for (int i = 0; i < m_iArtSpawnCicles; ++i){//tatarinrafa:Lets add an oportunity of spawning several arts
		if (::Random.randF(0.f, 1.f) < m_fArtefactSpawnProbability) {

//---- Выбор арта из списка артифактов (вычислить согласно распределению вероятностей)

			float rnd = ::Random.randF(.0f, 1.f - EPS_L);
			float prob_threshold = 0.f;

			std::size_t i = 0;
			for (; i<m_ArtefactSpawn.size(); i++)
			{
				prob_threshold += m_ArtefactSpawn[i].probability;
				if (rnd<prob_threshold) break;
			}
		R_ASSERT(i<m_ArtefactSpawn.size());
			newgameartscount += 1;
			Msg("New game arts spawning: %s -> %s, total arts = %u", name(), m_ArtefactSpawn[i].section.c_str(), newgameartscount);
			Fvector pos						= position();
			CSE_Abstract* l_tpSE_Abstract	= alife().spawn_item(*m_ArtefactSpawn[i].section, pos, m_tNodeID, m_tGraphID, 0xffff);

//---- Alife shit
		R_ASSERT3(l_tpSE_Abstract, "Can't spawn artefact ", m_ArtefactSpawn[i].section.c_str());

			CSE_ALifeDynamicObject*	oALifeDynamicObject = smart_cast<CSE_ALifeDynamicObject*>(l_tpSE_Abstract);

		R_ASSERT2(oALifeDynamicObject, "Non-ALife object in the 'game.spawn'");

			oALifeDynamicObject->m_tSpawnID			= m_tSpawnID;
			oALifeDynamicObject->m_bALifeControl	= true;
			ai().alife().spawns().assign_artefact_position(this, oALifeDynamicObject);

			Fvector	t		= oALifeDynamicObject->o_Position;
			u32	p			= oALifeDynamicObject->m_tNodeID;
			float q			= oALifeDynamicObject->m_fDistance;
			alife().graph().change(oALifeDynamicObject, m_tGraphID, oALifeDynamicObject->m_tGraphID);
			oALifeDynamicObject->o_Position			= t;
			oALifeDynamicObject->m_tNodeID			= p;
			oALifeDynamicObject->m_fDistance		= q;

		}
	}
}

void CSE_ALifeAnomalousZone::on_spawn						()
{
	inherited::on_spawn		();
	spawn_artefacts			();
}
