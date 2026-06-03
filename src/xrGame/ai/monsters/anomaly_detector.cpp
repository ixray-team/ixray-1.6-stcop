#include "stdafx.h"
#include "anomaly_detector.h"
#include "basemonster/base_monster.h"
#include "../../restricted_object.h"
#include "../../AnomalyZone.h"
#include "../../Level.h"
#include "../../space_restriction_manager.h"

CAnomalyDetector::CAnomalyDetector(CBaseMonster *monster) : m_object(monster)
{
}

CAnomalyDetector::~CAnomalyDetector()
{
}

void CAnomalyDetector::load(const char* section)
{
	m_radius				= pSettings->read_if_exists<float>(section,"Anomaly_Detect_Radius",15.f);
	m_time_to_rememeber		= pSettings->read_if_exists<u32>(section,"Anomaly_Detect_Time_Remember",30000);
	shared_str IgnoredCLSIDsSection = pSettings->read_if_exists<str_c>(section, "Anomaly_Detect_Ignore_List", "");
	if (IgnoredCLSIDsSection.size()) {
		R_ASSERT3(pSettings->section_exist(IgnoredCLSIDsSection), "Unable to find section [%s]", IgnoredCLSIDsSection.c_str());
		for (u32 i = 0; i < pSettings->line_count(IgnoredCLSIDsSection); ++i) {
			str_c Key = nullptr;
			str_c Value = nullptr;
			IVERIFY(pSettings->r_line(IgnoredCLSIDsSection, i, Key, Value));
			IgnoredCLSIDS.emplace(TEXT2CLSID(Key));
		}
	}
}

void CAnomalyDetector::reinit()
{
	m_storage.clear();

	m_active = false;
}


void CAnomalyDetector::update_schedule()
{
	if (m_active)
		m_object->feel_touch_update(m_object->Position(), m_radius);

	if (m_storage.empty()) 
		return;

	xr_vector<ALife::_OBJECT_ID>			temp_out_restrictors;
	xr_vector<ALife::_OBJECT_ID>			temp_in_restrictors;
	
	temp_in_restrictors.reserve(m_storage.size());
	
	// add new restrictions
	for (ANOMALY_INFO_VEC_IT it = m_storage.begin(); it != m_storage.end(); it++) {
		if (it->time_registered == 0) {
			temp_in_restrictors.push_back(it->object->ID());
			it->time_registered = time();
		}
	}

	m_object->control().path_builder().restrictions().add_restrictions(temp_out_restrictors,temp_in_restrictors);

	// remove old restrictions
	temp_in_restrictors.clear();
	for (ANOMALY_INFO_VEC_IT it = m_storage.begin(); it != m_storage.end(); it++) {
		if (it->time_registered + m_time_to_rememeber < time()) {
			temp_in_restrictors.push_back(it->object->ID());
		}
	}

	m_object->control().path_builder().restrictions().remove_restrictions(temp_out_restrictors,temp_in_restrictors);

	
	// remove from storage
	m_storage.erase		(
		std::remove_if(
			m_storage.begin(),
			m_storage.end(),
			remove_predicate(m_time_to_rememeber)
		),
		m_storage.end()
	);
}

void CAnomalyDetector::on_contact(CObject *obj)
{
	if (!m_active)
	{
		return;
	}
	if(!obj || obj->getDestroy())
	{
		return;
	}
	CGameObject* game_object = obj->cast_game_object();
	if (!game_object)
	{
		return;
	}
	CAnomalyZone* custom_zone = game_object->cast_anomaly_zone();
	if (!custom_zone)
	{
		return;
	}
	
	// if its NOT A restrictor - skip
	if (custom_zone->restrictor_type() == RestrictionSpace::eRestrictorTypeNone)
	{
		return;
	}
	
	if (IgnoredCLSIDS.find(custom_zone->CLS_ID) != IgnoredCLSIDS.end()) { 
		return; 
	}

	if (Level().space_restriction_manager().restriction_presented(
		m_object->control().path_builder().restrictions().in_restrictions(),custom_zone->cName()))
	{
		return;
	}

	ANOMALY_INFO_VEC_IT it = std::find(m_storage.begin(), m_storage.end(), custom_zone);	
	if (it != m_storage.end())
	{
		return;
	}

	SAnomalyInfo			info;
	info.object				= obj;
	info.time_registered	= 0;
	m_storage.push_back		(info);
}
