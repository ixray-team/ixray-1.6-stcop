/////////////////////////////////////
// file name: AcidFog.h
// author: lost alpha
// created: 18/08/2011
// edited: 18/08/2011
// purpose: rusty fog anomaly
//////////////////////////////////////
#pragma once

#include "customzone.h"
#include "script_export_space.h"

class CRustyFog : public CCustomZone
{
private:
	typedef	CCustomZone	inherited;
public:
	CRustyFog(void);
	virtual ~CRustyFog(void);

	virtual void Load(LPCSTR section);
	virtual void Postprocess(f32 val);
	virtual bool EnableEffector() {return true;}


	virtual void Affect(SZoneObjectInfo* O);

protected:
	virtual bool BlowoutState();
	bool m_bLastBlowoutUpdate;
private:
	void RustyItems();
	float m_rust_damage;
public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
add_to_type_list(CRustyFog)
#undef script_type_list
#define script_type_list save_type_list(CRustyFog)