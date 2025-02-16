#pragma once
#include "../BaseMonster/base_monster.h"
#include "../../../script_export_space.h"

class CStateManagerKarlik;

class CKarlik : public CBaseMonster {
	typedef		CBaseMonster		inherited;
	
public:
					CKarlik 			();
	virtual			~CKarlik 			();	

	virtual void	Load				(LPCSTR section);
	virtual void	CheckSpecParams		(u32 spec_params);
	float	GetDeltaPower		() {return m_fDeltaPower;}


private:
	float m_fDeltaPower;

public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

add_to_type_list(CKarlik)
#undef script_type_list
#define script_type_list save_type_list(CKarlik)
