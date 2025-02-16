#pragma once

#include "OutfitBase.h"

struct SBoneProtections;

class CHelmet: public COutfitBase {
private:
    typedef	COutfitBase inherited;
public:
							CHelmet					(void);

	virtual void			Load					(LPCSTR section);

	float					m_fShowNearestEnemiesDistance;

protected:
	virtual bool			install_upgrade_impl	( LPCSTR section, bool test );
};