#include "stdafx.h"
#include "IXRayGameConstants.h"
#include "GamePersistent.h"

bool	m_bUseHighQualityIcons = false;

namespace GameConstants
{
	void LoadConstants()
	{
		m_bUseHighQualityIcons = READ_IF_EXISTS(pSettings, r_bool, "inventory", "use_hq_icons", false);

		Msg("# IX-Ray GameConstants are loaded");
	}
	
	bool GetUseHQ_Icons()
	{
		return m_bUseHighQualityIcons;
	}
}