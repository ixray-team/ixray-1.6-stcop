////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_sound_data.h
//	Created 	: 02.02.2005
//  Modified 	: 02.02.2005
//	Author		: Dmitriy Iassenev
//	Description : Stalker sound data
////////////////////////////////////////////////////////////////////////////

#pragma once

class CAI_Stalker;

class CStalkerSoundData : 
	public CSound_UserData
{
private:
	CAI_Stalker			*m_object;

public:
	IC					CStalkerSoundData	(CAI_Stalker *object);
	virtual ~CStalkerSoundData	() override;
	virtual void		invalidate			() override;
	virtual void		accept				(CSound_UserDataVisitor *visitor) override;
	IC		CAI_Stalker &object				() const;
};

#include "stalker_sound_data_inline.h"