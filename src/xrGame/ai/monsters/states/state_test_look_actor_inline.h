#pragma once

#include "../../../level.h"


void CStateMonsterLookActor::execute()
{
	this->object->set_action			(ACT_STAND_IDLE);
	this->object->dir().face_target	(Level().CurrentEntity()->Position(), 1200);
}


void CStateMonsterTurnAwayFromActor::execute()
{
	Fvector point;
	Fvector dir;
	dir.sub			(this->object->Position(), Level().CurrentEntity()->Position());
	dir.normalize	();
	point.mad		(this->object->Position(), dir, 2.f);
	
	this->object->set_action			(ACT_STAND_IDLE);
	this->object->dir().face_target	(point, 1200);
}



void CStateMonstertTestIdle::execute()
{
	this->object->set_action			(ACT_STAND_IDLE);
}
