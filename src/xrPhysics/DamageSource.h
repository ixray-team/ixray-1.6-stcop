#pragma once
 

class IDamageSource
{
public:
	virtual							~IDamageSource			()												{}				;
	virtual			void			SetInitiator			(ALife::_OBJECT_ID id)										=0				;
	virtual			ALife::_OBJECT_ID				Initiator				()												=0				;
	virtual			IDamageSource	*cast_IDamageSource		()												=0				;//{return this	;}
};