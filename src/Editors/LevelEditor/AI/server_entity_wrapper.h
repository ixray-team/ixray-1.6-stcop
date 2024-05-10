////////////////////////////////////////////////////////////////////////////
//	Module 		: server_entity_wrapper.h
//	Created 	: 16.10.2004
//  Modified 	: 16.10.2004
//	Author		: Dmitriy Iassenev
//	Description : Server entity wrapper
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "..\xrEngine\object_interfaces.h"

class ISE_Abstract;

class CServerEntityWrapper : public IPureSerializeObject<IReader,IWriter> {
private:
	ISE_Abstract			*m_object;

public:
	IC						CServerEntityWrapper	(ISE_Abstract *object = 0);
	virtual					~CServerEntityWrapper	();	
	virtual	void			save					(IWriter &stream);
	virtual	void			load					(IReader &stream);
			void			save_update				(IWriter &stream);
			void			load_update				(IReader &stream);
	IC		ISE_Abstract	&object					() const;
};

#include "server_entity_wrapper_inline.h"