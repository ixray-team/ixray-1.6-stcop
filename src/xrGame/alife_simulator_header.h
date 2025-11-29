////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_simulator_header.h
//	Created 	: 05.01.2003
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator header
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "object_interfaces.h"
#include "alife_space.h"

class CALifeSimulatorHeader final {
protected:
	u32								m_version;

public:
	IC CALifeSimulatorHeader(const char* section);
	~CALifeSimulatorHeader();
	void save(IWriter &tMemoryStream);
	void load(IReader &tFileStream);
	void Serialize(ISaveObject& Object);
	
	IC		u32						version					() const;
			bool					valid					(IReader &file_stream) const;
};

#include "alife_simulator_header_inline.h"