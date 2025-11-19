///////////////////////////////////////////////////////////////
// ThornArtifact.h
// ThornArtefact - артефакт колючка
///////////////////////////////////////////////////////////////

#pragma once
#include "Artefact.h"

class CThornArtefact final : public CArtefact 
{
private:
	typedef CArtefact inherited;
public:
	CThornArtefact(void);
	virtual ~CThornArtefact(void);

	virtual void Load				(LPCSTR section);

protected:
};