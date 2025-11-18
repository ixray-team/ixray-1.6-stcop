///////////////////////////////////////////////////////////////
// RustyHairArtifact.h
// RustyHairArtefact - артефакт ржавые волосы
///////////////////////////////////////////////////////////////

#pragma once
#include "Artefact.h"

class CRustyHairArtefact final : public CArtefact 
{
private:
	typedef CArtefact inherited;
public:
	CRustyHairArtefact(void);
	virtual ~CRustyHairArtefact(void);

	virtual void Load				(LPCSTR section);

protected:
};