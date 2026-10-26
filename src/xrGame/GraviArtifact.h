///////////////////////////////////////////////////////////////
// GraviArtifact.h
// GraviArtefact - гравитационный артефакт, прыгает на месте
// и парит над землей
///////////////////////////////////////////////////////////////

#pragma once
#include "Artefact.h"

class CGraviArtefact : public CArtefact 
{
private:
	typedef CArtefact inherited;
public:
	CGraviArtefact(void);
	virtual ~CGraviArtefact(void) override;

	virtual void Load				(const char* section) override;

protected:
	virtual void	UpdateCLChild	() override;
	//параметры артефакта
	float m_fJumpHeight;
	float m_fEnergy;
};
