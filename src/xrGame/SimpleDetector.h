#pragma once
#include "CustomDetector.h"

class CUIArtefactDetectorSimple;

class CSimpleDetector final : public CCustomDetector
{
	using inherited = CCustomDetector;
public:
	CSimpleDetector();
	~CSimpleDetector() override = default;

	virtual CCustomDetector* cast_custom_detector() { return this; }
	virtual CCustomDevice* cast_custom_device() { return this; }

protected:
	void UpdateAf() override;
	void CreateUI() override;
	CUIArtefactDetectorSimple& ui();
};

