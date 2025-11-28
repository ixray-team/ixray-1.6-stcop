#pragma once

#include "../xrCore/FTimer.h"
#include "CameraFirstEye.h"

class CSpectrCameraFirstEye :
	public CCameraFirstEye
{
private:
	using inherited = CCameraFirstEye;
	float const& m_fTimeDelta;
public:
	CSpectrCameraFirstEye(float const& fTimeDelta, CObject* p, u32 flags = 0);
	virtual ~CSpectrCameraFirstEye();

	CSpectrCameraFirstEye& operator=(CSpectrCameraFirstEye& copy) = delete;

	virtual void Move(int cmd, float val = 0, float factor = 1.0f);
};