#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"

class IELAdapter;
class CELFlyingMovement;
struct SELFlyingSettings;

// Owns a short certified continuation independently of the tactical search.
class CELFlyingContinuity final
{
public:
	void Prepare(IELAdapter& Adapter, CELFlyingMovement& Movement, const SELFlyingSettings& Settings, const Fvector& Destination);
	bool Take(const Fvector& Origin, SELNavigationResult& Route);
	EELGeometryResult Connect(IELAdapter& Adapter, CELFlyingMovement& Movement, const SELFlyingSettings& Settings, SELNavigationResult& Route, const Fvector* RequiredContact);
	void Clear();

private:
	static SELNavigationResult Curve(const SELNavigationIdentity& Identity, const Fvector& Start, const Fvector& Forward, const Fvector& Goal, const Fvector& EndDirection, float Handle);
	SELNavigationResult Prepared;
	u32 AttemptFrame = u32(-1);
	u32 ConnectFrame = u32(-1);
	u32 Attempt = 0;
};
