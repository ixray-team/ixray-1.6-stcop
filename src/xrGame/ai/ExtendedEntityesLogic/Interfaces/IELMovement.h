#pragma once

class IELMovement
{
public:
	virtual ~IELMovement() = default;
	virtual bool MoveTo(const Fvector& TargetPosition, u32 TargetVertex, bool IsRunning) = 0;
	virtual void Stop() = 0;
};
