#pragma once

class IELBudget
{
public:
	virtual ~IELBudget() = default;
	virtual void BeginFrame(u32 FrameIdentifier) = 0;
	virtual bool TryConsume(u32 OperationCount = 1) = 0;
};
