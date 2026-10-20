#pragma once

enum class EELBehaviorState : u8
{
	Running,
	Completed,
	WaitingForBudget,
	WaitingForResult,
	Blocked,
	NoProgress,
	InvalidTarget
};

enum class EELRecovery : u8
{
	Continue,
	Restart,
	Release
};

struct SELBehaviorValidation
{
	EELBehaviorState State = EELBehaviorState::Running;
	const char* Reason = "Выполняется";
	u32 StartedAt = 0;
	u32 Deadline = 0;
	u32 WaitingSince = 0;
};

class IELBehavior
{
public:
	virtual ~IELBehavior() = default;
	virtual void Enter() = 0;
	virtual void Update(float DeltaTime) = 0;
	virtual SELBehaviorValidation Validate() const = 0;
	virtual EELRecovery Recover(const SELBehaviorValidation& Validation) = 0;
	virtual void Exit() = 0;
};
