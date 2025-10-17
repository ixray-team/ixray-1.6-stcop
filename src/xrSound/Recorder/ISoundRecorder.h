#pragma once

class ISoundRecorder
{
public:
	virtual ~ISoundRecorder() = default;

	virtual bool IsStarted() = 0;
	virtual void Start() = 0;
	virtual void Stop() = 0;
};