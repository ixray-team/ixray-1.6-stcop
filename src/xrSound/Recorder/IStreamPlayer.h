#pragma once

class IStreamPlayer
{
public:
	virtual ~IStreamPlayer() = default;
	virtual bool IsPlaying() = 0;
	virtual void PushToPlay(const void* data, int count) = 0;
	virtual void Update() = 0;
	virtual void SetDistance(float value) = 0;
	virtual void SetPosition(const Fvector& pos) = 0;
};