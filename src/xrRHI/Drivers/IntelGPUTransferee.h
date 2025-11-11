#pragma once

class CIntelReader :
	public IRHIGPU
{
public:
	CIntelReader() {};
	~CIntelReader() {};

	virtual void Initialize() {}
	virtual u32	 GetPercentActive() { return 100; }
	virtual u32	 GetGPUCount() { return 1; }

	virtual CNvReader* GetNV() override { return nullptr; }
	virtual bool SetDepthBounds(bool, float zMin, float zMax) override { return false; };
};