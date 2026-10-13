#pragma once

class IRenderVisual;

class UI_API UI3dIconCache final
{
public:
	static UI3dIconCache& Instance();
	IRenderVisual* Acquire(const shared_str& Name);
	void Release(IRenderVisual* Visual);
	bool Matches(IRenderVisual* Visual, const shared_str& Name) const;
	size_t GetModelCount() const;

private:
	UI3dIconCache() = default;
	~UI3dIconCache() = default;

	struct ModelEntry
	{
		IRenderVisual* Visual = nullptr;
		u32 References = 0;
	};

	xr_map<xr_string, ModelEntry> Models;
	xr_map<IRenderVisual*, xr_string> Instances;
	mutable xrCriticalSection Lock;
};
