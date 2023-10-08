#pragma once

enum class EEngineExternalUI {
	HQIcons,
	None
};

enum class EEngineExternalPhysical {
	None
};

enum class EEngineExternalGame {
	None
};

enum class EEngineExternalRender {
	None
};

class ENGINE_API CEngineExternal final {
	CInifile* pOptions = nullptr;

public:
	CEngineExternal();
	~CEngineExternal();

	bool operator[](const EEngineExternalUI& ID) const;
	bool operator[](const EEngineExternalPhysical& ID) const;
	bool operator[](const EEngineExternalGame& ID) const;
	bool operator[](const EEngineExternalRender& ID) const;
};

ENGINE_API CEngineExternal& EngineExternal();
