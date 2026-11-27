#pragma once

#include "../../xrCore/LocatorAPI.h"
#include <array>

namespace Sandbox
{
	struct EmbeddedFile
	{
		const char* Name;
		const unsigned char* Bytes;
		size_t Size;
	};

	struct EmbeddedLevel
	{
		const char* Folder;
		const EmbeddedFile* Files;
		size_t Count;
	};

	const EmbeddedLevel* FindEmbeddedLevel(const char* folder);

	class EmbeddedLevelRegistration
	{
	public:
		EmbeddedLevelRegistration() = default;
		~EmbeddedLevelRegistration();
		EmbeddedLevelRegistration(const EmbeddedLevelRegistration&) = delete;
		EmbeddedLevelRegistration& operator=(const EmbeddedLevelRegistration&) = delete;
		EmbeddedLevelRegistration(EmbeddedLevelRegistration&& other) noexcept;
		EmbeddedLevelRegistration& operator=(EmbeddedLevelRegistration&& other) noexcept;

		bool Install(const EmbeddedLevel& level);
		void Reset();
		bool IsInstalled() const { return Installed; }

	private:
		struct SRegisteredFile
		{
			xr_string Path;
			CLocatorAPI::SMemoryFileRestore Restore;
		};

		std::array<SRegisteredFile, 7> Registered = {};
		size_t Count = 0;
		bool Installed = false;
	};
}
