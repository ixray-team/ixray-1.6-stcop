#include "stdafx.h"

namespace 
{
	// Lookup table для software реализации
	static u32 crc32_table[256]; 

	class Crc32Initializer final
	{
	public:
		static void init() noexcept 
		{
			static Crc32Initializer initializer;
		}

	private:
		Crc32Initializer() noexcept
		{
			// Инициализация таблицы для software реализации
			constexpr u32 POLYNOMIAL = 0xEDB88320;
			for (u32 i = 0; i < 256; ++i) {
				u32 crc = i;
				for (int j = 0; j < 8; ++j) {
					crc = (crc >> 1) ^ ((crc & 1) ? POLYNOMIAL : 0);
				}
				crc32_table[i] = crc;
			}
		}
	};

	// Аппаратная реализация с SSE4.2
	u32 crc32_sse42(const void* P, size_t len, u32 starting_crc = ~0u) noexcept
	{
		const u8* buffer = static_cast<const u8*>(P);
		u32 crc = starting_crc;

		// Обрабатываем по 8 байт за раз
		while (len >= 8)
		{
			crc = (u32)_mm_crc32_u64(crc, *reinterpret_cast<const u64*>(buffer));
			buffer += 8;
			len -= 8;
		}

		// Обрабатываем оставшиеся 4 байта
		if (len >= 4)
		{
			crc = _mm_crc32_u32(crc, *reinterpret_cast<const u32*>(buffer));
			buffer += 4;
			len -= 4;
		}

		// Обрабатываем оставшиеся 2 байта
		if (len >= 2)
		{
			crc = _mm_crc32_u16(crc, *reinterpret_cast<const u16*>(buffer));
			buffer += 2;
			len -= 2;
		}

		// Обрабатываем последний байт
		if (len)
		{
			crc = _mm_crc32_u8(crc, *buffer);
		}

		return crc;
	}
}

u32 crc32(const void* P, size_t len)
{
	Crc32Initializer::init();

	if (CPU::ID().hasFeature(CPUFeature::SSE42))
	{
		return crc32_sse42(P, len);
	}

	// Software fallback
	u32 crc = ~0u;
	const u8* buffer = static_cast<const u8*>(P);

	while (len--)
	{
		crc = (crc >> 8) ^ crc32_table[(crc & 0xFF) ^ *buffer++];
	}

	return ~crc;
}

u32 crc32(const void* P, size_t len, u32 starting_crc)
{
	Crc32Initializer::init();

	if (CPU::ID().hasFeature(CPUFeature::SSE42))
	{
		return crc32_sse42(P, len, ~starting_crc);
	}

	// Software fallback
	u32 crc = ~starting_crc;
	const u8* buffer = static_cast<const u8*>(P);

	while (len--)
	{
		crc = (crc >> 8) ^ crc32_table[(crc & 0xFF) ^ *buffer++];
	}

	return ~crc;
}

u32 path_crc32(const char* path, size_t len)
{
	Crc32Initializer::init();
	u32 crc = ~0u;
	const u8* buffer = reinterpret_cast<const u8*>(path);

	// Для путей используем только software реализацию, 
	// так как нужно пропускать символы '/' и '\'
	while (len--) {
		const u8 c = *buffer;
		if (c != '/' && c != '\\') {
			crc = (crc >> 8) ^ crc32_table[(crc & 0xFF) ^ *buffer];
		}
		++buffer;
	}

	return ~crc;
}