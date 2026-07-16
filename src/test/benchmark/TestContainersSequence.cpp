#include "../../xrCore/stdafx.h"
#include "Benchmark.h"
#include <random>

#include "../../xrCore/Containers/ArbitraryList.h"
#include "../../xrCore/Containers/FixedVector.h"

constexpr int TestSize = 1'000'000;

ArbitraryList<int> GArbitraryList;
std::vector<int> GStdVector;
FixedVector<int, TestSize> GFixedVector;

alignas(buffer_vector<int>::value_type) std::byte GBuffer[sizeof(int) * TestSize];
buffer_vector<int> GBufferVector(GBuffer, TestSize);

static std::vector<int> Keys(TestSize);
void InitKeysSequence()
{
	std::cout << "Vector test size: " << TestSize << std::endl;

	std::mt19937 rng(12345);
	std::iota(Keys.begin(), Keys.end(), 0);

	std::shuffle(Keys.begin(), Keys.end(), rng);
}

BENCHMARK(ArbitraryList, PushBack)
{
	GArbitraryList = {};

	for (int i = 0; i < TestSize; ++i)
	{
		GArbitraryList.push_back(i);
	}
}

BENCHMARK(StdVector, PushBack)
{
	GStdVector.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GStdVector.push_back(i);
	}
}

BENCHMARK(FixedVector, PushBack)
{
	GFixedVector.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GFixedVector.push_back(i);
	}
}

BENCHMARK(BufferVector, PushBack)
{
	GBufferVector.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GBufferVector.push_back(i);
	}
}

BENCHMARK(ArbitraryList, Find)
{
	volatile int result = -1;

	for (int value = 0; value < 30'000; ++value)
	{
		for (size_t i = 0; i < GArbitraryList.size(); ++i)
		{
			if (GArbitraryList[i] == value)
			{
				result = static_cast<int>(i);
				break;
			}
		}
	}
}

BENCHMARK(StdVector, Find)
{
	volatile int result = -1;

	for (int value = 0; value < 30'000; ++value)
	{
		auto it = std::find(GStdVector.begin(), GStdVector.end(), value);

		if (it != GStdVector.end())
		{
			result = static_cast<int>(std::distance(GStdVector.begin(), it));
		}
	}
}

BENCHMARK(FixedVector, Find)
{
	volatile int result = -1;

	for (int value = 0; value < 30'000; ++value)
	{
		auto it = std::find(GFixedVector.begin(), GFixedVector.end(), value);

		if (it != GFixedVector.end())
		{
			result = static_cast<int>(it - GFixedVector.begin());
		}
	}
}

BENCHMARK(BufferVector, Find)
{
	volatile int result = -1;

	for (int value = 0; value < 30'000; ++value)
	{
		auto it = std::find(GBufferVector.begin(), GBufferVector.end(), value);

		if (it != GBufferVector.end())
		{
			result = static_cast<int>(std::distance(GBufferVector.begin(), it));
		}
	}
}

BENCHMARK(ArbitraryList, RandomRead)
{
	volatile int sum = 0;

	for (int i = 0; i < Keys.size(); ++i)
	{
		sum += GArbitraryList[Keys[i]];
	}
}

BENCHMARK(StdVector, RandomRead)
{
	volatile int sum = 0;

	for (int i = 0; i < Keys.size(); ++i)
	{
		sum += GStdVector[Keys[i]];
	}
}

BENCHMARK(FixedVector, RandomRead)
{
	volatile int sum = 0;

	for (int i = 0; i < Keys.size(); ++i)
	{
		sum += GFixedVector[Keys[i]];
	}
}

BENCHMARK(BufferVector, RandomRead)
{
	volatile int sum = 0;

	for (int i = 0; i < Keys.size(); ++i)
	{
		sum += GBufferVector[Keys[i]];
	}
}

BENCHMARK(ArbitraryList, Copy_And_EraseFast)
{
	auto Copy = GArbitraryList;

	while (Copy.size())
	{
		Copy.erase_fast(Copy.size() / 2);
	}
}

BENCHMARK(StdVector, Copy_And_EraseFast)
{
	auto Copy = GStdVector;

	while (!Copy.empty())
	{
		const size_t idx = Copy.size() / 2;
		Copy[idx] = Copy.back();
		Copy.pop_back();
	}
}

BENCHMARK(BufferVector, Copy_And_EraseFast)
{
	static alignas(int) std::byte Buffer[sizeof(int) * TestSize];

	buffer_vector<int> Copy(Buffer, TestSize, GBufferVector);

	while (!Copy.empty())
	{
		const size_t idx = Copy.size() / 2;

		Copy[idx] = Copy.back();
		Copy.pop_back();
	}
}