#include "../../xrCore/stdafx.h"
#include "Benchmark.h"
#include <random>

#include "../../xrCore/Containers/FixedMap.h"
#include "../../xrCore/Containers/associative_vector.h"
#include "../../xrCore/Containers/DenseHashMap.h"

xr_hash_map<int, int> GHashMap;
xr_map<int, int> GMap;
FixedMAP<int, int> GFixedMap;
associative_vector<int, int> GAssociativeVector;
dense_hash_map<int, int> GDenseHashMap;

constexpr int TestSize = 50'000;

static std::vector<int> Keys(TestSize);

void InitKeysAssociative()
{
	std::cout << "Map test size: " << TestSize << std::endl;

	GDenseHashMap.set_empty_key(-1);
	GDenseHashMap.set_deleted_key(-2);

	std::mt19937 rng(12345);
	std::iota(Keys.begin(), Keys.end(), 0);

	std::shuffle(Keys.begin(), Keys.end(), rng);
}

BENCHMARK(DenseHashMap, Insert)
{
	GDenseHashMap.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GDenseHashMap.insert({Keys[i], Keys[i]});
	}
}

BENCHMARK(XrHashMap, Insert)
{
	GHashMap.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GHashMap.insert({Keys[i], Keys[i]});
	}
}

BENCHMARK(XrMap, Insert)
{
	GMap.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GMap.insert({Keys[i], Keys[i]});
	}
}

BENCHMARK(FixedMap, Insert)
{
	GFixedMap.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GFixedMap.insert(Keys[i], Keys[i]);
	}
}

BENCHMARK(AssociativeVector, Insert)
{
	GAssociativeVector.clear();

	for (int i = 0; i < TestSize; ++i)
	{
		GAssociativeVector.insert({Keys[i], Keys[i]});
	}
}

BENCHMARK(XrHashMap, Iterate)
{
	volatile int Sum = 0;

	for (const auto& Pair : GHashMap)
	{
		Sum += Pair.second;
	}
}

BENCHMARK(AssociativeVector, Iterate)
{
	volatile int Sum = 0;

	for (const auto& Pair : GAssociativeVector)
	{
		Sum += Pair.second;
	}
}

BENCHMARK(XrMap, Iterate)
{
	volatile int Sum = 0;

	for (const auto& Pair : GMap)
	{
		Sum += Pair.second;
	}
}

BENCHMARK(FixedMap, Iterate)
{
	volatile int Sum = 0;

	for (auto Node = GFixedMap.begin(); Node != GFixedMap.end(); ++Node)
	{
		Sum += Node->val;
	}
}

BENCHMARK(DenseHashMap, Iterate)
{
	volatile int Sum = 0;

	for (const auto& Pair : GDenseHashMap)
	{
		Sum += Pair.second;
	}
}

BENCHMARK(XrHashMap, Find)
{
	volatile int Sum = 0;

	for (int i = 0; i < TestSize; ++i)
	{
		auto It = GHashMap.find(i);

		if (It != GHashMap.end())
		{
			Sum += It->second;
		}
	}
}

BENCHMARK(XrMap, Find)
{
	volatile int Sum = 0;

	for (int i = 0; i < TestSize; ++i)
	{
		auto It = GMap.find(i);

		if (It != GMap.end())
		{
			Sum += It->second;
		}
	}
}

BENCHMARK(FixedMap, Find)
{
	volatile int Sum = 0;

	for (int i = 0; i < TestSize; ++i)
	{
		auto Node = GFixedMap.find(i);

		if (Node)
		{
			Sum += Node->val;
		}
	}
}

BENCHMARK(AssociativeVector, Find)
{
	volatile int Sum = 0;

	for (int i = 0; i < TestSize; ++i)
	{
		auto It = GAssociativeVector.find(i);

		if (It != GAssociativeVector.end())
		{
			Sum += It->second;
		}
	}
}

BENCHMARK(DenseHashMap, Find)
{
	volatile int Sum = 0;

	for (int i = 0; i < TestSize; ++i)
	{
		auto It = GDenseHashMap.find(i);

		if (It != GDenseHashMap.end())
		{
			Sum += It->second;
		}
	}
}

BENCHMARK(XrHashMap, FindMiss)
{
	volatile int Sum = 0;

	for (int i = TestSize; i < TestSize * 2; ++i)
	{
		if (GHashMap.find(i) != GHashMap.end())
		{
			++Sum;
		}
	}
}

BENCHMARK(XrMap, FindMiss)
{
	volatile int Sum = 0;

	for (int i = TestSize; i < TestSize * 2; ++i)
	{
		if (GMap.find(i) != GMap.end())
		{
			++Sum;
		}
	}
}

BENCHMARK(FixedMap, FindMiss)
{
	volatile int Sum = 0;

	for (int i = TestSize; i < TestSize * 2; ++i)
	{
		if (GFixedMap.find(i))
		{
			++Sum;
		}
	}
}

BENCHMARK(AssociativeVector, FindMiss)
{
	volatile int Sum = 0;

	for (int i = TestSize; i < TestSize * 2; ++i)
	{
		if (GAssociativeVector.find(i) != GAssociativeVector.end())
		{
			++Sum;
		}
	}
}

BENCHMARK(DenseHashMap, FindMiss)
{
	volatile int Sum = 0;

	for (int i = TestSize; i < TestSize * 2; ++i)
	{
		if (GDenseHashMap.find(i) != GDenseHashMap.end())
		{
			++Sum;
		}
	}
}

BENCHMARK(FixedMap, TraverseAny)
{
	static volatile int Sum = 0;

	GFixedMap.traverseANY
	(
		[](auto Node)
		{
			Sum += Node->val;
		}
	);
}

BENCHMARK(DenseHashMap, Erase)
{
	auto Copy = GDenseHashMap;

	for (int i = 0; i < TestSize; ++i)
	{
		Copy.erase(Keys[i]);
	}
}

BENCHMARK(XrHashMap, Erase)
{
	auto Copy = GHashMap;

	for (int i = 0; i < TestSize; ++i)
	{
		Copy.erase(Keys[i]);
	}
}

BENCHMARK(XrMap, Erase)
{
	auto Copy = GMap;

	for (int i = 0; i < TestSize; ++i)
	{
		Copy.erase(Keys[i]);
	}
}

BENCHMARK(AssociativeVector, Erase)
{
	auto Copy = GAssociativeVector;

	for (int i = 0; i < TestSize; ++i)
	{
		Copy.erase(Keys[i]);
	}
}