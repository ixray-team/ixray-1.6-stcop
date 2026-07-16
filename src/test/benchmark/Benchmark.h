#pragma once
/*
====================================================
 IX-Ray Benchmark Framework
====================================================
 Author      : ForserX
 Description : Lightweight microbenchmark framework 
 Version     : 1.0
====================================================
*/

#include <algorithm>
#include <chrono>
#include <cfloat>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace benchmark
{

struct Result
{
	std::string Category;
	std::string Group;
	std::string Name;

	double TotalMs = 0.0;
	double AvgMs = 0.0;
	double MinMs = 0.0;
	double MaxMs = 0.0;

	uint64_t Iterations = 0;
};

using TestFunction = std::function<void()>;

struct Test
{
	std::string Category;
	std::string Group;
	std::string Name;

	TestFunction Function;
};

class Runner
{
public:
	static Runner& Instance()
	{
		static Runner Runner;
		return Runner;
	}

	void Register(const char* Category, const char* Group, const char* Name, TestFunction Function)
	{
		Tests.push_back({Category, Group, Name, std::move(Function)});
	}

	void RunAll(uint32_t Warmup = 3, uint32_t Iterations = 20)
	{
		std::vector<Result> Results;

		std::string CurrentCategory;

		for (auto& Test : Tests)
		{
			if (CurrentCategory.empty())
			{
				CurrentCategory = Test.Category;
			}

			if (Test.Category != CurrentCategory)
			{
				Print(CurrentCategory, Results);

				Results.clear();
				CurrentCategory = Test.Category;
			}

			Results.push_back(Run(Test, Warmup, Iterations));
		}

		if (!Results.empty())
		{
			Print(CurrentCategory, Results);
		}
	}

private:
	Result Run(Test& Test, uint32_t Warmup, uint32_t Iterations)
	{
		for (uint32_t i = 0; i < Warmup; ++i)
		{
			Test.Function();
		}

		Result Result;

		Result.Category = Test.Category;
		Result.Group = Test.Group;
		Result.Name = Test.Name;
		Result.Iterations = Iterations;
		Result.MinMs = DBL_MAX;

		for (uint32_t i = 0; i < Iterations; ++i)
		{
			auto Begin = Clock::now();

			Test.Function();

			auto End = Clock::now();

			double Ms = std::chrono::duration<double, std::milli>(End - Begin).count();

			Result.TotalMs += Ms;

			if (Ms < Result.MinMs)
			{
				Result.MinMs = Ms;
			}

			if (Ms > Result.MaxMs)
			{
				Result.MaxMs = Ms;
			}
		}

		Result.AvgMs = Result.TotalMs / Iterations;

		return Result;
	}

	void Print(const std::string& Category, const std::vector<Result>& Results)
	{
		std::map<std::string, std::vector<Result>> BenchmarkMap;
		std::set<std::string> Groups;

		for (const auto& Result : Results)
		{
			BenchmarkMap[Result.Name].push_back(Result);
			Groups.insert(Result.Group);
		}

		constexpr int NameWidth = 24;
		constexpr int ColumnWidth = 18;

		std::cout << "=========================================================================\n";
		std::cout << Category << '\n';

		std::cout << std::left << std::setw(NameWidth) << "Benchmark";

		for (const auto& Group : Groups)
		{
			std::cout << std::setw(ColumnWidth) << Group;
		}

		std::cout << "Winner\n";
		std::cout << "-------------------------------------------------------------------------\n";

		for (auto& [BenchmarkName, BenchmarkResults] : BenchmarkMap)
		{
			std::sort
			(
				BenchmarkResults.begin(),
				BenchmarkResults.end(),
				[](const Result& A, const Result& B)
				{
					return A.Group < B.Group;
				}
			);

			const Result* Winner = &BenchmarkResults.front();

			for (const auto& Result : BenchmarkResults)
			{
				if (Result.AvgMs < Winner->AvgMs)
				{
					Winner = &Result;
				}
			}

			std::cout << std::left << std::setw(NameWidth) << BenchmarkName;

			for (const auto& Group : Groups)
			{
				auto Iterator = std::find_if
				(
					BenchmarkResults.begin(),
					BenchmarkResults.end(),
					[&](const Result& Result)
					{
						return Result.Group == Group;
					}
				);

				if (Iterator == BenchmarkResults.end())
				{
					std::cout << std::setw(ColumnWidth) << "-";
					continue;
				}

				std::ostringstream Stream;
				Stream << std::fixed << std::setprecision(3) << Iterator->AvgMs << " ms";

				std::cout << std::setw(ColumnWidth) << Stream.str();
			}

			std::cout << Winner->Group << '\n';
		}

		std::cout << '\n';
	}

private:
	using Clock = std::chrono::high_resolution_clock;

	std::vector<Test> Tests;
};

class AutoRegister
{
public:
	AutoRegister(const char* Cat, const char* Group, const char* Name, TestFunction Function)
	{
		Runner::Instance().Register(Cat, Group, Name, std::move(Function));
	}
};

inline std::string GetCategory(const char* File)
{
	const char* Name = std::strrchr(File, '\\');

	if (!Name)
	{
		Name = std::strrchr(File, '/');
	}

	Name = Name ? Name + 1 : File;

	std::string Result(Name);

	size_t Dot = Result.find_last_of('.');

	if (Dot != std::string::npos)
	{
		Result.erase(Dot);
	}

	return Result;
}

} 

#define BENCHMARK_CAT2(A, B) A##B
#define BENCHMARK_CAT(A, B) BENCHMARK_CAT2(A, B)

#define BENCHMARK(Group, Name)                                         \
	static void BENCHMARK_CAT(Benchmark_, __LINE__)();                 \
	static benchmark::AutoRegister BENCHMARK_CAT(Register_, __LINE__)( \
		benchmark::GetCategory(__FILE__).c_str(),                      \
		#Group,                                                        \
		#Name,                                                         \
		BENCHMARK_CAT(Benchmark_, __LINE__)                            \
	);                                                                 \
	static void BENCHMARK_CAT(Benchmark_, __LINE__)()