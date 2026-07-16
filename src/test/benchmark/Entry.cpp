#include "../../xrCore/stdafx.h"
#include "Benchmark.h"

void InitKeysAssociative();
void InitKeysSequence();

int main()
{
	std::cout << "=========================================================================\n";
	std::cout << "IX-Ray Benchmark\n";
	std::cout << "=========================================================================\n\n";

	InitKeysAssociative();
	InitKeysSequence();

	benchmark::Runner::Instance().RunAll();

	return 0;
}