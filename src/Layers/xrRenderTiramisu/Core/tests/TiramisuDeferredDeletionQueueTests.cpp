#include "Core/TiramisuDeferredDeletionQueue.h"

#include <cstdlib>
#include <iostream>
#include <vector>

namespace
{
int Failures = 0;

void Check(const bool Condition, const char* Expression, const int Line)
{
	if (Condition)
	{
		return;
	}
	++Failures;
	std::cerr << "line " << Line << ": check failed: " << Expression << '\n';
}

#define DELETE_CHECK(Expression) Check((Expression), #Expression, __LINE__)
} // namespace

int main()
{
	Tiramisu::TiramisuDeferredDeletionQueue Queue;
	xr_vector<int> Deleted;

	DELETE_CHECK(!Queue.Enqueue(1, {}));
	DELETE_CHECK(Queue.Enqueue(3, [&]
							   { Deleted.push_back(30); }));
	DELETE_CHECK(Queue.Enqueue(1, [&]
							   { Deleted.push_back(10); }));
	DELETE_CHECK(Queue.Enqueue(3, [&]
							   { Deleted.push_back(31); }));
	DELETE_CHECK(Queue.Size() == 3);

	DELETE_CHECK(Queue.Collect(0) == 0);
	DELETE_CHECK(Deleted.empty());
	DELETE_CHECK(Queue.Collect(1) == 1);
	DELETE_CHECK((Deleted == xr_vector<int>{10}));
	DELETE_CHECK(Queue.Collect(2) == 0);
	DELETE_CHECK(Queue.Collect(3) == 2);
	DELETE_CHECK((Deleted == xr_vector<int>{10, 30, 31}));
	DELETE_CHECK(Queue.Empty());

	DELETE_CHECK(Queue.Enqueue(9, [&]
							   { Deleted.push_back(90); }));
	DELETE_CHECK(Queue.Enqueue(7, [&]
							   { Deleted.push_back(70); }));
	DELETE_CHECK(Queue.Flush() == 2);
	DELETE_CHECK((Deleted == xr_vector<int>{10, 30, 31, 70, 90}));
	DELETE_CHECK(Queue.Empty());

	return Failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
