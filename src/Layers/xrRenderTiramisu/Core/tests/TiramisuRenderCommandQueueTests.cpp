#include "RenderCommandQueue.h"

#include <cstdlib>
#include <iostream>
#include <thread>
#include <vector>

using Tiramisu::RenderCommands::TiramisuRenderCommandQueue;

namespace
{
int Failures = 0;

void Check(const bool Condition, const char* Expression, const int Line)
{
    if (Condition) return;
    ++Failures;
    std::cerr << "line " << Line << ": check failed: " << Expression << '\n';
}

#define QUEUE_CHECK(Expression) Check((Expression), #Expression, __LINE__)
}

int main()
{
    TiramisuRenderCommandQueue Queue;
    xr_vector<int> Order;
    Queue.Enqueue("first", [&Order] { Order.push_back(1); });
    Queue.Enqueue("second", [&Order] { Order.push_back(2); });
    QUEUE_CHECK(!Queue.Empty());
    Queue.Execute();
    QUEUE_CHECK((Order == xr_vector<int>{1, 2}));
    QUEUE_CHECK(Queue.Empty());

    Queue.Enqueue("outer", [&Queue, &Order]
    {
        Order.push_back(3);
        Queue.Enqueue("next-drain", [&Order] { Order.push_back(4); });
    });
    Queue.Execute();
    QUEUE_CHECK((Order == xr_vector<int>{1, 2, 3}));
    QUEUE_CHECK(!Queue.Empty());
    Queue.Execute();
    QUEUE_CHECK((Order == xr_vector<int>{1, 2, 3, 4}));

    Queue.Enqueue("discarded", [&Order] { Order.push_back(99); });
    Queue.Clear();
    Queue.Execute();
    QUEUE_CHECK((Order == xr_vector<int>{1, 2, 3, 4}));

    constexpr int ProducerCount = 4;
    constexpr int CommandsPerProducer = 64;
    xr_vector<std::thread> Producers;
    for (int Producer = 0; Producer < ProducerCount; ++Producer)
    {
        Producers.emplace_back([&Queue]
        {
            for (int Index = 0; Index < CommandsPerProducer; ++Index)
                Queue.Enqueue("concurrent", [] {});
        });
    }
    for (std::thread& Producer : Producers)
        Producer.join();
    QUEUE_CHECK(static_cast<int>(Queue.Drain().size()) == ProducerCount * CommandsPerProducer);
    QUEUE_CHECK(Queue.Empty());

    if (Failures != 0)
        return EXIT_FAILURE;
    std::cout << "Tiramisu render-command queue tests passed.\n";
    return EXIT_SUCCESS;
}
