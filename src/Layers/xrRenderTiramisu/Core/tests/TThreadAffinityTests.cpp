#include "Core/TThreadAffinity.h"

#include <cstdlib>
#include <iostream>

using namespace Tiramisu::Threading;

namespace
{
int Failures = 0;

void Check(const bool Condition, const char* Expression, const int Line)
{
    if (Condition) return;
    ++Failures;
    std::cerr << "line " << Line << ": check failed: " << Expression << '\n';
}

#define THREAD_CHECK(Expression) Check((Expression), #Expression, __LINE__)
}

int main()
{
    constexpr size_t GameThread = 11;
    constexpr size_t RenderThread = 22;
    constexpr size_t WorkerThread = 33;

    THREAD_CHECK(IsThreadRoleSatisfied(
        EThreadRole::Game, false, GameThread, GameThread, GameThread));
    THREAD_CHECK(IsThreadRoleSatisfied(
        EThreadRole::Render, false, GameThread, GameThread, GameThread));
    THREAD_CHECK(!IsThreadRoleSatisfied(
        EThreadRole::Game, false, WorkerThread, GameThread, GameThread));
    THREAD_CHECK(!IsThreadRoleSatisfied(
        EThreadRole::Render, false, WorkerThread, GameThread, GameThread));

    THREAD_CHECK(IsThreadRoleSatisfied(
        EThreadRole::Game, true, GameThread, GameThread, RenderThread));
    THREAD_CHECK(!IsThreadRoleSatisfied(
        EThreadRole::Render, true, GameThread, GameThread, RenderThread));
    THREAD_CHECK(IsThreadRoleSatisfied(
        EThreadRole::Render, true, RenderThread, GameThread, RenderThread));
    THREAD_CHECK(!IsThreadRoleSatisfied(
        EThreadRole::Game, true, RenderThread, GameThread, RenderThread));
    THREAD_CHECK(!IsThreadRoleSatisfied(
        EThreadRole::Game, true, WorkerThread, GameThread, RenderThread));
    THREAD_CHECK(!IsThreadRoleSatisfied(
        EThreadRole::Render, true, WorkerThread, GameThread, RenderThread));

    if (Failures != 0)
        return EXIT_FAILURE;
    std::cout << "Tiramisu thread-affinity contract tests passed.\n";
    return EXIT_SUCCESS;
}
