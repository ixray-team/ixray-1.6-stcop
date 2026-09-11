#pragma once
#include "StdAfx.h"
#include "Shell.h"

struct SShellManager
{
	static constexpr u32 MAX_SHELLS = 128;

	u32 head, tail, count;
	xr_vector<CShell*> managed_shells;

	SShellManager(): head(0u), tail(0u), count(0u)
	{
		managed_shells.resize(MAX_SHELLS);
	}

	void Push(CShell* shell)
	{
		if (count == MAX_SHELLS)
		{
			managed_shells[head]->DestroyObject();
			managed_shells[head] = nullptr;
			head = (head + 1u) % MAX_SHELLS;
			--count;
		}

		managed_shells[tail] = shell;
		tail = (tail + 1u) % MAX_SHELLS;
		++count;
	}
};