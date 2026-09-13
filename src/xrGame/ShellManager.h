#pragma once
#include "StdAfx.h"
#include "Shell.h"

struct SShellManager
{
	static constexpr int MAX_SHELLS = 128;

	int head, tail, count;
	xr_vector<CShell*> managed_shells;

	SShellManager(): head(0), tail(0), count(0)
	{
		managed_shells.resize(MAX_SHELLS);
	}

	void Push(CShell* shell)
	{
		if (count == MAX_SHELLS)
		{
			managed_shells[head]->DestroyObject();
			managed_shells[head] = nullptr;
			head = (head + 1) % MAX_SHELLS;
			--count;
		}

		managed_shells[tail] = shell;
		tail = (tail + 1) % MAX_SHELLS;
		++count;
	}
	
	void net_Relcase(const CObject* to_remove)
	{
		for (auto& managed_shell : managed_shells)
		{
			if (managed_shell == to_remove)
			{
				managed_shell = nullptr;
				clamp(--head, 0, MAX_SHELLS);
				clamp(--tail, 0, MAX_SHELLS);
				clamp(--count, 0, MAX_SHELLS);
			}
		}
	}
};