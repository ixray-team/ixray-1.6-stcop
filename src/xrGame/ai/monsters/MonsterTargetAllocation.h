#pragma once

class CMonsterTargetAllocation
{
public:
	static constexpr u32 MaxMembers = 32;
	static constexpr u32 MaxTargets = 24;
	float Distance[MaxMembers][MaxTargets] = {};
	u32 Load[MaxTargets] = {};
	u32 Target[MaxMembers] = {};
	bool Fixed[MaxMembers] = {};

	constexpr CMonsterTargetAllocation()
	{
		for (u32 Member = 0; Member < MaxMembers; ++Member)
		{
			Target[Member] = u32(-1);
			for (float& Value : Distance[Member])
			{
				Value = -1.f;
			}
		}
	}

	constexpr void Distribute(u32 MemberCount, u32 TargetCount)
	{
		if (MemberCount > MaxMembers || TargetCount > MaxTargets)
		{
			return;
		}
		for (u32 Round = 0; Round < MemberCount; ++Round)
		{
			u32 BestMember = u32(-1), BestTarget = u32(-1);
			for (u32 Member = 0; Member < MemberCount; ++Member)
			{
				if (Fixed[Member] || Target[Member] != u32(-1))
				{
					continue;
				}
				for (u32 Enemy = 0; Enemy < TargetCount; ++Enemy)
				{
					const float Range = Distance[Member][Enemy];
					if (!(Range >= 0.f))
					{
						continue;
					}
					if (BestTarget == u32(-1) || Load[Enemy] < Load[BestTarget] ||
						(Load[Enemy] == Load[BestTarget] && Range < Distance[BestMember][BestTarget]))
					{
						BestMember = Member;
						BestTarget = Enemy;
					}
				}
			}
			if (BestTarget == u32(-1))
			{
				break;
			}
			Target[BestMember] = BestTarget;
			++Load[BestTarget];
		}
	}
};
