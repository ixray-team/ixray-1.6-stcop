#pragma once

class IGame_Patrol
{
public:
    IGame_Patrol() = default;
    IGame_Patrol(const char* patrol) {};
    virtual	const Fvector& point(u32 index) const = 0;
    virtual u32 point(LPCSTR name) const = 0;
};