#pragma once

class IGame_Patrol
{
public:
    IGame_Patrol() = default;
    IGame_Patrol(const char* patrol) {};

    virtual ~IGame_Patrol() = default;

    virtual	const Fvector& point(u32 index) const = 0;
    virtual u32 point(const char* name) const = 0;
};