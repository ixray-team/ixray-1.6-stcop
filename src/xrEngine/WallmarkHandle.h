#pragma once

namespace StaticWallmarkHandle {
    enum
    {
        flTimeToLive = 1 << 0,
        flHandler = 1 << 1,
        flForceRemove = 1 << 2,
        flForceSpawn = 1 << 3,
    };
    
    class ENGINE_API CWallmarkHandle{
        Flags8* HandledWallmarkFlags = nullptr;
    public:
        CWallmarkHandle(Flags8* HandledWallmarkFlags) : HandledWallmarkFlags(HandledWallmarkFlags) {}

        IC void Reset(){HandledWallmarkFlags = nullptr;}
        IC bool IsValid(){return HandledWallmarkFlags;}
        IC void Destroy(){VERIFY(HandledWallmarkFlags); HandledWallmarkFlags->set(flForceRemove, true);}
    };

    using WallmarkHandlePtr = xr_shared_ptr<CWallmarkHandle>;
}