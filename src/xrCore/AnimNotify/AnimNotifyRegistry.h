#pragma once
#include "AnimNotifyTypes.h"

class XRCORE_API CAnimNotifyRegistry
{
    xr_hash_map<shared_str, IAnimNotify*> map;
    
    CAnimNotifyRegistry();
public:
    
    static CAnimNotifyRegistry& GetInstance();
    
    bool contains(const shared_str& name);
    IAnimNotify* get(const shared_str& name);

    CAnimNotifyRegistry& operator=( const CAnimNotifyRegistry& ) = delete;
    CAnimNotifyRegistry( const CAnimNotifyRegistry& ) = delete;
    CAnimNotifyRegistry& operator=( CAnimNotifyRegistry&& ) = delete;
    CAnimNotifyRegistry(CAnimNotifyRegistry&& ) = delete;
};
