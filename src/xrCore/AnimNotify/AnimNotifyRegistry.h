#pragma once

class XRCORE_API CAnimNotifyRegistry
{
public:
    
    struct SAnimNotifyInfo
    {
        shared_str GiveInfo = "";
        shared_str DisableInfo = "";
        shared_str Functor = "";
    };
    
    static CAnimNotifyRegistry& GetInstance();
    
    bool contains(const shared_str& name);
    const SAnimNotifyInfo& get(const shared_str& name);

    CAnimNotifyRegistry& operator=( const CAnimNotifyRegistry& ) = delete;
    CAnimNotifyRegistry( const CAnimNotifyRegistry& ) = delete;
    CAnimNotifyRegistry& operator=( CAnimNotifyRegistry&& ) = delete;
    CAnimNotifyRegistry(CAnimNotifyRegistry&& ) = delete;

private:
    xr_hash_map<shared_str, SAnimNotifyInfo> map;
    CAnimNotifyRegistry();
};
