#pragma once
#include "smart_zone.h"
#include "space_restrictor.h"

class CDynamicWallmarkRegistry
{	
    CDynamicWallmarkRegistry();

    xr_hash_map<shared_str, xr_hash_map<shared_str, FactoryPtr<IWallMarkArray>>> registry;

public:

    CDynamicWallmarkRegistry(const CDynamicWallmarkRegistry&) = delete;
    CDynamicWallmarkRegistry& operator=(const CDynamicWallmarkRegistry&) = delete;
    CDynamicWallmarkRegistry(CDynamicWallmarkRegistry&&) = delete;
    CDynamicWallmarkRegistry& operator=(CDynamicWallmarkRegistry&&) = delete;
	
    static CDynamicWallmarkRegistry& Instance();

    wm_shader GetWallmarkShader(shared_str shader, shared_str texture);
	void ClearWallmarks();
};

class CDynamicWallmarkZone final :
    public CSmartZone 
{
private:
    using inherited = CSmartZone;

    struct rq_data
    {
        Fvector StartPos;
        Fvector Dir;
        CDynamicWallmarkZone* self;
    };

    static bool trace_callback(const collide::rq_result& result, LPVOID params);
    static bool test_callback(const collide::ray_defs& rd, CObject* object, LPVOID params);

protected:

    shared_str shader = "effects\\wallmark";
    shared_str texture = "";
    float h = 1.0f, w = 1.0f, r = 0.0f;

    bool CurrentStatus = false;
	StaticWallmarkHandle::WallmarkHandlePtr handler = nullptr;

public:

    virtual bool					net_Spawn(CSE_Abstract* DC) override;
    
    virtual void save (NET_Packet &output_packet) override;
    virtual void load (IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

    void SwitchWallmark(bool isOn);

};