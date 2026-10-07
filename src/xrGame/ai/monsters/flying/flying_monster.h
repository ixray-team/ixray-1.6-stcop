#pragma once
#include "../basemonster/base_monster.h"
#include "flying_movement_controller.h"

// Adapts CBaseMonster without changing the ground monster implementation.
class CFlyingMonster : public CBaseMonster
{
    using inherited = CBaseMonster;
public:
    CFlyingMonster();
    ~CFlyingMonster() override;
    void Load(const char* section) override;
    void reinit() override;
    bool net_Spawn(CSE_Abstract* data) override;
    void net_Destroy() override;
    void UpdateCL() override;
	void spatial_move() override;
    bool ScriptCallbacksEnabled() const override { return false; }
    void shedule_Update(u32 dt) override;
    // Engine object-update policy: simulation must not depend on camera visibility.
    bool AlwaysTheCrow() override { return g_Alive(); }
    void Think() override {}
    void Exec_Action(float dt) override {}
    bool UsedAI_Locations() override { return false; }
    void Die(CObject* who) override;
    void net_Export(NET_Packet& packet) override;
    void save(NET_Packet& packet) override;
    void load(IReader& reader) override;
    void Serialize(ISaveObject& object) override;
    char* get_monster_class_name() override { return (char*)"flying_monster"; }

    const CFlyingMovementController& flight() const { return m_flight; }

protected:
	void SetNativeWander(bool Enabled) { m_wander = Enabled; }
    virtual float SpeciesBehaviorStep(float Dt) { return Dt; }
    virtual void update_species_behavior(float dt) {}
    virtual void save_species(NET_Packet& packet) {}
    virtual void load_species(IReader& reader, u8 version) {}
    CFlyingMovementController& flight_controller() { return m_flight; }
    virtual void update_flight_animation(float dt);

private:
	Fbox GroundSpatialCell;
	bool GroundSpatialCellValid = false;
	void RefreshGroundSpatialCell(const Fvector& RegisteredPosition);
    CFlyingMovementController m_flight;
    MotionID m_fly_motion, m_idle_motion;
    xr_vector<CBlend*> m_flight_blends;
    bool m_animation_flying = false;
    bool m_wander = false;
    float m_wander_radius = 40.f;
    float m_wander_height = 15.f;
    float m_wander_timer = 0.f;
    Fvector m_wander_origin = {0,0,0};
    u32 m_last_frame = u32(-1);
    bool m_restore_command = false;
    bool m_restore_landing = false;
    bool m_restore_landed = false;
    bool m_loaded_flight_data = false;
    Fvector m_restore_destination = {0,0,0};
    void choose_wander_destination();
    void play_flight_animation(bool flying);
    void capture_command();
    void restore_command();
};
