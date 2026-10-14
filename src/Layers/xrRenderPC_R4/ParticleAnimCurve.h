#pragma once
#include "RenderAPI.h"
#include "../../xrEngine/ParticleAnimCurveInterface.h"

namespace PS
{
    struct st_PACKey
    {
        Fvector4 value;
        float time;
    };

    // Exported per member: the Particle Editor defines the editor-only members (declared under _EDITOR) in its own module.
    class CPACDef : public IPAC
    {
        shared_str m_Name;
        xr_vector<st_PACKey*> m_Keys;
        float m_MaxTime = 0.0f;
        float m_MinValue = 0.0f;
        float m_MaxValue = 1.0f;

        RENDER_API Fvector4 CalculateIntermedialeValue(st_PACKey* A, st_PACKey* B, float time);

    public:
        RENDER_API ~CPACDef();

        IC const char* getName() const {return m_Name.c_str();}
        RENDER_API void setName(const char* name);
        
        RENDER_API void Save(IWriter& F);
        RENDER_API bool Load(IReader& F);
        
        RENDER_API void Save2(CInifile& ini);
        RENDER_API bool Load2(CInifile& ini);

        RENDER_API virtual Fvector4 GetValueOnIndex(int index) override ;
        RENDER_API virtual Fvector4 GetValueOnTime(float time) override ;
        virtual float GetMaxTime() const override { return m_MaxTime; }
        virtual float GetMinValue() const override { return m_MinValue; }
        virtual float GetMaxValue() const override { return m_MaxValue; }

        /*
         *  Based on current index, current time, delta time and reverse + loop flag update current index and time and return current RGBA value
         *  Faster than externally calculate current time and search for value on time
         */
        RENDER_API virtual Fvector4 FastUpdateValue(size_t& CurrentIndex, float& CurrentTime, float dt, bool Loop, bool Reverse = false) override;
        
        RENDER_API void Clone(CPACDef* source);
#ifdef _EDITOR
		void FillProp(const char* pref, PropItemVec& items, void* owner);
        void OnEditClicked(ButtonValue* B, bool& bModif, bool&);
		bool Validate(bool bMsg);

        void SplitKeysForPlot(
            xr_vector<float>& R_keys_y,
            xr_vector<float>& G_keys_y,
            xr_vector<float>& B_keys_y,
            xr_vector<float>& A_keys_y,
            xr_vector<float>& keys_x
            );
        void UpdateCurveFromKeys(
            const xr_vector<float>& R_keys_y,
            const xr_vector<float>& G_keys_y,
            const xr_vector<float>& B_keys_y,
            const xr_vector<float>& A_keys_y,
            const xr_vector<float>& keys_x
        );
#endif
    };

    namespace PAC
    {
        enum class Version: u16
        {
            Original = 0x0001,
            MAX,
            Latest = MAX - 1,
        };

        enum class Chunks: u32
        {
            VERSION = 0x0001,
            NAME,
            TIME_MAX,
            FLAGS,
            KEYS_NUM,
            KEYS,
        };
    }
}
