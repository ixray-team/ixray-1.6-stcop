#pragma once
#include "../../xrParticles/ParticleAnimCurveInterface.h"

namespace PS
{
    struct st_PACKey
    {
        Fvector4 value;
        float time;
    };

    class ECORE_API CPACDef : public IPAC
    {
        shared_str m_Name;
        xr_vector<st_PACKey*> m_Keys;
        float m_MaxTime = 0.0f;
        float m_MinValue = 0.0f;
        float m_MaxValue = 1.0f;

        Fvector4 CalculateIntermedialeValue(st_PACKey* A, st_PACKey* B, float time);

    public:
        ~CPACDef();

        IC LPCSTR getName() const {return m_Name.c_str();}
        void setName(LPCSTR name);
        
        void Save(IWriter& F);
        bool Load(IReader& F);
        
        void Save2(CInifile& ini);
        bool Load2(CInifile& ini);

        virtual Fvector4 GetValueOnIndex(int index) override ;
        virtual Fvector4 GetValueOnTime(float time) override ;
        virtual float GetMaxTime() const override { return m_MaxTime; }
        virtual float GetMinValue() const override { return m_MinValue; }
        virtual float GetMaxValue() const override { return m_MaxValue; }

        /*
         *  Based on current index, current time, delta time and reverse + loop flag update current index and time and return current RGBA value
         *  Faster than externally calculate current time and search for value on time
         */
        virtual Fvector4 FastUpdateValue(size_t& CurrentIndex, float& CurrentTime, float dt, bool Loop, bool Reverse = false) override;
        
#ifdef _EDITOR
        void Clone(CPACDef* source);
		void FillProp(LPCSTR pref, PropItemVec& items, void* owner);
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
