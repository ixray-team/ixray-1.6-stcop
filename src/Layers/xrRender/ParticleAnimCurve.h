#pragma once
#include "../../xrEngine/envelope.h"

namespace PS
{
    struct st_PACKey
    {
        Fvector4 value;
        float time;
    };

    class ECORE_API CPACDef
    {
        shared_str m_Name;
        xr_vector<st_PACKey*> m_Keys;
        float m_MaxTime = 0.0f;

    public:
        ~CPACDef();

        IC LPCSTR getName() const {return m_Name.c_str();}
        void setName(LPCSTR name);
        
        void Save(IWriter& F);
        bool Load(IReader& F);
        
        void Save2(CInifile& ini);
        bool Load2(CInifile& ini);

        Fvector4 GetValueOnIndex(int index);
        Fvector4 GetValueOnTime(float time);
        IC float GetMaxTime() const { return m_MaxTime; }
        
#ifdef _EDITOR
        void Clone(CPACDef* source);
		void FillProp(LPCSTR pref, PropItemVec& items, void* owner);
        void OnEditClicked(ButtonValue* B, bool& bModif, bool&);
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
