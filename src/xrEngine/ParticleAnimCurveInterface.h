#pragma once

namespace PS
{
    class ENGINE_API IPAC
    {
    public:

        virtual ~IPAC() = default;
    
        virtual Fvector4 GetValueOnIndex(int index) = 0;
        virtual Fvector4 GetValueOnTime(float time) = 0;
        virtual float GetMaxTime() const = 0;
        virtual float GetMinValue() const = 0;
        virtual float GetMaxValue() const = 0;

        /*
         *  Based on current index, current time, delta time and reverse + loop flag update current index and time and return current RGBA value
         *  Faster than externally calculate current time and search for value on time
         */
        virtual Fvector4 FastUpdateValue(size_t& CurrentIndex, float& CurrentTime, float dt, bool Loop, bool Reverse = false) = 0;
    };

    class ENGINE_API IPACLibrary
    {
    public:
        virtual ~IPACLibrary() = default;
        virtual PS::IPAC* FindIPAC(LPCSTR name) = 0;
    };

    class ENGINE_API CPACLibraryWrapper
    {
        IPACLibrary* PACLibrary = nullptr;

        CPACLibraryWrapper() = default;
        
    public:
        void SetPACLibrary(IPACLibrary* NewPACLibrary);
        IPACLibrary* GetPACLibrary(){return this->PACLibrary;}

        static CPACLibraryWrapper& GetInstance();

        CPACLibraryWrapper(const CPACLibraryWrapper&) = delete;
        CPACLibraryWrapper& operator=(const CPACLibraryWrapper&) = delete;
        CPACLibraryWrapper(CPACLibraryWrapper&&) = delete;
        CPACLibraryWrapper& operator=(CPACLibraryWrapper&&) = delete;
    };
}