#include "stdafx.h"
#include "ParticleAnimCurveInterface.h"

void PS::CPACLibraryWrapper::SetPACLibrary(IPACLibrary* NewPACLibrary)
{
    R_ASSERT2(!NewPACLibrary || !PACLibrary, "Attempt to init PACLibrary while other present!");
    PACLibrary = NewPACLibrary;
}

PS::CPACLibraryWrapper& PS::CPACLibraryWrapper::GetInstance()
{
    static PS::CPACLibraryWrapper instance;
    return instance;
}
