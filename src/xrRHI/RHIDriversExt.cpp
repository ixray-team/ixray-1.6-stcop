#include "RHI.h"
#include "RHIDriversExt.h"

// nts
#include "Drivers/IntelGPUTransferee.h"

bool CIntelReader::SetDepthBounds(bool enable, float zMin, float zMax)
{
    return GRHI->APILevel == ERHI_API_LAYER::D3D12 &&
        GRHI->DevicePtr->SetDepthBounds(enable, zMin, zMax);
}
