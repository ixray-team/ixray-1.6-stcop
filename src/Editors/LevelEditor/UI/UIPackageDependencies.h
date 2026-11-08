#pragma once

#include "../Packages/PackageDependencies.h"

namespace PackageDependenciesUI
{
void Draw(const SdkPackages::DependencyReport& Report, bool IsCurrent, const xr_string& Error, bool& ShowOnlyProblems, float Height);
}
