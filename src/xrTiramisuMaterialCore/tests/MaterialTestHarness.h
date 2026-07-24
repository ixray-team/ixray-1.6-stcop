#pragma once

#include "MaterialTypes.h"

#include <cstdlib>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

class TiramisuMaterialTestRunner
{
public:
    explicit TiramisuMaterialTestRunner(xr_string InSuiteName) : SuiteName(std::move(InSuiteName)) {}

    void Check(const bool Condition, const xr_string_view Expression, const xr_string_view File, const int Line)
    {
        ++CheckCount;
        if (Condition)
            return;

        ++FailureCount;
        std::cerr << File << ':' << Line << ": check failed: " << Expression << '\n';
    }

    [[nodiscard]] int Finish() const
    {
        if (FailureCount != 0)
        {
            std::cerr << SuiteName << ": " << FailureCount << " of " << CheckCount << " checks failed.\n";
            return EXIT_FAILURE;
        }

        std::cout << SuiteName << ": " << CheckCount << " checks passed.\n";
        return EXIT_SUCCESS;
    }

private:
    xr_string SuiteName;
    int CheckCount = 0;
    int FailureCount = 0;
};

inline bool HasDiagnostic(const xr_vector<FMaterialDiagnostic>& Diagnostics, const xr_string_view Code)
{
    for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
        if (Diagnostic.Code == Code)
            return true;
    return false;
}

#define MATERIAL_CHECK(Runner, Expression) (Runner).Check((Expression), #Expression, __FILE__, __LINE__)
#include <utility>
