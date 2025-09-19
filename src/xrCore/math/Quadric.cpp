#include "stdafx.h"
#include "Quadric.h"

void MxMatrix::SymmetricSubFrom(MxMatrix& A, const MxVector& a, const MxVector& b)
{
    for (u32 i = 0; i < 5; i++)
    {
        for (u32 j = 0; j < 5; j++)
        {
            A(i, j) -= a[i] * b[j];
        }
    }
}

MxQuadric::MxQuadric(const MxVector& p1, const MxVector& p2, const MxVector& p3, float areaVal) 
    : matrix(), vector(), scalar(0.0f), area(areaVal)
{
    MxVector e1 = p2 - p1;
    float e1Norm = e1.norm();
    if (e1Norm > 0) e1 = e1 * (1.0f / e1Norm);

    MxVector e2 = p3 - p1;
    float e2Proj = e1.dot(e2);
    e2 = e2 - (e1 * e2Proj);
    float e2Norm = e2.norm();
    if (e2Norm > 0) e2 = e2 * (1.0f / e2Norm);

    float p1e1 = p1.dot(e1);
    float p1e2 = p1.dot(e2);

    for (u32 i = 0; i < 5; i++)
    {
        matrix(i, i) = 1.0f;
    }

    MxMatrix::SymmetricSubFrom(matrix, e1, e1);
    MxMatrix::SymmetricSubFrom(matrix, e2, e2);

    vector = (e1 * p1e1) + (e2 * p1e2) - p1;
    scalar = p1.dot(p1) - (p1e1 * p1e1) - (p1e2 * p1e2);
}