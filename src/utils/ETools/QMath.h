#include <cassert>
#include <cmath>

class MxVector
{
public:
    union {
        struct { float x, y, z, w, d; };
        float data[5];
    };

    MxVector()
    {
        for (int i = 0; i < 5; ++i)
            data[i] = 0.0f;
    }

    MxVector(float x, float y, float z, float w, float d)
        : x(x), y(y), z(z), w(w), d(d) {
    }

    MxVector operator+(const MxVector& other) const
    {
        return MxVector(x + other.x, y + other.y, z + other.z, w + other.w, d + other.d);
    }

    MxVector operator-(const MxVector& other) const
    {
        return MxVector(x - other.x, y - other.y, z - other.z, w - other.w, d - other.d);
    }

    MxVector operator*(float scalar) const
    {
        return MxVector(x * scalar, y * scalar, z * scalar, w * scalar, d * scalar);
    }

    float dot(const MxVector& other) const
    {
        float result = 0.0f;
        for (int i = 0; i < 5; ++i)
            result += data[i] * other.data[i];
        return result;
    }

    float norm() const
    {
        return std::sqrt(this->dot(*this));
    }

    float& operator[](size_t index)
    {
        assert(index < 5 && "Index out of bounds");
        return data[index];
    }

    const float& operator[](size_t index) const
    {
        assert(index < 5 && "Index out of bounds");
        return data[index];
    }
};

class MxMatrix
{
public:
    MxVector i, j, k, l, m;

    MxMatrix() : i(), j(), k(), l(), m() {}

    MxMatrix operator+(const MxMatrix& other) const
    {
        MxMatrix result;
        result.i = i + other.i;
        result.j = j + other.j;
        result.k = k + other.k;
        result.l = l + other.l;
        result.m = m + other.m;
        return result;
    }

    MxMatrix operator*(float scalar) const
    {
        MxMatrix result;
        result.i = i * scalar;
        result.j = j * scalar;
        result.k = k * scalar;
        result.l = l * scalar;
        result.m = m * scalar;
        return result;
    }

    friend MxMatrix operator*(float scalar, const MxMatrix& matrix)
    {
        return matrix * scalar;
    }

    float& operator()(size_t row, size_t col)
    {
        assert(row < 5 && col < 5 && "Index out of bounds");
        return (&i)[row][col];
    }

    const float& operator()(size_t row, size_t col) const
    {
        assert(row < 5 && col < 5 && "Index out of bounds");
        return (&i)[row][col];
    }

    MxVector operator*(const MxVector& vec) const
    {
        MxVector result;
        result.x = i.dot(vec);
        result.y = j.dot(vec);
        result.z = k.dot(vec);
        result.w = l.dot(vec);
        result.d = m.dot(vec);
        return result;
    }

    static void SymmetricSubFrom(MxMatrix& A, const MxVector& a, const MxVector& b)
    {
        for (unsigned int i = 0; i < 5; i++)
        {
            for (unsigned int j = 0; j < 5; j++)
            {
                A(i, j) -= a[i] * b[j];
            }
        }
    }
};

class MxQuadric
{
public:
    MxMatrix matrix;
    MxVector vector;
    float scalar;
    float area;

    MxQuadric() : scalar(0.0f), area(0.0f) {}

    MxQuadric(const MxMatrix& mat, const MxVector& vec, float scl)
        : matrix(mat), vector(vec), scalar(scl), area(0.0f) {
    }

    MxQuadric(const MxVector& p1, const MxVector& p2, const MxVector& p3, float areaVal)
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

        for (unsigned int i = 0; i < 5; i++)
        {
            matrix(i, i) = 1.0f;
        }

        MxMatrix::SymmetricSubFrom(matrix, e1, e1);
        MxMatrix::SymmetricSubFrom(matrix, e2, e2);

        vector = (e1 * p1e1) + (e2 * p1e2) - p1;
        scalar = p1.dot(p1) - (p1e1 * p1e1) - (p1e2 * p1e2);
    }

    MxQuadric operator+(const MxQuadric& other) const
    {
        return MxQuadric(matrix + other.matrix, vector + other.vector, scalar + other.scalar);
    }

    MxQuadric& operator+=(const MxQuadric& other)
    {
        matrix = matrix + other.matrix;
        vector = vector + other.vector;
        scalar += other.scalar;
        area += other.area;
        return *this;
    }

    float evaluate(const MxVector& v) const
    {
        float term1 = v.dot(matrix * v);
        float term2 = 2 * vector.dot(v);
        return term1 + term2 + scalar;
    }
};
