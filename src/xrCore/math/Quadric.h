#pragma once

class XRCORE_API MxVector
{
public:
    union
    {
        struct 
        {
            float x, y, z, w, d;
        };
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
        VERIFY(index < 5 && "Index out of bounds");
        return data[index];
    }

    const float& operator[](size_t index) const
    {
        VERIFY(index < 5 && "Index out of bounds");
        return data[index];
    }
};

class XRCORE_API MxMatrix
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

    static void SymmetricSubFrom(MxMatrix& A, const MxVector& a, const MxVector& b);
};

class XRCORE_API MxQuadric
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

    MxQuadric(const MxVector& p1, const MxVector& p2, const MxVector& p3, float areaVal);

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
