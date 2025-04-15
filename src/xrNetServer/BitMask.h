#pragma once

template<typename Type>
class BitMask
{
public:
    explicit BitMask(Type value = 0) : bits(value) {}

    template <typename T>
    void set(std::size_t index, T value)
    {
        bits.set(index, value);
    }

    template <typename T>
    T get(size_t index) const
    {
        return static_cast<T>(bits.test(index));
    }

private:
    std::bitset<sizeof(Type) * 8> bits;
};

using xr_bitmask_8 = BitMask<u32>;