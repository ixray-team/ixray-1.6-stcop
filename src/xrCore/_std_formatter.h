#pragma once

//////////////////////////////////////////////
// Math: Vectors 

template<class T, class CharT>
struct std::formatter<_vector3<T>, CharT> : std::formatter<T, CharT>
{
    template<class FormatContext>
    auto format(_vector3<T> t, FormatContext& fc) const
    {
        auto Iter = std::format_to(fc.out(), "[");
        fc.advance_to(Iter);

        Iter = std::formatter<T>::format(t.x, fc);
        Iter = std::format_to(Iter, ", ");
        fc.advance_to(Iter);

        Iter = std::formatter<T>::format(t.y, fc);
        Iter = std::format_to(Iter, ", ");
        fc.advance_to(Iter);

        Iter = std::formatter<T>::format(t.z, fc);
        Iter = std::format_to(Iter, "]");
        fc.advance_to(Iter);

        return Iter;
    }
};

template<class T, class CharT>
struct std::formatter<_vector2<T>, CharT> : std::formatter<T, CharT>
{
    template<class FormatContext>
    auto format(_vector2<T> t, FormatContext& fc) const
    {
        auto Iter = std::format_to(fc.out(), "[");
        fc.advance_to(Iter);

        Iter = std::formatter<T>::format(t.x, fc);
        Iter = std::format_to(Iter, ", ");
        fc.advance_to(Iter);

        Iter = std::formatter<T>::format(t.y, fc);
        Iter = std::format_to(Iter, "]");
        fc.advance_to(Iter);

        return Iter;
    }
};


//////////////////////////////////////////////
// Math: Matrix 

template<class T, class CharT>
struct std::formatter<_matrix33<T>, CharT> : std::formatter<T, CharT>
{
    template<class FormatContext>
    auto format(_matrix33<T> t, FormatContext& fc) const
    {
        std::formatter<_vector3<T>> vector_formatter;
        auto Iter = vector_formatter.format(t.i, fc);
        Iter = std::format_to(Iter, "\r\n");
        fc.advance_to(Iter);

        Iter = vector_formatter.format(t.j, fc);
        Iter = std::format_to(Iter, "\r\n");
        fc.advance_to(Iter);

        Iter = vector_formatter.format(t.k, fc);
        fc.advance_to(Iter);

        return Iter;
    }
};

template<class T, class CharT>
struct std::formatter<_matrix<T>, CharT> : std::formatter<T, CharT>
{
    template<class FormatContext>
    auto format(_matrix<T> m, FormatContext& fc) const
    {
        for (size_t VecID = 0; VecID < 4; VecID++)
        {
            auto Iter = std::format_to(fc.out(), "[");
            fc.advance_to(Iter);

            Iter = std::formatter<T>::format(m.m[VecID][0], fc);
            Iter = std::format_to(Iter, ", ");
            fc.advance_to(Iter);

            Iter = std::formatter<T>::format(m.m[VecID][1], fc);
            Iter = std::format_to(Iter, ", ");
            fc.advance_to(Iter);

            Iter = std::formatter<T>::format(m.m[VecID][2], fc);
            Iter = std::format_to(Iter, ", ");
            fc.advance_to(Iter);

            Iter = std::formatter<T>::format(m.m[VecID][3], fc);
            Iter = std::format_to(Iter, "]\r\n");
            fc.advance_to(Iter);
        }

        return fc.out();
    }
};