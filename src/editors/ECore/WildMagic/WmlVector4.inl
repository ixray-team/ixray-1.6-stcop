// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2004.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 ()
{
    // the vector is uninitialized
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 (Real fX, Real fY, Real fZ, Real fW)
{
    this->m_afTuple[0] = fX;
    this->m_afTuple[1] = fY;
    this->m_afTuple[2] = fZ;
    this->m_afTuple[3] = fW;
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 (const Vector4& rkV)
{
    memcpy(this->m_afTuple,rkV.m_afTuple,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 (const Vector<4,Real>& rkV)
{
    memcpy(this->m_afTuple,(const Real*)rkV,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>& Vector4<Real>::operator= (const Vector4& rkV)
{
    memcpy(this->m_afTuple,rkV.m_afTuple,4*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>& Vector4<Real>::operator= (const Vector<4,Real>& rkV)
{
    memcpy(this->m_afTuple,(const Real*)rkV,4*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::X () const
{
    return this->m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::X ()
{
    return this->m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::Y () const
{
    return this->m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::Y ()
{
    return this->m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::Z () const
{
    return this->m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::Z ()
{
    return this->m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::W () const
{
    return this->m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::W ()
{
    return this->m_afTuple[3];
}
//----------------------------------------------------------------------------
