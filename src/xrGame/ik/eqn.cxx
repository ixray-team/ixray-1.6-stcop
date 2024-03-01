/*
  This source code is a part of IKAN.
  Copyright (c) 2000 University of Pennsylvania
  Center for Human Modeling and Simulation
  All Rights Reserved.

  IN NO EVENT SHALL THE UNIVERSITY OF PENNSYLVANIA BE LIABLE TO ANY
  PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
  DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS
  SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF PENNSYLVANIA
  HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

  Permission to use, copy, modify and distribute this software and its
  documentation for educational, research and non-profit purposes,
  without fee, and without a written agreement is hereby granted,
  provided that the above copyright notice and the following three
  paragraphs appear in all copies. For for-profit purposes, please
  contact University of Pennsylvania
 (http://hms.upenn.edu/software/ik/ik.html) for the software license
  agreement.


  THE UNIVERSITY OF PENNSYLVANIA SPECIFICALLY DISCLAIM ANY
  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS"
  BASIS, AND THE UNIVERSITY OF PENNSYLVANIA HAS NO OBLIGATION
  TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
  MODIFICATIONS.

 */
#include "stdafx.h"
#include "eqn.h"


/*
 * Put angle in range 0 .. 2*M_PI. Bounds on range of Psi 
 */
/*
const double LowBound  = 0;
const double HighBound = 2*M_PI;
const double TwoPi = 2*M_PI;


static double angle_normalize(double theta)
{
    while (theta < LowBound)
	theta += TwoPi;
    while (theta > HighBound)
	theta -= TwoPi;

    return theta;
}
*/

static int solve_trig1_aux(float c, 
			   float a2b2,
			   float atan2ba,
			   float theta[2])
{
    float temp  = a2b2-c*c;
    int num;

    if (temp < 0.0f)
	return 0;

    temp  = atan2(_sqrt(temp), c);
    num =  (_abs(temp) > 1e-6f) ? 2 : 1;

    theta[0] = atan2ba;
    if (num == 2)
    {
        theta[1] = theta[0] - temp;
        theta[0] += temp;

	//theta[0] = angle_normalize(theta[0]);
	//theta[1] = angle_normalize(theta[1]);

	if (theta[0] > theta[1])
	{
		swap(theta[0],theta[1]);
	//	temp = theta[0]; 
	//    theta[0] = theta[1];
	 //   theta[1] = temp;
	}
    }
    return num;
}



/*
 *  Solve a*cos(theta) + b*sin(theta) = c
 *  Either one or two solutions. Return the answer in radians.
 *  Also sort the answers in increasing order.
 */

static int local_solve_trig1(float a, float b, float c, float theta[2])
{
    return solve_trig1_aux(c, a*a+b*b, atan2(b,a), theta);
}

#define GOT_ROOTS (1)
#define GOT_CRITS (2)

//
// The critical points are where the derivative is 0
//
int PsiEquation::crit_points(float *t) const
{
    if (!(*status_ptr & GOT_CRITS))
    {
	// CANNOT use solve_trig1_aux here 
	*num_crits_ptr = (u8)local_solve_trig1(beta, -alpha, 0, (float *) crit_pts);
	*status_ptr |= GOT_CRITS;
    }

    switch(num_crits)
    {
    case 1:
	t[0] = crit_pts[0];
	break;
    case 2:
	t[0] = crit_pts[0];
	t[1] = crit_pts[1];
	break;
    default:
	break;
    }
    return num_crits;
}


//
// Return the roots of the equation
// 
int PsiEquation::roots(float *t) const
{
    if (!(*status_ptr & GOT_ROOTS))
    {
	*num_roots_ptr =(u8) solve_trig1_aux(-xi, a2b2, atan2ba, (float *) root_pts);
	*status_ptr  |= GOT_ROOTS;
    }

    switch(num_roots)
    {
    case 1:
	t[0] = root_pts[0];
	break;
    case 2:
	t[0] = root_pts[0];
	t[1] = root_pts[1];
	break;
    default:
	break;
    }
    return num_roots;
}

int PsiEquation::solve(float v, float *t) const
{
    // consistency_check(alpha,beta,-xi+v,a2b2,atan2ba);
    // return solve_trig1(alpha, beta, -xi+v, t);
    return solve_trig1_aux(-xi+v, a2b2, atan2ba, t);
}

/*
 * Returns the regions of intersections of 
 *
 *	a * cos(psi) + b*sin(psi) + c = low 
 * and
 *	a * cos(psi) + b*sin(psi) + c = high
 * 
 * from 0 to 3 possible regions
 *
 */