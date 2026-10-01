#ifndef BLOOM_LUM_HISTOGRAM_H
#define BLOOM_LUM_HISTOGRAM_H

#include "autoexposure.hlsli"

#define HISTOGRAM_BINS 256
#define HistogramMinEV100 autoexposure_metering.y
#define HistogramMaxEV100 autoexposure_metering.z
static const float HistogramLowPercent = 0.05f;
static const float HistogramHighPercent = 0.95f;

#endif
