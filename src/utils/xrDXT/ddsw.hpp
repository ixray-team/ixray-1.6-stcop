#pragma once
#include <Windows.h>
#include <nvtt/nvtt.h>
#include "dds.h"

class DDSWriter : public nvtt::OutputHandler
{
public:
	HFILE& file;

	DDSWriter(HFILE& file) : file(file) {};
	virtual void beginImage(int size, int width, int height, int depth, int face, int miplevel) override {}
	virtual bool writeData(const void* data, int size) override;
	virtual void endImage() override {};
};


class DDSErrorHandler : public nvtt::ErrorHandler
{
public:
	virtual void error(nvtt::Error e) override;
};