#pragma once

struct WAVHeader
{
	char riff[4];
	u32 overall_size;
	char wave[4];
	char fmt_chunk_marker[4];
	u32 length_of_fmt;
	u16 format_type;
	u16 channels;
	u32 sample_rate;
	u32 byterate;
	u16 block_align;
	u16 bits_per_sample;
	char data_chunk_header[4];
	u32 data_size;
};

namespace XRay::Importer::Audio
{
	ECORE_API bool ImportWav(shared_str Path, shared_str Out, float Quality, u8* Comment, size_t CommentLen);
}