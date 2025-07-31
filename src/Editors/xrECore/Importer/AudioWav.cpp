#include "stdafx.h"
#include "AudioWav.h"

#include <vorbis/vorbisfile.h>
#include <vorbis/vorbisenc.h>

struct WAVFormat
{
	uint16_t format_type;
	uint16_t channels;
	uint32_t sample_rate;
	uint16_t bits_per_sample;
	uint16_t block_align;
};

bool find_fmt_and_data_chunks(FILE* f, WAVFormat& fmt_out, uint32_t& data_offset, uint32_t& data_size)
{
	char RiffHeader[12] = {};
	if (fread(RiffHeader, 1, 12, f) != 12 || memcmp(RiffHeader, "RIFF", 4) != 0 || memcmp(RiffHeader + 8, "WAVE", 4) != 0)
	{
		return false;
	}

	bool found_fmt = false;
	bool found_data = false;

	while (!feof(f))
	{
		char chunk_id[4];
		uint32_t chunk_size = 0;
		if (fread(chunk_id, 1, 4, f) != 4 || fread(&chunk_size, 4, 1, f) != 1)
			break;

		if (memcmp(chunk_id, "fmt ", 4) == 0)
		{
			if (chunk_size < 16)
				return false;

			uint16_t format_type, channels, block_align, bits_per_sample;
			uint32_t sample_rate, byte_rate;

			if (fread(&format_type, sizeof(uint16_t), 1, f) != 1 ||
				fread(&channels, sizeof(uint16_t), 1, f) != 1 ||
				fread(&sample_rate, sizeof(uint32_t), 1, f) != 1 ||
				fread(&byte_rate, sizeof(uint32_t), 1, f) != 1 ||
				fread(&block_align, sizeof(uint16_t), 1, f) != 1 ||
				fread(&bits_per_sample, sizeof(uint16_t), 1, f) != 1)
				return false;

			if (chunk_size > 16)
			{
				fseek(f, chunk_size - 16, SEEK_CUR);
			}

			fmt_out = { format_type, channels, sample_rate, bits_per_sample, block_align };
			found_fmt = true;
		}
		else if (memcmp(chunk_id, "data", 4) == 0)
		{
			data_offset = ftell(f);
			data_size = chunk_size;
			found_data = true;
			break;
		}
		else
		{
			// Пропустить неизвестный чанк
			fseek(f, chunk_size, SEEK_CUR);
		}
	}

	return found_fmt && found_data;
}

bool XRay::Importer::Audio::ImportWav(shared_str Path, shared_str Out, float Quality, u8* Comment, size_t CommentLen)
{
	if (Quality < 0.5f)
	{
		Quality = 0.5f;
	}

	FILE* fin = fopen(*Path, "rb");
	if (!fin)
	{
		Msg("! Can't open input");
		return false;
	}

	WAVFormat fmt;
	uint32_t data_offset = 0, data_size = 0;
	if (!find_fmt_and_data_chunks(fin, fmt, data_offset, data_size))
	{
		Msg("! Invalid or unsupported WAV file");
		fclose(fin);
		return false;
	}

	if (fmt.format_type != 1 || fmt.bits_per_sample != 16)
	{
		Msg("! Only 16-bit PCM WAV supported");
		fclose(fin);
		return false;
	}

	fseek(fin, data_offset, SEEK_SET);


	FILE* fout = fopen(*Out, "wb");
	if (!fout)
	{
		fclose(fin);
		return false;
	}

	// Vorbis setup
	vorbis_info vi;
	vorbis_info_init(&vi);

	if (vorbis_encode_init_vbr(&vi, fmt.channels, fmt.sample_rate, Quality))
	{
		Msg("! vorbis_encode_init_vbr() failed");
		return false;
	}

	vorbis_comment vc;
	memset(&vc, 0, sizeof(vc));

	if (Comment && CommentLen > 0)
	{
		vc.user_comments = (char**)_ogg_malloc(sizeof(*vc.user_comments));
		vc.comment_lengths = (int*)_ogg_malloc(sizeof(*vc.comment_lengths));
		vc.comments = 1;
		vc.user_comments[0] = (char*)_ogg_malloc(CommentLen);
		memcpy(vc.user_comments[0], Comment, CommentLen);
		vc.comment_lengths[0] = CommentLen;
	}
	else
	{
		VERIFY(!"NO ENTRY!!! EMPTY COMMENT!!!");
		vorbis_comment_init(&vc);
		vorbis_comment_add_tag(&vc, "ENCODER", "IX-Ray SDK AudioFile");
	}

	vorbis_dsp_state vd;
	vorbis_block vb;
	vorbis_analysis_init(&vd, &vi);
	vorbis_block_init(&vd, &vb);

	ogg_stream_state os;
	srand((unsigned int)time(NULL));
	ogg_stream_init(&os, rand());

	ogg_packet header_packet, header_comm, header_code;
	vorbis_analysis_headerout(&vd, &vc, &header_packet, &header_comm, &header_code);
	ogg_stream_packetin(&os, &header_packet);
	ogg_stream_packetin(&os, &header_comm);
	ogg_stream_packetin(&os, &header_code);

	ogg_page og;
	while (ogg_stream_flush(&os, &og))
	{
		fwrite(og.header, 1, og.header_len, fout);
		fwrite(og.body, 1, og.body_len, fout);
	}

	// Main encoding loop
	const int READ_SAMPLES = 1024;
	s16 read_buffer[READ_SAMPLES * 2]; // max 2 channels
	while (!feof(fin))
	{
		size_t samples_read = fread(read_buffer, fmt.block_align, READ_SAMPLES, fin);
		if (samples_read == 0) break;

		float** buffer = vorbis_analysis_buffer(&vd, (int)samples_read);
		for (size_t i = 0; i < samples_read; ++i)
		{
			for (int ch = 0; ch < fmt.channels; ++ch)
			{
				s16 val = read_buffer[i * fmt.channels + ch];
				buffer[ch][i] = val / 32768.f;
			}
		}

		vorbis_analysis_wrote(&vd, (int)samples_read);

		while (vorbis_analysis_blockout(&vd, &vb) == 1)
		{
			vorbis_analysis(&vb, nullptr);
			vorbis_bitrate_addblock(&vb);

			ogg_packet op;
			while (vorbis_bitrate_flushpacket(&vd, &op))
			{
				ogg_stream_packetin(&os, &op);
				while (ogg_stream_pageout(&os, &og))
				{
					fwrite(og.header, 1, og.header_len, fout);
					fwrite(og.body, 1, og.body_len, fout);
				}
			}
		}
	}

	vorbis_analysis_wrote(&vd, 0);

	while (vorbis_analysis_blockout(&vd, &vb) == 1)
	{
		vorbis_analysis(&vb, nullptr);
		vorbis_bitrate_addblock(&vb);
		ogg_packet op;
		while (vorbis_bitrate_flushpacket(&vd, &op))
		{
			ogg_stream_packetin(&os, &op);
			while (ogg_stream_pageout(&os, &og))
			{
				fwrite(og.header, 1, og.header_len, fout);
				fwrite(og.body, 1, og.body_len, fout);
			}
		}
	}

	// Cleanup
	ogg_stream_clear(&os);
	vorbis_block_clear(&vb);
	vorbis_dsp_clear(&vd);

	if (vc.user_comments && Comment && CommentLen > 0)
	{
		if (vc.user_comments[0]) _ogg_free(vc.user_comments[0]);
		_ogg_free(vc.user_comments);
		_ogg_free(vc.comment_lengths);
		if (vc.vendor) _ogg_free(vc.vendor);
	}
	else
	{
		vorbis_comment_clear(&vc);
	}

	vorbis_info_clear(&vi);

	fclose(fin);
	fclose(fout);
}