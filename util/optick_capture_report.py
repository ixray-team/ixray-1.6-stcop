"""Summarize an Optick capture (.opt) without the Optick GUI.

Usage:
	python optick_capture_report.py <capture.opt> [--filter TEXT] [--top N]
	python optick_capture_report.py <capture.opt> --nested PARENT CHILD

The default report lists events per thread: max, average, and call count.
--nested measures, for each PARENT span, how much CHILD work runs inside it
on any thread, when the first CHILD starts, and when the last one ends.
This separates real work from waiting.

Format notes (Optick protocol version 0x1A, see optick_core.cpp):
	file   = 8-byte header + zlib stream
	stream = chunks of {u32 version, u32 size, u16 type, u16 app} + payload
	type 0 = FrameDescriptionBoard (threads, event descriptions)
	type 1 = EventFrame (scope header, categories, events)
"""

import argparse
import bisect
import collections
import statistics
import struct
import zlib

FRAME_DESCRIPTION_BOARD = 0
EVENT_FRAME = 1


class Reader:
	def __init__(self, data, pos):
		self.data = data
		self.pos = pos

	def read(self, fmt):
		values = struct.unpack_from(fmt, self.data, self.pos)
		self.pos += struct.calcsize(fmt)
		return values if len(values) > 1 else values[0]

	def string(self):
		length = self.read("<I")
		value = self.data[self.pos:self.pos + length].decode("utf8", "replace")
		self.pos += length
		return value


def iterate_chunks(data):
	pos = 0
	while pos + 12 <= len(data):
		_, size, chunk_type, _ = struct.unpack_from("<IIHH", data, pos)
		yield chunk_type, pos + 12, size
		pos += 12 + size


def load_capture(path):
	with open(path, "rb") as file:
		raw = file.read()
	data = zlib.decompressobj().decompress(raw[8:])

	frequency = 1
	threads = []
	names = []
	for chunk_type, pos, _ in iterate_chunks(data):
		if chunk_type != FRAME_DESCRIPTION_BOARD:
			continue
		reader = Reader(data, pos)
		reader.read("<I")  # board number
		frequency = reader.read("<q")
		reader.read("<Q")  # origin
		reader.read("<I")  # precision
		reader.read("<qq")  # time slice
		for _ in range(reader.read("<I")):
			reader.read("<Q")  # thread id
			reader.read("<I")  # process id
			threads.append(reader.string())
			reader.read("<iiI")  # max depth, priority, mask
		for _ in range(reader.read("<I")):
			reader.read("<Q")  # fiber id
		reader.read("<I")  # forced main thread index
		for _ in range(reader.read("<I")):
			name = reader.string()
			reader.string()  # file
			reader.read("<IIIfB")  # line, filter, color, budget, flags
			names.append(name)

	# (thread index, event name) -> [(start, duration)]
	events = collections.defaultdict(list)
	for chunk_type, pos, _ in iterate_chunks(data):
		if chunk_type != EVENT_FRAME:
			continue
		reader = Reader(data, pos)
		_, thread, _ = reader.read("<Iii")  # board, thread, fiber
		reader.read("<qq")  # scope time
		reader.read("<I")  # frame type
		for is_events in (False, True):
			for _ in range(reader.read("<I")):
				start, finish, description = reader.read("<qqI")
				if is_events and description < len(names):
					events[(thread, names[description])].append((start, finish - start))

	return frequency, threads, events


def main():
	parser = argparse.ArgumentParser(description="Summarize an Optick capture.")
	parser.add_argument("capture")
	parser.add_argument("--filter", default="", help="case-insensitive event name substring")
	parser.add_argument("--top", type=int, default=50)
	parser.add_argument("--nested", nargs=2, metavar=("PARENT", "CHILD"))
	args = parser.parse_args()

	frequency, threads, events = load_capture(args.capture)

	def to_ms(ticks):
		return ticks * 1000.0 / frequency

	def thread_name(index):
		return threads[index] if 0 <= index < len(threads) else str(index)

	if args.nested:
		parent_name, child_name = args.nested
		parents = sorted(x for (_, name), spans in events.items() if name == parent_name for x in spans)
		children = sorted((start, duration) for (_, name), spans in events.items() if name == child_name for (start, duration) in spans)
		child_starts = [x[0] for x in children]

		rows = []
		for start, duration in parents:
			inside = children[bisect.bisect_left(child_starts, start):bisect.bisect_right(child_starts, start + duration)]
			if not inside:
				rows.append((duration, 0, 0, 0))
				continue
			work = sum(x[1] for x in inside)
			first = inside[0][0] - start
			last = max(x[0] + x[1] for x in inside) - start
			rows.append((duration, work, first, last))

		if not rows:
			print("No '%s' events found" % parent_name)
			return

		for label, column in (("parent duration", 0), ("child work", 1), ("first child start", 2), ("last child end", 3)):
			print("%-18s median %8.3f ms" % (label, to_ms(statistics.median(r[column] for r in rows))))
		return

	summary = []
	for (thread, name), spans in events.items():
		if args.filter.lower() not in name.lower():
			continue
		durations = [x[1] for x in spans]
		summary.append((max(durations), sum(durations) / len(durations), len(durations), thread_name(thread), name))

	for longest, average, count, thread, name in sorted(summary, reverse=True)[:args.top]:
		print("%8.2f max %8.3f avg %7d  %-22s %s" % (to_ms(longest), to_ms(average), count, thread, name))


if __name__ == "__main__":
	main()
