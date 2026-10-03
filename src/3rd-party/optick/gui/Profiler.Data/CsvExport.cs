using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.IO.Compression;
using System.Text;

namespace Profiler.Data
{
	// CSV export of a capture.
	//
	// ExportSummary (default) writes one row per unique (thread, name, file, line)
	// with aggregated statistics - a few thousand rows for a full capture.
	// ExportFull writes every zone instance, but only with the essential columns.
	public static class CsvExporter
	{
		private class SummaryRow
		{
			public String Thread;
			public String Name;
			public String File;
			public Int32 Line;
			public Int64 Count;
			public double Total;
			public double Self;
			public double Min = Double.MaxValue;
			public double Max = Double.MinValue;
		}

		public static void ExportSummary(String path, IEnumerable<FrameGroup> groups)
		{
			using (StreamWriter writer = new StreamWriter(path, false, new UTF8Encoding(true)))
				WriteSummary(writer, groups);
		}

		public static void ExportFull(String path, IEnumerable<FrameGroup> groups)
		{
			using (StreamWriter writer = new StreamWriter(path, false, new UTF8Encoding(true)))
				WriteFull(writer, groups);
		}

		public static void ExportFullGzip(String path, IEnumerable<FrameGroup> groups)
		{
			using (FileStream file = new FileStream(path, FileMode.Create, FileAccess.Write))
			using (GZipStream gzip = new GZipStream(file, CompressionMode.Compress))
			using (StreamWriter writer = new StreamWriter(gzip, new UTF8Encoding(true)))
				WriteFull(writer, groups);
		}

		private static void WriteSummary(StreamWriter writer, IEnumerable<FrameGroup> groups)
		{
			Dictionary<String, SummaryRow> summary = new Dictionary<String, SummaryRow>();

			ForEachEntry(groups, (threadName, frame, node, level) =>
			{
				Entry entry = node.Entry;
				EventDescription description = entry.Description;

				String key = threadName + "\u0001" + description.Name + "\u0001" + description.Path.File + "\u0001" + description.Path.Line;
				SummaryRow row;
				if (!summary.TryGetValue(key, out row))
				{
					row = new SummaryRow
					{
						Thread = threadName,
						Name = description.Name,
						File = description.Path.File,
						Line = description.Path.Line,
					};
					summary.Add(key, row);
				}

				double duration = entry.Duration;
				double self = node.SelfDuration;
				if (self < 0.0)
					self = 0.0;

				row.Count++;
				row.Total += duration;
				row.Self += self;
				row.Min = Math.Min(row.Min, duration);
				row.Max = Math.Max(row.Max, duration);
			});

			List<SummaryRow> rows = new List<SummaryRow>(summary.Values);
			rows.Sort((a, b) => b.Total.CompareTo(a.Total));

			writer.WriteLine("Thread,Name,File,Line,Count,Total(ms),Self(ms),Mean(ms),Min(ms),Max(ms)");

			foreach (SummaryRow row in rows)
			{
				writer.WriteLine(String.Join(",",
					Escape(row.Thread),
					Escape(row.Name),
					Escape(row.File),
					row.Line.ToString(CultureInfo.InvariantCulture),
					row.Count.ToString(CultureInfo.InvariantCulture),
					Format(row.Total),
					Format(row.Self),
					Format(row.Total / row.Count),
					Format(row.Min),
					Format(row.Max)));
			}
		}

		private static void WriteFull(StreamWriter writer, IEnumerable<FrameGroup> groups)
		{
			writer.WriteLine("Thread,Start(ms),Duration(ms),Depth,Name");

			ForEachEntry(groups, (threadName, frame, node, level) =>
			{
				Entry entry = node.Entry;

				writer.WriteLine(String.Join(",",
					Escape(threadName),
					Format(entry.StartMS),
					Format(entry.Duration),
					level.ToString(CultureInfo.InvariantCulture),
					Escape(entry.Description.Name)));
			});
		}

		private static void ForEachEntry(IEnumerable<FrameGroup> groups, Action<String, EventFrame, EventNode, Int32> action)
		{
			foreach (FrameGroup group in groups)
			{
				if (group == null || group.Threads == null)
					continue;

				for (int threadIndex = 0; threadIndex < group.Threads.Count; ++threadIndex)
				{
					ThreadData thread = group.Threads[threadIndex];
					if (thread == null || thread.Description == null || thread.Events == null)
						continue;

					if (thread.Description.Origin == ThreadDescription.Source.Sampling)
						continue;

					String threadName = thread.Description.FullName;

					for (int frameIndex = 0; frameIndex < thread.Events.Count; ++frameIndex)
					{
						EventFrame frame = thread.Events[frameIndex];
						if (frame == null)
							continue;

						frame.Root.ForEachChild((node, level) =>
						{
							EventNode eventNode = node as EventNode;
							if (eventNode == null || eventNode.Entry == null || eventNode.Entry.Description == null)
								return true;

							action(threadName, frame, eventNode, level);
							return true;
						});
					}
				}
			}
		}

		private static String Format(double value)
		{
			return value.ToString("0.###", CultureInfo.InvariantCulture);
		}

		private static String Escape(String value)
		{
			if (String.IsNullOrEmpty(value))
				return String.Empty;

			if (value.IndexOfAny(new char[] { ',', '"', '\n', '\r' }) == -1)
				return value;

			return "\"" + value.Replace("\"", "\"\"") + "\"";
		}
	}
}
