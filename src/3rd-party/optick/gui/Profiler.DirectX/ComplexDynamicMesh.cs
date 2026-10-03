using System;
using System.Collections.Generic;
using SharpDX.Direct3D11;
using System.Windows.Media;
using System.Windows;

namespace Profiler.DirectX
{
	public class ComplexDynamicMesh
	{
		List<DynamicMesh> DIPs = new List<DynamicMesh>();

		public ComplexDynamicMesh(DirectXCanvas canvas, int chunkCount = 20)
		{
			double scaleX = 1.0 / chunkCount;

			for (int i = 0; i < chunkCount; ++i)
			{
				DynamicMesh mesh = canvas.CreateMesh();
				// Vertices are stored relative to the chunk origin (0..1 inside the chunk).
				// Storing them in absolute space (chunkCount * unit + i) made float32
				// quantize their positions and snapped the bars at high zoom.
				mesh.LocalTransform = new Matrix(scaleX, 0.0, 0.0, 1.0, (double)i / chunkCount, 0.0);
				DIPs.Add(mesh);
			}
		}

		private DynamicMesh SelectMesh(Point p)
		{
			int index = Math.Min(DIPs.Count - 1, Math.Max((int)(p.X * DIPs.Count), 0));
			return DIPs[index];
		}

		// Splits a rect into per-chunk pieces so that every chunk only stores its local part.
		private void ForEachChunk(Rect rect, Action<int, Rect> action)
		{
			double left = rect.Left;
			double right = rect.Right;
			if (right <= left)
				return;

			int first = Math.Max(0, (int)Math.Floor(left * DIPs.Count));
			int last = Math.Min(DIPs.Count - 1, (int)Math.Floor(right * DIPs.Count));

			for (int i = first; i <= last; ++i)
			{
				double chunkLeft = (double)i / DIPs.Count;
				double chunkRight = (double)(i + 1) / DIPs.Count;
				double l = Math.Max(left, chunkLeft);
				double r = Math.Min(right, chunkRight);
				if (r <= l)
					continue;

				action(i, new Rect(l, rect.Top, r - l, rect.Height));
			}
		}

		private static Color Lerp(Color a, Color b, double t)
		{
			t = Math.Max(0.0, Math.Min(1.0, t));
			return Color.FromArgb(
				(byte)(a.A + (b.A - a.A) * t),
				(byte)(a.R + (b.R - a.R) * t),
				(byte)(a.G + (b.G - a.G) * t),
				(byte)(a.B + (b.B - a.B) * t));
		}

		public void AddRect(Rect rect, System.Windows.Media.Color color)
		{
			ForEachChunk(rect, (i, part) => DIPs[i].AddRect(part, color));
		}

		public void AddRect(Rect rect, System.Windows.Media.Color[] colors)
		{
			double inv = rect.Width > 0.0 ? 1.0 / rect.Width : 0.0;
			ForEachChunk(rect, (i, part) =>
			{
				double t0 = (part.Left - rect.Left) * inv;
				double t1 = (part.Right - rect.Left) * inv;

				Color[] partColors = new Color[]
				{
					Lerp(colors[0], colors[1], t0),
					Lerp(colors[0], colors[1], t1),
					Lerp(colors[3], colors[2], t1),
					Lerp(colors[3], colors[2], t0),
				};

				DIPs[i].AddRect(part, partColors);
			});
		}

		public void AddRect(System.Windows.Point[] rect, System.Windows.Media.Color color)
		{
			SelectMesh(rect[0]).AddRect(rect, color);
		}

		public void AddTri(System.Windows.Point a, System.Windows.Point b, System.Windows.Point c, System.Windows.Media.Color color)
		{
			SelectMesh(a).AddTri(a, b, c, color);
		}

		public void AddLine(System.Windows.Point start, System.Windows.Point finish, System.Windows.Media.Color color)
		{
			SelectMesh(start).AddLine(start, finish, color);
		}

		public List<Mesh> Freeze(SharpDX.Direct3D11.Device device)
		{
			List<Mesh> result = new List<Mesh>(DIPs.Count);
			DIPs.ForEach(dip =>
			{
				Mesh mesh = dip.Freeze(device);
				if (mesh != null)
					result.Add(mesh);
			});
			return result;
		}
	}
}
