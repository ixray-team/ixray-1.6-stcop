using System;
using System.Windows.Controls;
using System.Windows.Media;

using SharpDX;
using SharpDX.Direct3D;
using SharpDX.Direct3D11;
using SharpDX.DXGI;
using SharpDX.D3DCompiler;
using System.Diagnostics;
using System.Reflection;
using System.IO;
using System.Runtime.InteropServices;
using SharpDX.Windows;

namespace Profiler.DirectX
{
	/// <summary>
	/// Interaction logic for DirectXCanvas.xaml
	/// </summary>
	public partial class DirectXCanvas : UserControl
	{
		public SharpDX.Direct3D11.Device RenderDevice;
		public SwapChain SwapChain;
		public Factory RenderFactory;
		public RenderControl RenderCanvas;
		public SwapChainDescription SwapChainDesc;

		[StructLayout(LayoutKind.Sequential, Size = 32)]
		public struct WorldProjection
		{
			public SharpDX.Matrix View;
			public SharpDX.Matrix World;
		}

		SharpDX.Matrix UnitView;
		SharpDX.Matrix PixelView;

		WorldProjection WP = new WorldProjection();
		SharpDX.Direct3D11.Buffer WPConstantBuffer;

		Vector4 ViewOrigin;
		SharpDX.Direct3D11.Buffer OriginConstantBuffer;

		RasterizerStateDescription RasterizerDesc = new RasterizerStateDescription
		{
			CullMode = CullMode.None,
			DepthBias = 0,
			DepthBiasClamp = 0,
			FillMode = FillMode.Solid,
			IsAntialiasedLineEnabled = true,
			IsDepthClipEnabled = false,
			IsFrontCounterClockwise = false,
			IsMultisampleEnabled = true,
			IsScissorEnabled = false,
			SlopeScaledDepthBias = 0
		};

		public Texture2D BackBuffer;
		public RenderTargetView RTView;

		public Fragment DefaultFragment;
		public Fragment TextFragment;

		public TextManager Text;

		public Fragment LoadFragment(string fileName, InputElement[] inputElements)
		{
			Fragment result = new Fragment();

			ShaderBytecode vertexShaderByteCode = null;
			ShaderBytecode pixelShaderByteCode = null;

			Assembly assembly = Assembly.GetExecutingAssembly();

			using (Stream streamPS = assembly.GetManifestResourceStream("Profiler.DirectX.Shaders." + fileName + "_ps.fxo"))
			{
				pixelShaderByteCode = ShaderBytecode.FromStream(streamPS);
			}

			using (Stream streamVS = assembly.GetManifestResourceStream("Profiler.DirectX.Shaders." + fileName + "_vs.fxo"))
			{
				vertexShaderByteCode = ShaderBytecode.FromStream(streamVS);
			}

			result.VS = new VertexShader(RenderDevice, vertexShaderByteCode.Data);
			result.PS = new PixelShader(RenderDevice, pixelShaderByteCode.Data);

			result.Layout = new InputLayout(RenderDevice, vertexShaderByteCode, inputElements);

			vertexShaderByteCode.Dispose();
			pixelShaderByteCode.Dispose();

			return result;
		}


		SamplerStateDescription TextSamplerDescription = new SamplerStateDescription
		{
			AddressU = TextureAddressMode.Clamp,
			AddressV = TextureAddressMode.Clamp,
			AddressW = TextureAddressMode.Clamp,
			Filter = Filter.MinMagPointMipLinear
		};

		public SamplerState TextSamplerState;

		RenderTargetBlendDescription AlphaBlendStateDescription = new RenderTargetBlendDescription(true,
																									   BlendOption.SourceAlpha,
																									   BlendOption.InverseSourceAlpha,
																									   BlendOperation.Add,
																									   BlendOption.One,
																									   BlendOption.Zero,
																									   BlendOperation.Add,
																									   ColorWriteMaskFlags.All);

		public BlendState AlphaBlendState;

		public DirectXCanvas()
		{
			InitializeComponent();

			RenderCanvas = new RenderControl();
			RenderForm.Child = RenderCanvas;

			SwapChainDesc = new SwapChainDescription()
			{
				BufferCount = 2,
				ModeDescription = new ModeDescription(RenderCanvas.ClientSize.Width, RenderCanvas.ClientSize.Height, new Rational(60, 1), Format.R8G8B8A8_UNorm),
				IsWindowed = true,
				OutputHandle = RenderCanvas.Handle,
				SampleDescription = new SampleDescription(4, 0),
				SwapEffect = SwapEffect.Discard,
				Usage = Usage.RenderTargetOutput,
			};

			SharpDX.Direct3D11.Device device;
			SwapChain swapChain;

			DeviceCreationFlags flags = DeviceCreationFlags.None;

#if DEBUG
			//flags |= DeviceCreationFlags.Debug;
#endif

			SharpDX.Direct3D11.Device.CreateWithSwapChain(DriverType.Hardware, flags, SwapChainDesc, out device, out swapChain);

			SwapChain = swapChain;
			RenderDevice = device;

			RenderFactory = SwapChain.GetParent<Factory>();
			RenderFactory.MakeWindowAssociation(RenderCanvas.Handle, WindowAssociationFlags.IgnoreAll);

			RenderDevice.ImmediateContext.Rasterizer.State = new RasterizerState(RenderDevice, RasterizerDesc);

			WPConstantBuffer = SharpDX.Direct3D11.Buffer.Create(RenderDevice, BindFlags.ConstantBuffer, ref WP);

			Vector4 origin = new Vector4(0.0f, 0.0f, 0.0f, 0.0f);
			OriginConstantBuffer = SharpDX.Direct3D11.Buffer.Create(RenderDevice, BindFlags.ConstantBuffer, ref origin);

			DefaultFragment = LoadFragment(@"Basic", new[]
			{
				new InputElement("POSITION", 0, Format.R32G32_Float, 0, 0),
				new InputElement("COLOR", 0, Format.R8G8B8A8_UNorm, 8, 0)
			});

			TextFragment = LoadFragment(@"Text", new[]
			{
				new InputElement("POSITION", 0, Format.R32G32_Float, 0, 0),
				new InputElement("TEXCOORD", 0, Format.R32G32_Float, 8, 0),
				new InputElement("COLOR", 0, Format.R8G8B8A8_UNorm, 16, 0)
			});

			Text = new TextManager(this);
			TextSamplerState = new SamplerState(device, TextSamplerDescription);

			BlendStateDescription blendDescription = new BlendStateDescription();
			blendDescription.RenderTarget[0] = AlphaBlendStateDescription;
			AlphaBlendState = new BlendState(device, blendDescription);

			RenderCanvas.Paint += RenderCanvas_Paint;
			RenderCanvas.Resize += RenderCanvas_Resize;

			UnitView = SharpDX.Matrix.Scaling(2.0f, -2.0f, 1.0f);
			UnitView.TranslationVector = new Vector3(-1.0f, 1.0f, 0.0f);

			OnResize();
		}

		private void OnResize()
		{
			// Dispose all previous allocated resources
			Utilities.Dispose(ref BackBuffer);
			Utilities.Dispose(ref RTView);

			// Resize the backbuffer
			SwapChain.ResizeBuffers(SwapChainDesc.BufferCount, RenderCanvas.ClientSize.Width, RenderCanvas.ClientSize.Height, Format.Unknown, SwapChainFlags.None);

			// Get the backbuffer from the swapchain
			BackBuffer = Texture2D.FromSwapChain<Texture2D>(SwapChain, 0);

			// Renderview on the backbuffer
			RTView = new RenderTargetView(RenderDevice, BackBuffer);

			PixelView = SharpDX.Matrix.Scaling(2.0f / RenderCanvas.ClientSize.Width, -2.0f / RenderCanvas.ClientSize.Height, 1.0f);
			PixelView.TranslationVector = new Vector3(-1.0f, 1.0f, 0.0f);
		}

		private void RenderCanvas_Resize(object sender, EventArgs e)
		{
			OnResize();
		}

		SharpDX.Color ConvertColor(Brush brush)
		{
			var color = brush != null ? (brush as SolidColorBrush).Color : Colors.Black;
			return Utils.Convert(color);
		}

		public enum Layer
		{
			Background,
			Normal,
			Foreground,
		}

		public delegate void OnDrawHandler(DirectXCanvas canvas, Layer layer);
		public event OnDrawHandler OnDraw;

		public void Update()
		{
			RenderCanvas.Refresh();
		}

		public struct Stats
		{
			public int DIPs { get; set; }
			public int Tris { get; set; }
			public void Reset()
			{
				DIPs = 0;
				Tris = 0;
			}
			public void Add(int triCount)
			{
				Tris += triCount;
				DIPs += 1;
			}
		}

		public Stats Statistics = new Stats();

		private void RenderCanvas_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			var context = RenderDevice.ImmediateContext;

			Statistics.Reset();

			context.ClearRenderTargetView(RTView, ConvertColor(Background));

			if (OnDraw != null)
			{
				foreach (Layer layer in Enum.GetValues(typeof(Layer)))
				{
					OnDraw(this, layer);
					Text.Render(this);
				}
			}

			SwapChain.Present(0, PresentFlags.None);
		}

		// Moves the translation out of the matrix. At high zoom the translation is huge
		// (ViewUnit.Left * Zoom) and float32 subtraction of two such values in the vertex
		// shader caused visible snapping; subtracting a small origin before the multiply
		// keeps full precision.
		private System.Windows.Media.Matrix RebaseOrigin(System.Windows.Media.Matrix world, out float originX, out float originY)
		{
			double ox = Math.Abs(world.M11) > double.Epsilon ? -world.OffsetX / world.M11 : 0.0;
			double oy = Math.Abs(world.M22) > double.Epsilon ? -world.OffsetY / world.M22 : 0.0;

			System.Windows.Media.Matrix rebased = world;
			rebased.OffsetX = 0.0;
			rebased.OffsetY = 0.0;

			originX = (float)ox;
			originY = (float)oy;
			return rebased;
		}

		public void Draw(Mesh mesh)
		{
			if (mesh != null && mesh.Fragment != null && mesh.VertexBuffer != null && mesh.IndexBuffer != null)
			{
				WP.View = mesh.Projection == Mesh.ProjectionType.Unit ? UnitView : PixelView;

				System.Windows.Media.Matrix world = System.Windows.Media.Matrix.Multiply(mesh.LocalTransform, mesh.WorldTransform);

				float originX, originY;
				WP.World = Utils.Convert(RebaseOrigin(world, out originX, out originY));
				ViewOrigin = new Vector4(originX, originY, 0.0f, 0.0f);

				// Cull using the same rebased transform to keep the test precise as well.
				SharpDX.Matrix vw = SharpDX.Matrix.Multiply(WP.World, WP.View);
				Vector4 posA = Vector2.Transform(new Vector2((float)(mesh.AABB.Left - originX), (float)(mesh.AABB.Bottom - originY)), vw);
				Vector4 posB = Vector2.Transform(new Vector2((float)(mesh.AABB.Right - originX), (float)(mesh.AABB.Top - originY)), vw);

				float minX = Math.Min(posA.X, posB.X);
				float maxX = Math.Max(posA.X, posB.X);

				if (maxX < -1f || minX > 1f)
					return;

				PrimitiveTopology topology = mesh.Geometry == Mesh.GeometryType.Polygons ? PrimitiveTopology.TriangleList : PrimitiveTopology.LineList;
				int indexCount = (mesh.Geometry == Mesh.GeometryType.Polygons ? 3 : 2) * mesh.PrimitiveCount;

				SetAlphaBlend(mesh.UseAlpha);
				Setup(mesh.Fragment, mesh.VertexBufferBinding, mesh.IndexBuffer, topology);
				RenderDevice.ImmediateContext.DrawIndexed(indexCount, 0, 0);
				SetAlphaBlend(false);

				Statistics.Add(mesh.PrimitiveCount);
			}
		}

		public void SetAlphaBlend(bool isOn)
		{
			RenderDevice.ImmediateContext.OutputMerger.SetBlendState(isOn ? AlphaBlendState : null);
		}

		// Projects a unit-space point through the same matrices that are used to draw
		// unit meshes and converts the result into render-target pixel coordinates.
		// Used to place text exactly on top of its mesh regardless of the surface size.
		public System.Windows.Point UnitToPixel(System.Windows.Point unit, System.Windows.Media.Matrix world)
		{
			// Computed fully in double, exactly like Tracy's (time - viewStart) * pxns.
			// The unit projection maps world [0..1] to the whole render target, so the
			// pixel position is simply the transformed point scaled by the target size.
			System.Windows.Point p = world.Transform(unit);
			return new System.Windows.Point(p.X * RenderCanvas.ClientSize.Width,
											p.Y * RenderCanvas.ClientSize.Height);
		}

		public void Setup(Fragment fragment, VertexBufferBinding vb, SharpDX.Direct3D11.Buffer ib, PrimitiveTopology topology = PrimitiveTopology.TriangleList)
		{
			var context = RenderDevice.ImmediateContext;

			// Prepare All the stages
			context.InputAssembler.InputLayout = fragment.Layout;
			context.InputAssembler.PrimitiveTopology = topology;
			context.InputAssembler.SetVertexBuffers(0, vb);
			context.InputAssembler.SetIndexBuffer(ib, Format.R32_UInt, 0);
			context.VertexShader.Set(fragment.VS);
			context.PixelShader.Set(fragment.PS);
			context.Rasterizer.SetViewport(new Viewport(0, 0, RenderCanvas.ClientSize.Width, RenderCanvas.ClientSize.Height, 0.0f, 1.0f));
			context.OutputMerger.SetTargets(RTView);

			context.UpdateSubresource(new WorldProjection[] { WP }, WPConstantBuffer);
			context.VertexShader.SetConstantBuffer(0, WPConstantBuffer);
			context.PixelShader.SetConstantBuffer(0, WPConstantBuffer);

			context.UpdateSubresource(new Vector4[] { ViewOrigin }, OriginConstantBuffer);
			context.VertexShader.SetConstantBuffer(1, OriginConstantBuffer);
		}

		public void Dispose()
		{
			RenderDevice.ImmediateContext.Flush();
			Utilities.Dispose(ref OriginConstantBuffer);
			Utilities.Dispose(ref RTView);
			Utilities.Dispose(ref BackBuffer);
			Utilities.Dispose(ref RenderDevice);
			Utilities.Dispose(ref DefaultFragment);
			Utilities.Dispose(ref SwapChain);
			Utilities.Dispose(ref RenderFactory);
			Utilities.Dispose(ref TextSamplerState);
		}

		public enum MeshType
		{
			Poly,
			Text,
		}

		public DynamicMesh CreateMesh(MeshType type = MeshType.Poly)
		{
			DynamicMesh mesh = new DynamicMesh(RenderDevice);

			switch (type)
			{
				case MeshType.Poly:
					mesh.Fragment = DefaultFragment;
					mesh.Projection = Mesh.ProjectionType.Unit;
					break;

				case MeshType.Text:
					mesh.Fragment = TextFragment;
					mesh.Projection = Mesh.ProjectionType.Pixel;
					break;

				default:
					break;
			}

			return mesh;
		}
	}
}
