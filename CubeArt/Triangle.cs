using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using System.Drawing.Imaging;
using System.Drawing;
using BloomPostprocess;

namespace TriangleRender
{
    /// <summary>
    /// This is the main type for your game.
    /// </summary>
    public class Triangle : Game
    {
        GraphicsDeviceManager graphics;
        bool saveToImage;
        BloomComponent bloom;
        //Camera
        Matrix projectionMatrix;
        Matrix viewMatrix;
        Matrix worldMatrix;
        RenderTarget2D screenshot;

        //BasicEffect for rendering
        BasicEffect basicEffect;
        //Geometric info
        Helpers.VertexPositionColorNormal[] triangleVertices;
        VertexBuffer vertexBuffer;

        

        public Triangle(bool save)
        {
            saveToImage = save;
            graphics = new GraphicsDeviceManager(this);
            graphics.PreferredBackBufferWidth = 720;
            graphics.PreferredBackBufferHeight = 720;
            graphics.ApplyChanges();
            Content.RootDirectory = "Content";
        }

        /// <summary>
        /// Allows the game to perform any initialization it needs to before starting to run.
        /// This is where it can query for any required services and load any non-graphic
        /// related content.  Calling base.Initialize will enumerate through any components
        /// and initialize them as well.
        /// </summary>
        protected override void Initialize()
        {
            base.Initialize();
            float width;
            float height;
            float depth;
            float adjust;
            width = 40f;
            adjust = 0.125f;
            height = width*(1-adjust) + (adjust)*(width * ((float) System.Math.Sqrt(3.0f)) / 2f);
            depth = 80f;
            //Setup Camera

            //bloom = new BloomComponent(this);
            //Components.Add(bloom);
            //bloom.Settings = new BloomSettings("None", 0.0f, 0, 0f, 1, 0, 0);
            //bloom.Initialize();
            //System.Console.WriteLine(bloom.Settings.BloomThreshold.ToString());

            worldMatrix = Matrix.Identity;
            viewMatrix = Matrix.CreateLookAt(new Vector3(width/2,width/2, width/ 2), new Vector3(0,0, 0), new Vector3(0, 0, 1));



            projectionMatrix = Matrix.CreateOrthographic(width, height, 0, depth);

            //BasicEffect
            basicEffect = new BasicEffect(GraphicsDevice);
            basicEffect.Alpha = 1f;

            // Want to see the colors of the vertices, this needs to be on
            basicEffect.VertexColorEnabled = true;

            //Lighting requires normal information which VertexPositionColor does not have
            //If you want to use lighting and VPC you need to create a custom def
            basicEffect.LightingEnabled = true;

            //vertexBuffer = new VertexBuffer(GraphicsDevice, typeof(
            //   Helpers.VertexPositionColorNormal), 36, BufferUsage.
            //   WriteOnly);
            Generate.Shapes.Vertex [] [] cubes = 
                    Generate.Vertices.shapesToTriangles(Generate.Clusters.simpleStack(new Generate.Shapes.Position(0,0,0),8,System.Drawing.Color.Gray));
            int vertexCount = 0;

            for(int i = 0; i < cubes.Length; i++)
            {
                vertexCount = vertexCount + cubes[i].Length;
            }

            triangleVertices = new Helpers.VertexPositionColorNormal[vertexCount];
            int idx = 0;
            for (int h = 0; h < cubes.Length; h++)
            {
                for (int i = 0; i < cubes[h].Length; i++)
                {
                    triangleVertices[idx] = new Helpers.VertexPositionColorNormal(
                        new Vector3((float)cubes[h][i].PositionX, (float)cubes[h][i].PositionY, (float)cubes[h][i].PositionZ),
                        new Microsoft.Xna.Framework.Color((float)cubes[h][i].ColorR, (float)cubes[h][i].ColorG, (float)cubes[h][i].ColorB),
                        new Vector3((float)cubes[h][i].NormalX, (float)cubes[h][i].NormalY, (float)cubes[h][i].NormalZ));
                    idx += 1;
                }

            }
            vertexBuffer = new VertexBuffer(GraphicsDevice, typeof(Helpers.VertexPositionColorNormal), triangleVertices.Length, BufferUsage.WriteOnly);
            vertexBuffer.SetData<Helpers.VertexPositionColorNormal>(triangleVertices);


        }

        /// <summary>
        /// LoadContent will be called once per game and is the place to load
        /// all of your content.
        /// </summary>
        protected override void LoadContent()
        {

        }

        /// <summary>
        /// UnloadContent will be called once per game and is the place to unload
        /// game-specific content.
        /// </summary>
        protected override void UnloadContent()
        {
        }


        protected override void Update(GameTime gameTime)
        {
            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            //bloom.BeginDraw();
            if (saveToImage)
            {
                screenshot = new RenderTarget2D(GraphicsDevice, GraphicsDevice.PresentationParameters.BackBufferWidth, GraphicsDevice.PresentationParameters.BackBufferHeight, false, SurfaceFormat.Bgra32, DepthFormat.None);
                GraphicsDevice.SetRenderTarget(screenshot);
            }

            basicEffect.Projection = projectionMatrix;
            basicEffect.View = viewMatrix;
            basicEffect.World = worldMatrix;
            //basicEffect.EnableDefaultLighting();
            basicEffect.DirectionalLight0.Enabled = true;
            basicEffect.DirectionalLight1.Enabled = false;
            basicEffect.DirectionalLight2.Enabled = true;
            basicEffect.DirectionalLight0.DiffuseColor = new Vector3(1.0f, 1.0f, 1.0f);
            basicEffect.DirectionalLight0.Direction = new Vector3(-1, 0, 0);
            basicEffect.DirectionalLight0.SpecularColor = new Vector3(0.5f, 0.5f, 0.5f);
            basicEffect.DirectionalLight1.DiffuseColor = new Vector3(0.5f, 0.5f, 0.5f);
            basicEffect.DirectionalLight1.Direction = new Vector3(0, -1, 0);
            basicEffect.DirectionalLight1.SpecularColor = new Vector3(0.5f, 0.5f, 0.5f);
            basicEffect.DirectionalLight2.DiffuseColor = new Vector3(0.25f, 0.25f, 0.25f);
            basicEffect.DirectionalLight2.Direction = new Vector3(0, 0, -1);
            basicEffect.DirectionalLight2.SpecularColor = new Vector3(0.5f, 0.5f, 0.5f);
            basicEffect.AmbientLightColor = new Vector3(0.2f, 0.2f, 0.2f);
            basicEffect.EmissiveColor = new Vector3(0.2f, 0.2f, 0.2f);
            GraphicsDevice.Clear(new Microsoft.Xna.Framework.Color(32,32,32));
            GraphicsDevice.SetVertexBuffer(vertexBuffer);

            //Turn off culling so we see both sides of our rendered triangle
            RasterizerState rasterizerState = new RasterizerState();
            rasterizerState.CullMode = CullMode.None;
            GraphicsDevice.RasterizerState = rasterizerState;

            foreach (EffectPass pass in basicEffect.CurrentTechnique.Passes){
                pass.Apply();
                GraphicsDevice.DrawPrimitives(PrimitiveType.TriangleList, 0, vertexBuffer.VertexCount);
            }

            base.Draw(gameTime);
            if(saveToImage) {
                GraphicsDevice.Present();
                GraphicsDevice.SetRenderTarget(null);

                using (Bitmap bitmap = new Bitmap(screenshot.Width, screenshot.Height, PixelFormat.Format32bppArgb))
                {
                    System.Drawing.Rectangle rect = new System.Drawing.Rectangle(0, 0, screenshot.Width, screenshot.Height);

                    Microsoft.Xna.Framework.Color[] rawData = new Microsoft.Xna.Framework.Color[screenshot.Width * screenshot.Height];
                    screenshot.GetData<Microsoft.Xna.Framework.Color>(rawData);
                    Microsoft.Xna.Framework.Color tempColor;
                    for (int row = 0; row < screenshot.Height; row++)
                    {
                        for (int column = 0; column < screenshot.Width; column++)
                        {
                            // Assumes row major ordering of the array.
                            tempColor = rawData[row * screenshot.Width + column];
                            bitmap.SetPixel(column, row, System.Drawing.Color.FromArgb((int)tempColor.B, (int)tempColor.G, (int)tempColor.R));
                        }
                    }


                    bitmap.Save("test.png", ImageFormat.Png);
                }
            }
        }
    }
}
