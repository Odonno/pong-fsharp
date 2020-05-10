module GameCore

open Model
open Core
open View
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open Microsoft.Xna.Framework.Input;

type GameLoop ()
    as this = 
    inherit Game()

    let width, height = 600, 400

    let resolution = Windowed (width, height)

    let mutable graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable keyboardInfo = { Pressed = []; KeysDown = []; KeysUp = [] }
    let mutable currentModel: World = initialWorld (width, height)

    do 
        match resolution with
        | FullScreen _ -> 
            graphics.PreferredBackBufferWidth <- width
            graphics.PreferredBackBufferHeight <- height
            graphics.IsFullScreen <- true
        | Windowed _ -> 
            graphics.PreferredBackBufferWidth <- width
            graphics.PreferredBackBufferHeight <- height
    
    override __.LoadContent () = 
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

    override __.Update (gameTime) =
        keyboardInfo <- updateKeyboardInfo (Keyboard.GetState()) keyboardInfo

        let mouseInfo = getMouseInfo (Mouse.GetState())
        let runState = { 
            TotalTime = gameTime.TotalGameTime.TotalMilliseconds 
            Elapsed = gameTime.ElapsedGameTime.TotalMilliseconds
            Keyboard = keyboardInfo
            Mouse = mouseInfo
        }
        
        currentModel <- updateModel runState currentModel

    override __.Draw (_) =
        this.GraphicsDevice.Clear Color.Black
        
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp)
        
        draw spriteBatch currentModel

        spriteBatch.End()