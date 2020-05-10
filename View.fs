module View

open Core
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open Model

let rectangleTexture (spriteBatch: SpriteBatch) (width, height) =
    let rect = new Texture2D(spriteBatch.GraphicsDevice, width, height)
    let data = [| for _ in 1 .. width * height -> Color.White |]
    rect.SetData(data)
    rect

let drawPlayer spriteBatch (paddle: Paddle) =
    let playerTexture = rectangleTexture spriteBatch (paddle.Size.Width, paddle.Size.Height)

    let coordPlayer = (paddle.Position.X, paddle.Position.Y) |> asVector2
    spriteBatch.Draw(playerTexture, coordPlayer, Color.White)

let drawBall spriteBatch (ball: Ball) =
    let playerTexture = rectangleTexture spriteBatch (ball.Size.Width, ball.Size.Height)

    let coordPlayer = (ball.Position.X, ball.Position.Y) |> asVector2
    spriteBatch.Draw(playerTexture, coordPlayer, Color.White)

let draw (spriteBatch: SpriteBatch) (world: World) =
    drawPlayer spriteBatch world.Player.Paddle
    drawPlayer spriteBatch world.Bot.Paddle

    drawBall spriteBatch world.Ball