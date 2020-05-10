module Core

open Model
open System
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework

type Resolution =
    | Windowed of int * int
    | FullScreen of int * int

type KeyboardInfo = {
    Pressed: Keys list;
    KeysDown: Keys list;
    KeysUp: Keys list;
}

type MouseInfo = {
    Position: int * int;
    Pressed: bool * bool;
}

type RunState = {
    TotalTime: float;
    Elapsed: float;
    Keyboard: KeyboardInfo;
    Mouse: MouseInfo;
}
    
type RunState with
    member __.IsPressed key = List.contains key __.Keyboard.Pressed
    member __.WasJustPressed key = List.contains key __.Keyboard.KeysDown

let asVector2 (x, y) = 
    Vector2 (float32 x, float32 y)

let asRectangle (x, y, width, height) = 
    Rectangle (x, y, width, height)

let updateKeyboardInfo (keyboard: KeyboardState) (existing: KeyboardInfo) =
    let pressed = keyboard.GetPressedKeys() |> Set.ofArray
    {
        Pressed = pressed |> Set.toList
        KeysDown = Set.difference pressed (existing.Pressed |> Set.ofList) |> Set.toList
        KeysUp = Set.difference (existing.Pressed |> Set.ofList) pressed |> Set.toList
    }

let getMouseInfo (mouse: MouseState) =
    {
        Position = mouse.X, mouse.Y
        Pressed = mouse.LeftButton = ButtonState.Pressed, mouse.RightButton = ButtonState.Pressed
    }

let movePlayer (runState: RunState) (world: World) =
    let goUp = runState.IsPressed Keys.Up
    let goDown = runState.IsPressed Keys.Down

    let distance = (runState.Elapsed / 10. * world.Player.Paddle.Speed) |> int

    match (goUp, goDown) with
    | (true, false) -> 
        let minY = 0

        { world with
            Player = 
            { world.Player with
                Paddle = 
                { world.Player.Paddle with
                    Position = 
                    { world.Player.Paddle.Position with
                        Y = Math.Max(minY, world.Player.Paddle.Position.Y - distance)
                    }
                }
            }
        }
    | (false, true) -> 
        let maxY = world.Dimension.Height - world.Player.Paddle.Size.Height

        { world with
            Player = 
            { world.Player with
                Paddle = 
                { world.Player.Paddle with
                    Position = 
                    { world.Player.Paddle.Position with
                        Y = Math.Min(maxY, world.Player.Paddle.Position.Y + distance)
                    }
                }
            }
        }
    | _ -> world 

let moveBot (runState: RunState) (world: World) =
    let botControlledByPlayer = false

    let (goUp, goDown) =
        if botControlledByPlayer then
            (runState.IsPressed Keys.Up, runState.IsPressed Keys.Down)
        else
            let isBallAbove = world.Ball.Position.Y < world.Bot.Paddle.Position.Y + 20
            let isBallBelow = world.Ball.Position.Y > (world.Bot.Paddle.Position.Y + world.Bot.Paddle.Size.Height - 20)

            (isBallAbove, isBallBelow)

    let distance = (runState.Elapsed / 10. * world.Bot.Paddle.Speed) |> int
    
    match (goUp, goDown) with
    | (true, false) -> 
        let minY = 0

        { world with
            Bot = 
            { world.Bot with
                Paddle = 
                { world.Bot.Paddle with
                    Position = 
                    { world.Bot.Paddle.Position with
                        Y = Math.Max(minY, world.Bot.Paddle.Position.Y - distance)
                    }
                }
            }
        }
    | (false, true) -> 
        let maxY = world.Dimension.Height - world.Bot.Paddle.Size.Height

        { world with
            Bot = 
            { world.Bot with
                Paddle = 
                { world.Bot.Paddle with
                    Position = 
                    { world.Bot.Paddle.Position with
                        Y = Math.Min(maxY, world.Bot.Paddle.Position.Y + distance)
                    }
                }
            }
        }
    | _ -> world 

let moveBall (runState: RunState) (world: World) =
    { world with
        Ball = 
        { world.Ball with
            Position = 
            { world.Ball.Position with
                X = world.Ball.Position.X + ((runState.Elapsed / 10. * world.Ball.Speed * world.Ball.Direction.X) |> int)
                Y = world.Ball.Position.Y + ((runState.Elapsed / 10. * world.Ball.Speed * world.Ball.Direction.Y) |> int)
            }
        }
    }

let nextBall (runState: RunState) (world: World) = 
    let botWon = world.Ball.Position.X < -world.Ball.Size.Width
    let playerWon = world.Ball.Position.X > world.Dimension.Width

    match (playerWon, botWon) with
    | (true, _) ->
        { world with
            Player = 
                { world.Player with
                    Score = world.Player.Score + 1
                }
            Ball = newBall (world.Dimension.Width, world.Dimension.Height)
        }
    | (_, true) ->
        { world with
            Bot = 
                { world.Bot with
                    Score = world.Bot.Score + 1
                }
            Ball = newBall (world.Dimension.Width, world.Dimension.Height)
        }
    | _ -> world

let wallCollision (runState: RunState) (world: World) = 
    let topCollision = world.Ball.Position.Y < 0
    let bottomCollision = world.Ball.Position.Y > world.Dimension.Height - world.Ball.Size.Height

    if (topCollision) then
        { world with
            Ball = 
            { world.Ball with
                Direction = 
                    { world.Ball.Direction with
                        Y = world.Ball.Direction.Y |> Math.Abs
                    }
                Speed = world.Ball.Speed + 0.1
            }
        }
    elif (bottomCollision) then
        { world with
            Ball = 
            { world.Ball with
                Direction = 
                    { world.Ball.Direction with
                        Y = -(world.Ball.Direction.Y |> Math.Abs)
                    }
                Speed = world.Ball.Speed + 0.1
            }
        }
    else
        world

let paddleCollision (runState: RunState) (world: World) = 
    let collision (ball: Ball) (paddle: Paddle) =
        let paddleRectangle = Rectangle(paddle.Position.X, paddle.Position.Y, paddle.Size.Width, paddle.Size.Height)
        let ballRectangle = Rectangle(ball.Position.X, ball.Position.Y, ball.Size.Width, ball.Size.Height)

        if (paddleRectangle.Intersects(ballRectangle)) then
            let centerPaddle = 
                Vector2(
                    (paddle.Position.X + paddle.Size.Width / 2) |> float32,
                    (paddle.Position.Y + paddle.Size.Height / 2) |> float32
                )
            let centerBall = 
                Vector2(
                    (ball.Position.X + ball.Size.Width / 2) |> float32,
                    (ball.Position.Y + ball.Size.Height / 2) |> float32
                )

            let diffVector = 
                centerBall - centerPaddle

            let mutable newDirectionVector = 
                diffVector |> Vector2.Normalize

            if (newDirectionVector.X = 0.f) then
                if (ball.Direction.X > 0.) then
                    newDirectionVector <- -Vector2.UnitX
                else
                    newDirectionVector <- Vector2.UnitX

            while (Math.Abs(newDirectionVector.X) < 0.3f) do
                newDirectionVector <- 
                    Vector2.Multiply(newDirectionVector, Vector2(3.f, 1.f)) |> Vector2.Normalize

            { world with
                Ball = 
                { world.Ball with
                    Direction = 
                        { world.Ball.Direction with
                            X = newDirectionVector.X |> float
                            Y = newDirectionVector.Y |> float
                        }
                    Speed = world.Ball.Speed + 0.2
                }
            }
        else
            world

    if world.Ball.Direction.X < 0. then
        collision world.Ball world.Player.Paddle
    else
        collision world.Ball world.Bot.Paddle

let updateModel (runState: RunState) (world: World) =
    [
        nextBall
        wallCollision
        paddleCollision
        movePlayer
        moveBot
        moveBall
    ]
    |> List.scan (fun w f -> f runState w) world
    |> List.last
