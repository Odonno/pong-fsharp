module Model

open System
open System.Numerics

type Position = {
    X: int;
    Y: int;
}
type Size = {
    Width: int;
    Height: int;
}
type Direction = {
    X: float;
    Y: float;
}

type Paddle = {
    Size: Size;
    Position: Position;
    Speed: float;
}

type Ball = {
    Size: Size;
    Position: Position;
    Direction: Direction;
    Speed: float;
}

type Player = {
    Score: int;
    Paddle: Paddle;
}

type World = {
    Dimension: Size;
    Player: Player;
    Bot: Player;
    Ball: Ball;
}

let newBall (width, height) = 
    let randomDirection () =
        let random = Random()
        let mutable vector = Vector2.Zero

        while (Math.Abs(vector.X) < 0.3f) do
            vector <-
                Vector2(
                    ((random.NextDouble() * 2. - 1.) * 3.) |> float32,
                    (random.NextDouble() * 2. - 1.) |> float32
                ) |> Vector2.Normalize

        {
            X = vector.X |> float
            Y = vector.Y |> float
        }

    {
        Size = {
            Width = 10
            Height = 10
        }
        Position = {
            X = width / 2 - 5
            Y = height / 2 - 5
        }
        Direction = randomDirection()
        Speed = 3.
    }

let initialWorld (width, height) = 
    {
        Dimension = {
            Width = width
            Height = height
        }
        Player = {
            Score = 0
            Paddle = {
                Size = {
                    Width = 10
                    Height = 60
                }
                Position = {
                    X = 20
                    Y = height / 2 - 30
                }
                Speed = 5.
            }
        }
        Bot = {
            Score = 0
            Paddle = {
                Size = {
                    Width = 10
                    Height = 60
                }
                Position = {
                    X = width - 20 - 10
                    Y = height / 2 - 30
                }
                Speed = 5.
            }
        }
        Ball = newBall (width, height)
    }