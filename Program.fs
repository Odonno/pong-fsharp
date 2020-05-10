[<EntryPoint>]
let main _ =
    use game = new GameCore.GameLoop()
    game.Run ()
    0