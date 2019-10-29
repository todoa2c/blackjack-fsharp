open System

type Mark = Club | Diamond | Heart | Spade
type Number = int
type Card = Mark * Number
type Deck = Card list

type Turn = Player | Dealer

type Player = {
    Cards: Card list
}
type Dealer = {
    Cards: Card list
}

type GameResult = InProgress | PlayerWon | DealerWon | PlayerBusted | DealerBusted | Draw

// TODO ゲーム中の状態を管理
type BlackjackState = {
    Deck: Deck
    Player: Player
    Dealer: Dealer
    Turn: Turn
    Result: GameResult
}

type Action = Hit | Stand

type Point = Point of int | Bust

let shuffle list =
    let swap (array : _[]) i j =
        let tmp = array.[i]
        array.[i] <- array.[j]
        array.[j] <- tmp

    let shuffle array =
        let rand = Random()
        Array.iteri (fun i _ -> swap array i (rand.Next(i, Array.length array))) array
        array

    list |> List.toArray |> shuffle |> Array.toList

let createDeck =
    [1 .. 13]
    |> List.collect (fun i -> [for m in [Club; Diamond; Heart; Spade] -> m, i])
    |> shuffle

let cardInfo card =
    match card with
    | Club, i -> sprintf "クラブの%i" i
    | Diamond, i -> sprintf "ダイヤの%i" i
    | Heart, i -> sprintf "ハートの%i" i
    | Spade, i -> sprintf "スペードの%i" i

let startGame =
    { Deck=createDeck; Player={Cards=[]}; Dealer={Cards=[]}; Turn=Player; Result=InProgress }

let calcPoint cards =
    let cardPoint (mark, number) =
        match number with
        | 11 | 12 | 13 -> 10
        | _ -> number  // TODO ace can be one or eleven

    match List.sumBy cardPoint cards with
    | point when point > 21 -> Bust
    | point -> Point(point)

let drawOneCard (deck: Deck) : Card * Deck =
    match deck with
    | first :: rest -> first, rest
    | _ -> failwith "No card available"

let drawCard state =
    let card, deck = drawOneCard state.Deck
    match state.Turn with
    | Player -> 
        let cards = card :: state.Player.Cards
        if calcPoint cards = Bust then { state with Deck=deck; Player={Cards=cards}; Result=PlayerBusted}
        else { state with Player={Cards=cards}; Deck=deck }
    | Dealer -> 
        let cards = card :: state.Dealer.Cards
        if calcPoint cards = Bust then { state with Deck=deck; Dealer={Cards=cards}; Result=DealerBusted}
        else { state with Dealer={Cards=cards}; Deck=deck }

let cardLog state =
    match state.Turn with
    | Player -> printfn "あなたの引いたカードは%sです" (cardInfo state.Player.Cards.[0])
    | Dealer -> printfn "ディーラーの引いたカードは%sです" (cardInfo state.Dealer.Cards.[0])
    state

let changeTurn state =
    match state.Turn with
    | Player -> { state with Turn=Dealer}
    | Dealer -> { state with Turn=Player}

let rec humanAction state =
    match state.Result with
    | InProgress ->
        printfn "カードを引きますか？ y/n"
        match Console.ReadLine() with
        | "y" | "Y" -> state |> drawCard |> cardLog |> humanAction
        | "n" | "N" -> state |> changeTurn 
        | _ -> humanAction state
    | _ -> { state with Turn=Dealer }

let judge state =
    let humanPoint = calcPoint state.Player.Cards
    let dealerPoint = calcPoint state.Dealer.Cards
    match humanPoint, dealerPoint with
    | Bust, _ -> PlayerBusted
    | _, Bust -> DealerBusted
    | human, dealer when human > dealer -> PlayerWon
    | human, dealer when human < dealer -> DealerWon
    | _, _ -> Draw

let rec dealerAction state =
    let point = calcPoint state.Dealer.Cards
    match point with
    | Bust -> { state with Result=DealerBusted }
    | Point point ->
        if point < 17 then state |> drawCard |> cardLog |> dealerAction
        else { state with Result=(judge state)} 

let printJudgement result =
    match result with
    | PlayerBusted -> printfn "あなたの負けです (バスト)"
    | DealerBusted -> printfn "あなたの勝ちです (ディーラーバスト)"
    | Draw -> printfn "引き分けです"
    | PlayerWon -> printfn "あなたの勝ちです"
    | DealerWon -> printfn "あなたの負けです"
    | InProgress -> failwith "まだ決着が付いていません"

[<EntryPoint>]
let main argv =
    let state = startGame
    let humanEnd = state |> drawCard |> cardLog |> drawCard |> cardLog |> changeTurn
    let humanNeedAction = humanEnd |> drawCard |> cardLog |> drawCard |> changeTurn

    let humanActionDone = humanAction humanNeedAction
    match humanActionDone.Result with
    | PlayerBusted -> printJudgement humanActionDone.Result
    | _ ->
        let dealerDone = humanActionDone |> cardLog |> dealerAction
        printJudgement dealerDone.Result
    0
