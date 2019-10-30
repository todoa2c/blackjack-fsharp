open System

type Mark = Club | Diamond | Heart | Spade
type Number = int
type Card = Mark * Number
type Deck = Card list

type Point = Point of int | Bust

type Cards = Card list

type StandCards = Cards * Point

type Player = 
    | Cards of Cards
    | StandCards of StandCards

type Dealer =
    | Cards of Cards
    | StandCards of StandCards

type BlackjackInitialized = {
    Deck: Deck
    PlayerTurn: Cards
    Dealer: Cards
}
type BlackjackPlayerEnd = {
    Deck: Deck
    PlayerEnd: StandCards
    DealerTurn: Cards
}
type BlackjackDealerEnd = {
    Deck: Deck
    PlayerEnd: StandCards
    DealerEnd: StandCards
}

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

let calcPoint cards =
    let cardPoint (_, number) =
        match number with
        | 11 | 12 | 13 -> 10
        | _ -> number

    let pointsWithoutAce = cards |> List.map cardPoint |> List.filter (fun p -> p > 1)
    let aceNum = (List.length cards) - (List.length pointsWithoutAce)
    let pointWithoutAceSum = pointsWithoutAce |> List.sum
    let pointsWithAce1, pointsWithAce11 = pointWithoutAceSum + aceNum, pointWithoutAceSum + aceNum + 10

    match aceNum, pointsWithAce1, pointsWithAce11 with
    | 0, _, _ when pointWithoutAceSum <= 21-> Point pointWithoutAceSum
    | _, _, p11 when p11 <= 21 -> Point p11
    | _, p1, _ when p1 <= 21 -> Point p1
    | _, _, _ -> Bust

let rec drawCards deck n =
    match deck, n with
    | deck, n when n <= 0 -> [], deck
    | [], n -> failwith "ドローできるカードがありません"
    | first :: rest, n -> 
        let cards, _deck = drawCards rest (n - 1)
        first :: cards, _deck

let rec playerAction state =
    let playerPoint = calcPoint state.PlayerTurn
    match playerPoint with
    | Bust -> {Deck=state.Deck; PlayerEnd=(state.PlayerTurn, Bust); DealerTurn=state.Dealer }
    | Point p ->
        printfn "カードを引きますか？ y/n"
        match Console.ReadLine() with
        | "y" | "Y" ->
            let cards, deck = drawCards state.Deck 1
            printfn "あなたの引いたカードは%sです" (cardInfo cards.[0])
            playerAction {Deck=deck; PlayerTurn=cards @ state.PlayerTurn; Dealer=state.Dealer }
        | "n" | "N" -> {Deck=state.Deck; PlayerEnd=(state.PlayerTurn, playerPoint); DealerTurn=state.Dealer }
        | _ -> playerAction state

let rec dealerAction (state : BlackjackPlayerEnd) : BlackjackDealerEnd =
    let point = calcPoint state.DealerTurn
    match point with
    | Bust -> { Deck=state.Deck; PlayerEnd=state.PlayerEnd; DealerEnd=(state.DealerTurn, point) }
    | Point point when point >= 17 -> { Deck=state.Deck; PlayerEnd=state.PlayerEnd; DealerEnd=(state.DealerTurn, Point point) }
    | _ ->
        let cards, deck = drawCards state.Deck 1
        printfn "ディーラーの引いたカードは%sです" (cardInfo cards.[0])
        dealerAction { Deck=deck; PlayerEnd=state.PlayerEnd; DealerTurn=cards @ state.DealerTurn }

let printJudgement result =
    let _, playerPoint = result.PlayerEnd
    let _, dealerPoint = result.DealerEnd
    match playerPoint, dealerPoint with
    | Bust, _ -> printfn "あなたの負けです (バスト)"
    | _, Bust -> printfn "あなたの勝ちです (ディーラーバスト)"
    | p, d when p = d -> printfn "引き分けです"
    | p, d when p > d -> printfn "あなたの勝ちです"
    | _ -> printfn "あなたの負けです"

let initialize : BlackjackInitialized =
    let deck = createDeck
    let playerCards, deck = drawCards deck 2
    let dealerCards, deck = drawCards deck 2
    { Deck=deck; PlayerTurn=playerCards; Dealer=dealerCards }

let initializedMessage (initialized : BlackjackInitialized) =
    printfn "あなたの引いたカードは%sです" (cardInfo initialized.PlayerTurn.[0])
    printfn "あなたの引いたカードは%sです" (cardInfo initialized.PlayerTurn.[1])
    printfn "ディーラーの引いたカードは%sです" (cardInfo initialized.Dealer.[0])
    initialized

[<EntryPoint>]
let main argv =
    let playerActionWaiting = initialize |> initializedMessage
    let playerActionDone = playerAction playerActionWaiting
    match playerActionDone.PlayerEnd with
    | _, Bust -> printfn "あなたの負けです(バスト)"
    | _, _ ->
        printfn "ディーラーの引いたカードは%sです" (cardInfo playerActionDone.DealerTurn.[0])
        let waitForResult = dealerAction playerActionDone
        printJudgement waitForResult

    0
