open System

type Mark = Club | Diamond | Heart | Spade
type Number = int
type Card = Mark * Number
type Deck = Card list

type Point =
    | Point of int
    | Bust

type Cards = Card list

type StandCards = Cards * Point

type BlackjackInitialized = {
    Deck: Deck
    PlayerTurn: Cards
    Dealer: Cards
}
type BlackjackDealerTurn = {
    Deck: Deck
    PlayerEnd: StandCards
    DealerTurn: Cards
}
type BlackjackPlayerEnd = 
    | PlayerBustEnd
    | BlackjackDealerTurn of BlackjackDealerTurn

type BlackjackJudge = {
    PlayerEnd: StandCards
    DealerEnd: StandCards
}

type Result =
    | PlayerBust
    | DealerBust
    | Judge of BlackjackJudge

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
    | [], _ -> failwith "ドローできるカードがありません"
    | first :: rest, n -> 
        let cards, _deck = drawCards rest (n - 1)
        first :: cards, _deck

let rec playerAction state =
    let playerPoint = calcPoint state.PlayerTurn
    match playerPoint with
    | Bust -> PlayerBustEnd
    | Point _ ->
        printfn "カードを引きますか？ y/n"
        match Console.ReadLine() with
        | "y" | "Y" ->
            let cards, deck = drawCards state.Deck 1
            printfn "あなたの引いたカードは%sです" (cardInfo cards.[0])
            playerAction {Deck=deck; PlayerTurn=cards @ state.PlayerTurn; Dealer=state.Dealer }
        | "n" | "N" -> BlackjackDealerTurn {Deck=state.Deck; PlayerEnd=(state.PlayerTurn, playerPoint); DealerTurn=state.Dealer }
        | _ -> playerAction state

let rec dealerAction state =
    printfn "ディーラーの引いたカードは%sです" (cardInfo state.DealerTurn.[0])
    let point = calcPoint state.DealerTurn
    match point with
    | Bust -> DealerBust
    | Point point when point >= 17 -> Judge { PlayerEnd=state.PlayerEnd; DealerEnd=(state.DealerTurn, Point point) }
    | _ ->
        let cards, deck = drawCards state.Deck 1
        dealerAction { Deck=deck; PlayerEnd=state.PlayerEnd; DealerTurn=cards @ state.DealerTurn }

let printJudgement result =
    match result with
    | PlayerBust -> printfn "あなたの負けです (バスト)"
    | DealerBust -> printfn "あなたの勝ちです (ディーラーバスト)"
    | Judge judge ->
        let _, playerPoint = judge.PlayerEnd
        let _, dealerPoint = judge.DealerEnd
        match playerPoint, dealerPoint with
        | p, d when p > d -> printfn "あなたの勝ちです"
        | p, d when p < d -> printfn "あなたの負けです"
        | _, _ -> printfn "引き分けです"

let initialize =
    let deck = createDeck
    let playerCards, deck = drawCards deck 2
    let dealerCards, deck = drawCards deck 2
    { Deck=deck; PlayerTurn=playerCards; Dealer=dealerCards }

let printStartMessage initialized =
    printfn "あなたの引いたカードは%sです" (cardInfo initialized.PlayerTurn.[0])
    printfn "あなたの引いたカードは%sです" (cardInfo initialized.PlayerTurn.[1])
    printfn "ディーラーの引いたカードは%sです" (cardInfo initialized.Dealer.[1])
    initialized

[<EntryPoint>]
let main _ =
    let playerActionWaiting = initialize |> printStartMessage
    let playerActionDone = playerAction playerActionWaiting
    match playerActionDone with
    | PlayerBustEnd -> printJudgement PlayerBust
    | BlackjackDealerTurn turn ->
        let waitForResult = dealerAction turn
        printJudgement waitForResult

    0
