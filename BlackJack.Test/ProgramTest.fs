module BlackJackTest

open NUnit.Framework
open Program

[<Test>]
let testCreateDeck () =
    let deck = createDeck
    Assert.That((List.length deck), Is.EqualTo(52))

[<Test>]
let testCalcPoints () =
    Assert.That(calcPoint [(Club, 1)], Is.EqualTo(Point(11)))
    Assert.That(calcPoint [(Club, 1); (Diamond, 1)], Is.EqualTo(Point(12)))
    Assert.That(calcPoint [(Club, 1); (Diamond, 1); (Heart, 1)], Is.EqualTo(Point(13)))
    Assert.That(calcPoint [(Club, 1); (Diamond, 1); (Heart, 1); (Spade, 1)], Is.EqualTo(Point(14)))

    Assert.That(calcPoint [(Club, 1); (Diamond, 13)], Is.EqualTo(Point(21)))
    Assert.That(calcPoint [(Club, 1); (Diamond, 1); (Heart, 8)], Is.EqualTo(Point(20)))
