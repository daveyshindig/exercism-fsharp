﻿module IsogramTest
    
open NUnit.Framework

open Isogram

[<Test>]
let ``Empty string`` () =
    let input = ""
    let expected = true
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Isogram with only lower case characters`` () =
    let input = "isogram"
    let expected = true
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Word with one duplicated character`` () =
    let input = "eleven"
    let expected = false
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Longest reported english isogram`` () =
    let input = "subdermatoglyphic"
    let expected = true
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Word with duplicated character in mixed case`` () =
    let input = "Alphabet"
    let expected = false
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Hypothetical isogrammic word with hyphen`` () =
    let input = "thumbscrew-japingly"
    let expected = true
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Isogram with duplicated non letter character`` () =
    let input = "Hjelmqvist-Gryb-Zock-Pfund-Wax"
    let expected = true
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Made-up name that is an isogram`` () =
    let input = "Emily Jung Schwartzkopf"
    let expected = true
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Word with duplicated accented character`` () =
    let input = "éléphant"
    let expected = false
    let actual = isogram input
    Assert.That(actual, Is.EqualTo(expected))