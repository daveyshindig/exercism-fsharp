module SpaceAge

type Planet =
  | Earth
  | Mercury
  | Venus
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

let spaceAge (planet : Planet) (secs : decimal) : decimal =
    let earthYears = secs / 31557600m
    match planet with
    | Earth -> System.Math.Round(earthYears, 2)
    | Mercury -> System.Math.Round(earthYears / 0.2408467m, 2)
    | Venus -> System.Math.Round(earthYears / 0.61519726m, 2)
    | Mars -> System.Math.Round(earthYears / 1.8808158m, 2)
    | Jupiter -> System.Math.Round(earthYears / 11.862615m, 2)
    | Saturn -> System.Math.Round(earthYears / 29.447498m, 2)
    | Uranus -> System.Math.Round(earthYears / 84.016846m, 2)
    | Neptune -> System.Math.Round(earthYears / 164.79132m, 2)