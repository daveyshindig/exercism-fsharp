module RobotName

let gen = System.Random()
let letters = ['A' .. 'Z']

type Robot = { name: string; }

let mkRobot() =
    let str =  string (letters.[gen.Next(0, 25)]) + string (letters.[gen.Next(0, 25)]) + 
               string (gen.Next(100, 999))
    { name = str }

let name robot = robot.name
let reset (_: Robot) = mkRobot()