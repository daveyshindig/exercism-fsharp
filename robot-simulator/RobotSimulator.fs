module RobotSimulator

type Bearing = North | East | South | West

let createRobot bearing (x, y) = (bearing, (x, y))

let turnRight robot =
    match robot with
    | (North, (x, y)) -> (East, (x, y))
    | (East, (x, y)) -> (South, (x, y))
    | (South, (x, y)) -> (West, (x, y))
    | (West, (x, y)) -> (North, (x, y))

let turnLeft robot =
    match robot with
    | (North, (x, y)) -> (West, (x, y))
    | (East, (x, y)) -> (North, (x, y))
    | (South, (x, y)) -> (East, (x, y))
    | (West, (x, y)) -> (South, (x, y))

let advanceRobot robot =
    match robot with
    | (North, (x, y)) -> (North, (x, y + 1))
    | (East, (x, y)) -> (East, (x + 1, y))
    | (South, (x, y)) -> (South, (x, y - 1))
    | (West, (x, y)) -> (West, (x - 1, y))

let simulate robot cmds = 
    let ls = [for c in cmds -> c]
    let rec simrec r cmds =
        match cmds with
        | [] -> r
        | 'R' :: tl -> simrec (turnRight r) tl
        | 'L' :: tl -> simrec (turnLeft r) tl
        | 'A' :: tl -> simrec (advanceRobot r) tl
        | _ -> failwith "SYNTAX ERROR: ROBOT DOES NOT UNDERSTAND"
    simrec robot ls