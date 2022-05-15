namespace fsandbox

module Common =
    let curry f x y = f (x, y)

    let uncurry f (x, y) = f x y

    let curry3 f x y z = f (x, y, z)

    let uncurry3 f (x, y, z) = f x y z
    
    let rec fix f x = f (fix f) x