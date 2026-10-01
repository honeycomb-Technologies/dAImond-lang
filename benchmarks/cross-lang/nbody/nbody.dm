module nbody_bench

-- N-body simulation of the solar system (5 bodies)
-- Tests struct operations and floating-point math

extern fn sqrt(x: float) -> float

struct Body {
    x: float,
    y: float,
    z: float,
    vx: float,
    vy: float,
    vz: float,
    mass: float
}

fn advance(bodies: List[Body], dt: float) -> List[Body] {
    let mut bs = bodies
    let nbodies = 5

    let mut i = 0
    while i < nbodies {
        let mut j = i + 1
        while j < nbodies {
            let mut bi = bs[i]
            let mut bj = bs[j]
            let dx = bi.x - bj.x
            let dy = bi.y - bj.y
            let dz = bi.z - bj.z
            let dist = sqrt(dx * dx + dy * dy + dz * dz)
            let mag = dt / (dist * dist * dist)
            bi.vx = bi.vx - dx * bj.mass * mag
            bi.vy = bi.vy - dy * bj.mass * mag
            bi.vz = bi.vz - dz * bj.mass * mag
            bj.vx = bj.vx + dx * bi.mass * mag
            bj.vy = bj.vy + dy * bi.mass * mag
            bj.vz = bj.vz + dz * bi.mass * mag
            bs[i] = bi
            bs[j] = bj
            j = j + 1
        }
        i = i + 1
    }

    let mut k = 0
    while k < nbodies {
        let mut b = bs[k]
        b.x = b.x + dt * b.vx
        b.y = b.y + dt * b.vy
        b.z = b.z + dt * b.vz
        bs[k] = b
        k = k + 1
    }

    return bs
}

fn energy(bodies: List[Body]) -> float {
    let nbodies = 5
    let mut e = 0.0

    let mut i = 0
    while i < nbodies {
        let bi = bodies[i]
        e = e + 0.5 * bi.mass * (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz)
        let mut j = i + 1
        while j < nbodies {
            let bj = bodies[j]
            let dx = bi.x - bj.x
            let dy = bi.y - bj.y
            let dz = bi.z - bj.z
            let dist = sqrt(dx * dx + dy * dy + dz * dz)
            e = e - bi.mass * bj.mass / dist
            j = j + 1
        }
        i = i + 1
    }

    return e
}

fn main() {
    let pi = 3.141592653589793
    let solar_mass = 4.0 * pi * pi
    let days_per_year = 365.24

    let mut bodies: List[Body] = []

    -- Sun
    bodies.push(Body {
        x: 0.0, y: 0.0, z: 0.0,
        vx: 0.0, vy: 0.0, vz: 0.0,
        mass: solar_mass
    })

    -- Jupiter
    bodies.push(Body {
        x: 4.84143144246472090e+00,
        y: -1.16032004402742839e+00,
        z: -1.03622044471123109e-01,
        vx: 1.66007664274403694e-03 * days_per_year,
        vy: 7.69901118419740425e-03 * days_per_year,
        vz: -6.90460016972063023e-05 * days_per_year,
        mass: 9.54791938424326609e-04 * solar_mass
    })

    -- Saturn
    bodies.push(Body {
        x: 8.34336671824457987e+00,
        y: 4.12479856412430479e+00,
        z: -4.03523417114321381e-01,
        vx: -2.76742510726862411e-03 * days_per_year,
        vy: 4.99852801234917238e-03 * days_per_year,
        vz: 2.30417297573763929e-05 * days_per_year,
        mass: 2.85885980666130812e-04 * solar_mass
    })

    -- Uranus
    bodies.push(Body {
        x: 1.28943695621391310e+01,
        y: -1.51111514016986312e+01,
        z: -2.23307578892655734e-01,
        vx: 2.96460137564761618e-03 * days_per_year,
        vy: 2.37847173959480950e-03 * days_per_year,
        vz: -2.96589568540237556e-05 * days_per_year,
        mass: 4.36624404335156298e-05 * solar_mass
    })

    -- Neptune
    bodies.push(Body {
        x: 1.53796971148509165e+01,
        y: -2.59193146099879641e+01,
        z: 1.79258772950371181e-01,
        vx: 2.68067772490389322e-03 * days_per_year,
        vy: 1.62824170038242295e-03 * days_per_year,
        vz: -9.51592254519715870e-05 * days_per_year,
        mass: 5.15138902046611451e-05 * solar_mass
    })

    -- Offset momentum
    let mut px = 0.0
    let mut py = 0.0
    let mut pz = 0.0
    let mut i = 0
    while i < 5 {
        let b = bodies[i]
        px = px + b.vx * b.mass
        py = py + b.vy * b.mass
        pz = pz + b.vz * b.mass
        i = i + 1
    }
    let mut sun = bodies[0]
    sun.vx = 0.0 - px / solar_mass
    sun.vy = 0.0 - py / solar_mass
    sun.vz = 0.0 - pz / solar_mass
    bodies[0] = sun

    println(float_to_string(energy(bodies)))

    let n = 5000000
    let dt = 0.01
    let mut step = 0
    while step < n {
        bodies = advance(bodies, dt)
        step = step + 1
    }

    println(float_to_string(energy(bodies)))
}
