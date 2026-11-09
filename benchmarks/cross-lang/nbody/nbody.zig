// N-body simulation of the solar system (5 bodies)
// Tests struct operations and floating-point math

const std = @import("std");
const math = std.math;

const pi: f64 = 3.141592653589793;
const solar_mass: f64 = 4.0 * pi * pi;
const days_per_year: f64 = 365.24;
const nbodies: usize = 5;

const Body = struct {
    x: f64,
    y: f64,
    z: f64,
    vx: f64,
    vy: f64,
    vz: f64,
    mass: f64,
};

fn advance(bodies: *[nbodies]Body, dt: f64) void {
    for (0..nbodies) |i| {
        for ((i + 1)..nbodies) |j| {
            const dx = bodies[i].x - bodies[j].x;
            const dy = bodies[i].y - bodies[j].y;
            const dz = bodies[i].z - bodies[j].z;
            const dist = @sqrt(dx * dx + dy * dy + dz * dz);
            const mag = dt / (dist * dist * dist);
            bodies[i].vx -= dx * bodies[j].mass * mag;
            bodies[i].vy -= dy * bodies[j].mass * mag;
            bodies[i].vz -= dz * bodies[j].mass * mag;
            bodies[j].vx += dx * bodies[i].mass * mag;
            bodies[j].vy += dy * bodies[i].mass * mag;
            bodies[j].vz += dz * bodies[i].mass * mag;
        }
    }
    for (0..nbodies) |i| {
        bodies[i].x += dt * bodies[i].vx;
        bodies[i].y += dt * bodies[i].vy;
        bodies[i].z += dt * bodies[i].vz;
    }
}

fn energy(bodies: *const [nbodies]Body) f64 {
    var e: f64 = 0.0;
    for (0..nbodies) |i| {
        e += 0.5 * bodies[i].mass *
            (bodies[i].vx * bodies[i].vx +
            bodies[i].vy * bodies[i].vy +
            bodies[i].vz * bodies[i].vz);
        for ((i + 1)..nbodies) |j| {
            const dx = bodies[i].x - bodies[j].x;
            const dy = bodies[i].y - bodies[j].y;
            const dz = bodies[i].z - bodies[j].z;
            const dist = @sqrt(dx * dx + dy * dy + dz * dz);
            e -= bodies[i].mass * bodies[j].mass / dist;
        }
    }
    return e;
}

pub fn main() !void {
    var bodies = [nbodies]Body{
        // Sun
        Body{ .x = 0.0, .y = 0.0, .z = 0.0, .vx = 0.0, .vy = 0.0, .vz = 0.0, .mass = solar_mass },
        // Jupiter
        Body{
            .x = 4.84143144246472090e+00,
            .y = -1.16032004402742839e+00,
            .z = -1.03622044471123109e-01,
            .vx = 1.66007664274403694e-03 * days_per_year,
            .vy = 7.69901118419740425e-03 * days_per_year,
            .vz = -6.90460016972063023e-05 * days_per_year,
            .mass = 9.54791938424326609e-04 * solar_mass,
        },
        // Saturn
        Body{
            .x = 8.34336671824457987e+00,
            .y = 4.12479856412430479e+00,
            .z = -4.03523417114321381e-01,
            .vx = -2.76742510726862411e-03 * days_per_year,
            .vy = 4.99852801234917238e-03 * days_per_year,
            .vz = 2.30417297573763929e-05 * days_per_year,
            .mass = 2.85885980666130812e-04 * solar_mass,
        },
        // Uranus
        Body{
            .x = 1.28943695621391310e+01,
            .y = -1.51111514016986312e+01,
            .z = -2.23307578892655734e-01,
            .vx = 2.96460137564761618e-03 * days_per_year,
            .vy = 2.37847173959480950e-03 * days_per_year,
            .vz = -2.96589568540237556e-05 * days_per_year,
            .mass = 4.36624404335156298e-05 * solar_mass,
        },
        // Neptune
        Body{
            .x = 1.53796971148509165e+01,
            .y = -2.59193146099879641e+01,
            .z = 1.79258772950371181e-01,
            .vx = 2.68067772490389322e-03 * days_per_year,
            .vy = 1.62824170038242295e-03 * days_per_year,
            .vz = -9.51592254519715870e-05 * days_per_year,
            .mass = 5.15138902046611451e-05 * solar_mass,
        },
    };

    // Offset momentum
    var px: f64 = 0.0;
    var py: f64 = 0.0;
    var pz: f64 = 0.0;
    for (0..nbodies) |i| {
        px += bodies[i].vx * bodies[i].mass;
        py += bodies[i].vy * bodies[i].mass;
        pz += bodies[i].vz * bodies[i].mass;
    }
    bodies[0].vx = -px / solar_mass;
    bodies[0].vy = -py / solar_mass;
    bodies[0].vz = -pz / solar_mass;

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d:.9}\n", .{energy(&bodies)});

    const n: usize = 5000000;
    const dt: f64 = 0.01;
    for (0..n) |_| {
        advance(&bodies, dt);
    }

    try stdout.print("{d:.9}\n", .{energy(&bodies)});
}
