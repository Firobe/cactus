(lang dune 2.8)
(name cactus-heater)

(generate_opam_files true)

(source (github Firobe/cactus))
(license MIT)
(authors "Virgile Robles")
(maintainers "virgile.robles@pm.me")

(package
    (name cactus-heater)
    (synopsis "Heater regulator")
    (description "Heater regulator")
    (depends
        gpiod
        ppx_deriving
        lwt
        i2c
        (dune :build)
    )
)

