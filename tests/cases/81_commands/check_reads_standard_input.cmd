printf "module helper\nfn helper_value() -> int {\n    return 1\n}\n" > {work}/helper.dm && cd {work} && {compiler} check unsaved.dm --stdin < {source}
