cp {source} {work}/bad.dm && cd {work} && {compiler} check bad.dm; code=$?; ls; exit $code
