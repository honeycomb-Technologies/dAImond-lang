cd {work} && {compiler} run {source} -- "a b" "it's" '$HOME' -o; code=$?; ls; exit $code
