cp {source} {work}/f.dm && cd {work} && {compiler} fmt f.dm && cat f.dm && {compiler} fmt f.dm -o g.dm && cmp f.dm g.dm
