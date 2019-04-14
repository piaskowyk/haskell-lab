silnia n = if n == 0 then 1
            else n * silnia (n - 1)

nieporzadek n = if n == 0 then 1
                else if n == 1 then 0
                else (n - 1) * (nieporzadek (n - 1) + nieporzadek(n - 2))
