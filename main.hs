{-
    imprimiedo 2 paginas por carilla 
      solo las de "front de la hoja"
     2 si, 2 no, y asi...
     -> para un i dado:
     imprimo i,i+1 -> i+2,i+3 NO -> retomo con i+4
-}
takeEvery _    [] = []
takeEvery rate xs = take rate xs : takeEvery rate rest
    where
        rest = drop (2*rate) xs

render xs = tail.init.show $ concat xs

pageTemplate = [ "┌────┐"
               , "│xxxx│"
               , "└────┘" ]

mkPage d = [top, "│" ++ content ++ "│", bottom]
    where
        top     = pageTemplate !! 0
        bottom  = pageTemplate !! 2
        content = padding ++ (show d)
            where
                padding = concat $ take (4 - n) $ repeat " "
                n = length(show d)

-- dada una lista **de pagina** `ps`, crea una pagina
-- composePages ps = 


preview = init.concat $ map (++"\n") $ mkPage 123


separator = "---------"

main = do
    let from  = 204
        to    = 214
        ps    = enumFromTo from to
        rate  = 2
        front = takeEvery rate ps
        back  = takeEvery rate (drop rate ps)

    let toPrint = [ "pages: " ++ show ps
                  , separator
                  , "front: " ++ show front
                  , "back: " ++ show back
                  , separator
                  , "front printable: " ++ render front
                  , "back printable: " ++ render back ]
    
    sequence_ $ map putStrLn toPrint
    putStrLn preview
    

    