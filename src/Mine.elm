module Mine exposing (Mine, Options, generateRandomMines)

import Random exposing (Generator)
import Set


type alias Mine =
    ( Int, Int )


type alias Options =
    { width : Int
    , height : Int
    }

randomMinesGenerator : Options -> Generator ( Mine )
randomMinesGenerator ({ width, height } as options) =
  Random.pair
    (Random.int 0 (width - 1))
    (Random.int 0 (height - 1))


generateRandomMines : Options -> (( Mine) -> msg) -> Cmd msg
generateRandomMines options msg =
    Random.generate msg (randomMinesGenerator options)
