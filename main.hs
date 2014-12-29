import Logic
import Prelude hiding ((^), (&&), (||))

main = mapM_ (putStrLn.show) [p && q && r
                             ,p || q || r
                             ,p ==> q
                             ,p <=> q
                             ,p || p
                             ]

