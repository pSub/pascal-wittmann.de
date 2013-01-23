import           Application          (makeApplication)
import           Prelude              (IO)
import           Settings             (parseExtra)
import           Yesod.Default.Config (fromArgs)
import           Yesod.Default.Main   (defaultMain)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
