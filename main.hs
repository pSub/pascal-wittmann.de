import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withHomepage)

main :: IO ()
main = defaultMain fromArgs withHomepage