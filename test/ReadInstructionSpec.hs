import Test.Hspec

import           Language.Paradocs.Renderer
import           Language.Paradocs.RendererState
import           Language.Paradocs.MonadStorage
import qualified Data.HashMap.Strict                as HashMap

main :: IO ()
main = hspec $ do
  describe "%read" $ do
    let storage = HashMap.fromList [
                      ("a.pd", "content")
                    ]

    let rendered = runHashMapStorage (renderString "%read a.pd") storage

    it "reads the content of a.pd" $ do
      renderedToString rendered `shouldBe` "content"
