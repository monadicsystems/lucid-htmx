import Lucid
import Lucid.Htmx
import Test.HUnit
import Test.Hspec


-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "attributes" testAttributes
  describe "standalone" testStandAlone

testAttributes :: Spec
testAttributes = do
  it "boost"     (renderText (div_ [ hxBoost_ "some-text"] "")  `shouldBe`
                  "<div data-hx-boost=\"some-text\"></div>")
  it "get"       (renderText (div_ [ hxGet_ "/some/url"] "")  `shouldBe`
                  "<div data-hx-get=\"/some/url\"></div>")

testStandAlone :: Spec
testStandAlone = do
  it "disable"   (renderText (div_ [ hxDisable_] "")  `shouldBe`
                  "<div data-hx-disable=\"\"></div>")
