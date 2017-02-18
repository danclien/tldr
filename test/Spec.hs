import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "normalize - given test cases" $ do

    it "`../bar` normalizes to `/bar`" $ do
      normalize "../bar" `shouldBe` "/bar"

    it "`/foo/bar` normalizes to `/foo/bar`" $ do
      normalize "/foo/bar" `shouldBe` "/foo/bar"

    it "`/foo/bar/../baz` normalizes to `/foo/baz`" $ do
      normalize "/foo/bar/../baz" `shouldBe` "/foo/baz"

    it "`/foo/bar/./baz/` normalizes to `/foo/bar/baz/`" $ do
      normalize "/foo/bar/./baz/" `shouldBe` "/foo/bar/baz/"

    it "`/foo/../../baz` normalizes to `/baz`" $ do
      normalize "/foo/../../baz" `shouldBe` "/baz"

  describe "normalize - additional test cases" $ do

    it "`foo` normalizes to `/foo`" $ do
      normalize "foo" `shouldBe` "/foo"

    it "`/foo/` normalizes to `/foo/`" $ do
      normalize "/foo/" `shouldBe` "/foo/"

    it "`/foo/bar/..` normalizes to `/foo`" $ do
      normalize "/foo/bar/.." `shouldBe` "/foo"

    it "`./foo` normalizes to `/foo`" $ do
      normalize "./foo" `shouldBe` "/foo"

    it "`/foo/bar/../../baz` normalizes to `/baz`" $ do
      normalize "/foo/bar/../../baz" `shouldBe` "/baz"

    it "`/foo/bar/.././baz/` normalizes to `/foo/baz/`" $ do
      normalize "/foo/bar/.././baz/" `shouldBe` "/foo/baz/"
      
    it "`/foo/././baz` normalizes to `/foo/baz`" $ do
      normalize "/foo/././baz" `shouldBe` "/foo/baz"
