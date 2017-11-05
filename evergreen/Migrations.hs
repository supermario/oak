module Migrations where
import qualified Hilt.Postgres
import Data.Text (Text)
import qualified
       Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258
import qualified
       Schema_20170908170731_44b2a37edc1ba22c0539e5a9ead21c4b2adf5898
import qualified
       Schema_20171105162631_3038a271e40837c57156402364c9118dfebd9573
import qualified
       Schema_20171105173923_d7cabc46feefe09136f396fc01c558f7a5b4c31a

allMigrations :: [(Text, Hilt.Postgres.Handle -> IO ())]
allMigrations
  = [("c1474a1f412a2b4bb8f7c24a4ab5b54398825258",
      Schema_20170823221628_c1474a1f412a2b4bb8f7c24a4ab5b54398825258.migration),
     ("44b2a37edc1ba22c0539e5a9ead21c4b2adf5898",
      Schema_20170908170731_44b2a37edc1ba22c0539e5a9ead21c4b2adf5898.migration),
     ("3038a271e40837c57156402364c9118dfebd9573",
      Schema_20171105162631_3038a271e40837c57156402364c9118dfebd9573.migration),
     ("d7cabc46feefe09136f396fc01c558f7a5b4c31a",
      Schema_20171105173923_d7cabc46feefe09136f396fc01c558f7a5b4c31a.migration)]