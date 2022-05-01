module Development.IDE.Types.Monitoring
(Monitoring(..)
) where

import           Data.Int
import           Data.Text                (Text)

-- | An abstraction for runtime monitoring inspired by the 'ekg' package
data Monitoring = Monitoring {
    -- | Register an integer-valued metric.
    registerGauge   :: Text -> IO Int64 -> IO (),
    -- | Register a non-negative, monotonically increasing, integer-valued metric.
    registerCounter :: Text -> IO Int64 -> IO (),
    start :: IO (IO ())
  }

instance Semigroup Monitoring where
    a <> b = Monitoring {
        registerGauge   = \n v -> registerGauge a n v >> registerGauge b n v,
        registerCounter = \n v -> registerCounter a n v >> registerCounter b n v,
        start = do
            a' <- start a
            b' <- start b
            return $ a' >> b'
      }

instance Monoid Monitoring where
    mempty = Monitoring {
        registerGauge   = \_ _ -> return (),
        registerCounter = \_ _ -> return (),
        start = return $ return ()
      }
