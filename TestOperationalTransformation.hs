import OperationalTransformation

import Test.QuickCheck

import Control.Monad (liftM, liftM2)

instance Arbitrary Action where
  arbitrary = oneof [ liftM (Skip . (+1) . abs) arbitrary
                    , liftM Insert arbitrary
                    , liftM Delete arbitrary
                    ]

data StringOperationPair = SOP String Operation deriving (Show)

instance Arbitrary StringOperationPair where
  arbitrary = do
    string <- arbitrary
    operation <- arbitraryOperation string
    return $ SOP string operation

-- | has at least 1 element
arbitraryList :: (Arbitrary a) => Gen [a]
arbitraryList = liftM2 (:) arbitrary arbitrary

arbitraryOperation :: String -> Gen Operation
arbitraryOperation "" = oneof [return [], liftM ((:[]) . Insert) arbitraryList]
arbitraryOperation s = do
  len <- choose (1, (min 10 (length s)))
  oneof [ liftM ((Skip len):) $ arbitraryOperation (drop len s)
        , do s2 <- liftM2 (:) arbitrary arbitrary -- make sure that the string a length of at least one
             next <- arbitraryOperation s
             return $ (Insert s2):next
        , do let (before, after) = splitAt len s
             next <- arbitraryOperation after
             return $ (Delete before):next
        ]

data StringOperationTriple = SOT String Operation Operation deriving (Show)

instance Arbitrary StringOperationTriple where
  arbitrary = do
    (SOP string operation1) <- arbitrary
    operation2 <- arbitraryOperation (apply operation1 string)
    return $ SOT string operation1 operation2

data StringConcurrentOperationTriple = SCOT String Operation Operation deriving (Show)

instance Arbitrary StringConcurrentOperationTriple where
  arbitrary = do
    string <- arbitrary
    operationA <- arbitraryOperation string
    operationB <- arbitraryOperation string
    return $ SCOT string operationA operationB

deltaLength :: Operation -> Int
deltaLength = sum . map len
  where len (Skip _)   = 0
        len (Insert i) = length i
        len (Delete d) = -(length d)

prop_length :: StringOperationPair -> Bool
prop_length (SOP str op) = length (apply op str) == length str + deltaLength op

prop_merge_length :: StringOperationTriple -> Bool
prop_merge_length (SOT _ a b) = deltaLength a + deltaLength b == deltaLength (merge a b)

prop_merge_well_formed :: StringOperationTriple -> Bool
prop_merge_well_formed (SOT _ a b) = wellFormed $ merge a b

wellFormed :: Operation -> Bool
wellFormed = all (not . nullLength)
  where nullLength (Skip n) = n == 0
        nullLength (Insert i) = i == ""
        nullLength (Delete d) = d == ""

prop_merge_apply :: StringOperationTriple -> Bool
prop_merge_apply (SOT s a b) = apply b (apply a s) == apply (merge a b) s

prop_xform_length :: StringConcurrentOperationTriple -> Bool
prop_xform_length (SCOT _ a b) = let (a', b') = xform a b
                                 in deltaLength a + deltaLength b' == deltaLength b + deltaLength a'

prop_xform_apply :: StringConcurrentOperationTriple -> Bool
prop_xform_apply (SCOT s a b) = let (a', b') = xform a b
                                in (apply b' . apply a $ s) == (apply a' . apply b $ s)

main :: IO ()
main = do
  quickCheck prop_length
  quickCheck prop_merge_length
  quickCheck prop_merge_well_formed
  quickCheck prop_merge_apply
  quickCheck prop_xform_length
  quickCheck prop_xform_apply
