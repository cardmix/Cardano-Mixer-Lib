{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module MixerProofs.Groth16 where

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Map                         (fromList)
import           Data.Text                        (Text, unpack)
import           GHC.Generics                     (Generic)
import           Plutus.V1.Ledger.Value           (Value)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), (<*>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          (Show (..), IO, (<$>), (<*>), (^))
import           Text.Read                        (readMaybe)

import           Crypto
import           Utils.Common                     (replicate, last, init, drop)

-------------------------------------- User Data ------------------------------------------------------

data DepositSecret = DepositSecret
    {
        getR1 :: Fr,
        getR2 :: Fr
    } deriving (Generic, FromJSON, ToJSON)

instance Show DepositSecret where
    show (DepositSecret r1 r2) = show $ fromZp r1 * fieldPrime R + fromZp r2

readDepositSecret :: Text -> Maybe DepositSecret
readDepositSecret str = do
        n <- readMaybe (unpack str) :: Maybe Integer
        let (r1, r2) = divMod n (fieldPrime R)
        return $ DepositSecret (toZp r1) (toZp r2)

generateDepositSecret :: IO DepositSecret
generateDepositSecret = DepositSecret <$> generateZp <*> generateZp

data ShieldedAccountSecret = ShieldedAccountSecret
    {
        getV1 :: Fr,
        getV2 :: Fr,
        getV3 :: Fr
    } deriving (Show, Generic, FromJSON, ToJSON)

generateShieldedAccountSecret :: IO ShieldedAccountSecret
generateShieldedAccountSecret = ShieldedAccountSecret <$> generateZp <*> generateZp <*> generateZp

data WithdrawParams = WithdrawParams
    {
        wpAddress       :: !Text,
        wpValue         :: !Value,
        wpDepositNum    :: !(Integer, Integer),
        wpPublicInputs  :: !PublicInputs,
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)

---------------------------------------- Mixer State ---------------------------------------------------

treeSize :: Integer
treeSize = 10

data MerkleTree = MerkleTree Integer [Fr]
    deriving (Show, Generic, ToJSON, FromJSON)
type MixerState = [MerkleTree]

getMerkleLeafNumber :: MixerState -> Fr -> Maybe (Integer, Integer)
getMerkleLeafNumber state leaf = do
        k <- nTree state
        m <- nDep $ state !! k
        return (k, m + 1)
    where nDep  (MerkleTree _ tree) = findIndex (leaf ==) tree
          nTree s                   = findIndex ((/=) Nothing . nDep) s

getMerkleTree :: MixerState -> (Integer, Integer) -> Maybe MerkleTree
getMerkleTree state (k, m) = do
    MerkleTree _ tree <- if length state <= k
                            then Nothing
                            else Just $ state !! k
    if 1 > m || m > 2^treeSize
        then Nothing
        else Just $ MerkleTree m $ padToPowerOfTwo treeSize $ take m tree

constructStateFromList :: ([Fr], MixerState) -> ([Fr], MixerState)
constructStateFromList ([], state)  = ([], state)
constructStateFromList (lst, state) = constructStateFromList (drop (2 ^ treeSize) lst, state ++ [MerkleTree n tree'])
    where tree  = take (2 ^ treeSize) lst
          tree' = padToPowerOfTwo treeSize tree
          n     = length tree

------------------------------------ Withdraw Proof ---------------------------------------------------

-- Public signals manipulations

toWithdrawPublicSignals :: PublicInputs -> PublicSignals
toWithdrawPublicSignals (PublicInputs subs) = PublicSignals $ [one, zero, zero, zero, zero, zero] ++ subs

fromWithdrawPublicSingals :: PublicSignals -> PublicInputs
fromWithdrawPublicSingals (PublicSignals subs) = PublicInputs $ drop 6 subs

isWithdrawPublicInputs :: PublicInputs -> Bool
isWithdrawPublicInputs (PublicInputs subs) = length subs == 7

getWithdrawRootInput :: PublicInputs -> Fr
getWithdrawRootInput (PublicInputs subs) = head subs

getWithdrawAddressInput :: PublicInputs -> Fr
getWithdrawAddressInput (PublicInputs subs) = subs !! 1

getWithdrawKeyInput :: PublicInputs -> Fr
getWithdrawKeyInput (PublicInputs subs) = subs !! 2

-- Proving algorithms

generateWithdrawProof :: SetupArguments -> ReferenceString ->
    ZKProofSecret -> Fr -> DepositSecret -> ShieldedAccountSecret -> MixerState -> ((Integer, Integer), PublicInputs, Proof)
generateWithdrawProof sa@(SetupArguments r1cs _) crs secret pkh ds sas state = (lastDeposit, PublicInputs insPub, prove secret pa)
    where
        (lastDeposit, _, insPub, insPriv) = computeWithdrawWires pkh ds sas state
        -- constructing witness
        w  = fromList $ zip ((0 :: Integer) : [6..37]) ([one] ++ insPub ++ insPriv)
        sol = solveR1CS r1cs w
        pa = ProveArguments sa crs sol

generateSimulatedWithdrawProof :: ZKProofSecret -> Fr -> DepositSecret -> ShieldedAccountSecret -> MixerState -> ((Integer, Integer), PublicInputs, Proof)
generateSimulatedWithdrawProof secret pkh ds sas state = (lastDeposit, PublicInputs insPub, proof)
    where
        (lastDeposit, outs, insPub, _) = computeWithdrawWires pkh ds sas state
        subs = PublicSignals $ [one] ++ outs ++ insPub
        proof = simulate withdrawSecret secret withdrawCRS subs

{-# INLINABLE verifyWithdraw #-}
verifyWithdraw :: PublicSignals -> Proof -> Bool
verifyWithdraw = verify withdrawCRS

computeWithdrawWires :: Fr -> DepositSecret -> ShieldedAccountSecret -> MixerState -> ((Integer, Integer), [Fr], [Fr], [Fr])
computeWithdrawWires pkh (DepositSecret r1 r2) (ShieldedAccountSecret v1 v2 v3) state = ((k, m), publicOutputWires, publicInputWires, privateInputWires)
    where
        a                              = pkh
        leaf                           = mimcHash r1 r2
        (k, n)                         = fromMaybe (0, 0) $ getMerkleLeafNumber state leaf
        MerkleTree m leafs             = state !! k
        key                            = mimcHash zero r1
        keyA                           = mimcHash a r2
        oh                             = mimcHash v1 v2
        nh                             = mimcHash (v1 + toZp (m - n)) v3 -- mimcHash v1 v3
        coPath                         = getMerkleCoPath leafs n         -- getMerkleCoPath leafs m
        l                              = getDepositPath treeSize n       -- replicate treeSize zero
        root                           = last coPath

        publicOutputWires = replicate 5 zero
        publicInputWires  = [root, a, key, keyA, toZp m, oh, nh]
        privateInputWires = [r1, r2] ++ init coPath ++ l ++ [v1, v2, v3]

getDepositPath :: Integer -> Integer -> [Fr]
getDepositPath sz n 
    | sz < 1    = []
    | otherwise = if even n then one : getDepositPath (sz - 1) (divide (n+1) 2) else zero : getDepositPath (sz - 1) (divide (n+1) 2)

------------------------------ Withdraw Problem Definition --------------------------------------------

{-# INLINABLE withdrawCRS #-}
withdrawCRS :: ReducedReferenceString
withdrawCRS = ReducedReferenceString
    {
refRedGa =
CP (Zp 2962808132192645709303929256121200626929825804628452490452913348449412958498004536904103955886464926053012921154610) (Zp 679129300650575393469522051966851082207294995590238362611558554821464818958366513079755774644183085840369698494854),
refRedHb =
CP (E (P [Zp 1261644115556860102391786103447487717747719830016211525915982204360031389482535635821529430117843090681351073416641,Zp 497003931250988459858456668583087226398835000663603375005735246222283223871116867909503008521201931296420115538514]))
 (E (P [Zp 346767903789379418631827973894053281846716124287532671527285015779088543681782261154785511704685095784206522844805,Zp 1798691007771700898320957610721716570641902132786737673589991931409836882207000191842828333889555428694237121337081])),
refRedHg =
CP (E (P [Zp 352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160,Zp 3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758]))
 (E (P [Zp 1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905,Zp 927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582])),
refRedHd =
CP (E (P [Zp 793310515576546375376197586584938606145447457885833418418862098875157733837206933974690506282396110829272448930322,Zp 2401812686662466240586310640839082611224928990726843422085490218745927662719853456421276717592433965203705826156686]))
 (E (P [Zp 1532933976922923923118537127999250920080288291406362989530725824145112936729111963156221596964525911227748331811711,Zp 3457115855191445366451629453008131980783317023042781014821377771743413679661760463245137285144032132861592760490662])),
refRedGpub = [
CP (Zp 772914638315510536434808547936453333246797384412527375260302473409677148213173926553817719526793613254813909700039) (Zp 3240615840836709215566189846835358695521845969469076160485188855615511783080171921713831547938576732542331958176096),
CP (Zp 1080583416942919975700073659758805446731186572061701405271542694202884929605199924008419669781016212115746787867945) (Zp 1905497636985483284766798937052138188818234646921861450084014801710617279364701240084603481324414293248214897744947),
CP (Zp 46833045880219615219055317848493728883659423408577388693942307553183823871709268982012339032760426396722799014971) (Zp 3397649603579063069861968119665777630630341980911455262057529658682512891086906344572750678150653786011296546669085),
CP (Zp 3702802284141416963111599512127507092160775151827275316420263578970585565834570917025443390022218121124414832236267) (Zp 1667155977404380121468875115139377207280241765810406000125963317977797590219250768910821141403172710382461657868627),
CP (Zp 942756193919990564221856197473868742868681538073209992797728543851691917300130568319130309111122928521211863312715) (Zp 3845327632282244965427455664621272879442073570378783867939093835449486949191727545188239126048054807047638420338809),
CP (Zp 1075200036007838694114881485385673436316943533927772464515032756777939590985047045928085642405197579812950734192131) (Zp 2405955389614807795348798920290538250390431812066662021154420759947690074159416646903948055542192902619686885287950),
CP (Zp 1881691867641114165598448781235342872682721156218012123041309751323952113664735710406462703834590373399304877745787) (Zp 2340776971749423095922283510582073939587153916592345143766407832158219217508786510573037776157346350000647393265355),
CP (Zp 1468834107071505170064071383956182084412646219303162129528441077669610649329430228954217521228482599778218589988242) (Zp 2291250366312019823791958927839321262683702361078049076704200386445528348899192371378261925412970490750653118432608),
CP (Zp 1205072556134728630677870932115538948111916130511488434063085150573164431736540031265028334959630810015435235564632) (Zp 3071704565298381264075165240746656808669236231123531167917682372444101595393701727508551642054585683598723445231028),
CP (Zp 2328219789085201086244186116425525148466776425011088954851716948189789822073467169601401284055643990973815556703580) (Zp 1135248106145145638919617832422385765553205327572711500117008012651159008923438095426915181985306785778282624509865),
CP (Zp 2579031825390811437901846140472224546348712201810675125936940585675094561664851618288127708908182798737049418651974) (Zp 123334848805146298208715701114612647042673322566534118211154758150081526266500928050067627318940208637123605640186),
CP (Zp 2658522789945323728570701912038254683912955917042661457539376168347356736368503852343111908018296386594002308314572) (Zp 2870495372229411147852762569255998938517888650669006178476321567816598284592366358916727097136382747543208697860677),
CP (Zp 2958231612095078888336879513688146191837961411207746839537816952165238111800700825126773363765705923766042510458517) (Zp 1181498956204683386392110778171660004667196335939158449629964178671869211760361991021015481440241366178952346861732)
]
    }

-- This is just a test value. The actual secret was destroyed in the process.
withdrawSecret :: ZKSetupSecret
withdrawSecret = ZKSetupSecret
  {
    secretAlpha = Zp 31166319297144590835384498065326183885678969878576160684492776349571265369561,
    secretBeta  = Zp 22217971736162304410974286655175391311938300667854221456573795939742078018842,
    secretGamma = Zp 24231911266267795336766923032129536952739002865218585400658043457508491429146,
    secretDelta = Zp 23922179100645229395296072467022171660941001855324254442556550539532739473801,
    secretX     = Zp 34962842270303863939238049877882078985638713473987714022146679502607935256580
  }