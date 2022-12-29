module ABB (Arv(..),arvVazia,ehVazia,ehNoNulo,arvEsq,arvDir,infoNo,atualizaNo,coletaNo,insereNo,removeNo,mapArv) where



data Arv a b c d e = NoNulo | No (a,b,c,d,e) (Arv a b c d e) (Arv a b c d e) deriving (Eq,Show,Read,Ord)



arvVazia :: Arv a b c d e 
arvVazia = NoNulo



ehVazia :: Arv a b c d e -> Bool
ehVazia (NoNulo) = True
ehVazia _ = False



ehNoNulo :: Arv a b c d e -> Bool
ehNoNulo NoNulo = True
ehNoNulo _ = False



arvEsq :: Arv a b c d e -> Arv a b c d e
arvEsq (NoNulo) = error "Nao ha subArvore esquerda"
arvEsq (No _ arvE _) = arvE



arvDir :: Arv a b c d e -> Arv a b c d e
arvDir (NoNulo) = error "Nao ha subArvore direita"
arvDir (No _ _ arvD) = arvD



infoNo :: Arv a b c d e -> (a,b,c,d,e) 
infoNo NoNulo = error "Arvore vazia"
infoNo (No inf _ _) = inf



verifica :: Ord a => a -> Arv a b c d e -> Bool
verifica _ NoNulo = False
verifica n (No (x,y,z,k,w) arvE arvD)
  | n == x = True
  | n < x = verifica n arvE
  | otherwise = verifica n arvD

atualizaAx :: Ord a => (a,b,c,d,e) -> Arv a b c d e -> Arv a b c d e 
atualizaAx _ NoNulo = NoNulo
atualizaAx (x1,y1,z1,k1,w1) (No (x,y,z,k,w) arvE arvD)
  | x1 == x = No (x1,y1,z1,k1,w1) arvE arvD
  | x1 > x = No (x,y,z,k,w) arvE (atualizaAx (x1,y1,z1,k1,w1) arvD)
  | otherwise = No (x,y,z,k,w) (atualizaAx (x1,y1,z1,k1,w1) arvE) arvD

atualizaNo :: Ord a => (a,b,c,d,e) -> Arv a b c d e -> Arv a b c d e 
atualizaNo (x,y,z,k,w) tree = if verifica x tree then atualizaAx (x,y,z,k,w) tree else error "Nao encontrado"



coletaNo :: Ord a => a -> Arv a b c d e -> (a,b,c,d,e)
coletaNo _ NoNulo = error "Nao encontrado"
coletaNo n (No (x,y,z,k,w) arvE arvD)
  | n == x = (x,y,z,k,w)
  | n < x = coletaNo n arvE 
  | otherwise = coletaNo n arvD



insereNoAx :: Ord a => (a,b,c,d,e) -> Arv a b c d e -> Arv a b c d e 
insereNoAx (x1,y1,z1,k1,w1) NoNulo = No (x1,y1,z1,k1,w1) NoNulo NoNulo
insereNoAx (x1,y1,z1,k1,w1) (No (x,y,z,k,w) arvE arvD)
  | x == x1 = (No (x,y,z,k,w) arvE arvD)
  | x1 < x = No (x,y,z,k,w) (insereNoAx (x1,y1,z1,k1,w1) arvE) arvD
  | otherwise = No (x,y,z,k,w) arvE (insereNoAx (x1,y1,z1,k1,w1) arvD) 

insereNo :: Ord a => (a,b,c,d,e) -> Arv a b c d e -> Arv a b c d e
insereNo (x,y,z,k,w) tree = if verifica x tree then error "O codigo inserido ja existe" else insereNoAx (x,y,z,k,w) tree



minArv :: Ord a => Arv a b c d e -> (a,b,c,d,e)
minArv tree
  | ehNoNulo (arvEsq tree) = infoNo tree
  | otherwise = minArv (arvEsq tree)

remove :: Ord a => Arv a b c d e -> Arv a b c d e -> Arv a b c d e
remove arvE arvD = No (x,y,z,k,w) arvE novaArv
  where (x,y,z,k,w) = minArv arvD
        novaArv = removeNoAx x arvD

removeNoAx :: Ord a => a -> Arv a b c d e -> Arv a b c d e
removeNoAx _ NoNulo = NoNulo
removeNoAx n (No (x,y,z,k,w) arvE arvD)
  | n < x = No (x,y,z,k,w) (removeNoAx n arvE) arvD
  | n > x = No (x,y,z,k,w) arvE (removeNoAx n arvD)
  | ehNoNulo arvD = arvE
  | ehNoNulo arvE = arvD
  | otherwise = remove arvE arvD

removeNo :: Ord a => a -> Arv a b c d e -> Arv a b c d e
removeNo n tree = if verifica n tree then removeNoAx n tree else error "Nao encontrado"


mapArv :: ((a,b,c,d,e) -> (a,b,c,d,e)) -> Arv a b c d e -> Arv a b c d e
mapArv _ NoNulo = NoNulo
mapArv f (No inf arvE arvD) = No (f inf) (mapArv f arvE) (mapArv f arvD)