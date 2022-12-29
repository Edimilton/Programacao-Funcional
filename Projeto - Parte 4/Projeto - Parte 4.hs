import ABB
import Data.List


type Quant = Int
type CodProd = Int
type NomeProd = String
type PrecoProd = Int
type Produto = (CodProd, NomeProd, PrecoProd)
type Menu = [Produto]

type CodCliente = Int
type NomeCliente = String
type CategCliente = Char 
type ConsumoAnual = Int
type Cliente = (CodCliente, NomeCliente, CategCliente, MesAniversario, ConsumoAnual)

data MesAniversario = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez deriving (Eq,Show,Ord,Read)

type Quantidade = Int
type SolCliente = (CodProd, NomeProd, Quantidade)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente, PedidoCliente)]

type Compra = Int
type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]
type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco,Desconto,PrecoFinal)

type Clientes = Arv CodCliente NomeCliente CategCliente MesAniversario ConsumoAnual


{-
(a) Adicionar um cliente no cadastro (equivalente à questão 3.2.a), informando seu código, nome e mês de
aniversário. O cliente será adicionado como sendo da categoria A e com consumo anual zerado. Para
esta questão considere que o código do cliente a inserir é informado por você e não mais calculado
como nas partes anteriores, para evitar mais funções no módulo ABB.
-}

adicionaCliente :: Clientes -> CodCliente -> NomeCliente -> MesAniversario -> Clientes
adicionaCliente clts cod nom mes = insereNo (cod,nom,'A',mes,0) clts 



{-
(b) Coleta dados de um cliente, informando seu código (equivalente à questão 3.2.b)
-}

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente clts cod = coletaNo cod clts




{-
(c) Atualiza o consumo anual do cliente, informando seu código (equivalente à questão 3.2.c)
-}

atualizaConsumoAx :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumoAx clts cod comp 
  | ehVazia clts = error "Nao ha clientes para atualizar"
  | otherwise = mapArv (\(x,y,z,k,w) -> if x == cod then (x,y,z,k,w + comp) else (x,y,z,k,w)) clts

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clts cod comp = if clts == (atualizaConsumoAx clts cod comp) then error "Cliente nao encontrado" else atualizaConsumoAx clts cod comp




{-
(d) Atualiza categorias de clientes (equivalente à questão 3.2.d)
-}

atualiza :: Int -> Char
atualiza k
  | k < 50000 = 'A'
  | 50000 <= k && k < 150000 = 'B'
  | 150000 <= k && k < 250000 = 'C'
  | 250000 <= k && k < 350000 = 'D'
  | k >= 350000 = 'E'

atualizaClientes :: Clientes -> Clientes
atualizaClientes clts
  | ehVazia clts = error "Nao ha clientes para atualizar"
  | otherwise = mapArv (\(x,y,z,k,w) -> (x,y,atualiza w,k, 0)) clts



{-
(e) Gera o total da conta do cliente (equivalente à questão 3.3.e)
-}

somaPreco :: PedidoTotalizado -> Int
somaPreco [] = 0
somaPreco pedT = (sum . f) pedT 
   where f = map (\(_,_,_,p) -> p)

devolveCat :: Cliente -> CategCliente
devolveCat (_,_,cat,_,_) = cat

buscaCateg :: Clientes -> CodCliente -> CategCliente
buscaCateg clts cod = devolveCat $ coletaNo cod clts 

totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clts cod pedT
  | catClt == 'A' = (preco, 0, preco)
  | catClt == 'B' = (preco, floor $ 0.03 * fromIntegral preco, preco - (floor $ 0.03 * fromIntegral preco))
  | catClt == 'C' = (preco, floor $ 0.05 * fromIntegral preco, preco - (floor $ 0.05 * fromIntegral preco))
  | catClt == 'D' = (preco, floor $ 0.1  * fromIntegral preco, preco - (floor $ 0.1  * fromIntegral preco))
  | catClt == 'E' = (preco, floor $ 0.15 * fromIntegral preco, preco - (floor $ 0.15 * fromIntegral preco))
    where 
       preco = somaPreco pedT
       catClt = buscaCateg clts cod



{-
(f) Gera a conta do cliente (equivalente à questão 3.4.e)
-}

-- ----------------------------------------------------------------------------

geraPIax :: Menu -> SolCliente -> ProdTotalizado
geraPIax menu (x,y,z) = (head . f . g) menu
   where f = map (\(a,b,c) -> (x,y,z, z * c)) 
         g = filter (\(a,b,c) -> x == a)

geraPedidoImpressao :: PedidoCliente-> Menu -> PedidoTotalizado
geraPedidoImpressao pedCl menu = map f pedCl
   where f = geraPIax menu

formataValor :: Int -> String
formataValor valor = (replicate (8 - length valorOrganizado) '.') ++ valorOrganizado
  where
    valorOrganizado
      | comprimentoValor == 2 = organiza2
      | comprimentoValor == 1 = organiza1
      | otherwise = organiza

    paramentro = comprimentoValor - 2 
    comprimentoValor = (length (show valor))

    organiza2 = (take paramentro (show valor)) ++ "0." ++ (drop paramentro (show valor))
    organiza1 = (take paramentro (show valor)) ++ "0.0" ++ (drop paramentro (show valor))
    organiza = (take paramentro (show valor)) ++ "." ++ (drop paramentro (show valor))

formataLinha :: ProdTotalizado -> String
formataLinha (codProd, nomeProd, quant, preco) = (replicate (4 - length (show codProd)) ' ') ++ (show codProd) ++ "  " ++ (replicate (3 - length (show quant)) ' ') ++ (show quant) ++ "  " ++ nomeProd ++ (replicate (25 - length nomeProd) '.') ++ (formataValor preco) ++ "\n"

formataLinhas :: PedidoTotalizado -> String
formataLinhas pedT = (concat . f) pedT
    where f = map formataLinha

formataTotal :: Totalizacao -> String
formataTotal (val1, val2, val3) = "\n" ++ inicioTotal ++ paramentro1 (valor val1) ++ valor val1 ++ "\n" ++ inicioDesconto ++ paramentro2 (valor val2) ++ valor val2 ++ "\n" ++ inicioAPagar ++ paramentro3 (valor val3) ++ valor val3 ++"\n"
  where
    inicioTotal = "           Total"
    inicioDesconto = "           Desconto"
    inicioAPagar = "           A Pagar"
    paramentro1 valor = (replicate (20 - length (show valor)) '.')
    paramentro2 valor = (replicate (17 - length (show valor)) '.')
    paramentro3 valor = (replicate (18 - length (show valor)) '.')
    valor x = (formataValor x)

--  ----------------------------------------------------------------------------

geraConta :: CodCliente -> Clientes -> Menu -> PedidoCliente -> IO ()
geraConta cod clts menu pedClt = putStr ((inicio cod) ++ (formataLinhas pedT) ++ (formataTotal (totalPedido clts cod pedT)))
  where
    pedT = geraPedidoImpressao pedClt menu
    inicio cod = "PEDIDO " ++ (show cod) ++ "\n" ++ "COD   QTD  PRODUTO                     PRECO\n"



{-
(g) Transforma todos dados dos clientes que estão no TAD em uma lista de clientes ordenada pelo código
do cliente, em ordem crescente de código. Ou seja, transforma a árvore em uma lista com os mesmos
dados, ordenados pelo código.
-}

transformaLista :: Clientes -> [Cliente]
transformaLista clts
  | ehVazia clts = []
  | ehVazia (arvEsq clts) && ehVazia (arvDir clts) = [(infoNo clts)]
  | ehVazia (arvEsq clts) = [(infoNo clts)] ++ transformaLista (arvDir clts)
  | ehVazia (arvDir clts) = transformaLista (arvEsq clts) ++ [(infoNo clts)]
  | otherwise = transformaLista (arvEsq clts) ++ [(infoNo clts)] ++ transformaLista (arvDir clts)




-- (h) Usando funções da biblioteca Data.List do GHCi e a lista obtida como resultado do item (g), faça:

{-
Gerar uma lista de nomes de clientes, ordenada alfabeticamente, com os clientes que
nasceram em um mês fornecido como parâmetro;
-}

recNome :: MesAniversario -> [Cliente] -> [NomeCliente] 
recNome _ [] = []
recNome aniv clts = if listClt /= [] then map (\(_,nom,_,_,_) -> nom) listClt else []
  where listClt = filter (\(_,_,_,mes,_) -> mes == aniv) clts

geraListaClienMes :: Clientes -> MesAniversario -> [NomeCliente]
geraListaClienMes clts mes = sort $ recNome mes cltsL
  where cltsL = transformaLista clts




{-
Uma função retornando um par cartesiano, cujo primeiro elemento é uma lista dos meses que
aparecem na lista (g) e o segundo elemento uma lista dos que não aparecem na lista (g)
-}

meses :: Clientes -> ([MesAniversario],[MesAniversario])
meses clts = (mesApar,mesNotApar)
 where
  cltsL = transformaLista clts
  mesApar = sort $ nub $ map (\(_,_,_,mes,_) -> mes) cltsL
  mesNotApar = filter (\mes -> (not.elem mes) mesApar) listaMeses
  listaMeses = [Jan,Fev,Mar,Abr,Mai,Jun,Jul,Ago,Set,Out,Nov,Dez]