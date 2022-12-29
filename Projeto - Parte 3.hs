import Data.Char


type Quant = Int
type CodProd = Int
type NomeProd = String
type PrecoProd = Int
type Produto = (CodProd, NomeProd, PrecoProd)
type Menu = [Produto]
--type Menu [(CodProd, NomeProd, PrecoProd)]

type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int
type Cliente = (CodCliente, NomeCliente, CategCliente, ConsumoAnual)
type Clientes = [Cliente]
--type Clientes [(CodCliente, NomeCliente, CategCliente, ConsumoAnual)]

type Quantidade = Int
type SolCliente = (CodProd, NomeProd, Quantidade)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente, PedidoCliente)]

cardapio :: Menu
cardapio = [(150, "Hamburguer", 1000), (15, "Agua", 400), (2, "Coca-cola", 600), (40, "Batata-frita", 850), (52, "Tartelete", 1550)]

fregueses :: Clientes
fregueses = [(5, "Melani", 'A', 37000), (4, "Marcos Sa", 'A', 38000), (3, "Mateus Oliveira", 'A', 30000), (2, "Sofia Reis", 'B', 50000), (1, "Paulo Souza", 'C', 100000)]

pedidosRest :: Pedidos
pedidosRest = [(12,[(150,"Hamburguer", 1),(2,"Coca-cola",2)]), (13,[(40,"Batata-frita",4),(15,"Agua",3),(2,"Coca-cola",1)])]

codigo :: Produto -> CodProd
codigo (codProd, _, _) = codProd

codigosCardapio :: Menu -> [CodProd]
codigosCardapio [] = []
codigosCardapio (x:xs) = codigo x : codigosCardapio xs 

-- (A) 

-- 3.1

--(a) Adiciona um produto no cardápio. Se o código do produto já existir no cardápio deve retornar uma mensagem de erro sinalizando que existe um produto já cadastrado para aquele código.

adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu (c,n,p) 
  | filter (\(x,_,_) -> x == c) menu /= [] = error "Produto ja cadastrado"
  | otherwise = (c,n,p) : menu



-- (b) Remove um produto no cardápio, informando seu código. Se o código do produto não existir no cardápio deve retornar uma mensagem de erro sinalizando que não existe um produto no cardápio para aquele código.

removeProdMenu :: Menu -> CodProd -> Menu
removeProdMenu menu c 
  | filter (\(x,_,_) -> x /= c) menu == menu = error "Produto nao encontrado. Por favor, insira um codigo valido" 
  | otherwise = filter (\(x,_,_) -> x /= c) menu


-- (c)Coleta um produto no cardápio, informando seu código. Para simplificar, considere que esta operação só tem o caso de sucesso, ou seja, o item consultado sempre vai existir no cardápio.

coletaProdMenu :: Menu -> CodProd -> Produto
coletaProdMenu menu c = (head . f) menu
   where f = filter (\(x,_,_) -> x == c)


-- (B)

-- 3.2)

--(a) Adiciona um cliente na lista de clientes do restaurante. O cliente será adicionado sempre no início da lista. O código do novo cliente é gerado adicionando-se 1 ao código do cliente mais antigo, que está no início da lista vigente. Caso a lista vigente esteja vazia, o primeiro cliente a ser adicionado terá código 1. O novo cliente possuirá categoria A e consumo anual 0 ao se cadastrar. Observe que a lista assim construída será ordenada, de forma decrescente, pelo código do cliente.

adicionaCliente :: Clientes -> NomeCliente -> Clientes
adicionaCliente listaClientes nomeCliente
  | listaClientes == [] = (1, nomeCliente, 'A', 0) : listaClientes
  | otherwise = ((length listaClientes) + 1, nomeCliente, 'A', 0) : listaClientes 



--(b) Consulta os dados do cliente, informando seu código. Para simplificar, considere que esta operação só tem o caso de sucesso, ou seja, o código consultado sempre vai existir na lista de clientes.

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente clts c = (head . f) clts
   where f = filter (\(x,_,_,_) -> x == c)



--(c) Atualiza o consumo anual do cliente a cada compra, informando o código do cliente e o valor da compra corrente, que será acrescido ao valor vigente do consumo anual. Considere que o código do cliente está correto e existe na lista de clientes.

-- DUVIDA

type Compra = Int

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clts cod comp = map (\(x,y,z,k) -> if x == cod then (x,y,z, k + comp) else (x,y,z,k)) clts 




--(d) Atualiza a lista dos clientes a cada ano. Esta função atualiza as categorias de todos os clientes da lista de acordo com o consumo anual acumulado no ano e zera o consumo anual para o próximo ano.

atualiza :: Int -> Char
atualiza k
  | k < 50000 = 'A'
  | 50000 <= k && k < 150000 = 'B'
  | 150000 <= k && k < 250000 = 'C'
  | 250000 <= k && k < 350000 = 'D'
  | k >= 350000 = 'E'


atualizaClientes :: Clientes -> Clientes
atualizaClientes clts = map (\(x,y,z,k) -> (x,y, atualiza k , 0)) clts




-- (C)

-- 3.3)




{-- (a) Adiciona produtos ao pedido que está sendo registrado pelo atendente, 
informando o código do produto e a quantidade. A função assume por simplicidade que o código do 
produto existe no cardápio do restaurante e que o atendente não comete erros. Caso o código já exista na lista de pedidos do 
cliente a quantidade deverá ser incrementada para atender à nova solicitação. O nome do produto deve ser buscado no cardápio do
restaurante pelo código do produto digitado pelo atendente. -}


produt :: CodProd -> Menu -> SolCliente
produt cod menu = (head . f) menu
   where f = filter (\(x,y,z) -> x == cod)

mudaM :: Quantidade -> SolCliente -> SolCliente
mudaM q (a,b,c) = (a,b,q)

adcPP :: PedidoCliente -> CodProd -> Quantidade -> PedidoCliente
adcPP pedCl cod q = map (\(x,y,z) -> if x == cod then (x,y,z+q) else (x,y,z)) pedCl

adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quantidade -> PedidoCliente
adicionaProdPedido pedCl men cd q = if (adcPP pedCl cd q) == pedCl then (f . g) men : pedCl else adcPP pedCl cd q
   where f = mudaM q 
         g = produt cd


{- (b) Cancela um produto já solicitado pelo cliente, informando o código e a
quantidade do produto a ser cancelado. Por simplicidade, suponha que o código a
ser cancelado existe na lista de pedidos do cliente. No ato do cancelamento, se a
quantidade a ser cancelada for igual ou superior à quantidade já solicitada, o item
deve ser removido da lista de pedidos do cliente. Se o cancelamento for parcial, o
item permanecerá na lista, mas com a quantidade decrementada da quantidade
cancelada.
cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente -} 

cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto pedCl cd q = (f . g) pedCl
  where f =  filter (\(x,y,z) -> z > 0) 
        g = map (\(x,y,z) -> if x == cd then (x,y, z - q) else (x,y,z))



{- (c)Adiciona  um  pedido  de um cliente na lista global de pedidos do restaurante quando a conta é paga. 
 O pedido será adicionado no final da lista vigente e o código do pedido será o código do cliente. -}

adicionaPedido :: Pedidos -> PedidoCliente -> CodCliente -> Pedidos
adicionaPedido pedidos pedidosCliente codCliente = pedidos ++ pedidoAtual
  where pedidoAtual = [(codCliente, pedidosCliente)]
 



{- (d)  Gera  lista  completa  do  pedido,  que  será  usada para  a  impressão  da  conta quando esta for finalizada.
Os precos de cada produto são coletados do cardápio, usando o código do produto para fazer a coleta. 
O preco gerado na lista de saída já é o preco do item totalizado, ou seja, o preco unitário multiplicado   
pela quantidade


geraPedidoImpressao:: PedidoCliente-> Menu -> PedidoTotalizado -}

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]

geraPIax :: Menu -> SolCliente -> ProdTotalizado
geraPIax menu (x,y,z) = (head . f . g) menu
   where f = map (\(a,b,c) -> (x,y,z, z * c)) 
         g = filter (\(a,b,c) -> x == a)

geraPedidoImpressao :: PedidoCliente-> Menu -> PedidoTotalizado
geraPedidoImpressao pedCl menu = map f pedCl
   where f = geraPIax menu




{- (e) Gera o total da conta a partir do pedido do cliente totalizado, após aplicar a
função do item 3.3 (d). Esta função soma os precos totais de cada produto
solicitado e aplica o desconto por categoria de cliente, gerando o preco final. O
valor do desconto será também representado por um inteiro, assim como o preco
final. Para tal o atendente pergunta ao cliente seu código e informa ao sistema. O
resultado desta função também será usado para atualizar o consumo anual do
cliente pela função do item 3.2 (d).
-}

type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco,Desconto,PrecoFinal)

somaPreco :: PedidoTotalizado -> Int
somaPreco [] = 0
somaPreco pedT = (sum . f) pedT 
   where f = map (\(_,_,_,p) -> p)

buscaCateg :: Clientes -> CodCliente -> CategCliente
buscaCateg clt cd = if g clt == [] then error "Cliente nao encontrado" else (head . f . g) clt
   where f = map (\(_,_,z,_) -> z)
         g = filter (\(x,y,z,k) -> x == cd)

totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clt cd pedT
  | buscaCateg clt cd == 'A' = (preco, 0, preco)
  | buscaCateg clt cd == 'B' = (preco, floor $ 0.03 * fromIntegral preco, preco - (floor $ 0.03 * fromIntegral preco))
  | buscaCateg clt cd == 'C' = (preco, floor $ 0.05 * fromIntegral preco, preco - (floor $ 0.05 * fromIntegral preco))
  | buscaCateg clt cd == 'D' = (preco, floor $ 0.1  * fromIntegral preco, preco - (floor $ 0.1  * fromIntegral preco))
  | buscaCateg clt cd == 'E' = (preco, floor $ 0.15 * fromIntegral preco, preco - (floor $ 0.15 * fromIntegral preco))
    where preco = somaPreco pedT




{- (f) Entrega de pedido. O cliente informa seu código ao atendente da coleta. O
pedido é entregue e confirmado. Ao confirmar a entrega, o sistema remove o
pedido entregue da lista de pedidos do restaurante através da função abaixo.
entregaPedido :: Pedidos -> CodCliente -> Pedidos -}


entregaPedAx :: Pedidos -> CodCliente -> Pedidos
entregaPedAx ped cd = filter (\(x,y) -> x /= cd) ped

entregaPedido :: Pedidos -> CodCliente -> Pedidos
entregaPedido ped cd = if entregaPedAx ped cd == ped then error "Codigo nao encontrado" else entregaPedAx ped cd


-- (D)

-- 3.4 

{- (a) Formata o valor unitário do produto, o valor do desconto e o valor total da conta. Dado o valor gere uma string com o valor formatado. Você deve colocar duas casas decimais e justificar o valor à direita. O total previsto para o valor são
8 caracteres. Caso o valor não ocupe os 8 caracteres você deve preencher com ‘.’.
Para simplificar suponha que jamais o valor inteiro excederá 7 dígitos, o que é
razoável para os valores de um restaurante fast-food.
formataValor :: Int -> String
formataValor 400 retorna “....4.00”
formataValor 8800 retorna “...88.00”
formataValor 50 retorna “....0.50”
formataValor 5 retorna “....0.05” -}

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





{- (b) Formata uma linha da conta, que possui quatro informações: código do
produto, quantidade do produto, nome do produto e preço totalizado. Entre as três
primeiras informações deve ser reservado 2 espaços em branco. O código do
produto deve usar no máximo 4 caracteres e ser justificado à direita, usando
brancos para o preenchimento de caracteres à esquerda não utilizados, se for o
caso. A quantidade deve usar no máximo 3 caracteres e vir justificada à direita,
deixando brancos precedendo o dígito se for o caso. O nome do produto deve
usar no máximo 25 caracteres e justificado à esquerda, usando ‘.’ à direita caso o
nome seja menor que 25 caracteres. O preço deve ser formatado usando a função
do item 3.4.(a). No final da linha deve ser adicionado ‘\n’ para permitir pular de
linha quando a conta for emitida.
formataLinha :: ProdTotalizado -> String
formataLinha (15,”Agua”,2,800)
retorna

“ 15 2 Agua.........................8.00\n” -}

formataLinha :: ProdTotalizado -> String
formataLinha (codProd, nomeProd, quant, preco) = (replicate (4 - length (show codProd)) ' ') ++ (show codProd) ++ "  " ++ (replicate (3 - length (show quant)) ' ') ++ (show quant) ++ "  " ++ nomeProd ++ (replicate (25 - length nomeProd) '.') ++ (formataValor preco) ++ "\n"





{- (c) Formata todas as linhas de uma conta usando a função do item 3.4 (b). O
efeito desta função é juntar as linhas correspondentes a todos os produtos
solicitados por um cliente.
formataLinhas :: PedidoTotalizado -> String
formataLinhas [(15,”Agua”, 2, 800),(2,“Coca-cola”,1,600)]
retorna

“ 15 2 Agua.........................8.00\n 
  2 1 Coca-cola....................6.00\n” -}

formataLinhas :: PedidoTotalizado -> String
formataLinhas pedT = (concat . f) pedT
    where f = map formataLinha




{- (d) Formata o total da conta, usando o resultado da função 3.3 (e). Os nomes
Total, Desconto e A pagar devem vir alinhados com os nomes dos
produtos, deixando uma linha em branco entre eles e os produtos.
formataTotal:: Totalizacao-> String
Suponha o exemplo onde um valor da totalização é dado por (1400,70,1330).
O símbolo – representa um branco.
formataTotal (1400,70,1330)
retorna
“\n-----------Total.......................14.40\n
-----------Desconto.....................0.70\n
-----------A-Pagar.....................13.30\n” -}

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





{- (e) Gera conta. Usa várias funções anteriores e putStr para gerar a conta
formatada. O símbolo – simboliza espaço em branco.
geraConta:: CodCliente -> Clientes-> Menu -> PedidoCliente -> IO ()
Suponha que um cliente da categoria A, com código 4, solicitou duas águas e
uma coca-cola. A função deve retornar


(((pedidoCliente geraPedidoImpressao) totalPedido) formatTotal)

 PEDIDO—4
 COD---QTD--PRODUTO---------------------PRECO
 --15----2--Agua.........................8.00
 ---2----1--Coca-cola....................6.00

 -----------Total.......................14.00
 -----------Desconto.....................0.70
 -----------A Pagar.....................13.30 -}

geraConta :: CodCliente -> Clientes-> Menu -> PedidoCliente -> IO ()
geraConta codCliente clientes menu pedidoCliente = putStr ((cabecalho numPedido) ++ (formataLinhas pedidoTotalizado) ++ (formataTotal (totalPedido clientes codCliente pedidoTotalizado)))
  where
    pedidoTotalizado = geraPedidoImpressao pedidoCliente menu
    cabecalho numPedido = "PEDIDO " ++ (show numPedido) ++ "\n" ++ "COD   QTD  PRODUTO                     PRECO\n"
    numPedido = codCliente





{- (f) Lista os produtos do cardápio, obedecendo ao formato do exemplo abaixo
onde o símbolo – simboliza um espaço em branco. Cada linha da lista inclui o
código do produto, o nome do produto e o preço formatado. O código deve usar
no máximo 4 caracteres e ser justificado à direita. O nome do produto deve vir
depois de 2 espaços em branco e ter no máximo 25 caracteres, justificado à

esquerda. Os caracteres faltantes devem ser preenchidos com ‘.’. O preço deve
obedecer à formatação pedida no item 3.4 (a). É aconselhável fazer funções
auxiliares de formatação de linha como no caso da formatação de conta dos itens
anteriores.
listaCardapio :: Menu -> IO()
CARDAPIO
-150--Hamburguer..................10.00
--15--Agua.........................4.00
---2--Coca-cola....................6.00
--40--Batata-frita.................8.50
--52--Tartelete...................15.50 -}


listaCardapio :: Menu -> IO()
listaCardapio menu = putStr (concat [organiza (codProd, nomeProd, precoProd) | (codProd, nomeProd, precoProd) <- menu])

  where organiza (codProd, nomeProd, precoProd) = (replicate (4 - length (show codProd)) ' ') ++ (show codProd) ++ "  " ++ nomeProd ++ (replicate (25 - length nomeProd) '.') ++ (formataValor precoProd) ++ "\n"





{- (g) Lista os códigos e nomes de clientes de uma dada categoria.
type Categoria = Char
listaClientesCat :: Clientes -> Categoria -> IO()
A formatação da listagem deve obedecer a: seis caracteres para o código
justificado à direita, 5 caracteres em branco e 50 caracteres para o nome do
cliente, justificado à esquerda. Cada cliente deve vir em uma linha. A categoria
deve constar do cabeçalho da listagem como no exemplo abaixo. O símbolo – não
deve aparecer; foi introduzido no exemplo para sinalizar o espaço em branco
requerido.

CLIENTES
CATEGORIA A
-----4-----Marcos Sa
-----3-----Mateus Oliveira
Caso a categoria não possua clientes deve exibir

CLIENTES
CATEGORIA E
Nao ha clientes para a categoria E -}


type Categoria = Char

listaClientesCat :: Clientes -> Categoria -> IO()
listaClientesCat clientes categoria
  | clientesOrganizados == [] = putStrLn (cabecalho categoria ++ "Nao ha clientes para a categoria" ++ (show categoria))
  | otherwise = putStrLn ((cabecalho categoria) ++ clientesOrganizados)

    where
      cabecalho categoria = "CLIENTES\nCATEGORIA " ++ [categoria] ++ "\n"

      clientesOrganizados = concat [organiza codCliente nomeCliente | (codCliente, nomeCliente, categCliente, consumoAnual) <- clientes, categCliente == categoria]
      
      organiza codCliente nomeCliente = "     " ++ (show codCliente) ++ (replicate (6 - length (show codCliente)) ' ') ++ nomeCliente ++ "\n"




      
{- (h)  Lista  os  códigos  e  nomes  de  todos  os  clientes,  separados  por  categoria.  A formatação segue o item (f).
listaClientes:: Clientes -> IO() 

Um exemplo para a lista de clientes introduzida anteriormente seria:

CLIENTES 
CATEGORIA A
-----4-----Marcos Sa
-----3-----Mateus Oliveira

CATEGORIA B
-----2-----Sofia Reis

CATEGORIA C
-----1-----Paulo Souza

CATEGORIA D
Nao ha clientes para a categoria D

CATEGORIA E
Nao ha clientes para a categoria E -}


listaClientes :: Clientes -> IO()
listaClientes clientes = putStrLn ("CLIENTES\n\n" ++ cabecalho 'A' ++ clientesOrganizados 'A' ++ "\n" ++ cabecalho 'B' ++ clientesOrganizados 'B' ++ "\n" ++  cabecalho 'C' ++ clientesOrganizados 'C' ++ "\n" ++ cabecalho 'D' ++ clientesOrganizados 'D' ++ "\n" ++ cabecalho 'E' ++ clientesOrganizados 'E')

    where
      
      cabecalho categoria = "CATEGORIA " ++ [categoria] ++ "\n"

      organiza codCliente nomeCliente = "     " ++ (show codCliente) ++ (replicate (6 - length (show codCliente)) ' ') ++ nomeCliente ++ "\n"

      clientesOrganizados categoria
        | [nomeCliente | (codCliente, nomeCliente, categCliente, consumoAnual) <- clientes, categCliente == categoria] == [] = ("Nao ha clientes para a categoria " ++ [categoria])
        
        | otherwise = concat [organiza codCliente nomeCliente | (codCliente, nomeCliente, categCliente,  consumoAnual) <- clientes, categCliente == categoria]



-- (E)

{- Incorpora compra utilizando as funções atualizaConsumo e totalPedido.
Esta função acrescenta o preço final obtido da função totalPedido no
consumo anual do cliente.
incorporaCompra:: Clientes -> CodCliente -> PedidoTotalizado ->
Clientes -}

devolveUltimo :: Totalizacao -> Int
devolveUltimo (x,y,z) = z

incorporaCompraAx :: Clientes -> CodCliente -> PedidoTotalizado -> Clientes
incorporaCompraAx clt cd pedT = atualizaConsumo clt cd compra
    where compra = (devolveUltimo (totalPedido clt cd pedT)) 

incorporaCompra :: Clientes -> CodCliente -> PedidoTotalizado -> Clientes
incorporaCompra clt cd pedT = if clt == (incorporaCompraAx clt cd pedT) then error "Codigo nao encontrado" else incorporaCompraAx clt cd pedT



-- (F)

{-Faça as funções de ordenação de listas abaixo, adaptando os algoritmos vistos em
sala de aula, usando um algoritmo diferente em cada item:-}

{-a. Ordena os produtos do cardápio pelo código do produto.
ordenaMenu :: Menu -> Menu -}

codMenu :: Produto -> CodProd
codMenu = (\(x,_,_) -> x) 

insOrd :: Produto -> Menu -> Menu
insOrd prod menu = takeWhile (\(x,_,_) -> x <= cod) menu ++ [prod] ++ dropWhile (\(x,_,_) -> x <= cod) menu
   where cod = codMenu prod

ordInsercao :: Menu -> Menu
ordInsercao menu = foldr insOrd [] menu

ordenaMenu :: Menu -> Menu
ordenaMenu menu = ordInsercao menu




{- b. Ordena os clientes pelo nome do cliente.
ordenaClientes :: Clientes -> Clientes -}

pgN :: Cliente -> String
pgN = (\(_,y,_,_) -> y)

quickSortAx :: Cliente -> Clientes -> Clientes
quickSortAx clt freg = (filter  (\(_,nome,_,_) -> nome <= (pgN clt)) freg) ++ [clt] ++ (filter (\(_,nome,_,_) -> nome > (pgN clt)) freg)

quickSort :: Clientes -> Clientes
quickSort clts = foldr quickSortAx [] clts 

ordenaClientes :: Clientes -> Clientes
ordenaClientes clts = quickSort clts

-- (G)

{- (a) Modifique a função 3.4.f da parte 1, para que o cardápio seja impresso de
acordo com a ordem crescente dos códigos dos produtos, usando a função do item
F(a).
listaCardapioOrd :: Menu -> IO() -}

formata :: Produto -> String
formata (x,y,z) = (replicate (4 - length (show x)) ' ') ++ (show x) ++ "  " ++ y ++ (replicate (25 - length y) '.') ++ (formataValor z) ++ "\n"

listaCardapioAx :: Menu -> [Char]
listaCardapioAx [] = []
listaCardapioAx menu = (concat . f) menu
   where f = map formata

listaCardapioModificada :: Menu -> IO()
listaCardapioModificada menu = (putStr . listaCardapioAx) menu

listaCardapioOrd :: Menu -> IO()
listaCardapioOrd menu = (listaCardapioModificada . ordenaMenu) menu




{- (b) Modifique as funções 3.4.g e 3.4.h da parte 1, para que a lista de clientes seja
impressa de acordo com a ordem alfabética dos nomes dos clientes, usando a
função do item F(b).-}

--3.4.g 
-- listaClientesCatOrd :: Clientes -> Categoria -> IO()

pgC :: Cliente -> CodCliente
pgC (x,y,z,k) = x

formataCl :: Cliente -> String
formataCl clt = "     " ++ ((show . pgC) clt) ++ (replicate (6 - length ( (show . pgC) clt) ) ' ') ++ (pgN clt) ++ "\n"
 
clientesOrg :: Clientes -> String
clientesOrg clts = (concat . f) clts
  where f = map formataCl

recolheCliente :: Categoria -> Clientes -> Clientes
recolheCliente cat clts = filter (\(_,_,z,_) -> z == cat) clts

inicio :: Char -> String
inicio ch = "CLIENTES\nCATEGORIA " ++ [ch] ++ "\n"

listaClientesCatModificada :: Clientes -> Categoria -> IO()
listaClientesCatModificada clts cat
  | (recolheCliente cat clts) == [] = putStrLn ((inicio cat) ++ "Nao ha clientes para a categoria" ++ " " ++ [cat])
  | otherwise = putStrLn ((inicio cat) ++ (clientesOrg . f) clts)
    where f = recolheCliente cat

listaClientesCatOrd :: Clientes -> Categoria -> IO()
listaClientesCatOrd clts cat = listaClientesCatModificada f cat 
   where f = ordenaClientes clts


-- 3.4.h
-- listaClientesOrd :: Clientes -> IO() 

inicio2 :: Char -> String
inicio2 ch = "CATEGORIA " ++ [ch] ++ "\n"

listaClientesCatModificadaAx :: Clientes -> Categoria -> String
listaClientesCatModificadaAx clts cat
  | (recolheCliente cat clts) == [] = (inicio2 cat) ++ "Nao ha clientes para a categoria" ++ " " ++ [cat] ++ "\n"
  | otherwise = (inicio2 cat) ++ (clientesOrg . f) clts
    where f = recolheCliente cat

listaClientesModificada :: Clientes -> IO()
listaClientesModificada clts = putStrLn ("CLIENTES\n\n" ++ (listaClientesCatModificadaAx clts 'A') ++ "\n" ++ (listaClientesCatModificadaAx clts 'B') ++ "\n" ++  (listaClientesCatModificadaAx clts 'C') ++ "\n" ++ (listaClientesCatModificadaAx clts 'D') ++ "\n" ++ (listaClientesCatModificadaAx clts 'E'))

listaClientesOrd :: Clientes -> IO()
listaClientesOrd clts = (listaClientesModificada . ordenaClientes) clts
