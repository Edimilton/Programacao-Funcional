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


-- 3.1

--(a) Adiciona um produto no cardápio. Se o código do produto já existir no cardápio deve retornar uma mensagem de erro sinalizando que existe um produto já cadastrado para aquele código.

codigo :: Produto -> CodProd
codigo (codProd, _, _) = codProd

codigosCardapio :: Menu -> [CodProd]
codigosCardapio menu = [codProd | (codProd, _, _) <- menu]

adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu produto
  | elem (codigo produto) (codigosCardapio menu) = error "Este produto ja existe no cardapio"
  | otherwise = produto : menu



-- (b) Remove um produto no cardápio, informando seu código. Se o código do produto não existir no cardápio deve retornar uma mensagem de erro sinalizando que não existe um produto no cardápio para aquele código.

removeProdMenu :: Menu -> CodProd -> Menu
removeProdMenu menu codRemocao
  | elem codRemocao (codigosCardapio menu) = [(codProd, nomeProd, precoProd) | (codProd, nomeProd , precoProd) <- menu, codProd /= codRemocao]
  | otherwise = error "Esse produto não está no cardapio"







-- (c)Coleta um produto no cardápio, informando seu código. Para simplificar, considere que esta operação só tem o caso de sucesso, ou seja, o item consultado sempre vai existir no cardápio.

coletaProdMenu :: Menu -> CodProd -> Produto
coletaProdMenu menu codDesejado = head [ (codProd, nomeProd, precoProd) | (codProd, nomeProd , precoProd) <- menu, codProd == codDesejado ]



-- 3.2)

--(a) Adiciona um cliente na lista de clientes do restaurante. O cliente será adicionado sempre no início da lista. O código do novo cliente é gerado adicionando-se 1 ao código do cliente mais antigo, que está no início da lista vigente. Caso a lista vigente esteja vazia, o primeiro cliente a ser adicionado terá código 1. O novo cliente possuirá categoria A e consumo anual 0 ao se cadastrar. Observe que a lista assim construída será ordenada, de forma decrescente, pelo código do cliente.


adicionaCliente :: Clientes -> NomeCliente -> Clientes
adicionaCliente listaClientes nomeCliente
  | listaClientes == [] = (1, nomeCliente, 'A', 0) : listaClientes
  | otherwise = ((length listaClientes) + 1, nomeCliente, 'A', 0) : listaClientes 





--(b) Consulta os dados do cliente, informando seu código. Para simplificar, considere que esta operação só tem o caso de sucesso, ou seja, o código consultado sempre vai existir na lista de clientes.

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente listaClientes codDesejado = head [ (codCliente, nomeCliente, categCliente, consumoAnual) | (codCliente, nomeCliente, categCliente, consumoAnual) <- listaClientes, codCliente == codDesejado ]


--(c) Atualiza o consumo anual do cliente a cada compra, informando o código do cliente e o valor da compra corrente, que será acrescido ao valor vigente do consumo anual. Considere que o código do cliente está correto e existe na lista de clientes.

type Compra = Int
atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo listaClientes codDesejado novaCompra = [ (codCliente, nomeCliente, categCliente, consumoAnual + novaCompra) | (codCliente, nomeCliente, categCliente, consumoAnual) <- listaClientes, codCliente == codDesejado ]

--(d) Atualiza a lista dos clientes a cada ano. Esta função atualiza as categorias de todos os clientes da lista de acordo com o consumo anual acumulado no ano e zera o consumo anual para o próximo ano.

atualizaClientes :: Clientes -> Clientes
atualizaClientes listaClientes = [ (codCliente, nomeCliente, atualizaCateg consumoAnual, zera consumoAnual) | (codCliente, nomeCliente, categCliente, consumoAnual) <- listaClientes]
  where 
    atualizaCateg  consumoAnual
      | consumoAnual < 50000 = 'A'
      | 50000 <= consumoAnual && consumoAnual < 150000 = 'B'
      | 150000 <= consumoAnual && consumoAnual < 250000 = 'C'
      | 250000 <= consumoAnual && consumoAnual < 350000 = 'D'
      | consumoAnual >= 350000 = 'E'
    zera consumoAnual = 0 





-- 3.3)




-- (a) Adiciona produtos ao pedido que está sendo registrado pelo atendente, informando o código do produto e a quantidade. A função assume por simplicidade que o código do produto existe no cardápio do restaurante e que o atendente não comete erros. Caso o código já exista na lista de pedidos do cliente a quantidade deverá ser incrementada para atender à nova solicitação. O nome do produto deve ser buscado no cardápio do restaurante pelo código do produto digitado pelo atendente. 


adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quantidade -> PedidoCliente
adicionaProdPedido pedidosCliente menu codDesejado quantDesejada
  | elem codDesejado (codigosCardapio pedidosCliente) = [(codProd, nomeProd, quantDesejada + quant) | (codProd, nomeProd, quant) <- pedidosCliente, codProd == codDesejado] ++ [(codProd, nomeProd, quant) | (codProd, nomeProd, quant) <- pedidosCliente, codProd /= codDesejado]
  | otherwise = (codDesejado, (nomeProdDesejado codDesejado), quantDesejada) : pedidosCliente
    where nomeProdDesejado codDesejado = head [nomeProd | (codProd, nomeProd, precoProd) <- menu, codProd == codDesejado]





{- (b) Cancela um produto já solicitado pelo cliente, informando o código e a
quantidade do produto a ser cancelado. Por simplicidade, suponha que o código a
ser cancelado existe na lista de pedidos do cliente. No ato do cancelamento, se a
quantidade a ser cancelada for igual ou superior à quantidade já solicitada, o item
deve ser removido da lista de pedidos do cliente. Se o cancelamento for parcial, o
item permanecerá na lista, mas com a quantidade decrementada da quantidade
cancelada.
cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente -} 

cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto pedidosCliente codDesejado quantDesejada
  | quantDesejada >= (quantPedido pedidosCliente codDesejado) = [(codProd, nomeProd, quant) | (codProd, nomeProd, quant) <- pedidosCliente, codProd /= codDesejado]
  
  | otherwise = [(codProd, nomeProd, quant - quantDesejada) | (codProd, nomeProd, quant) <- pedidosCliente, codProd == codDesejado] ++ [(codProd, nomeProd, quant) | (codProd, nomeProd, quant) <- pedidosCliente, codProd /= codDesejado]

  where quantPedido pedidosCliente codDesejado = head [quant | (codProd, nomeProd, quant) <- pedidosCliente, codProd == codDesejado]




-- (c)Adiciona  um  pedido  de um cliente na lista global de  pedidos do restaurante quando a conta é paga. O pedido será adicionado no final dalista vigente e o código do pedido será o código do cliente.

adicionaPedido :: Pedidos -> PedidoCliente -> CodCliente -> Pedidos
adicionaPedido pedidos pedidosCliente codCliente = pedidos ++ pedidoAtual
  where pedidoAtual = [(codCliente, pedidosCliente)]
 



{- (d)  Gera  lista  completa  do  pedido,  que  será  usada para  a  impressão  da  conta quando esta for finalizada.Os precos de cada produto são coletados do cardápio, usando o código do produto para fazer a coleta. O preco gerado na lista de saída já é   o   preco   do   item totalizado,   ou   seja,   o   preco   unitário   multiplicado   pela quantidade

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]

geraPedidoImpressao:: PedidoCliente-> Menu -> PedidoTotalizado -}

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]

geraPedidoImpressao :: PedidoCliente-> Menu -> PedidoTotalizado
geraPedidoImpressao pedidosCliente menu = [(codDesejado, nomeProd, quant, quant * (preco codDesejado) ) | (codDesejado, nomeProd, quant) <- pedidosCliente]
  where
    preco codDesejado = head [precoProd | (codProd, nomeProd, precoProd) <- menu, codProd == codDesejado]




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


totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clientes codDesejado pedidoTotalizado
  | categoria == 'A' = (precoConta, 0, precoConta)
  | categoria == 'B' = (precoConta, floor (0.03 * fromIntegral precoConta), precoConta - (floor (0.03* fromIntegral precoConta)))
  | categoria == 'C' = (precoConta, floor (0.05 * fromIntegral precoConta), precoConta - (floor (0.05 * fromIntegral precoConta)))
  | categoria == 'D' = (precoConta, floor (0.1 * fromIntegral precoConta), precoConta - (floor (0.1 * fromIntegral precoConta)))
  | categoria == 'E' = (precoConta, floor (0.15 * fromIntegral precoConta), precoConta - (floor ( 0.15 * fromIntegral precoConta)))
  where 
    clienteCategoria clientes codDesejado = head [categCliente | (codCliente, nomeCliente, categCliente, consumoAnual) <- clientes, codCliente == codDesejado]

    categoria = (clienteCategoria clientes codDesejado)
    
    precoConta = sum [preco | (codProd, nomeProd, quant, preco) <- pedidoTotalizado]




{- (f) Entrega de pedido. O cliente informa seu código ao atendente da coleta. O
pedido é entregue e confirmado. Ao confirmar a entrega, o sistema remove o
pedido entregue da lista de pedidos do restaurante através da função abaixo.
entregaPedido :: Pedidos-> CodCliente -> Pedidos -}


entregaPedido :: Pedidos-> CodCliente -> Pedidos
entregaPedido pedidos codDesejado = [(codCliente, pedidoCliente) | (codCliente, pedidoCliente) <- pedidos, codCliente /= codDesejado]





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
formataLinhas pedidoTotalizado = concat [(formataLinha ((codProd, nomeProd, quant, preco))) | (codProd, nomeProd, quant, preco) <- pedidoTotalizado]






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
