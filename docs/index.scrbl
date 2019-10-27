#lang scribble/base

@(require scribble/manual scriblib/figure)

@title{Rasterizador}

@section{Inicializando (requer Racket)}

@itemize[
 @item{
  Rode
  @commandline{scribble doc/rasterizador.scrbl}
  Para gerar o HTML dessa documentação; ou
  
  @commandline{scribble --pdf doc/rasterizador.scrbl}
  Para gerar como PDF}

 @item{
  Rode
  @commandline{racket test.rkt}
  Para inicializar um Rasterizador}]

@section{Definindo um Rasterizador}

@itemize[
         
 @item{Copie para um arquivo Racket

  @codeblock{
   #lang racket

   (require "serve.rkt")

   (make-grid x-scale y-scale pixel-size)

   (app)}

  Em que x-scale e y-scale são as quantidades de pixels em cada eixo do Grid e pixel-size é o tamanho do pixel (em pixels do monitor)}

 @item{
  Salve como @italic{nome}.rkt e rode
  @commandline{racket @italic{nome}.rkt}}]


@section{Interagindo com um Rasterizador}

@itemize[
 @item{@bold{Coordenada (X, Y, X1 ...) - formato:} Inteiro positivo compatível com o tamanho do Grid}    

 @item{@bold{Color - formato:} (r, g, b), onde r, g e b são inteiros positivos menores que 256}

 @item{@bold{Fill? - formato:} Qualquer inteiro indicando para desenhar a figura preenchida ou #f para desenhar somente o contorno}

 @item{@bold{Point:} Dado X, Y e Color, o pixel correspondente é desenhado com a cor dada}

 @item{@bold{Line:} Um segmento de reta é desenhado do ponto (X1, Y1) até o (X2, Y2)}

 @item{@bold{Circle:} Um circulo com centro (X, Y) e raio R (inteiro positivo compatível) é desenhado}

 @item{@bold{Rectangle:} Um retângulo é desenhado do ponto (X, Y) até o (X + A, Y + B), para A e B inteiros positivos}

 @item{@bold{Curve:} Uma curva quadratica é desenhada do ponto (X1, Y1) até o (X3, Y3) com ponto de controle (X2, Y2) e largura Length (inteiro positivo)}

 @item{@bold{Figures:} Utilizando sintaxe Racket é possivel desenhar várias figuras de uma vez. Exemplo:
        @commandline{((rect 0 0 200 140 0 (0,168,89)) (trian 17 70 100 17 182 70 0 (255,204,41)) (trian 17 70 100 122 182 70 0 (255,204,41)) (circle 100 70 35 0 (62,64,149)) (curve 66 63 105 53 133 83 6 (255,255,255)) (point 66 60 (255,255,255)))}
        Obs: entre duas expressões de figuras deve haver somente um espaço}]
  

       
