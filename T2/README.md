INF1771 - T2 - 2014.1
=====================

Instruções de Uso [ Prolog ]
----------------------------

  São fornecidos dois binários 32-bit na pasta `bin`. A sequência de execução
    do programa é a seguinte:

  1) Preparar os arquivos de entrada `map.txt` e `items.txt` em `data`
  
  2) No terminal, partindo da raiz do projeto:
      cd build
      prepare_input.bat

    obs!
    |  Os outros arquivos `Batch` fornecidos em `build/` podem ser usados para 
    |    compilar o `parser` de input, assim como o executador do `Prolog`.

  3) Feito isso, será gerado um arquivo `problem.pl` em `src/prolog/` que 
       contém a definição do problema. Ao final deste arquivo pode ser alterada
       a posição inicial do agente, não esquecendo-se de alterar igualmente os
       predicados `safe` e `visited`. 
     Idealmente, alterando-se os arquivos `map.txt` e `items.txt` e repetindo-se
       o passo 2, mudanças sobre a estrutura do mapa e posicionamento de itens
       e indicadores de perigos serão feitas.

     !!! OBS !!!
      A posição padrão do link pode ser alterada no final do arquivo 
        `prolog_aux.lua` que encontra-se na raiz do projeto.

  4) No terminal, a partir da raiz faça:
       
       cd bin
       run_prolog_x86.exe

  5) Os outros arquivos `Batch` fornecidos em `build/` podem ser usados para
       compilar o `parser` de input, assim como o executador do `Prolog`.
     Para tanto é utilizado o GCC, e espera-se que o SWI-Prolog encontre-se 
       instalado no `Path` apresentado em sala de aula, isto é:
       "C:\Program Files (x86)\swipl\".

Instruções de Uso [ Visualização ]
----------------------------------
  
  1) Assumindo que os passos foram feitos, haverá um viz.log em logs/ 
  Caso você não tenha gerado, existe um para demonstração. 
  Para executar a visualização, va na raiz:

    cd visualization/
    love .



Contato
-------

  Carlos Mattoso  [ 1210553 ]
    calmattoso _at_ hotmail _dot_ com
  Leonardo Kaplan [ 1212509 ]
    contato _dot_ leonardokaplan _at_ gmail _dot_ com