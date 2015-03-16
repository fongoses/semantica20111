# Uso de Scala via Linha de Comando #

**Comandos importantes**:
  1. Como compilar um arquivo(que deve ter a extensão scala):
    * Para compilar um arquivo é necessário ir ao diretório do scala (via linha de comando), geralmente "C:\Arquivos de Programas\Scala\bin"
    * Garantir que o arquivo a ser compilado está na raiz desse diretório
    * Executar o seguinte comando: `scalac Nome_do_arquivo`
    * Se NENHUMA mensagem for exibida, ele compilou sem nenhum problema
    * Caso apareça uma dessas seguintes mensagens de erro:
      * `erro: Acesso negado` - Você está com problemas com as propriedades da pasta
      * `erro: Java.IO.IOExtension` - Você digitou algum texto no arquivo com acentuação
  1. Como executar o seu programa:
    * Basta executar o seguinte comando:	`scala -classpath . NomeObjetoMain`
    * No arquivo que está na SVN é L3, ou seja, `scala -classpath . L3`


### Download ###

Algumas ferramentas para Windows:
  * Compilador: http://www.scala-lang.org/
  * IDE do Notepad++: http://notepad-plus-plus.org/
  * Plugin para o Notepad++: http://lampsvn.epfl.ch/trac/scala/browser/scala-tool-support/trunk/src/notepad-plus/userDefineLang.xml
    * _Copiar o arquivo para a pasta %APPDATA%\Notepad++_