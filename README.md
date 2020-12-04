# Linguagem HCB
Linguagem de programação baseada na Linguagem B

# Versão 0.9
Compativel com 8086 e DOS

Objetivo: uma linguagem extremamente portavel, onde se possa criar um compilador em quaisquer linguagem em pouquissimo tempo.

Esta linguagem eh baseada na linguagem B, onde nao se tem tipos, sendo todas as variaveis de tamanho da WORD padrao do processador, exemplo neste prototipo de compilador feito para 8086, todas as variaveis tem 16bits de tamanho.

Compilador atual feito em BASIC no padrao QuickBASIC/Visual Basic for DOS/QB64

# Novas funcoes:
- Suporte basico a linguagem:
	- Declaracao de funcoes/variaveis externas pelo comando 'extern'
	- Declaracao de variaveis locais pelo comando 'auto'
	- Importacao de arquivos externos pelo comando '#include'
	- Importacao de arquivos assembly pelo comando '#includeasm'
	- Geracao direta de comandos em assembly pelo comando '#asm'
	- Suporte a comandos basicos de logica: if, while
	- Suporte a chamar rotinas locais
	- Suporte a operacoes matematicas: +,-,*,/,%
	- Suporte a operacoes logicas/binarias basicas: <<,>>,<,<=,>,>=,==,!=
- Inclusao de exemplo onde demonstre o uso basico da linguagem, o arquivo vt.hcb eh apenas um exemplo nao sendo considerado parte da biblioteca padrao da linguagem, que sera escrita posteriormente.
