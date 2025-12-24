# NSCLO

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/NSCLO.CL`

## ソースコード

```jcl
/.  SCL                     ./
PGM    (SRC-?WSRC,
        SRCL-?SRCL,
        CONFL-?CONFL,
        STORE-?STORE,
        REP-?REP,
        TRACE-?TRACE)
    VAR    ?PGMEC   ,INTEGER,VALUE-0
    VAR    ?PGM     ,STRING*99
    VAR    ?COMENT  ,STRING*20
    VAR    ?NAME    ,NAME!MOD!GEN
    VAR    ?LIB     ,NAME,VALUE-TOKCLIB
    VAR    ?SRCN    ,STRING*8
/.                           ./
    PARA   ?WSRC  ,NAME!MOD!COM(100),IN,REQ
/.  PARA   ?SRC   ,NAME!MOD,IN,REQ           ./
    PARA   ?STORE ,INTEGER,IN,VALUE-@YES
    PARA   ?REP   ,INTEGER,IN,VALUE-@YES
    PARA   ?SRCL  ,INTEGER,IN,VALUE-@NO
    PARA   ?CONFL ,INTEGER,IN,VALUE-@NO
    PARA   ?TRACE ,INTEGER,IN,VALUE-@NO
/.                           ./

START:

    SNDMSG MSG-'<< NSCL  翻訳開始 >>',TO-XCTL
    OVRPRTF FILE-XLIST,TOFILE-COMP1.XUCL
    OVRPRTF FILE-XELIST,TOFILE-COMP2.XUCL


STEPO1:
/.  HLDSPLF   FILE-*   ./

    FOR       ?X   :=        1    STEP      1     TO       100
     DO  IF   %CHECK(?WSRC(?X))   THEN
              ?NAME      :=            %NCAT(?WSRC(?X),?LIB)
              ?SRCN      :=            %STRING(?WSRC(?X))
              SNDMSG MSG-?SRCN,TO-XCTL

CL SRC-?NAME,STORE-@YES,OBJLIB-TOKCLIBO,REP-@YES,
   SRCL-@NO,PCHECK-@YES,WORK-100
              IF   @PGMEC  ^=   0      THEN
                   IF   @PGMEL  =   'I'
                        THEN      ?COMENT   :=   '<]]NSCL  翻訳ｴﾗｰ:'
                        ?PGM      :=   ?COMENT && @PGMEM
                                    && ' '    && @PGMES  && ' >'
                        SNDMSG    MSG-?PGM,TO-XCTL
                   ELSE
                                  ?COMENT   :=   '<]]NSCL  ABEND  :'
                        GOTO      RTN  END
                   END
              END
    END
RTN:
     ?PGMEC    :=   @PGMEC
     ?PGM      :=   ?COMENT && @PGMEM && ' ' &&  @PGMES && ' >'
     IF   ?PGMEC    ^=   0
          THEN      SNDMSG    MSG-?PGM,TO-XCTL
          END
     SNDMSG    MSG-'<< NSCL  翻訳終了 >>',TO-XCTL
     RETURN    PGMEC-?PGMEC

```
