/.  SCBL                     ./
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
    VAR    ?LIB     ,NAME,VALUE-TOKSRLIB
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
    DEFLIBL  TOKMDLIB/TOKFLIB/TOKELIBO/TOKELIB/TOKDLIB/TOKKLIB/
    TOKWLIB/TOKJLIB/OSKFLIB/BMSFLIB/LINKSLIB/TOKDLIB/ZAIEXCEL/
    ONLBLIB/TOKSOLIB/TOKDTLIB/TOKKPLIB/TOKBILIB
    SNDMSG MSG-'<< SCBL   –|–óŠJŽn >>',TO-XCTL
    OVRPRTF FILE-XLIST,TOFILE-COMP1.XUCL
    OVRPRTF FILE-XELIST,TOFILE-COMP2.XUCL


STEPO1:
/.  HLDSPLF   FILE-*   ./

    FOR       ?X   :=        1    STEP      1     TO       100
     DO  IF   %CHECK(?WSRC(?X))   THEN
              ?NAME      :=            %NCAT(?WSRC(?X),?LIB)
              ?SRCN      :=            %STRING(?WSRC(?X))
              SNDMSG MSG-?SRCN,TO-XCTL

              COBOLG     SRC-?NAME,
                         STORE-?STORE,
                         OBJLIB-TOKSOLIB,
                         REP-@YES,
                         CPYLIB-TOKPLIB,
                         FFDLIB-@LIBL,
                         MEDLIB-@LIBL,
                         SRCL-?SRCL,
                         CONFL-?CONFL,
                         DATE-@SYS,
                         TRACE-?TRACE,
                         SUB-@YES,
                         MEDOCC-@YES,
                         MEDCTL-5,
                         FLOW-@YES,
                         MSGLNG-@NAT
              IF   @PGMEC  ^=   0      THEN
                   IF   @PGMEL  =   'I'
                        THEN      ?COMENT   :=   '<]]SCBL   –|–ó´×°:'
                        ?PGM      :=   ?COMENT && @PGMEM
                                    && ' '    && @PGMES  && ' >'
                        SNDMSG    MSG-?PGM,TO-XCTL
                   ELSE
                                  ?COMENT   :=   '<]]SCBL   ABEND  :'
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
     SNDMSG    MSG-'<< SCBL   –|–óI—¹ >>',TO-XCTL
     RETURN    PGMEC-?PGMEC
