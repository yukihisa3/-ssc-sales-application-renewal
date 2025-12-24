/. ***********************************************************  ./
/. *     ÉTÉJÉ^ÇÃÉ^ÉlÅ@ì¡îÃÉVÉXÉeÉÄÅiñ{é–ÉVÉXÉeÉÄÅj          *  ./
/. *   SYSTEM-NAME :    Ç`ÇbÇnÇrêUë÷â¸ëPÅ@Å@                 *  ./
/. *   JOB-ID      :    PFU32100                             *  ./
/. *   JOB-NAME    :    Ç`ÇbÇnÇrêUë÷è∆âÔÅ@Å@Å@Å@Å@Å@         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PFU32100'
    VAR       ?STEP     ,STRING*8
    VAR       ?SOKCD    ,STRING*2,VALUE-'  '       /.ëqå…CD./
    VAR       ?DSOKCD   ,STRING*2,VALUE-'  '       /.ë„ï\ëqå…CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.‹∞∏Ω√∞ºÆ›ï∂éö./
    VAR       ?WKSTN    ,NAME!MOD                  /.‹∞∏Ω√∞ºÆ›ñºëO./
    VAR       ?BUMONI   ,STRING*4,VALUE-'    '     /.ïîñÂCD./
    VAR       ?TANTOI   ,STRING*8,VALUE-'        ' /.íSìñé“CD./
    VAR       ?PGNM     ,STRING*40                 /.“Øæ∞ºﬁ1    ./
    VAR       ?KEKA1    ,STRING*40                 /.      2    ./
    VAR       ?KEKA2    ,STRING*40                 /.      3    ./
    VAR       ?KEKA3    ,STRING*40                 /.      4    ./
    VAR       ?KEKA4    ,STRING*40                 /.      5    ./

/.##Ãﬂ€∏ﬁ◊—äJén“Øæ∞ºﬁ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?PGNM := 'ì`ï[ÇdÇwÇbÇdÇkéÊçû_É`ÉFÉbÉNÇÃÇ›'

/.##◊≤Ãﬁ◊ÿÿΩƒìoò^##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKDTLIB/TOKKLIB/TOKELIBO/TOKDLIB
             /ZAIEXCEL/TOKMDLIB

/.## ‹∞∏Ω√∞ºÆ›ñºéÊìæ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ‹∞∏Ω√∞ºÆ›ñº = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ëqå…∫∞ƒﬁéÊìæ##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              ?KEKA4 := 'ëqå…ÉRÅ[ÉhéÊìæ'
              GOTO ABEND
    END

/.##ïîñÂ∫∞ƒﬁíSìñé“∫∞ƒﬁéÊìæ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMONI,?TANTOI)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              ?KEKA4 := 'ïîñÂÇbÇcíSìñé“ÇbÇcéÊìæ'
              GOTO ABEND
    END

/.##Ç`ÇbÇnÇrêUë÷è∆âÔ##./
SFU3210I:

    ?STEP :=   'SFU3210I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL PGM-SFU3210I.TOKSOLIB,PARA-(?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Ç`ÇbÇnÇrêUë÷è∆âÔ'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

    ?KEKA1 :=  'Ç`ÇbÇnÇrêUë÷è∆âÔÇ™àŸèÌèIóπÇµÇ‹ÇµÇΩÅB'
    ?KEKA2 :=  ''
    ?KEKA3 :=  'ÉçÉOçÃéÊÇµÅCÇmÇ`ÇuÇ÷òAóçÇµÇƒâ∫Ç≥Ç¢ÅB'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
