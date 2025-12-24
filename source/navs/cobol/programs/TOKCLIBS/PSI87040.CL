/. ***********************************************************  ./
/. *     ÉTÉJÉ^ÇÃÉ^ÉlÅ@ì¡îÃÉVÉXÉeÉÄÅiñ{é–ÉVÉXÉeÉÄÅj          *  ./
/. *   SYSTEM-NAME :    éxï•è∆çáÅ@ÇcÇbÇlÇiÇ`ÇoÇ`Çm           *  ./
/. *   JOB-ID      :    PSI87041                             *  ./
/. *   JOB-NAME    :    ïsè∆çáÉäÉXÉgçÏê¨                     *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/. *óöó******************************************************  ./
/. *   2018/03/07  ÉPÅ[ÉàÅ[ëŒâûÅií«â¡Åj                      *  ./
/. *   0000/00/00                                            *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI87041'
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                  /.“Øæ∞ºﬁ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./

/.##Ãﬂ€∏ﬁ◊—ñºèÃæØƒ##./
    ?PGNM :=  'ÇcÇbÇlÇiÇ`ÇoÇ`ÇmÅiéxï•ÉfÅ[É^ÅjéÊçûïœä∑èàóù'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ÇrÇnÇqÇsÅiéÊà¯êÊÅ{ì`ï[î‘çÜèáÅj##./
SORT1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SIHARASF.TOKKLIB,INRL-100,INBF-40,
              OUTFILE-SIHARASF.TOKKLIB,OUTBF-40,
              KEY-1!8!CA,
              KEY1-22!9!CA,
              RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ÅyÇrÇnÇqÇsèàóùÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg0880##./
SSI87041:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00000880')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇWÇWÇOÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg0882##./
SSI87042:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00000882')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇWÇWÇQÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg0883##./
SSI87043:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00000883')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇWÇWÇRÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg1427##./
SSI87044:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00001427')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇSÇQÇVÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg14272##./
SSI87045:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00014272')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇSÇQÇVÇQÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg14273##./
SSI87046:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00014273')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇSÇQÇVÇRÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg13938##./
SSI87047:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00013938')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇRÇXÇRÇWÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg17137##./
SSI87048:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00017137')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇVÇPÇRÇVÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg100403##./
SSI87049:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00100403')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇOÇOÇSÇOÇRÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg100441##./
SSI87050:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00100441')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇOÇOÇSÇSÇPÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg100427##./
SSI87051:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00100427')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇOÇOÇSÇQÇVÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg100404##./
SSI87052:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00100404')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇOÇOÇSÇOÇSÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg100442##./
SSI87053:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00100442')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇOÇOÇSÇSÇQÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg100428##./
SSI87054:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00100428')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇOÇOÇSÇQÇWÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg139381##./
SSI87055:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00139381')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇRÇXÇRÇWÇPÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg171371##./
SSI87056:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00171371')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇVÇPÇRÇVÇPÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg1731##./
SSI87057:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00001731')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇVÇRÇPÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg1732##./
SSI87058:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK871.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00001732')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇPÇVÇRÇQÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg7601##./
SSI87059:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00007601')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇVÇUÇOÇPÅz'
              GOTO ABEND END

/.##ïsè∆çáÉäÉXÉg7602##./
SSI87060:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASF.TOKKLIB
    OVRF      FILE-SETGK871,TOFILE-SETGK881.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8704P.TOKELIBO,PARA-('00007602')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'Åyïsè∆çáÉäÉXÉgÅ@ÇVÇUÇOÇQÅz'
              GOTO ABEND END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ÇcÇbÇlÇiÇ`ÇoÇ`Çmïsè∆çáÉäÉXÉgòAë±î≠çs'
    ?KEKA2 :=  'èàóùÇ™ê≥èÌèIóπÇµÇ‹ÇµÇΩÅBî≠çsÇ≥ÇÍí†ï['
    ?KEKA3 :=  'ÇämîFÇµÇƒâ∫Ç≥Ç¢ÅB'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ÇcÇbÇlÇiÇ`ÇoÇ`Çmïsè∆çáÉäÉXÉgòAë±î≠çs'
    ?KEKA2 :=  'èàóùÇ™àŸèÌèIóπÇµÇ‹ÇµÇΩÅBÉçÉOÉäÉXÉgÇ'
    ?KEKA3 :=  'èoóÕÇµÇmÇ`ÇuÇ÷òAóçÇµÇƒâ∫Ç≥Ç¢ÅB'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

    RETURN    PGMEC-@PGMEC
