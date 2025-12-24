/. ***********************************************************  ./
/. *   ÉTÉJÉ^ÇÃÉ^Él                                          *  ./
/. *   SYSTEM-NAME :    ó¨í ÇaÇlÇrÅ@Å@                       *  ./
/. *   JOB-ID      :    PSY52100                             *  ./
/. *   JOB-NAME    :    î[ïiì˙àÍäáïœçXÅiîÑè„ÇeÅEî≠íçÇlÇrÇfÅj *  ./
/. *   UPDATE      :    2019/07/24                           *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.‹∞∏Ω√∞ºÆ›ï∂éö./
    VAR       ?WKSTN    ,NAME!MOD                  /.‹∞∏Ω√∞ºÆ›ñºëO./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY52100'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.ïîñÂ./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.ïîñÂ+íSìñé“./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.íSìñé“./
    VAR       ?BTYMD    ,STRING*8,VALUE-'00000000' /.éÛêMì˙ït    ./
    VAR       ?BTTIME   ,STRING*4,VALUE-'0000'     /.éÛêMéûä‘    ./
    VAR       ?BTTOKC   ,STRING*8,VALUE-'00000000' /.éÛêMéÊà¯êÊ  ./
    VAR       ?SOKCD    ,STRING*2,VALUE-'  '       /.ëqå…  ./
    VAR       ?FDATE    ,STRING*8,VALUE-'00000000' /.ïœçXëOî[ïiì˙./
    VAR       ?TDATE    ,STRING*8,VALUE-'00000000' /.ïœçXå„î[ïiì˙./
    VAR       ?DENNO    ,STRING*180

    VAR       ?P7       ,STRING*2,VALUE-'00'       /.ë„ï\ëqå…    ./
    VAR       ?OPR1     ,STRING*50                 /.“Øæ∞ºﬁ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./

/.-----------------------------------------------------------./
    VAR       ?MSG1   ,STRING*80                  /.äJénèIóπMSG./
    VAR       ?PGNM   ,STRING*40                  /.“Øæ∞ºﬁ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
/.-----------------------------------------------------------./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##Ãﬂ€∏ﬁ◊—ñºèÃæØƒ##./
    ?PGNM :=  'î[ïiì˙àÍäáïœçX'

/.##Ãﬂ€∏ﬁ◊—äJén“Øæ∞ºﬁ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?P7)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := 'ëqå…ÉRÅ[ÉhéÊìæ'
              GOTO ABEND
    END

/.##ÉçÉOÉCÉìÉÜÅ[ÉUÅ[èÓïÒéÊìæ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.----------------------------------------------------------------./
/.##î[ïiì˙àÍäáïœçXéwé¶./
SSY5210I:

    ?STEP :=   'SSY5210I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?DENNO := '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000'

    DEFLIBL   TOKELIB/TOKELIBO/TOKDTLIB/TOKDLIB/TOKFLIB/TOKKLIB/
              BMSFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB

    CALL      PGM-SSY5210I.TOKSOLIB,PARA-(?BTYMD,?BTTIME,?BTTOKC,
                                          ?SOKCD,?FDATE,?TDATE,?DENNO)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##éÊè¡èIóπ##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'î[ïiì˙àÍäáïœçXéwé¶'
              GOTO   ABEND
    END

    ?MSGX :=  'éÛêMì˙Å@ÅÅ'  && ?BTYMD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'éÛêMéûçèÅ@Å@ÅÅ'  && ?BTTIME
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'éÊà¯êÊÅ@Å@Å@ÅÅ'  && ?BTTOKC
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'ëqå…ÇbÇcÅ@Å@ÅÅ'  && ?SOKCD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'ïœçXëOî[ïiì˙ÅÅ'  && ?FDATE
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'ïœçXå„î[ïiì˙ÅÅ'  && ?TDATE
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'ì`ï[î‘çÜÇPÅ@ÅÅ'  && %SBSTR(?DENNO,1,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇQÅ@ÅÅ'  && %SBSTR(?DENNO,10,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇRÅ@ÅÅ'  && %SBSTR(?DENNO,19,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇSÅ@ÅÅ'  && %SBSTR(?DENNO,28,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇTÅ@ÅÅ'  && %SBSTR(?DENNO,37,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇUÅ@ÅÅ'  && %SBSTR(?DENNO,46,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇVÅ@ÅÅ'  && %SBSTR(?DENNO,55,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇWÅ@ÅÅ'  && %SBSTR(?DENNO,64,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇXÅ@ÅÅ'  && %SBSTR(?DENNO,73,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇOÅÅ'  && %SBSTR(?DENNO,82,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇPÅÅ'  && %SBSTR(?DENNO,91,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇQÅÅ'  && %SBSTR(?DENNO,100,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇRÅÅ'  && %SBSTR(?DENNO,109,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇSÅÅ'  && %SBSTR(?DENNO,118,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇTÅÅ'  && %SBSTR(?DENNO,127,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇUÅÅ'  && %SBSTR(?DENNO,136,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇVÅÅ'  && %SBSTR(?DENNO,145,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇWÅÅ'  && %SBSTR(?DENNO,154,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇPÇXÅÅ'  && %SBSTR(?DENNO,163,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ì`ï[î‘çÜÇQÇOÅÅ'  && %SBSTR(?DENNO,172,9)
    SNDMSG    ?MSGX,TO-XCTL

/.##î[ïiì˙àÍäáïœçX##./
SSY5220B:

    ?STEP :=   'SSY5220B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    OVRF FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF FILE-BMSHACL3,TOFILE-BMSHACL3.BMSFLIB
    CALL      PGM-SSY5220B.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                          ?BTYMD,?BTTIME,?BTTOKC,
                                          ?SOKCD,?FDATE,?TDATE,?DENNO)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'î[ïiì˙àÍäáïœçX'
              GOTO ABEND END

/.----------------------------------------------------------------./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'î[ïiì˙àÍäáïœçXèàóùÅ@Å@Ç™àŸèÌÇµÇ‹ÇµÇΩÅB'
    ?KEKA2 :=  'ÉçÉOçÃéÊÇµÅCÇmÇ`ÇuÇ÷òAóçÇµÇƒâ∫Ç≥Ç¢ÅB'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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