/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    DENSTOP                              *  ./
/. *   JOB-NAME    :    伝票更新更新停止                     * ./
/. *               :    新受配信システムへ移行               *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'DENSTOP '    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?WKSTN    ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ./
    VAR ?WKSTNN   ,NAME                         /.ﾜｰｸｽﾃｰｼｮﾝ名前型./
/.###伝票更新ﾁｪｯｸﾊﾟﾗﾒﾀ###./
    VAR ?CHK      ,STRING*1,VALUE-' '           /.ﾌﾟﾛｸﾞﾗﾑﾊﾟﾗﾒﾀ./

    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB

/.##実行ﾜｰｸｽﾃｰｼｮﾝ取得##./
    ?WKSTNN :=  @ORGWS
    ?WKSTN  :=  %STRING(?WKSTNN)
    ?MSGX   :=  '## ﾜｰｸｽﾃｰｼｮﾝ = ' && ?WKSTN && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  IF     ?WKSTN ^=  'WKSTN001'   THEN
       IF  ?WKSTN ^=  'WKSTN530'   THEN
            MENU MERRWS.TOKMLIB
            RETURN
       END
    END
./
/.##実行確認##./
    ?OPR1  :=  '　＃＃＃＃＃＃＃　伝票更新処理　＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　伝票更新処理  ＥＮＤ'
    ?OPR3  :=  '　　伝票更新処理を終了致します。'
    ?OPR4  :=  '　　確認して下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##伝票更新起動確認##./
KAKUNIN:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
    CALL      PGM-SNJ9040B.TOKELIBO,PARA-(?CHK)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    /.##伝票更新起動済みの場合##./
    IF        ?CHK  =  '0'        THEN
              MENU MDENSTPE.TOKMLIB
              RETURN
    END

/.##伝票更新Ｆ初期化（更新）##./
SNJ9060B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
    CALL      PGM-SNJ9060B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##伝票更新Ｆ初期化（実行）##./
SNJ9070B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
    CALL      PGM-SNJ9070B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##伝票更新ジョブ停止##./
PCANJOB:

    ?STEP :=   'PCANJOB '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CANJOB JOB-DENPYONW.*
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND          END

/.##ＶＬＤ初期化（５００）##./
DVLD500:

    ?STEP :=   'DVLD500 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DACTVLD VLD-500,SWEEP-@YES
 /. IF        @PGMEC    ^=   0    THEN
              GOTO ABEND          END
 ./
/.##ＶＬＤ初期化（６００）##./
DVLD600:

    ?STEP :=   'DVLD600 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DACTVLD VLD-600,SWEEP-@YES
 /. IF        @PGMEC    ^=   0    THEN
              GOTO ABEND          END
 ./
/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:    /.##ﾌﾟﾛｸﾞﾗﾑ正常終了時##./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.##ﾌﾟﾛｸﾞﾗﾑ異常終了時##./

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    DSPLOG CLR-@YES,EDT-@JEF

    RETURN    PGMEC-@PGMEC
