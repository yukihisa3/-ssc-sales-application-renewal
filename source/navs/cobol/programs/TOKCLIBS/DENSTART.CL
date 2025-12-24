/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    DENSTART                             *  ./
/. *   JOB-NAME    :    伝票更新処理スタート                 * ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'DENSTART'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    DEFLIBL TOKELIB/TOKFLIB

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
    ?OPR2  :=  '　　伝票更新処理  ＳＴＡＲＴ'
    ?OPR3  :=  '　　伝票更新処理を開始致します。'
    ?OPR4  :=  '　　確認して下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ＶＬＤＦの開設##./
PACTVLD:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ACTVLD VLD-500,MAXDATA-500
/.  ACTVLD VLD-600,MAXDATA-500
./
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
    IF        ?CHK  =  '1'        THEN
              MENU MDENSTRE.TOKMLIB
              RETURN
    END

/.##伝票更新ジョブ投入##./
PSBMJOB:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  SBMJOB JOB-DENPYONW,JOBD-XJDV5R4.XUCL,JOBK-@B,./
    SBMJOB JOB-DENPYONW,JOBD-SKTJOBD.XUCL,JOBK-@B,
           PGM-PNJ80000.TOKCLIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

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