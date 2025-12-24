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

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    DEFLIBL TOKELIB/TOKFLIB

/.##実行ﾜｰｸｽﾃｰｼｮﾝ取得##./

/.##ＶＬＤＦの開設##./
PACTVLD:

    ACTVLD VLD-500,MAXDATA-500
    ACTVLD VLD-600,MAXDATA-500

/.##伝票更新ジョブ投入##./
PSBMJOB:

    SBMJOB JOB-DENPYO,JOBD-SKTJOBD.XUCL,JOBK-@B,
           PGM-PKY03010.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:    /.##ﾌﾟﾛｸﾞﾗﾑ正常終了時##./

    RETURN    PGMEC-@PGMEC

ABEND:  /.##ﾌﾟﾛｸﾞﾗﾑ異常終了時##./

    DSPLOG CLR-@YES,EDT-@JEF

    RETURN    PGMEC-@PGMEC
