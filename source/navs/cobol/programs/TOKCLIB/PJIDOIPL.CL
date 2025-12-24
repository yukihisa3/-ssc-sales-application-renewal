/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    PJIDOIPL                             *  ./
/. *   JOB-NAME    :    (1)実行制御マスタ更新                *  ./
/. *               :    (2)自動受信時間監視                  *  ./
/. ***********************************************************  ./
    PGM
/.ワークエリア./
    VAR ?RC       ,STRING*1,VALUE-' '           /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PJIDOIPL'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?CHK      ,STRING*1,VALUE-'1'           /.ﾌﾟﾛｸﾞﾗﾑ引数./

    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB

/.**実行制御マスタ自動受信起動更新                         ./
SCV0030B:

    OVRF      FILE-JHMJIKF,TOFILE-JHMJIKL1.TOKFLIB
    CALL      PGM-SCV0030B.TOKELIB,PARA-(?CHK)
    IF  @PGMEC    ^=   0    THEN
        ?RC       :=   '1'
        GOTO ABEND
    END

/.**実行制御マスタ自動受信起動更新                         ./
SCV0070B:

    OVRF      FILE-JHMTJSF,TOFILE-JHMTJSL1.TOKFLIB
    OVRF      FILE-JHMKAIF,TOFILE-JHMKAIL1.TOKFLIB
    OVRF      FILE-JHMEOSF,TOFILE-JHMEOSL1.TOKFLIB
    CALL      PGM-SCV0070B.TOKELIB
    IF  @PGMEC    ^=   0    THEN
        GOTO ABEND
    END

RTN:    /.ﾌﾟﾛｸﾞﾗﾑ正常終了時処理./

    CALL SCVMSG.TOKELIB,PARA-('## JIDO ST OK     ##')
    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    CALL SCVMSG.TOKELIB,PARA-('## JIDO ST ERROR  ##')
    CALL SCVMSG.TOKELIB,PARA-('## NAV ﾍ ﾚﾝﾗｸ     ##')

    RETURN    PGMEC-@PGMEC
