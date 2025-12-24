/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ケーヨ－オンラインシステム　　　     *  ./
/. *   JOB-ID      :    PSY11010                             *  ./
/. *   JOB-NAME    :    仕入伝票発行           　　　　　　  *  ./
/. *               :    ＜全社分出力＞                       *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*1,IN,VALUE-' ' /.1=自倉庫,2=選択倉庫./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*6,VALUE-'      '
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?JHSDENF  ,NAME!MOD
    VAR ?JHSDENFN ,STRING*17
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY11010'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL TOKFLIB/TOKELIB

/.##自倉庫／選択倉庫判定##./
CHK:
    ?FILNMH := 'JHSDEN'
    IF      ?KBN   =    '2'   THEN
            GOTO              SKY1701I
    END
    IF      ?KBN   =    '3'   THEN
            ?FILNMH := 'SOKDEN'
    END
/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
          ELSE
              GOTO FILNMCHG
    END

/.##倉庫ｺｰﾄﾞ選択##./
SKY1701I:

    ?STEP :=   'SKY1701I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SKY1701I.TOKELIB,PARA-(?SOKCD)
    IF        @PGMEC    ^=   0    THEN
              ?PGMEC := @PGMEC
              IF    ?PGMEC  =  4010
                 THEN
                    ?MSGX := '##取消終了##'
                    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                    RETURN PGMEC-@PGMEC
                 ELSE
                    GOTO   ABEND
                 END
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?SOKCD
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?JHSDENF  :=    %NCAT(?FILID,?LIBID)
    ?JHSDENFN :=    %STRING(?JHSDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?JHSDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  仕入伝票発行（タイプ　　_型）                              ./
PSY11010:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-KSHIRED,TOFILE-?JHSDENF
    CALL      PGM-SSY1101L.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

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
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
