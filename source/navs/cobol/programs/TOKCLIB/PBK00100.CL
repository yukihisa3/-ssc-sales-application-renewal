/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    オンライン管理システム               *  ./
/. *   JOB-ID      :    PBK00100                             *  ./
/. *   JOB-NAME    :    オンラインデータバックアップ         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'JHSDEN'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'ONLBLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?BACKUPF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?BACKUPFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY00130'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?P2       ,STRING*15,VALUE-'               '  /.ｶﾅ./
    VAR ?P3       ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名称./
    VAR   ?PGNM    ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?KEKA1   ,STRING*40                  /.      2    ./
    VAR   ?KEKA2   ,STRING*40                  /.      3    ./
    VAR   ?KEKA3   ,STRING*40                  /.      4    ./
    VAR   ?KEKA4   ,STRING*40                  /.      5    ./
    VAR   ?OPR1    ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR   ?OPR2    ,STRING*50                  /.      2    ./
    VAR   ?OPR3    ,STRING*50                  /.      3    ./
    VAR   ?OPR4    ,STRING*50                  /.      4    ./
    VAR   ?OPR5    ,STRING*50                  /.      5    ./
/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM  :=  'オンラインデータバックアップ'
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾊﾞｯｸｱｯﾌﾟ指示画面##./
SBK0010I:

    ?STEP :=   'SBK0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-JHMEOSL1,TOFILE-JHMEOSL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBK0010I.TOKELIB,PARA-(?P1,?P2,?P3)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '（取引先選択）'
              GOTO ABEND
    END

/.##オンラインデータ抽出指示入力##./
PHYOJI:

    ?STEP :=   'PHYOJI  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ｹｰﾖｰ特別対応##./
    IF    ?P3 = 'NPS     '  THEN
          ?P3 := 'BAK00173'
    END
/.##ｴﾝﾁｮｰ特別対応##./
    IF    ?P3 = 'ENCHOPC '  THEN
          ?P3 := 'BAK24279'
    END
/.##ｴﾝﾁｮｰ特別対応##./
    IF    ?P3 = 'IONHACS '  THEN
          ?LIBNM := 'TOKFLIB '
    END
/.##　ﾊﾞｯｸｱｯﾌﾟ取引先名表示##./
    ?MSGX     :=    '## BK取引先CD = ' && ?P1 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##　ﾊﾞｯｸｱｯﾌﾟﾌｧｲﾙ名表示##./
    ?MSGX     :=    '## BK取引先名 = ' && ?P2 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?FILNM    :=    ?P3                   /.ﾌｧｲﾙ名ｾｯﾄ./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?BACKUPF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?BACKUPFN :=    %STRING(?BACKUPF)
    ?MSGX     :=    '## BKﾌｧｲﾙ名 = ' && ?BACKUPFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


/.##オンラインデータ抽出##./
PBACKUP:

    ?STEP :=   'SSY0014B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ｼｮﾘｶｲｼﾒｯｾｰｼﾞ ##./
    ?OPR1  :=  'オンラインデータバックアップ処理'
    ?OPR2  :=  'オンラインデータのバックアップを行ないます。'
    ?OPR3  :=  'ＭＯをセットして下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    SAVFILE FILE-?BACKUPF,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '（ＳＡＶＦＩＬＥ）'
              GOTO ABEND
    END
/.##ｲｵﾝ新特別対応##./
    IF    ?P1 = '00087371'  THEN
          SAVFILE FILE-JDATAF1.TOKFLIB/JDATAF2.TOKFLIB/
                  JDATAF3.TOKFLIB/JDATAF4.TOKFLIB/
                  JDATAF5.TOKFLIB/JDATAF6.TOKFLIB/
                  JDATAF7.TOKFLIB/JDATAF8.TOKFLIB/
                  JDATAF9.TOKFLIB/IONHAC1.TOKFLIB/
                  IONHAC2.TOKFLIB/IONHAC3.TOKFLIB/
                  IONHAC4.TOKFLIB/IONHAC5.TOKFLIB/
                  IONHAC6.TOKFLIB,TODEV-MO,MODE-@USED
          IF      @PGMEC    ^=   0   THEN
                  ?KEKA4 :=  '（ＳＡＶＦＩＬＥ２）'
                  GOTO ABEND
          END
    END
/.##ケーヨー特別対応##./
    IF    ?P1 = '00000173'  THEN
          SAVFILE FILE-BAK00173.ONLBLIB,TODEV-MO,MODE-@USED
          IF      @PGMEC    ^=   0   THEN
                  ?KEKA4 :=  '（ＳＡＶＦＩＬＥ３）'
                  GOTO ABEND
          END
    END
/.##エンチョー特別対応##./
    IF    ?P1 = '00024279'  THEN
          SAVFILE FILE-BAK24279.ONLBLIB,TODEV-MO,MODE-@USED
          IF      @PGMEC    ^=   0   THEN
                  ?KEKA4 :=  '（ＳＡＶＦＩＬＥ４）'
                  GOTO ABEND
          END
    END
/.##島忠特別対応##./
    IF    ?P1 = '00072028'  THEN
          SAVFILE FILE-BAK72028.ONLBLIB,TODEV-MO,MODE-@USED
          IF      @PGMEC    ^=   0   THEN
                  ?KEKA4 :=  '（ＳＡＶＦＩＬＥ５）'
                  GOTO ABEND
          END
    END

RTN:

    ?KEKA1 :=  'バックアップが正常終了しました。'
    ?KEKA2 :=  'ＭＯをはずして下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
