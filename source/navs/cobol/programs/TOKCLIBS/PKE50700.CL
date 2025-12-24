/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷検品システム                     *  ./
/. *   JOB-ID      :    PKE50700                             *  ./
/. *   JOB-NAME    :    (出荷検品）マスタデータ抽出          *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?SNDF1    ,STRING*6,VALUE-'KNPTEN'      /.店舗ﾏｽﾀ./
    VAR ?SNDK1    ,STRING*6,VALUE-'KNPTEK'      /.店舗ﾏｽﾀ件数./
    VAR ?SNDF1H   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF1HN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF1K   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF1KN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF2    ,STRING*6,VALUE-'KNPKEN'      /.検品取引先./
    VAR ?SNDK2    ,STRING*6,VALUE-'KNPKEK'      /.検品取引先件数./
    VAR ?SNDF2H   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF2HN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF2K   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF2KN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF3    ,STRING*6,VALUE-'KNPKPG'      /.検品ｸﾞﾙｰﾌﾟ./
    VAR ?SNDK3    ,STRING*6,VALUE-'KNPKPK'      /.検品ｸﾞﾙｰﾌﾟ件数./
    VAR ?SNDF3H   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF3HN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF3K   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF3KN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF4    ,STRING*6,VALUE-'KNPTAN'      /.ﾛｹｰｼｮﾝﾏｽﾀ./
    VAR ?SNDK4    ,STRING*6,VALUE-'KNPTAK'      /.ﾛｹｰｼｮﾝ件数./
    VAR ?SNDF4H   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF4HN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDF4K   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDF4KN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKWLIB '    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE50700'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*1,VALUE-' '           /.店舗ﾏｽﾀ  ./
    VAR ?P2       ,STRING*1,VALUE-' '            /.商品ﾏｽﾀ  ./
    VAR ?P3       ,STRING*1,VALUE-' '           /.ﾛｹﾏｽﾀ./
    VAR ?P4       ,STRING*1,VALUE-' '            /.検品ｸﾞﾙｰﾌﾟﾏｽﾀ./
    VAR ?P5       ,STRING*1,VALUE-' '            /.倉庫別出荷取引先./
    VAR ?P6       ,STRING*2,VALUE-'  '           /.実行倉庫ＣＤ./
    VAR ?P7       ,STRING*2,VALUE-'  '           /.代表倉庫ＣＤ./
    VAR ?M1       ,STRING*7,VALUE-'ﾃﾝﾎﾟﾏｽﾀ'     /.店舗ﾏｽﾀ  ./
    VAR ?M2       ,STRING*10,VALUE-'ｼｮｳﾋﾝﾒｲｼｮｳ'  /.商品ﾏｽﾀ  ./
    VAR ?M3       ,STRING*9,VALUE-'ﾛｹｰｼｮﾝﾏｽﾀ'   /.ﾛｹﾏｽﾀ./
    VAR ?M4       ,STRING*11,VALUE-'ｹﾝﾋﾟﾝｸﾞﾙｰﾌﾟ' /.検品ｸﾞﾙｰﾌﾟﾏｽﾀ./
    VAR ?M5       ,STRING*11,VALUE-'ｿｳｺﾍﾞﾂｹﾝﾋﾟﾝ' /.倉庫別出荷取引先./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO

/.##倉庫ｺｰﾄﾞ取得##./
STEP000:

    ?STEP :=   'STEP000 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P6,?P7)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##抽出ﾊﾟﾗﾒｰﾀ取得##./
STEP010:

    ?STEP :=   'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SKE5070I.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P5)

    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END

/.##店舗ﾏｽﾀ抽出#./
STEP020:
    /.##店舗ﾏｽﾀ抽出選択の場合##./
    IF   ?P1 ^=  ' '    THEN
         SNDMSG    ?M1,TO-XCTL
         ?FILNM    :=    ?SNDF1 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF1H   :=    %NCAT(?FILID,?LIBID)
         ?SNDF1HN  :=    %STRING(?SNDF1H)
         ?MSGX     :=    '## 店舗ﾌｧｲﾙ名 = ' && ?SNDF1HN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
         ?FILNM    :=    ?SNDK1 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF1K   :=    %NCAT(?FILID,?LIBID)
         ?SNDF1KN  :=    %STRING(?SNDF1K)
         ?MSGX     :=    '## 店舗件数F  = ' && ?SNDF1KN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ELSE
         GOTO       STEP030
    END

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF FILE-SOKKENL1,TOFILE-SOKKENL1.TOKFLIB
    OVRF FILE-KNPTEN01,TOFILE-?SNDF1H
    OVRF FILE-KNPTEK01,TOFILE-?SNDF1K
    CALL      PGM-SKE5080B.TOKELIBO,PARA-(?P6)

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##倉庫別検品取引先ﾏｽﾀ抽出#./
STEP030:
    /.##倉庫別検品取引先ﾏｽﾀ抽出選択の場合##./
    IF   ?P5 ^=  ' '    THEN
         SNDMSG    ?M5,TO-XCTL
         ?FILNM    :=    ?SNDF2 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF2H   :=    %NCAT(?FILID,?LIBID)
         ?SNDF2HN  :=    %STRING(?SNDF2H)
         ?MSGX     :=    '## 倉庫別検品 = ' && ?SNDF2HN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
         ?FILNM    :=    ?SNDK2 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF2K   :=    %NCAT(?FILID,?LIBID)
         ?SNDF2KN  :=    %STRING(?SNDF2K)
         ?MSGX     :=    '## 倉庫別件数 = ' && ?SNDF2KN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ELSE
         GOTO       STEP040
    END

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SOKKENL1,TOFILE-SOKKENL1.TOKFLIB
    OVRF FILE-KNPKEN01,TOFILE-?SNDF2H
    OVRF FILE-KNPKEK01,TOFILE-?SNDF2K
    CALL      PGM-SKE5090B.TOKELIBO,PARA-(?P6)

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##検品ｸﾞﾙｰﾌﾟﾏｽﾀ抽出#./
STEP040:
    /.##検品ｸﾞﾙｰﾌﾟﾏｽﾀ抽出選択の場合##./
    IF   ?P4 ^=  ' '    THEN
         SNDMSG    ?M4,TO-XCTL
         ?FILNM    :=    ?SNDF3 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF3H   :=    %NCAT(?FILID,?LIBID)
         ?SNDF3HN  :=    %STRING(?SNDF3H)
         ?MSGX     :=    '## 検品Ｇﾏｽﾀ = ' && ?SNDF3HN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
         ?FILNM    :=    ?SNDK3 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF3K   :=    %NCAT(?FILID,?LIBID)
         ?SNDF3KN  :=    %STRING(?SNDF3K)
         ?MSGX     :=    '## 検品Ｇ件数= ' && ?SNDF3KN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ELSE
         GOTO       STEP050
    END

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SOKKPGL1,TOFILE-SOKKPGL1.TOKFLIB
    OVRF FILE-KNPKPG01,TOFILE-?SNDF3H
    OVRF FILE-KNPKPK01,TOFILE-?SNDF3K
    CALL      PGM-SKE5100B.TOKELIBO,PARA-(?P6)

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ﾛｹｰｼｮﾝﾏｽﾀ抽出#./
STEP050:
    /.##ﾛｹｰｼｮﾝﾏｽﾀ抽出選択の場合##./
    IF   ?P3 ^=  ' '    THEN
         SNDMSG    ?M3,TO-XCTL
         ?FILNM    :=    ?SNDF4 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF4H   :=    %NCAT(?FILID,?LIBID)
         ?SNDF4HN  :=    %STRING(?SNDF4H)
         ?MSGX     :=    '## ﾛｹｰｼｮﾝＭ = ' && ?SNDF4HN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
         ?FILNM    :=    ?SNDK4 && ?P6
         ?FILID    :=    %NAME(?FILNM)
         ?LIBID    :=    %NAME(?LIBNM)
         ?SNDF4K   :=    %NCAT(?FILID,?LIBID)
         ?SNDF4KN  :=    %STRING(?SNDF4K)
         ?MSGX     :=    '## ﾛｹｰｼｮﾝF  = ' && ?SNDF4KN && ' ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ELSE
         GOTO       STEP060
    END

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SOKKENL1,TOFILE-SOKKENL1.TOKFLIB
    OVRF FILE-KNPTAN01,TOFILE-?SNDF4H
    OVRF FILE-KNPTAK01,TOFILE-?SNDF4K
    CALL      PGM-SKE5110B.TOKELIBO,PARA-(?P6)

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.抽出#./
STEP060:

    IF   ?P2 ^=  ' '    THEN
         SNDMSG    ?M2,TO-XCTL
    END
    IF   ?P3 ^=  ' '    THEN
         SNDMSG    ?M3,TO-XCTL
    END
    IF   ?P4 ^=  ' '    THEN
         SNDMSG    ?M4,TO-XCTL
    END
    IF   ?P5 ^=  ' '    THEN
         SNDMSG    ?M5,TO-XCTL
    END
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

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