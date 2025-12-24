/. ***********************************************************  ./
/. *     サカタのタネ　　　　　　　　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    コーナンＥＤＩ                  　   *  ./
/. *   JOB-ID      :    PSY12130                             *  ./
/. *   JOB-NAME    :    ＰＯＲケース用ピッキングリスト発行   *  ./
/. *   MENU        :    XXXXXXXX-XX                          *  ./
/. *   CREATE      :    2021.01.06 S22460450                 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY12130'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8,VALUE-'        '    /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?JTORI    ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?SOKCD    ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?OSOKCD   ,STRING*2,VALUE-'00'          /.倉庫(指示)./
    VAR ?CENTERF  ,STRING*5,VALUE-'00000'       /.センター(FROM)./
    VAR ?CENTERT  ,STRING*5,VALUE-'00000'       /.センター(TO)  ./
    VAR ?ROUTEF   ,STRING*2,VALUE-'00'          /.ルート　(FROM)./
    VAR ?ROUTET   ,STRING*2,VALUE-'00'          /.ルート　(TO   ./
    VAR ?BUMONF   ,STRING*2,VALUE-'00'          /.部門　　(FROM)./
    VAR ?BUMONT   ,STRING*2,VALUE-'00'          /.部門　　(TO  )./
    VAR ?HDATE    ,STRING*8,VALUE-'00000000'    /.発注日./
    VAR ?NDATE    ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?TANAF    ,STRING*6,VALUE-'000000'      /._番　　(FROM)./
    VAR ?TANAT    ,STRING*6,VALUE-'000000'      /._番　　(TO  )./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.作成件数./
    VAR ?SYORINM1 ,STRING*40
    VAR ?SYORINM2 ,STRING*50
    VAR ?SYORINM3 ,STRING*50
    VAR ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                  /.      2    ./
    VAR ?KEKA2    ,STRING*40                  /.      3    ./
    VAR ?KEKA3    ,STRING*40                  /.      4    ./
    VAR ?KEKA4    ,STRING*40                  /.      5    ./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ＰＯＲケース用ピッキングリスト発行'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKSOLIB/TOKFLIB/TOKKLIB/TOKDTLIB/
            TOKMDLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##ＰＯＲケース用ピッキングリスト発行指示##./
SSY1213I:

    ?STEP :=  'SSY1213I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-KNCASEL2,TOFILE-KNCASEL2.TOKDTLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY1213I.TOKSOLIB
             ,PARA-(?BUMON,?TANCD,?SOKCD,?DSOKCD,
                    ?JDATE,?JTIME,?JTORI,?OSOKCD,?CENTERF,?CENTERT,
                    ?ROUTEF,?ROUTET,?BUMONF,?BUMONT,?HDATE,?NDATE,
                    ?TANAF,?TANAT)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  'ピッキングリスト発行指示'
              GOTO ABEND
         END
    END

    ?MSGX :=  'バッチ（日）＝' && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（時）＝' && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（取）＝' && ?JTORI
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '出力倉庫　　＝' && ?OSOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始センター＝' && ?CENTERF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了センター＝' && ?CENTERT
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始ルート　＝' && ?ROUTEF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了ルート　＝' && ?ROUTET
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始部門　　＝' && ?BUMONF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了部門　　＝' && ?BUMONT
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '発注日　　　＝' && ?HDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日　　　＝' && ?NDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始_番　　＝' && ?TANAF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了_番　　＝' && ?TANAT
    SNDMSG    ?MSGX,TO-XCTL

/.##ＰＯＲ一括用ピッキングリストデータ抽出##./
SSY1214B:

    ?STEP :=   'SSY1214B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KNCASEL2,TOFILE-KNCASEL2.TOKDTLIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-KNPKLCWK,TOFILE-KNPKLCWK.TOKDTLIB
    CALL      PGM-SSY1214B.TOKSOLIB,
              PARA-(?JDATE,?JTIME,?JTORI,?OSOKCD,?CENTERF,?CENTERT,
                    ?ROUTEF,?ROUTET,?BUMONF,?BUMONT,?HDATE,?NDATE,
                    ?TANAF,?TANAT)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'リストデータ抽出'
              GOTO ABEND
    END

/.##ＰＯＲケース用ピッキングリスト出力##./
SSY1215L:

    ?STEP :=   'SSY1215L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-KNPKLCW1,TOFILE-KNPKLCW1.TOKDTLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-SSY1215L.TOKSOLIB

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ピッキングリスト出力'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
