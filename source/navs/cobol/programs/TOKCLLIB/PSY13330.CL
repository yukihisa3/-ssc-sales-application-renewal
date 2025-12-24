/. ***********************************************************  ./
/. *     サカタのタネ　　　　　　　　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    コーナンＥＤＩ                  　   *  ./
/. *   JOB-ID      :    PSY13330                             *  ./
/. *   JOB-NAME    :    コーナン送信データ作成（手書）　　   *  ./
/. *   MENU        :    XXXXXXXX-XX                          *  ./
/. *   CREATE      :    2021.09.15 S22470090                 *  ./
/. *   流用　　    :    PSY12330                             *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY13330'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    VAR ?CENTERF  ,STRING*5,VALUE-'00000'       /.店舗ＣＤ(FROM)./
    VAR ?CENTERT  ,STRING*5,VALUE-'00000'       /.店舗ＣＤ(TO)  ./
    VAR ?ROUTEF   ,STRING*2,VALUE-'00'          /.ルート　(FROM)./
    VAR ?ROUTET   ,STRING*2,VALUE-'00'          /.ルート　(TO   ./
    VAR ?BUMONF   ,STRING*2,VALUE-'00'          /.部門　　(FROM)./
    VAR ?BUMONT   ,STRING*2,VALUE-'00'          /.部門　　(TO  )./
    VAR ?HDATE    ,STRING*8,VALUE-'00000000'    /.発注日./
    VAR ?NDATE    ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?TANAF    ,STRING*6,VALUE-'00000'       /._番　　(FROM)./
    VAR ?TANAT    ,STRING*6,VALUE-'00000'       /._番　　(TO  )./
    VAR ?DELFLG   ,STRING*1,VALUE-' '           /.削除有無ＦＬＧ./
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
    ?PGNM := 'コーナン送信データ作成（手書）'
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

/.##コーナン送信データ作成作成指示（手書）##./
SSY1333I:

    ?STEP :=  'SSY1333I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-KTBULKL3,TOFILE-KTBULKL3.TOKDTLIB
    CALL      PGM-SSY1333I.TOKSOLIB
             ,PARA-(?BUMON,?TANCD,?SOKCD,?DSOKCD,
                    ?JDATE,?JTIME,?JTORI,?OSOKCD,?NDATE)

    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  'コーナン送信データ作成指示（手書）'
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
    ?MSGX :=  '納品日　　　＝' && ?NDATE
    SNDMSG    ?MSGX,TO-XCTL

/.##取引先ＰＤデータ抽出（手書）##./
SSY1234B:

    ?STEP :=   'SSY1234B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KNBULKL3,TOFILE-KTBULKL3.TOKDTLIB
    OVRF      FILE-KNSNDPDW,TOFILE-KTSNDPDW.TOKDTLIB
    CALL      PGM-SSY1234B.TOKSOLIB,
              PARA-(?BUMON,?TANCD,?JDATE,?JTIME,?JTORI,?OSOKCD,
                    ?NDATE)
    ?PGMEC    :=    @PGMEC
    IF        ?PGMEC     =   4010 THEN
              SNDMSG    '対象データなし',TO-XCTL
              GOTO ABEND
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '取引先ＰＤデータ抽出（手書）'
              GOTO ABEND
    END

/.##データＳＯＲＴ１##./
SORT1:

    ?STEP :=   'SORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF  INFILE-KTSNDPDW.TOKDTLIB,
           OUTFILE-KTSNDPDW.TOKDTLIB,
           KEY-'FA02!A',     /.取引先コード./
           KEY1-'FA03!A',    /.ルートコード./
           KEY2-'FB!A',      /.ＰＤコード　./
           RCDL-@DSP
    IF     @PGMEC    ^=   0    THEN
           ?KEKA4 :=  'データＳＯＲＴ１'
           GOTO ABEND
    END

/.##取引先ＰＤデータ作成（送信用）（手書）##./
SSY1335B:

    ?STEP :=   'SSY1335B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KTSNDPDW,TOFILE-KTSNDPDW.TOKDTLIB
    OVRF      FILE-KTSNDPD,TOFILE-KTSNDPD.TOKDTLIB
    CALL      PGM-SSY1335B.TOKSOLIB

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '取引先ＰＤデータ作成（送信用）'
              GOTO ABEND
    END

/.##データＳＯＲＴ２##./
SORT2:

    ?STEP :=   'SORT2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTD  INFILE-KTSNDPD.TOKDTLIB,
           INRL-64,
           OUTFILE-KTSNDPD.TOKDTLIB,
           KEY-1!6!DA,     /.取引先コード./
           KEY1-7!2!DA,   /.ルートコード./
           KEY2-11!5!DA,  /.レコード連番./
           RCDL-@DSP
    IF     @PGMEC    ^=   0    THEN
           ?KEKA4 :=  'データＳＯＲＴ２'
           GOTO ABEND
    END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                     ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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