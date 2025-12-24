/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ナフコ新ＥＤＩ                       *  ./
/. *   JOB-ID      :    PSY37330                             *  ./
/. *   JOB-NAME    :    取込データ振分処理                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY37330'
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?R        ,STRING*1,VALUE-'R'
    VAR       ?W        ,STRING*1,VALUE-'W'
    VAR       ?TANCD    ,STRING*2,VALUE-'  '
    VAR       ?BUMON    ,STRING*4,VALUE-'    '
    VAR       ?TANBUM   ,STRING*6
    VAR       ?INDATE   ,STRING*8,VALUE-'20111227'
    VAR       ?INTIME   ,STRING*6,VALUE-'133014'
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'テクノス出荷確定データ取込'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKFLIB/TOKELIB/TOKKLIB/TOKWLIB/TOKELIBO

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              ?KEKA4 :=  '【部門取得】'
              GOTO ABEND
    END

/.##担当者CD指定##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  ?KEKA4 :=  '【担当者ＣＤ指定】'
                  GOTO  ABEND
              END
    END

/.##数量／箱数確定Ｆ作成##./
SSY3740B:

    ?STEP :=   'SSY3740B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TRKAKUL2,TOFILE-TRKAKUL2.TOKKLIB
    OVRF      FILE-TAHAKOL2,TOFILE-TRHAKOL2.TOKKLIB
    OVRF      FILE-NFSUTEL4,TOFILE-NFSUTEL4.TOKKLIB
    OVRF      FILE-NFHAKOL4,TOFILE-NFHAKOL4.TOKKLIB
    CALL      PGM-SSY3740B.TOKELIBO,
              PARA-(?INDATE,?INTIME)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【数量／箱数確定Ｆ作成】'
              GOTO ABEND END

RTN:

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '取込処理が正常終了しました。'
    ?KEKA2 :=  'エラーリストが発行された場合は確認を'
    ?KEKA3 :=  '行なって下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX    :=  '## 取込日次 = ' && ?INDATE  &&
                 ' 取込時間 = ' && ?INTIME
    SNDMSG MSG-?MSGX,TO-XCTL
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '取込処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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