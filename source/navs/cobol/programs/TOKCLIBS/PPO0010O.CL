/. ************************************************************ ./
/. *     サカタのタネ　特販システム（本社システム）           * ./
/. *   SYSTEM-NAME :    ダイユーエイト新ＥＤＩシステム　　　　* ./
/. *   JOB-ID      :    PPO0010O                              * ./
/. *   JOB-NAME    :    ダイユーエイト                       * ./
/. *                    ＰＯＳ売上情報リスト発行              * ./
/. ************************************************************ ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PPO0010O'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.取引先    ./
    VAR ?P2       ,STRING*8,VALUE-'00000000'    /.開始売上期間./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.終了売上期間./
    VAR ?P4       ,STRING*6,VALUE-'000000'      /.開始出力店舗./
    VAR ?P5       ,STRING*6,VALUE-'000000'      /.終了出力店舗./
    VAR ?P6       ,STRING*13,VALUE-'0000000000000' /.開始商品ＣＤ./
    VAR ?P7       ,STRING*13,VALUE-'0000000000000' /.終了商品ＣＤ./
    VAR ?P8       ,STRING*1,VALUE-'0'           /.帳票種別    ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ダイユーエイトＰＯＳ売上情報データ抽出'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


/.##ダイユーエイトＰＯＳ売上情報リスト発行指示##./
STEP010:

    ?STEP :=   'STEP010'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-DYPSURL2,TOFILE-DYPSURL2.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-PSO0010I.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7,?P8)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END
    ?MSGX :=  '取引先コード　　= ' && ?P1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '売上期間開始　　= ' && ?P2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '売上期間終了　　= ' && ?P3
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始出力店舗　　= ' && ?P4
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了出力店舗　　= ' && ?P5
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始商品ＣＤ　　= ' && ?P6
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了商品ＣＤ　　= ' && ?P7
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '帳票種別　　　　= ' && ?P8
    SNDMSG    ?MSGX,TO-XCTL

/.##ダイユーエイトＰＯＳ売上情報Ｆ抽出##./
STEP020:

    ?STEP :=   'STEP020'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DYPSURL1,TOFILE-DYPSURL1.TOKKLIB
    CALL      PGM-PSO0020B.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7,?P8)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＰＯＳ売上情報Ｆ抽出'
              GOTO ABEND
    END

/.##ダイユーエイトＰＯＳ売上情報リスト発行##./
STEP030:

    ?STEP :=   'STEP030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DYPSLTL2,TOFILE-DYPSLTL2.TOKKLIB
    CALL      PGM-PSO0010L.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P6,?P7,?P8)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '情報リスト発行'
              GOTO ABEND
    END

/.##ＰＯＳ情報累積ファイルレコード削除##./
STEP040:

    ?STEP :=   'STEP040'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-PSO0090B.TOKELIBO

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '情報リスト発行'
              GOTO ABEND
    END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '最新データに更新しました。'
    ?KEKA3 :=  'オンラインＰＯＳ情報リスト発行可能です。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
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