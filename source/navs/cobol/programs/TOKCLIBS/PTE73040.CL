/. ************************************************************ ./
/. *     サカタのタネ　特販システム（本社システム）           * ./
/. *   SYSTEM-NAME :    ダイユーエイト新ＥＤＩシステム　　　　* ./
/. *   JOB-ID      :    PTE73040                              * ./
/. *   JOB-NAME    :    ダイユーエイト納品明細書発行（手書）  * ./
/. *   2018/02/28 カトーレック出力追加　　　　　　　　　　　  * ./
/. ************************************************************ ./
    PGM (P1-?CHK)
/.###ﾊﾟﾗﾒﾀｴﾘｱ定義####./
    PARA ?CHK     ,STRING*1,IN,VALUE-' '        /.起動FLG./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PTE73040'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*5,VALUE-'00000'       /.店舗(開始)./
    VAR ?P7       ,STRING*5,VALUE-'00000'       /.店舗(終了)./
    VAR ?P8       ,STRING*8,VALUE-'00000000'    /.納品日(開始)./
    VAR ?P9       ,STRING*8,VALUE-'00000000'    /.納品日(終了)./
    VAR ?P10      ,STRING*9,VALUE-'000000000'   /.伝票Ｎｏ(開始)./
    VAR ?P11      ,STRING*9,VALUE-'000000000'   /.伝票Ｎｏ(終了)./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ダイユーエイト納品明細書発行（手書）'
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

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P5)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END


/.##ダイユーエイト手書納品明細書発指示##./
STE7304I:

    ?STEP :=   'STE7304I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-DYSYUKL6,TOFILE-DYSYUKL6.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-STE7304I.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7,?P8,?P9,?P10,?P11)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END


/.##ダイユーエイト手書納品明細書発行##./
STE7605L:

    ?STEP :=   'STE7305L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

 /.##本社##./
    IF  ?CHK = '1'  THEN
        OVRPRTF   FILE-XU04LP,TOFILE-DAIYUPRT.XUCL
    END
 /.##片岡##./
    IF  ?CHK = '2'  THEN
        OVRPRTF   FILE-XU04LP,TOFILE-DAIYUKAT.XUCL
    END
 /.##鴻巣##./
    IF  ?CHK = '3'  THEN
        OVRPRTF   FILE-XU04LP,TOFILE-DAIYUKOU.XUCL
    END
 /.##西尾##./
    IF  ?CHK = '4'  THEN
        OVRPRTF   FILE-XU04LP,TOFILE-DAIYUNIS.XUCL
    END
 /.##さくら##./
    IF  ?CHK = '5'  THEN
        OVRPRTF   FILE-XU04LP,TOFILE-DAIYUSKR.XUCL
    END
 /.##カトーレック岡山##./
    IF  ?CHK = '6'  THEN
        OVRPRTF   FILE-XU04LP,TOFILE-DAIYUKAR.TOKMDLIB
    END
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-DYSYUKL3,TOFILE-DYSYUKL3.TOKKLIB
    CALL      PGM-STE7305L.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P6,?P7,?P8,?P9,?P10,?P11)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '手書納品明細書発行'
              GOTO ABEND
    END


    ?MSGX :=  '受信日　　　　　= ' && ?P1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信時刻　　　　= ' && ?P2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '取引先　　　　　= ' && ?P3
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '倉庫　　　　　　= ' && ?P4
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '代表倉庫　　　　= ' && ?P5
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '店舗(開始)　　　= ' && ?P6
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '店舗(終了)　　　= ' && ?P7
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日(開始)　　= ' && ?P8
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日(終了)　　= ' && ?P9
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票Ｎｏ(開始)　= ' && ?P10
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票Ｎｏ(終了)　= ' && ?P11
    SNDMSG    ?MSGX,TO-XCTL


RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '最新データに更新しました。'
    ?KEKA3 :=  'オンライン納品明細書発行可能です。'
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