/. ***********************************************************  ./
/. *     サカタのタネ　営業第２部　ＮＡＶＳ基幹システム      *  ./
/. *   SYSTEM-NAME :    営利種子ストックＮＯ管理　　　　　   *  ./
/. *   JOB-ID      :    PST00400                             *  ./
/. *   JOB-NAME    :    ストックＮＯ実績データ抽出指示　　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PST00400'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.取引先ＣＤ./
    VAR ?P2       ,STRING*8,VALUE-'00000000'    /.開始納品日./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.終了納品日./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.開始倉庫ＣＤ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.終了倉庫ＣＤ./
    VAR ?P6       ,STRING*13,VALUE-'             '
    VAR ?P7       ,STRING*13,VALUE-'             '
    VAR ?P8       ,STRING*6,VALUE-'000000'      /.開始ＳＴＮＯ./
    VAR ?P9       ,STRING*6,VALUE-'000000'      /.終了ＳＴＮＯ./
    VAR ?P10      ,STRING*1,VALUE-'0'           /.ストックＣＫ./
    VAR ?P11      ,STRING*1,VALUE-'0'           /.出荷数ＣＫ./
    VAR ?SOKCD    ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ストックＮＯ実績データ再抽出'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKMDLIB/TOKSOLIB/TOKDTLIB
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##ストックＮＯ実績データ抽出指示##./
SST0040I:

    ?STEP :=   'SST0040I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SST0040I.TOKSOLIB
             ,PARA-(?SOKCD,?DSOKCD,?P1,?P2,?P3,?P4,?P5,?P6,?P7,
                   ?P8,?P9,?P10,?P11)
    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  'ストックＮＯ実績データ抽出指示'
              GOTO ABEND
         END
    END
/.  ?MSGX :=  '##取引先ＣＤ= ' && ?P1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##開始納品日= ' && ?P2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##終了納品日= ' && ?P3
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##開始倉庫  = ' && ?P4
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##狩猟倉庫　= ' && ?P5
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##開始商品  = ' && ?P6
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##終了商品  = ' && ?P7
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##開始ＳＴＮ= ' && ?P8
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##終了ＳＴＮ= ' && ?P9
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##ＳＴＮＣＫ= ' && ?P10
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##出荷数ＣＫ= ' && ?P11
    SNDMSG    ?MSGX,TO-XCTL
./
/.##ストックＮＯ実績データ出力##./
SST0050V:

    ?STEP :=   'SST0050V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SST0050V.TOKSOLIB
             ,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7,
                   ?P8,?P9,?P10,?P11)
    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ストックＮＯ実績データ出力'
              GOTO ABEND END

/.##ＣＳＶデータＰＣ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-STNOJCSV.TOKDTLIB,PARA-STNOJCSV,UNIT-1
    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＣＳＶデータＰＣ転送'
              GOTO ABEND END

RTN:

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