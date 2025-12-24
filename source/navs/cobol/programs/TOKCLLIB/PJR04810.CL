/. ***********************************************************  ./
/. *   サカタのタネ　                                        *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム                     *  ./
/. *   JOB-ID      :    PJR04810                             *  ./
/. *   JOB-NAME    :    受領返品データ承認入力　　　　　　　 *  ./
/. ***********************************************************  ./
    PGM (P1-?JIKKBN)

/.##INパラメタ##./
    PARA  ?JIKKBN ,STRING*1,IN,VALUE-' '
            /.実行種別                  ./
            /.  ' '    :   通常起動     ./
            /.  '1'    :   纏め機動　   ./

/.--------------------------------------------------------------./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?PGMES    ,STRING*5                     /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ  ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PJR04810'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./

    VAR ?BUMON    ,STRING*4,VALUE-'    '         /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '     /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '           /.担当者CD./
    VAR ?PGNM     ,STRING*40                     /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                     /.      2    ./
    VAR ?KEKA2    ,STRING*40                     /.      3    ./
    VAR ?KEKA3    ,STRING*40                     /.      4    ./
    VAR ?KEKA4    ,STRING*40                     /.      5    ./
              /.ワークファイルＩＤ表示用./
    VAR       ?NHACLSTW ,STRING*17,VALUE-'                 '
    /.端末別ファイルＩＤ編集用./
              /.端末番号./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '
              /.一覧ワーク./
    VAR       ?CRWK     ,STRING*4,VALUE-'CRWK'
    VAR       ?CRWKF    ,STRING*1,VALUE-'F'
    VAR       ?CRWK1    ,STRING*1,VALUE-'1'
    VAR       ?CRWKXXXF ,NAME!MOD
    VAR       ?CRWKXXX1 ,NAME!MOD
    VAR       ?P4       ,STRING*2,VALUE-'0'        /.倉庫ＣＤ  ./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'HGWORKLB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM  :=  '受領返品データ承認入力'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB/TOKMDLIB/TOKKLIB/TOKSOLIB/
            HGWORKLB/TOKDLIB

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P7)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO ABEND
    END

/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 端末NO = ' && ?TANNO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##端末別リスト出力ワークファイルＩＤ取得##./
   /.発注書店舗ワーク./
    ?FILNM    :=    ?CRWK && ?TANNO && ?CRWK1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CRWKXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?CRWKXXXF)
    ?MSGX     :=    '##発注書店舗ワーク　　=' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##部門ｺｰﾄﾞ担当者ｺｰﾄﾞ取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##受領返品データ承認入力##./
SJR0481I:

    ?STEP :=   'SJR0481I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JIKKBN = '1' THEN
        OVRF FILE-COMRHEL1,TOFILE-COMRMHL2.TOKDTLIB
    END

    OVRF FILE-COMRWKL1,TOFILE-?CRWKXXXF
    CALL      SJR0481I.TOKSOLIB,PARA-(?JIKKBN,?BUMON,?TANCD,?JIKKBN)

    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【受領返品データ承認入力】'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC


ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '受領返品データ承認入力　が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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