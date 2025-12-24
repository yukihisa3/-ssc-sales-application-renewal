/. ***********************************************************  ./
/. *   株式会社サカタのタネ　営業第２部　ＮＡＶシステム      *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援                       *  ./
/. *   JOB-ID      :    PSY38800                             *  ./
/. *   JOB-NAME    :    ナフコ作場数量一括変更　　　　　　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY38800'
    VAR       ?STEP     ,STRING*8
    VAR       ?JSOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫./
    VAR       ?DSOKCD   ,STRING*2,VALUE-'00'       /.代表倉庫./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?KANRI    ,STRING*8,VALUE-'00000000' /.受信日付./
    VAR       ?BTYMD    ,STRING*8,VALUE-'00000000' /.受信日付./
    VAR       ?BTTIME   ,STRING*4,VALUE-'0000'     /.受信時間./
    VAR       ?BTTOKC   ,STRING*8,VALUE-'00000000' /.受信取引先./
    VAR       ?SAKUBA   ,STRING*2,VALUE-'00'       /.作場./
    VAR       ?NOUDT    ,STRING*8,VALUE-'00000000' /.納品日./
    VAR       ?TENPO    ,STRING*5,VALUE-'00000'    /.店舗./
    VAR       ?NFSYOCD  ,STRING*8,VALUE-'00000000' /.ﾅﾌｺ商品CD./
    VAR       ?SKBKBN   ,STRING*1,VALUE-'0'        /.作場変更区分./
    VAR       ?SURKBN   ,STRING*1,VALUE-'0'        /.数量変更区分./
    VAR       ?HENSAKUB ,STRING*2,VALUE-'00'       /.変更作場./
    VAR       ?HENSURYO ,STRING*5,VALUE-'00000'    /.変更数量./
    VAR       ?UPDDATE  ,STRING*8,VALUE-'00000000' /.実行日付./
    VAR       ?UPDTIME  ,STRING*6,VALUE-'000000'   /.実行時刻./
    VAR       ?KOUSIN   ,STRING*1,VALUE-'0'        /.更新区分./
    VAR       ?KOUSIN1  ,STRING*1,VALUE-'0'        /.更新区分１./
    VAR       ?KOUSIN2  ,STRING*1,VALUE-'0'        /.更新区分２./
    VAR       ?TYUKENSU ,STRING*7,VALUE-'0000000'  /.抽出件数./
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./

/.-----------------------------------------------------------./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.----------------------------------------------------------------./
/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ナフコ作場数量一括変更'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB/TOKMDLIB/
              TOKDTLIB/TOKSOLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?JSOKCD,?DSOKCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##資源の獲得##./
ASS:
     ASSIGN FILE-SKSRHEF.TOKDTLIB!@XCL
     IF     @PGMEC ^=  0  THEN
       ?OPR1 := '　＃＃＃＃＃＃＃　排　他　確　認　＃＃＃＃＃＃＃　'
       ?OPR2 := '　＃＜　ナフコ作場数量一括変更処理　排他制御＞＃　'
       ?OPR3 := '　＃他の端末にて資源が使用されております。　　＃　'
       ?OPR4 := '　＃確認して下さい。（全端末未使用ですか？）　＃　'
       ?OPR5 := '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
       CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       GOTO   ASS     END

/.----------------------------------------------------------------./
/.##ナフコ作場数量一括変更指示##./
SSY3880I:

    ?STEP :=   'SSY3880I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SSY3880I.TOKSOLIB,
              PARA-(?BUMON,?TANCD,?BTYMD,?BTTIME,?BTTOKC,?SAKUBA,
                    ?NOUDT,?TENPO,?NFSYOCD,?SKBKBN,?SURKBN,
                    ?HENSAKUB,?HENSURYO,?UPDDATE,?UPDTIME)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ作場数量一括変更指示'
              GOTO   ABEND
    END

    ?MSGX :=  '部門　　　　＝'  && ?BUMON
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '担当者　　　＝'  && ?TANCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信日　　　＝'  && ?BTYMD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信時刻　　＝'  && ?BTTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '取引先　　　＝'  && ?BTTOKC
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場　　　　＝'  && ?SAKUBA
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日　　　＝'  && ?NOUDT
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '店舗ＣＤ　　＝'  && ?TENPO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'ナフコ商品　＝'  && ?NFSYOCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場変更区分＝'  && ?SKBKBN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '数量変更区分＝'  && ?SURKBN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '変更作場　　＝'  && ?HENSAKUB
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '変更数量　　＝'  && ?HENSURYO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '基準日付　　＝'  && ?UPDDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '基準時刻　　＝'  && ?UPDTIME
    SNDMSG    ?MSGX,TO-XCTL

/.----------------------------------------------------------------./
/.##ナフコ作場数量一括変更データ抽出##./
SSY3881B:

    ?STEP :=   'SSY3881B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SSY3881B.TOKSOLIB,
              PARA-(?BUMON,?TANCD,?BTYMD,?BTTIME,?BTTOKC,?SAKUBA,
                    ?NOUDT,?TENPO,?NFSYOCD,?SKBKBN,?SURKBN,
                    ?HENSAKUB,?HENSURYO,?UPDDATE,?UPDTIME,?KOUSIN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ作場数量一括変更データ抽出'
              GOTO   ABEND
    END
    /.##抽出対象データ無しのとき、画面を表示##./
    IF        ?KOUSIN    =  '1'    THEN
              GOTO   KOUSINNS
    END

/.----------------------------------------------------------------./
/.##ナフコ作場数量一括変更データ更新指示##./
SSY3882B:

    ?STEP :=   'SSY3882B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SSY3882B.TOKSOLIB,
              PARA-(?BUMON,?TANCD,?UPDDATE,?UPDTIME,?TYUKENSU)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ作場数量一括変更データ更新指示'
              GOTO   ABEND
    END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ作場数量一括変更が正常終了しまし'
    ?KEKA2 :=  'た。必ず変更前、変更後の作場ＣＤで発注'
    ?KEKA3 :=  '関係資料処理を実行して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ作場数量一括変更が異常終了しまし'
    ?KEKA2 :=  'た。ログを採取し、ＮＡＶへ連絡して下さ'
    ?KEKA3 :=  'い。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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

    RETURN    PGMEC-?PGMEC

KOUSINNS:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ作場数量一括変更処理を実施しまし'
    ?KEKA2 :=  'たが、指定された条件の対象データが存在'
    ?KEKA3 :=  'しません。再度、確認して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    RETURN    PGMEC-@PGMEC

