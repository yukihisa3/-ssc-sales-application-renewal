/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援                       *  ./
/. *   JOB-ID      :    PSY38350                             *  ./
/. *   JOB-NAME    :    発注確定取消処理　　　　　　　　     *  ./
/. ***********************************************************  ./
    PGM   (SYURUI-?SYURUI)

    PARA ?SYURUI,STRING*1,IN,VALUE-' '  /. 確定取消種類 ./
                                        /.  1:オンライン分 ./
                                        /.  2:本発・手書分 ./

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY38350'
    VAR       ?STEP     ,STRING*8
    VAR       ?TANNO    ,STRING*03,VALUE-'   '     /.端末番号./
    VAR       ?HAC      ,STRING*3,VALUE-'HAC'
    VAR       ?HACW     ,STRING*1,VALUE-'W'
    VAR       ?HACLSTW  ,NAME!MOD
    VAR       ?NHACLSTW ,STRING*17,VALUE-'                 '
    VAR       ?CSV      ,STRING*3,VALUE-'CSV'
    VAR       ?CSVW     ,STRING*1,VALUE-'W'
    VAR       ?HACCSVW  ,NAME!MOD
    VAR       ?NHACCSVW ,STRING*17,VALUE-'                 '
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*7,VALUE-'TOKDLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?KANRINO  ,STRING*8,VALUE-'00000000' /.管理番号    ./
    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?BTYMD    ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?BTTIME   ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?BTTOKC   ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?P4       ,STRING*2,VALUE-'0'        /.倉庫ＣＤ  ./
    VAR       ?SKBCD    ,STRING*2,VALUE-'00'       /.作場./
    VAR       ?P5       ,STRING*1,VALUE-'0'        /.発注／訂正  ./
    VAR       ?P6       ,STRING*1,VALUE-'0'        /.出力順      ./
    VAR       ?SYUPTN   ,STRING*1,VALUE-'0'        /.出力パターン./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
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

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '発注確定取消処理'

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P7)
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
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##確定種類チェック##./
    IF        ?SYURUI    =  '1'   THEN
              GOTO SKY6101I
    ELSE
          IF  ?SYURUI    =  '2'   THEN
              GOTO SKY6101I
          ELSE
              ?MSGX :=  '確定種類が指定されていません！！'
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '確定種類＝'  && '？？？？？？？？'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO ABEND
          END
    END

/.##担当者CD指定##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0  THEN
              IF  ?PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  ?KEKA4 :=  '【担当者ＣＤ指定】'
                  GOTO  ABEND
              END
    END

    ?MSGX :=  '所属部門＝'  && ?BUMON
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '担当者　＝'  && ?TANCD
    SNDMSG    ?MSGX,TO-XCTL

/.##ナフコ発注確定　取消し処理指示##./
SSY3835I:

    ?STEP :=   'SSY3835I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      PGM-SSY3835I.TOKELIBO,PARA-(?SYURUI,?BUMON,?TANCD,
                                          ?KANRINO,?SKBCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
    END

    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確定　取消し処理指示'
              GOTO   ABEND
    END

    IF        ?SYURUI    =  '1'   THEN
              ?MSGX :=  '確定種類＝'  && 'オンライン'
    ELSE
          IF  ?SYURUI    =  '2'   THEN
              ?MSGX :=  '確定種類＝'  && '本発/ＦＡＸ/手書'
          ELSE
              ?MSGX :=  '確定種類＝'  && '？？？？？？？？'
          END
    END
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '管理番号＝'  && ?KANRINO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '作場ＣＤ＝'  && ?SKBCD
    SNDMSG    ?MSGX,TO-XCTL

/.##発注確定取消し処理##./
SSY3836B:

    ?STEP :=   'SSY3836B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SSY3836B.TOKELIBO,PARA-(?SYURUI,
                                          ?KANRINO,?SKBCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確定　取消し処理'
              GOTO ABEND END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '発注確定取消処理が正常終了しました'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '発注確定取消し処理が異常しました。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
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
